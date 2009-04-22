%%	The contents of this file are subject to the Common Public Attribution
%%	License Version 1.0 (the “License”); you may not use this file except
%%	in compliance with the License. You may obtain a copy of the License at
%%	http://opensource.org/licenses/cpal_1.0. The License is based on the
%%	Mozilla Public License Version 1.1 but Sections 14 and 15 have been
%%	added to cover use of software over a computer network and provide for
%%	limited attribution for the Original Developer. In addition, Exhibit A
%%	has been modified to be consistent with Exhibit B.
%%
%%	Software distributed under the License is distributed on an “AS IS”
%%	basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%	License for the specific language governing rights and limitations
%%	under the License.
%%
%%	The Original Code is Spice Telephony.
%%
%%	The Initial Developers of the Original Code is 
%%	Andrew Thompson and Micah Warren.
%%
%%	All portions of the code written by the Initial Developers are Copyright
%%	(c) 2008-2009 SpiceCSM.
%%	All Rights Reserved.
%%
%%	Contributor(s):
%%
%%	Andrew Thompson <athompson at spicecsm dot com>
%%	Micah Warren <mwarren at spicecsm dot com>
%%

%% @doc The cook is a process that is spawned per call in queue, it
%% executes the queue's 'recipe' on the call and handles call delivery to an
%% agent. When it finds one or more dispatchers bound to its call it requests
%% that each dispatcher generate a list of local agents matching the call's
%% criteria and selects the best one to offer it to.
-module(cook).
-author("Micah").

-behaviour(gen_server).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("queue.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-define(TICK_LENGTH, 500).

-define(RINGOUT, 4).
-define(DEFAULT_PATHCOST, 15).

%% API
-export([
	start_link/3,
	start/3,
	stop/1,
	restart_tick/1,
	stop_tick/1,
	start_at/4
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {
		recipe = [] :: recipe(),
		ticked = 1 :: pos_integer(), % number of ticks we've done
		call :: pid() | 'undefined',
		queue :: string() | 'undefined',
		continue = true :: bool(),
		ringingto :: pid(),
		ringcount = 0 :: non_neg_integer(),
		tref :: any() % timer reference
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Starts a cook linked to the parent process for `Call' processed by `Recipe' for call_queue named `Queue'.
-spec(start_link/3 :: (Call :: pid(), Recipe :: recipe(), Queue :: string()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start_link(Call, Recipe, Queue) when is_pid(Call) ->
    gen_server:start_link(?MODULE, [Call, Recipe, Queue], []).

%% @doc Starts a cook not linked to the parent process for `Call' processed by `Recipe' for call_queue named `Queue'.
-spec(start/3 :: (Call :: pid(), Recipe :: recipe(), Queue :: string()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start(Call, Recipe, Queue) when is_pid(Call) ->
	gen_server:start(?MODULE, [Call, Recipe, Queue], []).

%% @doc starts a new cook on the give `node()' `Node' for `Call' to be process by `Recipe' for the call_queue named `Queue'.
%% This is used in place of start and start_link to allow a queue on a different node to start the cook on the same node
%% the media exists on.
start_at(Node, Call, Recipe, Queue) ->
	F = fun() ->
		case init([Call, Recipe, Queue]) of
			{ok, State} ->
				?DEBUG("about to enter loop", []),
				gen_server:enter_loop(?MODULE, [], State)%;
			%{stop, Reason} ->
			%	?CONSOLE("not entering loop due to ~p", [Reason]),
			%	{error, Reason}
		end
	end,
	{ok, proc_lib:spawn_link(Node, F)}.
	
%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init([Call, Recipe, Queue]) ->
	?DEBUG("Cook starting for call ~p from queue ~p", [Call, Queue]),
	?DEBUG("node check.  self:  ~p;  call:  ~p", [node(self()), node(Call)]),
	process_flag(trap_exit, true),
	%case is_process_alive(Call) of
	%	true ->
			%process_flag(trap_exit, true),
			{ok, Tref} = timer:send_interval(?TICK_LENGTH, do_tick),
			State = #state{recipe=Recipe, call=Call, queue=Queue, tref=Tref},
			{ok, State}.%;
	%	false ->
	%		?CONSOLE("Call process not alive, aborting start", []),
	%		{stop, media_pid_dead}
%	end.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
handle_call(stop, From, State) ->
	?NOTICE("Stop requested from ~p", [From]),
	{stop, normal, ok, State};
handle_call({stop, Reason}, From, State) -> 
	?NOTICE("Stop requested from ~p for ~p.", [From, Reason]),
	{stop, {normal, Reason}, ok, State};
handle_call(Request, _From, State) ->
    {reply, {unknown_call, Request}, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast(restart_tick, State) ->
	case do_route(State#state.ringcount, State#state.queue, State#state.ringingto, State#state.call) of
		nocall -> 
			{stop, {call_not_queued, State#state.call}, State};
		rangout -> 
			State2 = State#state{ringingto = undefined, ringcount = 0},
			NewRecipe = do_recipe(State2#state.recipe, State2#state.ticked, State2#state.queue, State2#state.call),
			State3 = State2#state{ticked = State2#state.ticked + 1, recipe = NewRecipe},
			{ok, Tref} = timer:send_interval(?TICK_LENGTH, do_tick),
			{noreply, State3#state{tref=Tref}};
		{ringing, Apid, Ringcount} -> 
			State2 = State#state{ringingto = Apid, ringcount = Ringcount},
			NewRecipe = do_recipe(State2#state.recipe, State2#state.ticked, State2#state.queue, State2#state.call),
			State3 = State2#state{ticked = State2#state.ticked + 1, recipe = NewRecipe},
			{ok, Tref} = timer:send_interval(?TICK_LENGTH, do_tick),
			{noreply, State3#state{tref=Tref}}
	end;
handle_cast(stop_tick, State) ->
	timer:cancel(State#state.tref),
	{noreply, State#state{tref=undefined}};
handle_cast({stop_ringing, AgentPid}, State) when AgentPid =:= State#state.ringingto ->
	?DEBUG("Ordered to stop ringing ~p", [AgentPid]),
	agent:set_state(AgentPid, idle),
	gen_server:cast(State#state.call, stop_ringing),
	{noreply, State#state{ringingto=undefined}};
handle_cast({stop_ringing_keep_state, AgentPid}, State) when AgentPid =:= State#state.ringingto ->
	?DEBUG("Ordered to stop ringing without changing state ~p", [AgentPid]),
	gen_server:cast(State#state.call, stop_ringing),
	{noreply, State#state{ringingto=undefined}};
handle_cast(remove_from_queue, State) ->
	Qpid = queue_manager:get_queue(State#state.queue),
	call_queue:remove(Qpid, State#state.call),
	{noreply, State};
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(Msg, State) ->
	?DEBUG("unhandled cast ~p", [Msg]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info(do_tick, State) ->
	?DEBUG("do_tick caught, beginning processing...", []),
	case whereis(queue_manager) of % do we even need this?  We do have a terminate that should catch a no-proc.
		undefined ->
			{stop, queue_manager_undefined, State};
		_Else ->
			case queue_manager:get_queue(State#state.queue) of
				undefined ->
					{stop, {queue_undefined, State#state.queue}, State};
				_Other ->
					?DEBUG("Doing tick ~p", [State]),
					case do_route(State#state.ringcount, State#state.queue, State#state.ringingto, State#state.call) of
						nocall -> 
							{stop, {call_not_queued, State#state.call}, State};
						rangout -> 
							State2 = State#state{ringingto = undefined, ringcount = 0},
							NewRecipe = do_recipe(State2#state.recipe, State2#state.ticked, State2#state.queue, State2#state.call),
							State3 = State2#state{ticked = State2#state.ticked + 1, recipe = NewRecipe},
							{noreply, State3};
						{ringing, Apid, Ringcount} -> 
							State2 = State#state{ringingto = Apid, ringcount = Ringcount},
							NewRecipe = do_recipe(State2#state.recipe, State2#state.ticked, State2#state.queue, State2#state.call),
							State3 = State2#state{ticked = State2#state.ticked + 1, recipe = NewRecipe},
							{noreply, State3}
					end
			end
	end;
handle_info(Info, State) ->
	?DEBUG("received random info message: ~p", [Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @private
terminate(normal, _State) ->
	?DEBUG("normal death", []),
	ok;
terminate(shutdown, _State) ->
	?DEBUG("shutdown death", []),
	ok;
terminate({normal, Reason}, _State) ->
	?NOTICE("An inelegant shutdown requested for ~p", [Reason]),
	ok;
terminate(Reason, State) ->
	?WARNING("Unusual death:  ~p", [Reason]),
	timer:cancel(State#state.tref),
	Qpid = wait_for_queue(State#state.queue),
	?INFO("Looks like the queue recovered, dieing now",[]),
	call_queue:add(Qpid, State#state.call),
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @doc Restart the cook at `Pid'.
restart_tick(Pid) ->
	gen_server:cast(Pid, restart_tick).

%% @doc Pause the cook running at `Pid'.
stop_tick(Pid) ->
	gen_server:cast(Pid, stop_tick).

%% @private
wait_for_queue(Qname) ->
	case whereis(queue_manager) of
		undefined ->
			?INFO("Waiting on the queue manager", []),
			receive
			after 1000 ->
				ok
			end;
		QMPid when is_pid(QMPid) ->
			?DEBUG("Queue manager is available", []),
			ok
	end,
	case queue_manager:get_queue(Qname) of
		undefined ->
			?INFO("Waiting on queue ~p" , [Qname]),
			receive
			after 300 ->
				ok
			end,
			wait_for_queue(Qname);
		Qpid when is_pid(Qpid) ->
			?DEBUG("Queue ~p is back up", [Qname]),
			Qpid
	end.

%% @doc Stop the cook with reason `normal'.
stop(Pid) ->
	gen_server:call(Pid, stop).

%% @private
-spec(do_route/4 :: (Ringcount :: non_neg_integer(), Queue :: string(), 'undefined' | pid(), pid()) -> 'nocall' | {'ringing', pid(), non_neg_integer()} | 'rangout').
do_route(Ringcount, _Queue, Agentpid, _Callpid) when is_pid(Agentpid) ->
	?DEBUG("still ringing: ~p times", [Ringcount]),
	{ringing, Agentpid, Ringcount + 1};
%do_route(Ringcount, _Queue, Agentpid, Callpid) when is_pid(Agentpid), Ringcount > ?RINGOUT, is_pid(Callpid) ->
%	?CONSOLE("rang out",[]),
	%agent:set_state(Agentpid, idle),
%	rangout;
do_route(_Ringcount, Queue, undefined, Callpid) when is_pid(Callpid), is_list(Queue) ->
	?DEBUG("Searching for agent to ring to...",[]),
	Qpid = queue_manager:get_queue(Queue),
	case call_queue:get_call(Qpid, Callpid) of
		{_Key, Call} ->
			Dispatchers = Call#queued_call.dispatchers,
			Agents = sort_agent_list(Dispatchers),
			?DEBUG("Dispatchers:  ~p; Agents:  ~p", [Dispatchers, Agents]),
			case offer_call(Agents, Call) of
				none -> 
					rangout;
				Apid when is_pid(Apid) -> 
					{ringing, Apid, 0}
			end;
		none -> 
			?DEBUG("No call to ring",[]),
			nocall
	end.

%% @private
-spec(sort_agent_list/1 :: (Dispatchers :: [pid()]) -> [{non_neg_integer(), pid()}]).
sort_agent_list([]) -> 
	[];
sort_agent_list(Dispatchers) when is_list(Dispatchers) -> 
	F = fun(Dpid) ->
		try dispatcher:get_agents(Dpid) of
			[] ->
				?DEBUG("empty list, might as well tell this dispatcher to regrab", []),
				dispatcher:regrab(Dpid),
				[];
			Ag ->
				Ag
		catch
			What:Why ->
				?INFO("Caught:  ~p:~p", [What, Why]),
				[]
		end
	end,
	Agents = lists:map(F, Dispatchers),
	Agents2 = lists:flatten(Agents),
	% calculate costs and sort by same.
	Agents3 = lists:sort([{ARemote + Askills + Aidle, APid} ||
		{_AName, APid, AState} <- Agents2,
		ARemote <-
		case APid of
			_X when node() =:= node(APid) ->
				[0];
			_Y ->
				[?DEFAULT_PATHCOST]
		end,
		Askills <- [length(AState#agent.skills)],
		Aidle <- [element(2, AState#agent.lastchangetimestamp)]]),
	Agents3.


%% @private
-spec(offer_call/2 :: (Agents :: [{non_neg_integer, pid()}], Call :: #queued_call{}) -> 'none' | pid()).
offer_call([], _Call) ->
	?DEBUG("No valid agents found", []),
	none;
offer_call([{_ACost, Apid} | Tail], Call) ->
	case gen_server:call(Call#queued_call.media, {ring_agent, Apid, Call, ?TICK_LENGTH * (?RINGOUT + 1)}) of
		ok ->
			Agent = agent:dump_state(Apid),
			Callrec = gen_server:call(Call#queued_call.media, get_call),
			?INFO("cook offering call:  ~p to ~p", [Callrec#call.id, Agent#agent.login]),
			cdr:ringing(Callrec, Agent#agent.login),
			Apid;
		invalid ->
			offer_call(Tail, Call)
	end.

%% @private
-spec(check_conditions/4 :: (Conditions :: [recipe_condition()], Ticked :: non_neg_integer(), Queue :: string(), Call :: pid()) -> 'true' | 'false').
check_conditions([], _Ticked, _Queue, _Call) ->
	true;
check_conditions([{ticks, Ticks} | Conditions], Ticked, Queue, Call) when Ticked > 0 andalso (Ticked rem Ticks) == 0 ->
	check_conditions(Conditions, Ticked, Queue, Call);
check_conditions([{ticks, _Ticks} | _Conditions], _Ticked, _Queue, _Call) ->
	false;
check_conditions([{available_agents, Comparision, Number} | Conditions], Ticked, Queue, Call) ->
	Qpid = queue_manager:get_queue(Queue),
	{_Key, Callrec} = call_queue:get_call(Qpid, Call),
	L = agent_manager:find_avail_agents_by_skill(Callrec#queued_call.skills),
	Agents = length(L),
	case Comparision of
		'>' when Agents > Number ->
			check_conditions(Conditions, Ticked, Queue, Call);
		'<' when Agents < Number ->
			check_conditions(Conditions, Ticked, Queue, Call);
		'=' when Agents =:= Number ->
			check_conditions(Conditions, Ticked, Queue, Call);
		_Else ->
			false
	end;
check_conditions([{eligible_agents, Comparision, Number} | Conditions], Ticked, Queue, Call) ->
	Qpid = queue_manager:get_queue(Queue),
	{_Key, Callrec} = call_queue:get_call(Qpid, Call),
	L = agent_manager:find_by_skill(Callrec#queued_call.skills),
	Agents = length(L),
	?DEBUG("Number: ~p; agents: ~p", [Number, Agents]),
	case Comparision of
		'>' when Agents > Number ->
			check_conditions(Conditions, Ticked, Queue, Call);
		'<' when Agents < Number ->
			check_conditions(Conditions, Ticked, Queue, Call);
		'=' when Number =:= Agents ->
			check_conditions(Conditions, Ticked, Queue, Call);
		_Else ->
			false
	end;
check_conditions([{calls_queued, Comparision, Number} | Conditions], Ticked, Queue, Call) ->
	Qpid = queue_manager:get_queue(Queue),
	Count = call_queue:call_count(Qpid),
	case Comparision of
		'>' when Count > Number ->
			check_conditions(Conditions, Ticked, Queue, Call);
		'<' when Count < Number ->
			check_conditions(Conditions, Ticked, Queue, Call);
		'=' when Number =:= Count ->
			check_conditions(Conditions, Ticked, Queue, Call);
		_Else ->
			false
	end;
check_conditions([{queue_position, Comparision, Number} | Conditions], Ticked, Queue, Call) ->
	Qpid = queue_manager:get_queue(Queue),
	Calls = call_queue:to_list(Qpid),
	Test = fun(Needle, #queued_call{media = Mpid}) ->
		Needle =:= Mpid
	end,
	Position = util:list_index(Test, Call, Calls),
	case Comparision of
		'>' when Position > Number ->
			check_conditions(Conditions, Ticked, Queue, Call);
		'<' when Position < Number, Position > 0 ->
			check_conditions(Conditions, Ticked, Queue, Call);
		'=' when Position =:= Number ->
			check_conditions(Conditions, Ticked, Queue, Call);
		_Else ->
			false
	end.
		
%% @private
-spec(do_recipe/4 :: (Recipe :: recipe(), Ticked :: non_neg_integer(), Queuename :: string(), Call :: pid()) -> recipe()).
do_recipe([], _Ticked, _Queuename, _Call) ->
	[];
do_recipe([{Conditions, Op, Args, Runs} | Recipe], Ticked, Queuename, Call) when is_list(Queuename), is_pid(Call) ->
	case check_conditions(Conditions, Ticked, Queuename, Call) of
		true ->
			Doneop = do_operation({Conditions, Op, Args, Runs}, Queuename, Call),
			case Doneop of
				{Newconds, Newop, Newargs, Newruns} when Runs =:= run_once ->
					%add to the output recipe
					[{Newconds, Newop, Newargs, Newruns} | do_recipe(Recipe, Ticked, Queuename, Call)];
				{Newconds, Newop, Newargs, Newruns} when Runs =:= run_many ->
					lists:append([{Newconds, Newop, Newargs, Newruns}, {Conditions, Op, Args, Runs}], do_recipe(Recipe, Ticked, Queuename, Call));
				ok when Runs =:= run_many ->
					[{Conditions, Op, Args, Runs} | do_recipe(Recipe, Ticked, Queuename, Call)];
				ok when Runs =:= run_once ->
					do_recipe(Recipe, Ticked, Queuename, Call)
					% don't, just dance.
			end;
		false ->
			[{Conditions, Op, Args, Runs} | do_recipe(Recipe, Ticked, Queuename, Call)]
	end.
%do_recipe([Head | Recipe], Ticked, Queuename, Call) when is_list(Queuename), is_pid(Call) ->
%	% this is here in case a recipe is not due to be run.
%	[Head | do_recipe(Recipe, Ticked, Queuename, Call)].

%% @private
-spec(do_operation/3 :: (Recipe :: recipe_step(), Queuename :: string(), Callid :: pid()) -> 'ok' | recipe_step()).
do_operation({_Conditions, Op, Args, _Runs}, Queuename, Callid) when is_list(Queuename), is_pid(Callid) ->
	?INFO("do_operation ~p with args ~p", [Op, Args]),
	Pid = queue_manager:get_queue(Queuename), %TODO look up the pid only once, maybe?
	case Op of
		add_skills ->
			call_queue:add_skills(Pid, Callid, Args),
			ok;
		remove_skills ->
			call_queue:remove_skills(Pid, Callid, Args),
			ok;
		set_priority ->
			[Priority] = Args,
			call_queue:set_priority(Pid, Callid, Priority),
			ok;
		prioritize ->
			{{Prior, _Time}, _Call} = call_queue:get_call(Pid, Callid),
			call_queue:set_priority(Pid, Callid, Prior + 1),
			ok;
		deprioritize ->
			{{Prior, _Time}, _Call} = call_queue:get_call(Pid, Callid),
			call_queue:set_priority(Pid, Callid, Prior - 1),
			ok;
		voicemail ->
			case gen_server:call(Callid, voicemail) of
				ok ->
					?DEBUG("voicemail successfully, removing from queue", []),
					call_queue:bgremove(Pid, Callid);
				invalid ->
					?WARNING("voicemail failed.", []),
					ok
			end;
		add_recipe ->
			list_to_tuple(Args);
		announce ->
			gen_server:call(Callid, {announce, Args}),
			ok
	end.

-ifdef(EUNIT).

test_primer() ->
	["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	mnesia:start().

queue_interaction_test_() ->
	{timeout, 60,
	{
		foreach,
		fun() ->
			test_primer(),
			queue_manager:start([node()]),
			{ok, Pid} = queue_manager:add_queue("testqueue"),
			{ok, Dummy} = dummy_media:start("testcall"),
			dummy_media:set_skills(Dummy, [english, testskill]),
			register(media_dummy, Dummy),
			{Pid, Dummy}
		end,
		fun({Pid, _Dummy}) ->
			unregister(media_dummy),
			try call_queue:stop(Pid)
			catch
				exit:{noproc, Detail} ->
					?debugFmt("caught exit:~p ; some tests will kill the original call_queue process.", [Detail])
			end,
			queue_manager:stop()
		end,
		[
			{"Add skills once",
			fun() ->
				{exists, Pid} = queue_manager:add_queue("testqueue"),
				call_queue:set_recipe(Pid, [{[{ticks, 1}], add_skills, [newskill1, newskill2], run_once}]),
				call_queue:add(Pid, whereis(media_dummy)),
				receive
				after ?TICK_LENGTH + 2000 ->
					true
				end,
				{_Key, #queued_call{skills=CallSkills}} = call_queue:ask(Pid),
				?assertEqual(lists:sort([english, testskill, newskill1, newskill2, {'_node', node()}]), lists:sort(CallSkills))
			end},
			{"remove skills once",
			fun() ->
				{exists, Pid} = queue_manager:add_queue("testqueue"),
				call_queue:set_recipe(Pid, [{[{ticks, 1}], remove_skills, [testskill], run_once}]),
				call_queue:add(Pid, whereis(media_dummy)),
				receive
				after ?TICK_LENGTH + 2000 ->
					true
				end,
				{_Key, #queued_call{skills=CallSkills}} = call_queue:ask(Pid),
				?assertEqual([{'_node', node()}, english], CallSkills)
			end},
			{"Set Priority once",
			fun() ->
				{exists, Pid} = queue_manager:add_queue("testqueue"),
				call_queue:set_recipe(Pid, [{[{ticks, 1}], set_priority, [5], run_once}]),
				call_queue:add(Pid, whereis(media_dummy)),
				receive
				after ?TICK_LENGTH + 2000 ->
					true
				end,
				{{Prior, _Time}, _Call} = call_queue:ask(Pid),
				?assertEqual(5, Prior)
			end},
			{"Prioritize once",
			fun() ->
				{exists, Pid} = queue_manager:add_queue("testqueue"),
				call_queue:set_recipe(Pid, [{[{ticks, 1}], prioritize, [], run_once}]),
				{ok, Dummy1} = dummy_media:start("C1"),
				call_queue:add(Pid, Dummy1),
				receive
				after ?TICK_LENGTH * 3 ->
					ok
				end,
				{{Prior, _Time}, _Call} = call_queue:get_call(Pid, Dummy1),
				?assertEqual(2, Prior)
			end},
			{"Prioritize many (waiting for 2 ticks)",
			fun() ->
				{exists, Pid} = queue_manager:add_queue("testqueue"),
				call_queue:set_recipe(Pid, [{[{ticks, 1}], prioritize, [], run_many}]),
				{ok, Dummy1} = dummy_media:start("C1"),
				call_queue:add(Pid, Dummy1),
				receive
				after ?TICK_LENGTH * 2 + 100 ->
					ok
				end,
				{{Prior, _Time}, _Call} = call_queue:get_call(Pid, Dummy1),
				?assertEqual(3, Prior)
			end},
			{"Deprioritize once",
			fun() -> 
				{exists, Pid} = queue_manager:add_queue("testqueue"),
				call_queue:set_recipe(Pid, [{[{ticks, 1}], deprioritize, [], run_once}]),
				{ok, Dummy1} = dummy_media:start("C1"),
				call_queue:add(Pid, 2, Dummy1),
				receive
				after ?TICK_LENGTH * 3 ->
					ok
				end,
				{{Prior, _Time}, _Call} = call_queue:get_call(Pid, Dummy1),
				?assertEqual(1, Prior)
			end},
			{"Deprioritize many (waiting for 2 ticks)",
			fun() ->
				{exists, Pid} = queue_manager:add_queue("testqueue"),
				call_queue:set_recipe(Pid, [{[{ticks, 1}], deprioritize, [], run_many}]),
				{ok, Dummy1} = dummy_media:start("C1"),
				call_queue:add(Pid, 4, Dummy1),
				receive
				after ?TICK_LENGTH * 2 + 100 ->
					ok
				end,
				{{Prior, _Time}, _Call} = call_queue:get_call(Pid, Dummy1),
				?assertEqual(2, Prior)
			end},
			{"Deprioritize to below zero",
			fun() ->
				{exists, Pid} = queue_manager:add_queue("testqueue"),
				call_queue:set_recipe(Pid, [{[{ticks, 1}], deprioritize, [], run_many}]),
				{ok, Dummy1} = dummy_media:start("C1"),
				call_queue:add(Pid, 1, Dummy1),
				receive
				after ?TICK_LENGTH * 2 + 100 ->
					ok
				end,
				{{Prior, _Time}, _Call} = call_queue:get_call(Pid, Dummy1),
				?assertEqual(0, Prior)
			end},
			{"Add recipe once",
			fun() ->
				{exists, Pid} = queue_manager:add_queue("testqueue"),
				call_queue:set_recipe(Pid, [{[{ticks, 1}], add_recipe, [[], prioritize, [], run_once], run_once}]),
				Dummy1 = whereis(media_dummy),
				call_queue:add(Pid, Dummy1),
				receive
				after ?TICK_LENGTH * 2 + 100 ->
					ok
				end,
				{{Prior, _Time}, _Call} = call_queue:get_call(Pid, Dummy1),
				?assertEqual(2, Prior)
			end},
			{"Add recipe many",
			fun() ->
				{exists, Pid} = queue_manager:add_queue("testqueue"),
				Subrecipe = [[], prioritize, [], run_many],
				call_queue:set_recipe(Pid, [{[{ticks, 1}], add_recipe, Subrecipe, run_many}]),
				Dummy1 = whereis(media_dummy),
				call_queue:add(Pid, Dummy1),
				receive
				after ?TICK_LENGTH * 3 + 100 ->
					ok
				end,
				{{Prior, _Time}, _Call} = call_queue:get_call(Pid, Dummy1),
				?assertEqual(4, Prior)
			end},
			{"Voice mail once for supported media",
			fun() ->
				{exists, Pid} = queue_manager:add_queue("testqueue"),
				call_queue:set_recipe(Pid, [{[{ticks, 1}], voicemail, [], run_once}]),
				Dummy1 = whereis(media_dummy),
				call_queue:add(Pid, Dummy1),
				receive
				after ?TICK_LENGTH * 2 + 100 ->
					ok
				end,
				?assertEqual(none, call_queue:get_call(Pid, Dummy1))
			end},
			{"Voicemail for unsupported media",
			fun() ->
				{exists, Pid} = queue_manager:add_queue("testqueue"),
				call_queue:set_recipe(Pid, [{[{ticks, 1}], voicemail, [], run_once}]),
				Dummy1 = whereis(media_dummy),
				call_queue:add(Pid, Dummy1),
				gen_server:call(Dummy1, set_failure),
				receive
				after ?TICK_LENGTH + 100 ->
					gen_server:call(Dummy1, set_success)
				end,
				?assertMatch({_Key, #queued_call{id="testcall"}}, call_queue:get_call(Pid, Dummy1))
			end},
			{"Annouce (media doesn't matter)",
			fun() ->
				{exists, Pid} = queue_manager:add_queue("testqueue"),
				call_queue:set_recipe(Pid, [{[{ticks, 1}], announce, "random data", run_once}]),
				Dummy1 = whereis(media_dummy),
				call_queue:add(Pid, Dummy1),
				receive
				after ?TICK_LENGTH + 100 ->
					ok
				end,
				?assertMatch({_Key, #queued_call{id="testcall"}}, call_queue:get_call(Pid, Dummy1))
			end},
			{"Skip some ticks",
			fun() ->
				{exists, Pid} = queue_manager:add_queue("testqueue"),
				call_queue:set_recipe(Pid, [{[{ticks, 2}], prioritize, [], run_many}]),
				Dummy1 = whereis(media_dummy),
				call_queue:add(Pid, Dummy1),
				receive
				after ?TICK_LENGTH * 4 + 100 ->
					ok
				end,
				{{Priority, _Time}, _Call} = call_queue:get_call(Pid, Dummy1),
				?assertEqual(3, Priority)
			end
			},
			{"Waiting for queue rebirth",
			fun() ->
				call_queue_config:new_queue(#call_queue{
					name = "testqueue",
					recipe = [{[{ticks, 1}], add_skills, [newskill1, newskill2], run_once}]
				}),
				{exists, Pid} = queue_manager:add_queue("testqueue"),
				call_queue:add(Pid, whereis(media_dummy)),
				{_Pri, _CallRec} = call_queue:ask(Pid),
				?assertEqual(1, call_queue:call_count(Pid)),
				timer:sleep(100), % TODO this is hear because the cook is not started quickly.
					% thus, if the queue dies while the cook is starting, the media is orphaned.
					% we should prolly do somthing about that.
				gen_server:call(Pid, {stop, testy}),
				?assert(is_process_alive(Pid) =:= false),
				receive
				after 1000 ->
					ok
				end,
				NewPid = queue_manager:get_queue("testqueue"),
				?CONSOLE("The calls:  ~p", [call_queue:print(NewPid)]),
				?assertEqual(1, call_queue:call_count(NewPid)),
				call_queue:stop(NewPid),
				call_queue_config:destroy_queue("testqueue")
			end
			},
			{"Queue Manager dies",
			fun() ->
				call_queue_config:new_queue(#call_queue{
					name = "testqueue",
					recipe = [{[{ticks, 1}], add_skills, [newskill1, newskill2], run_once}]
				}),
				{exists, Pid} = queue_manager:add_queue("testqueue"),
				call_queue:add(Pid, whereis(media_dummy)),
				{_Pri, _CallRec} = call_queue:ask(Pid),
				?assertEqual(1, call_queue:call_count(Pid)),
				?CONSOLE("before death:  ~p", [call_queue:print(Pid)]),
				QMPid = whereis(queue_manager),
				exit(QMPid, kill),
				receive
				after 300 ->
					ok
				end,

				?assert(is_process_alive(QMPid) =:= false),
				?assert(is_process_alive(Pid) =:= false),
				queue_manager:start([node()]),
				receive
				after 1000 ->
					ok
				end,
				NewPid = queue_manager:get_queue("testqueue"),

				?assertEqual(1, call_queue:call_count(NewPid)),
				call_queue:stop(NewPid),
				call_queue_config:destroy_queue("testqueue")
			end
			}
		]
	}
	}.	

tick_manipulation_test_() ->
	{foreach,
	fun() ->
		test_primer(),
		queue_manager:start([node()]),
		{ok, Pid} = queue_manager:add_queue("testqueue"),
		{ok, Dummy} = dummy_media:start("testcall"),
		dummy_media:set_skills(Dummy, [english, testskill]),
		{Pid, Dummy}
	end,
	fun({Pid, Dummy}) ->
		dummy_media:stop(Dummy),
		try call_queue:stop(Pid)
		catch
			exit:{noproc, Detail} ->
				?debugFmt("caught exit:~p ; some tests will kill the original call_queue process.", [Detail])
		end,
		queue_manager:stop()
	end,
	[
		fun({Pid, Dummy}) -> 
			{"Stop Tick Test",
			fun() ->	
				call_queue:set_recipe(Pid, [{[{ticks, 1}], prioritize, [], run_many}]),
				call_queue:add(Pid, Dummy),
				{_Pri, #queued_call{cook = Cookpid}} = call_queue:ask(Pid),
				stop_tick(Cookpid),
				receive
				after ?TICK_LENGTH * 3 + 100 ->
					ok
				end,
				{{Priority, _Time}, _Callrec} = call_queue:ask(Pid),
				?assertEqual(1, Priority)
			end}
		end,
		fun({Pid, Dummy}) ->
			{"Restart Tick Test",
			fun() ->
				call_queue:set_recipe(Pid, [{[{ticks, 1}], prioritize, [], run_many}]),
				call_queue:add(Pid, Dummy),
				{_Pri, #queued_call{cook = Cookpid}} = call_queue:ask(Pid),
				stop_tick(Cookpid),
				receive
				after ?TICK_LENGTH * 3 + 100 ->
					ok
				end,
				{{Priority1, _Time1}, _Callrec} = call_queue:ask(Pid),
				restart_tick(Cookpid),
				receive
				after ?TICK_LENGTH * 2 + 100 ->
					ok
				end,
				{{Priority2, _Time2}, _Callrec} = call_queue:ask(Pid),
				?assertEqual(1, Priority1),
				?assertEqual(4, Priority2)
			end}
		end
	]
	}.

condition_checking_test_() ->
	{foreach,
	fun() ->
		test_primer(),
		agent_auth:start(),
		queue_manager:start([node()]),
		{ok, Qpid} = queue_manager:add_queue("testqueue"),
		{ok, Mpid} = dummy_media:start("testcall"),
		dispatch_manager:start(),
		agent_manager:start([node()]),
		{Qpid, Mpid}
	end,
	fun({Qpid, Mpid}) ->
		agent_auth:stop(),
		dummy_media:stop(Mpid),
		call_queue:stop(Qpid),
		queue_manager:stop(),
		agent_manager:stop(),
		dispatch_manager:stop()
	end,
	[
		fun({_Qpid, Mpid}) ->
			{"Ticks on even ticks",
			fun() ->
				?assertEqual(false, check_conditions([{ticks, 2}], 0, "testqueue", Mpid)),
				?assertEqual(false, check_conditions([{ticks, 2}], 1, "testqueue", Mpid)),
				?assertEqual(true, check_conditions([{ticks, 2}], 2, "testqueue", Mpid)),
				?assertEqual(false, check_conditions([{ticks, 2}], 3, "testqueue", Mpid))
			end}
		end,
		fun({Qpid, Mpid}) ->
			{"Checks for the number of eligible agents correctly",
			fun() ->
				{ok, Eligible1} = agent_manager:start_agent(#agent{login = "eligible1", skills = [cando]}),
				{ok, Eligible2} = agent_manager:start_agent(#agent{login = "eligible2", skills = [cando]}),
				{ok, Eligible3} = agent_manager:start_agent(#agent{login = "elibible3", skills = [cando]}),
				agent:set_state(Eligible2, idle),
				agent:set_state(Eligible3, idle),
				{ok, Noteligible} = agent_manager:start_agent(#agent{login = "noteligible", skills = [nodo]}),
				dummy_media:set_skills(Mpid, [cando]),
				call_queue:add(Qpid, Mpid),
				call_queue:remove_skills(Qpid, Mpid, [english, '_node']),
				?assertEqual(false, check_conditions([{eligible_agents, '>', 3}], 0, "testqueue", Mpid)),
				?assertEqual(true, check_conditions([{eligible_agents, '>', 2}], 0, "testqueue", Mpid)),
				?assertEqual(false, check_conditions([{eligible_agents, '<', 3}], 0, "testqueue", Mpid)),
				?assertEqual(true, check_conditions([{eligible_agents, '<', 4}], 0, "testqueue", Mpid)),
				?assertEqual(false, check_conditions([{eligible_agents, '=', 4}], 0, "testqueue", Mpid)),
				?assertEqual(false, check_conditions([{eligible_agents, '=', 2}], 0, "testqueue", Mpid)),
				?assertEqual(true, check_conditions([{eligible_agents, '=', 3}], 0, "testqueue", Mpid)),
				agent:stop(Eligible1),
				agent:stop(Eligible2),
				agent:stop(Noteligible)
			end}
		end,
		fun({Qpid, Mpid}) ->
			{"Checks for available agents",
			fun() ->
				{ok, Available1} = agent_manager:start_agent(#agent{login = "available1"}),
				agent:set_state(Available1, idle),
				{ok, Available2} = agent_manager:start_agent(#agent{login = "available2"}),
				agent:set_state(Available2, idle),
				{ok, Notavailable} = agent_manager:start_agent(#agent{login = "notavailable"}),
				call_queue:add(Qpid, Mpid),

				?assertEqual(true, check_conditions([{available_agents, '<', 3}], 0, "testqueue", Mpid)),
				?assertEqual(false, check_conditions([{available_agents, '<', 2}], 0, "testqueue", Mpid)),
				?assertEqual(true, check_conditions([{available_agents, '>', 1}], 0, "testqueue", Mpid)),
				?assertEqual(false, check_conditions([{available_agents, '>', 2}], 0, "testqueue", Mpid)),
				?assertEqual(true, check_conditions([{available_agents, '=', 2}], 0, "testqueue", Mpid)),
				?assertEqual(false, check_conditions([{available_agents, '=', 1}], 0, "testqueue", Mpid)),
				?assertEqual(false, check_conditions([{available_agents, '=', 3}], 0, "testqueue", Mpid)),
				
				agent:stop(Available1),
				agent:stop(Available2),
				agent:stop(Notavailable)
			end}
		end,
		fun({Qpid, _Mpid}) ->
			{"Check the queue postiion",
			fun() ->
				{ok, First} = dummy_media:start("first"),
				{ok, Second} = dummy_media:start("second"),
				{ok, Third} = dummy_media:start("third"),
				
				call_queue:add(Qpid, First),
				call_queue:add(Qpid, Second),
				call_queue:add(Qpid, Third),
				
				?assertEqual(false, check_conditions([{queue_position, '>', 1}], 0, "testqueue", First)),
				?assertEqual(true, check_conditions([{queue_position, '>', 1}], 0, "testqueue", Second)),
				?assertEqual(true, check_conditions([{queue_position, '>', 1}], 0, "testqueue", Third)),
				
				?assertEqual(true, check_conditions([{queue_position, '<', 2}], 0, "testqueue", First)),
				?assertEqual(false, check_conditions([{queue_position, '<', 2}], 0, "testqueue", Second)),
				?assertEqual(false, check_conditions([{queue_position, '<', 2}], 0, "testqueue", Third)),
				
				?assertEqual(false, check_conditions([{queue_position, '=', 2}], 0, "testqueue", First)),
				?assertEqual(true, check_conditions([{queue_position, '=', 2}], 0, "testqueue", Second)),
				?assertEqual(false, check_conditions([{queue_position, '=', 2}], 0, "testqueue", Third)),
				
				dummy_media:stop(First),
				dummy_media:stop(Second),
				dummy_media:stop(Third)
			end}
		end
	]}.

%
%-spec(check_conditions/4 :: (Conditions :: [recipe_condition()], Ticked :: non_neg_integer(), Queue :: string(), Call :: pid()) -> 'true' | 'false').
%
%
%-type(recipe_condition() ::
%	{'ticks', pos_integer()} |
%	{'eligible_agents', recipe_comparison(), non_neg_integer()} |
%	{'available_agents', recipe_comparison(), non_neg_integer()} |
%	{'queue_position', recipe_comparison(), non_neg_integer()} |
%	{'calls_queued', recipe_comparison(), non_neg_integer()}).
%







agent_interaction_test_() ->
	{foreach,
	fun() ->
		test_primer(),
		queue_manager:start([node()]),
		{ok, QPid} = queue_manager:add_queue("testqueue"),
		{ok, MPid} = dummy_media:start("testcall"),
		dispatch_manager:start(),
		agent_auth:start(),
		agent_manager:start([node()]),
		{ok, APid} = agent_manager:start_agent(#agent{login = "testagent"}),
		{QPid, MPid, APid}
	end,
	fun({QPid, MPid, APid}) ->
		dummy_media:stop(MPid),
		try call_queue:stop(QPid)
		catch
			exit:{noproc, Detail} ->
				?debugFmt("caught exit:~p ; some tests will kill the original call_queue process.", [Detail])
		end,
		agent_auth:stop(),
		queue_manager:stop(),
		dispatch_manager:stop(),
		agent:stop(APid),
		agent_manager:stop()
	end,
	[
		fun({QPid, MPid, APid}) ->
			{"Ring to an agent",
			fun() ->
				agent:set_state(APid, idle),
				call_queue:add(QPid, MPid),
				receive
				after ?TICK_LENGTH * 2 + 100 ->
					ok
				end,
				{ok, Statename} = agent:query_state(APid),
				?assertEqual(ringing, Statename)
			end}
		end,
		fun({QPid, MPid, APid}) ->
			{"Ring to an agent, which rings out, and rings to another agent",
			fun() ->
				{ok, APid2} = agent_manager:start_agent(#agent{login = "testagent2"}),
				agent:set_state(APid, idle),
				agent:set_state(APid2, idle),
				call_queue:add(QPid, MPid),
				receive
				after ?TICK_LENGTH + 100 ->
					ok
				end,
				?CONSOLE("test time!",[]),
				{ok, Statename} = agent:query_state(APid),
				{ok, Statename2} = agent:query_state(APid2),
				?assertEqual(ringing, Statename),
				?assertEqual(idle, Statename2),
				% we wait 2 extra ticks because a ring does occur until a tick.
				% Call enters q:  tick_length
				% 1st tick, 2nd tick...RINGOUT'th tick
				% dispather unbinds, tests new agent
				% ringout'th + 1 tick, rings to an agent
				receive
				after ?TICK_LENGTH * (?RINGOUT + 3) + 100 ->
					ok
				end,
				?CONSOLE("2nd test time!", []),
				{ok, Statename3} = agent:query_state(APid),
				{ok, Statename4} = agent:query_state(APid2),
				?assertEqual(idle, Statename3),
				?assertEqual(ringing, Statename4),
				agent:stop(APid2)
			end}
		end,
		fun({QPid, _MPid, APid}) ->
			{"Media says the the ring to the agent is invalid.",
			fun() ->
				{ok, Media} = dummy_media:start("testcall"),
				dummy_media:set_mode(Media, ring_agent, fail),
				call_queue:add(QPid, Media),
				agent:set_state(APid, idle),
				receive
				after ?TICK_LENGTH * 3 + 100 ->
					ok
				end,
				{ok, Statename} = agent:query_state(APid),
				?assertEqual(idle, Statename),
				dummy_media:stop(Media)
			end}
		end,
		fun({QPid, _MPid, APid}) ->
			{"Agent cannot take the call in queue (regrab)",
			fun() ->
				{ok, Media} = dummy_media:start("testcall"),
				dummy_media:set_skills(Media, [german]),
				?CONSOLE("Media response to getting call:  ~p", [gen_server:call(Media, get_call)]),
				call_queue:add(QPid, Media),
				agent:set_state(APid, idle),
				receive
				after ?TICK_LENGTH * 2 + 100 ->
					ok
				end,
				{ok, Statename} = agent:query_state(APid),
				?assertEqual(idle, Statename),
				dummy_media:stop(Media)
			end}
		end,
		fun({QPid, MPid, _APid}) ->
			{"Agent with the _all skill overrides other skill checking",
			fun() ->
				dummy_media:set_skills(MPid, [german]),
				call_queue:add(QPid, MPid),
				{ok, APid2} = agent_manager:start_agent(#agent{login = "testagent2", skills=[english, '_all']}),
				agent:set_state(APid2, idle),
				timer:sleep(?TICK_LENGTH * 2 + 100),
				{ok, Statename} = agent:query_state(APid2),
				?assertEqual(ringing, Statename),
				agent:stop(APid2)
			end}
		end,
		fun({QPid, MPid, APid}) ->
			{"Call with the _all skill overrides other skill checking",
			fun() ->
				dummy_media:set_skills(MPid, [german, '_all']),
				call_queue:add(QPid, MPid),
				agent:set_state(APid, idle),
				timer:sleep(?TICK_LENGTH * 2 + 100),
				{ok, Statename} = agent:query_state(APid),
				?assertEqual(ringing, Statename)
			end}
		end

	]
	}.

multinode_test_() ->
	{
		foreach,
		fun() ->
			["testpx", Host] = string:tokens(atom_to_list(node()), "@"),
			Master = list_to_atom(lists:append("master@", Host)),
			Slave = list_to_atom(lists:append("slave@", Host)),
			slave:start(net_adm:localhost(), master, " -pa debug_ebin"),
			slave:start(net_adm:localhost(), slave, " -pa debug_ebin"),
			
			mnesia:stop(),
			
			mnesia:change_config(extra_db_nodes, [Master, Slave]),
			mnesia:delete_schema([node(), Master, Slave]),
			mnesia:create_schema([node(), Master, Slave]),
			
			cover:start([Master, Slave]),
			
			rpc:call(Master, mnesia, start, []),
			rpc:call(Slave, mnesia, start, []),
			mnesia:start(),
			
			mnesia:change_table_copy_type(schema, Master, disc_copies),
			mnesia:change_table_copy_type(schema, Slave, disc_copies),
			
			rpc:call(Master, queue_manager, start, [[Master, Slave]]),
			rpc:call(Slave, queue_manager, start, [[Master, Slave]]),
			
			{Master, Slave}
		end,
		fun({Master, Slave}) ->
			cover:stop([Master, Slave]),
			
			slave:stop(Master),
			slave:stop(Slave),
			mnesia:stop(),
			mnesia:delete_schema([node(), Master, Slave]),
			ok
		end,
		[
			fun({Master, Slave}) ->
				{"Media goes into a queue on a different node",
				fun() ->
					?CONSOLE("cook multi 1", []),
					{ok, Media} = rpc:call(Slave, dummy_media, start, ["testcall"]),
					?assert(node(Media) =:= Slave),
					QPid = rpc:call(Master, queue_manager, get_queue, ["default_queue"]),
					?CONSOLE("das pid:  ~p", [QPid]),
					?assert(is_pid(QPid)),
					%QPid = queue_manager:get_queue("default_queue"),
					call_queue:set_recipe(QPid, [{[{ticks, 1}], prioritize, [], run_many}]),
					call_queue:add(QPid, Media),
					receive
					after ?TICK_LENGTH * 2 + 100 ->
						ok
					end,
					{{Priority, _Time}, _Mediarec} = call_queue:get_call(QPid, Media),
					?assertEqual(3, Priority)
				end}
			end
		]
	}.




	
-define(MYSERVERFUNC, fun() -> {ok, Dummy} = dummy_media:start("testcall"), {ok, Pid} = start(Dummy,[{[{ticks, 1}], set_priority, 5, run_once}], "testqueue"), {Pid, fun() -> stop(Pid) end} end).

-include("gen_server_test.hrl").


-endif.

