%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%%
%% The Original Code is Spice Telphony.
%%
%% The Initial Developer of the Original Code is
%% Andrew Thompson and Micah Warren.
%% Portions created by the Initial Developers are Copyright (C)
%% SpiceCSM. All Rights Reserved.

%% Contributor(s):

%% Andrew Thompson <athompson at spicecsm dot com>
%% Micah Warren <mwarren at spicecsm dot com>
%%

%% @doc The cook is a process that is spawned per call in queue, it
%% executes the queue's 'recipe' on the call and handles call delivery to an
%% agent. When it finds one or more dispatchers bound to its call it requests
%% that each dispatcher generate a list of local agents matching the call's
%% criteria and selects the best one to offer it to.
-module(cook).
-author("Micah").

-behaviour(gen_server).

-include("call.hrl").
-include("agent.hrl").
-include("queue.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-define(TICK_LENGTH, 500).
-else.
-define(TICK_LENGTH, 10000).
-endif.

-define(RINGOUT, 4).

%% API
-export([start_link/3, start/3, stop/1, restart_tick/1, stop_tick/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {
		recipe = [] :: recipe(),
		ticked = 0 :: integer(), % number of ticks we've done
		call :: pid() | 'undefined',
		queue :: atom() | 'undefined',
		continue = true :: bool(),
		ringingto :: pid(),
		ringcount = 0 :: non_neg_integer(),
		tref :: any() % timer reference
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Starts a cook linked to the parent process for `Call' processed by `Recipe' for call_queue named `Queue'.
-spec(start_link/3 :: (Call :: pid(), Recipe :: recipe(), Queue :: atom()) -> {'ok', pid()}).
start_link(Call, Recipe, Queue) when is_pid(Call) ->
    gen_server:start_link(?MODULE, [Call, Recipe, Queue], []).

%% @doc Starts a cook not linked to the parent process for `Call' processed by `Recipe' for call_queue named `Queue'.
-spec(start/3 :: (Call :: pid(), Recipe :: recipe(), Queue :: atom()) -> {'ok', pid()}).
start(Call, Recipe, Queue) when is_pid(Call) ->
	gen_server:start(?MODULE, [Call, Recipe, Queue], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init([Call, Recipe, Queue]) ->
	% TODO check for a call right away.
	?CONSOLE("Queue:  ~p", [Queue]),
	process_flag(trap_exit, true),
	{ok, Tref} = timer:send_interval(?TICK_LENGTH, do_tick),
	State = #state{ticked=0, recipe=Recipe, call=Call, queue=Queue, tref=Tref},
    {ok, State}.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call({stop, Reason}, From, State) -> 
	?CONSOLE("Stop requested from ~p for ~p.", [From, Reason]),
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
	%% TODO - actually tell the backend to stop ringing if it's an outband ring
	{noreply, State#state{ringingto=undefined}};
handle_cast(remove_from_queue, State) ->
	Qpid = queue_manager:get_queue(State#state.queue),
	call_queue:remove(Qpid, State#state.call),
	{noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info(do_tick, State) ->
	case whereis(queue_manager) of
		undefined ->
			{stop, queue_manager_undefined, State};
		_Else ->
			case queue_manager:get_queue(State#state.queue) of
				undefined ->
					{stop, {queue_undefined, State#state.queue}, State};
				_Other ->
					?CONSOLE("Doing tick ~p", [State]),
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
	?CONSOLE("received random info message: ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @private
terminate(normal, _State) ->
	?CONSOLE("normal death", []),
	ok;
terminate(shutdown, _State) ->
	?CONSOLE("shutdown death", []),
	ok;
terminate({normal, Reason}, _State) ->
	?CONSOLE("An inelegant shutdown requested for ~p", [Reason]),
	ok;
terminate(Reason, State) ->
	?CONSOLE("Unusual death:  ~p", [Reason]),
	timer:cancel(State#state.tref),
	Qpid = wait_for_queue(State#state.queue),
	?CONSOLE("Looks like the queue recovered, dieing now",[]),
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

wait_for_queue(Qname) ->
	case whereis(queue_manager) of
		undefined ->
			?CONSOLE("Waiting on the queue manager", []),
			receive
			after 1000 ->
				ok
			end;
		QMPid when is_pid(QMPid) ->
			?CONSOLE("Queue manager is available", []),
			ok
	end,
	case queue_manager:get_queue(Qname) of
		undefined ->
			?CONSOLE("Waiting on queue ~p" , [Qname]),
			receive
			after 300 ->
				ok
			end,
			wait_for_queue(Qname);
		Qpid when is_pid(Qpid) ->
			?CONSOLE("Queue ~p is back up", [Qname]),
			Qpid
	end.

stop(Pid) ->
	gen_server:call(Pid, stop).

%% @private
-spec(do_route/4 :: (Ringcount :: non_neg_integer(), Queue :: atom(), 'undefined' | pid(), pid()) -> 'nocall' | {'ringing', pid(), non_neg_integer()} | 'rangout').
do_route(Ringcount, _Queue, Agentpid, Callpid) when is_pid(Agentpid), Ringcount =< ?RINGOUT, is_pid(Callpid) ->
	?CONSOLE("still ringing: ~p of ~p times", [Ringcount, ?RINGOUT]),
	{ringing, Agentpid, Ringcount + 1};
do_route(Ringcount, _Queue, Agentpid, Callpid) when is_pid(Agentpid), Ringcount > ?RINGOUT, is_pid(Callpid)  -> 
	?CONSOLE("rang out",[]),
	agent:set_state(Agentpid, idle),
	rangout;
do_route(_Ringcount, Queue, undefined, Callpid) when is_pid(Callpid), is_atom(Queue) -> 
	?CONSOLE("Searching for agent to ring to...",[]),
	Qpid = queue_manager:get_queue(Queue),
	case call_queue:get_call(Qpid, Callpid) of
		{_Key, Call} ->
			Dispatchers = Call#queued_call.dispatchers,
			Agents = sort_agent_list(Dispatchers),
			?CONSOLE("Agents to ring to ~p", [Agents]),
			case offer_call(Agents, Call) of
				none -> 
					rangout;
				Apid when is_pid(Apid) -> 
					{ringing, Apid, 0}
			end;
		none -> 
			?CONSOLE("No agent to ring to",[]),
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
				?CONSOLE("empty list, might as well tell this dispatcher to regrab", []),
				dispatcher:regrab(Dpid),
				[];
			Ag ->
				Ag
		catch
			_:_ ->
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
				[15] % TODO macro the magic number
		end,
		Askills <- [length(AState#agent.skills)],
		Aidle <- [element(2, AState#agent.lastchangetimestamp)]]),
	Agents3.


%% @private
-spec(offer_call/2 :: (Agents :: [{non_neg_integer, pid()}], Call :: #queued_call{}) -> 'none' | pid()).
offer_call([], _Call) ->
	none;
offer_call([{_ACost, Apid} | Tail], Call) ->
	case gen_server:call(Call#queued_call.media, {ring_agent, Apid, Call}) of
		ok ->
			?CONSOLE("cook offering call:  ~p to ~p", [Call, Apid]),
			Apid;
		invalid ->
			offer_call(Tail, Call)
	end.

%% @private
-spec(do_recipe/4 :: (Recipe :: recipe(), Ticked :: non_neg_integer(), Queuename :: atom(), Call :: pid()) -> recipe()).
do_recipe([], _Ticked, _Queuename, _Call) ->
	[];
do_recipe([{Ticks, Op, Args, Runs} | Recipe], Ticked, Queuename, Call) when (Ticked rem Ticks) == 0, is_atom(Queuename), is_pid(Call) ->
	Doneop = do_operation({Ticks, Op, Args, Runs}, Queuename, Call),
	case Doneop of
		% TODO {T, O, A, R}?
		{T, O, A, R} when Runs =:= run_once ->
			%add to the output recipe
			[{T, O, A, R} | do_recipe(Recipe, Ticked, Queuename, Call)];
		{T, O, A, R} when Runs =:= run_many ->
			lists:append([{T, O, A, R}, {Ticks, Op, Args, Runs}], do_recipe(Recipe, Ticked, Queuename, Call));
		ok when Runs =:= run_many ->
			[{Ticks, Op, Args, Runs} | do_recipe(Recipe, Ticked, Queuename, Call)];
		ok when Runs =:= run_once ->
			do_recipe(Recipe, Ticked, Queuename, Call)
			% don't, just dance.
	end;
do_recipe([Head | Recipe], Ticked, Queuename, Call) when is_atom(Queuename), is_pid(Call) ->
	% this is here in case a recipe is not due to be run.
	[Head | do_recipe(Recipe, Ticked, Queuename, Call)].

%% @private
-spec(do_operation/3 :: (Recipe :: recipe_step(), Queuename :: atom(), Callid :: pid()) -> 'ok' | recipe_step()).
do_operation({_Ticks, Op, Args, _Runs}, Queuename, Callid) when is_atom(Queuename), is_pid(Callid) ->
	?CONSOLE("do_opertion ~p", [Op]),
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
					?CONSOLE("voicemail successfully, removing from queue", []),
					call_queue:bgremove(Pid, Callid);
				invalid ->
					?CONSOLE("voicemail failed.", []),
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
			{ok, Pid} = queue_manager:add_queue(testqueue),
			{ok, Dummy} = dummy_media:start(#call{id="testcall", skills=[english, testskill]}),
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
				{exists, Pid} = queue_manager:add_queue(testqueue),
				call_queue:set_recipe(Pid, [{1, add_skills, [newskill1, newskill2], run_once}]),
				call_queue:add(Pid, whereis(media_dummy)),
				receive
				after ?TICK_LENGTH + 2000 ->
					true
				end,
				{_Key, #queued_call{skills=CallSkills}} = call_queue:ask(Pid),
				?assertEqual(lists:sort([english, testskill, newskill1, newskill2, node()]), lists:sort(CallSkills))
			end},
			{"remove skills once",
			fun() ->
				{exists, Pid} = queue_manager:add_queue(testqueue),
				call_queue:set_recipe(Pid, [{1, remove_skills, [testskill], run_once}]),
				call_queue:add(Pid, whereis(media_dummy)),
				receive
				after ?TICK_LENGTH + 2000 ->
					true
				end,
				{_Key, #queued_call{skills=CallSkills}} = call_queue:ask(Pid),
				?assertEqual([node(), english], CallSkills)
			end},
			{"Set Priority once",
			fun() ->
				{exists, Pid} = queue_manager:add_queue(testqueue),
				call_queue:set_recipe(Pid, [{1, set_priority, [5], run_once}]),
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
				{exists, Pid} = queue_manager:add_queue(testqueue),
				call_queue:set_recipe(Pid, [{1, prioritize, [], run_once}]),
				{ok, Dummy1} = dummy_media:start(#call{id="C1"}),
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
				{exists, Pid} = queue_manager:add_queue(testqueue),
				call_queue:set_recipe(Pid, [{1, prioritize, [], run_many}]),
				{ok, Dummy1} = dummy_media:start(#call{id="C1"}),
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
				{exists, Pid} = queue_manager:add_queue(testqueue),
				call_queue:set_recipe(Pid, [{1, deprioritize, [], run_once}]),
				{ok, Dummy1} = dummy_media:start(#call{id="C1"}),
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
				{exists, Pid} = queue_manager:add_queue(testqueue),
				call_queue:set_recipe(Pid, [{1, deprioritize, [], run_many}]),
				{ok, Dummy1} = dummy_media:start(#call{id="C1"}),
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
				{exists, Pid} = queue_manager:add_queue(testqueue),
				call_queue:set_recipe(Pid, [{1, deprioritize, [], run_many}]),
				{ok, Dummy1} = dummy_media:start(#call{id="C1"}),
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
				{exists, Pid} = queue_manager:add_queue(testqueue),
				call_queue:set_recipe(Pid, [{1, add_recipe, [1, prioritize, [], run_once], run_once}]),
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
				{exists, Pid} = queue_manager:add_queue(testqueue),
				Subrecipe = [1, prioritize, [], run_many],
				call_queue:set_recipe(Pid, [{1, add_recipe, Subrecipe, run_many}]),
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
				{exists, Pid} = queue_manager:add_queue(testqueue),
				call_queue:set_recipe(Pid, [{1, voicemail, [], run_once}]),
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
				{exists, Pid} = queue_manager:add_queue(testqueue),
				call_queue:set_recipe(Pid, [{1, voicemail, [], run_once}]),
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
				{exists, Pid} = queue_manager:add_queue(testqueue),
				call_queue:set_recipe(Pid, [{1, announce, ["random data"], run_once}]),
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
				{exists, Pid} = queue_manager:add_queue(testqueue),
				call_queue:set_recipe(Pid, [{2, prioritize, [], run_many}]),
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
				call_queue_config:new_queue(testqueue, {recipe, [{1, add_skills, [newskill1, newskill2], run_once}]}),
				{exists, Pid} = queue_manager:add_queue(testqueue),
				call_queue:add(Pid, whereis(media_dummy)),
				{_Pri, _CallRec} = call_queue:ask(Pid),
				?assertEqual(1, call_queue:call_count(Pid)),
				gen_server:call(Pid, {stop, testy}),
				?assert(is_process_alive(Pid) =:= false),
				receive
				after 1000 ->
					ok
				end,
				NewPid = queue_manager:get_queue(testqueue),
				?CONSOLE("The calls:  ~p", [call_queue:print(NewPid)]),
				?assertEqual(1, call_queue:call_count(NewPid)),
				call_queue:stop(NewPid),
				call_queue_config:destroy(testqueue)
			end
			},
			{"Queue Manager dies",
			fun() ->
				call_queue_config:new_queue(testqueue, {recipe, [{1, add_skills, [newskill1, newskill2], run_once}]}),
				{exists, Pid} = queue_manager:add_queue(testqueue),
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
				NewPid = queue_manager:get_queue(testqueue),

				?assertEqual(1, call_queue:call_count(NewPid)),
				call_queue:stop(NewPid),
				call_queue_config:destroy(testqueue)
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
		{ok, Pid} = queue_manager:add_queue(testqueue),
		{ok, Dummy} = dummy_media:start(#call{id="testcall", skills=[english, testskill]}),
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
				call_queue:set_recipe(Pid, [{1, prioritize, [], run_many}]),
				call_queue:add(Pid, Dummy),
				{_Pri, #queued_call{cook = Cookpid}} = call_queue:ask(Pid),
				stop_tick(Cookpid),
				receive
				after ?TICK_LENGTH * 3 + 100 ->
					ok
				end,
				{{Priority, _Time}, _Callrec} = call_queue:ask(Pid),
				?assertEqual(Priority, 1)
			end}
		end,
		fun({Pid, Dummy}) ->
			{"Restart Tick Test",
			fun() ->
				call_queue:set_recipe(Pid, [{1, prioritize, [], run_many}]),
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

agent_interaction_test_() ->
	{foreach,
	fun() ->
		test_primer(),
		queue_manager:start([node()]),
		{ok, QPid} = queue_manager:add_queue(testqueue),
		{ok, MPid} = dummy_media:start(#call{id="testcall", skills=[english, testskill]}),
		{ok, APid} = agent:start(#agent{login = "testagent"}),
		{QPid, MPid, APid}
	end,
	fun({QPid, MPid, APid}) ->
		dummy_media:stop(MPid),
		try call_queue:stop(QPid)
		catch
			exit:{noproc, Detail} ->
				?debugFmt("caught exit:~p ; some tests will kill the original call_queue process.", [Detail])
		end,
		queue_manager:stop(),
		agent:stop(APid)
	end,
	[
		fun({QPid, MPid, APid}) ->
			{"Ring to an agent",
			fun() ->
				?assert(true)
			end}
		end
	]
	}.







	
-define(MYSERVERFUNC, fun() -> {ok, Dummy} = dummy_media:start(#call{}), {ok, Pid} = start(Dummy,[{1, set_priority, [5], run_once}], testqueue), {Pid, fun() -> stop(Pid) end} end).

-include("gen_server_test.hrl").


-endif.

