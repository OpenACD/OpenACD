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

-define(TICK_LENGTH, 500).
-define(RINGOUT, 3).

-else.

-define(TICK_LENGTH, 500).
-define(RINGOUT, 60).

-endif.

-define(DEFAULT_PATHCOST, 15).

%% API
-export([
	start_link/4,
	start/4,
	stop/1,
	restart_tick/1,
	stop_tick/1,
	start_at/5
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-type(call_key() :: {pos_integer(), {pos_integer(), pos_integer(), pos_integer()}}).
-record(state, {
		recipe = [] :: recipe(),
		ticked = 1 :: pos_integer(), % number of ticks we've done
		call :: pid() | 'undefined',
		queue :: string() | 'undefined',
		key :: call_key(),
		ringstate = none :: 'none' | 'ringing',
		tref :: any() % timer reference
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================

%% @doc Starts a cook linked to the parent process for `Call' processed by `Recipe' for call_queue named `Queue'.
-spec(start_link/4 :: (Call :: pid(), Recipe :: recipe(), Queue :: string(), Key :: call_key()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start_link(Call, Recipe, Queue, Key) when is_pid(Call) ->
    gen_server:start_link(?MODULE, [Call, Recipe, Queue, Key], []).

%% @doc Starts a cook not linked to the parent process for `Call' processed by `Recipe' for call_queue named `Queue'.
-spec(start/4 :: (Call :: pid(), Recipe :: recipe(), Queue :: string(), Key :: call_key()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start(Call, Recipe, Queue, Key) when is_pid(Call) ->
	gen_server:start(?MODULE, [Call, Recipe, Queue, Key], []).

%% @doc starts a new cook on the give `node()' `Node' for `Call' to be process by `Recipe' for the call_queue named `Queue'.
%% This is used in place of start and start_link to allow a queue on a different node to start the cook on the same node
%% the media exists on.
-spec(start_at/5 :: (Node :: atom(), Call :: pid(), Recipe :: recipe(), Queue :: string(), Key :: call_key()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start_at(Node, Call, Recipe, Queue, Key) ->
	F = fun() ->
		{ok, State} = init([Call, Recipe, Queue, Key]),
		?DEBUG("about to enter loop", []),
		gen_server:enter_loop(?MODULE, [], State)%;
	end,
	{ok, proc_lib:spawn_link(Node, F)}.
	
%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init([Call, Recipe, Queue, Key]) ->
	?DEBUG("Cook starting for call ~p from queue ~p", [Call, Queue]),
	?DEBUG("node check.  self:  ~p;  call:  ~p", [node(self()), node(Call)]),
	process_flag(trap_exit, true),
	Tref = erlang:send_after(?TICK_LENGTH, self(), do_tick),
	State = #state{recipe=Recipe, call=Call, queue=Queue, tref=Tref, key = Key},
	{ok, State}.

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
	case do_route(State#state.ringstate, State#state.queue, State#state.call) of
		nocall -> 
			{stop, {call_not_queued, State#state.call}, State};
		Ringstate -> 
			State2 = State#state{ringstate = Ringstate},
			Qpid = queue_manager:get_queue(State2#state.queue),
			NewRecipe = do_recipe(State2#state.recipe, State2#state.ticked, Qpid, State2#state.call),
			State3 = State2#state{ticked = State2#state.ticked + 1, recipe = NewRecipe},
			Tref = erlang:send_after(?TICK_LENGTH, self(), do_tick),
			{noreply, State3#state{tref=Tref}}
	end;
handle_cast(stop_ringing, State) ->
	{noreply, State#state{ringstate = none}};
handle_cast(stop_tick, State) ->
	erlang:cancel_timer(State#state.tref),
	{noreply, State#state{tref=undefined}};
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
	%?DEBUG("do_tick caught, beginning processing...", []),
	case whereis(queue_manager) of % TODO do we even need this?  We do have a terminate that should catch a no-proc.
		undefined ->
			{stop, queue_manager_undefined, State};
		_Else ->
			case queue_manager:get_queue(State#state.queue) of
				undefined ->
					{stop, {queue_undefined, State#state.queue}, State};
				Qpid ->
					case do_route(State#state.ringstate, State#state.queue, State#state.call) of
						nocall -> 
							{stop, {call_not_queued, State#state.call}, State};
						Ringstate -> 
							State2 = State#state{ringstate = Ringstate},
							NewRecipe = do_recipe(State2#state.recipe, State2#state.ticked, Qpid, State2#state.call),
							Tref = erlang:send_after(?TICK_LENGTH, self(), do_tick),
							State3 = State2#state{ticked = State2#state.ticked + 1, recipe = NewRecipe, tref = Tref},
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
	erlang:cancel_timer(State#state.tref),
	Qpid = wait_for_queue(State#state.queue),
	?INFO("Looks like the queue recovered (~w), dieing now",[Qpid]),
	call_queue:add_at(Qpid, State#state.key, State#state.call),
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
-spec(restart_tick/1 :: (Pid :: pid()) -> 'ok').
restart_tick(Pid) ->
	gen_server:cast(Pid, restart_tick).

%% @doc Pause the cook running at `Pid'.
-spec(stop_tick/1 :: (Pid :: pid()) -> 'ok').
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
-spec(stop/1 :: (Pid :: pid()) -> 'ok').
stop(Pid) ->
	gen_server:call(Pid, stop).

%% @private
-spec(do_route/3 :: (Ringstate :: 'ringing' | 'none', Queue :: string(), Callpid :: pid()) -> 'nocall' | 'ringing' | 'none').
do_route(ringing, _Queue, _Callpid) ->
	?DEBUG("still ringing", []),
	ringing;
do_route(none, Queue, Callpid) ->
	%?DEBUG("Searching for agent to ring to...",[]),
	Qpid = queue_manager:get_queue(Queue),
	case call_queue:get_call(Qpid, Callpid) of
		{_Key, Call} ->
			Dispatchers = Call#queued_call.dispatchers,
			Agents = sort_agent_list(Dispatchers),
			%?DEBUG("Dispatchers:  ~p; Agents:  ~p", [Dispatchers, Agents]),
			offer_call(Agents, Call);
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
		Aidle <- [AState#agent.lastchange]]),
	Agents3.


%% @private
-spec(offer_call/2 :: (Agents :: [{non_neg_integer, pid()}], Call :: #queued_call{}) -> 'none' | 'ringing').
offer_call([], _Call) ->
	%?DEBUG("No valid agents found", []),
	none;
offer_call([{_ACost, Apid} | Tail], Call) ->
	case gen_media:ring(Call#queued_call.media, Apid, Call, ?TICK_LENGTH * ?RINGOUT) of
		ok ->
			Agent = agent:dump_state(Apid),
			Callrec = gen_media:get_call(Call#queued_call.media),
			?INFO("cook offering call:  ~p to ~p", [Callrec#call.id, Agent#agent.login]),
			ringing;
		invalid ->
			offer_call(Tail, Call)
	end.

%% @private
-spec(check_conditions/4 :: (Conditions :: [recipe_condition()], Ticked :: non_neg_integer(), Qpid :: pid(), Call :: pid()) -> 'true' | 'false').
check_conditions([], _Ticked, _Qpid, _Call) ->
	true;
check_conditions([{ticks, Ticks} | Conditions], Ticked, Qpid, Call) when Ticked > 0 andalso (Ticked rem Ticks) == 0 ->
	check_conditions(Conditions, Ticked, Qpid, Call);
check_conditions([{ticks, _Ticks} | _Conditions], _Ticked, _Qpid, _Call) ->
	false;
check_conditions([{available_agents, Comparision, Number} | Conditions], Ticked, Qpid, Call) ->
	{_Key, Callrec} = call_queue:get_call(Qpid, Call),
	L = agent_manager:find_avail_agents_by_skill(Callrec#queued_call.skills),
	Agents = length(L),
	case Comparision of
		'>' when Agents > Number ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		'<' when Agents < Number ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		'=' when Agents =:= Number ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		_Else ->
			false
	end;
check_conditions([{eligible_agents, Comparision, Number} | Conditions], Ticked, Qpid, Call) ->
	{_Key, Callrec} = call_queue:get_call(Qpid, Call),
	L = agent_manager:find_by_skill(Callrec#queued_call.skills),
	Agents = length(L),
	?DEBUG("Number: ~p; agents: ~p", [Number, Agents]),
	case Comparision of
		'>' when Agents > Number ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		'<' when Agents < Number ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		'=' when Number =:= Agents ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		_Else ->
			false
	end;
check_conditions([{calls_queued, Comparision, Number} | Conditions], Ticked, Qpid, Call) ->
	Count = call_queue:call_count(Qpid),
	case Comparision of
		'>' when Count > Number ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		'<' when Count < Number ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		'=' when Number =:= Count ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		_Else ->
			false
	end;
check_conditions([{queue_position, Comparision, Number} | Conditions], Ticked, Qpid, Call) ->
	Calls = call_queue:to_list(Qpid),
	Test = fun(Needle, #queued_call{media = Mpid}) ->
		Needle =:= Mpid
	end,
	Position = util:list_index(Test, Call, Calls),
	case Comparision of
		'>' when Position > Number ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		'<' when Position < Number, Position > 0 ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		'=' when Position =:= Number ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		_Else ->
			false
	end;
check_conditions([{client, Comparision, ClientID} | Conditions], Ticked, Qpid, Call) ->
	Callrec = gen_media:get_call(Call),
	Client = Callrec#call.client,
	case Comparision of
		'=' when Client#client.id == ClientID ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		'!=' when Client#client.id =/= ClientID ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		_Else ->
			false
	end;
check_conditions([{hour, Comparision, CHour} | Conditions], Ticked, Qpid, Call) ->
	{_, {Hour, _, _}} = calendar:local_time(),
	case Comparision of
		'>' when Hour > CHour ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		'<' when Hour < CHour ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		'=' when Hour == CHour ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		_Else ->
			false
	end;
check_conditions([{weekday, Comparision, CDay} | Conditions], Ticked, Qpid, Call) ->
	Day = calendar:day_of_the_week(calendar:local_time()),
	case Comparision of
		'>' when Day > CDay ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		'<' when Day < CDay ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		'=' when Day == CDay ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		_Else ->
			false
	end;
check_conditions([{type, Comparision, CType} | Conditions], Ticked, Qpid, Call) ->
	Callrec = gen_media:get_call(Call),
	Type = Callrec#call.type,
	case Comparision of
		'=' when Type == CType ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		'!=' when Type =/= CType ->
			check_conditions(Conditions, Ticked, Qpid, Call);
		_Else ->
			false
	end.

%% @private
-spec(do_recipe/4 :: (Recipe :: recipe(), Ticked :: non_neg_integer(), Qpid :: pid(), Call :: pid()) -> recipe()).
do_recipe([], _Ticked, _Qpid, _Call) ->
	[];
do_recipe([{Conditions, Op, Args, Runs} | Recipe], Ticked, Qpid, Call) when is_pid(Qpid), is_pid(Call) ->
	case check_conditions(Conditions, Ticked, Qpid, Call) of
		true ->
			Doneop = do_operation({Conditions, Op, Args, Runs}, Qpid, Call),
			case Doneop of
				{Newconds, Newop, Newargs, Newruns} when Runs =:= run_once ->
					%add to the output recipe
					[{Newconds, Newop, Newargs, Newruns} | do_recipe(Recipe, Ticked, Qpid, Call)];
				{Newconds, Newop, Newargs, Newruns} when Runs =:= run_many ->
					lists:append([{Newconds, Newop, Newargs, Newruns}, {Conditions, Op, Args, Runs}], do_recipe(Recipe, Ticked, Qpid, Call));
				ok when Runs =:= run_many ->
					[{Conditions, Op, Args, Runs} | do_recipe(Recipe, Ticked, Qpid, Call)];
				ok when Runs =:= run_once ->
					do_recipe(Recipe, Ticked, Qpid, Call)
					% don't, just dance.
			end;
		false ->
			[{Conditions, Op, Args, Runs} | do_recipe(Recipe, Ticked, Qpid, Call)]
	end.

%% @private
-spec(do_operation/3 :: (Recipe :: recipe_step(), Qpid :: pid(), Callpid :: pid()) -> 'ok' | recipe_step()).
do_operation({_Conditions, Op, Args, _Runs}, Qpid, Callpid) when is_pid(Qpid), is_pid(Callpid) ->
	?INFO("do_operation ~p with args ~p", [Op, Args]),
	case Op of
		add_skills ->
			call_queue:add_skills(Qpid, Callpid, Args),
			ok;
		remove_skills ->
			call_queue:remove_skills(Qpid, Callpid, Args),
			ok;
		set_priority ->
			call_queue:set_priority(Qpid, Callpid, Args),
			ok;
		prioritize ->
			{{Prior, _Time}, _Call} = call_queue:get_call(Qpid, Callpid),
			call_queue:set_priority(Qpid, Callpid, Prior + 1),
			ok;
		deprioritize ->
			{{Prior, _Time}, _Call} = call_queue:get_call(Qpid, Callpid),
			call_queue:set_priority(Qpid, Callpid, Prior - 1),
			ok;
		voicemail ->
			case gen_media:voicemail(Callpid) of
				ok ->
					?DEBUG("voicemail successfully, removing from queue", []),
					call_queue:bgremove(Qpid, Callpid);
				invalid ->
					?WARNING("voicemail failed.", []),
					ok
			end;
		add_recipe ->
			list_to_tuple(Args);
		announce ->
			gen_media:announce(Callpid, Args),
			ok
	end.

-ifdef(EUNIT).

test_primer() ->
	["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	mnesia:start().

do_operation_test_() ->
	{foreach,
	fun() ->
		{ok, QMPid} = gen_leader_mock:start(queue_manager),
		{ok, QPid} = gen_server_mock:new(),
		{ok, Mpid} = gen_server_mock:new(),
		Assertmocks = fun() ->
			gen_leader_mock:assert_expectations(QMPid),
			gen_server_mock:assert_expectations(QPid),
			gen_server_mock:assert_expectations(Mpid)
		end,
		?CONSOLE("Start args:  ~p", [{QMPid, QPid, Mpid, Assertmocks}]),
		{QMPid, QPid, Mpid, Assertmocks}
	end,
	fun({QMPid, QPid, Mpid, _Assertmocks}) ->
		gen_server_mock:stop(QPid),
		gen_leader_mock:stop(QMPid),
		gen_server_mock:stop(Mpid),
		timer:sleep(10)
	end,
	[fun({_QMPid, QPid, Mpid, Assertmocks}) ->
		{"add skills",
		fun() ->
			gen_server_mock:expect_call(Mpid, fun('$gen_media_get_call', _From, State) -> 
				{ok, #call{id = "testcall", source = Mpid}, State}
			end),
			gen_server_mock:expect_call(QPid, fun({add_skills, "testcall", [skill1, skill2]}, _From, _State) -> ok end),
			ok = do_operation({"conditions", add_skills, [skill1, skill2], "runs"}, QPid, Mpid),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, Assertmocks}) ->
		{"remove skills",
		fun() ->
			gen_server_mock:expect_call(Mpid, fun('$gen_media_get_call', _From, State) -> 
				{ok, #call{id = "testcall", source = Mpid}, State}
			end),
			gen_server_mock:expect_call(QPid, fun({remove_skills, "testcall", [english]}, _From, _State) -> ok end),
			ok = do_operation({"conditions", remove_skills, [english], "runs"}, QPid, Mpid),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, Assertmocks}) ->
		{"set priority",
		fun() ->
			gen_server_mock:expect_call(QPid, fun({set_priority, Incpid, 5}, _From, _State) -> 
				Incpid = Mpid,
				ok
			end),
			ok = do_operation({"conditions", set_priority, 5, "runs"}, QPid, Mpid),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, Assertmocks}) ->
		{"prioritize",
		fun() ->
			gen_server_mock:expect_call(QPid, fun({get_call, Incpid}, _From, State) ->
				Incpid = Mpid,
				{ok, {{7, now()}, #call{id = "testcall", source = Mpid}}, State}
			end),
			gen_server_mock:expect_call(QPid, fun({set_priority, Incpid, 8}, _Fun, _State) -> 
				Incpid = Mpid,
				ok
			end),
			
			ok = do_operation({"conditions", prioritize, "doesn't matter", "runs"}, QPid, Mpid),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, Assertmocks}) ->
		{"deprioritize",
		fun() ->
			gen_server_mock:expect_call(QPid, fun({get_call, Incpid}, _From, State) ->
				Incpid = Mpid,
				{ok, {{27, now()}, #call{id = "testcall", source = Mpid}}, State}
			end),
			gen_server_mock:expect_call(QPid, fun({set_priority, Incpid, 26}, _Fun, _State) -> 
				Incpid = Mpid,
				ok
			end),
			ok = do_operation({"conditions", deprioritize, "doesn't matter", "runs"}, QPid, Mpid),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, Assertmocks}) ->
		{"voicemail with media that can handle it",
		fun() ->
			gen_server_mock:expect_call(Mpid, fun('$gen_media_voicemail', _From, State) -> 
				{ok, ok, State}
			end),
			gen_server_mock:expect_cast(QPid, fun({remove, Incpid}, _State) ->
				Incpid = Mpid,
				ok
			end),
			ok = do_operation({"conditions", voicemail, "doesn't matter", "runs"}, QPid, Mpid),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, Assertmocks}) ->
		{"voicemail with media that doesn't support it",
		fun() ->
			gen_server_mock:expect_call(Mpid, fun('$gen_media_voicemail', _From, State) ->
				{ok, invalid, State}
			end),
			ok = do_operation({"conditions", voicemail, "doesn't matter", "runs"}, QPid, Mpid),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, Assertmocks}) ->
		{"add recipe",
		fun() ->
			?assertEqual({1, 2, 3}, do_operation({"conditions", add_recipe, [1, 2, 3], "runs"}, QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, Assertmocks}) ->
		{"announce",
		fun() ->
			gen_server_mock:expect_call(Mpid, fun({'$gen_media_announce', "do the robot"}, _From, _State) -> ok end),
			ok = do_operation({"conditions", announce, "do the robot", "runs"}, QPid, Mpid),
			Assertmocks()
		end}
	end]}.

check_conditions_test_() ->
	[{"ticks tests",
	{foreach,
	fun() ->
		{ok, QMPid} = gen_leader_mock:start(queue_manager),
		{ok, QPid} = gen_server_mock:new(),
		{ok, Mpid} = gen_server_mock:new(),
		{ok, AMpid} = gen_leader_mock:start(agent_manager),
		Assertmocks = fun() ->
			gen_leader_mock:assert_expectations(QMPid),
			gen_server_mock:assert_expectations(QPid),
			gen_server_mock:assert_expectations(Mpid)
		end,
		?CONSOLE("Start args:  ~p", [{QMPid, QPid, Mpid, Assertmocks}]),
		{QMPid, QPid, Mpid, AMpid, Assertmocks}
	end,
	fun({QMPid, QPid, Mpid, AMpid, _Assertmocks}) ->
		gen_server_mock:stop(QPid),
		gen_leader_mock:stop(QMPid),
		gen_server_mock:stop(Mpid),
		gen_leader_mock:stop(AMpid),
		timer:sleep(10)
	end,
	[fun({_QMPid, _QPid, _Mpid, _AMpid, Assertmocks}) ->
		{"no more conditions to check",
		fun() ->
			?assert(check_conditions([], "ticked doesn't matter", "queue doesn't matter", "call doesn't matter")),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, _Mpid, _AMpid, Assertmocks}) ->
		{"checking ticks",
		fun() ->
			?assert(check_conditions([{ticks, 5}], 5, QPid, "call")),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, _Mpid, _AMpid, Assertmocks}) ->
		{"checking ticks that aren't equal, but at right frequency",
		fun() ->
			?assert(check_conditions([{ticks, 7}], 42, QPid, "call")),
			Assertmocks()
		end}
	end,
	fun({_, QPid, _, _, Assertmocks}) ->
		{"checking ticks not at the right frequency",
		fun() ->
			?assertNot(check_conditions([{ticks, 3}], 7, QPid, "call")),
			Assertmocks()
		end}
	end]}},
	{"available agents",
	{foreach,
	fun() ->
		{ok, QMPid} = gen_leader_mock:start(queue_manager),
		{ok, QPid} = gen_server_mock:new(),
		{ok, Mpid} = gen_server_mock:new(),
		{ok, AMpid} = gen_leader_mock:start(agent_manager),
		Assertmocks = fun() ->
			gen_leader_mock:assert_expectations(QMPid),
			gen_server_mock:assert_expectations(QPid),
			gen_server_mock:assert_expectations(Mpid),
			gen_leader_mock:assert_expectations(AMpid)
		end,
		?CONSOLE("Start args:  ~p", [{QMPid, QPid, Mpid, AMpid, Assertmocks}]),
		gen_server_mock:expect_call(QPid, fun({get_call, Incpid}, _From, State) ->
			?CONSOLE("get_call", []),
			Mpid = Incpid,
			{ok, {"key", #queued_call{id = "testcall", media = Mpid, skills = [english]}}, State}
		end),
		gen_leader_mock:expect_call(AMpid, fun(list_agents, _From, State, _Elec) ->
			?CONSOLE("list_agents", []),
			List = [#agent{login = "agent1", id = "agent1", skills = ['_all'], state = idle},
			#agent{login = "agent2", id = "agent2", skills = ['_all'], state = idle},
			#agent{login = "agent3", id = "agent3", skills = ['_all'], state = idle}],
			Out = lists:map(fun(Rec) ->
				{Rec#agent.login, {element(2, agent:start_link(Rec)), Rec#agent.id}}
			end, List),
			{ok, Out, State}
		end),
		{QMPid, QPid, Mpid, AMpid, Assertmocks}
	end,
	fun({QMPid, QPid, Mpid, AMpid, _Assertmocks}) ->
		gen_server_mock:stop(QPid),
		gen_leader_mock:stop(QMPid),
		gen_server_mock:stop(Mpid),
		gen_leader_mock:stop(AMpid),
		timer:sleep(10)
	end,
	[fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) -> 
		{"available agents > condition is true",
		fun() ->
			?assert(check_conditions([{available_agents, '>', 2}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"available agents > condition is false",
		fun() ->
			?assertNot(check_conditions([{available_agents, '>', 4}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"available agents = condition is true",
		fun() ->
			?assert(check_conditions([{available_agents, '=', 3}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"available agents = condition is false",
		fun() ->
			?assertNot(check_conditions([{available_agents, '=', 52}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"available agents < condition is true",
		fun() ->
			?assert(check_conditions([{available_agents, '<', 4}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"available agents < condition is false",
		fun() ->
			?assertNot(check_conditions([{available_agents, '<', 2}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end]}},
	{"eligible agents",
	{foreach,
	fun() ->
		{ok, QMPid} = gen_leader_mock:start(queue_manager),
		{ok, QPid} = gen_server_mock:new(),
		{ok, Mpid} = gen_server_mock:new(),
		{ok, AMpid} = gen_leader_mock:start(agent_manager),
		Assertmocks = fun() ->
			gen_leader_mock:assert_expectations(QMPid),
			gen_server_mock:assert_expectations(QPid),
			gen_server_mock:assert_expectations(Mpid),
			gen_leader_mock:assert_expectations(AMpid)
		end,
		?CONSOLE("Start args:  ~p", [{QMPid, QPid, Mpid, AMpid, Assertmocks}]),
		gen_server_mock:expect_call(QPid, fun({get_call, Incpid}, _From, State) ->
			?CONSOLE("get_call", []),
			Mpid = Incpid,
			{ok, {"key", #queued_call{id = "testcall", media = Mpid, skills = [english]}}, State}
		end),
		gen_leader_mock:expect_call(AMpid, fun(list_agents, _From, State, _Elec) ->
			?CONSOLE("list_agents", []),
			List = [#agent{login = "agent1", id = "agent1", skills = ['_all'], state = idle},
			#agent{login = "agent2", id = "agent2", skills = ['_all'], state = idle},
			#agent{login = "agent3", id = "agent3", skills = ['_all'], state = idle}],
			Out = lists:map(fun(Rec) ->
				{Rec#agent.login, {element(2, agent:start_link(Rec)), Rec#agent.id}}
			end, List),
			{ok, Out, State}
		end),
		{QMPid, QPid, Mpid, AMpid, Assertmocks}
	end,
	fun({QMPid, QPid, Mpid, AMpid, _Assertmocks}) ->
		gen_server_mock:stop(QPid),
		gen_leader_mock:stop(QMPid),
		gen_server_mock:stop(Mpid),
		gen_leader_mock:stop(AMpid),
		timer:sleep(10)
	end,
	[fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"eleigible > comparision is true",
		fun() ->
			?assert(check_conditions([{eligible_agents, '>', 2}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"eleigible > comparision is false",
		fun() ->
			?assertNot(check_conditions([{eligible_agents, '>', 4}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"eleigible = comparision is true",
		fun() ->
			?assert(check_conditions([{eligible_agents, '=', 3}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"eleigible = comparision is false",
		fun() ->
			?assertNot(check_conditions([{eligible_agents, '=', 4}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"eleigible < comparision is true",
		fun() ->
			?assert(check_conditions([{eligible_agents, '<', 4}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"eleigible < comparision is false",
		fun() ->
			?assertNot(check_conditions([{eligible_agents, '<', 2}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end
	]}},
	{"calls queued",
	{foreach,
	fun() ->
		{ok, QMPid} = gen_leader_mock:start(queue_manager),
		{ok, QPid} = gen_server_mock:new(),
		{ok, Mpid} = gen_server_mock:new(),
		{ok, AMpid} = gen_leader_mock:start(agent_manager),
		Assertmocks = fun() ->
			gen_leader_mock:assert_expectations(QMPid),
			gen_server_mock:assert_expectations(QPid),
			gen_server_mock:assert_expectations(Mpid),
			gen_leader_mock:assert_expectations(AMpid)
		end,
		?CONSOLE("Start args:  ~p", [{QMPid, QPid, Mpid, AMpid, Assertmocks}]),
		gen_server_mock:expect_call(QPid, fun(call_count, _From, State) ->
			{ok, 3, State}
		end),
		{QMPid, QPid, Mpid, AMpid, Assertmocks}
	end,
	fun({QMPid, QPid, Mpid, AMpid, _Assertmocks}) ->
		gen_server_mock:stop(QPid),
		gen_leader_mock:stop(QMPid),
		gen_server_mock:stop(Mpid),
		gen_leader_mock:stop(AMpid),
		timer:sleep(10)
	end,
	[fun({_QMPid, QPid, _Mpid, _AMpid, Assertmocks}) ->
		{"calls > cond is true",
		fun() ->
			?assert(check_conditions([{calls_queued, '>', 2}], "doesn't matter", QPid, "doesn't matter")),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, _Mpid, _AMpid, Assertmocks}) ->
		{"calls > cond is false",
		fun() ->
			?assertNot(check_conditions([{calls_queued, '>', 4}], "doesn't matter", QPid, "doesn't matter")),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, _Mpid, _AMpid, Assertmocks}) ->
		{"calls = cond is true",
		fun() ->
			?assert(check_conditions([{calls_queued, '=', 3}], "doesn't matter", QPid, "doesn't matter")),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, _Mpid, _AMpid, Assertmocks}) ->
		{"calls = cond is false",
		fun() ->
			?assertNot(check_conditions([{calls_queued, '=', 4}], "doesn't matter", QPid, "doesn't matter")),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, _Mpid, _AMpid, Assertmocks}) ->
		{"calls < cond is true",
		fun() ->
			?assert(check_conditions([{calls_queued, '<', 4}], "doesn't matter", QPid, "doesn't matter")),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, _Mpid, _AMpid, Assertmocks}) ->
		{"calls < cond is false",
		fun() ->
			?assertNot(check_conditions([{calls_queued, '<', 2}], "doesn't matter", QPid, "doesn't matter")),
			Assertmocks()
		end}
	end]}},
	{"queue position",
	{foreach,
	fun() ->
		{ok, QMPid} = gen_leader_mock:start(queue_manager),
		{ok, QPid} = gen_server_mock:new(),
		{ok, Mpid} = gen_server_mock:new(),
		{ok, AMpid} = gen_leader_mock:start(agent_manager),
		Assertmocks = fun() ->
			gen_leader_mock:assert_expectations(QMPid),
			gen_server_mock:assert_expectations(QPid),
			gen_server_mock:assert_expectations(Mpid),
			gen_leader_mock:assert_expectations(AMpid)
		end,
		?CONSOLE("Start args:  ~p", [{QMPid, QPid, Mpid, AMpid, Assertmocks}]),
		gen_server_mock:expect_call(QPid, fun(to_list, _From, State) ->
			Out = [#queued_call{media = undefined, id = 1},
				#queued_call{media = Mpid, id = 2},
				#queued_call{media = undefined, id = 3}],
			{ok, Out, State}
		end),
		{QMPid, QPid, Mpid, AMpid, Assertmocks}
	end,
	fun({QMPid, QPid, Mpid, AMpid, _Assertmocks}) ->
		gen_server_mock:stop(QPid),
		gen_leader_mock:stop(QMPid),
		gen_server_mock:stop(Mpid),
		gen_leader_mock:stop(AMpid),
		timer:sleep(10)
	end,
	[fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"pos > cond true",
		fun() ->
			?assert(check_conditions([{queue_position, '>', 1}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"pos > cond false",
		fun() ->
			?assertNot(check_conditions([{queue_position, '>', 3}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"pos = cond true",
		fun() ->
			?assert(check_conditions([{queue_position, '=', 2}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"pos = cond false",
		fun() ->
			?assertNot(check_conditions([{queue_position, '=', 1}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"pos < cond true",
		fun() ->
			?assert(check_conditions([{queue_position, '<', 3}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"pos < cond false",
		fun() ->
			?assertNot(check_conditions([{queue_position, '<', 1}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end]}},
	{"client comparison",
	{foreach,
	fun() ->
		{ok, QMPid} = gen_leader_mock:start(queue_manager),
		{ok, QPid} = gen_server_mock:new(),
		{ok, Mpid} = gen_server_mock:new(),
		{ok, AMpid} = gen_leader_mock:start(agent_manager),
		Assertmocks = fun() ->
			gen_leader_mock:assert_expectations(QMPid),
			gen_server_mock:assert_expectations(QPid),
			gen_server_mock:assert_expectations(Mpid),
			gen_leader_mock:assert_expectations(AMpid)
		end,
		?CONSOLE("Start args:  ~p", [{QMPid, QPid, Mpid, AMpid, Assertmocks}]),
		gen_server_mock:expect_call(Mpid, fun('$gen_media_get_call', _From, State) ->
				Client = #client{id="00010001"},
				Out = #call{id="foo", client=Client, source=Mpid},
			{ok, Out, State}
		end),
		{QMPid, QPid, Mpid, AMpid, Assertmocks}
	end,
	fun({QMPid, QPid, Mpid, AMpid, _Assertmocks}) ->
		gen_server_mock:stop(QPid),
		gen_leader_mock:stop(QMPid),
		gen_server_mock:stop(Mpid),
		gen_leader_mock:stop(AMpid),
		timer:sleep(10)
	end,
	[
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"client = cond true",
		fun() ->
			?assert(check_conditions([{client, '=', "00010001"}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"client != cond true",
		fun() ->
			?assert(check_conditions([{client, '!=', "00010002"}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"client = cond false",
		fun() ->
			?assertNot(check_conditions([{client, '=', "00010002"}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"client != cond false",
		fun() ->
			?assertNot(check_conditions([{client, '!=', "00010001"}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end]}},
	{"client comparison",
	{foreach,
	fun() ->
		{ok, QMPid} = gen_leader_mock:start(queue_manager),
		{ok, QPid} = gen_server_mock:new(),
		{ok, Mpid} = gen_server_mock:new(),
		{ok, AMpid} = gen_leader_mock:start(agent_manager),
		Assertmocks = fun() ->
			gen_leader_mock:assert_expectations(QMPid),
			gen_server_mock:assert_expectations(QPid),
			gen_server_mock:assert_expectations(Mpid),
			gen_leader_mock:assert_expectations(AMpid)
		end,
		?CONSOLE("Start args:  ~p", [{QMPid, QPid, Mpid, AMpid, Assertmocks}]),
		gen_server_mock:expect_call(Mpid, fun('$gen_media_get_call', _From, State) ->
				Out = #call{id="foo", type=voice, source=Mpid},
			{ok, Out, State}
		end),
		{QMPid, QPid, Mpid, AMpid, Assertmocks}
	end,
	fun({QMPid, QPid, Mpid, AMpid, _Assertmocks}) ->
		gen_server_mock:stop(QPid),
		gen_leader_mock:stop(QMPid),
		gen_server_mock:stop(Mpid),
		gen_leader_mock:stop(AMpid),
		timer:sleep(10)
	end,
	[fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"type = cond true",
		fun() ->
			?assert(check_conditions([{type, '=', voice}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"type != cond true",
		fun() ->
			?assert(check_conditions([{type, '!=', voicemail}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"type = cond false",
		fun() ->
			?assertNot(check_conditions([{type, '=', voicemail}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, _AMpid, Assertmocks}) ->
		{"type != cond false",
		fun() ->
			?assertNot(check_conditions([{type, '!=', voice}], "doesn't matter", QPid, Mpid)),
			Assertmocks()
		end}
	end]}}
	].

tick_manipulation_test_() ->
	{foreach,
	fun() ->
		test_primer(),
		queue_manager:start([node()]),
		{ok, Pid} = queue_manager:add_queue("testqueue", []),
		{ok, Dummy} = dummy_media:start([{id, "testcall"}, {skills, [english, testskill]}]),
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

agent_interaction_test_() ->
	{foreach,
	fun() ->
		test_primer(),
		?DEBUG("queue_manager:  ~p", [queue_manager:start([node()])]),
		{ok, QPid} = queue_manager:add_queue("testqueue", []),
		?DEBUG("call_queue:  ~p", [QPid]),
		{ok, MPid} = dummy_media:start("testcall"),
		?DEBUG("dummy_media:  ~p", [MPid]),
		?DEBUG("dispatch_manager:  ~p", [dispatch_manager:start()]),
		?DEBUG("agent_manager:  ~p", [agent_manager:start([node()])]),
		{ok, APid} = agent_manager:start_agent(#agent{login = "testagent"}),
		?DEBUG("agent:  ~p", [APid]),
		{QPid, MPid, APid}
	end,
	fun({QPid, MPid, APid}) ->
		?DEBUG("stopping dummy_media:  ~p", [dummy_media:stop(MPid)]),
		?DEBUG("stopping dispatch_manager:  ~p", [dispatch_manager:stop()]),
		try call_queue:stop(QPid)
		catch
			exit:{noproc, Detail} ->
				?debugFmt("caught exit:~p ; some tests will kill the original call_queue process.", [Detail])
		end,
		?DEBUG("stopping queue_manager:  ~p", [queue_manager:stop()]),
		?DEBUG("stopping agent:  ~p", [agent:stop(APid)]),
		?DEBUG("Stopping agent_manager:  ~p", [agent_manager:stop()])
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
				after ?TICK_LENGTH * (?RINGOUT) + 100 ->
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
				?assertEqual(released, Statename),
				dummy_media:stop(Media)
			end}
		end,
		fun({QPid, _MPid, APid}) ->
			{"Agent cannot take the call in queue (regrab)",
			fun() ->
				{ok, Media} = dummy_media:start([{id, "testcall"}, {skills, [german]}]),
				%dummy_media:set_skills(Media, [german]),
				?CONSOLE("Media response to getting call:  ~p", [gen_media:get_call(Media)]),
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
		fun({QPid, _MPid, _APid}) ->
			{"Agent with the _all skill overrides other skill checking",
			fun() ->
				{ok, MPid} = dummy_media:start([{id, "testcall"}, {skills, [german]}]),
				%dummy_media:set_skills(MPid, [german]),
				call_queue:add(QPid, MPid),
				{ok, APid2} = agent_manager:start_agent(#agent{login = "testagent2", skills=[english, '_all']}),
				agent:set_state(APid2, idle),
				timer:sleep(?TICK_LENGTH * 2 + 100),
				{ok, Statename} = agent:query_state(APid2),
				?assertEqual(ringing, Statename),
				agent:stop(APid2)
			end}
		end,
		fun({QPid, _MPid, APid}) ->
			{"Call with the _all skill overrides other skill checking",
			fun() ->
				{ok, MPid} = dummy_media:start([{id, "testcall"}, {skills, [german, '_all']}]),
				%dummy_media:set_skills(MPid, [german, '_all']),
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
			end,
			fun({Master, Slave}) ->
				{"Cook is on master, queue is on slave, the slave dies, cook re-inserts",
				fun() ->
					call_queue_config:new_queue(#call_queue{name = "testqueue"}),
					{ok, Qpid} = rpc:call(Slave, queue_manager, add_queue, ["testqueue", []]),
					{ok, Media} = rpc:call(Master, dummy_media, start, ["testcall"]),
					call_queue:add(Qpid, Media),
					slave:stop(Slave),
					timer:sleep(1000),
					Newpid = rpc:call(Master, queue_manager, get_queue, ["testqueue"]),
					?assertNot(undefined =:= Newpid),
					?assertNot(Qpid =:= Newpid),
					?assertMatch({{_Priority, _Time}, _Mediarec}, call_queue:get_call(Newpid, Media))
				end}
			end
		]
	}.




	
-define(MYSERVERFUNC, fun() -> {ok, Dummy} = dummy_media:start("testcall"), {ok, Pid} = start(Dummy,[{[{ticks, 1}], set_priority, 5, run_once}], "testqueue", {1, now()}), {Pid, fun() -> stop(Pid) end} end).

-include("gen_server_test.hrl").


-endif.

