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
%%	The Original Code is OpenACD.
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
%%	Andrew Thompson <andrew at hijacked dot us>
%%	Micah Warren <micahw at lordnull dot com>
%%

%% @doc Handles the creation and desctruction of dispatchers.
%% Locally registered on each node.
%% There is to be 1 dipatcher for every available agent on a node.
%% @see dispatcher
-module(dispatch_manager).
-author("Micah").

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

%% API
-export([
	start_link/0,
	start/0, 
	stop/0, 
	count_dispatchers/0,
	deep_inspect/0,
	request_end/2,
	cook_started/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	dispatchers = [] :: [pid()],
	monitored_items = dict:new() :: dict()
	}).
	
-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================

%% @doc start a dispatch manager linked to the calling process.
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc start a dispatch manager linked to no process.
-spec(start/0 :: () -> {'ok', pid()}).
start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop the dispatch manager with reason `normal'.
-spec(stop/0 :: () -> any()).
stop() -> 
	gen_server:call(?MODULE, stop).

-spec(count_dispatchers/0 :: () -> non_neg_integer()).
count_dispatchers() ->
	gen_server:call(?MODULE, count_dispatchers).

-spec(deep_inspect/0 :: () -> 'ok').
deep_inspect() ->
	gen_server:cast(?MODULE, deep_inspect).

%% @doc The calling process should be considered a cook when the 
%% dispatch_manager does dispatch balancing.  Returns a monitor reference to
%% the dispatch_manager process so the calling process can inform the
%% dispatch manager to start counting itself should the manager die.
-spec(cook_started/0 :: () -> reference()).
cook_started() ->
	Self = self(),
	gen_server:cast(dispatch_manager, {cook_started, Self}),
	erlang:monitor(process, whereis(dispatch_manager)).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%% @private
init([]) ->
	?DEBUG("~p starting at ~p", [?MODULE, node()]),
	process_flag(trap_exit, true),
	case whereis(agent_manager) of
		undefined ->
			{ok, #state{}};
		_Else ->
			Agents = agent_manager:list(),
			F = fun({Login, {Pid, _, Time, _}}) ->
				?DEBUG("Checking status of ~s (~p)", [Login, Pid]),
				case Time of
					0 ->
						gen_server:cast(dispatch_manager, {end_avail, Pid});
					_ ->
						gen_server:cast(dispatch_manager, {now_avail, Pid})
				end
			end,
			spawn(fun() -> 
				timer:sleep(10),
				?DEBUG("Spawn waking up with agents ~p", [Agents]),
				lists:foreach(F, Agents),
				?DEBUG("Spawn done.", [])
			end),
			{ok, #state{}}
	end.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
handle_call(count_dispatchers, _From, State) ->
	{reply, length(State#state.dispatchers), State};
handle_call(stop, _From, State) -> 
	{stop, normal, ok, State};
handle_call(dump, _From, State) ->
	{reply, State, State};
handle_call(Request, _From, State) ->
	{reply, {unknown_call, Request}, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast({now_avail, AgentPid}, State) -> 
	?DEBUG("Someone's (~p) available now.", [AgentPid]),
	FindAgent = fun
		(_Mon, _Value, true) ->
			true;
		(_Mon, {agent, APid}, _Found) ->
			APid =:= AgentPid;
		(_Mon, _Val, Found) ->
			Found
	end,
	case dict:fold(FindAgent, false, State#state.monitored_items) of
		true ->
			{noreply, balance(State)};
		false ->
			Ref = erlang:monitor(process, AgentPid),
			MonItems = dict:store(Ref, {agent, AgentPid}, State#state.monitored_items),
			{noreply, balance(State#state{monitored_items = MonItems})}
	end;
handle_cast({end_avail, AgentPid}, State) -> 
	?DEBUG("An agent (~p) is no longer available.", [AgentPid]),
	FilterAgent = fun
		(MonRef, {agent, Pid}, Dict) ->
			if
				AgentPid =:= Pid ->
					erlang:demonitor(MonRef),
					dict:erase(MonRef, Dict);
				true ->
					Dict
			end;
		(_MonRef, _Value, Dict) ->
			Dict
	end,
	NewDict = dict:fold(FilterAgent, State#state.monitored_items, State#state.monitored_items),
	State2 = State#state{monitored_items = NewDict},
	{noreply, balance(State2)};
handle_cast({cook_started, Cook}, State) ->
	?DEBUG("A cook (~p) reporting", [Cook]),
	PidKnownFun = fun
		(_Mon, _Val, true) ->
			true;
		(_Mon, {cook, Pid}, _Acc) ->
			Pid =:= Cook;
		(_Mon, _Val, Acc) ->
			Acc
	end,
	case dict:fold(PidKnownFun, false, State#state.monitored_items) of
		false ->
			?DEBUG("Cook ~p wasn't known", [Cook]),
			Mon = erlang:monitor(process, Cook),
			Cooks = dict:store(Mon, {cook, Cook}, State#state.monitored_items),
			State2 = State#state{monitored_items = Cooks},
			{noreply, balance(State2)};
		true ->
			?DEBUG("Cook ~p already known", [Cook]),
			{noreply, State}
	end;
handle_cast(deep_inspect, #state{dispatchers = Disps} = State) ->
	Fun = fun(Pid) ->
		{ok, Dispstate} = gen_server:call(Pid, dump_state),
		Queued = element(2, Dispstate),
		QueueRef = element(4, Dispstate),
		[Pid, Queued, QueueRef]
	end,
	Mapped = lists:map(Fun, Disps),
	io:format("Pid\tQueuedCall\tQueuepid~n"),
	lists:foreach(fun(L) -> io:format("~p\t~p\t~p~n", L) end, Mapped),
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info({'DOWN', MonitorRef, process, Object, _Info}, State) ->
	?DEBUG("Announce that a monitored item (~p) is down", [MonitorRef]),
	FilterFun = fun
		(MonRef, {Type, MonObj}) ->
			MonRef =/= MonitorRef
	end,
	NewDict = dict:filter(FilterFun, State#state.monitored_items),
	State2 = State#state{monitored_items = NewDict},
	{noreply, balance(State2)};
handle_info({'EXIT', Pid, Reason}, #state{dispatchers = Dispatchers} = State) ->
	case (Reason =:= normal orelse Reason =:= shutdown) of
		true ->
			%?DEBUG("Dispatcher exited normally ~p", [Pid]),
			ok;
		false ->
			?NOTICE("Dispatcher unexpected exit:  ~p ~p", [Pid, Reason])
	end,
	CleanD = lists:delete(Pid, Dispatchers),
	State2 = State#state{dispatchers = CleanD},
	{noreply, balance(State2)};
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @private
terminate(Reason, State) ->
	?NOTICE("Termination cause:  ~p.  State:  ~p", [Reason, State]),
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
	
%% @private
-spec(balance/1 :: (State :: #state{}) -> #state{}).
balance(#state{dispatchers = Dispatchers} = State) ->
	#state{monitored_items = Monitored} = State,
	{CookCount, AgentCount} = count_cooks_agents(Monitored),
	TargetNum = if
		CookCount < AgentCount ->
			CookCount;
		true ->
			AgentCount
	end,
	NumDisp = length(Dispatchers),
	if
		NumDisp < TargetNum ->
			?DEBUG("Starting new dispatcher", []),
			case dispatcher:start_link() of
				{ok, Pid} ->
					balance(State#state{dispatchers = [ Pid | Dispatchers]});
				_ ->
					balance(State)
			end;
		TargetNum < NumDisp ->
			?DEBUG("More dispatcher than needed (should be temporary)", []),
			State;
		true ->
			?DEBUG("It is fully balanced!",[]),
			State
	end.

count_cooks_agents(Dict) ->
	dict:fold(fun count_cooks_agents/3, {0,0}, Dict).

count_cooks_agents(_MonRef, {agent, _Pid}, {Cooks, Agents}) ->
	{Cooks, Agents + 1};

count_cooks_agents(_MonRef, {cook, _Pid}, {Cooks, Agents}) ->
	{Cooks + 1, Agents}.

request_end(_Olders, 0) ->
	ok;

request_end([], _X) ->
	ok;

request_end([Old | Tail], X) ->
	case dispatcher:stop(Old, false) of
		no ->
			request_end(Tail, X);
		ok ->
			request_end(Tail, X - 1)
	end.

%balance(State) when length(State#state.agents) < length(State#state.dispatchers) -> 
%	%?DEBUG("Killing a dispatcher",[]),
%	%[Pid | Dispatchers] = State#state.dispatchers,
%	%?DEBUG("Pid I'm about to kill: ~p.", [Pid]),
%	%try dispatcher:stop(Pid) of
%		%ok ->
%			% if it dies, we'll get the exit message.
%			%balance(State#state{dispatchers=Dispatchers});
%		%_ ->
%			%balance(State#state{dispatchers=[Pid | Dispatchers]})
%	%catch
%		%_:_ ->
%			%balance(State#state{dispatchers=Dispatchers})
%	%end;
%	Diff = length(State#state.dispatchers) - length(State#state.agents),
%	case Diff of
%		_ when Diff > 10 ->
%			Dispatchers = balance_down([], lists:reverse(State#state.dispatchers), Diff, false),
%			State#state{dispatchers = Dispatchers};
%		_ ->
%			State
%	end;
%balance(State) -> 
%	?DEBUG("It is fully balanced!",[]),
%	State.

%balance_down(Out, _In, 0, _Force) ->
%	lists:reverse(Out);
%balance_down(Out, [], _Count, true) ->
%	lists:reverse(Out);
%balance_down(Out, [], Count, false) ->
%	?DEBUG("switching to hard kill mode; ~p holdouts", [Count]),
%	balance_down([], lists:reverse(Out), Count, true);
%balance_down(Out, [D | In], Count, Force) ->
%	try dispatcher:stop(D, Force) of
%		ok ->
%			balance_down(Out, In, Count - 1, Force);
%		_ ->
%			?DEBUG("dispatcher declined to die", []),
%			balance_down([D | Out], In, Count, Force)
%	catch
%		_:_ ->
%			balance_down(Out, In, Count - 1, Force)
%	end.

-ifdef(TEST).

dump() ->
	gen_server:call(?MODULE, dump).

test_primer() ->
	%["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	mnesia:start().

count_downs(FilterPid, N) ->
	receive
		{'DOWN', _MonRef, process, FilterPid, _Info} ->
			count_downs(FilterPid, N+1)
	after 10 ->
		N
	end.

zombie() ->
	receive headshot -> exit end.

fake_cook() ->
	spawn(fun() ->
		dispatch_manager:cook_started(),
		zombie()
	end).

dict_find_by_val(Needle, Dict) ->
	List = dict:to_list(Dict),
	[X || {X, Needle0} <- List, Needle0 =:= Needle].

monitor_test_() ->
	util:start_testnode(),
	N = util:start_testnode(dispatch_manger_monitor_tests),
	{spawn, N, {inorder, {foreach, 
	fun() ->
		test_primer(),
		agent_manager:start([node()]),
		queue_manager:start([node()]),
		ok
	end,
	fun(ok) ->
		ok
	end,
	[{"An agent gets monitored only once", fun() ->
		{ok, State} = init([]),
		FakeAgent = spawn(fun zombie/0),
		{noreply, S1} = handle_cast({now_avail, FakeAgent}, State),
		{noreply, S2} = handle_cast({end_avail, FakeAgent}, S1),
		{noreply, S3} = handle_cast({now_avail, FakeAgent}, S2),
		{noreply, S4} = handle_cast({end_avail, FakeAgent}, S3),
		FakeAgent ! headshot,
		Count = count_downs(FakeAgent, 0),
		?assertEqual(0, Count)
	end},
	{"Cook gets monitored only once", fun() ->
		{ok, State} = init([]),
		Cook = spawn(fun() ->
			dispatch_manager:cook_started(),
			dispatch_manager:cook_started(),
			zombie()
		end),
		{noreply, State2} = handle_cast({cook_started, Cook}, State),
		{noreply, State3} = handle_cast({cook_started, Cook}, State2),
		Cook ! headshot,
		Count = count_downs(Cook, 0),
		?assertEqual(1, Count)
	end}]}}}.

balance_test_() ->
	util:start_testnode(),
	N = util:start_testnode(dispatch_manager_balance_tests),
	{spawn, N, {inorder, {foreach,
	fun() ->
		test_primer(),
		agent_manager:start([node()]),
		queue_manager:start([node()]),
		start(),
		ok
	end,
	fun(ok) ->
		agent_manager:stop(),
		queue_manager:stop(),
		stop(),
		timer:sleep(50)
	end,
	[{"Agent started, but is still released", fun() ->
		Cook = fake_cook(),
		{ok, _Apid} = agent_manager:start_agent(#agent{login = "testagent"}),
		receive
		after 100 ->
			ok
		end,
		State1 = dump(),
		?assertEqual({1,0}, count_cooks_agents(State1#state.monitored_items)),
		?assertEqual([], State1#state.dispatchers),
		Cook ! headshot
	end},
	{"Agent started then set available, so a dispatcher starts", fun() ->
		Cook = fake_cook(),
		timer:sleep(10),
		State1 = dump(),
		?assertEqual({1,0}, count_cooks_agents(State1#state.monitored_items)),
		?assertEqual([], State1#state.dispatchers),
		{ok, Apid} = agent_manager:start_agent(#agent{login = "testagent"}),
		agent:set_state(Apid, idle),
		receive
		after 100 ->
			ok
		end,
		State2 = dump(),
		?assertMatch({1,1}, count_cooks_agents(State2#state.monitored_items)),
		?assertEqual(1, length(State2#state.dispatchers)),
		Cook ! headshot
	end},
	% commented out until proper behavior defined.
	%{"Agent died, but dispatchers don't die automatically", fun() ->
		%{ok, Apid} = agent_manager:start_agent(#agent{login = "testagent"}),
		%agent:set_state(Apid, idle),
		%receive
		%after 100 ->
		%	ok
		%end,
		%State1 = dump(),
		%?assertMatch([{Apid, _}], dict:to_list(State1#state.agents)),
		%?assertEqual(1, length(State1#state.dispatchers)),
		%exit(Apid, kill),
		%receive
		%after 100 ->
		%	ok
		%end,
		%State2 = dump(),
		%?assertEqual(dict:new(), State2#state.agents),
		%?assertEqual(1, length(State2#state.dispatchers))
	%end},
	{"Unexpected dispatcher death", fun() ->
		Cook = fake_cook(),
		{ok, Apid} = agent_manager:start_agent(#agent{login = "testagent"}),
		agent:set_state(Apid, idle),
		#state{dispatchers = [PidToKill]} = dump(),
		exit(PidToKill, test_kill),
		receive
		after 100 ->
			ok
		end,
		State1 = dump(),
		?assertEqual(1, length(State1#state.dispatchers)),
		?assertNot([PidToKill] =:= State1#state.dispatchers)
	end},
	{"Agent unavailable, do a dispatcher ends", fun() ->
		Cook = fake_cook(),
		{ok, Apid} = agent_manager:start_agent(#agent{login = "testagent"}),
		agent:set_state(Apid, idle),
		receive
		after 100 ->
			ok
		end,
		State1 = dump(),
		?assertMatch({1,1}, count_cooks_agents(State1#state.monitored_items)),
		?assertEqual(1, length(State1#state.dispatchers)),
		agent:set_state(Apid, released, default),
		receive
		after 100 ->
			ok
		end,
		State2 = dump(),
		?assertEqual({1,0}, count_cooks_agents(State2#state.monitored_items)),
		Cook ! headshot
	end},
	{"Agent avail and already tracked", fun() ->
		Cook = fake_cook(),
		{ok, Apid} = agent_manager:start_agent(#agent{login = "testagent"}),
		agent:set_state(Apid, idle),
		receive
		after 100 ->
			ok
		end,
		State1 = dump(),
		?assertMatch({1,1}, count_cooks_agents(State1#state.monitored_items)),
		?assertEqual(1, length(State1#state.dispatchers)),
		gen_server:cast(?MODULE, {now_avail, Apid}),
		State2 = dump(),
		?assertMatch({1,1}, count_cooks_agents(State2#state.monitored_items)),
		?assertEqual(1, length(State1#state.dispatchers)),
		Cook ! headshot
	end},
	{"Dispatcher unfortunately dies, but notices agents and cooks on it's return.", fun() ->
		FakeCookLoop = fun(Ref, CbFun) ->
			receive
				headshot -> ok;
			{'DOWN', _, _, _, _} ->
				timer:sleep(6),
				Ref0 = dispatch_manager:cook_started(),
				CbFun(Ref, CbFun)
			end
		end,
		Cooks= [spawn(fun() ->
			Ref = dispatch_manager:cook_started(),
			FakeCookLoop(Ref, FakeCookLoop)
		end) || _ <- lists:seq(1, 5)],
		agent_dummy_connection:start_x(10),
		Agents = agent_manager:list(),
		Setrel = fun(I) ->
			{_Login, {Pid, _, _, _}} = lists:nth(I, Agents),
			agent:set_state(Pid, released, default)
		end,
		lists:foreach(Setrel, lists:seq(1, 5)),
		#state{monitored_items = ExpectedMonitored, dispatchers = Unexpecteddispatchers} = gen_server:call(dispatch_manager, dump),
		exit(whereis(dispatch_manager), kill),
		timer:sleep(5),
		{ok, _Pid} = start(),
		timer:sleep(1000),
		#state{monitored_items = NewMonitored, dispatchers = Newdispathers} = Dump = gen_server:call(dispatch_manager, dump),
		?DEBUG("Expected:  ~p", [ExpectedMonitored]),
		?DEBUG("New agents:  ~p", [NewMonitored]),
		?assertEqual(length(dict:to_list(ExpectedMonitored)), length(dict:to_list(NewMonitored))),
		?assertEqual(5, length(Newdispathers)),
		lists:foreach(fun(I) ->
			?assertNot(lists:member(I, Unexpecteddispatchers))
		end, Newdispathers),
		AssertFold = fun
			(_Mon, {agent, Pid} = Val, _) ->
				?assertEqual(1, length(dict_find_by_val(Val, ExpectedMonitored)));
			(_, _, _) ->
				ok
		end,
		dict:fold(AssertFold, ok, NewMonitored),
		?DEBUG("Bing", []),
		[Cook ! headshot || Cook <- Cooks]
	end},
	{"agent spawns but no cooks, so no dispatchers", fun() ->
		{ok, Apid} = agent_manager:start_agent(#agent{login = "testagent"}),
		agent:set_state(Apid, idle),
		receive after 100 -> ok end,
		State1 = dump(),
		?assertEqual(0, length(State1#state.dispatchers))
	end},
	{"cook spanws, but no agents, so no dispatchers", fun() ->
		Cook = fake_cook(),
		State1 = dump(),
		?assertEqual(0, length(State1#state.dispatchers))
	end},
	{"cook spanws, and there's an agent, so a dispatcher is spawned", fun() ->
		{ok, Apid} = agent_manager:start_agent(#agent{login = "testagent"}),
		agent:set_state(Apid, idle),
		receive after 100 -> ok end,
		Cook = fake_cook(),
		timer:sleep(10),
		State1 = dump(),
		?assertEqual(1, length(State1#state.dispatchers))
	end} ]}}}.

gen_server_test_start() ->
	util:start_testnode(),
	N = util:start_testnode(dispatch_manager_gen_server_tests),
	Start = fun() ->
		{ok, _Pid} = ?MODULE:start_link(),
		?MODULE
	end,
	Stop = fun(_) ->
		?MODULE:stop()
	end,
	{N, Start, Stop}.

-define(GEN_SERVER_TEST, fun gen_server_test_start/0).

-include("gen_server_test.hrl").

-endif.
