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
	now_avail/2,
	end_avail/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	dispatchers = [] :: [pid()],
	agents = dict:new() :: dict(),
	channel_count = 0 :: non_neg_integer()
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

-spec(now_avail/2 :: (AgentPid :: pid(), Channels :: [atom()]) -> 'ok').
now_avail(AgentPid, Channels) ->
	gen_server:cast(?MODULE, {now_avail, AgentPid, Channels}).

-spec(end_avail/1 :: (AgentPid :: pid()) -> 'ok').
end_avail(AgentPid) ->
	gen_server:cast(?MODULE, {end_avail, AgentPid}).
	
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
			spawn(fun() -> 
				timer:sleep(10),
				?DEBUG("Spawn waking up with agents ~p", [Agents]),
				[case A#agent_cache.channels of
					[] ->
						gen_server:cast(dispatch_manager, {end_avail, A#agent_cache.pid});
					_ ->
						gen_server:cast(dispatch_manager, {now_avail, A#agent_cache.pid, A#agent_cache.channels})
				end || {_Id, A} <- Agents],
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
handle_cast({now_avail, AgentPid, Channels}, #state{channel_count = Chans} = State) -> 
	?DEBUG("Someone's (~p) available now.", [AgentPid]),
	case dict:find(AgentPid, State#state.agents) of
		{ok, {Ref, Channels}} ->
			{noreply, balance(State)};
		{ok, {Ref, DiffChannels}} ->
			Diff = length(Channels) - length(DiffChannels),
			NewCount = Chans + Diff,
			NewDict = dict:store(AgentPid, {Ref, Channels}, State#state.agents),
			{noreply, balance(State#state{agents = NewDict, channel_count = NewCount})};
		error -> 
			Ref = erlang:monitor(process, AgentPid),
			NewDict = dict:store(AgentPid, {Ref, Channels}, State#state.agents),
			NewCount = State#state.channel_count + length(Channels),
			State2 = State#state{agents = NewDict, channel_count = NewCount},
			{noreply, balance(State2)}
	end;
handle_cast({end_avail, AgentPid}, State) -> 
	?DEBUG("An agent (~p) is no longer available.", [AgentPid]),
	{NewDict, NewCount} = case dict:find(AgentPid, State#state.agents) of
		error ->
			{State#state.agents, State#state.channel_count};
		{ok, {Ref, Chans}} ->
			%{ok, Ref} = dict:find(AgentPid, State#state.agents),
			erlang:demonitor(Ref),
			OutD = dict:erase(AgentPid, State#state.agents),
			OutC = State#state.channel_count - length(Chans),
			{OutD, OutC}
	end,
	State2 = State#state{agents = NewDict, channel_count = NewCount},
	{noreply, balance(State2)};
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
handle_info({'DOWN', _MonitorRef, process, Object, _Info}, State) -> 
	?DEBUG("Announcement that an agent (~p) is down, balancing in response.", [Object]),
	case dict:find(Object, State#state.agents) of
		error ->
			{noreply, balance(State)};
		{ok, {_MonRef, Chans}} ->
			NewCount = State#state.channel_count - length(Chans),
			NewDict = dict:erase(Object, State#state.agents),
			NewState = State#state{agents = NewDict, channel_count = NewCount},
			{noreply, balance(NewState)}
	end;
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
balance(#state{dispatchers = Dispatchers, channel_count = NumAgents} = State) ->
	NumDisp = length(Dispatchers),
	case NumAgents of
		X when X > NumDisp ->
			?DEBUG("Starting new dispatcher",[]),
			case dispatcher:start_link() of
				{ok, Pid} ->
					balance(State#state{dispatchers = [ Pid | Dispatchers]});
				_ ->
					balance(State)
			end;
		_ ->
			?DEBUG("It is fully balanced!",[]),
			State
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

count_downs(FilterPid, N) ->
	receive
		{'DOWN', _MonRef, process, FilterPid, _Info} ->
			count_downs(FilterPid, N+1)
	after 10 ->
		N
	end.

zombie() ->
	receive headshot -> exit end.

monitor_test_() ->
	{setup, fun() ->
		{ok, State} = init([]),
		meck:new(dispatcher),
		meck:expect(dispatcher, start_link, fun() -> {ok, self()} end),
		State
	end,
	fun(_) ->
		meck:validate(dispatcher),
		meck:unload(dispatcher)
	end,
	fun(State0) -> [

		{"An agent gets monitored only once", fun() ->
			FakeAgent = spawn(fun zombie/0),
			{noreply, S1} = handle_cast({now_avail, FakeAgent, []}, State0),
			{noreply, S2} = handle_cast({end_avail, FakeAgent}, S1),
			{noreply, S3} = handle_cast({now_avail, FakeAgent, []}, S2),
			{noreply, S4} = handle_cast({end_avail, FakeAgent}, S3),
			FakeAgent ! headshot,
			Count = count_downs(FakeAgent, 0),
			?assertEqual(0, Count)
		end},

		{"An agent gets monitored, period", fun() ->
			FakeAgent = spawn(fun zombie/0),
			{noreply, S1} = handle_cast({now_avail, FakeAgent, []}, State0),
			FakeAgent ! headshot,
			Count = count_downs(FakeAgent, 0),
			?assertEqual(1, Count)
		end},

		{"An agent gets monitored once, channel difference", fun() ->
			FakeAgent = spawn(fun zombie/0),
			{noreply, S1} = handle_cast({now_avail, FakeAgent, []}, State0),
			{noreply, S2} = handle_cast({now_avail, FakeAgent, [skill]}, S1),
			FakeAgent ! headshot,
			Count = count_downs(FakeAgent, 0),
			?assertEqual(1, Count)
		end},

		{"Handling an agent death", fun() ->
			FakeAgent = spawn(fun zombie/0),
			{noreply, S1} = handle_cast({now_avail, FakeAgent, [skill]}, State0),
			FakeAgent ! headshot,
			Down = receive
				{'DOWN', _, process, FakeAgent, _} = X -> X
			after 10 -> ?assert(nodown) end,
			{noreply, S2} = handle_info(Down, S1),
			?assertEqual(State0#state.agents, S2#state.agents)
		end}

	] end}.

balance_test_() ->
	{setup, fun() ->
		meck:new(dispatcher),
		meck:expect(dispatcher, start_link, fun() -> {ok, spawn(fun zombie/0)} end),
		{ok, State} = ?MODULE:init([]),
		State
	end,
	fun(_) ->
		meck:validate(dispatcher),
		meck:unload(dispatcher)
	end,
	fun(State0) -> [

		{"New agent channels, dispatcher starts", fun() ->
			Zombie = spawn(fun zombie/0),
			{noreply, State1} = handle_cast({now_avail, Zombie, [skill]}, State0),
			?assertMatch([{Zombie, _}], dict:to_list(State1#state.agents)),
			?assertEqual(1, length(State1#state.dispatchers))
		end},

		{"New agent multiple channels, dispatchers start", fun() ->
			Zombie = spawn(fun zombie/0),
			{noreply, State1} = handle_cast({now_avail, Zombie, [voice, dummy]}, State0),
			?assertMatch([{Zombie, _}], dict:to_list(State1#state.agents)),
			?assertEqual(2, length(State1#state.dispatchers))
		end},

		{"agent dies, but dispatchers don't die with it", fun() ->
			Zombie = spawn(fun zombie/0),
			{noreply, State1} = handle_cast({now_avail, Zombie, [skill]}, State0),
			Zombie ! headshot,
			Down = receive
				{'DOWN', _, process, Zombie, _} = X -> X
			after 10 -> ?assert(nodown) end,
			{noreply, State2} = handle_info(Down, State1),
			?assertEqual(dict:new(), State2#state.agents),
			?assertEqual(1, length(State2#state.dispatchers))
		end},

		{"unexpected dispatcher death", fun() ->
			process_flag(trap_exit, true),
			Zombie = spawn(fun zombie/0),
			{noreply, State1} = handle_cast({now_avail, Zombie, [skill]}, State0),			[Dispatcher] = State1#state.dispatchers,
			Exit = {'EXIT', Dispatcher, headshot},
			{noreply, State2} = handle_info(Exit, State1),
			?assertEqual(1, length(State2#state.dispatchers))
		end}

	] end}.

-endif.
