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
%%	Micah Warren <micahw at fusedsolutions dot com>
%%

%% @doc gen_server started by dispatch_supervisor to search for and bind a call
%% to an agent.
%% @see dispatch_manager
-module(dispatcher).
-author("Micah").

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-define(POLL_INTERVAL, 500).

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, stop/2, get_agents/1, bound_call/1, regrab/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	call :: #queued_call{} | 'undefined',
	tref :: any(), % timer reference
	qpid :: pid(),
	tried_queues = [] :: [pid()],
	agents = [] :: [pid()],
	cook_mon :: reference() | 'undefined'
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================
%% @doc Start linked to the calling process.  Ususally done by the {@link dispatch_manager}.
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() ->
	gen_server:start_link(?MODULE, [], []).
	
%% @doc Starts not linked to a process.
-spec(start/0 :: () -> {'ok', pid()}).
start() ->
	gen_server:start(?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%% @private
init([]) ->
	%?DEBUG("Dispatcher starting", []),
	State = #state{},
	case grab_best() of
		none ->
			?DEBUG("no call to grab, lets start a timer", []),
			Tref = erlang:send_after(?POLL_INTERVAL, self(), grab_best),
			{ok, State#state{tref=Tref}};
		{Qpid, Call} ->
			?DEBUG("sweet, grabbed a call: ~p", [Call#queued_call.id]),
			{ok, State#state{call=Call, qpid=Qpid, tried_queues = [Qpid]}}
	end.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
handle_call(get_agents, From, State) when is_record(State#state.call, queued_call) -> 
	Call = State#state.call,
	case agent_manager:filtered_route_list(Call#queued_call.skills) of
		[] ->
			gen_server:reply(From, []),
			handle_cast(regrab, State);
		List ->
			{reply, List, State}
	end;
handle_call(bound_call, _From, State) ->
	case State#state.call of
		undefined ->
			{reply, none, State};
		Call ->
			{reply, Call, State}
		end;
handle_call({stop, Force}, _From, State) when is_record(State#state.call, queued_call) ->
	case Force of
		true ->
			{stop, normal, ok, State};
		false ->
			{reply, no, State}
	end;
handle_call({stop, _Force}, _From, State) ->
	{stop, normal, ok, State};
handle_call(dump_state, _From, State) ->
	{reply, {ok, State}, State};
handle_call(Request, _From, State) ->
	{reply, {unknown_call, Request}, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast({update_skills, Skills}, #state{call = Call} = State) ->
	Newcall = Call#queued_call{skills = Skills},
	{noreply, State#state{call = Newcall}};
handle_cast(regrab, #state{tried_queues = Tried, call = OldCall} = State) ->
	OldQ = State#state.qpid,
	Queues = queue_manager:get_best_bindable_queues(),
	Filtered = [Elem || {_Qnom, Qpid, {_Pos, _QueuedCall}, _Weight} = Elem <- Queues, not lists:member(Qpid, Tried)],
	case loop_queues(Filtered) of
		none -> 
			?DEBUG("No new queue found, maintaining same state, releasing hold for another dispatcher", []),
			call_queue:ungrab(State#state.qpid, OldCall#queued_call.id),
			Tref = erlang:send_after(?POLL_INTERVAL, self(), grab_best),
			case State#state.cook_mon of
				undefined -> ok;
				Monitor -> erlang:demonitor(Monitor)
			end,
			{noreply, State#state{qpid = undefined, call = undefined, tref = Tref, tried_queues = [], cook_mon = undefined}};
		{Qpid, Call} ->
			call_queue:ungrab(State#state.qpid, OldCall#queued_call.id),
			?DEBUG("updating from call ~s in ~p to ~s in ~p", [OldCall#queued_call.id, State#state.qpid, Call#queued_call.id, Qpid]),
			Cookmon = erlang:monitor(process, Call#queued_call.cook),
			{noreply, State#state{qpid=Qpid, call=Call, tried_queues = [Qpid | Tried], cook_mon = Cookmon}}
	end;
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info(grab_best, State) ->
	case grab_best() of
		none ->
			Tref = erlang:send_after(?POLL_INTERVAL, self(), grab_best),
			{noreply, State#state{tref = Tref}};
		{Qpid, Call} ->
			{noreply, State#state{call=Call, qpid=Qpid, tref=undefined}}
	end;
handle_info({'DOWN', Mon, process, Pid, Reason}, #state{cook_mon = Mon} = State) when Reason =:= normal orelse Reason =:= shutdown ->
	?DEBUG("Monitor'ed cook (~p) exited cleanly, going down", [Pid]),
	{stop, Reason, State};
handle_info({'DOWN', Mon, process, Pid, Reason}, #state{cook_mon = Mon} = State) ->
	?DEBUG("Monitored cook (~p) died messily, moving onto another call.", [Pid]),
	handle_cast(regrab, State);
handle_info(Info, State) ->
	?DEBUG("unexpected info ~p", [Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @private
terminate(Reason, State) ->
	?NOTICE("Teminated:  ~p", [Reason]),
	Call = State#state.call,
	catch call_queue:ungrab(State#state.qpid, Call#queued_call.id),
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

%% @doc queries the agent_manager for available agents with an appropriate skill-list.
%% @see agent_manager:find_avail_agents_by_skill/1
-spec(get_agents/1 :: (Pid :: pid()) -> {non_neg_integer(), [{string, pid(), #agent{}}]}).
get_agents(Pid) -> 
	gen_server:call(Pid, get_agents).

-spec(loop_queues/1 :: (Queues :: [{string(), pid(), {any(), #queued_call{}}, non_neg_integer()}]) -> {pid(), #queued_call{}} | 'none').
loop_queues([]) ->
	%?DEBUG("queue list is empty", []),
	none;
loop_queues(Queues) ->
	%?DEBUG("queues: ~p", [Queues]),
	Total = lists:foldl(fun(Elem, Acc) -> Acc + element(4, Elem) end, 0, Queues),
	Rand = random:uniform(Total),
	{Name, Qpid, Call, Weight} = biased_to(Queues, 0, Rand),
	%?DEBUG("grabbing call", []),
	case call_queue:grab(Qpid) of
			none -> 
				loop_queues(lists:delete({Name, Qpid, Call, Weight}, Queues));
			{_Key, Call2} ->
				%?DEBUG("grabbed call ~p", [Call2#queued_call.id]),
				link(Call2#queued_call.cook),
				{Qpid, Call2}
	end.

-spec(biased_to/3 :: (
		Queue :: [{string(), pid(), {any(), #queued_call{}}, non_neg_integer()}],
		Acc :: non_neg_integer(),
		Random :: integer()) ->
		{string(), pid(), {any(), #queued_call{}}, non_neg_integer()} | 'none').
biased_to([], _Acc, _Random) ->
	none;
biased_to([Queue | Tail], Acc, Random) -> 
	Acc2 = Acc + element(4, Queue),
	case Random =< Acc2 of
		true ->
			Queue;
		false -> 
			biased_to(Tail, Acc2, Random)
	end.

%% @doc returns the call this is bound to
-spec(bound_call/1 :: (Pid :: pid()) -> #queued_call{} | 'none').
bound_call(Pid) ->
	gen_server:call(Pid, bound_call).

-spec(grab_best/0 :: () -> {pid(), #queued_call{}} | 'none').
grab_best() ->
	Queues = queue_manager:get_best_bindable_queues(),
	loop_queues(Queues).

%% @doc tries to grab a new call ignoring the queue it's current call is bound to
-spec(regrab/1 :: (pid()) -> 'ok').
regrab(Pid) -> 
	%?DEBUG("dispatcher trying to regrab", []),
	gen_server:cast(Pid, regrab),
	ok.

%% @doc Stops the dispatcher at `pid() Pid' with reason `normal'.
-spec(stop/2 :: (pid(),  boolean()) -> 'ok').
stop(Pid, Force) -> 
	gen_server:call(Pid, {stop, Force}, infinity).
	
-ifdef(TEST).

-define(MAX_RANDOM_TEST, 100000).

random_test() ->
	["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	mnesia:start(),
	queue_manager:start([node()]),
	{_, Pid1} = queue_manager:add_queue("queue1", [{weight, 1}]),
	{_, Pid2} = queue_manager:add_queue("queue2", [{weight, 2}]),
	{_, Pid3} = queue_manager:add_queue("queue3", [{weight, 3}]),
	{_, Pid4} = queue_manager:add_queue("queue4", [{weight, 3}]),
	PCalls = [Call || N <- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], Call <- ["C" ++ integer_to_list(N)]],
	?debugFmt("PCalls:  ~p", [PCalls]),
	F = fun(Callrec) -> 
		{ok, Mpid} = dummy_media:start([{id, Callrec}, {queues, none}]),
		Mpid
	end,
	Mediapids = lists:map(F, PCalls),
	?debugFmt("Mediapids ~p", [Mediapids]),
	Calls = list_to_tuple(Mediapids),
	?debugFmt("Calls is:  ~p", [Calls]),
	call_queue:add(Pid1, 1, element(1, Calls)),
	call_queue:add(Pid1, 1, element(2, Calls)),
	call_queue:add(Pid1, 1, element(3, Calls)),
	call_queue:add(Pid1, 1, element(4, Calls)),
	call_queue:add(Pid2, 1, element(5, Calls)),
	call_queue:add(Pid2, 1, element(6, Calls)),
	call_queue:add(Pid2, 1, element(7, Calls)),
	call_queue:add(Pid3, 1, element(8, Calls)),
	call_queue:add(Pid3, 1, element(9, Calls)),
	call_queue:add(Pid3, 1, element(10, Calls)),
	Queues = queue_manager:get_best_bindable_queues(),
	Total = lists:foldl(fun(Elem, Acc) -> Acc + element(4, Elem) end, 0, Queues),
	Dict = dict:new(),
	Dict2 = dict:store("queue1", 0, Dict),
	Dict3 = dict:store("queue2", 0, Dict2),
	Dict4 = dict:store("queue3", 0, Dict3),
	Dict5 = dict:store("queue4", 0, Dict4),
	Out = randomtest_loop(Queues, Total, Dict5, 0),
	?debugFmt("queue1:~n	w:  ~p~n	Calls:~p~n	Ratio:~p~n", [1, 4, 11]),
	?debugFmt("queue2:~n	w:  ~p~n	Calls:~p~n	Ratio:~p~n", [2, 3, 33]),
	?debugFmt("queue3:~n	w:  ~p~n	Calls:~p~n	Ratio:~p~n", [3, 3, 55]),
	?debugFmt("queue4:~n	w:  ~p~n	Calls:~p~n	Ratio:~p~n", [3, 0, 0]),
	?debugFmt("Total: ~p~n", [Total]),
	?debugFmt("out:  ~p~n", [Out]),
	V1 = dict:fetch("queue1", Out) div (?MAX_RANDOM_TEST div 100), %div by 100 to make a percentage.
	V2 = dict:fetch("queue2", Out) div (?MAX_RANDOM_TEST div 100),
	V3 = dict:fetch("queue3", Out) div (?MAX_RANDOM_TEST div 100),
	V4 = dict:fetch("queue4", Out) div (?MAX_RANDOM_TEST div 100),
	?debugFmt("q1:  ~p~n", [V1]),
	?debugFmt("q2:  ~p~n", [V2]),
	?debugFmt("q3:  ~p~n", [V3]),
	?debugFmt("q4:  ~p~n", [V4]),
	% these 3 were valid when queueing strategy multiplied weight by calls
	% that is no longer true.
%	?assert((V1 > 17) and (V1 < 22)),
%	?assert((V2 > 29) and (V2 < 33)),
%	?assert((V3 > 46) and (V3 < 50)),
	?assert((9 < V1) and (V1 < 13)),
	?assert((31 < V2) and (V2 < 35)),
	?assert((53 < V3) and (V3 < 57)),
	?assert(V4 =:= 0),
	call_queue:stop(Pid1),
	call_queue:stop(Pid2),
	call_queue:stop(Pid3), 
	call_queue:stop(Pid4),
	queue_manager:stop(),
	mnesia:stop(),
	mnesia:delete_schema([node()]).

randomtest_loop(_Queues, _Total, Dict, ?MAX_RANDOM_TEST) -> 
	Dict;
randomtest_loop(Queues, Total, Dict, Acc) ->
	Rand = random:uniform(Total),
	{Name, _Qpid, _Call, _Weight} = biased_to(Queues, 0, Rand),
	Dict2 = dict:store(Name, dict:fetch(Name, Dict) +1, Dict),
	randomtest_loop(Queues, Total, Dict2, Acc+1).

agents_avail_test_() ->
	{foreach,
	fun() ->
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		mnesia:create_schema([node()]),
		mnesia:start(),
		crypto:start(),
		queue_manager:start([node()]),
		{_, Q1} = queue_manager:add_queue("q1", []),
		agent_manager:start([node()]),
		dispatch_manager:start(),
		{_, A1} = agent_manager:start_agent(#agent{login = "a1", id = "a1"}),
		{_, A2} = agent_manager:start_agent(#agent{login = "a2", id = "a2", skills = ['_all']}),
		{Q1, A1, A2}
	end,
	fun(_) ->
		agent_manager:stop(),
		dispatch_manager:stop(),
		queue_manager:stop(),
		mnesia:stop(),
		mnesia:delete_schema([node()])
	end,
	[fun({_Q1, A1, A2}) ->
		{"a call is routed even if the agent that can take it starts released",
		fun() ->
			dummy_media:q([{skills, [skills]}]),
			agent:set_state(A1, idle),
			timer:sleep(10),
			?assertEqual({ok, idle}, agent:query_state(A1)),
			agent:set_state(A2, idle),
			timer:sleep(10),
			?assertEqual({ok, ringing}, agent:query_state(A2))
		end}
	end]}.
	
prevent_infinite_regrabbing_test_() ->
	{timeout,
	10,
	fun() ->
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		mnesia:create_schema([node()]),
		mnesia:start(),
		crypto:start(),
		queue_manager:start([node()]),
		{_, Q1} = queue_manager:add_queue("q1", []),
		{_, Q2} = queue_manager:add_queue("q2", []),
		agent_manager:start([node()]),
		{_, A1} = agent_manager:start_agent(#agent{login = "a1"}),
		{ok, M1} = dummy_media:q([{skills, [skills]}, {queues, ["q1"]}]),
		{ok, M2} = dummy_media:q([{skills, [skills]}, {queues, ["q2"]}]),
		agent:set_state(A1, idle),
		timer:sleep(100),
		{ok, State} = init([]),
		{Regrabs, Grab_bests} = O = recloop(0, 0, State),
		?DEBUG("regrabs and grab_bests:  ~p", [O]),
		?assertEqual(2, Regrabs),
		% two because the dispatcher will start bound to q1.
		% the regrab bounds it to q2 (regrab 1)
		% then it's bound to nothing (regrab 2)
		% next message should be a grab_best, which restarts the cycle.
		?assertEqual(1, Grab_bests),
		% must remember to clean up the mess I've made.
		agent_manager:stop(),
		queue_manager:stop(),
		mnesia:stop()
	end}.
	
recloop(Regrabs, Grab_bests, _State) when (Regrabs > 2); Grab_bests >= 1 ->
	{Regrabs, Grab_bests};
recloop(Regrabs, Grab_bests, State) ->
	receive
		{'$gen_call',{Pid, Ref} = From, Msg} ->
			?DEBUG("recloop ~p", [Msg]),
			{reply, Reply, Newstate} = handle_call(Msg, From, State),
			gen_server:reply(From, Reply),
			recloop(Regrabs, Grab_bests, Newstate);
		{'$gen_cast', Msg} ->
			?DEBUG("recloop ~p", [Msg]),
			{noreply, Newstate} = handle_cast(Msg, State),
			Newgrabs = case Msg of
				regrab ->
					Regrabs + 1;
				_ ->
					Regrabs
			end,
			recloop(Newgrabs, Grab_bests, Newstate);
		Msg ->
			{noreply, Newstate} = handle_info(Msg, State),
			?DEBUG("recloop ~p", [Msg]),
			Newgrabs = case Msg of
				grab_best ->
					Grab_bests + 1;
				_ ->
					Grab_bests
			end,
			recloop(Regrabs, Newgrabs, Newstate)
	after (?POLL_INTERVAL - 100) ->
		recloop(Regrabs, Grab_bests, State)
	end.
	
grab_test_() ->
	{
		foreach,
		fun() ->
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			mnesia:create_schema([node()]),
			mnesia:start(),
			queue_manager:start([node()]),
			{_, Pid1} = queue_manager:add_queue("queue1", [{weight, 1}]),
			{_, Pid2} = queue_manager:add_queue("queue2", [{weight, 2}]),
			{_, Pid3} = queue_manager:add_queue("queue3", [{weight, 99}]),
			agent_manager:start([node()]),
			[Pid1, Pid2, Pid3]
		end,
		fun(Pids) -> 
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			agent_manager:stop(),
			lists:foreach(fun(P) -> call_queue:stop(P) end, Pids),
			queue_manager:stop()
		end,
		[
			fun([Pid1, Pid2, Pid3]) ->
				{"there is a call we want",
				fun() -> 
					% circumventing usual start because the lack of agents will
					% make the cook tell the dispatcher to continually regrab.
					% I just want to make sure the dispatcher on it's very first 
					% grab gets C3, the call in the highest weighted queue.
					%{ok, Pid} = start(),
					{ok, State} = init([]),
					PCalls = [Call || N <- [1, 2, 3], Call <- ["C" ++ integer_to_list(N)]],
					F = fun(Callrec) -> 
						{ok, Mpid} = dummy_media:start([{id, Callrec}, {queues, none}]),
						Mpid
					end,
					Calls = lists:map(F, PCalls),
					?DEBUG("calls:  ~p", [Calls]),
					call_queue:add(Pid1, 1, lists:nth(1, Calls)),
					call_queue:add(Pid2, 1, lists:nth(2, Calls)),
					call_queue:add(Pid3, 1, lists:nth(3, Calls)),
					%receive after ?POLL_INTERVAL -> ok end,
					%Call = bound_call(Pid),
					{noreply, #state{call = Call} = _Newstate} = handle_info(grab_best, State),
					?assertEqual("C3", Call#queued_call.id)
					%stop(Pid, true)
				end}
			end,
			fun(_Pids) ->
				{"There's no call.  At all.",
				fun() -> 
					{ok, Pid} = start(),
					 Call = bound_call(Pid),
					 ?assertEqual(none, Call),
					 stop(Pid, true)
				end}
			end,
			fun([Pid1, _Pid2, _Pid3]) ->
				{"Regrabbing with no other calls",
				fun() ->
					{ok, MPid} = dummy_media:start([{id, "testcall"}, {queues, none}]),
					call_queue:add(Pid1, MPid),
					{ok, DPid} = dispatcher:start(),
					Queuedcall = dispatcher:bound_call(DPid),
					dispatcher:regrab(DPid),
					?assertEqual(none, dispatcher:bound_call(DPid))
				end}
			end,
			fun([Pid1, Pid2, _Pid3]) ->
				{"Regrabbing with a call in another queue",
				fun() ->
					{ok, MPid1} = dummy_media:start([{id, "C1"}, {queues, none}]),
					{ok, MPid2} = dummy_media:start([{id, "C2"}, {queues, none}]),
					call_queue:add(Pid1, MPid1),
					call_queue:add(Pid2, MPid2),
					{ok, DPid} = dispatcher:start(),
					Queuedcall = dispatcher:bound_call(DPid),
					dispatcher:regrab(DPid),
					Regrabbed = dispatcher:bound_call(DPid),
					?assert(is_record(Regrabbed, queued_call)),
					?assertNot(Queuedcall =:= Regrabbed)
				end}
			end,
			fun([Pid1, _Pid2, _Pid3]) ->
				{"Regrabbing with a call in the same queue",
				fun() ->
					{ok, MPid1} = dummy_media:start([{id, "C1"}, {queues, none}]),
					{ok, MPid2} = dummy_media:start([{id, "C2"}, {queues, none}]),
					call_queue:add(Pid1, MPid1),
					call_queue:add(Pid1, MPid2),
					{ok, DPid} = dispatcher:start(),
					Queuedcall = dispatcher:bound_call(DPid),
					dispatcher:regrab(DPid),
					Regrabbed = dispatcher:bound_call(DPid),
					?assertEqual(none, Regrabbed)
				end}
			end,
			fun([Pid1, _Pid2, _Pid3]) ->
				{"loop_queues test",
				fun() ->
						?assertEqual(none, loop_queues([{"queue1", Pid1, {wtf, #queued_call{media=self(), id="foo"}}, 10}]))
				end}
			end
		]
	}.

bias_to_test() ->
	?assertEqual(none, biased_to([], 0, 20)).

-define(MYSERVERFUNC, fun() ->
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		mnesia:create_schema([node()]),
		mnesia:start(),
		?debugFmt("~p~n", [mnesia:system_info(tables)]),
		{ok, _Pid2} = queue_manager:start([node()]),
		{ok, Pid} = start(),
		{Pid, fun() ->
			queue_manager:stop(),
			Ref = stop(Pid, true),
			mnesia:stop(),
			mnesia:delete_schema([node]),
			Ref
		end}
	end).

-include("gen_server_test.hrl").

-endif.

