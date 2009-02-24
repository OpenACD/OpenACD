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

%% @doc gen_server started by dispatch_supervisor to search for and bind a call
%% to an agent.
%% @see dispatch_manager
-module(dispatcher).
-author("Micah").

-include("call.hrl").
-include("agent.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-define(POLL_INTERVAL, 500).
-else.
-define(POLL_INTERVAL, 10000).
-endif.

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, stop/1, get_agents/1, bound_call/1, regrab/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	call :: #queued_call{} | 'undefined',
	tref, % timer reference
	qpid :: pid(),
	agents = [] :: [pid()]}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
	gen_server:start_link(?MODULE, [], []).
start() ->
	?CONSOLE("Pid is ~p", [self()]),
	gen_server:start(?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%% @private
init([]) ->
	?CONSOLE("Dispatcher starting", []),
	State = #state{},
	case grab_best() of
		none ->
			?CONSOLE("no call to grab, lets start a timer", []),
			{ok, Tref} = timer:send_interval(?POLL_INTERVAL, grab_best),
			{ok, State#state{tref=Tref}};
		{Qpid, Call} ->
			?CONSOLE("sweet, grabbed a call: ~p", [Call]),
			{ok, State#state{call=Call, qpid=Qpid}}
	end.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
handle_call(get_agents, _From, State) when is_record(State#state.call, queued_call) -> 
	Call = State#state.call,
	?CONSOLE("get_agents, Call is ~p.", [Call]),
	{reply, agent_manager:find_avail_agents_by_skill(Call#queued_call.skills), State};
handle_call(bound_call, _From, State) ->
	case State#state.call of
		undefined ->
			{reply, none, State};
		Call ->
			{reply, Call, State}
		end;
handle_call(stop, _From, State) when is_record(State#state.call, queued_call) ->
	Call = State#state.call,
	call_queue:ungrab(State#state.qpid, Call#queued_call.id),
	{stop, normal, ok, State};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call(regrab, _From, State) ->
	OldQ = State#state.qpid,
	Queues = queue_manager:get_best_bindable_queues(),
	Filtered = lists:filter(fun(Elem) -> element(2, Elem) =/= OldQ end, Queues),
	?CONSOLE("looping through filtered queues...", []),
	case loop_queues(Filtered) of
		none -> 
			{reply, State#state.call, State};
		{Qpid, Call} ->
			OldCall = State#state.call,
			call_queue:ungrab(State#state.qpid, OldCall#queued_call.id),
			{reply, Call, State#state{qpid=Qpid, call=Call}}
	end;
handle_call(Request, _From, State) ->
	{reply, {unknown_call, Request}, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info(grab_best, State) ->
	?CONSOLE("dispatcher trying to grab", []),
	case grab_best() of
		none ->
			?CONSOLE("no dice", []),
			{noreply, State};
		{Qpid, Call} ->
			?CONSOLE("success!", []),
			% TODO problem if the tref doesn't exist?
			timer:cancel(State#state.tref),
			{noreply, State#state{call=Call, qpid=Qpid, tref=undefined}}
	end;
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @private
terminate(_Reason, _State) ->
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
-spec(get_agents/1 :: (Pid :: pid()) -> [{string, pid(), #agent{}}]).
get_agents(Pid) -> 
	gen_server:call(Pid, get_agents).

-spec(loop_queues/1 :: (Queues :: [] | [{atom(), pid(), {{any()}, #queued_call{}}, non_neg_integer()}]) -> 'none' | {pid(), #queued_call{}}).
loop_queues([]) -> 
	none;
loop_queues(Queues) -> 
	Total = lists:foldl(fun(Elem, Acc) -> Acc + element(4, Elem) end, 0, Queues),
	Rand = random:uniform(Total),
	{Name, Qpid, Call, Weight} = biased_to(Queues, 0, Rand),
	case call_queue:grab(Qpid) of
			none -> 
				loop_queues(lists:delete({Name, Qpid, Call, Weight}, Queues));
			{_Key, Call2} -> 
				{Qpid, Call2}
	end.

-spec(biased_to/3 :: (Queue :: [tuple()], Acc :: non_neg_integer(), Random :: non_neg_integer()) -> tuple() | 'none').
biased_to([Queue | Tail], Acc, Random) -> 
	Acc2 = Acc + element(4, Queue),
	case Acc2 of
		_ when Random =< Acc2 ->
			Queue;
		_ -> 
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
-spec(regrab/1 :: (pid()) -> {pid(), #queued_call{}} | 'none').
regrab(Pid) -> 
	?CONSOLE("dispatcher trying to regrab", []),
	gen_server:call(Pid, regrab).
	
-spec(stop/1 :: (pid()) -> 'ok').
stop(Pid) -> 
	?CONSOLE("just before the gen_server handle call pid:  ~p.  Me:  ~p", [Pid, self()]),
	gen_server:call(Pid, stop).
	
-ifdef(EUNIT).

-define(MAX_RANDOM_TEST, 100000).

random_test() ->
	["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	mnesia:start(),
	queue_manager:start([node()]),
	{_, Pid1} = queue_manager:add_queue(queue1, 1),
	{_, Pid2} = queue_manager:add_queue(queue2, 2),
	{_, Pid3} = queue_manager:add_queue(queue3, 3),
	{_, Pid4} = queue_manager:add_queue(queue4, 3),
	PCalls = [Call || N <- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], Call <- ["C" ++ integer_to_list(N)]],
	?debugFmt("PCalls:  ~p", [PCalls]),
	F = fun(Callrec) -> 
		{ok, Mpid} = dummy_media:start(Callrec),
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
	Dict2 = dict:store(queue1, 0, Dict),
	Dict3 = dict:store(queue2, 0, Dict2), 
	Dict4 = dict:store(queue3, 0, Dict3), 
	Dict5 = dict:store(queue4, 0, Dict4),
	Out = randomtest_loop(Queues, Total, Dict5, 0),
	?debugFmt("queue1:~n	w:  ~p~n	Calls:~p~n	Ratio:~p~n", [1, 4, 5]),
	?debugFmt("queue2:~n	w:  ~p~n	Calls:~p~n	Ratio:~p~n", [2, 3, 8]),
	?debugFmt("queue3:~n	w:  ~p~n	Calls:~p~n	Ratio:~p~n", [3, 3, 12]),
	?debugFmt("queue4:~n	w:  ~p~n	Calls:~p~n	Ratio:~p~n", [3, 0, 0]),
	?debugFmt("Total: ~p~n", [Total]),
	?debugFmt("out:  ~p~n", [Out]),
	V1 = dict:fetch(queue1, Out) div (?MAX_RANDOM_TEST div 100), %div by 100 to make a percentage.
	V2 = dict:fetch(queue2, Out) div (?MAX_RANDOM_TEST div 100),
	V3 = dict:fetch(queue3, Out) div (?MAX_RANDOM_TEST div 100),
	V4 = dict:fetch(queue4, Out) div (?MAX_RANDOM_TEST div 100),
	?assert((V1 > 18) and (V1 < 22)),
	?assert((V2 > 29) and (V2 < 33)),
	?assert((V3 > 46) and (V3 < 50)),
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



grab_test_() ->
	{
		foreach,
		fun() ->
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			mnesia:create_schema([node()]),
			mnesia:start(),
			queue_manager:start([node()]),
			{_, Pid1} = queue_manager:add_queue(queue1, 1),
			{_, Pid2} = queue_manager:add_queue(queue2, 2),
			{_, Pid3} = queue_manager:add_queue(queue3, 99),
			agent_manager:start([node()]),
			[Pid1, Pid2, Pid3]
		end,
		fun(Pids) -> 
			queue_manager:stop(),
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			agent_manager:stop(),
			lists:foreach(fun(P) -> call_queue:stop(P) end, Pids)
		end,
		[
			fun([Pid1, Pid2, Pid3]) ->
				{"there is a call we want",
				fun() -> 
					{ok, Pid} = start(),
					PCalls = [Call || N <- [1, 2, 3], Call <- ["C" ++ integer_to_list(N)]],
					F = fun(Callrec) -> 
						{ok, Mpid} = dummy_media:start(Callrec),
						Mpid
					end,
					Calls = lists:map(F, PCalls),
					call_queue:add(Pid1, 1, lists:nth(1, Calls)),
					call_queue:add(Pid2, 1, lists:nth(2, Calls)),
					call_queue:add(Pid3, 1, lists:nth(3, Calls)),
					receive after ?POLL_INTERVAL -> ok end,
					Call = bound_call(Pid),
					?assertEqual("C3", Call#queued_call.id),
					stop(Pid)
				end}
			end,
			fun(_Pids) ->
				{"There's no call.  At all.",
				fun() -> 
					{ok, Pid} = start(),
					 Call = bound_call(Pid),
					 ?assertEqual(none, Call),
					 stop(Pid)
				end}
			end,
			fun([Pid1, _Pid2, _Pid3]) ->
				{"Regrabbing with no other calls",
				fun() ->
					{ok, MPid} = dummy_media:start("testcall"),
					call_queue:add(Pid1, MPid),
					{ok, DPid} = dispatcher:start(),
					Queuedcall = dispatcher:bound_call(DPid),
					?assertEqual(Queuedcall, dispatcher:regrab(DPid))
				end}
			end,
			fun([Pid1, Pid2, _Pid3]) ->
				{"Regrabbing with a call in another queue",
				fun() ->
					{ok, MPid1} = dummy_media:start("C1"),
					{ok, MPid2} = dummy_media:start("C2"),
					call_queue:add(Pid1, MPid1),
					call_queue:add(Pid2, MPid2),
					{ok, DPid} = dispatcher:start(),
					Queuedcall = dispatcher:bound_call(DPid),
					Regrabbed = dispatcher:regrab(DPid),
					?assert(is_record(Regrabbed, queued_call)),
					?assertNot(Queuedcall =:= Regrabbed)
				end}
			end,
			fun([Pid1, _Pid2, _Pid3]) ->
				{"Regrabbing with a call in the same queue",
				fun() ->
					{ok, MPid1} = dummy_media:start("C1"),
					{ok, MPid2} = dummy_media:start("C2"),
					call_queue:add(Pid1, MPid1),
					call_queue:add(Pid1, MPid2),
					{ok, DPid} = dispatcher:start(),
					Queuedcall = dispatcher:bound_call(DPid),
					Regrabbed = dispatcher:regrab(DPid),
					?assertEqual(Queuedcall, Regrabbed)
				end}
			end
		]
	}.
	
-define(MYSERVERFUNC, fun() ->
		mnesia:stop(),
		mnesia:delete_schema([node]),
		mnesia:create_schema([node()]),
		mnesia:start(),
		?debugFmt("~p~n", [mnesia:system_info(tables)]),
		{ok, _Pid2} = queue_manager:start([node()]),
		{ok, Pid} = start(),

		{Pid, fun() ->
			Res = stop(Pid),
			queue_manager:stop(),
			mnesia:stop(),
			mnesia:delete_schema([node]),
			Res
		end}
	end).

-include("gen_server_test.hrl").

-endif.

