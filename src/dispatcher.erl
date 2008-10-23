%%%-------------------------------------------------------------------
%%% File          : dispatcher.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  10/15/08
%%%-------------------------------------------------------------------
-module(dispatcher).
-author("Micah").

-include("call.hrl").
-include("agent.hrl").

-ifdef(EUNIT).
%-export([randomtest/0]).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, grab/1, stop/1, get_agents/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	call :: #call{} | 'undefined',
	agents = [] :: [pid()]}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link(?MODULE, [], []).
start() ->
	gen_server:start(?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call(get_agents, _From, State) when is_record(State#state.call, call) -> 
	Call = State#state.call,
	{reply, agent_manager:find_avail_agents_by_skill(Call#call.skills), State};
handle_call(grab_best, _From, State) -> 
	Queues = queue_manager:get_best_bindable_queues(),
	case loop_queues(Queues) of
		none -> 
			{reply, none, State};
		Call -> 
			{reply, Call, State#state{call=Call}}
	end;
handle_call({grab_from, Queue}, _From, State) -> 
	case call_queue:grab(Queue) of
		none -> 
			{reply, {ok, none}, State};
		{_Key, Value} -> 
			State2 = State#state{call=Value},
			{reply, {ok, Value}, State2}
	end;
handle_call(stop, _From, State) ->
	io:format("okay, okay, I'll die.~n"),
	{stop, normal, ok, State};
handle_call(_Request, _From, State) ->
	Reply = unknown,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec(get_agents/1 :: (Pid :: pid()) -> [{string, pid(), #agent{}}]).
get_agents(Pid) -> 
	gen_server:call(Pid, get_agents).

-spec(loop_queues/1 :: (Queues :: [] | [tuple()]) -> 'none' | #call{}).
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
				Call2
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

%-spec(grab/1 :: (Queue :: pid()) -> {'ok', 'none'} | {'ok', #call{}}).
%grab(Queue) -> 
%	gen_server:call(?MODULE, {grab_from, Queue}).

-spec(grab/1 :: (Pid :: pid()) -> 'none' | #call{}).
grab(Pid) -> 
	gen_server:call(Pid, grab_best).
	
-spec(stop/1 :: (pid()) -> 'ok').
stop(Pid) -> 
	io:format("just before the gen_server handle call pid:  ~p.  Me:  ~p", [Pid, self()]),
	gen_server:call(Pid, stop).
	
-ifdef(EUNIT).


random_test() -> 
	queue_manager:start(),
	{_, Pid1} = queue_manager:add_queue(queue1, 1),
	{_, Pid2} = queue_manager:add_queue(queue2, 2),
	{_, Pid3} = queue_manager:add_queue(queue3, 3),
	{_, Pid4} = queue_manager:add_queue(queue4, 3),
	Calls = list_to_tuple([Call || N <- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], Call <- [#call{id="C" ++ integer_to_list(N)}]]),
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
	io:format("queue1:~n	w:  ~p~n	Calls:~p~n	Ratio:~p~n", [1, 4, 5]),
	io:format("queue2:~n	w:  ~p~n	Calls:~p~n	Ratio:~p~n", [2, 3, 8]),
	io:format("queue3:~n	w:  ~p~n	Calls:~p~n	Ratio:~p~n", [3, 3, 12]),
	io:format("queue4:~n	w:  ~p~n	Calls:~p~n	Ratio:~p~n", [3, 0, 0]),
	io:format("Total: ~p~n", [Total]),
	io:format("out:  ~p~n", [Out]),
	V1 = dict:fetch(queue1, Out) div 1000,
	V2 = dict:fetch(queue2, Out) div 1000,
	V3 = dict:fetch(queue3, Out) div 1000,
	V4 = dict:fetch(queue4, Out) div 1000,
	?assert((V1 > 18) and (V1 < 22)),
	?assert((V2 > 29) and (V2 < 33)),
	?assert((V3 > 46) and (V3 < 50)),
	?assert(V4 =:= 0),
	call_queue:stop(Pid1),
	call_queue:stop(Pid2),
	call_queue:stop(Pid3), 
	call_queue:stop(Pid4),
	queue_manager:stop().

randomtest_loop(_Queues, _Total, Dict, 100000) -> 
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
			queue_manager:start(),
			{_, Pid1} = queue_manager:add_queue(queue1, 1),
			{_, Pid2} = queue_manager:add_queue(queue2, 2),
			{_, Pid3} = queue_manager:add_queue(queue3, 99),
			[Pid1, Pid2, Pid3]
		end,
		fun(Pids) -> 
			queue_manager:stop(),
			lists:foreach(fun(P) -> call_queue:stop(P) end, Pids)
		end,
		[
			{"there is a call we want",
			fun() -> 
				Pid1 = queue_manager:get_queue(queue1),
				Pid2 = queue_manager:get_queue(queue2),
				Pid3 = queue_manager:get_queue(queue3),
				{ok, Pid} = start(),
				Calls = [Call || N <- [1, 2, 3], Call <- [#call{id="C" ++ integer_to_list(N)}]],
				call_queue:add(Pid1, 1, lists:nth(1, Calls)),
				call_queue:add(Pid2, 1, lists:nth(2, Calls)),
				call_queue:add(Pid3, 1, lists:nth(3, Calls)),
				Call = grab(Pid),
				?assertEqual("C3", Call#call.id),
				stop(Pid)
			end},
			{"There's no call.  At all.",
			fun() -> 
				{ok, Pid} = start(),
				 Call = grab(Pid),
				 ?assertEqual(none, Call),
				 stop(Pid)
			end}
		]
	}.
	
-endif.

