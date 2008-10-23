%%%-------------------------------------------------------------------
%%% File          : cook.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  10/21/08
%%%-------------------------------------------------------------------
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

%% API
-export([start_link/3, start/3, stop/1, restart_tick/1, stop_tick/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {recipe = [] :: recipe(),
				ticked = 0 :: integer(),
				call :: string() | 'undefined',
				queue :: pid() | 'undefined',
				continue = true :: bool(),
				tref :: any()}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Call, Recipe, QueuePid) ->
    gen_server:start_link(?MODULE, [Call, Recipe, QueuePid], []).
start(Call, Recipe, QueuePid) -> 
	gen_server:start(?MODULE, [Call, Recipe, QueuePid], []).
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
init([Call, Recipe, QueuePid]) ->
	{ok, Tref} = timer:send_interval(?TICK_LENGTH, do_tick),	
	State = #state{ticked=0, recipe=Recipe, call=Call, queue=QueuePid, tref=Tref},
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(restart_tick, State) ->
	State2 = do_tick(State),
	{ok, Tref} = timer:send_interval(?TICK_LENGTH, do_tick),
	{noreply, State2#state{tref=Tref}};
handle_cast(stop_tick, State) -> 
	timer:cancel(State#state.tref),
	{noreply, State#state{tref=undefined}};
handle_cast(stop, State) -> 
	{stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(do_tick, State) -> 
	{noreply, do_tick(State)};
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
restart_tick(Pid) -> 
	gen_server:cast(Pid, restart_tick).

stop_tick(Pid) -> 
	gen_server:cast(Pid, stop_tick).

-spec(do_tick/1 :: (State :: #state{}) -> #state{}).
do_tick(#state{recipe=Recipe} = State) -> 
	do_route(State),
	Recipe2 = do_recipe(Recipe, State),
	State#state{ticked= State#state.ticked+1, recipe=Recipe2}.

stop(Pid) -> 
	gen_server:cast(Pid, stop).

-spec(do_route/1 :: (State :: #state{}) -> 'ok').
do_route(State) -> 
	Qpid = State#state.queue,
	case call_queue:get_call(Qpid, State#state.call) of
		{_Key, Call} ->
			case Call#call.bound of
				[] -> 
					ok;
				% get the list of agents from the dispatchers, then flatten it.
				Dispatchers -> 
					Agents = lists:map(fun(Dpid) -> 
						try dispatcher:get_agents(Dpid) of
							Ag -> 
								Ag
						catch
							_:_ -> 
								[]
						end
					end,
					Dispatchers),
				Agents2 = lists:flatten(Agents),
				
				% calculate costs and sort by same.
				Agents3 = lists:sort([{ARemote + Askills + Aidle, APid} || {_AName, APid, AState} <- Agents2, 
																ARemote <- 
																	case APid of
																		_X when node() =:= node(APid) -> 
																			[0]; 
																		_Y -> 
																			[15]
																		end,
																Askills <- [length(AState#agent.skills)],
																Aidle <- [element(2, AState#agent.lastchangetimestamp)]]),
				% offer the call to each agent.
				offer_call(Agents3, Call),
				ok
			end;
		 none -> 
			 ok 
	end.

-spec(offer_call/2 :: (Agents :: [{non_neg_integer, pid()}], Call :: #call{}) -> 'ok').
offer_call([], _Call) -> 
	ok;
offer_call([{_ACost, Apid} | Tail], Call) -> 
	case agent:set_state(Apid, ringing, Call) of
		ok ->
			ok;
		invalid -> 
			offer_call(Tail, Call)
	end.

-spec(do_recipe/2 :: (Recipe :: recipe(), State :: #state{}) -> recipe()).
do_recipe([{Ticks, Op, Args, Runs} | Recipe], #state{ticked=Ticked} = State) when Ticks rem Ticked == 0 -> 
	Doneop = do_operation({Ticks, Op, Args, Runs}, State),
	case Doneop of 
		{T, O, A, R} when Runs =:= run_once -> 
			%add to the output recipe
			
			[{T, O, A, R} | do_recipe(Recipe, State)];
		{T, O, A, R} when Runs =:= run_many -> 
			lists:append([{T, O, A, R}, {Ticks, Op, Args, Runs}], do_recipe(Recipe, State));
		ok when Runs =:= run_many -> 
			[{Ticks, Op, Args, Runs} | do_recipe(Recipe, State)];
		ok when Runs =:= run_once ->
			do_recipe(Recipe, State)
			% don't, just dance.
	end;
do_recipe([Head | Recipe], State) -> 
	[Head | do_recipe(Recipe, State)];
do_recipe([], _State) -> 
	[].

-spec(do_operation/2 :: (Recipe :: recipe_step(), State :: #state{}) -> 'ok' | recipe_step()).
do_operation({_Ticks, Op, Args, _Runs}, State) -> 
	#state{queue=Pid, call=Callid} = State,
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
		new_queue -> 
			io:format("NIY~n"),
			ok;
		voicemail -> 
			io:format("NIY~n"),
			ok;
		add_recipe -> 
			list_to_tuple(Args);
		announce -> 
			io:format("NIY~n"),
			ok
	end.

-ifdef(EUNIT).


recipe_test_() -> 
	{timeout, 60, 
	{
		foreach,
		fun() -> 
			queue_manager:start(),
			{ok, Pid} = queue_manager:add_queue(testqueue),
			call_queue:add(Pid, 1, #call{id="testcall", skills=[english, testskill]}),
			Pid
		end,
		fun(Pid) -> 
			call_queue:stop(Pid),
			queue_manager:stop()
		end,
		[
			{"Add skills once",
			fun() -> 
				{exists, Pid} = queue_manager:add_queue(testqueue),
				call_queue:set_recipe(Pid, [{1, add_skills, [newskill1, newskill2], run_once}]),
				{ok, MyPid} = start("testcall", [{1, add_skills, [newskill1, newskill2], run_once}], Pid),
				receive
				after ?TICK_LENGTH + 2000 ->
					true
				end,
				{_Key, #call{skills=CallSkills}} = call_queue:ask(Pid),
				?assertEqual(lists:sort([english, testskill, newskill1, newskill2]), lists:sort(CallSkills)),
				stop(MyPid)
			end},
			{"remove skills once",
			fun() -> 
				{exists, Pid} = queue_manager:add_queue(testqueue),
				call_queue:set_recipe(Pid, [{1, remove_skills, [testskill], run_once}]),
				{ok, MyPid} = start("testcall", [{1, remove_skills, [testskill], run_once}], Pid),
				receive
				after ?TICK_LENGTH + 2000 -> 
					true
				end,
				{_Key, #call{skills=CallSkills}} = call_queue:ask(Pid),
				?assertEqual([english], CallSkills),
				stop(MyPid)
			end},
			{"Set Priority once",
			fun() -> 
				{exists, Pid} = queue_manager:add_queue(testqueue),
				call_queue:set_recipe(Pid, [{1, set_priority, [5], run_once}]),
				{ok, MyPid} = start("testcall", [{1, set_priority, [5], run_once}], Pid),
				receive
				after ?TICK_LENGTH + 2000 -> 
					true
				end,
				{{Prior, _Time}, _Call} = call_queue:ask(Pid),
				?assertEqual(5, Prior),
				stop(MyPid)
			end}
		]
	}
	}.

-endif.

