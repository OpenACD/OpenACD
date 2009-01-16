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
		ticked = 0 :: integer(),
		call :: string() | 'undefined',
		queue :: pid() | 'undefined',
		continue = true :: bool(),
		ringingto :: pid(),
		ringcount = 0 :: non_neg_integer(),
		tref :: any()
}).

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
%% @private
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
%% @private
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast(restart_tick, State) ->
	State2 = do_tick(State),
	{ok, Tref} = timer:send_interval(?TICK_LENGTH, do_tick),
	{noreply, State2#state{tref=Tref}};
handle_cast(stop_tick, State) -> 
	timer:cancel(State#state.tref),
	{noreply, State#state{tref=undefined}};
handle_cast({stop_ringing, AgentPid}, State) when AgentPid =:= State#state.ringingto ->
	%% XXX - actually tell the backend to stop ringing if it's an outband ring
	{noreply, State#state{ringingto=undefined}};
handle_cast(remove_from_queue, State) ->
	call_queue:remove(State#state.queue, State#state.call),
	{noreply, State};
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
%% @private
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
%% @private
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
%% @private
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
	State2 = do_route(State),
	Recipe2 = do_recipe(Recipe, State2),
	State2#state{ticked= State2#state.ticked+1, recipe=Recipe2}.

stop(Pid) -> 
	gen_server:cast(Pid, stop).

-spec(do_route/1 :: (State :: #state{}) -> #state{}).
do_route(State) when State#state.ringingto =/= undefined, State#state.ringcount =< ?RINGOUT ->
	io:format("still ringing: ~p of ~p times~n", [State#state.ringcount, ?RINGOUT]),
	State#state{ringcount=State#state.ringcount +1};
do_route(State) when State#state.ringingto =/= undefined, State#state.ringcount > ?RINGOUT ->
	io:format("rang out~n"),
	agent:set_state(State#state.ringingto, idle),
	do_route(State#state{ringingto=undefined});
do_route(State) when State#state.ringingto =:= undefined ->
	%io:format("starting new ring~n"),
	Qpid = State#state.queue,
	case call_queue:get_call(Qpid, State#state.call) of
		{_Key, Call} ->
			case Call#call.bound of
				[] -> 
					State;
				% get the list of agents from the dispatchers, then flatten it.
				Dispatchers -> 
					Agents = lists:map(fun(Dpid) -> 
						try dispatcher:get_agents(Dpid) of
							[] ->
								io:format("empty list, might as well tell this dispatcher to regrab~n"),
								dispatcher:regrab(Dpid),
								[];
							Ag -> 
								Ag
						catch
							_:_ -> 
								[]
						end
					end,
					Dispatchers),
				Agents2 = lists:flatten(Agents),

				%io:format("Got agents ~p for call ~p~n", [Agents2, Call]),
				
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
				case offer_call(Agents3, Call) of
					none ->
						State;
					Agent ->
						State#state{ringingto=Agent, ringcount=0}
				end
			end;
		 none -> 
			 State
	end.

-spec(offer_call/2 :: (Agents :: [{non_neg_integer, pid()}], Call :: #call{}) -> 'none' | pid()).
offer_call([], _Call) -> 
	none;
offer_call([{_ACost, Apid} | Tail], Call) -> 
	%case agent:set_state(Apid, ringing, Call) of
	case gen_server:call(Call#call.source, {ring_agent, Apid, Call}) of
		ok ->
			io:format("cook offering call:  ~p to ~p~n", [Call, Apid]),
			Apid;
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

