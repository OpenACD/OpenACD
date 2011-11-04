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

%% @doc The cook is a process that is spawned per call in queue, it
%% executes the queue's 'recipe' on the call and kicks off call delivery 
%% to agents. When it finds one or more dispatchers bound to its call it 
%% requests that each dispatcher generate a list of local agents matching 
%% the call's criteria and selects the best one to offer it to.  It then
%% has gen_media try to ring that agent.
%% @see call_queue
%% @see dispatcher
%% @see gen_media
-module(cook).
-author("Micah").

-behaviour(gen_server).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("queue.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(TICK_LENGTH, 500).
-define(RINGOUT, 3).

-else.

-define(TICK_LENGTH, 1000).
-define(RINGOUT, 60).

-endif.

-define(DEFAULT_PATHCOST, 15).

%% API
-export([
	start_link/5,
	start/5,
	stop/1,
	restart_tick/1,
	stop_tick/1,
	start_at/6
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-type(call_key() :: {pos_integer(), {pos_integer(), pos_integer(), pos_integer()}}).
-record(state, {
		recipe = [] :: recipe(),
		ticked = 1 :: pos_integer(), % number of ticks we've done
		call :: pid() | 'undefined',
		callid :: string,
		queue :: string(),
		qpid :: pid(),
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
-spec(start_link/5 :: (Call :: pid(), Recipe :: recipe(), Queue :: string(), Qpid :: pid(), Key :: call_key()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start_link(Call, Recipe, Queue, Qpid, Key) when is_pid(Call) ->
	gen_server:start_link(?MODULE, [Call, Recipe, Queue, Qpid, Key], []).

%% @doc Starts a cook not linked to the parent process for `Call' processed by `Recipe' for call_queue named `Queue'.
-spec(start/5 :: (Call :: pid(), Recipe :: recipe(), Queue :: string(), Qpid :: pid(), Key :: call_key()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start(Call, Recipe, Queue, Qpid, Key) when is_pid(Call) ->
	gen_server:start(?MODULE, [Call, Recipe, Queue, Qpid, Key], []).

%% @doc starts a new cook on the give `node()' `Node' for `Call' to be process by `Recipe' for the call_queue named `Queue'.
%% This is used in place of start and start_link to allow a queue on a different node to start the cook on the same node
%% the media exists on.
-spec(start_at/6 :: (Node :: atom(), Call :: pid(), Recipe :: recipe(), Queue :: string(), Qpid :: pid(), Key :: call_key()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start_at(Node, Call, Recipe, Queue, Qpid, Key) ->
	F = fun() ->
		case init([Call, Recipe, Queue, Qpid, Key]) of
			{ok, State} ->
				?DEBUG("about to enter loop ~p, ~p", [get('$ancestors'), Call]),
				put('$ancestors', [Call]), % we don't want to die with the queue
				gen_server:enter_loop(?MODULE, [], State);
			{error, Reason} ->
				{error, Reason}
		end
	end,
	{ok, proc_lib:spawn_link(Node, F)}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init([Call, InRecipe, Queue, Qpid, {_Priority, {MSec, Sec, _MsSec}} = Key]) ->
	process_flag(trap_exit, true),
	try gen_media:get_call(Call) of
		CallRec ->
			?DEBUG("Cook starting for call ~p from queue ~p (~p)", [CallRec#call.id, Queue, Qpid]),
			?DEBUG("node check.  self:  ~p;  call:  ~p", [node(self()), node(Call)]),
			Tref = erlang:send_after(?TICK_LENGTH, self(), do_tick),
			OptRecipe = optimize_recipe(InRecipe),
			Now = util:now(),
			Recipe = case round(Now - (MSec * 1000000 + Sec)) of
				Ticked when Ticked > 1 ->
					?DEBUG("fast forwarding", []),
					fast_forward(OptRecipe, Ticked / (?TICK_LENGTH / 1000), Qpid, Call);
				_Else ->
					do_recipe(OptRecipe, 0, Qpid, Call)
			end,
			State = #state{recipe=Recipe, call=Call, queue=Queue, qpid = Qpid, tref=Tref, key = Key, callid = CallRec#call.id},
			{ok, State}
	catch
		Why:Reason ->
			?ERROR("~p:~p", [Why, Reason]),
			{error, Reason}
	end.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
handle_call(stop, From, #state{callid = CallID} = State) ->
	?NOTICE("Stop requested from ~p for ~p", [From, CallID]),
	{stop, normal, ok, State};
handle_call({stop, Reason}, From, #state{callid = CallID} = State) ->
	?NOTICE("Stop requested from ~p for ~p with reason ~p.", [From, CallID, Reason]),
	{stop, {normal, Reason}, ok, State};
handle_call(Request, _From, State) ->
    {reply, {unknown_call, Request}, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast(restart_tick, #state{qpid = Qpid} = State) ->
	case do_route(State#state.ringstate, Qpid, State#state.call) of
		nocall ->
			{stop, {call_not_queued, State#state.call}, State};
		Ringstate ->
			State2 = State#state{ringstate = Ringstate},
			NewRecipe = do_recipe(State2#state.recipe, State2#state.ticked, Qpid, State2#state.call),
			State3 = State2#state{ticked = State2#state.ticked + 1, recipe = NewRecipe},
			Tref = erlang:send_after(?TICK_LENGTH, self(), do_tick),
			{noreply, State3#state{tref=Tref}}
	end;
handle_cast(stop_ringing, #state{qpid = Qpid} = State) ->
	?DEBUG("rang out or ring aborted, trying to find new candidate", []),
	case do_route(none, Qpid, State#state.call) of
		nocall ->
			{stop, {call_not_queued, State#state.call}, State};
		Ringstate ->
			State2 = State#state{ringstate = Ringstate},
			{noreply, State2}
	end;
handle_cast({ring_to, Apid, QCall}, State) ->
	Agent = agent:dump_state(Apid),
	Newstate = case offer_call([{Agent#agent.login, {Apid, Agent#agent.id, [], node(Apid)}}], QCall) of
		ringing ->
			State#state{ringstate = ringing};
		none ->
			State
	end,
	{noreply, Newstate};
handle_cast(stop_tick, State) ->
	erlang:cancel_timer(State#state.tref),
	{noreply, State#state{tref=undefined}};
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(Msg, #state{callid = CallID} = State) ->
	?DEBUG("unhandled cast ~p ~p", [Msg, CallID]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info(do_tick, #state{qpid = Qpid} = State) ->
	NewRecipe = do_recipe(State#state.recipe, State#state.ticked, Qpid, State#state.call),
	Tref = case NewRecipe of
		[] ->
			% empty recipe, don't wake up later
			undefined;
		_ ->
			% TODO calculate if we can sleep longer here
			erlang:send_after(?TICK_LENGTH, self(), do_tick)
	end,
	State2 = State#state{ticked = State#state.ticked + 1, recipe = NewRecipe, tref = Tref},
	{noreply, State2};
handle_info(grab, #state{qpid = Qpid} = State) ->
	% TODO - we should wait to see if more nodes want to bind to make distributed delivery fairer
	?DEBUG("a dispatcher grabbed the call", []),
	case do_route(State#state.ringstate, Qpid, State#state.call) of
		nocall ->
			{stop, {call_not_queued, State#state.call}, State};
		Ringstate ->
			State2 = State#state{ringstate = Ringstate},
			{noreply, State2}
	end;
handle_info({'EXIT', From, Reason}, #state{qpid = From} = State) when Reason == shutdown; Reason == normal ->
	{stop, Reason, State};
handle_info({'EXIT', From, _Reason}, #state{qpid = From, callid = CallID} = State) ->
	?NOTICE("queue ~p died unexpectedly - trying to add call ~p back into the new queue", [State#state.queue, CallID]),
	Qpid = wait_for_queue(State#state.queue),
	call_queue:add_at(Qpid, State#state.key, State#state.call),
	gen_media:set_queue(State#state.call, Qpid),
	{stop, normal, State};
handle_info(Info, #state{callid = CallID} = State) ->
	?DEBUG("received random info message: ~p ~p", [Info, CallID]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @private
terminate(normal, #state{callid = CallID}) ->
	?DEBUG("normal cook death for ~p", [CallID]),
	ok;
terminate(shutdown, #state{callid = CallID}) ->
	?DEBUG("shutdown cook death for ~p", [CallID]),
	ok;
terminate({normal, Reason}, #state{callid = CallID}) ->
	?NOTICE("An inelegant cook shutdown requested for ~p with reason ~p", [CallID, Reason]),
	ok;
terminate(Reason, #state{callid = CallID}) ->
	?WARNING("Unusual cook death for ~p with reason ~p", [CallID, Reason]),
	%erlang:cancel_timer(State#state.tref),
	%Qpid = wait_for_queue(State#state.queue),
	%?INFO("Looks like the queue ~s recovered (~w), dieing now",[State#state.queue, Qpid]),
	%case call_queue:get_call(Qpid, State#state.call) of
		%none ->
			%?INFO("Call was not in queue ~s - adding it", [State#state.queue]),
			%call_queue:add_at(Qpid, State#state.key, State#state.call);
		%_ ->
			%ok
	%end,
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
-spec(do_route/3 :: (Ringstate :: 'ringing' | 'none', Queue :: pid(), Callpid :: pid()) -> 'nocall' | 'ringing' | 'none').
do_route(ringing, _Qpid, _Callpid) ->
	%?DEBUG("still ringing", []),
	ringing;
do_route(none, Qpid, Callpid) ->
	%?DEBUG("Searching for agent to ring to...",[]),
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
-spec(sort_agent_list/1 :: (Dispatchers :: [pid()]) -> [{string(), pid(), #agent{}}]).
sort_agent_list([]) ->
	[];
sort_agent_list(Dispatchers) when is_list(Dispatchers) ->
	F = fun(Dpid) ->
		try dispatcher:get_agents(Dpid) of
			[] ->
				?DEBUG("empty list, might as well tell this dispatcher to regrab", []),
				%dispatcher:regrab(Dpid),
				[];
			{unknown_call, get_agents} ->
				[];
			Ag ->
				[{K, {Apid, Aid, Askills, node(Dpid)}} || {K, {Apid, Aid, Askills}} <- Ag]
		catch
			What:Why ->
				?INFO("Caught:  ~p:~p", [What, Why]),
				[]
		end
	end,
	Agents = lists:map(F, Dispatchers),
	Agents2 = lists:flatten(Agents),
	% XXX - sort_agents_by_elegibility doesn't sort by pathcost yet
	agent_manager:sort_agents_by_elegibility(Agents2).
	%Out = agent_manager:rotate_based_on_list_count(Agents3),
	%?DEBUG("The out:  ~p", [Out]),
	%Out.

%% @private
-spec(offer_call/2 :: (Agents :: [{string(), pid(), #agent{}}], Call :: #queued_call{}) -> 'none' | 'ringing').
offer_call([], _Call) ->
	%?DEBUG("No valid agents found", []),
	none;
offer_call([{_Key, {Apid, Aid, _Skills, _Node}} | Tail], Call) ->
	case gen_media:ring(Call#queued_call.media, Apid, Call, ?getRingout) of
		ok ->
			%Callrec = gen_media:get_call(Call#queued_call.media),
			?INFO("cook offering call:  ~p to ~p", [Call#queued_call.id, Aid]),
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
	%?DEBUG("Number: ~p; agents: ~p", [Number, Agents]),
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
check_conditions([{client_calls_queued, Comparision, Number} | Conditions], Ticked, Qpid, Call) ->
	Callrec = gen_media:get_call(Call),
	Count = call_queue:call_count_by_client(Qpid, Callrec#call.client),
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
	Day = calendar:day_of_the_week(element(1, calendar:local_time())),
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
	end;
check_conditions([{caller_name, Comparison, RegEx} | Conditions], Ticked, Qpid, Call) ->
	Callrec = gen_media:get_call(Call),
	{Name, _} = Callrec#call.callerid,
	case re:compile(RegEx) of
		{error, Err} ->
			?WARNING("Err compiling regex:  ~p", [Err]),
			false;
		{ok, CompiledReg} ->
			Match = case re:run(Name, CompiledReg) of
				{match, _} -> match;
				ElseMatch -> ElseMatch
			end,
			case {Match, Comparison} of
				{match, '!='} -> false;
				{nomatch, '='} -> false;
				_ ->
					check_conditions(Conditions, Ticked, Qpid, Call)
			end
	end;
check_conditions([{caller_id, Comparison, RegEx} | Conditions], Ticked, Qpid, Call) ->
	Callrec = gen_media:get_call(Call),
	{_, Id} = Callrec#call.callerid,
	case re:compile(RegEx) of
		{error, Err} ->
			?WARNING("Err compiling regex:  ~p", [Err]),
			false;
		{ok, Compiled} ->
			Match = case re:run(Id, Compiled) of
				{match, _} -> match;
				ElseMatch -> ElseMatch
			end,
			case {Match, Comparison} of
				{match, '!='} -> false;
				{nomatch, '='} -> false;
				_ ->
					check_conditions(Conditions, Ticked, Qpid, Call)
			end
	end.

%% @private
-spec(do_recipe/4 :: (Recipe :: recipe(), Ticked :: non_neg_integer(), Qpid :: pid(), Call :: pid()) -> recipe()).
do_recipe([], _Ticked, _Qpid, _Call) ->
	[];
do_recipe(Recipe, Ticked, Qpid, Call) when is_pid(Qpid), is_pid(Call) ->
	do_recipe(Recipe, Ticked, Qpid, Call, []).

do_recipe([], _Ticked, _Qpid, _Call, Acc) ->
	Acc;
do_recipe([{Conditions, Op, Runs, _Comment} = OldAction | Recipe], Ticked, Qpid, Call, Acc) ->
	case check_conditions(Conditions, Ticked, Qpid, Call) of
		true ->
			Doneop = do_operation(Op, Qpid, Call),
			case Runs of
				run_once ->
					do_recipe(Recipe, Ticked, Qpid, Call, lists:append(Doneop, Acc));
				run_many ->
					do_recipe(Recipe, Ticked, Qpid, Call, lists:append([Doneop, [OldAction], Acc]))
			end;
		false ->
			do_recipe(Recipe, Ticked, Qpid, Call, lists:append([OldAction], Acc))
	end.

%% @private
-spec(fast_forward/4 :: (Recipe :: recipe(), Ticked :: non_neg_integer(), Qpid :: pid(), Call :: pid()) -> recipe()).
fast_forward(Recipe, Ticked, Qpid, Call) ->
	fast_forward(Recipe, Ticked, Qpid, Call, 0, []).

fast_forward([], ToTicked, _Qpid, _Call, Ticked, Acc) when Ticked >= ToTicked ->
	lists:reverse(Acc);
fast_forward([], ToTick, Qpid, Call, Ticked, Acc) ->
	fast_forward(lists:reverse(Acc), ToTick, Qpid, Call, Ticked + 1, []);
fast_forward([{Conditions, Ops, Runs, _Comment} = OldAction | Recipe], ToTick, Qpid, Call, Ticked, Acc) ->
	case fast_forward_check_conditions(Conditions, Ticked, Qpid, Call) of
		true ->
			Doneop = fast_forward_do_op(Ops, Qpid, Call),
			case Runs of
				run_once ->
					fast_forward(Recipe, ToTick, Qpid, Call, Ticked, lists:append(Doneop, Acc));
				run_many ->
					fast_forward(Recipe, ToTick, Qpid, Call, Ticked, lists:append([Doneop, [OldAction], Acc]))
			end;
		false ->
			fast_forward(Recipe, ToTick, Qpid, Call, Ticked, lists:append([OldAction], Acc))
	end.

fast_forward_check_conditions([], _Ticked, _Qpid, _Call) -> 
	true;
fast_forward_check_conditions([Condition | Tail], Ticked, Qpid, Call) ->
	AssumeFalse = [available_agents, eligible_agents, calls_queued, client_calls_queued, queue_position],
	case lists:member(element(1, Condition), AssumeFalse) of
		true ->
			false;
		false ->
			case check_conditions([Condition], Ticked, Qpid, Call) of
				true ->
					fast_forward_check_conditions(Tail, Ticked, Qpid, Call);
				false ->
					false
			end
	end.

fast_forward_do_op(Ops, Qpid, Call) ->
	fast_forward_do_op(Ops, Qpid, Call, []).

fast_forward_do_op([], _, _, Acc) ->
	lists:reverse(Acc);
fast_forward_do_op([{Op, Args} | Tail], Qpid, Call, Acc) ->
	Out = case Op of
		add_recipe ->
			list_to_tuple(Args);
		_ ->
			ok
	end,
	Newacc = case Out of
		ok ->
			Acc;
		_ ->
			[Out | Acc]
	end,
	fast_forward_do_op(Tail, Qpid, Call, Newacc).	

%% @private
-spec(do_operation/3 :: (Operations :: [recipe_operation()], Qpid :: pid(), Callpid :: pid()) -> [recipe_step()]).
do_operation(Operations, Qpid, Callpid) when is_pid(Qpid), is_pid(Callpid) ->
	do_operation(Operations, Qpid, Callpid, []).

-spec(do_operation/4 :: (Operations :: [recipe_operation()], Qpid :: pid(), Callpid :: pid(), Acc :: [recipe_step()]) -> [recipe_step()]).
do_operation([], _Qpid, _Callpid, Acc) ->
	lists:reverse(Acc);
do_operation([{Op, Args} | Tail], Qpid, Callpid, Acc) ->
	Out = case Op of
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
			call_queue:set_priority(Qpid, Callpid, Prior - 1),
			ok;
		deprioritize ->
			{{Prior, _Time}, _Call} = call_queue:get_call(Qpid, Callpid),
			call_queue:set_priority(Qpid, Callpid, Prior + 1),
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
	end,
	Newacc = case Out of
		ok ->
			Acc;
		_ ->
			[Out | Acc]
	end,
	do_operation(Tail, Qpid, Callpid, Newacc).

optimize_recipe(Recipe) ->
	optimize_recipe(Recipe, []).

optimize_recipe([], Acc) ->
	lists:reverse(Acc);
optimize_recipe([{Conds, Op, Runs, Comment} | Tail], Acc) ->
	Newacc = [{sort_conditions(Conds), Op, Runs, Comment} | Acc],
	optimize_recipe(Tail, Newacc).

sort_conditions(Conditions) ->
	lists:sort(fun sort_conditions_compare/2, Conditions).

sort_conditions_compare(CondA, CondB) ->
	Condlist = [type, client, ticks, hour, weekday, calls_queued, queue_position, client_calls_queued, available_agents, eligible_agents],
	A = element(1, CondA),
	B = element(1, CondB),
	util:list_index(A, Condlist) < util:list_index(B, Condlist).

-ifdef(TEST).

init_test_() ->
	{setup,
	fun() ->
		{ok, MediaPid} = gen_server_mock:new(),
		{ok, Qpid} = gen_server_mock:new(),
		MakeTime = fun(Seconds) ->
			Fulltime = util:now() - Seconds,
			Msec = util:floor(Fulltime / 1000000),
			Sec = Fulltime - Msec,
			{Msec, Sec, 0}
		end,
		Client = #client{label = "testclient", id="testclient"},
		MediaRec = #call{id = "testmedia", source = MediaPid, client = Client},
		SeedMedia = fun() ->
			gen_server_mock:expect_call(MediaPid, fun(A, B, State) ->
				{ok, MediaRec, State}
			end)
		end,
		?DEBUG("init test call ~p", [MediaRec]),
		{MediaRec, Qpid, MakeTime, SeedMedia}
	end,
	fun({Media, Qpid, _, _}) ->
		gen_server_mock:stop(Media#call.source),
		gen_server_mock:stop(Qpid)
	end,
	fun({Media, Qpid, MakeTime, SeedMedia}) ->
		[{"simple priority change based on client on init", fun() ->
			Recipe = [{[{client, '=', "testclient"}], [{prioritize, []}], run_once, <<"test">>}],
			SeedMedia(),
			SeedMedia(),
			SeedMedia(),
			gen_server_mock:expect_call(Qpid, fun({get_call, _}, _, GState) -> {ok, {{1, "whatever"}, "whatever"}, GState} end),
			gen_server_mock:expect_call(Qpid, fun({set_priority, _, _}, _, _) -> ?DEBUG("prioritizing", []), ok end),
			{ok, State} = init([Media#call.source, Recipe, "default_queue", Qpid, {1, os:timestamp()}]),
			?assertEqual([], State#state.recipe),
			gen_server_mock:assert_expectations(Qpid)
		end}]
	end}.

fast_forward_test_() ->
	% if an no_gen_server_mock_expectation occurs, it may be due to a 
	% recipe looping forward too many times.
	{setup,
	fun() ->
		{ok, MediaPid} = gen_server_mock:new(),
		{ok, Qpid} = gen_server_mock:new(),
		MakeTime = fun(Seconds) ->
			Fulltime = util:now() - Seconds,
			Msec = util:floor(Fulltime / 1000000),
			Sec = Fulltime - Msec,
			{Msec, Sec, 0}
		end,
		Client = #client{label = "testclient", id="testclient"},
		MediaRec = #call{id = "testmedia", source = MediaPid, client = Client},
		SeedMedia = fun() ->
			gen_server_mock:expect_call(MediaPid, fun(A, B, State) ->
				{ok, MediaRec, State}
			end)
		end,
		?DEBUG("dsaflhadslkfhkjsadfL ~p", [MediaRec]),
		{MediaRec, Qpid, MakeTime, SeedMedia}
	end,
	fun({Media, Qpid, _, _}) ->
		gen_server_mock:stop(Media#call.source),
		gen_server_mock:stop(Qpid)
	end,
	fun({Media, Qpid, MakeTime, SeedMedia}) ->
		[{"Simple fast forward (ticks only)",
		fun() ->
			InRecipe = [{[{ticks, 5}], [{prioritize, []}], run_once, <<"test">>}],
			?assertEqual([], fast_forward(InRecipe, 10, Qpid, Media#call.source))
		end},
		{"Fast forward run may (ticks only)",
		fun() ->
			InRecipe = [{[{ticks, 5}], [{prioritize, []}], run_many, <<"test">>}],
			?assertEqual(InRecipe, fast_forward(InRecipe, 10, Qpid, Media#call.source))
		end},
		{"Fast forward client check matches, run_once",
		fun() ->
			SeedMedia(),
			InRecipe = [{[{client, '=', "testclient"}], [{prioritize, []}], run_once, <<"test">>}],
			?assertEqual([], fast_forward(InRecipe, 5, Qpid, Media#call.source))
		end},
		{"Fast forward client check mismatch, run_once",
		fun() ->
			InRecipe = [{[{client, '=', "goober"}], [{prioritize, []}], run_once, <<"test">>}],
			[SeedMedia() || _ <- lists:seq(0, 5)],
			?assertEqual(InRecipe, fast_forward(InRecipe, 5, Qpid, Media#call.source))
		end},
		{"Fast forward client check mismatch, run_many",
		fun() ->
			[SeedMedia() || _ <- lists:seq(0, 5)],
			InRecipe = [{[{client, '=', "goober"}], [{prioritize, []}], run_many, <<"test">>}],
			?assertEqual(InRecipe, fast_forward(InRecipe, 5, Qpid, Media#call.source))
		end},
		{"Fast forward media type check mismatch run_once",
		fun() ->
			[SeedMedia() || _ <- lists:seq(0, 5)],
			InRecipe = [{[{type, '!=', voice}], [{prioritize, []}], run_once, <<"test">>}],
			?assertEqual(InRecipe, fast_forward(InRecipe, 5, Qpid, Media#call.source))
		end},
		{"Fast forward media type check mismatch run_many",
		fun() ->
			[SeedMedia() || _ <- lists:seq(0, 5)],
			InRecipe = [{[{type, '!=', voice}], [{prioritize, []}], run_many, <<"test">>}],
			?assertEqual(InRecipe, fast_forward(InRecipe, 5, Qpid, Media#call.source))
		end},
		{"Fast forward ticks and media type (both match) run_once",
		fun() ->
			[SeedMedia() || _ <- lists:seq(0, 5)],
			InRecipe = [{[{type, '=', voice}, {ticks, 5}], [{prioritize, []}], run_once, <<"test">>}],
			?assertEqual([], fast_forward(InRecipe, 10, Qpid, Media#call.source))
		end},
		{"Fast forward ticks and media type, insufficent ticks, run_once",
		fun() ->
			[SeedMedia() || _ <- lists:seq(0, 10)],
			InRecipe = [{[{type, '=', voice}, {ticks, 20}], [{prioritize, []}], run_once, <<"test">>}],
			?assertEqual(InRecipe, fast_forward(InRecipe, 10, Qpid, Media#call.source))
		end},
		{"Fast forward adding recipe run_once",
		fun() ->
			InRecipe = [{[{ticks, 5}], [{add_recipe, [[{ticks, 10}], [{prioritize, []}], run_once, <<"inner test">>]}], run_once, <<"test">>}],
			OutRecipe = [{[{ticks, 10}], [{prioritize, []}], run_once, <<"inner test">>}],
			?assertEqual(OutRecipe, fast_forward(InRecipe, 6, Qpid, Media#call.source))
		end},
		{"Fast forward adding multiple recipes",
		fun() ->
			InRecipe = [{[{ticks, 5}], [{add_recipe, [[{ticks, 30}], [{prioritize, []}], run_once, <<"inner test">>]}], run_many, <<"test">>}],
			OutRecipe = [{[{ticks, 5}], [{add_recipe, [[{ticks, 30}], [{prioritize, []}], run_once, <<"inner test">>]}], run_many, <<"test">>}, {[{ticks, 30}], [{prioritize, []}], run_once, <<"inner test">>}, {[{ticks, 30}], [{prioritize, []}], run_once, <<"inner test">>}],
			?assertEqual(OutRecipe, fast_forward(InRecipe, 10, Qpid, Media#call.source))
		end},
		{"Fast forward added recipe runs",
		fun() ->
			InRecipe = [{[{ticks, 5}], [{add_recipe, [[{ticks, 5}], [{prioritize, []}], run_once, <<"inner test">>]}], run_once, <<"test">>}],
			?assertEqual([], fast_forward(InRecipe, 12, Qpid, Media#call.source))
		end}]
	end}.

sort_conditions_test_() ->
	[{"Simple comparison",
	fun() ->
		Input = [{eligible_agents, '=', "Doesn't matter"}, {queue_position, '=', "Doesn't matter"}, {type, '=', "Doesn't matter"}],
		Expected = [{type, '=', "Doesn't matter"}, {queue_position, '=', "Doesn't matter"}, {eligible_agents, '=', "Doesn't matter"}],
		?assertEqual(Expected, sort_conditions(Input))
	end},
	{"It's a reverse",
	fun() ->
		Expected = [
			{type, nomatter, nomatter},
			{client, nomatter, nomatter},
			{ticks, nomatter, nomatter},
			{hour, nomatter, nomatter},
			{weekday, nomatter, nomatter},
			{calls_queued, nomatter, nomatter},
			{queue_position, nomatter, nomatter},
			{client_calls_queued, nomatter, nomatter},
			{available_agents, nomatter, nomatter},
			{eligible_agents, nomatter, nomatter}
		],
		Input = lists:reverse(Expected),
		?assertEqual(Expected, sort_conditions(Input))
	end}].

test_primer() ->
	%["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
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
			?assertEqual([], do_operation([{add_skills, [skill1, skill2]}], QPid, Mpid)),
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
			?assertEqual([], do_operation([{remove_skills, [english]}], QPid, Mpid)),
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
			?assertEqual([], do_operation([{set_priority, 5}], QPid, Mpid)),
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
			gen_server_mock:expect_call(QPid, fun({set_priority, Incpid, 6}, _Fun, _State) ->
				Incpid = Mpid,
				ok
			end),
			?assertEqual([], do_operation([{prioritize, "doesn't matter"}], QPid, Mpid)),
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
			gen_server_mock:expect_call(QPid, fun({set_priority, Incpid, 28}, _Fun, _State) ->
				Incpid = Mpid,
				ok
			end),
			?assertEqual([], do_operation([{deprioritize, "doesn't matter"}], QPid, Mpid)),
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
			?assertEqual([], do_operation([{voicemail, "doesn't matter"}], QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, Assertmocks}) ->
		{"voicemail with media that doesn't support it",
		fun() ->
			gen_server_mock:expect_call(Mpid, fun('$gen_media_voicemail', _From, State) ->
				{ok, invalid, State}
			end),
			?assertEqual([], do_operation([{voicemail, "doesn't matter"}], QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, Assertmocks}) ->
		{"add recipe",
		fun() ->
			?assertEqual([{1, 2, 3}], do_operation([{add_recipe, [1, 2, 3]}], QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, Assertmocks}) ->
		{"announce",
		fun() ->
			gen_server_mock:expect_call(Mpid, fun({'$gen_media_announce', "do the robot"}, _From, _State) -> ok end),
			?assertEqual([], do_operation([{announce, "do the robot"}], QPid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, QPid, Mpid, Assertmocks}) ->
		{"M-m-m-m-m-multi-op! (add and remove skills)",
		fun() ->
			gen_server_mock:expect_call(Mpid, fun('$gen_media_get_call', _From, State) ->
				{ok, #call{id = "c1", source = Mpid}, State}
			end),
			gen_server_mock:expect_call(QPid, fun({add_skills, "c1", [add_skills]}, _From, _State) -> ok end),
			gen_server_mock:expect_call(Mpid, fun('$gen_media_get_call', _From, State) ->
				{ok, #call{id = "c1", source = Mpid}, State}
			end),
			gen_server_mock:expect_call(QPid, fun({remove_skills, "c1", [remove_skills]}, _From, _State) -> ok end),
			?assertEqual([], do_operation([{add_skills, [add_skills]}, {remove_skills, [remove_skills]}], QPid, Mpid)),
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
		AgentList = [#agent{login = "agent1", id = "agent1", skills = ['_all'], state = idle},
			#agent{login = "agent2", id = "agent2", skills = ['_all'], state = idle},
			#agent{login = "agent3", id = "agent3", skills = ['_all'], state = idle}],
		Outlist = [begin
			gen_leader_mock:expect_cast(AMpid, fun({update_skill_list, _, _}, _, _) ->
				ok
			end),
			{ok, P} = agent:start_link(Rec),
			{{0, z, length(Rec#agent.skills), os:timestamp()}, {P, Rec#agent.id, Rec#agent.skills}}
		end || Rec <- AgentList ],
		gen_leader_mock:expect_call(AMpid, fun(list_avail_agents, _From, State, _Elec) ->
			{ok, Outlist, State}
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
		StartAgents = [#agent{login = "agent1", id = "agent1", skills = ['_all'], state = idle},
			#agent{login = "agent2", id = "agent2", skills = ['_all'], state = idle},
			#agent{login = "agent3", id = "agent3", skills = ['_all'], state = idle}],
		List = [begin
			gen_leader_mock:expect_cast(AMpid, fun({update_skill_list, _, _}, _, _) ->
				ok
			end),
			{ok, P} = agent:start_link(Rec),
			{Rec#agent.login, {P, Rec#agent.id, util:now(), Rec#agent.skills}}
		end || Rec <- StartAgents],
		gen_leader_mock:expect_call(AMpid, fun(list_agents, _From, State, _Elec) ->
			{ok, List, State}
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
	{"media type",
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
	end]}},
	{"medias for given client in queue",
	{foreach,
	fun() ->
		{ok, QMPid} = gen_leader_mock:start(queue_manager),
		{ok, Qpid} = gen_server_mock:new(),
		{ok, Mpid} = gen_server_mock:new(),
		{ok, AMpid} = gen_leader_mock:start(agent_manager),
		Assertmocks = fun() ->
			gen_leader_mock:assert_expectations(QMPid),
			gen_server_mock:assert_expectations(Qpid),
			gen_server_mock:assert_expectations(Mpid),
			gen_leader_mock:assert_expectations(AMpid)
		end,
		Primer = {QMPid, Qpid, Mpid, AMpid, Assertmocks},
		?CONSOLE("start args:  ~p", [Primer]),
		gen_server_mock:expect_call(Mpid, fun('$gen_media_get_call', _From, State) ->
			Out = #call{
				id = "foo",
				type=voice,
				source = Mpid,
				client = #client{
					id = "clientid",
					label = "clientlabel"
				}
			},
			{ok, Out, State}
		end),
		Primer
	end,
	fun({QMPid, Qpid, Mpid, AMpid, _Assertmocks}) ->
		gen_server_mock:stop(Qpid),
		gen_leader_mock:stop(QMPid),
		gen_leader_mock:stop(AMpid),
		gen_server_mock:stop(Mpid),
		timer:sleep(10)
	end,
	[fun({_QMPid, Qpid, Mpid, _AMpid, Assertmocks}) ->
		{"medias for client = cond true",
		fun() ->
			gen_server_mock:expect_call(Qpid, fun({call_count_by_client, _Client}, _From, State) ->
				{ok, 3, State}
			end),
			?assert(check_conditions([{client_calls_queued, '=', 3}], "doesn't matter", Qpid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, Qpid, Mpid, _AMpid, Assertmocks}) ->
		{"medias for client > true",
		fun() ->
			gen_server_mock:expect_call(Qpid, fun({call_count_by_client, _Client}, _From, State) ->
				{ok, 4, State}
			end),
			?assert(check_conditions([{client_calls_queued, '>', 3}], "doesn't matter", Qpid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QMPid, Qpid, Mpid, _AMpid, Assertmocks}) ->
		{"medias for client < true",
		fun() ->
			gen_server_mock:expect_call(Qpid, fun({call_count_by_client, _Client}, _From, State) ->
				{ok, 2, State}
			end),
			?assert(check_conditions([{client_calls_queued, '<', 3}], "doesn't matter", Qpid, Mpid)),
			Assertmocks()
		end}
	end]}},
	{"Caller id tests",
	{foreach,
	fun() ->
		{ok, QMPid} = gen_leader_mock:start(queue_manager),
		{ok, Qpid} = gen_server_mock:new(),
		{ok, Mpid} = gen_server_mock:new(),
		{ok, AMpid} = gen_leader_mock:start(agent_manager),
		Assertmocks = fun() ->
			gen_leader_mock:assert_expectations(QMPid),
			gen_server_mock:assert_expectations(Qpid),
			gen_server_mock:assert_expectations(Mpid),
			gen_leader_mock:assert_expectations(AMpid)
		end,
		Primer = {QMPid, Qpid, Mpid, AMpid, Assertmocks},
		?CONSOLE("start args:  ~p", [Primer]),
		gen_server_mock:expect_call(Mpid, fun('$gen_media_get_call', _From, State) ->
			Out = #call{
				id = "foo",
				type=voice,
				callerid  = {"Caller Name", "Caller Number"},
				source = Mpid,
				client = #client{
					id = "clientid",
					label = "clientlabel"
				}
			},
			{ok, Out, State}
		end),
		Primer
	end,
	fun({QMPid, Qpid, Mpid, AMpid, _Assertmocks}) ->
		gen_server_mock:stop(Qpid),
		gen_leader_mock:stop(QMPid),
		gen_leader_mock:stop(AMpid),
		gen_server_mock:stop(Mpid),
		timer:sleep(10)
	end,
	[fun({_QmPid, Qpid, Mpid, AMpid, Assertmocks}) ->
		{"exact name match", fun() ->
			?assert(check_conditions([{caller_name, '=', "^Caller Name$"}], "doesn't matter", Qpid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QmPid, Qpid, Mpid, AMpid, Assertmocks}) ->
		{"exact name mismatch", fun() ->
			?assert(check_conditions([{caller_name, '!=', "^Not The Name$"}], "doesn't matter", Qpid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QmPid, Qpid, Mpid, AMpid, Assertmocks}) ->
		{"id match", fun() ->
			?assert(check_conditions([{caller_id, '=', "Number$"}], "doesn't matter", Qpid, Mpid)),
			Assertmocks()
		end}
	end,
	fun({_QmPid, Qpid, Mpid, AMpid, Assertmocks}) ->
		{"id not match", fun() ->
			?assert(check_conditions([{caller_id, '!=', "^Number"}], "doesn't matter", Qpid, Mpid)),
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
		{ok, Dummy} = dummy_media:start([{id, "testcall"}, {skills, [english, testskill]}, {queues, none},{priority, 5}]),
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
				call_queue:set_recipe(Pid, [{[{ticks, 1}], [{prioritize, []}], run_many, "Comment"}]),
				Call = gen_media:get_call(Dummy),
				call_queue:add(Pid, Dummy, Call),
				{_Pri, #queued_call{cook = Cookpid}} = call_queue:ask(Pid),
				stop_tick(Cookpid),
				receive
				after ?TICK_LENGTH * 3 + 100 ->
					ok
				end,
				{{Priority, _Time}, _Callrec} = call_queue:ask(Pid),
				?assertEqual(5, Priority)
			end}
		end,
		fun({Pid, Dummy}) ->
			{"Restart Tick Test",
			fun() ->
				call_queue:set_recipe(Pid, [{[{ticks, 1}], [{prioritize, []}], run_many, "Comment"}]),
				Call = gen_media:get_call(Dummy),
				call_queue:add(Pid, Dummy, Call),
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
				?assertEqual(5, Priority1),
				?assertEqual(2, Priority2)
			end}
		end
	]
	}.

agent_interaction_test_() ->
	{foreach,
	fun() ->
		test_primer(),
		cdr:start(),
		?DEBUG("queue_manager:  ~p", [queue_manager:start([node()])]),
		{ok, QPid} = queue_manager:add_queue("testqueue", []),
		?DEBUG("call_queue:  ~p", [QPid]),
		{ok, MPid} = dummy_media:start([{id, "testcall"}, {queues, none}]),
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
		?DEBUG("Stopping agent_manager:  ~p", [agent_manager:stop()]),
		gen_event:stop(cdr)
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
				receive after 1000 -> ok end,
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
				agent:set_state(APid2, wrapup)
			end}
		end,
		fun({QPid, _MPid, APid}) ->
			{"Media says the the ring to the agent is invalid.",
			fun() ->
				{ok, Media} = dummy_media:start([{id, "testcall"}, {queues, none}]),
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
				{ok, Media} = dummy_media:start([{id, "testcall"}, {skills, [german]}, {queues, none}]),
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
				{ok, MPid} = dummy_media:start([{id, "testcall"}, {skills, [german]}, {queues, none}]),
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
				{ok, MPid} = dummy_media:start([{id, "testcall"}, {skills, [german, '_all']}, {queues, none}]),
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

% TODO re-enabled when rebar can handle running eunit with a node 
% (distrubutued).  Or these can be re-written to not require a live node.
multinode_test_d() ->
	{
		foreach,
		fun() ->
			["testpx", Host] = string:tokens(atom_to_list(node()), "@"),
			Master = list_to_atom(lists:append("master@", Host)),
			Slave = list_to_atom(lists:append("slave@", Host)),
			M = slave:start(net_adm:localhost(), master, " -pa debug_ebin"),
			S = slave:start(net_adm:localhost(), slave, " -pa debug_ebin"),
			?CONSOLE("M start:  ~p;  S start:  ~p", [M, S]),
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
					{ok, Media} = rpc:call(Slave, dummy_media, start, [[{id, "testcall"}, {queues, none}]]),
					?assert(node(Media) =:= Slave),
					QPid = rpc:call(Master, queue_manager, get_queue, ["default_queue"]),
					?CONSOLE("das pid:  ~p", [QPid]),
					?assert(is_pid(QPid)),
					%QPid = queue_manager:get_queue("default_queue"),
					call_queue:set_recipe(QPid, [{[{ticks, 1}], [{prioritize, []}], run_many, "Comment"}]),
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
					{ok, Media} = rpc:call(Master, dummy_media, start, [[{id, "testcall"}, {queues, none}]]),
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

-define(MYSERVERFUNC, fun() -> {ok, Dummy} = dummy_media:start([{id, "testcall"}, {queues, none}]), {ok, Pid} = start(Dummy,[{[{ticks, 1}], [{set_priority, 5}], run_once, "Comment"}], "testqueue", self(), {1, now()}), {Pid, fun() -> stop(Pid) end} end).

-include("gen_server_test.hrl").


-endif.

