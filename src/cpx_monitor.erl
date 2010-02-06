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

%% @doc Stat tracker and monitoring gen_leader module.  A combination of the cdr handler
%% and cpx_supervisor.  This handles health stats, and non-process monitoring.

-module(cpx_monitor).
-author(micahw).

-behaviour(gen_leader).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type(proplist_item() :: atom() | {any(), any()}).
-type(proplist() :: [proplist_item()]).
-type(health_type() :: 'system' | 'node' | 'queue' | 'agent' | 'media').
-type(health_key() :: {health_type(), Name :: string() | atom()}).
-type(time() :: integer()).
-type(health_value() :: number() | {number(), number(), number(), number() | {'time', time()}}).
-type(health_details() :: [{string() | atom() | binary(), health_value()}]).
-type(other_details() :: proplist()).
% internal struct of the ets table.
%-type(health_tuple() :: {health_key(), health_details(), other_details(), time()}).

-include("log.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([
	start_link/1,
	start/1,
	stop/0,
	set/3,
	drop/1,
	get_health/1,
	health/4,
	to_proplist/1,
	subscribe/0,
	subscribe/1,
	unsubscribe/0,
	get_key/1,
	get_health_list/1,
	get_details_list/1
	]).

%% gen_server callbacks
-export([init/1, 
	elected/3,
	surrendered/3,
	handle_DOWN/3,
	handle_leader_call/4,
	handle_leader_cast/3,
	from_leader/3,
	handle_call/4, 
	handle_cast/3, 
	handle_info/2,
	terminate/2, 
	code_change/4]).

-record(state, {
	nodes = [] :: [atom()],
	monitoring = [] :: [atom()],
	down = [] :: [atom()],
	auto_restart_mnesia = true :: 'true' | 'false',
	status = stable :: 'stable' | 'merging' | 'split',	
	splits = [] :: [{atom(), integer()}],
	merge_status = none :: 'none' | any(),
	merging = none :: 'none' | [atom()],
	ets :: any(),
	subscribers = [] :: [{pid(), fun()}]
}).

-type(state() :: #state{}).
-define(GEN_LEADER, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================

-type(node_opt() :: {nodes, [atom()] | atom()}).
-type(auto_restart_mnesia_opt() :: 'auto_restart_mnesia' | {'auto_restart_mnesia', boolean()}).
-type(option() :: [node_opt() | auto_restart_mnesia_opt()]).
-type(options() :: [option()]).
%% @doc Start the monitor using the passed options.<ul>
%% <li>nodes :: Nodes to monitor, multiple uses append new values.</li>
%% <li>auto_restart_mnesia :: if set to true, this will restart mnesia after
%% a merge when recoving from a net-split.  Otherwise mnesia must be started
%% manuall.</li>
%% </ul>
-spec(start_link/1 :: (Args :: options()) -> {'ok', pid()}).
start_link(Args) ->
	Nodes = lists:flatten(proplists:get_all_values(nodes, Args)),
	?DEBUG("nodes:  ~p", [Nodes]),
    gen_leader:start_link(?MODULE, Nodes, [{heartbeat, 1}], ?MODULE, Args, []).

%% @doc See {@link start_link/1}
-spec(start/1 :: (Args :: options()) -> {'ok', pid()}).
start(Args) ->
	Nodes = lists:flatten(proplists:get_all_values(nodes, Args)),
	?DEBUG("nodes:  ~p", [Nodes]),
	gen_leader:start(?MODULE, Nodes, [{heartbeat, 1}], ?MODULE, Args, []).

%% @doc Stops the monitor.
-spec(stop/0 :: () -> any()).
stop() ->
	gen_leader:call(?MODULE, stop).

%% @doc Creates a health entry into the ets table using the given params.
%% If there is an entry with the given name already, it is updated with the
%% passed params.  Returns 'ok' immediately.  If Other does not contain an
%% entry with the key of `node', `{node, node()}' is prepended to it.
-spec(set/3 :: (Key :: health_key(), Health :: health_details(), Other :: other_details()) -> 'ok').
set({_Type, _Name} = Key, Health, Other) ->
	gen_leader:leader_cast(?MODULE, {set, {Key, Health, Other, node()}}).

%% @doc Remove the entry with the key `Key' from being tracked.
-spec(drop/1 :: (Key :: health_key()) -> 'ok').
drop({_Type, _Name} = Key) ->
	gen_leader:leader_cast(?MODULE, {drop, Key}).

%% @Gets all the records of the given type or after a given time from the ets
-spec(get_health/1 :: (Type :: health_type() | time()) -> {'ok', [{health_key(), health_details(), other_details()}]}).
get_health(Time) when is_integer(Time) ->
	gen_leader:leader_call(?MODULE, {get, Time});
get_health(Type) ->
	gen_leader:leader_call(?MODULE, {get, Type}).

%% @doc Subscribe the calling process to all events from cpx_monitor.
%% @see subscribe/1
-spec(subscribe/0 :: () -> 'ok').
subscribe() ->
	subscribe(fun(_) -> true end).

%% @doc Subscribe the calling process to certain events.  If the fun returns
%% true, the event is forwarded on, otherwise not.  Subsequent calls to
%% this from the same pid over-writes the previous setting.
-spec(subscribe/1 :: (Fun :: fun()) -> 'ok').
subscribe(Fun) ->
	Pid = self(),
	gen_leader:leader_cast(?MODULE, {subscribe, Pid, Fun}).

%% @doc Unsubscribe the calling process to events from cpx_monitor.
-spec(unsubscribe/0 :: () -> 'ok').
unsubscribe() ->
	Pid = self(),
	gen_leader:leader_cast(?MODULE, {unsubscribe, Pid}).

%% @doc Give the three numbers min, goal, and max, calculate a number from
%% 0 to 100 indicating how 'healthy' X is.  Values at or below Min are 0, at or
%% above Max are set to 100.  the Min can be the larger value, indicating that
%% as X gets bigger the utilization/health value should get smaller.
-spec(health/4 :: (Min :: float(), Goal :: float(), Max :: float(), X :: float()) -> float()).
health(_Min, Goal, _Max, Goal) ->
	50.0;
health(Min, _Goal, Max, X) when Min < Max, X >= Max ->
	100.0;
health(Min, _Goal, Max, X) when Min < Max, X =< Min ->
	0.0;
health(Max, _Goal, Min, X) when Min < Max, X >= Max ->
	0.0;
health(Max, _Goal, Min, X) when Min < Max, X =< Min ->
	100.0;
health(Bigmin, Goal, Smallmax, X) when Bigmin >= Goal, Goal >= Smallmax, X > Goal ->
	( (50 * (Goal - X)) / (Bigmin - Goal) ) + 50;
health(Bigmin, Goal, Smallmax, X) when Bigmin >= Goal, Goal >= Smallmax, X > Smallmax ->
	(50 * (2 * Goal - X - Smallmax)) / (Goal - Smallmax);
%	( 50 * (3 * X - 2 * Smallmax + Goal) ) / (Goal - Smallmax);
health(Min, Goal, Max, X) when Min =< Goal, Goal =< Max, Goal =< X ->
	% g = Goal
	% m = Max
	% x = X
	% y = (100g - 50m - 50x)/(g - m)
	(100 * Goal - 50 * Max - 50 * X) / (Goal - Max);
health(Min, Goal, Max, X) when Min =< Goal, Goal =< Max ->
	% similar to above, but w/ a twist!
	% g = Goal
	% m = Min
	% x = X
	% y = (50x - 50m) / (g - m)
	(50 * X - 50 * Min) / (Goal - Min).

%% @doc turns the passed health tuple into a proplist.
-spec(to_proplist/1 :: (Tuple :: {health_key(), health_details(), other_details()}) -> proplist()).
to_proplist({{Type, Name}, Health, Details}) ->
	[{type, Type},
	{name, Name},
	{health, Health},
	{details, Details}].

%% @doc Convience function to get the key from a health tuple.
-spec(get_key/1 :: (Tuple :: 
	{health_key(), health_details(), other_details()} | 
	{health_key(), health_details(), other_details(), pos_integer()}) -> health_key()).
get_key({Key, _, _, _}) ->
	Key;
get_key({Key, _, _}) ->
	Key.

%% @doc Conviennce function to get the abstract health numbers from a health tuple.
-spec(get_health_list/1 :: (Tuple :: 
	{health_key(), health_details(), other_details()} | 
	{health_key(), health_details(), other_details(), pos_integer()}) -> health_details()).
get_health_list({_, Health, _, _}) ->
	Health;
get_health_list({_, Health, _}) ->
	Health.

%% @doc Convience function to get the details list from a health tuple.
-spec(get_details_list/1 :: (Tuple :: 
	{health_key(), health_details(), other_details()} | 
	{health_key(), health_details(), other_details(), pos_integer()}) -> health_details()).
get_details_list({_, _, Details, _}) ->
	Details;
get_details_list({_, _, Details}) ->
	Details.
	
%%====================================================================
%% gen_server callbacks
%%====================================================================
%% @hidden
init(Args) when is_list(Args) ->
	process_flag(trap_exit, true),
	Nodes = lists:flatten(proplists:get_all_values(nodes, Args)),
	Mons = Nodes,
	Tid = ets:new(?MODULE, []),
    {ok, #state{
		nodes = Nodes, 
		monitoring = Mons,
		auto_restart_mnesia = proplists:get_value(auto_restart_mnesia, Args, true),
		ets = Tid
	}}.

%% @hidden
elected(#state{ets = Tid} = State, Election, Node) -> 
	?INFO("elected by ~w", [Node]),
	% what was down and is now up?
	Cands = gen_leader:candidates(Election),
	lists:foreach(fun(Cnode) ->
		ets:insert(Tid, {{node, Cnode}, [{down, 100}], [{state, unreported}], util:now()})
	end, Cands),
	
	tell_cands(report, Election),
	
	Edown = gen_leader:down(Election),
	Foldf = fun({N, T}, {Up, Down}) ->
		case lists:member(N, Edown) of
			true ->
				{Up, [{N, T} | Down]};
			false ->
				{[{N, T} | Up], Down}
		end
	end,
	{Merge, Stilldown} = lists:foldl(Foldf, {[], []}, State#state.splits),
	Findoldest = fun({_N, T}, Time) when T > Time ->
			T;
		({_N, _T}, Time) ->
			Time
	end,
	Entry = {{node, node()}, [{leader, 50}], [], util:now()},
	entry(Entry, State, Election),
	
	case Merge of
		[] ->
			{ok, {Merge, Stilldown}, State};
		_Else ->
			Oldest = lists:foldl(Findoldest, 0, Merge),
			Mergenodes = lists:map(fun({N, _T}) -> N end, Merge),
			P = spawn_link(agent_auth, merge, [[node() | Mergenodes], Oldest, self()]),
			Qspawn = spawn_link(call_queue_config, merge, [[node() | Mergenodes], Oldest, self()]),
			Cdrspawn = spawn_link(cdr, merge, [[node() | Mergenodes], Oldest, self()]),
			?DEBUG("spawned for agent_auth:  ~p", [P]),
			?DEBUG("Spawned for call_queue_config:  ~w", [Qspawn]),
			?DEBUG("Spawned for cdr:  ~w", [Cdrspawn]),
			{ok, {Merge, Stilldown}, State#state{status = merging, merge_status = dict:new(), merging = Mergenodes, splits = Stilldown}}
	end.

%% @hidden
surrendered(#state{ets = Tid} = State, {_Merge, _Stilldown}, Election) ->
%	QH = qlc:q([Tuple || {{Type, _Name}, _Hp, _Data, _Time} = Tuple <- ets:table(Tid), Type =:= node]),
%	Matches = qlc:e(QH),
%	F = fun({{node, Name} = Key, Hp, _Data, _Time}) ->
%		case lists:member(Name, Merge) =:= proplists:get_value(down, Hp) of
%			true ->
%				Now = util:now(),
%				ets:insert(Tid, {Key, [{upsince, Now}], [], Now}),
%				ok;
%			false ->
%				ok
%		end
%	end,
%	lists:foreach(F, Matches),
	
	
	Edown = gen_leader:down(Election),
	Ealive = gen_leader:alive(Election),
	
	lists:foreach(fun(Node) -> 
		ets:insert(Tid, {{node, Node}, [{down, 100}], [], util:now()})
	end, Edown),

	lists:foreach(fun(Node) ->
		ets:insert(Tid, {{node, Node}, [{up, 50}], [{up, util:now()}], util:now()})
	end, Ealive),
	
	gen_leader:leader_cast(cpx_monitor, {ensure_live, node(), util:now()}),
	
	{ok, State}.
	
%% @hidden
handle_DOWN(Node, #state{ets = Tid} = State, Election) ->
	?ERROR("Node ~p is down!", [Node]),
	Newsplits = case proplists:get_value(Node, State#state.splits) of
		undefined ->
			[{Node, util:now()} | State#state.splits];
		_Time ->
			State#state.splits
	end,
	Entry = case ets:lookup(Tid, {node, Node}) of
		[] ->
			{{node, Node}, [{down, 100}], [], util:now()};
		[{Key, _Hp, Details, _Time2}] ->
			{Key, [{down, 100}], Details, util:now()}
	end,
	QH = qlc:q([Id || {Id, _Hp, Details, _Time} <- ets:table(Tid),
		Id =/= {node, Node},
		proplists:get_value(node, Details) == Node
	]),
	Idlist = qlc:e(QH),
	lists:foreach(fun(Id) -> entry({drop, Id}, State, Election) end, Idlist),
	%%ets:match_delete(Tid, {'_', '_', '_', Node}),
	entry(Entry, State, Election),
	{ok, State#state{splits = Newsplits}}.

%% @hidden
handle_leader_call({get, When}, _From, #state{ets = Tid} = State, _Election) when is_integer(When) ->
	F = fun({Key, Hp, Details, Time}, Acc) when Time >= When ->
		Realhp = calc_healths(Hp),
		[{Key, Realhp, Details} | Acc];
	(_, Acc) ->
		Acc
	end,
	Out = ets:foldl(F, [], Tid),
	{reply, {ok, Out}, State};
handle_leader_call({get, What}, _From, #state{ets = Tid} = State, _Election) when is_atom(What) ->
	F = fun({{Type, _Name} = Key, Hp, Details, _Time}, Acc) when Type =:= What ->
		Realhp = calc_healths(Hp),
		[{Key, Realhp, Details} | Acc];
	(_, Acc) ->
		Acc
	end,
	Results = ets:foldl(F, [], Tid),
	{reply, {ok, Results}, State};
handle_leader_call(Message, From, State, _Election) ->
	?WARNING("received unexpected leader_call ~p from ~p", [Message, From]),
	{reply, ok, State}.

%% @hidden
handle_leader_cast({reporting, Node}, State, Election) ->
	entry({{node, Node}, [{up, 50}], [{state, reported}], util:now()}, State, Election),
	{noreply, State};
handle_leader_cast({subscribe, Pid, Fun}, #state{subscribers = Subs} = State, _Election) ->
	Newsubs = case proplists:get_value(Pid, Subs) of
		undefined ->
			link(Pid),
			[{Pid, Fun} | Subs];
		_Else ->
			Midsub = proplists:delete(Pid, Subs),
			[{Pid, Fun} | Midsub]
	end,
	{noreply, State#state{subscribers = Newsubs}};
handle_leader_cast({unsubscribe, Pid}, #state{subscribers = Subs} = State, _Election) ->
	?DEBUG("removing ~w from subscribers", [Pid]),
	unlink(Pid),
	Newsubs = proplists:delete(Pid, Subs),
	{noreply, State#state{subscribers = Newsubs}};
handle_leader_cast({drop, Key}, State, Election) ->
	entry({drop, Key}, State, Election),
	{noreply, State};
handle_leader_cast({set, {Key, Hp, Details, Node}}, State, Election)  ->
	Trueentry = case proplists:get_value(node, Details) of
		undefined ->
			Newdetails = [{node, Node} | Details],
			{Key, Hp, Newdetails, util:now()};
		_Else ->
			{Key, Hp, Details, util:now()}
	end,
	entry(Trueentry, State, Election),
	{noreply, State};
handle_leader_cast({ensure_live, Node, Time}, State, Election) ->
	Alive = gen_leader:alive(Election),
	case lists:member(Node, Alive) of
		true ->
			?DEBUG("Node ~w is in election alive list", [Node]),
			Entry = {{node, Node}, [{up, 50}], [{up, Time}], Time},
			entry(Entry, State, Election),
			{noreply, State};
		false ->
			?WARNING("Node ~w does not appear in the election alive list", [Node]),
			Entry = {{node, Node}, [{up, 50}], [{up, Time}], Time},
			entry(Entry, State, Election),
			{noreply, State}
	end;
handle_leader_cast(Message, State, _Election) ->
	?WARNING("received unexpected leader_cast ~p", [Message]),
	{noreply, State}.

%% @hidden
from_leader(_Msg, State, _Election) -> 
	?DEBUG("Stub from leader.", []),
	{ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------
%% @hidden
handle_call(stop, _From, State, _Election) ->
	{stop, normal, ok, State};
handle_call(Request, _From, State, _Election) ->
	?WARNING("Unable to fulfill call request ~p", [Request]),
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%--------------------------------------------------------------------
%% @hidden
handle_cast(Request, State, _Election) ->
	?WARNING("Unable to fulfill cast request ~p.", [Request]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%--------------------------------------------------------------------
%% @hidden
handle_info({leader_event, report}, State) ->
	gen_leader:leader_cast(?MODULE, {reporting, node()}),
	{noreply, State};
handle_info({leader_event, Message}, #state{ets = Tid} = State) ->
	?DEBUG("Got message leader_event ~p", [Message]),
	case Message of
		{drop, Key} ->
			ets:delete(Tid, Key),
			{noreply, State};
		{set, Entry} ->
			ets:insert(Tid, Entry),
			{noreply, State}
	end;
handle_info({merge_complete, Mod, _Recs}, #state{merge_status = none, status = Status}= State) when Status == stable; Status == split ->
	?INFO("Prolly a late merge complete from ~w.", [Mod]),
	{noreply, State};
handle_info({merge_complete, Mod, Recs}, #state{status = merging} = State) ->
	Newmerged = dict:store(Mod, Recs, State#state.merge_status),
	case merge_complete(Newmerged) of
		true ->
			write_rows(Newmerged),
			case State#state.auto_restart_mnesia of
				true ->
					P = restart_mnesia(State#state.monitoring -- [node()]),
					?DEBUG("process to restart mnesia:  ~p", [P]);
				false ->
					ok
			end,
			case State#state.down of
				[] ->
					{noreply, State#state{status = stable, merging = [], merge_status = none}};
				_Else ->
					{noreply, State#state{status = split, merging = [], merge_status = none}}
			end;
		false ->
			{noreply, State#state{merge_status = Newmerged}}
	end;
handle_info({'EXIT', From, Reason}, #state{subscribers = Subs} = State) ->
	?INFO("~p said it died due to ~p.", [From, Reason]),
	Newsubs = proplists:delete(From, Subs),
	{noreply, State#state{subscribers = Newsubs}};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @hidden
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
%% @hidden
code_change(_OldVsn, State, _Election, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%store_node_state(Tid, Node, Hp) ->
%	case ets:lookup(Tid, {node, Node}) of
%		[] ->
%			ets:insert(Tid, {{node, Node}, Hp, [], util:now()});
%		[{Key, Health, Details, _Time2}] ->
%			case Hp of
%				down ->
%					ets:insert(Tid, {Key, [{down, 100}], Details, util:now()});
%				Proplist ->
%					Newlist = merge_properties(Health, Proplist),
%					ets:insert(Tid, {Key, Newlist, Details, util:now()})
%			end
%	end.

%fix_entries(Proplist) ->
%	fix_entries(Proplist, []).
%
%fix_entries([], Acc) ->
%	Acc;
%fix_entries([{Key, {Min, Goal, Max, {time, Start}}} | Tail], Acc) ->
%	Newval = health(Min, Goal, Max, util:now() - Start),
%	Newacc = [{Key, Newval} | Acc],
%	fix_entries(Tail, Newacc);
%fix_entries([{Key, {Min, Goal, Max, Val}} | Tail], Acc) ->
%	Newval = health(Min, Goal, Max, Val),
%	Newacc = [{Key, Newval} | Acc],
%	fix_entries(Tail, Newacc);
%fix_entries([{_Key, _Val} = E | Tail], Acc) ->
%	fix_entries(Tail, [E | Acc]).

%merge_properties([], Mutable) ->
%	Mutable;
%merge_properties([{Key, _Value} = Prop | Tail], Mutable) ->
%	Midmut = proplists:delete(Key, Mutable),
%	merge_properties(Tail, [Prop | Midmut]);
%merge_properties([Atom | Tail], Mutable) ->
%	Midmut = proplists:delete(Atom, Mutable),
%	merge_properties(Tail, [Atom | Midmut]).
	
restart_mnesia(Nodes) ->
	F = fun() ->
		?WARNING("automatically restarting mnesia on formerly split nodes: ~p", [Nodes]),
		lists:foreach(fun(N) -> 
			case net_adm:ping(N) of
				pong ->
					S = rpc:call(N, mnesia, stop, [], 1000), 
					?DEBUG("stoping mnesia on ~w got ~p", [N, S]),
					G = rpc:call(N, mnesia, start, [], 1000),
					?DEBUG("Starging mnesia on ~w got ~p", [N, G]);
				pang ->
					?ALERT("Not restarting mnesia on ~w, it's not pingable!", [N])
			end
		end, Nodes)
	end,
	spawn_link(F).
	
merge_complete(Dict) ->
	Agent = case dict:find(agent_auth, Dict) of
		error ->
			false;
		{ok, waiting} ->
			false;
		{ok, _AgentRows} ->
			true
	end,
	Queue = case dict:find(call_queue_config, Dict) of
		error ->
			false;
		{ok, waiting} ->
			false;
		{ok, _QueueRows} ->
			true
	end,
	Cdr = case dict:find(cdr, Dict) of
		error ->
			false;
		{ok, waiting} ->
			false;
		{ok, _Cdrrows} ->
			true
	end,
	case {Agent, Queue, Cdr} of
		{true, true, true} ->
			true;
		_Else ->
			false
	end.

write_rows(Dict) ->
	write_rows_loop(dict:to_list(Dict)).

write_rows_loop([]) ->
	%?DEBUG("No more to write.", []),
	ok;
write_rows_loop([{_Mod, Recs} | Tail]) ->
	%?DEBUG("Writing ~p to mnesia", [Recs]),
	F = fun() ->
		lists:foreach(fun(R) -> mnesia:write(R) end, Recs)
	end,
	mnesia:transaction(F),
	write_rows_loop(Tail).	

calc_healths(Hp) ->
	calc_healths(Hp, []).

calc_healths([], Acc) ->
	Acc;
calc_healths([{_Key, Val} = Head | Tail], Acc) when is_integer(Val); is_float(Val) ->
	calc_healths(Tail, [Head | Acc]);
calc_healths([{Key, {Min, Mid, Max, X}} | Tail], Acc) when is_integer(X); is_float(X) ->
	Hp = health(Min, Mid, Max, X),
	calc_healths(Tail, [{Key, Hp} | Acc]);
calc_healths([{Key, {Min, Mid, Max, {time, X}}} | Tail], Acc) ->
	Diff = util:now() - X,
	Hp = health(Min, Mid, Max, Diff),
	calc_healths(Tail, [{Key, Hp} | Acc]).

entry(Entry, #state{ets = Tid} = State, Election) ->
	Message = case Entry of
		{drop, Key} ->
			ets:delete(Tid, Key),
			Entry;
		_ ->
			case {get_key(Entry), proplists:get_value(agent, get_details_list(Entry))} of
				{_, undefined} ->
					ok;
				{{media, _}, Agent} ->
					QH = qlc:q([Key || {Key, _Hp, Det, _} <- ets:table(Tid), element(1, Key) =:= media, proplists:get_value(agent, Det) =:= Agent]),
					Keys = qlc:e(QH),
					% an agent can only ever be linked to one media, so we're
					% doing some housecleaning.
					?DEBUG("Keys to drop:  ~p", [Keys]),
					Foreach = fun(K) -> 
						tell_subs({drop, K}, State#state.subscribers),
						tell_cands({drop, K}, Election),
						ets:delete(Tid, K)
					end,
					lists:foreach(Foreach, Keys);
				_ ->
					ok
			end,
			ets:insert(Tid, Entry),
			{set, Entry}
	end,
	tell_subs(Message, State#state.subscribers),
	tell_cands(Message, Election).

tell_subs(Message, Subs) ->
	%?DEBUG("Telling subs ~p message", [Subs]),
	lists:foreach(fun({Pid, Fun}) ->
		case Fun(Message) of
			true ->
				Pid ! {cpx_monitor_event, Message};
			false ->
				ok
		end
	end, Subs).

tell_cands(Message, Election) ->
	Nodes = gen_leader:candidates(Election),
	Leader = gen_leader:leader_node(Election),
	%?DEBUG("Leader ~p is telling nodes ~p", [Leader, Nodes]),
	tell_cands(Message, Nodes, Leader).

tell_cands(_Message, [], _Leader) ->
	ok;
tell_cands(Message, [Leader | Tail], Leader) ->
	tell_cands(Message, Tail, Leader);
tell_cands(Message, [Node | Tail], Leader) ->
	{cpx_monitor, Node} ! {leader_event, Message},
	tell_cands(Message, Tail, Leader).

-ifdef(TEST).

health_test_() ->
	[{"straight translations",
	fun() ->
		?assertEqual(50.0, health(0, 50, 100, 50)),
		?assertEqual(0.0, health(0, 50, 100, 0)),
		?assertEqual(100.0, health(0, 50, 100, 100)),
		?assertEqual(25.0, health(0, 50, 100, 25))
	end},
	{"lopsied",
	fun() ->
		?assertEqual(50.0, health(0, 70, 100, 70)),
		?assertEqual(75.0, health(0, 8, 10, 9)),
		?assertEqual(25.0, health(0, 8, 10, 4))
	end},
	{"lopsided the other way",
	fun() ->
		?assertEqual(50.0, health(0, 3, 10, 3)),
		?assertEqual(25.0, health(0, 2, 10, 1)),
		?assertEqual(75.0, health(0, 4, 10, 7))
	end},
	{"Bigger is underutilized",
	fun() ->
		?assertEqual(25.0, health(100, 50, 0, 75)),
		?assertEqual(75.0, health(100, 50, 0, 25)),
		?assertEqual(75.0, health(150, 100, 0, 50)),
		?assertEqual(25.0, health(150, 50, 0, 100))
	end},
	{"Type check",
	fun() ->
		?assert(is_float(health(1, 3, 5, 4)))
	end},
	{"min == goal < Max",
	fun() ->
		?assertEqual(50.0, health(0, 0, 10, 0)),
		?assertEqual(75.0, health(0, 0, 10, 5)),
		?assertEqual(100.0, health(0, 0, 10, 10))
	end},
	{"min < goal = Max",
	fun() -> 
		?assertEqual(0.0, health(0, 10, 10, 0)),
		?assertEqual(25.0, health(0, 10, 10, 5)),
		?assertEqual(50.0, health(0, 10, 10, 10))
	end},
	{"min = goal > max",
	fun() ->
		?assertEqual(50.0, health(10, 10, 0, 10)),
		?assertEqual(75.0, health(10, 10, 0, 5)),
		?assertEqual(100.0, health(10, 10, 0, 0))
	end},
	{"min > goal = max",
	fun() ->
		?assertEqual(0.0, health(10, 0, 0, 10)),
		?assertEqual(25.0, health(10, 0, 0, 5)),
		?assertEqual(50.0, health(10, 0, 0, 0))
	end},
	{"X < min < max",
	fun() ->
		?assertEqual(0.0, health(0, 5, 10, -5))
	end},
	{"X > min > max",
	fun() ->
		?assertEqual(0.0, health(10, 5, 0, 15))
	end},
	{"min < max < X",
	fun() ->
		?assertEqual(100.0, health(0, 5, 10, 15))
	end},
	{"max < min < X",
	fun() ->
		?assertEqual(0.0, health(10, 5, 0, 15))
	end}].

data_grooming_test_() ->
	{foreach,
	fun() ->
		mnesia:start(),
		{ok, Cpx} = cpx_monitor:start([{nodes, [node()]}]),
		Cpx
	end,
	fun(_Cpx) ->
		cpx_monitor:stop(),
		mnesia:stop()
	end,
	[fun(Cpx) ->
		{"media previously linked to an agent is cleared on new media",
		fun() ->
			cpx_monitor:set({media, "old"}, [], [{agent, "agent"}]),
			cpx_monitor:set({agent, "decoy"}, [], [{agent, "agent"}]),
			cpx_monitor:subscribe(),
			cpx_monitor:set({media, "new"}, [], [{agent, "agent"}]),
			receive
				{cpx_monitor_event, {drop, {media, "old"}}} ->
					?assert(true);
				{cpx_monitor_event, {drop, Key}} ->
					?DEBUG("Bad drop:  ~p", [Key]),
					?assert("bad drop recieved")
			after 1000 ->
				?assert("no clear received")
			end,
			{ok, [M1 | _] = Medias} = cpx_monitor:get_health(media),
			?assertEqual(1, length(Medias)),
			?assertEqual({media, "new"}, get_key(M1))
		end}
	end]}.

handle_down_test() ->
	Tid = ets:new(?MODULE, []),
	Entries = [
		{{media, "cull1"}, [], [{node, "deadnode"}], util:now()},
		{{media, "keep1"}, [], [{node, "goodnode"}], util:now()}
	],
	ets:insert(Tid, Entries),
	State = #state{ets = Tid},
	% This is an election record, see gen_leader to find out what it is.
	Election = {election, node(), ?MODULE, none, [], [], [], [], [], [], undefined, undefined, [], [], 1, undefined, [], undefined, undefined, all},
	handle_DOWN("deadnode", State, Election),
	?assertEqual([], ets:lookup(Tid, {media, "cull1"})),
	?assertMatch([{{media, "keep1"}, [], [{node, "goodnode"}], _Time}], ets:lookup(Tid, {media, "keep1"})).


%-record(election, {
%          leader = none,
%          name,
%          leadernode = none,
%          candidate_nodes = [],
%          worker_nodes = [],
%          alive = [],
%          down = [],
%          monitored = [],
%          buffered = [],
%          status,
%          elid,
%          acks = [],
%          work_down = [],
%          cand_timer_int,
%          cand_timer,
%          pendack,
%          incarn,
%          nextel,
%          bcast_type              %% all | one. When `all' each election event
%          %% will be broadcast to all candidate nodes.
%         }).
%

time_dependant_test_() ->
	[{"Low health by time",
	fun() ->
		Now = util:now(),
		Out = calc_healths([{"key", {0, 5, 10, {time, Now}}}]),
		?assertEqual([{"key", 0.0}], Out)
	end},
	{"Low mid health by time",
	fun() ->
		Now = util:now(),
		Out = calc_healths([{"key", {0, 5, 10, {time, Now - 3}}}]),
		?assertEqual([{"key", 30.0}], Out)
	end},
	{"mid Health by time",
	fun() ->
		Now = util:now(),
		Out = calc_healths([{"key", {0, 5, 10, {time, Now - 5}}}]),
		?assertEqual([{"key", 50.0}], Out)
	end},
	{"Mid high healthy by time",
	fun() ->
		Now = util:now(),
		Out = calc_healths([{"key", {0, 5, 10, {time, Now - 7}}}]),
		?assertEqual([{"key", 70.0}], Out)
	end},
	{"High health by time",
	fun() ->
		Now = util:now(),
		Out = calc_healths([{"key", {0, 5, 10, {time, Now - 10}}}]),
		?assertEqual([{"key", 100.0}], Out)
	end}].
	
multinode_test_d() ->
	{foreach,
	fun() ->
		[_Name, Host] = string:tokens(atom_to_list(node()), "@"),
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
		
		rpc:call(Master, agent_auth, start, []),
		rpc:call(Slave, agent_auth, start, []),
		
		{Master, Slave}
	end,
	fun({Master, Slave}) ->
		rpc:call(Master, agent_auth, stop, []),
		rpc:call(Slave, agent_auth, stop, []),
		cover:stop([Master, Slave])
	end,
	[fun({Master, Slave}) ->
		{"Happy fun start!",
		fun() ->
			Mrez = rpc:call(Master, cpx_monitor, start, [[{nodes, [Master, Slave]}]]),
			Srez = rpc:call(Slave, cpx_monitor, start, [[{nodes, [Master, Slave]}]]),
			?INFO("Mrez  ~p", [Mrez]),
			?INFO("Srez ~p", [Srez]),
			?assertNot(Mrez =:= Srez),
			?assertMatch({ok, _Pid}, Mrez),
			?assertMatch({ok, _Pid}, Srez)
		end}
	end]}.
	
-endif.

