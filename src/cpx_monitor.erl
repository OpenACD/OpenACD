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

%% @doc Event dispatcher and limited data mogul.  Messages sent here are
%% forwarded on to the subscribers.  Some events will have it's payload 
%% cached for easy retreival later.

-module(cpx_monitor).
-author(micahw).

-behaviour(gen_leader).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("log.hrl").
-include_lib("stdlib/include/qlc.hrl").

-type(proplist_item() :: atom() | {any(), any()}).
-type(proplist() :: [proplist_item()]).
-type(data_type() :: 'system' | 'node' | 'queue' | 'agent' | 'media').
-type(data_key() :: {data_type(), Name :: string() | atom()}).
-type(time() :: integer() | {integer(), integer(), integer()}).
%-type(set_event_payload() :: {data_key(), proplist(), node()}).
%-type(drop_event_payload() :: data_key()),
%-type(info_event_payload() :: proplist())
%-type(event_struct() :: {data_key(), proplist()}).
%-type(event_message() :: 
%	{'set', time(), set_event_payload()} | 
%	{'drop', time(), data_key()} | 
%	{'info', time(), proplist()}).
%-type(item_monitored() :: 'none' | pid() | atom() | {atom(), atom()}).
%-type(monitor_reference() :: reference()).
%-type(cached_event() :: {data_key(), proplist(), node(), time(), item_monitored(), monitor_reference()}).

%% API
-export([
	start_link/1,
	start/1,
	stop/0,
	set/2,
	set/3,
	drop/1,
	info/1,
	subscribe/0,
	subscribe/1,
	unsubscribe/0,
	get_key/1,
	clear_dead_media/0
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

%% test helper for other modules that rely on this.
-ifdef(TEST).
-export([
	make_mock/0,
	make_mock/1,
	add_mocks/1,
	add_set/1,
	add_drop/1,
	add_info/1,
	add_message/1,
	add_message/2,
	assert_mock/0,
	stop_mock/0
]).
-endif.

-record(state, {
	nodes = [] :: [atom()],
	monitoring = [] :: [atom()],
	down = [] :: [atom()],
	auto_restart_mnesia = true :: 'true' | 'false',
	status = stable :: 'stable' | 'merging' | 'split',	
	splits = [] :: [{atom(), integer()}],
	merge_status = none :: 'none' | any(),
	merging = none :: 'none' | [atom()],
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
    gen_leader:start_link(?MODULE, Nodes, [{heartbeat, 1}], ?MODULE, Args, []).

%% @doc See {@link start_link/1}
-spec(start/1 :: (Args :: options()) -> {'ok', pid()}).
start(Args) ->
	Nodes = lists:flatten(proplists:get_all_values(nodes, Args)),
	gen_leader:start(?MODULE, Nodes, [{heartbeat, 1}], ?MODULE, Args, []).

%% @doc Stops the monitor.
-spec(stop/0 :: () -> any()).
stop() ->
	gen_leader:call(?MODULE, stop).

%% @doc Creates an entry into the cache using the given key and params.  
%% Existing entrys are overwritten.  If the params list does not contain an
%% entry key of `node', `{node, node()}' is prepended to it.
-spec(set/2 :: (Key :: data_key(), Params :: proplist()) -> 'ok').
set({_Type, _Name} = Key, Params) ->
	set(Key, Params, none).

%% @doc Same as above, but sets up monitoring of the passed pid or node so 
%% if it dies, a `drop' event is automcatically generated and sent out for 
%% the given `Key'.
-spec(set/3 :: (Key :: data_key(), Params :: proplist(), pid() | atom()) -> 'ok').
set({_Type, _Name} = Key, Params, WatchMe) ->
	{Node, RealParams} = case proplists:get_value(node, Params) of
		undefined ->
			{node(), [{node, node()} | Params]};
		Else ->
			{Else, Params}
	end,
	gen_leader:leader_cast(?MODULE, {set, os:timestamp(), {Key, RealParams, Node}, WatchMe}).

%% @doc Remove the entry with the key `Key' from being tracked.  If 
%% monitoring was set up, it's halted as well.
-spec(drop/1 :: (Key :: data_key()) -> 'ok').
drop({_Type, _Name} = Key) ->
	gen_leader:leader_cast(?MODULE, {drop, os:timestamp(), Key}).

%% @doc Forward a general message to the subscribers.  There is no caching
%% or monitoring done for this.
-spec(info/1 :: (Params :: proplist()) -> 'ok').
info(Params) ->
	%?DEBUG("infoing initial:  ~p", [Params]),
	gen_leader:leader_cast(?MODULE, {info, os:timestamp(), Params}).

%% @doc Subscribe the calling process to all events from ?MODULE.
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

%% @doc Unsubscribe the calling process to events from ?MODULE.
-spec(unsubscribe/0 :: () -> 'ok').
unsubscribe() ->
	Pid = self(),
	gen_leader:leader_cast(?MODULE, {unsubscribe, Pid}).

%% @doc Convience function to get the key from a health tuple.
-spec(get_key/1 :: (Tuple :: 
	{data_key(), time(), proplist()} | 
	{data_key(), time(), proplist()}) -> data_key()).
get_key({Key, _, _, _}) ->
	Key;
get_key({Key, _, _}) ->
	Key.
-spec(clear_dead_media/0 :: () -> 'ok').
clear_dead_media() ->
	gen_leader:leader_cast(?MODULE, clear_dead_media).

%% =====
%% test helpers for other modules
%% =====

-ifdef(TEST).

make_mock() ->
	make_mock([]).

make_mock(Events) ->
	{ok, Mock} = gen_leader_mock:start(?MODULE),
	add_mocks(Events),
	{ok, Mock}.

add_mocks([]) ->
	ok;
add_mocks([{set, Key, Params} | T]) ->
	add_set({Key, Params, none}),
	add_mocks(T);
add_mocks([{set, Key, Params, Watch} | T]) ->
	add_set({Key, Params, Watch}),
	add_mocks(T);
add_mocks([{drop, Key} | T]) ->
	add_drop(Key),
	add_mocks(T);
add_mocks([{info, Params} | T]) ->
	add_info(Params),
	add_mocks(T);
add_mocks([{message, Msg} | T]) ->
	add_message(Msg),
	add_mocks(T);
add_mocks([{message, Msg, Fun} | T]) ->
	add_message(Msg, Fun),
	add_mocks(T).

add_set({Key, Params, Watch}) ->
	gen_leader_mock:expect_leader_cast(whereis(cpx_monitor),
		fun({set, _Time, {InKey, InParams, _Node}, InWatch}, _State, _Election) ->
			InKey = Key,
			case Watch of
				ignore ->
					ok;
				InWatch ->
					ok;
				_ ->
					Watch = InWatch
			end,
			lists:all(fun(E) -> lists:member(E, InParams) end, Params),
			ok
		end
	);
add_set({Key, Params}) ->
	add_set({Key, Params, none}).

add_drop(Key) ->
	gen_leader_mock:expect_leader_cast(whereis(cpx_monitor), 
		fun({drop, _Time, AKey}, _State, _Election) ->
			Key = AKey,
			ok
		end
	).

add_info(Params) ->
	gen_leader_mock:expect_leader_cast(whereis(cpx_monitor),
		fun({info, _Time, TheParams}, _State, _Election) ->
			lists:all(fun(E) -> lists:memeber(E, TheParams) end, Params),
			ok
		end
	).

add_message(Msg) ->
	Fun = fun(_Msg, _State) ->
		ok
	end,
	add_message(Msg, Fun).

add_message(Msg, Fun) ->
	gen_leader_mock:expect_info(whereis(cpx_monitor), Fun).

assert_mock() ->
	gen_leader_mock:assert_expectations(whereis(cpx_monitor)).

stop_mock() ->
	gen_leader_mock:stop(whereis(cpx_monitor)).

-endif.
%%====================================================================
%% gen_server callbacks
%%====================================================================
%% @hidden
init(Args) when is_list(Args) ->
	process_flag(trap_exit, true),
	Nodes = lists:flatten(proplists:get_all_values(nodes, Args)),
	Mons = Nodes,
	ets:new(?MODULE, [named_table]),
    {ok, #state{
		nodes = Nodes, 
		monitoring = Mons,
		auto_restart_mnesia = proplists:get_value(auto_restart_mnesia, Args, true)
	}}.

%% @hidden
elected(State, Election, Node) -> 
	?INFO("elected by ~w", [Node]),
	% what was down and is now up?
	Cands = gen_leader:candidates(Election),
	lists:foreach(fun(Cnode) ->
		ets:insert(?MODULE, {{node, Cnode}, [{down, 100}], [{state, unreported}], Cnode, os:timestamp(), none, undefined})
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
	Event = {{node, node()}, [is_leader], node()},
	Cached = erlang:append_element(erlang:append_element(erlang:append_element(Event, os:timestamp()), none), undefined),
	ets:insert(?MODULE, Cached),
	tell_subs(Event, State#state.subscribers),
	tell_cands(Event, Election),
	
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
surrendered(State, {_Merge, _Stilldown}, Election) ->
%	QH = qlc:q([Tuple || {{Type, _Name}, _Hp, _Data, _Time} = Tuple <- ets:table(?MODULE), Type =:= node]),
%	Matches = qlc:e(QH),
%	F = fun({{node, Name} = Key, Hp, _Data, _Time}) ->
%		case lists:member(Name, Merge) =:= proplists:get_value(down, Hp) of
%			true ->
%				Now = util:now(),
%				ets:insert(?MODULE, {Key, [{upsince, Now}], [], Now}),
%				ok;
%			false ->
%				ok
%		end
%	end,
%	lists:foreach(F, Matches),
	
	
	Edown = gen_leader:down(Election),
	Ealive = gen_leader:alive(Election),
	
	lists:foreach(fun(Node) -> 
		ets:insert(?MODULE, {{node, Node}, [down], Node, os:timestamp(), none, undefined})
	end, Edown),

	lists:foreach(fun(Node) ->
		ets:insert(?MODULE, {{node, Node}, [{up, util:now()}], Node, os:timestamp(), none, undefined})
	end, Ealive),
	
	gen_leader:leader_cast(?MODULE, {ensure_live, node(), os:timestamp()}),
	
	{ok, State}.
	
%% @hidden
handle_DOWN(Node, State, Election) ->
	?ERROR("Node ~p is down!", [Node]),
	Newsplits = case proplists:get_value(Node, State#state.splits) of
		undefined ->
			[{Node, util:now()} | State#state.splits];
		_Time ->
			State#state.splits
	end,
	Time = os:timestamp(),
	Message = {{node, Node}, [{down, Time}], Node},
	Cached = erlang:append_element(erlang:append_element(erlang:append_element(Message, os:timestamp()), none), undefined),
	ets:insert(?MODULE, Cached),
	tell_cands({set, Message}, Election),
	tell_subs({set, Message}, State#state.subscribers),
	ets:safe_fixtable(?MODULE, true),
	Drops = qlc:e(qlc:q([begin
			ets:delete(?MODULE, Key),
			{drop, {Key, Time}}
		end ||
		{Key, _Time, _Props, WhichNode, _, _} <- ets:table(?MODULE),
		Key =/= {node, Node},
		WhichNode =:= Node
	])),
	ets:safe_fixtable(?MODULE, false),
	[begin
		tell_cands(D, Election),
		tell_subs(D, State#state.subscribers)
	end || D <- Drops],
	{ok, State#state{splits = Newsplits}}.

% =====
% handle_leader_call
% =====

%% @hidden
handle_leader_call({get, When}, _From, State, _Election) when is_integer(When) ->
	Out = qlc:e(qlc:q([X || {_, {MTime, Time, _STime}, _, _, _, _} = X <- ets:table(?MODULE), MTime * 1000000 + Time =< When])),
	{reply, {ok, Out}, State};
handle_leader_call({get, What}, _From, State, _Election) when is_atom(What) ->
	Results = qlc:e(qlc:q([X || {{GetWhat, _}, _, _, _, _, _} = X <- ets:table(?MODULE), GetWhat =:= What])),
	{reply, {ok, Results}, State};
handle_leader_call(Message, From, State, _Election) ->
	?WARNING("received unexpected leader_call ~p from ~p", [Message, From]),
	{reply, ok, State}.

% =====
% handle_leader_cast
% =====

%% @hidden
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
	unlink(Pid),
	Newsubs = proplists:delete(Pid, Subs),
	{noreply, State#state{subscribers = Newsubs}};
handle_leader_cast({reporting, Node}, State, Election) ->
	Time = os:timestamp(),
	Event = {{node, Node}, [{state, reported}], Node},
	cache_event(Event, Time, none, undefined),
	tell_cands({set, Time, Event}, Election),
	tell_subs({set, Time, Event}, State#state.subscribers),
	{noreply, State};
handle_leader_cast({drop, Time, Key} = Msg, State, Election) ->
	case qlc:e(qlc:q([ X || {DahKey, _, _, _, _, _} = X <- ets:table(?MODULE), DahKey =:= Key])) of
		[] ->
			% no need to do ets updates, or change monitoring
			ok;
		[{Key, _, _, _, none, _}] ->
			ets:delete(?MODULE, Key);
		[{Key, _, _, _, _Monitoring, Monref}] ->
			erlang:demonitor(Monref),
			ets:delete(?MODULE, Key)
	end,
	tell_subs(Msg, State#state.subscribers),
	tell_cands(Msg, Election),
	{noreply, State};
handle_leader_cast({info, Time, Params} = Msg, State, _Election) ->
	tell_subs(Msg, State#state.subscribers),
	%% no ets need updating, so not telling cands.
	{noreply, State};
handle_leader_cast({set, Time, {Key, Details, Node} = Event, Watchwhat}, State, Election) ->
	case qlc:e(qlc:q([X || {DahKey, _, _, _, _, _} = X <- ets:table(?MODULE), DahKey =:= Key])) of
		[] ->
			Monref = case Watchwhat of
				none ->
					undefined;
				_ ->
					erlang:monitor(process, Watchwhat)
			end,
			cache_event(Event, Time, Watchwhat, Monref);
		[{Key, _, _, _, Watchwhat, Monref}] ->
			cache_event(Event, Time, Watchwhat, Monref);
		[{Key, _, _, _, none, _}] ->
			NewMonref = erlang:monitor(process, Watchwhat),
			cache_event(Event, Time, Watchwhat, NewMonref);
		[{Key, _, _, _, _OtherWatched, Monref}] ->
			% the other one likely died.
			erlang:demonitor(Monref),
			NewMonref = erlang:monitor(process, Watchwhat),
			cache_event(Event, Time, Watchwhat, NewMonref)
	end,
	tell_cands({set, Time, Event}, Election),
	tell_subs({set, Time, Event}, State#state.subscribers),
	{noreply, State};
handle_leader_cast({ensure_live, Node, Time}, State, Election) ->
	Alive = gen_leader:alive(Election),
	case lists:member(Node, Alive) of
		true ->
			?DEBUG("Node ~w is in election alive list", [Node]);
		false ->
			?WARNING("Node ~w does not appear in the election alive list", [Node])
	end,
	Message = {{node, Node}, [{up, Time}], Node},
	cache_event(Message, Time, none, undefined),
	tell_cands({set, Time, Message}, Election),
	tell_subs({set, Time, Message}, State#state.subscribers),
	{noreply, State};
handle_leader_cast(clear_dead_media, State, Election) ->
	Dropples = qlc:e(qlc:q([Id || 
		{{Type, Id}, _Time, Details, _, _, _} <- ets:table(?MODULE), 
		Type == media, 
		begin 
			Module = case proplists:get_value(type, Details) of
				voice ->
					freeswitch_media_manager;
				email ->
					email_media_manager;
				dummy ->
					dummy_media_manager;
				_ ->
					false
			end,
			case Module of
				false ->
					false;
				_ ->
					Module:get_media(Id) =:= none
			end
		end
	])),
	Time = os:timestamp(),
	ets:safe_fixtable(?MODULE),
	[begin
		ets:delete(?MODULE, {media, Id}),
		Message = {drop, Time, {media, Id}},
		tell_cands(Message, Election),
		tell_subs(Message, State#state.subscribers)
	end || Id <- Dropples],
	{noreply, State};
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
handle_info({leader_event, Message}, State) ->
	?DEBUG("Got message leader_event ~p", [Message]),
	case Message of
		{drop, Key, _Time} ->
			ets:delete(?MODULE, Key),
			{noreply, State};
		{set, Entry} ->
			ets:insert(?MODULE, Entry),
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
handle_info({'DOWN', Monref, process, WatchWhat, Why}, State) ->
	?INFO("Down message for reference ~p of ~p due to ~p", [Monref, WatchWhat, Why]),
	case qlc:e(qlc:q([Key || 
		{Key, _Time, _, _, TestWatchWhat, TestMonref} <- ets:table(?MODULE), 
		TestWatchWhat =:= WatchWhat, 
		TestMonref =:= Monref])) of
		[] ->
			%% late down message.
			ok;
		[Akey] ->
			drop(Akey)
	end,
	{noreply, State};
handle_info({'EXIT', From, Reason}, #state{subscribers = Subs} = State) ->
	?INFO("~p said it died due to ~p.", [From, Reason]),
	Newsubs = proplists:delete(From, Subs),
	{noreply, State#state{subscribers = Newsubs}};
handle_info(dump_subs, #state{subscribers = Subs} = State) ->
	?DEBUG("Subs:  ~p", [Subs]),
	{noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @hidden
terminate(Reason, _State) ->
	?INFO("~s going down for ~p", [?MODULE, Reason]),
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

restart_mnesia(Nodes) ->
	F = fun() ->
		?WARNING("automatically restarting mnesia on formerly split nodes: ~p", [Nodes]),
		lists:foreach(fun(N) -> 
			case net_adm:ping(N) of
				pong ->
					S = rpc:call(N, mnesia, stop, [], 1000), 
					?DEBUG("stoping mnesia on ~w got ~p", [N, S]),
					G = rpc:call(N, mnesia, start, [], 1000),
					?DEBUG("Starting mnesia on ~w got ~p", [N, G]);
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

cache_event(Event, Time, WatchWhat, WatchRef) ->
	Cache = append_elements(Event, [Time, WatchWhat, WatchRef]),
	ets:insert(?MODULE, Cache).

append_elements(Tuple, []) ->
	Tuple;
append_elements(Tuple, [H | T]) ->
	append_elements(erlang:append_element(Tuple, H), T).

%% spawning each message out so if the fun fails or crashes, it won't halt
%% messages to other subscribers.  Also, means the filter funs aren't 
%% running in ?MODULE's thread.
tell_subs(Message, Subs) ->
	[spawn(fun() -> tell_sub_fun(Message, Pid, Fun) end) ||
	{Pid, Fun} <- Subs].

tell_sub_fun(Message, Pid, Fun) ->
	case Fun(Message) of
		true ->
			Pid ! {cpx_monitor_event, Message};
		false ->
			ok
	end.

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
	{?MODULE, Node} ! {leader_event, Message},
	tell_cands(Message, Tail, Leader).

-ifdef(TEST).

%data_grooming_test_() ->
%	{foreach,
%	fun() ->
%		mnesia:start(),
%		{ok, Cpx} = ?MODULE:start([{nodes, [node()]}]),
%		Cpx
%	end,
%	fun(_Cpx) ->
%		?MODULE:stop(),
%		mnesia:stop()
%	end,
%	[fun(Cpx) ->
%		{"media previously linked to an agent is cleared on new media",
%		fun() ->
%			?MODULE:set({media, "old"}, [], [{agent, "agent"}]),
%			?MODULE:set({agent, "decoy"}, [], [{agent, "agent"}]),
%			?MODULE:subscribe(),
%			?MODULE:set({media, "new"}, [], [{agent, "agent"}]),
%			receive
%				{?MODULE_event, {drop, {media, "old"}, _Time}} ->
%					?assert(true);
%				{?MODULE_event, {drop, Key}, _Time} ->
%					?DEBUG("Bad drop:  ~p", [Key]),
%					?assert("bad drop recieved")
%			after 1000 ->
%				?assert("no clear received")
%			end,
%			{ok, [M1 | _] = Medias} = ?MODULE:get_health(media),
%			?assertEqual(1, length(Medias)),
%			?assertEqual({media, "new"}, get_key(M1))
%		end}
%	end]}.

 handle_down_test() -> 
	ets:new(?MODULE, [named_table]),
	Entries = [
		{{media, "cull1"}, os:timestamp(), [{node, "deadnode"}], "deadnode", none, undefined},
		{{media, "keep1"}, os:timestamp(), [{node, "goodnode"}], "goodnode", none, undefined}
	],
	ets:insert(?MODULE, Entries),
	State = #state{}, 
	% This is an election record, see gen_leader to find out what it is
	% as the copy/pasta record below may be out of date
	Election = {election, node(), none, ?MODULE, node(), [], [], [], [], [],  none, undefined, undefined, [], [], 1, undefined, undefined, undefined, undefined, all},
	handle_DOWN("deadnode", State, Election),
	?assertEqual([], ets:lookup(?MODULE, {media, "cull1"})),
	?assertMatch([{{media, "keep1"}, _Time, [{node, "goodnode"}], "goodnode", none, undefined}], ets:lookup(?MODULE, {media, "keep1"})),
	ets:delete(?MODULE).

%-record(election, {
%          leader = none,
%          previous_leader = none,
%          name,
%          leadernode = none,
%          candidate_nodes = [],
%          worker_nodes = [],
%          down = [],
%          monitored = [],
%          buffered = [],
%          seed_node = none,
%          status,
%          elid,
%          acks = [],
%          work_down = [],
%          cand_timer_int,
%          cand_timer,
%          pendack,
%          incarn,
%          nextel,
%          %% all | one. When `all' each election event
%          %% will be broadcast to all candidate nodes.
%          bcast_type
%         }).

sub_mock() ->
	sub_mock(fun(_) -> true end).

sub_mock(Fun) ->
	{ok, Mock} = gen_server_mock:new(),
	%gen_server_mock:expect_info(Mock, fun(goober, _) ->
	%	?DEBUG("Mon:  ~p; me:  ~p", [whereis(?MODULE), self()]),
	%	ok
	%end),
	%Mock ! goober,
	gen_server_mock:expect_info(Mock, fun({sub, TheFun}, _) ->
		?DEBUG("mon:  ~p;  me:  ~p", [whereis(?MODULE), self()]),
		cpx_monitor:subscribe(TheFun),
		ok
	end),
	Mock ! {sub, Fun},
	{ok, Mock}.

mock_test_then_die([]) ->
	ok;
mock_test_then_die([H | T]) ->
	gen_server_mock:assert_expectations(H),
	gen_server_mock:stop(H),
	mock_test_then_die(T).

subscribers_test_() ->
	{foreach,
	fun() ->
		{ok, CpxMon} = cpx_monitor:start([{nodes, node()}]),
		ok
	end,
	fun(ok) ->
		cpx_monitor:stop()
	end,
	[fun(ok) -> {"simple info", fun() ->
		cpx_monitor:subscribe(),
		cpx_monitor:info([{<<"key">>, <<"value">>}]),
		receive
			{cpx_monitor_event, {info, _Time, [{<<"key">>, <<"value">>}]}} ->
				?assert(true);
			{cpx_monitor_event, Other} ->
				?assert(Other)
		after 20 ->
			?assert("timeout")
		end 
	end} end,
	fun(ok) -> {"simple set", fun() ->
		cpx_monitor:subscribe(),
		cpx_monitor:set({media, "media1"}, [{<<"key">>, <<"val">>}]),
		receive
			{cpx_monitor_event, {set, _Time, {{media, "media1"}, [{node, _Node}, {<<"key">>, <<"val">>}], _Node}}} ->
				?assert(true);
			{cpx_monitor_event, Other} ->
				?assert(Other)
		after 20 ->
			?assert("timeout")
		end
	end} end,
	fun(ok) -> {"getting a drop", fun() ->
		cpx_monitor:subscribe(),
		cpx_monitor:drop({media, "dead"}),
		receive
			{cpx_monitor_event, {drop, _Time, {media, "dead"}}} ->
				?assert(true);
			{cpx_monitor_event, Other} ->
				?assert(Other)
		after 20 ->
			?assert("timeout")
		end
	end} end,
	fun(ok) -> {"Filtering out infos", fun() ->
		Filter = fun(E) ->
			?DEBUG("filtering ~p", [E]),
			case E of
				{info, _, _} ->
					false;
				_ ->
					true
			end
		end,
		cpx_monitor:subscribe(Filter),
		cpx_monitor:info([{<<"key">>, <<"value">>}]),
		Good = receive
			{cpx_monitor_event, {info, _, _}} ->
				false;
			Other ->
				Other
		after 20 ->
			true
		end,
		?assert(Good)
	end} end,
	fun(ok) -> {"gen_server_mock test", fun() ->
		{ok, Mock} = sub_mock(),
		timer:sleep(10),
		gen_server_mock:expect_info(Mock, fun({cpx_monitor_event, _}, _) ->
			ok
		end),
		cpx_monitor:info([]),
		timer:sleep(10),
		mock_test_then_die([Mock])
	end} end,
	fun(ok) -> {"a bad filter tolerance", fun() ->
		BadFilter = fun(_) ->
			exit(<<"I suck">>)
		end,
		{ok, GoodMock} = sub_mock(),
		{ok, BadMock} = sub_mock(BadFilter),
		%% sleep to give the monitor time to set up the subs.
		timer:sleep(10),
		gen_server_mock:expect_info(GoodMock, fun({cpx_monitor_event, {info, _, _}}, _) ->
			ok
		end),
		cpx_monitor:info([{<<"key">>, <<"val">>}]),
		timer:sleep(10),
		mock_test_then_die([GoodMock, BadMock])
	end} end,
	fun(ok) -> {"a monitored process dies, causing a drop", fun() ->
		{ok, Watched} = gen_server_mock:new(),
		{ok, Watcher} = sub_mock(),
		gen_server_mock:expect_info(Watcher, fun({cpx_monitor_event, {set, _Time, {{media, "hi"}, _Params, _Node}}}, _) ->
			?DEBUG("got the set.", []),
			ok
		end),
		gen_server_mock:expect_info(Watcher, fun({cpx_monitor_event, {drop, _Time, {media, "hi"}}}, _) ->
			?DEBUG("Got the drop", []),
			ok
		end),
		timer:sleep(10),
		cpx_monitor:set({media, "hi"}, [], Watched),
		gen_server_mock:stop(Watched),
		timer:sleep(10),
		mock_test_then_die([Watcher])
	end} end]}.

ets_test_() ->
	{foreach,
	fun() ->
		{ok, _} = cpx_monitor:start([{nodes, node()}]),
		ok
	end,
	fun(ok) ->
		cpx_monitor:stop()
	end,
	[fun(ok) -> {"simple set", fun() ->
		cpx_monitor:set({media, "hi"}, [{<<"key">>, <<"val">>}]),
		timer:sleep(10),
		Out = qlc:e(qlc:q([X || {Key, _, _, _, none, undefined} = X <- ets:table(?MODULE), Key =:= {media, "hi"}])),
		Node = node(),
		?assertMatch([{{media, "hi"}, [{node, Node}, {<<"key">>, <<"val">>}], Node, _Time, none, undefined}], Out)
	end} end,
	fun(ok) -> {"set, reset", fun() ->
		cpx_monitor:set({media, "hi"}, []),
		timer:sleep(10),
		Time = qlc:e(qlc:q([T || {{media, "hi"}, _, _, T, _, _} <- ets:table(?MODULE)])),
		cpx_monitor:set({media, "hi"}, [{"hi", "bye"}]),
		timer:sleep(10),
		[{_Key, NewProps, _, NewTime, _, _}] = qlc:e(qlc:q([X || {{media, "hi"}, _, _, _, _, _} = X <- ets:table(?MODULE)])),
		Node = node(),
		?assertEqual([{node, Node}, {"hi", "bye"}], NewProps),
		?assertNot(NewTime =:= Time)
	end} end,
	fun(ok) -> {"set, drop", fun() ->
		cpx_monitor:set({media, "hi"}, []),
		cpx_monitor:drop({media, "hi"}),
		timer:sleep(10),
		Res = qlc:e(qlc:q([X || {{media, "hi"}, _, _, _, _, _} = X <- ets:table(?MODULE)])),
		?assertEqual([], Res)
	end} end,
	fun(ok) -> {"set, reset, die", fun() ->
		{ok, Mock} = gen_server_mock:new(),
		cpx_monitor:set({media, "hi"}, []),
		timer:sleep(10),
		Time = qlc:e(qlc:q([T || {{media, "hi"}, _, _, T, _, _} <- ets:table(?MODULE)])),
		cpx_monitor:set({media, "hi"}, [{"hi", "bye"}], Mock),
		timer:sleep(10),
		[{_Key, NewProps, _, NewTime, Mock, _Ref}] = qlc:e(qlc:q([X || {{media, "hi"}, _, _, _, _, _} = X <- ets:table(?MODULE)])),
		gen_server_mock:stop(Mock),
		timer:sleep(10),
		Res = qlc:e(qlc:q([X || {{media, "hi"}, _, _, _, _, _} = X <- ets:table(?MODULE)])),
		Node = node(),
		?assertEqual([{node, node()}, {"hi", "bye"}], NewProps),
		?assertNot(Time =:= NewTime),
		?assertEqual([], Res)
	end} end]}.
		
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
			Mrez = rpc:call(Master, ?MODULE, start, [[{nodes, [Master, Slave]}]]),
			Srez = rpc:call(Slave, ?MODULE, start, [[{nodes, [Master, Slave]}]]),
			?INFO("Mrez  ~p", [Mrez]),
			?INFO("Srez ~p", [Srez]),
			?assertNot(Mrez =:= Srez),
			?assertMatch({ok, _Pid}, Mrez),
			?assertMatch({ok, _Pid}, Srez)
		end}
	end]}.

-ifdef(PROFILE).

avg(Acc) ->
	Sum = lists:foldl(fun(X, S) -> S + X end, 0, Acc),
	Sum / length(Acc).

tdiff({InMeg, InSec, InMic}, {GotMeg, GotSec, GotMic}) ->
	InTime = InMeg * 1000000 + InSec + InMic / 1000000,
	GotTime = GotMeg * 1000000 + GotSec + GotMic / 1000000,
	GotTime - InTime.

profile_test_() ->
	{foreach,
	fun() ->
		case whereis(cpx_monitor) of
			undefined ->
				ok;
			Pid ->
				unregister(cpx_monitor),
				exit(Pid, kill)
		end,
		{ok, Mon} = cpx_monitor:start([{nodes, node()}]),
		Mon
	end,
	fun(_Mon) ->
		cpx_monitor:stop()
	end,
	[fun(_Mon) -> {timeout, 60, {"info message with little data", fun() ->
		cpx_monitor:subscribe(),
		Acc = [begin
			cpx_monitor:info([{"info", X}]),
			receive
				{cpx_monitor_event, {info, InTime, [{"info", X}]}} ->
					tdiff(InTime, os:timestamp())
			end
		end || X <- lists:seq(1, 1000)],
		?INFO("Average time:  ~f", [avg(Acc)])
	end}} end,
	fun(_Mon) -> {timeout, 60, {"info message with lots of data", fun() ->
		AtoZ = "abcdefghijklmnopqrstuvwxyz",
		MakeData = fun() ->
			[{make_ref(), X} || X <- AtoZ]
		end,
		cpx_monitor:subscribe(),
		Acc = [begin
			Data = [{"nth", X} | MakeData()],
			cpx_monitor:info(Data),
			receive
				{cpx_monitor_event, {info, InTime, _Data}} ->
					tdiff(InTime, os:timestamp())
			end
		end || X <- lists:seq(1, 1000)],
		?INFO("Average time:  ~f", [avg(Acc)])
	end}} end,
	fun(_Mon) -> {timeout, 60, {"alternating sets and drops, no data", fun() ->
		cpx_monitor:subscribe(),
		Acc = [begin
			case X rem 2 of
				1 ->
					cpx_monitor:set({agent, "1"}, []);
				0 ->
					cpx_monitor:drop({agent, "1"})
			end,
			receive
				{cpx_monitor_event, {_, InTime, _Data}} ->
					tdiff(InTime, os:timestamp())
			end
		end || X <- lists:seq(1, 1000)],
		?INFO("Average time:  ~f", [avg(Acc)])
	end}} end]}.

-endif.
	
-endif.

