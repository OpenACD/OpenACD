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
-define(STANDARD_TEST, true).
-include_lib("eunit/include/eunit.hrl").
-endif.
-ifdef(PROFILE).
-include_lib("eunit/include/eunit.hrl").
-endif.
-ifdef(PROFILE).
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
%-type(info_event_payload() :: any()}).
%-type(event_struct() :: {data_key(), proplist()}).
%-type(event_message() :: 
%	{'set', time(), set_event_payload()} | 
%	{'drop', time(), data_key()} | 
%	{'info', time(), info_event_payload()}).
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
	gen_leader:start_link(?MODULE, Nodes, [{heartbeat, 1}, {vardir, util:run_dir()}], ?MODULE, Args, []).

%% @doc See {@link start_link/1}
-spec(start/1 :: (Args :: options()) -> {'ok', pid()}).
start(Args) ->
	Nodes = lists:flatten(proplists:get_all_values(nodes, Args)),
	gen_leader:start(?MODULE, Nodes, [{heartbeat, 1}, {vardir, util:run_dir()}], ?MODULE, Args, []).

%% @doc Stops the monitor.
-spec(stop/0 :: () -> any()).
stop() ->
	gen_leader:call(?MODULE, stop).

%% @doc Creates an entry into the cache using the given key and params.  
%% Existing entrys are overwritten.  If the params list does not contain an
%% entry key of `node', `{node, node()}' is prepended to it.
-spec(set/2 :: (Key :: data_key(), Params :: proplist()) -> 'ok').
set({_Type, _Name} = Key, Params) ->
	set(Key, Params, ignore).

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
-spec(info/1 :: (Params :: any()) -> 'ok').
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
	Time = os:timestamp(),
	Event = {set, Time, {{node, node()}, [is_leader], node()}},
	Cached = erlang:append_element(erlang:append_element(erlang:append_element(Event, Time), none), undefined),
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
	tell_cands({set, os:timestamp(), Message}, Election),
	tell_subs({set, os:timestamp(), Message}, State#state.subscribers),
	ets:safe_fixtable(?MODULE, true),
	Drops = qlc:e(qlc:q([begin
			ets:delete(?MODULE, Key),
			{drop, os:timestamp(), Key}
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
handle_leader_cast({drop, _Time, Key} = Msg, State, Election) ->
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
handle_leader_cast({info, _Time, _Params} = Msg, State, _Election) ->
	tell_subs(Msg, State#state.subscribers),
	%% no ets need updating, so not telling cands.
	{noreply, State};
handle_leader_cast({set, Time, {Key, Details, Node} = Event, ignore}, State, Election) ->
	NewElements = [
		{2, Details},
		{3, Node},
		{4, Time}
	],
	ets:update_element(?MODULE, Key, NewElements),
	tell_cands({set, Time, Event}, Election),
	tell_subs({set, Time, Event}, State#state.subscribers),
	{noreply, State};
handle_leader_cast({set, Time, {Key, _Details, _Node} = Event, Watchwhat}, State, Election) ->
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
			NewMonref = case Watchwhat of
				none -> undefined;
				_ -> erlang:monitor(process, Watchwhat)
			end,
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
	% TODO This should not reference media modules directly.  Fix it to be
	% generic.
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
		{drop, _Time, Key} ->
			ets:delete(?MODULE, Key),
			{noreply, State};
		{set, Time, {Key, Data, Node}} ->
			ets:insert(?MODULE, {Key, Data, Node, Time, none, undefined}),
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

-ifdef(STANDARD_TEST).

public_api_test_() ->
	{foreach, fun() ->
		meck:new(gen_leader)
	end,
	fun(_) ->
		meck:unload(gen_leader)
	end, [
	fun(_) -> {"start_link/1", fun() ->
		meck:expect(gen_leader, start_link, fun(?MODULE, [node1, node2], [{heartbeat, 1}, {vardir, _}], ?MODULE, [{nodes, node1}, auto_restart_mnesia, {nodes, node2}], []) ->
			{ok, util:zombie()}
		end),
		Opts = [
			{nodes, node1},
			auto_restart_mnesia,
			{nodes, node2}
		],
		?assertMatch({ok, _}, start_link(Opts)),
		?assertEqual(1, length(meck:history(gen_leader))),
		?assert(meck:validate(gen_leader))
	end} end,

	fun(_) -> {"start/1", fun() ->
		meck:expect(gen_leader, start, fun(?MODULE, [node1, node2], [{heartbeat, 1}, {vardir, _}], ?MODULE, [{nodes, node1}, auto_restart_mnesia, {nodes, node2}], []) ->
			{ok, util:zombie()}
		end),
		Opts = [
			{nodes, node1},
			auto_restart_mnesia,
			{nodes, node2}
		],
		?assertMatch({ok, _}, start(Opts)),
		?assertEqual(1, length(meck:history(gen_leader))),
		?assert(meck:validate(gen_leader))
	end} end,

	fun(_) -> {"stop/0", fun() ->
		meck:expect(gen_leader, call, fun(?MODULE, stop) -> ok end),
		stop(),
		?assert(meck:validate(gen_leader)),
		?assertEqual(1, length(meck:history(gen_leader)))
	end} end,
			
	fun(_) -> {"set/2", fun() ->
		meck:expect(gen_leader, leader_cast, fun(?MODULE, {set, {_, _, _}, {{type, "name"}, [{node, Node}, {p1, v1}], Node}, ignore}) -> 
			?assertEqual(node(), Node),
			ok
		end),
		ok = set({type, "name"}, [{p1, v1}]),
		?assertEqual(1, length(meck:history(gen_leader)))
	end} end,

	fun(_) -> {"set/2, node given", fun() ->
		meck:expect(gen_leader, leader_cast, fun(?MODULE, {set, {_, _, _}, {{type, "name"}, [{node, "node"}, {p1, v1}], "node"}, ignore}) -> 
			ok
		end),
		ok = set({type, "name"}, [{node, "node"}, {p1, v1}]),
		?assertEqual(1, length(meck:history(gen_leader)))
	end} end,

	fun(_) -> {"set/3", fun() ->
		meck:expect(gen_leader, leader_cast, fun(?MODULE, {set, {_, _, _}, {{type, "name"}, [{node, Node}, {p1, v1}], Node}, dowatch}) ->
			?assertEqual(node(), Node),
			ok
		end),
		ok = set({type, "name"}, [{p1, v1}], dowatch),
		?assertEqual(1, length(meck:history(gen_leader)))
	end} end,

	fun(_) -> {"drop/1", fun() ->
		meck:expect(gen_leader, leader_cast, fun(?MODULE, {drop, {_,_,_}, {type, "name"}}) -> ok end),
		ok = drop({type, "name"}),
		?assertEqual(1, length(meck:history(gen_leader)))
	end} end,

	fun(_) -> {"info/1", fun() ->
		meck:expect(gen_leader, leader_cast, fun(?MODULE, {info, {_,_,_}, [{p1,v1}]}) -> ok end),
		ok = info([{p1, v1}]),
		?assertEqual(1, length(meck:history(gen_leader)))
	end} end,

	fun(_) -> {"subscribe/0", fun() ->
		Self = self(),
		meck:expect(gen_leader, leader_cast, fun(?MODULE, {subscribe, Pid, Fun}) ->
			?assertEqual(Self, Pid),
			[?assert(Fun(N)) || N <- lists:seq(1, 100)],
			ok
		end),
		ok = subscribe(),
		?assertEqual(1, length(meck:history(gen_leader)))
	end} end,

	fun(_) -> {"subscribe/1", fun() ->
		T = fun(_) -> false end,
		Self = self(),
		meck:expect(gen_leader, leader_cast, fun(?MODULE, {subscribe, Pid, Fun}) ->
			?assertEqual(Self, Pid),
			?assertEqual(T, Fun)
		end),
		ok = subscribe(T),
		?assertEqual(1, length(meck:history(gen_leader)))
	end} end,

	fun(_) -> {"unsubscribe/0", fun() ->
		Self = self(),
		meck:expect(gen_leader, leader_cast, fun(?MODULE, {unsubscribe, Pid}) ->
			?assertEqual(Self, Pid)
		end),
		ok = unsubscribe(),
		?assertEqual(1, length(meck:history(gen_leader)))
	end} end,

	fun(_) -> {"clear_dead_media/0", fun() ->
		meck:expect(gen_leader, leader_cast, fun(?MODULE, clear_dead_media) ->
			ok
		end),
		ok = clear_dead_media(),
		?assertEqual(1, length(meck:history(gen_leader)))
	end} end

	]}.

callback_test_() -> [
	{"init", fun() ->
		ExpectedState = #state{
			nodes = [node1, node2, node3],
			monitoring = [node1, node2, node3],
			auto_restart_mnesia = true
		},
		Opts = [{nodes, [node1, node2]}, auto_restart_mnesia, {nodes, node3}],
		?assertEqual({ok, ExpectedState}, init(Opts)),
		ets:delete(?MODULE)
	end},

	{"elected", [
		{"all up, no splits", fun() ->
			meck:new(gen_leader),
			meck:expect(gen_leader, candidates, fun(_) ->
				[node(), node1, node2]
			end),
			meck:expect(gen_leader, down, fun(_) -> [] end),
			meck:expect(gen_leader, leader_node, fun(_) -> node() end),
			{ok, State} = init([{nodes, [node(), node1, node2]}]),
			?assertEqual({ok, {[], []}, State}, elected(State, "election", node())),
			?assert(meck:validate(gen_leader)),
			meck:unload(gen_leader),
			ets:delete(?MODULE)
		end},

		{"some down, now up", fun() ->
			meck:new(gen_leader),
			meck:expect(gen_leader, candidates, fun(_) ->
				[node(), node1, node2]
			end),
			meck:expect(gen_leader, down, fun(_) -> [] end),
			meck:expect(gen_leader, leader_node, fun(_) -> node() end),
			{ok, State} = init([{nodes, [node(), node1, node2]}]),
			State0 = State#state{splits = [{node1, 1}]},
			ExpectedState = State#state{status = merging, merge_status = dict:new(), merging = [node1], splits = []},
			?assertEqual({ok, {[{node1, 1}], []}, ExpectedState}, elected(State0, "election", node())),
			?assert(meck:validate(gen_leader)),
			meck:unload(gen_leader),
			ets:delete(?MODULE)
		end},

		{"some up, some down still", fun() ->
			meck:new(gen_leader),
			meck:expect(gen_leader, candidates, fun(_) ->
				[node(), node1, node2]
			end),
			meck:expect(gen_leader, down, fun(_) -> [node2] end),
			meck:expect(gen_leader, leader_node, fun(_) -> node() end),
			{ok, State} = init([{nodes, [node(), node1, node2]}]),
			State0 = State#state{splits = [{node1, 1}, {node2, 2}]},
			ExpectedState = State#state{status = merging, merge_status = dict:new(), merging = [node1], splits = [{node2, 2}]},
			?assertEqual({ok, {[{node1, 1}], [{node2, 2}]}, ExpectedState}, elected(State0, "election", node())),
			?assert(meck:validate(gen_leader)),
			meck:unload(gen_leader),
			ets:delete(?MODULE)
		end}
		
	]},

	{"surrendered", fun() ->
		ets:new(?MODULE, [named_table]),
		meck:new(gen_leader),
		meck:expect(gen_leader, down, fun(_) -> [node1] end),
		meck:expect(gen_leader, alive, fun(_) -> [node()] end),
		meck:expect(gen_leader, leader_cast, fun(?MODULE, {ensure_live, Node, {_,_,_}}) ->
			?assertEqual(node(), Node)
		end),
		?assertEqual({ok, state}, surrendered(state, {merge, stilldown}, election)),
		?assert(meck:validate(gen_leader)),
		?assertEqual(3, length(meck:history(gen_leader))),
		ets:delete(?MODULE),
		meck:unload(gen_leader)
	end},

	{"handle_DOWN", fun() ->
		ets:new(?MODULE, [named_table]),
		meck:new(gen_leader),
		meck:expect(gen_leader, candidates, fun(_) -> [node(), deadnode, goodnode] end),
		meck:expect(gen_leader, leader_node, fun(_) -> node() end),
		Entries = [
			{{media, "cull1"}, os:timestamp(), [{node, deadnode}], deadnode, none, undefined},
			{{media, "keep1"}, os:timestamp(), [{node, goodnode}], goodnode, none, undefined}
		],
		ets:insert(?MODULE, Entries),
		State = #state{}, 
		% This is an election record, see gen_leader to find out what it is
		% as the copy/pasta record below may be out of date
		Election = {election, node(), ?MODULE, node(), [], [], [], [], [],  none, undefined, undefined, [], [], 1, undefined, undefined, undefined, undefined, all},
		handle_DOWN(deadnode, State, Election),
		?assertEqual([], ets:lookup(?MODULE, {media, "cull1"})),
		?assertMatch([{{media, "keep1"}, _Time, [{node, goodnode}], goodnode, none, undefined}], ets:lookup(?MODULE, {media, "keep1"})),
		?assert(meck:validate(gen_leader)),
		meck:unload(gen_leader),
		ets:delete(?MODULE)
	end},

	{"handle_leader_call", [
		{"{get, When}", fun() ->
			ets:new(?MODULE, [named_table]),
			{M,S,Little} = os:timestamp(),
			O1 = {1, {M,S,Little-10}, 1,1,1,1},
			O2 = {1, {M,S,Little+10}, 2,2,2,2},
			ets:insert(?MODULE, [O1,O2]),
			Expected = {reply, {ok, [O2]}, state},
			Now = M * 1000000 + S,
			?assertEqual(Expected, handle_leader_call({get, Now}, from, state, election)),
			ets:delete(?MODULE)
		end},

		{"{get, What}", fun() ->
			ets:new(?MODULE, [named_table]),
			O1 = {{goober, 1}, 1, 1,1,1,1},
			O2 = {{foo, 1}, 2, 2,2,2,2},
			ets:insert(?MODULE, [O1,O2]),
			Expected = {reply, {ok, [O1]}, state},
			?assertEqual(Expected, handle_leader_call({get, goober}, from, state, election)),
			ets:delete(?MODULE)
		end}
	]},

	{"handle_leader_cast", [

		{"{subscribe, Pid, Fun}, pid doesn't exist", fun() ->
			Zombie = util:zombie(),
			F = fun(_) -> true end,
			State = #state{subscribers = []},
			Expected = #state{subscribers = [{Zombie, F}]},
			?assertEqual({noreply, Expected}, handle_leader_cast({subscribe, Zombie, F}, State, election))
		end},

		{"{subscribe, Pid, Fun}, pid does exists", fun() ->
			Zombie = util:zombie(),
			F1 = fun(_) -> true end,
			F2 = fun(_) -> false end,
			State = #state{subscribers = [{Zombie, F1}]},
			Expected = #state{subscribers = [{Zombie, F2}]},
			?assertEqual({noreply, Expected}, handle_leader_cast({subscribe, Zombie, F2}, State, election))
		end},

		{"{unsubscirbe, Pid}", fun() ->
			Zombie = util:zombie(),
			State = #state{subscribers = [{Zombie, afun}]},
			Expected = #state{subscribers = []},
			?assertEqual({noreply, Expected}, handle_leader_cast({unsubscribe, Zombie}, State, election))
		end},

		{"{reporting, Node}", fun() ->
			ets:new(?MODULE, [named_table]),
			meck:new(gen_leader),
			meck:expect(gen_leader, leader_node, fun(_) -> node() end),
			meck:expect(gen_leader, candidates, fun(_) -> [] end),
			?assertEqual({noreply, #state{}}, handle_leader_cast({reporting, node()}, #state{}, election)),
			?assert(meck:validate(gen_leader)),
			meck:unload(gen_leader),
			ets:delete(?MODULE)
		end},

		{"drop tests",
			{setup, fun() ->
				ets:new(?MODULE, [named_table, public]),
				meck:new(gen_leader),
				meck:expect(gen_leader, leader_node, fun(_) -> node() end),
				meck:expect(gen_leader, candidates, fun(_) -> [] end)
			end,
			fun(_) ->
				meck:unload(gen_leader),
				ets:delete(?MODULE)
			end,
			fun(_) -> [
				{"{drop, _Time, Key}, not found", fun() ->
					?assertEqual({noreply, #state{}}, handle_leader_cast({drop, time, key}, #state{}, election)),
					?assert(meck:validate(gen_leader))
				end},

				{"{drop, _Time, Key}, easy drop", fun() ->
					O = {easy_drop, 1, 1, 1, none, 1},
					ets:insert(?MODULE, [O]),
					?assertEqual({noreply, #state{}}, handle_leader_cast({drop, time, easy_drop}, #state{}, election)),
					?assert(meck:validate(gen_leader))
				end},

				{"{drop, _Time, Key}, demonitor", fun() ->
					Zombie = util:zombie(),
					Mon = erlang:monitor(process, Zombie),
					O = {mon_drop, 1, 1, 1, Zombie, Mon},
					ets:insert(?MODULE, [O]),
					?assertEqual({noreply, #state{}}, handle_leader_cast({drop, time, mon_drop}, #state{}, election)),
					?assert(meck:validate(gen_leader))
				end}
			] end}
		},

		{"{info, _Time, Key}", fun() ->
			meck:new(faker),
			meck:expect(faker, didit, fun(_) -> false end),
			Fun = fun faker:didit/1,
			Msg = {info, time, params},
			State = #state{subscribers = [{util:zombie(), Fun}]},
			?assertEqual({noreply, State}, handle_leader_cast(Msg, State, election)),
			% let the spawned tell sups thing die.
			timer:sleep(10),
			?assertEqual(1, length(meck:history(faker))),
			?assert(meck:validate(faker)),
			meck:unload(faker)
		end},

		{"set",
			{setup, fun() ->
				ets:new(?MODULE, [named_table, public])
			end,
			fun(_) ->
				ets:delete(?MODULE)
			end,
			fun(_) ->
				{foreach, fun() ->
					meck:new(gen_leader),
					meck:expect(gen_leader, candidates, fun(_) -> [node()] end),
					meck:expect(gen_leader, leader_node, fun(_) -> [node()] end)
				end,
				fun(_) ->
					ets:delete_all_objects(?MODULE),
					meck:unload(gen_leader)
				end, [

					fun(_) -> {"{set, Time, Event, ignore}", fun() ->
						Msg = {set, time, {key, [], node()}, ignore},
						O = {key, 1, 1, 1, watchme, monref},
						ets:insert(?MODULE, [O]),
						Expect = {key, [], node(), time, watchme, monref},
						?assertEqual({noreply, #state{}}, handle_leader_cast(Msg, #state{}, election)),
						?assertEqual([Expect], ets:lookup(?MODULE, key)),
						?assert(meck:validate(gen_leader))
					end} end,

					fun(_) -> {"{set, Time, Event, Pid}, new pid", fun() ->
						Zombie = util:zombie(),
						Msg = {set, time, {key, [], node()}, Zombie},
						?assertEqual({noreply, #state{}}, handle_leader_cast(Msg, #state{}, election)),
						Node = node(),
						?assertMatch([{key, [], Node, time, Zombie, _Monref}], ets:lookup(?MODULE, key)),
						?assert(meck:validate(gen_leader))
					end} end,

					fun(_) -> {"{set, Time, Event, Pid}, same pid", fun() ->
						Zombie = util:zombie(),
						Monref = monitor(process, Zombie),
						O = {key, 1, 1, 1, Zombie, Monref},
						ets:insert(?MODULE, [O]),
						Msg = {set, time, {key, [], node()}, Zombie},
						?assertEqual({noreply, #state{}}, handle_leader_cast(Msg, #state{}, election)),
						Node = node(),
						?assertEqual([{key, [], Node, time, Zombie, Monref}], ets:lookup(?MODULE, key)),
						?assert(meck:validate(gen_leader))
					end} end,


					fun(_) -> {"{set, Time, Event, Pid}, replacing a pid", fun() ->
						Zombie = util:zombie(),
						Monref = monitor(process, Zombie),
						O = {key, 1, 1, 1, Zombie, Monref},
						ets:insert(?MODULE, [O]),
						NewZombie = util:zombie(),
						Node = node(),
						Msg = {set, time, {key, [], Node}, NewZombie},
						?assertEqual({noreply, #state{}}, handle_leader_cast(Msg, #state{}, election)),
						?assertMatch([{key, [], Node, time, NewZombie, _NewRef}], ets:lookup(?MODULE, key)),
						?assertNotMatch([{_,_,_,_,_,Monref}], ets:lookup(?MODULE, key)),
						?assert(meck:validate(gen_leader))
					end} end,

					fun(_) -> {"{set, Time, Event, Pid}, replacing a none", fun() ->
						O = {key, 1, 1, 1, none, undefined},
						ets:insert(?MODULE, [O]),
						NewZombie = util:zombie(),
						Node = node(),
						Msg = {set, time, {key, [], Node}, NewZombie},
						?assertEqual({noreply, #state{}}, handle_leader_cast(Msg, #state{}, election)),
						?assertMatch([{key, [], Node, time, NewZombie, _NewRef}], ets:lookup(?MODULE, key)),
						?assertNotMatch([{_,_,_,_,none,undefined}], ets:lookup(?MODULE, key)),
						?assert(meck:validate(gen_leader))
					end} end,

					fun(_) -> {"{set, Time, Event, none}, removing", fun() ->
						Zombie = util:zombie(),
						Monref = monitor(process, Zombie),
						O = {key, 1,1,1,Zombie,Monref},
						ets:insert(?MODULE, [O]),
						Node = node(),
						Msg = {set, time, {key, [], Node}, none},
						Expect = {key, [], Node, time, none, undefined},
						?assertEqual({noreply, #state{}}, handle_leader_cast(Msg, #state{}, election)),
						?assertEqual([Expect], ets:lookup(?MODULE, key)),
						?assert(meck:validate(gen_leader))
					end} end
				]}
			end}
		},

		{"{ensure_live, Node, Time}", fun() ->
			ets:new(?MODULE, [named_table]),
			meck:new(gen_leader),
			meck:expect(gen_leader, candidates, fun(_) -> [node()] end),
			meck:expect(gen_leader, leader_node, fun(_) -> node() end),
			meck:expect(gen_leader, alive, fun(_) -> [node()] end),
			?assertEqual({noreply, #state{}}, handle_leader_cast({ensure_live, node(), time}, #state{}, election)),
			?assert(meck:validate(gen_leader)),
			meck:unload(gen_leader),
			ets:delete(?MODULE)
		end}

% TODO This clause has hard-coded media modules, and as such needs to be
% revisted and re-worked.
%		{"clear_dead_media", fun() ->
%			?assert(false)
%		end}
	]},

	{"handle_call, stop", fun() ->
		?assertEqual({stop, normal, ok, #state{}}, handle_call(stop, from, #state{}, election))
	end},

	{"handle_info", [
		{"{leader_event, report}", fun() ->
			meck:new(gen_leader),
			meck:expect(gen_leader, leader_cast, fun(?MODULE, {reporting, Node}) ->
				?assertEqual(node(), Node)
			end),
			?assertEqual({noreply, #state{}}, handle_info({leader_event, report}, #state{})),
			?assert(meck:validate(gen_leader)),
			?assertEqual(1, length(meck:history(gen_leader))),
			meck:unload(gen_leader)
		end},

		{"{leader_event, Message}",
			{setup, fun() ->
				ets:new(?MODULE, [named_table, public])
			end,
			fun(_) ->
				ets:delete(?MODULE)
			end,
			fun(_) -> [

				{"drop", fun() ->
					ets:insert(?MODULE, [{key, value}]),
					?assertEqual({noreply, #state{}}, handle_info({leader_event, {drop, time, key}}, #state{})),	
					?assertEqual([], ets:lookup(?MODULE, key))
				end},

				{"set", fun() ->
					?assertEqual({noreply, #state{}}, handle_info({leader_event, {set, time, {key, data, node}}}, #state{})),
					?assertEqual([{key, data, node, time, none, undefined}], ets:lookup(?MODULE, key))
				end}
			] end}
		},

		% the next four need to be completed at some point.
		% code works, just needs to get under test.
		{"{merge_complete, Mod, _Recs}, late merge_complete", fun() ->
			?assert(false)
		end},

		{"{merge_complete, Mod, Recs}", ?_assert(false)},
		{"{'DOWN', Monref, process, WatchWhat, Why}", ?_assert(false)},
		{"{'EXIT', From, Reason}", ?_assert(false)}
	]}
	].

-endif.

-ifdef(PROFILE).

avg(Acc) ->
	Sum = lists:foldl(fun(X, S) -> S + X end, 0, Acc),
	Sum / length(Acc).

record_res(File, Group, Name, Avg) ->
	io:format(File, "~p	~s	~s	~f~n", [os:timestamp(), Group, Name, Avg]).

profile_tc_test_() ->
	Group = profile_tc,
	{foreach,
	fun() ->
		case whereis(cpx_monitor) of
			undefined ->
				ok;
			Pid ->
				unregister(cpx_monitor),
				exit(Pid, kill)
		end,
		{ok, _Mon} = cpx_monitor:start([{nodes, node()}]),
		{ok, File} = file:open("cpx_monitor-profile.txt", [append]),
		File
	end,
	fun(File) ->
		file:close(File),
		cpx_monitor:stop()
	end,
	[fun(File) -> Name = "info message with little data", {timeout, 60, {Name, fun() ->
		cpx_monitor:subscribe(),
		Acc = [begin
			cpx_monitor:info([{"info", X}]),
			receive
				{cpx_monitor_event, {info, InTime, [{"info", X}]}} ->
					timer:now_diff(os:timestamp(), InTime)
			end
		end || X <- lists:seq(1, 1000)],
		Avg = avg(Acc),
		?INFO("Average time:  ~f", [Avg]),
		record_res(File, Group, Name, Avg)
	end}} end,
	fun(File) -> Name = "info message with lots of data", {timeout, 60, {Name, fun() ->
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
					timer:now_diff(os:timestamp(), InTime)
			end
		end || X <- lists:seq(1, 1000)],
		Avg = avg(Acc),
		?INFO("Average time:  ~f", [avg(Acc)]),
		record_res(File, Group, Name, Avg)
	end}} end,
	fun(File) -> Name = "alternating sets and drops, no data", {timeout, 60, {Name, fun() ->
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
					timer:now_diff(os:timestamp(), InTime)
			end
		end || X <- lists:seq(1, 1000)],
		Avg = avg(Acc),
		?INFO("Average time:  ~f", [Avg]),
		record_res(File, Group, Name, Avg)
	end}} end,
	fun(File) -> Name = "set, set, set, drop", {timeout, 60, {Name, fun() ->
		cpx_monitor:subscribe(),
		Acc = [begin
			case X rem 4 of
				1 ->
					cpx_monitor:set({agent, "1"}, [], none);
				2 ->
					cpx_monitor:set({agent, "1"}, [{module, ?MODULE}], ignore);
				3 ->
					cpx_monitor:set({agent, "1"}, [{module, none}], ignore);
				0 ->
					cpx_monitor:drop({agent, "1"})
			end,
			receive
				{cpx_monitor_event, {_, InTime, Data}} ->
					timer:now_diff(os:timestamp(), InTime)
			end
		end || X <- lists:seq(1, 1024)],
		Avg = avg(Acc),
		?INFO("Average time:  ~f", [Avg]),
		record_res(File, Group, Name, Avg)
	end}} end,
	fun(File) -> Name = "dumb sets", {timeout, 60, {Name, fun() ->
		cpx_monitor:subscribe(),
		Acc = [begin
			case X rem 2 of
				1 -> cpx_monitor:set({agent, "1"}, [{module, ?MODULE}], ignore);
				0 -> cpx_monitor:set({agent, "1"}, [], ignore)
			end,
			receive
				{cpx_monitor_event, {_, InTime, _Data}} ->
					timer:now_diff(os:timestamp(), InTime)
			end
		end || X <- lists:seq(1, 1000)],
		Avg = avg(Acc),
		?INFO("Average time:  ~f", [Avg]),
		record_res(File, Group, Name, Avg)
	end}} end]}.

-endif.

-ifdef(PROFILE).

avg(Acc) ->
	Sum = lists:foldl(fun(X, S) -> S + X end, 0, Acc),
	Sum / length(Acc).

write_result(File, Group, Name, Time) ->
	io:format(File, "~p	~s	~s	~f~n", [os:timestamp(), Group, Name, Time]).

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
		{ok, File} = file:open("cpx_monitor-profile.txt", [append]),
		File
	end,
	fun(File) ->
		file:close(File),
		cpx_monitor:stop()
	end,
	[
	fun(File) -> Name = "alternating sets and drops, no data", {timeout, 60, {Name, fun() ->
		cpx_monitor:subscribe(),
		Acc = [begin
			InTime = os:timestamp(),
			case X rem 2 of
				1 ->
					cpx_monitor:set({agent, "1"}, [], []);
				0 ->
					cpx_monitor:drop({agent, "1"})
			end,
			receive
				{cpx_monitor_event, _} ->
					timer:now_diff(os:timestamp(), InTime)
			end
		end || X <- lists:seq(1, 1000)],
		?INFO("Average time:  ~f", [avg(Acc)]),
		write_result(File, profile, Name, avg(Acc))
	end}} end,
	fun(File) -> Name = "set, set, set, drop", {timeout, 60, {Name, fun() ->
		cpx_monitor:subscribe(),
		Acc = [begin
			InTime = os:timestamp(),
			case X rem 4 of
				0 -> cpx_monitor:drop({agent, "1"});
				1 -> cpx_monitor:set({agent, "1"}, [], []);
				2 -> cpx_monitor:set({agent, "1"}, [], [{module, ?MODULE}]);
				3 -> cpx_monitor:set({agent, "1"}, [], [{module, none}])
			end,
			receive
				{cpx_monitor_event, _} ->
					timer:now_diff(os:timestamp(), InTime)
			end
		end || X <- lists:seq(1, 1024)],
		Avg = avg(Acc),
		?INFO("Average time:`~f", [Avg]),
		write_result(File, profile, Name, avg(Acc))
	end}} end]}.

-endif.
