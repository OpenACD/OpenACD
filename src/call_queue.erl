-module(call_queue).

-type(key() :: {non_neg_integer(), {pos_integer(), non_neg_integer(), non_neg_integer()}}).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DEFAULT_WEIGHT, 5).

-behaviour(gen_server).
-include("call.hrl").
-export([start/1, start/2, start/3, start_link/1, start_link/2, start_link/3, set_recipe/2, set_weight/2, add/3, ask/1, print/1, remove/2, stop/1, grab/1, set_priority/3, to_list/1]).

-record(state, {
	queue = gb_trees:empty(),
	name :: atom(),
	recipe = [] :: recipe(),
	weight = ?DEFAULT_WEIGHT :: pos_integer()}).

%gen_server support
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-spec(start/1 :: (Name :: atom()) -> {ok, pid()}).
start(Name) ->
	gen_server:start(?MODULE, [Name, [], ?DEFAULT_WEIGHT], []).

-spec(start/2 :: (Name :: atom(), Recipe :: recipe()) -> {ok, pid()}).
start(Name, Recipe) ->
	gen_server:start(?MODULE, [Name, Recipe, ?DEFAULT_WEIGHT], []).

-spec(start/3 :: (Name :: atom(), Recipe :: recipe(), Weight :: pos_integer()) -> {ok, pid()}).
start(Name, Recipe, Weight) -> 
	gen_server:start(?MODULE, [Name, Recipe, Weight]).
	
-spec(start_link/1 :: (Name :: atom()) -> {ok, pid()}).
start_link(Name) ->
	gen_server:start_link(?MODULE, [Name, [], ?DEFAULT_WEIGHT], []).

-spec(start_link/2 :: (Name :: atom(), Recipe :: recipe()) -> {ok, pid()}).
start_link(Name, Recipe) -> 
	gen_server:start_link(?MODULE, [Name, Recipe, ?DEFAULT_WEIGHT], []).

-spec(start_link/3 :: (Name :: atom(), Recipe :: recipe(), Weight :: pos_integer()) -> {ok, pid()}).
start_link(Name, Recipe, Weight) -> 
	gen_server:start_link(?MODULE, [Name, Recipe, Weight], []).

init([Name, Recipe, Weight]) -> 
	{ok, #state{name=Name, recipe=Recipe, weight=Weight}}.

-spec(set_recipe/2 :: (Pid :: pid(), Recipe :: recipe()) -> 'ok' | 'error').
set_recipe(Pid, Recipe) -> 
	gen_server:call(Pid, {set_recipe, Recipe}).

-spec(set_weight/2 :: (Pid :: pid(), Weight :: pos_integer()) -> 'ok' | 'error').
set_weight(Pid, Weight) -> 
	gen_server:call(Pid, {set_weight, Weight}).

-spec(add/3 :: (Priority :: non_neg_integer(), Calldata :: #call{}, Pid :: pid()) -> ok).
add(Priority, Calldata, Pid) -> 
	gen_server:call(Pid, {add, Priority, Calldata}, infinity).

-spec(ask/1 :: (Pid :: pid()) -> 'none' | {key(), #call{}}).
ask(Pid) ->
	gen_server:call(Pid, ask).

-spec(grab/1 :: (Pid :: pid()) -> 'none' | {key(), #call{}}).
grab(Pid) ->
	gen_server:call(Pid, grab).

-spec(set_priority/3 :: (Calldata :: #call{}, Priority :: non_neg_integer(), Pid :: pid()) -> 'none' | 'ok';
						(Callid :: string(), Priority :: non_neg_integer(), Pid :: pid()) -> 'none' | 'ok').
set_priority(#call{} = Calldata, Priority, Pid) ->
	set_priority(Calldata#call.id, Priority, Pid);
set_priority(Callid, Priority, Pid) ->
	gen_server:call(Pid, {set_priority, Callid, Priority}).

-spec(to_list/1 :: (Pid :: pid()) -> []).
to_list(Pid) ->
	gen_server:call(Pid, to_list).

-spec(print/1 :: (Pid :: pid()) -> any()).
print(Pid) ->
	gen_server:call(Pid, print).

-spec(remove/2 :: (Calldata :: #call{}, Pid :: pid()) -> 'none' | 'ok';
					(Calldata :: string(), Pid :: pid()) -> 'none' | 'ok').
remove(#call{} = Calldata, Pid) ->
	remove(Calldata#call.id, Pid);
remove(Calldata, Pid) -> 
	gen_server:call(Pid, {remove, Calldata}).

-spec(stop/1 :: (Pid :: pid()) -> 'ok').
stop(Pid) ->
	gen_server:call(Pid, stop).

% find the first call in the queue that doesn't have a pid on this node
% in its bound list
find_unbound(none, _From) -> 
	none;
find_unbound({Key, #call{bound = []} = Value, _Iter}, _From) ->
	{Key, Value};
find_unbound({Key, #call{bound = B} = Value, Iter}, {From, _}) ->
	case lists:filter(fun(Pid) -> node(Pid) =:= node(From) end, B ) of
		[] ->
			{Key, Value};
		_ -> 
			find_unbound(gb_trees:next(Iter), {From, foo})
	end.

% return the {Key, Value} pair where Value#call.id == Needle or none
% ie:  lookup a call by ID, return the key in queue and the full call data
find_key(Needle, {Key, #call{id = Needle} = Value, _Iter}) ->
	{Key, Value};
find_key(Needle, {_Key, _Value, Iter}) ->
	find_key(Needle, gb_trees:next(Iter));
find_key(_Needle, none) -> 
	none.

handle_call({set_weight, Weight}, _From, State) ->
	{reply, ok, State#state{weight=Weight}};
handle_call({set_recipe, Recipe}, _From, State) ->
	{reply, ok, State#state{recipe=Recipe}};
handle_call({add, Priority, Calldata}, _From, State) -> 
	Trees = gb_trees:insert({Priority, now()}, Calldata, State#state.queue),
	{reply, ok, State#state{queue=Trees}};

handle_call(ask, From, State) ->
	%generate a call in queue excluding those already bound
	% return a tuple:  {key, val}
	{reply, find_unbound(gb_trees:next(gb_trees:iterator(State#state.queue)), From), State};

handle_call(grab, From, State) ->
	% ask and bind in one handy step
	%io:format("From:  ~p~n", [From]),
	case find_unbound(gb_trees:next(gb_trees:iterator(State#state.queue)), From) of
		none -> 
			{reply, none, State};
		{Key, Value} ->
			{Realfrom, _} = From, 
			State2 = State#state{queue=gb_trees:update(Key, Value#call{bound=lists:append(Value#call.bound, [Realfrom])}, State#state.queue)},
			{reply, {Key, Value}, State2}
	end;

handle_call(print, _From, State) ->
	{reply, State, State};

handle_call({remove, Id}, _From, State) ->
	case find_key(Id, gb_trees:next(gb_trees:iterator(State#state.queue))) of
		none ->
			{reply, none, State};
		{Key, _Value} ->
			State2 = State#state{queue=gb_trees:delete(Key, State#state.queue)},
			{reply, ok, State2}
		end;

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call({set_priority, Id, Priority}, _From, State) ->
	case find_key(Id, gb_trees:next(gb_trees:iterator(State#state.queue))) of
		none ->
			{reply, none, State};
		{{Oldpri, Time}, Value} ->
			State2 = State#state{queue=gb_trees:delete({Oldpri, Time}, State#state.queue)},
			State3 = State2#state{queue=gb_trees:insert({Priority, Time}, Value, State2#state.queue)},
			{reply, ok, State3}
		end;

handle_call(to_list, _From, State) ->
	{reply, lists:map(fun({_, Call}) -> Call end, gb_trees:to_list(State#state.queue)), State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) -> 
	{noreply, State}.

terminate(_Reason, _State) -> 
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% begin the defintions of the tests.

-ifdef(TEST).

add_test() ->
	{_, Pid} = start(goober),
	C1 = #call{id="C1"},
	?assert(add(1, C1, Pid) =:= ok),
	?assertMatch({{1, _Time}, C1}, ask(Pid)).
	
remove_test() ->
	{_, Pid} = start(goober),
	C1 = #call{id="C1"},
	C2 = #call{id="C2"},
	add(1, C1, Pid),
	add(1, C2, Pid),
	remove(C1, Pid),
	?assertMatch({_Key, C2}, grab(Pid)).
	
remove_id_test() ->
	{_, Pid} = start(goober),
	C1 = #call{id="C1"},
	add(1, C1, Pid),
	?assertMatch(ok, remove("C1", Pid)),
	?assertMatch(none, grab(Pid)).

remove_nil_test() ->
	{_, Pid} = start(goober), 
	?assertMatch(none, remove("C1", Pid)).

find_key_test() ->
	{_, Pid} = start(goober),
	C1 = #call{id="C1"},
	C2 = #call{id="C2", bound=[self()]},
	add(1, C1, Pid),
	?assertMatch(none, remove(C2, Pid)).

bound_test() ->
	{_, Node} = slave:start(net_adm:localhost(), boundtest),
	Pid = spawn(Node, erlang, exit, [normal]),
	C1 = #call{id="C1", bound=[Pid]},
	{_, Qpid} = start(foobar),
	add(1, C1, Qpid),
	?assertMatch({_Key, C1}, grab(Qpid)),
	?assertMatch(none, grab(Qpid)).

grab_test() -> 
	C1 = #call{id="C1"},
	C2 = #call{id="C2", bound=[self()]},
	C3 = #call{id="C3"},
	{_, Pid} = start(goober),
	add(1, C1, Pid),
	add(0, C2, Pid),
	add(1, C3, Pid),
	?assertMatch({_Key, C1}, grab(Pid)),
	?assertMatch({_Key, C3}, grab(Pid)),
	?assert(grab(Pid) =:= none).

grab_empty_test() -> 
	{_, Pid} = start(goober),
	?assert(grab(Pid) =:= none).

increase_priority_test() ->
	C1 = #call{id="C1"},
	C2 = #call{id="C2"},
	C3 = #call{id="C3"},
	{_, Pid} = start(goober),
	add(1, C1, Pid),
	add(1, C2, Pid),
	add(1, C3, Pid),
	?assertMatch({{1, _Time}, C1}, ask(Pid)),
	set_priority(C2, 0, Pid),
	?assertMatch({{0, _Time}, C2}, ask(Pid)).

increase_priority_nil_test() ->
	{_, Pid} = start(goober),
	?assertMatch(none, set_priority("C1", 1, Pid)).

decrease_priority_test() ->
	C1 = #call{id="C1"},
	C2 = #call{id="C2"},
	C3 = #call{id="C3"},
	{_, Pid} = start(goober),
	add(1, C1, Pid),
	add(1, C2, Pid),
	add(1, C3, Pid),
	?assertMatch({{1, _Time}, C1}, ask(Pid)),
	set_priority(C1, 2, Pid),
	?assertMatch({{1, _Time}, C2}, ask(Pid)).

queue_to_list_test() ->
	C1 = #call{id="C1"},
	C2 = #call{id="C2"},
	C3 = #call{id="C3"},
	{_, Pid} = start(goober),
	add(1, C1, Pid),
	add(1, C2, Pid),
	add(1, C3, Pid),
	?assertMatch([C1, C2, C3], to_list(Pid)).

empty_queue_to_list_test() -> 
	{_, Pid} = start(goober), 
	?assertMatch([], to_list(Pid)).
	
start_stop_test() ->
	{_, Pid} = start(goober),
	?assertMatch(ok, stop(Pid)).

-endif.
