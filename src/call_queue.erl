-module(call_queue).

%% depends on util, agent, cook, queue_manager

-type(key() :: {non_neg_integer(), {pos_integer(), non_neg_integer(), non_neg_integer()}}).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).
-include("call.hrl").
-include("queue.hrl").
-export([start/3, start_link/3, set_recipe/2, set_weight/2, get_weight/1, add/3, ask/1, get_call/2, print/1, remove/2, stop/1, grab/1, set_priority/3, to_list/1, add_skills/3, remove_skills/3, call_count/1]).

-record(state, {
	queue = gb_trees:empty(),
	name :: atom(),
	recipe = ?DEFAULT_RECIPE :: recipe(),
	weight = ?DEFAULT_WEIGHT :: pos_integer()}).

%gen_server support
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-spec(start/3 :: (Name :: atom(), Recipe :: recipe(), Weight :: pos_integer()) -> {ok, pid()}).
start(Name, Recipe, Weight) -> % Start linked queue custom default recipe and weight
	gen_server:start(?MODULE, [Name, Recipe, Weight], []).

-spec(start_link/3 :: (Name :: atom(), Recipe :: recipe(), Weight :: pos_integer()) -> {ok, pid()}).
start_link(Name, Recipe, Weight) -> % Start linked queue with custom recipe and weight
	gen_server:start_link(?MODULE, [Name, Recipe, Weight], []).

init([Name, Recipe, Weight]) -> 
	{ok, #state{name=Name, recipe=Recipe, weight=Weight}}.

-spec(set_recipe/2 :: (Pid :: pid(), Recipe :: recipe()) -> 'ok' | 'error').
set_recipe(Pid, Recipe) -> 
	gen_server:call(Pid, {set_recipe, Recipe}).

-spec(set_weight/2 :: (Pid :: pid(), Weight :: pos_integer()) -> 'ok' | 'error').
set_weight(Pid, Weight) -> 
	gen_server:call(Pid, {set_weight, Weight}).

-spec(get_weight/1 :: (Pid :: pid()) -> pos_integer()).
get_weight(Pid) -> 
	gen_server:call(Pid, get_weight).

-spec(add/3 :: (Pid :: pid(), Priority :: non_neg_integer(), Calldata :: #call{}) -> ok).
add(Pid, Priority, Calldata) -> 
	gen_server:call(Pid, {add, Priority, Calldata}, infinity).

-spec(get_call/2 :: (Pid ::pid(), Callid :: string()) -> 'none' | {key(), #call{}}).
get_call(Pid, Callid) -> 
	gen_server:call(Pid, {get_call, Callid}).

-spec(ask/1 :: (Pid :: pid()) -> 'none' | {key(), #call{}}).
ask(Pid) ->
	gen_server:call(Pid, ask).

-spec(grab/1 :: (Pid :: pid()) -> 'none' | {key(), #call{}}).
grab(Pid) ->
	gen_server:call(Pid, grab).

-spec(ungrab/2 :: (Pid :: pid(), Callid :: string()) -> 'ok').
ungrab(Pid, Callid) -> 
	gen_server:call(Pid, {ungrab, Callid}).

-spec(add_skills/3 :: (Pid :: pid(), Callid :: string(), Skills :: [atom()]) -> 'none' | 'ok').
add_skills(Pid, Callid, Skills) -> 
	gen_server:call(Pid, {add_skills, Callid, Skills}).
	
-spec(remove_skills/3 :: (Pid :: pid(), Callid :: string(), Skills :: [atom()]) -> 'none' | 'ok').
remove_skills(Pid, Callid, Skills) -> 
	gen_server:call(Pid, {remove_skills, Callid, Skills}).

-spec(set_priority/3 :: ( Pid :: pid(), Calldata :: #call{}, Priority :: non_neg_integer()) -> 'none' | 'ok';
						(Pid :: pid(), Callid :: string(), Priority :: non_neg_integer()) -> 'none' | 'ok').
set_priority(Pid, #call{} = Calldata, Priority) ->
	set_priority(Pid, Calldata#call.id, Priority);
set_priority(Pid, Callid, Priority) ->
	gen_server:call(Pid, {set_priority, Callid, Priority}).

-spec(to_list/1 :: (Pid :: pid()) -> [#call{}]).
to_list(Pid) ->
	gen_server:call(Pid, to_list).

-spec(print/1 :: (Pid :: pid()) -> any()).
print(Pid) ->
	gen_server:call(Pid, print).

-spec(remove/2 :: (Pid :: pid(), Calldata :: #call{}) -> 'none' | 'ok';
					(Pid :: pid(), Calldata :: string()) -> 'none' | 'ok').
remove(Pid, #call{} = Calldata) ->
	remove(Pid, Calldata#call.id);
remove(Pid, Calldata) -> 
	gen_server:call(Pid, {remove, Calldata}).

-spec(call_count/1 :: (Pid :: pid()) -> non_neg_integer()).
call_count(Pid) -> 
	gen_server:call(Pid, call_count).
	
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

handle_call({get_call, Callid}, _From, State) -> 
	case find_key(Callid, gb_trees:next(gb_trees:iterator(State#state.queue))) of
		none -> 
			{reply, none, State};
		{Key, Value} -> 
			{reply, {Key, Value}, State}
	end;
handle_call({ungrab, Callid}, From, State) ->
	case find_key(Callid, gb_trees:next(gb_trees:iterator(State#state.queue))) of
		none -> 
			{reply, ok, State};
		{Key, Value} -> 
			{reply, ok, State#state{queue=gb_trees:update(Key, Value#call{bound=lists:delete(From, Value#call.bound)}, State#state.queue)}}
	end;
handle_call({set_weight, Weight}, _From, State) ->
	{reply, ok, State#state{weight=Weight}};
handle_call(get_weight, _From, State) ->
		{reply, State#state.weight, State};
handle_call({set_recipe, Recipe}, _From, State) ->
	{reply, ok, State#state{recipe=Recipe}};
handle_call({add, Priority, Calldata}, _From, State) -> 
	{ok, Pid} = cook:start_link(Calldata#call.id, State#state.recipe, self()),
	Calldata2 = Calldata#call{cook=Pid},
	Trees = gb_trees:insert({Priority, now()}, Calldata2, State#state.queue),
	{reply, ok, State#state{queue=Trees}};

handle_call({add_skills, Callid, Skills}, _From, State) -> 
	case find_key(Callid, gb_trees:next(gb_trees:iterator(State#state.queue))) of
		none -> 
			{reply, none, State};
		{Key, #call{skills=OldSkills} = Value} -> 
			State2 = State#state{queue=gb_trees:update(Key, Value#call{skills=lists:merge(lists:sort(OldSkills), lists:sort(Skills))}, State#state.queue)},
			{reply, ok, State2}
	end;
handle_call({remove_skills, Callid, Skills}, _From, State) -> 
	case find_key(Callid, gb_trees:next(gb_trees:iterator(State#state.queue))) of
		none -> 
			{reply, none, State};
		{Key, #call{skills=OldSkills} = Value} -> 
			NewSkills = lists:subtract(OldSkills, Skills),
			State2 = State#state{queue=gb_trees:update(Key, Value#call{skills=NewSkills}, State#state.queue)},
			{reply, ok, State2}
	end;

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
			link(Realfrom),
			State2 = State#state{queue=gb_trees:update(Key, Value#call{bound=lists:append(Value#call.bound, [Realfrom])}, State#state.queue)},
			{reply, {Key, Value}, State2}
	end;

handle_call(print, _From, State) ->
	{reply, State, State};

handle_call({remove, Id}, _From, State) ->
	case find_key(Id, gb_trees:next(gb_trees:iterator(State#state.queue))) of
		none ->
			{reply, none, State};
		{Key, Value} ->
			cook:stop(Value#call.cook),
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

handle_call(call_count, _From, State) -> 
	{reply, length(gb_trees:to_list(State#state.queue)), State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'EXIT', From, _Reason}, State) ->
	Calls = gb_trees:to_list(State#state.queue),
	Cleancalls = clean_pid(From, State#state.recipe, Calls),
	Newtree = gb_trees:from_orddict(Cleancalls),
	{noreply, State#state{queue=Newtree}};
	
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	lists:foreach(fun({_K,V}) when is_pid(V#call.cook) -> cook:stop(V#call.cook); (_) -> ok end, gb_trees:to_list(State#state.queue)),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-spec(clean_pid/3 :: (Deadpid :: pid(), Recipe :: recipe(), Calls :: [#call{}]) -> [#call{}]).
clean_pid(Deadpid, Recipe, [{Key, Call} | Calls] ) -> 
	Bound = Call#call.bound,
	Cleanbound = lists:delete(Deadpid, Bound),
	case Call#call.cook of
		Deadpid -> 
			{ok, Pid} = cook:start_link(Call#call.id, Recipe, self());
		_ -> 
			Pid = Call#call.cook
	end,
	Cleancall = Call#call{bound=Cleanbound, cook=Pid},
	[{Key, Cleancall} | clean_pid(Deadpid, Recipe, Calls)];
clean_pid(_Deadpid, _Recipe, []) -> 
	[].


% begin the defintions of the tests.

-ifdef(TEST).

add_test() ->
	{_, Pid} = start(goober, ?DEFAULT_RECIPE, ?DEFAULT_WEIGHT),
	C1 = #call{id="C1"},
	?assert(add(Pid, 1, C1) =:= ok),
	{{Priority, _Time}, Call} = ask(Pid),
	?assertEqual(1, Priority),
	Id = Call#call.id,
	?assertEqual("C1", Id).
	
remove_test() ->
	{_, Pid} = start(goober, ?DEFAULT_RECIPE, ?DEFAULT_WEIGHT),
	C1 = #call{id="C1"},
	C2 = #call{id="C2"},
	add(Pid, 1, C1),
	add(Pid, 1, C2),
	remove(Pid, C1),
	{_Key, Call} = grab(Pid),
	?assertMatch("C2", Call#call.id).
	
remove_id_test() ->
	{_, Pid} = start(goober, ?DEFAULT_RECIPE, ?DEFAULT_WEIGHT),
	C1 = #call{id="C1"},
	add(Pid, 1, C1),
	?assertMatch(ok, remove(Pid, "C1")),
	?assertMatch(none, grab(Pid)).

remove_nil_test() ->
	{_, Pid} = start(goober, ?DEFAULT_RECIPE, ?DEFAULT_WEIGHT),
	?assertMatch(none, remove(Pid, "C1")).

find_key_test() ->
	{_, Pid} = start(goober, ?DEFAULT_RECIPE, ?DEFAULT_WEIGHT),
	C1 = #call{id="C1"},
	C2 = #call{id="C2", bound=[self()]},
	add(Pid, 1, C1),
	?assertMatch(none, remove(Pid, C2)).

bound_test() ->
	{_, Node} = slave:start(net_adm:localhost(), boundtest),
	Pid = spawn(Node, erlang, exit, [normal]),
	C1 = #call{id="C1", bound=[Pid]},
	{_, Qpid} = start(foobar, ?DEFAULT_RECIPE, ?DEFAULT_WEIGHT),
	add(Qpid, 1, C1),
	{_Key, Call} = grab(Qpid),
	?assertEqual("C1", Call#call.id),
	?assertMatch(none, grab(Qpid)).

grab_test() -> 
	C1 = #call{id="C1"},
	C2 = #call{id="C2", bound=[self()]},
	C3 = #call{id="C3"},
	{_, Pid} = start(goober, ?DEFAULT_RECIPE, ?DEFAULT_WEIGHT),
	add(Pid, 1, C1),
	add(Pid, 0, C2),
	add(Pid, 1, C3),
	{_Key, Call1} = grab(Pid),
	{_Key2, Call3} = grab(Pid),
	?assertEqual("C1", Call1#call.id),
	?assertEqual("C3", Call3#call.id),
	?assert(grab(Pid) =:= none).

grab_empty_test() -> 
	{_, Pid} = start(goober, ?DEFAULT_RECIPE, ?DEFAULT_WEIGHT),
	?assert(grab(Pid) =:= none).

increase_priority_test() ->
	C1 = #call{id="C1"},
	C2 = #call{id="C2"},
	C3 = #call{id="C3"},
	{_, Pid} = start(goober, ?DEFAULT_RECIPE, ?DEFAULT_WEIGHT),
	add(Pid, 1, C1),
	add(Pid, 1, C2),
	add(Pid, 1, C3),
	{_Key, Call1} = ask(Pid),
	?assertEqual("C1", Call1#call.id),
	set_priority(Pid, C2, 0),
	{_Key2, Call2} = ask(Pid),
	?assertEqual("C2", Call2#call.id).

increase_priority_nil_test() ->
	{_, Pid} = start(goober, ?DEFAULT_RECIPE, ?DEFAULT_WEIGHT),
	?assertMatch(none, set_priority(Pid, "C1", 1)).

decrease_priority_test() ->
	C1 = #call{id="C1"},
	C2 = #call{id="C2"},
	C3 = #call{id="C3"},
	{_, Pid} = start(goober, ?DEFAULT_RECIPE, ?DEFAULT_WEIGHT),
	add(Pid, 1, C1),
	add(Pid, 1, C2),
	add(Pid, 1, C3),
	{_Key, Call1} = ask(Pid),
	?assertEqual("C1", Call1#call.id),
	set_priority(Pid, C1, 2),
	{_Key2, Call2} = ask(Pid),
	?assertEqual("C2", Call2#call.id).

queue_to_list_test() ->
	C1 = #call{id="C1"},
	C2 = #call{id="C2"},
	C3 = #call{id="C3"},
	{_, Pid} = start(goober, ?DEFAULT_RECIPE, ?DEFAULT_WEIGHT),
	add(Pid, 1, C1),
	add(Pid, 1, C2),
	add(Pid, 1, C3),
	?assertMatch(["C1", "C2", "C3"], lists:map(fun(X) -> X#call.id end, to_list(Pid))).

empty_queue_to_list_test() -> 
	{_, Pid} = start(goober, ?DEFAULT_RECIPE, ?DEFAULT_WEIGHT),
	?assertMatch([], to_list(Pid)).
	
start_stop_test() ->
	{_, Pid} = start(goober, ?DEFAULT_RECIPE, ?DEFAULT_WEIGHT),
	?assertMatch(ok, stop(Pid)).

-endif.
