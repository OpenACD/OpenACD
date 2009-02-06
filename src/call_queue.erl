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

%% @doc An inplementation of priority queues, with some extensions for parallel
%% call delivery. The calls in queue can be re-prioritized at any time and they
%% can have skills added/removed to facilitate scripted/dynamic call delivery.
%% Each call can be 'bound' to by a single dispatcher from each node in a cluster.
-module(call_queue).

%% depends on util, agent, cook, queue_manager


-type(key() :: {non_neg_integer(), {pos_integer(), non_neg_integer(), non_neg_integer()}}).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).
-include("call.hrl").
-include("queue.hrl").
-export([
	start/3,
	start_link/3,
	set_recipe/2,
	set_weight/2,
	get_weight/1,
	add/3,
	add/2,
	ask/1,
	get_call/2,
	print/1,
	remove/2,
	stop/1,
	grab/1,
	ungrab/2,
	set_priority/3,
	to_list/1, 
	add_skills/3,
	remove_skills/3,
	call_count/1
]).
% TODO magic skill '_queue' (name of the queue, so agents can be assigned to the queue)
-record(state, {
	queue = gb_trees:empty(),
	name :: atom(),
	recipe = ?DEFAULT_RECIPE :: recipe(),
	weight = ?DEFAULT_WEIGHT :: pos_integer(),
	call_skills = [english, '_node'] :: [atom()]}). % TODO use these!

%gen_server support
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @doc Start a queue named `Name' with no link to the current process.
-spec(start/3 :: (Name :: string(), Recipe :: recipe(), Weight :: pos_integer()) -> {ok, pid()}).
start(Name, Recipe, Weight) -> % Start linked queue custom default recipe and weight
	gen_server:start(?MODULE, [Name, Recipe, Weight], []).

%% @doc Start a queue names `Name' with a link to the current process.
-spec(start_link/3 :: (Name :: string(), Recipe :: recipe(), Weight :: pos_integer()) -> {ok, pid()}).
start_link(Name, Recipe, Weight) -> % Start linked queue with custom recipe and weight
	gen_server:start_link(?MODULE, [Name, Recipe, Weight], []).

%% @private
init([Name, Recipe, Weight]) -> 
	{ok, #state{name=Name, recipe=Recipe, weight=Weight}}.

%% @doc Set the queue at `Pid''s recipe to `Recipe'.
-spec(set_recipe/2 :: (Pid :: pid(), Recipe :: recipe()) -> 'ok' | 'error').
set_recipe(Pid, Recipe) -> 
	gen_server:call(Pid, {set_recipe, Recipe}).

%% @doc Set the queue at `Pid''s weight to `Weight'.
-spec(set_weight/2 :: (Pid :: pid(), Weight :: pos_integer()) -> 'ok' | 'error').
set_weight(Pid, Weight) -> 
	gen_server:call(Pid, {set_weight, Weight}).

%% @doc Return the weight of the queue at `Pid'.
-spec(get_weight/1 :: (Pid :: pid()) -> pos_integer()).
get_weight(Pid) -> 
	gen_server:call(Pid, get_weight).

%% @doc Add the call `Calldata' to the queue at `Pid' with priority of `Priority'.
-spec(add/3 :: (Pid :: pid(), Priority :: non_neg_integer(), Calldata :: #call{}) -> ok).
add(Pid, Priority, Calldata) -> 
	% TODO time out of infinity?
	gen_server:call(Pid, {add, Priority, Calldata}, infinity).
	
%% @doc Add the call `Calldata' to the queue at `Pid' with default priority of 1.
-spec(add/2 :: (Pid :: pid(), Calldata :: #call{}) -> ok).
add(Pid, Calldata) -> 
	add(Pid, 1, Calldata).
	
%% @doc Query the queue at `Pid' for a call with the id of `Callid'.
-spec(get_call/2 :: (Pid ::pid(), Callid :: string()) -> 'none' | {key(), #call{}}).
get_call(Pid, Callid) -> 
	gen_server:call(Pid, {get_call, Callid}).

%% @doc Return the first call in the queue at `Pid' that doesn't have a dispatcher from this node already bound to it or `none'.
-spec(ask/1 :: (Pid :: pid()) -> 'none' | {key(), #call{}}).
ask(Pid) ->
	gen_server:call(Pid, ask).

%% @doc Bind to the first call in the queue at `Pid' that doesn't have a dispatcher from this node already bound to it or `none'.
-spec(grab/1 :: (Pid :: pid()) -> 'none' | {key(), #call{}}).
grab(Pid) ->
	gen_server:call(Pid, grab).

%% @doc Reverse of @link grab/1.  Releases the call `Callid' from any bound dispatchers at queue `Pid'.  Returns `ok'.
-spec(ungrab/2 :: (Pid :: pid(), Callid :: string()) -> 'ok').
ungrab(Pid, Callid) -> 
	gen_server:call(Pid, {ungrab, Callid}).

%% @doc Add the list of skills `Skills' to the call with the id of `Callid' in the queue at `Pid'. Returns `ok' on success, `none' on failure.
-spec(add_skills/3 :: (Pid :: pid(), Callid :: string(), Skills :: [atom()]) -> 'none' | 'ok').
add_skills(Pid, Callid, Skills) -> 
	gen_server:call(Pid, {add_skills, Callid, Skills}).

%% @doc Remove the list of skills `Skills' from the call with the id of `Callid' in the queue at `Pid'. Returns `ok' on success, `none' on failure.
-spec(remove_skills/3 :: (Pid :: pid(), Callid :: string(), Skills :: [atom()]) -> 'none' | 'ok').
remove_skills(Pid, Callid, Skills) -> 
	gen_server:call(Pid, {remove_skills, Callid, Skills}).

%% @doc Alter the priority of the call with the id of `Callid' in the queue at `Pid' to `Priority'.  Returns `ok' on success, `none' on failure.
-spec(set_priority/3 :: ( Pid :: pid(), Calldata :: #call{}, Priority :: non_neg_integer()) -> 'none' | 'ok';
						(Pid :: pid(), Callid :: string(), Priority :: non_neg_integer()) -> 'none' | 'ok').
set_priority(Pid, #call{} = Calldata, Priority) ->
	set_priority(Pid, Calldata#call.id, Priority);
set_priority(Pid, Callid, Priority) ->
	gen_server:call(Pid, {set_priority, Callid, Priority}).

%% @doc Returns a list of calls in queue at `Pid'.
-spec(to_list/1 :: (Pid :: pid()) -> [#call{}]).
to_list(Pid) ->
	gen_server:call(Pid, to_list).

%% @doc returns the state of the queue at `Pid'.
-spec(print/1 :: (Pid :: pid()) -> any()).
print(Pid) ->
% TODO call this dump?
	gen_server:call(Pid, print).

%% @doc Remove the call with id of `Calldata' from the queue at `Pid'.  Returns `ok' on success, `none' on failure.
-spec(remove/2 :: (Pid :: pid(), Calldata :: #call{}) -> 'none' | 'ok';
					(Pid :: pid(), Calldata :: string()) -> 'none' | 'ok').
remove(Pid, #call{id = Id}) ->
	remove(Pid, Id);
remove(Pid, Calldata) -> 
	gen_server:call(Pid, {remove, Calldata}).

%% @doc Return the number of calls in the queue at `Pid'.
-spec(call_count/1 :: (Pid :: pid()) -> non_neg_integer()).
call_count(Pid) -> 
	gen_server:call(Pid, call_count).

%% @doc Stop the queue at `Pid'.
-spec(stop/1 :: (Pid :: pid()) -> 'ok').
stop(Pid) ->
	gen_server:call(Pid, stop).

% find the first call in the queue that doesn't have a pid on this node
% in its bound list
%% @private
-spec(find_unbound/2 :: (GbTree :: {non_neg_integer(), tuple()}, From :: pid()) -> {key(), #call{}} | 'none').
find_unbound(GbTree, From) ->
	find_unbound_(gb_trees:next(gb_trees:iterator(GbTree)), From).

%% @private
-spec(find_unbound_/2 :: (Iterator :: {key(), #call{}, any()} | 'none', From :: pid()) -> {key(), #call{}} | 'none').
find_unbound_(none, _From) -> 
	none;
find_unbound_({Key, #call{bound = []} = Value, _Iter}, _From) ->
	{Key, Value};
find_unbound_({Key, #call{bound = B} = Value, Iter}, From) ->
	case lists:filter(fun(Pid) -> node(Pid) =:= node(From) end, B ) of
		[] ->
			{Key, Value};
		_ -> 
			find_unbound_(gb_trees:next(Iter), From)
	end.

% return the {Key, Value} pair where Value#call.id == Needle or none
% ie:  lookup a call by ID, return the key in queue and the full call data
%% @private
-spec(find_key/2 :: (Needle :: string(), GbTree :: {non_neg_integer(), tuple()}) -> {key(), #call{}} | 'none').
find_key(Needle, GbTree) ->
	find_key_(Needle, gb_trees:next(gb_trees:iterator(GbTree))).

%% @private
-spec(find_key_/2 :: (Needle :: string(), Iterator :: {key(), #call{}, any()} | 'none') -> {key(), #call{}} | 'none').
find_key_(Needle, {Key, #call{id = Needle} = Value, _Iter}) ->
	{Key, Value};
find_key_(Needle, {_Key, _Value, Iter}) ->
	find_key_(Needle, gb_trees:next(Iter));
find_key_(_Needle, none) -> 
	none.

%% @private
-spec(expand_magic_skills/3 :: (State :: #state{}, Call :: #call{}, Skills :: [atom()]) -> [atom()]).
expand_magic_skills(State, Call, Skills) ->
	lists:flatten(lists:map(
		fun('_node') when is_pid(Call#call.source) -> node(Call#call.source);
			('_node') -> ?CONSOLE("Can't expand magic skill _node~n", []), [];
			('_queue') when is_list(State#state.name) -> list_to_atom(State#state.name);
			('_queue') when is_atom(State#state.name) -> State#state.name;
			('_queue') -> ?CONSOLE("Can't expand magic skill _queue~n", []), [];
			(Skill) -> Skill
		end, Skills)).

%% @private
handle_call({get_call, Callid}, _From, State) -> 
	{reply, find_key(Callid, State#state.queue), State};
handle_call({ungrab, Callid}, {From, _Tag}, State) ->
	case find_key(Callid, State#state.queue) of
		none -> 
			{reply, ok, State};
		{Key, Value} -> 
			{reply, ok, State#state{queue=gb_trees:update(Key, Value#call{bound=lists:delete(From, Value#call.bound)}, State#state.queue)}}
	end;
handle_call({set_weight, Weight}, _From, State) when is_integer(Weight), Weight > 0 ->
	{reply, ok, State#state{weight=Weight}};
handle_call({set_weight, _Weight}, _From, State) -> % invalid weight
	{reply, error, State};
handle_call(get_weight, _From, State) ->
	{reply, State#state.weight, State};
handle_call({set_recipe, Recipe}, _From, State) ->
	{reply, ok, State#state{recipe=Recipe}};
handle_call({add, Priority, Calldata}, _From, State) ->
	{ok, Pid} = cook:start_link(Calldata#call.id, State#state.recipe, self()),
	NewSkills = lists:umerge(lists:sort(State#state.call_skills), lists:sort(Calldata#call.skills)),
	Calldata2 = Calldata#call{cook=Pid, skills=expand_magic_skills(State, Calldata, NewSkills)},
	Trees = gb_trees:insert({Priority, now()}, Calldata2, State#state.queue),
	{reply, ok, State#state{queue=Trees}};

handle_call({add_skills, Callid, Skills}, _From, State) ->
	case find_key(Callid, State#state.queue) of
		none -> 
			{reply, none, State};
		{Key, #call{skills=OldSkills} = Value} ->
			Skills2 = expand_magic_skills(State, Value, Skills),
			NewSkills = lists:umerge(lists:sort(OldSkills), lists:sort(Skills2)),
			State2 = State#state{queue=gb_trees:update(Key, Value#call{skills=NewSkills}, State#state.queue)},
			{reply, ok, State2}
	end;
handle_call({remove_skills, Callid, Skills}, _From, State) -> 
	case find_key(Callid, State#state.queue) of
		none -> 
			{reply, none, State};
		{Key, #call{skills=OldSkills} = Value} ->
			Skills2 = expand_magic_skills(State, Value, Skills),
			NewSkills = lists:subtract(OldSkills, Skills2),
			State2 = State#state{queue=gb_trees:update(Key, Value#call{skills=NewSkills}, State#state.queue)},
			{reply, ok, State2}
	end;

handle_call(ask, {From, _Tag}, State) ->
	%return a call in queue excluding those already bound
	% return a tuple:  {key, val}
	{reply, find_unbound(State#state.queue, From), State};

handle_call(grab, {From, _Tag}, State) ->
	% ask and bind in one handy step
	case find_unbound(State#state.queue, From) of
		none -> 
			{reply, none, State};
		{Key, Value} ->
			link(From),
			State2 = State#state{queue=gb_trees:update(Key, Value#call{bound=lists:append(Value#call.bound, [From])}, State#state.queue)},
			{reply, {Key, Value}, State2}
	end;

handle_call(print, _From, State) ->
	{reply, State, State};

handle_call({remove, Id}, _From, State) ->
	?CONSOLE("Trying to remove call ~p...", [Id]),
	case find_key(Id, State#state.queue) of
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
	case find_key(Id, State#state.queue) of
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
	{reply, gb_trees:size(State#state.queue), State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%% @private
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @private
handle_info({'EXIT', From, _Reason}, State) ->
	Calls = gb_trees:to_list(State#state.queue),
	Cleancalls = clean_pid(From, State#state.recipe, Calls),
	Newtree = gb_trees:from_orddict(Cleancalls),
	{noreply, State#state{queue=Newtree}};
	
handle_info(Info, State) ->
	?CONSOLE("got info ~p", [Info]),
	{noreply, State}.

%% @private
terminate(_Reason, State) ->
	lists:foreach(fun({_K,V}) when is_pid(V#call.cook) -> cook:stop(V#call.cook); (_) -> ok end, gb_trees:to_list(State#state.queue)),
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% @private
%% Cleans up both dead dispatchers and dead cooks from the calls.
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

queue_test_() ->
	{
		foreach,
		fun() ->
				{ok, Pid} = start(testq, ?DEFAULT_RECIPE, ?DEFAULT_WEIGHT),
				register(stupidqueue, Pid),
				Pid
		end,
		fun(Pid) ->
			stop(Pid)
		end,
		[
			{
				"Change weight test", fun() ->
					Pid = whereis(stupidqueue),
					?assertEqual(ok, set_weight(Pid, 2)),
					?assertEqual(2, get_weight(Pid))
				end
			}, {
				"Invalid weight change test", fun() ->
					Pid = whereis(stupidqueue),
					?assertEqual(error, set_weight(Pid, -1)),
					?assertEqual(?DEFAULT_WEIGHT, get_weight(Pid))
				end
			}, {
				"Call Count Test", fun() ->
					Pid = whereis(stupidqueue),
					?assertEqual(0, call_count(Pid)),
					?assertEqual(ok, add(Pid, #call{id="C1"})),
					?assertEqual(1, call_count(Pid)),
					?assertEqual(ok, remove(Pid, "C1")),
					?assertEqual(0, call_count(Pid)),
					?assertEqual(none, remove(Pid, "C1"))
				end
			}, {
				"Querying for a call by ID", fun() ->
					Pid = whereis(stupidqueue),
					?assertEqual(none, get_call(Pid, "C1")),
					?assertEqual(ok, add(Pid, #call{id="C1"})),
					{_Key, Call} = get_call(Pid, "C1"),
					?debugFmt("~p ~p~n", [Call, is_record(Call, call)]),
					?assertEqual(true, is_record(Call, call))
				end
			}, {
				"Ungrab test", fun() ->
					Pid = whereis(stupidqueue),
					?assertEqual(ok, add(Pid, #call{id="C1"})),
					?assertEqual(ok, add(Pid, #call{id="C2"})),
					?assertEqual(ok, add(Pid, #call{id="C3"})),
					{_Key, Call} = grab(Pid),
					{_Key2, Call2} = grab(Pid),
					?assertEqual(ok, ungrab(Pid, Call#call.id)),
					?assertEqual(ok, ungrab(Pid, Call2#call.id)),
					?assertEqual(ok, ungrab(Pid, "wtf"))
				end
			}, {
				"Add skills test", fun() ->
					Pid = whereis(stupidqueue),
					?assertEqual(ok, add(Pid, #call{id="C1"})),
					{_Key, Call} = get_call(Pid, "C1"),
					?assertEqual(false, lists:member(foo, Call#call.skills)),
					?assertEqual(false, lists:member(bar, Call#call.skills)),
					?assertEqual(ok, add_skills(Pid, "C1", [foo, bar])),
					{_Key, Call2} = get_call(Pid, "C1"),
					?assertEqual(true, lists:member(foo, Call2#call.skills)),
					?assertEqual(true, lists:member(bar, Call2#call.skills))
				end
			}, {
				"Add skills to unknown call test", fun() ->
					Pid = whereis(stupidqueue),
					?assertEqual(ok, add(Pid, #call{id="C1"})),
					?assertEqual(none, add_skills(Pid, "C2", [foo, bar]))
				end
			}, {
				"Remove skills test", fun() ->
					Pid = whereis(stupidqueue),
					?assertEqual(ok, add(Pid, #call{id="C1"})),
					{_Key, Call} = get_call(Pid, "C1"),
					?assertEqual(true, lists:member(english, Call#call.skills)),
					?assertEqual(ok, remove_skills(Pid, "C1", [english])),
					{_Key, Call2} = get_call(Pid, "C1"),
					?assertEqual(false, lists:member(english, Call2#call.skills))
				end
			}, {
				"Remove skills from unknown call test", fun() ->
					Pid = whereis(stupidqueue),
					?assertEqual(ok, add(Pid, #call{id="C1"})),
					?assertEqual(none, remove_skills(Pid, "C2", [english]))
				end
			}, {
				"Add magic skills test", fun() ->
					Pid = whereis(stupidqueue),
					?assertEqual(ok, add(Pid, #call{id="C1", source=self()})),
					{_Key, Call} = get_call(Pid, "C1"),
					?assertEqual(false, lists:member(testq, Call#call.skills)),
					?assertEqual(ok, add_skills(Pid, "C1", ['_queue'])),
					{_Key, Call2} = get_call(Pid, "C1"),
					?debugFmt("Skills are ~p~n", [Call2#call.skills]),
					?assertEqual(true, lists:member(testq, Call2#call.skills))
				end
			}, {
				"Remove magic skills test", fun() ->
					Pid = whereis(stupidqueue),
					?assertEqual(ok, add(Pid, #call{id="C1", source=self()})),
					{_Key, Call} = get_call(Pid, "C1"),
					?assertEqual(true, lists:member(node(), Call#call.skills)),
					?assertEqual(ok, remove_skills(Pid, "C1", ['_node'])),
					{_Key, Call2} = get_call(Pid, "C1"),
					?assertEqual(false, lists:member(node(), Call2#call.skills))
				end
			}, {
				"Ensure that call_skills are merged into the call's skill list on add", fun() ->
					Pid = whereis(stupidqueue),
					?assertEqual(ok, add(Pid, #call{id="C1", source=self(), skills=[madness]})),
					{_Key, Call} = get_call(Pid, "C1"),
					?assertEqual(true, lists:member(node(), Call#call.skills)),
					?assertEqual(true, lists:member(english, Call#call.skills))
				end
			}
		]
	}.


-endif.
