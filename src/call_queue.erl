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
	call_skills = [english, '_node'] :: [atom()]}).

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
-spec(add/3 :: (Pid :: pid(), Priority :: non_neg_integer(), Calldata :: pid()) -> ok).
add(Pid, Priority, Mediapid) when is_pid(Pid), is_pid(Mediapid), Priority >= 0 ->
	Callrec = gen_server:call(Mediapid, get_call),
	gen_server:call(Pid, {add, Priority, Mediapid, Callrec}).

%% @doc Add the call `Calldata' to the queue at `Pid' with default priority of 1.
-spec(add/2 :: (Pid :: pid(), Calldata :: pid()) -> ok).
add(Pid, Mediapid) when is_pid(Pid), is_pid(Mediapid) ->
	add(Pid, 1, Mediapid).

%% @doc Query the queue at `Pid' for a call with the ID or Pid of `Callid'.
-spec(get_call/2 :: (Pid :: pid(), Callid :: pid()) -> 'none' | {key(), #queued_call{}};
					(Pid :: pid(), Callid :: string()) -> 'none' | {key(), #queued_call{}}).
get_call(Pid, Callid) when is_pid(Pid) ->
	gen_server:call(Pid, {get_call, Callid}).

%% @doc Return the first call in the queue at `Pid' that doesn't have a dispatcher from this node already bound to it or `none'.
-spec(ask/1 :: (Pid :: pid()) -> 'none' | {key(), #call{}}).
ask(Pid) ->
	gen_server:call(Pid, ask).

%% @doc Bind to the first call in the queue at `Pid' that doesn't have a dispatcher from this node already bound to it or `none'.
-spec(grab/1 :: (Pid :: pid()) -> 'none' | {key(), #call{}}).
grab(Pid) when is_pid(Pid) ->
	gen_server:call(Pid, grab).

%% @doc Reverse of @link grab/1.  Releases the call `Callid' from any bound dispatchers at queue `Pid'.  Returns `ok'.
-spec(ungrab/2 :: (Pid :: pid(), Callid :: string()) -> 'ok').
ungrab(Pid, Mediapid) when is_pid(Mediapid), is_pid(Pid) ->
	#call{id = Cid} = gen_server:call(Mediapid, get_call),
	gen_server:call(Pid, {ungrab, Cid});
ungrab(Pid, Callid) when is_pid(Pid) ->
	gen_server:call(Pid, {ungrab, Callid}).

%% @doc Add the list of skills `Skills' to the call with the id of `Callid' in the queue at `Pid'. Returns `ok' on success, `none' on failure.
-spec(add_skills/3 :: (Pid :: pid(), Callid :: string() | pid(), Skills :: [atom(),...]) -> 'none' | 'ok').
add_skills(Pid, Mediapid, Skills) when is_pid(Mediapid) ->
	#call{id = Callid} = gen_server:call(Mediapid, get_call),
	gen_server:call(Pid, {add_skills, Callid, Skills});
add_skills(Pid, Callid, Skills) ->
	gen_server:call(Pid, {add_skills, Callid, Skills}).

%% @doc Remove the list of skills `Skills' from the call with the id of `Callid' in the queue at `Pid'. Returns `ok' on success, `none' on failure.
-spec(remove_skills/3 :: (Pid :: pid(), Callid :: string() | pid(), Skills :: [atom(),...]) -> 'none' | 'ok').
remove_skills(Pid, Mediapid, Skills) when is_pid(Mediapid) ->
	#call{id = Callid} = gen_server:call(Mediapid, get_call),
	gen_server:call(Pid, {remove_skills, Callid, Skills});
remove_skills(Pid, Callid, Skills) ->
	gen_server:call(Pid, {remove_skills, Callid, Skills}).

%% @doc Alter the priority of the call with the id or media pid of `Mediaid' in the queue at `Pid' to `Priority'.  Returns `ok' on success, `none' on failure.
-spec(set_priority/3 :: ( Pid :: pid(), Calldata :: string() | pid(), Priority :: non_neg_integer()) -> 'none' | 'ok').
set_priority(Pid, Mediaid, Priority) when Priority >= 0 ->
	gen_server:call(Pid, {set_priority, Mediaid, Priority}).

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
%-spec(remove/2 :: (Pid :: pid(), Calldata :: #call{}) -> 'none' | 'ok';
%					(Pid :: pid(), Calldata :: string()) -> 'none' | 'ok').
%remove(Pid, #call{id = Id}) ->
%	remove(Pid, Id);
%remove(Pid, Calldata) ->
%	gen_server:call(Pid, {remove, Calldata}).

remove(Pid, Callpid) when is_pid(Pid), is_pid(Callpid) ->
	gen_server:call(Pid, {remove, Callpid});
remove(Pid, Callid) when is_pid(Pid) ->
	case gen_server:call(Pid, {get_call, Callid}) of
		{_Key, #queued_call{media=Mediapid}} ->
			remove(Pid, Mediapid);
		none ->
			none
	end.

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
-spec(find_unbound/2 :: (GbTree :: {non_neg_integer(), tuple()}, From :: pid()) -> {key(), #queued_call{}} | 'none').
find_unbound(GbTree, From) when is_pid(From) ->
	find_unbound_(gb_trees:next(gb_trees:iterator(GbTree)), From).

%% @private
-spec(find_unbound_/2 :: (Iterator :: {key(), #call{}, any()} | 'none', From :: pid()) -> {key(), #queued_call{}} | 'none').
find_unbound_(none, _From) ->
	none;
find_unbound_({Key, #queued_call{dispatchers = []} = Callrec, _Iter}, _From) ->
	{Key, Callrec};
find_unbound_({Key, #queued_call{dispatchers = Dispatchers} = Callrec, Iter}, From) ->
	F = fun(Pid) ->
		node(Pid) =:= node(From)
	end,
	case lists:filter(F, Dispatchers) of
		[] ->
			{Key, Callrec};
		_ ->
			find_unbound_(gb_trees:next(Iter), From)
	end.

% return the {Key, Value} pair where Value#call.id == Needle or none
% ie:  lookup a call by ID, return the key in queue and the full call data
%% @private
-spec(find_key/2 :: (Needle :: string(), GbTree :: {non_neg_integer(), tuple()}) -> {key(), #queued_call{}} | 'none').
find_key(Needle, GbTree) ->
	find_key_(Needle, gb_trees:next(gb_trees:iterator(GbTree))).

%% @private
-spec(find_key_/2 :: (Needle :: string(), Iterator :: {key(), #queued_call{}, any()} | 'none') -> {key(), #queued_call{}} | 'none').
find_key_(Needle, {Key, #queued_call{id = Needle} = Value, _Iter}) ->
	{Key, Value};
find_key_(Needle, {_Key, _Value, Iter}) ->
	find_key_(Needle, gb_trees:next(Iter));
find_key_(_Needle, none) ->
	none.

%% @private
-spec(find_by_pid/2 :: (Needle :: pid(), GbTree :: any()) -> {key(), #queued_call{}} | 'none').
find_by_pid(Needle, GbTree) ->
	find_by_pid_(Needle, gb_trees:next(gb_trees:iterator(GbTree))).

%% @private
-spec(find_by_pid_/2 :: (Needle :: pid(), Interator :: {key(), #queued_call{}, any()} | 'none') -> {key(), #queued_call{}} | 'none').
find_by_pid_(Needle, {Key, #queued_call{media = Needle} = Value, _Iter}) ->
	{Key, Value};
find_by_pid_(Needle, {_Key, _Value, Iter}) ->
	find_by_pid_(Needle, gb_trees:next(Iter));
find_by_pid_(_Needle, none) ->
	none.

%% @private
-spec(expand_magic_skills/3 :: (State :: #state{}, Call :: #queued_call{}, Skills :: [atom()]) -> [atom()]).
expand_magic_skills(State, Call, Skills) ->
	lists:flatten(lists:map(
		fun('_node') when is_pid(Call#queued_call.media) -> node(Call#queued_call.media);
			('_node') -> ?CONSOLE("Can't expand magic skill _node~n", []), [];
			('_queue') when is_list(State#state.name) -> list_to_atom(State#state.name);
			('_queue') when is_atom(State#state.name) -> State#state.name;
			('_queue') -> ?CONSOLE("Can't expand magic skill _queue~n", []), [];
			(Skill) -> Skill
		end, Skills)).

%=====
% gen_server callbacks
%=====

%% @private
init([Name, Recipe, Weight]) ->
	process_flag(trap_exit, true),
	{ok, #state{name=Name, recipe=Recipe, weight=Weight}}.

%% @private
handle_call({get_call, Callpid}, _From, State) when is_pid(Callpid) ->
	{reply, find_by_pid(Callpid, State#state.queue), State};
handle_call({get_call, Callid}, _From, State) ->
	{reply, find_key(Callid, State#state.queue), State};
handle_call({ungrab, Callid}, {From, _Tag}, State) ->
	case find_key(Callid, State#state.queue) of
		none ->
			{reply, ok, State};
		{Key, Value} ->
			{reply, ok, State#state{queue=gb_trees:update(Key, Value#queued_call{dispatchers=lists:delete(From, Value#queued_call.dispatchers)}, State#state.queue)}}
	end;
handle_call({set_weight, Weight}, _From, State) when is_integer(Weight), Weight > 0 ->
	{reply, ok, State#state{weight=Weight}};
handle_call({set_weight, _Weight}, _From, State) -> % invalid weight
	{reply, error, State};
handle_call(get_weight, _From, State) ->
	{reply, State#state.weight, State};
handle_call({set_recipe, Recipe}, _From, State) ->
	{reply, ok, State#state{recipe=Recipe}};
handle_call({add, Priority, Callpid, Callrec}, From, State) when is_pid(Callpid) ->
	% TODO ensure cook is started on same node callpid is on
	?CONSOLE("adding call ~p request from ~p", [Callpid, From]),
	{ok, Cookpid} = cook:start_link(Callpid, State#state.recipe, State#state.name),
	Queuedrec = #queued_call{media=Callpid, id=Callrec#call.id, cook=Cookpid},
	?CONSOLE("queuedrec: ~p", [Queuedrec]),
	NewSkills = lists:umerge(lists:sort(State#state.call_skills), lists:sort(Callrec#call.skills)),
	Expandedskills = expand_magic_skills(State, Queuedrec, NewSkills),
	Value = Queuedrec#queued_call{skills=Expandedskills},
	Trees = gb_trees:insert({Priority, now()}, Value, State#state.queue),
	{reply, ok, State#state{queue=Trees}};


handle_call({add_skills, Callid, Skills}, _From, State) ->
	case find_key(Callid, State#state.queue) of
		none ->
			{reply, none, State};
		{Key, #queued_call{skills=OldSkills} = Value} ->
			Skills2 = expand_magic_skills(State, Value, Skills),
			NewSkills = lists:umerge(lists:sort(OldSkills), lists:sort(Skills2)),
			State2 = State#state{queue=gb_trees:update(Key, Value#queued_call{skills=NewSkills}, State#state.queue)},
			{reply, ok, State2}
	end;
handle_call({remove_skills, Callid, Skills}, _From, State) ->
	case find_key(Callid, State#state.queue) of
		none ->
			{reply, none, State};
		{Key, #queued_call{skills=OldSkills} = Value} ->
			Skills2 = expand_magic_skills(State, Value, Skills),
			NewSkills = lists:subtract(OldSkills, Skills2),
			State2 = State#state{queue=gb_trees:update(Key, Value#queued_call{skills=NewSkills}, State#state.queue)},
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
			link(From), % to catch exits from the dispather so we can clean out dead pids
			State2 = State#state{queue=gb_trees:update(Key, Value#queued_call{dispatchers=lists:append(Value#queued_call.dispatchers, [From])}, State#state.queue)},
			{reply, {Key, Value}, State2}
	end;

handle_call(print, _From, State) ->
	{reply, State, State};

handle_call({remove, Callpid}, _From, State) ->
	?CONSOLE("Trying to remove call ~p...", [Callpid]),
	case find_by_pid(Callpid, State#state.queue) of
		none ->
			{reply, none, State};
		{Key, #queued_call{cook=Cookpid}} ->
			cook:stop(Cookpid),
			State2 = State#state{queue=gb_trees:delete(Key, State#state.queue)},
			{reply, ok, State2}
		end;

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call({stop, Reason}, _From, State) ->
	{stop, Reason, ok, State};

handle_call({set_priority, Id, Priority}, _From, State) when is_pid(Id), Priority >= 0 ->
	case find_by_pid(Id, State#state.queue) of
		none ->
			{reply, none, State};
		{{Oldpri, Time}, Value} ->
			State2 = State#state{queue=gb_trees:delete({Oldpri, Time}, State#state.queue)},
			State3 = State2#state{queue=gb_trees:insert({Priority, Time}, Value, State2#state.queue)},
			{reply, ok, State3}
	end;

handle_call({set_priority, Id, Priority}, _From, State) when Priority >= 0 ->
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

handle_call(Request, _From, State) ->
	{reply, {unknown_call, Request}, State}.

%% @private
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @private
handle_info({'EXIT', From, Reason}, State) ->
	QMPid = whereis(queue_manager),
	?CONSOLE("Handling exit from ~p which died due to ~p.  Manager is ~p", [From, Reason, QMPid]),
	case QMPid of
		undefined -> 
			?CONSOLE("Can't find the manager.  dying", []),
			{stop, {queue_manager, noproc}, State};
		From -> 
			?CONSOLE("Seems to be the queue manager that died.  Dying with it.", []),
			{stop, {queue_manager_died, Reason}, State};
		_Else -> 
			Calls = gb_trees:to_list(State#state.queue),
			Cleancalls = clean_pid(From, State#state.recipe, Calls, State#state.name),
			Newtree = gb_trees:from_orddict(Cleancalls),
			{noreply, State#state{queue=Newtree}}
	end;

handle_info(Info, State) ->
	?CONSOLE("got info ~p", [Info]),
	{noreply, State}.

%% @private
terminate(normal, State) ->
	?CONSOLE("Normal terminate", []),
	lists:foreach(fun({_K,V}) when is_pid(V#call.cook) -> cook:stop(V#call.cook); (_) -> ok end, gb_trees:to_list(State#state.queue)),
	ok;
terminate(shutdown, State) ->
	?CONSOLE("Shutdown terminate", []),
	lists:foreach(fun({_K,V}) when is_pid(V#call.cook) -> cook:stop(V#call.cook); (_) -> ok end, gb_trees:to_list(State#state.queue)),
	ok;
terminate(Reason, _State) ->
	?CONSOLE("unusual terminate:  ~p", [Reason]),
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% @private
%% Cleans up both dead dispatchers and dead cooks from the calls.
-spec(clean_pid/4 :: (Deadpid :: pid(), Recipe :: recipe(), Calls :: [{key(), #queued_call{}}], QName :: atom()) -> [{key(), #queued_call{}}]).
clean_pid(Deadpid, Recipe, [{Key, Call} | Calls], QName) ->
	?CONSOLE("Cleaning dead pids out...", []),
	Bound = Call#queued_call.dispatchers,
	Cleanbound = lists:delete(Deadpid, Bound),
	case Call#queued_call.cook of
		Deadpid ->
			{ok, Pid} = cook:start_link(Call#queued_call.media, Recipe, QName);
		_ ->
			Pid = Call#queued_call.cook
	end,
	Cleancall = Call#queued_call{dispatchers=Cleanbound, cook=Pid},
	[{Key, Cleancall} | clean_pid(Deadpid, Recipe, Calls, QName)];
clean_pid(_Deadpid, _Recipe, [], _QName) ->
	[].


% begin the defintions of the tests.

-ifdef(TEST).



test_primer() -> 
	["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	mnesia:start().

call_in_out_grab_test_() -> 
	{
		foreach,
		fun() -> 
			test_primer(),
			queue_manager:start([node()]),
			{ok, Pid} = queue_manager:add_queue(testqueue),
			{ok, Dummy} = dummy_media:start(#call{id="testcall", skills=[english, testskill]}),
			call_queue:add(Pid, 1, Dummy),
			register(media_dummy, Dummy),
			register(testqueue, Pid),
			{Pid, Dummy}
		end,
		fun({Pid, Dummy}) -> 
			unregister(media_dummy),
			exit(Dummy, normal),
			?CONSOLE("Das pid:  ~p", [Pid]),
			try call_queue:stop(Pid)
			catch
				What1:Why1 ->
					?CONSOLE("Cleanup of call_queue caught ~p:~p", [What1, Why1])
			end,
			case whereis(queue_manager) of
				undefined ->
					?CONSOLE("queue_manager already dead.", []);
				_Else -> 
					try queue_manager:stop()
					catch
						What2:Why2 ->
							?CONSOLE("Cleanup of queue_manager caught ~p:~p", [What2, Why2])
					end
			end
		end,
		[
			{
				"Simple add", fun() ->
					Pid = whereis(testqueue),
					%{ok, _Dummy1} = dummy_media:start(#call{id="C1"}),
					%?assertMatch(ok, add(Pid, 1, Dummy1)),
					%% was added in the set-up, just make sure the data's valid.
					{{Priority, _Time}, Queuedcall} = ask(Pid),
					?assertEqual(Queuedcall#queued_call.media, whereis(media_dummy)),
					?assert(is_list(Queuedcall#queued_call.skills)),
					?assert(is_process_alive(Queuedcall#queued_call.cook)),
					?assertEqual(1, Priority)
				end
			}, {
				"Remove by ID", fun() -> 
					Pid = whereis(testqueue),
					?assertEqual(ok, remove(Pid, "testcall")),
					?assertEqual(none, remove(Pid, "testcall"))
				end
			}, {
				"Remove by Pid", fun() -> 
					Pid = whereis(testqueue),
					Mediapid = whereis(media_dummy),
					?assertEqual(ok, remove(Pid, Mediapid)),
					?assertEqual(none, remove(Pid, Mediapid))
				end
			}, {
				"Remove non-esistant call", fun() -> 
					Pid = whereis(testqueue),
					?assertEqual(none, remove(Pid, "not_an_id_or_pid"))
				end
			}, {
				"Find call by UID", fun() ->
					Pid = whereis(testqueue),
					{ok, Dummy1} = dummy_media:start(#call{id="C1"}),
					add(Pid, 1, Dummy1),
					{_Key1, Call1} = get_call(Pid, "testcall"),
					?assert(Call1#queued_call.media =:= whereis(media_dummy)),
					{_Key2, Call2} = get_call(Pid, "C1"),
					?assert(Dummy1 =:= Call2#queued_call.media),
					?assertEqual(none, get_call(Pid, "invalid_id")),
					dummy_media:stop(Dummy1)
				end
			}, {
				"Find call by pid", fun() ->
					Pid = whereis(testqueue),
					Dummy1 = whereis(media_dummy),
					{ok, Dummy2} = dummy_media:start(#call{id="C2"}),
					{ok, Dummy3} = dummy_media:start(#call{id="C3"}),
					add(Pid, 1, Dummy2),
					{_Key1, Call1} = get_call(Pid, Dummy1),
					{_Key2, Call2} = get_call(Pid, Dummy2),
					?assertEqual("testcall", Call1#queued_call.id),
					?assertEqual("C2", Call2#queued_call.id),
					?assertEqual(none, get_call(Pid, Dummy3)),
					dummy_media:stop(Dummy2),
					dummy_media:stop(Dummy3)
				end
			}, {
				"Grab binds once", fun() ->
					Pid = whereis(testqueue),
					%Dummy1 = whereis(media_dummy),
					{_Key, Call} = grab(Pid),
					?assertEqual("testcall", Call#queued_call.id),
					?assertEqual(none, grab(Pid))
				end
			}, {
				"Grab from an emtpy call queue", fun() ->
					Pid = whereis(testqueue),
					Dummy1 = whereis(media_dummy),
					remove(Pid, Dummy1),
					?assertEqual(none, grab(Pid))
				end
			}, {
				"Grab priority testing", fun() ->
					Pid = whereis(testqueue),
					%Dummy1 = whereis(media_dummy),
					{ok, Dummy2} = dummy_media:start(#call{id="C2"}),
					{ok, Dummy3} = dummy_media:start(#call{id="C3"}),
					add(Pid, 0, Dummy2),
					add(Pid, 1, Dummy3),
					{_Key2, Call2} = grab(Pid),
					?assertEqual("C2", Call2#queued_call.id),
					{_Key1, Call1} = grab(Pid),
					{_Key3, Call3} = grab(Pid),
					?assertEqual("testcall", Call1#queued_call.id),
					?assertEqual("C3", Call3#queued_call.id),
					?assertEqual(none, grab(Pid))
				end
			}, {
				"Ungrabbing", fun() -> 
					Pid = whereis(testqueue),
					{_Key1, Call1} = grab(Pid),
					ungrab(Pid, whereis(media_dummy)),
					{_Key2, Call2} = grab(Pid),
					?assert(Call1#queued_call.media =:= Call2#queued_call.media)
				end
			}
		]
	}.
	
call_update_test_() -> 
	{
		foreach,
		fun() -> 
			test_primer(),
			queue_manager:start([node()]),
			{ok, Pid} = queue_manager:add_queue(testqueue),
			{ok, Dummy} = dummy_media:start(#call{id="testcall", skills=[english, testskill]}),
			call_queue:add(Pid, 1, Dummy),
			register(media_dummy, Dummy),
			register(testqueue, Pid),
			{Pid, Dummy}
		end,
		fun({Pid, Dummy}) -> 
			unregister(media_dummy),
			exit(Dummy, normal),
			?CONSOLE("Das pid:  ~p", [Pid]),
			try call_queue:stop(Pid)
			catch
				What1:Why1 ->
					?CONSOLE("Cleanup of call_queue caught ~p:~p", [What1, Why1])
			end,
			case whereis(queue_manager) of
				undefined ->
					?CONSOLE("queue_manager already dead.", []);
				_Else -> 
					try queue_manager:stop()
					catch
						What2:Why2 ->
							?CONSOLE("Cleanup of queue_manager caught ~p:~p", [What2, Why2])
					end
			end
		end,
		[
			{
				"Set priority by id", fun() -> 
					Pid = whereis(testqueue),
					?assertEqual(ok, set_priority(Pid, "testcall", 2)),
					{Key, Call} = get_call(Pid, "testcall"),
					?assertEqual("testcall", Call#queued_call.id),
					?assertMatch({2, {_Macroseconds, _Seconds, _Microseconds}}, Key)
				end
			}, {
				"Set priority by pid", fun() -> 
					Pid = whereis(testqueue),
					Mediapid = whereis(media_dummy),
					?assertEqual(ok, set_priority(Pid, Mediapid, 2)),
					{Key, Call} = get_call(Pid, Mediapid),
					?assertEqual(Mediapid, Call#queued_call.media),
					?assertMatch({2, _Time}, Key)
				end
			}, {
				"Set priority of non-existant call", fun() -> 
					Pid = whereis(testqueue),
					?assertMatch(none, set_priority(Pid, "Not a valid id", 5))
				end
			}, {
				"increase priority", fun() -> 
					Pid = whereis(testqueue),
					Dummy = whereis(media_dummy),
					remove(Pid, Dummy),
					{ok, Dummy1} = dummy_media:start(#call{id="C1"}),
					{ok, Dummy2} = dummy_media:start(#call{id="C2"}),
					{ok, Dummy3} = dummy_media:start(#call{id="C3"}),
					add(Pid, 1, Dummy1),
					add(Pid, 1, Dummy2),
					add(Pid, 1, Dummy3),
					{_Key1, Call1} = ask(Pid),
					?assertEqual("C1", Call1#queued_call.id),
					set_priority(Pid, Dummy2, 0),
					{_Key2, Call2} = ask(Pid),
					?assertEqual("C2", Call2#queued_call.id)
				end
			}, {
				"decrease priority", fun() ->
					Pid = whereis(testqueue),
					Dummy = whereis(media_dummy),
					remove(Pid, Dummy),
					{ok, Dummy1} = dummy_media:start(#call{id="C1"}),
					{ok, Dummy2} = dummy_media:start(#call{id="C2"}),
					{ok, Dummy3} = dummy_media:start(#call{id="C3"}),
					add(Pid, 1, Dummy1),
					add(Pid, 1, Dummy2),
					add(Pid, 1, Dummy3),
					{_Key1, Call1} = ask(Pid),
					?assertEqual("C1", Call1#queued_call.id),
					set_priority(Pid, Dummy1, 2),
					{_Key2, Call2} = ask(Pid),
					?assertEqual("C2", Call2#queued_call.id)
				end
			}, {
				"Skill integrity test", fun() ->
					Pid = whereis(testqueue),
					{ok, Dummy1} = dummy_media:start(#call{id="C1", skills=[foo, bar, '_node']}),
					add(Pid, Dummy1),
					{_Key, Call} = get_call(Pid, Dummy1),
					?assertEqual(true, lists:member(foo, Call#queued_call.skills)),
					?assertEqual(false, lists:member(baz, Call#queued_call.skills)),
					?assertEqual(4, length(Call#queued_call.skills))
				end
			}, {
				"Add skills test", fun() ->
					Pid = whereis(testqueue),
					{ok, Dummy1} = dummy_media:start(#call{id="C1"}),
					?assertEqual(ok, add(Pid, Dummy1)),
					{_Key, Call} = get_call(Pid, "C1"),
					?assertEqual(false, lists:member(foo, Call#queued_call.skills)),
					?assertEqual(false, lists:member(bar, Call#queued_call.skills)),
					?assertEqual(ok, add_skills(Pid, "C1", [foo, bar])),
					{_Key, Call2} = get_call(Pid, "C1"),
					?assertEqual(true, lists:member(foo, Call2#queued_call.skills)),
					?assertEqual(true, lists:member(bar, Call2#queued_call.skills))
				end
			}, {
				"Add skills to unknown call test", fun() ->
					Pid = whereis(testqueue),
					?assertEqual(none, add_skills(Pid, "C2", [foo, bar]))
				end
			}, {
				"Add skills to call reference by pid", fun() ->
					Pid = whereis(testqueue),
					Dummy1 = whereis(media_dummy),
					add(Pid, Dummy1),
					{_Key, Call} = get_call(Pid, Dummy1),
					?assertEqual(false, lists:member(foo, Call#queued_call.skills)),
					?assertEqual(false, lists:member(bar, Call#queued_call.skills)),
					?assertEqual(ok, add_skills(Pid, Dummy1, [foo, bar])),
					{_Key, Call2} = get_call(Pid, Dummy1),
					?assertEqual(true, lists:member(foo, Call2#queued_call.skills)),
					?assertEqual(true, lists:member(bar, Call2#queued_call.skills))
				end
			}, {
				"Remove skills test", fun() ->
					Pid = whereis(testqueue),
					{ok, Dummy1} = dummy_media:start(#call{id="C1"}),
					?assertEqual(ok, add(Pid, Dummy1)),
					{_Key, Call} = get_call(Pid, "C1"),
					?assertEqual(true, lists:member(english, Call#queued_call.skills)),
					?assertEqual(ok, remove_skills(Pid, "C1", [english])),
					{_Key, Call2} = get_call(Pid, "C1"),
					?assertEqual(false, lists:member(english, Call2#queued_call.skills))
				end
			}, {
				"Remove skills from unknown call test", fun() ->
					Pid = whereis(testqueue),
					{ok, Dummy1} = dummy_media:start(#call{id = "C1"}),
					?assertEqual(ok, add(Pid, Dummy1)),
					?assertEqual(none, remove_skills(Pid, "C2", [english]))
				end
			}, {
				"Remove skills from call referenced by pid", fun() -> 
					Pid = whereis(testqueue),
					{ok, Dummy1} = dummy_media:start(#call{id="C1"}),
					add(Pid, Dummy1),
					{_Key, Call} = get_call(Pid, "C1"),
					?assertEqual(true, lists:member(english, Call#queued_call.skills)),
					?assertEqual(ok, remove_skills(Pid, Dummy1, [english])),
					{_Key, Call2} = get_call(Pid, "C1"),
					?assertEqual(false, lists:member(english, Call2#queued_call.skills))
				end
			}, {
				"Add magic skills test", fun() ->
					Pid = whereis(testqueue),
					{ok, Dummy1} = dummy_media:start(#call{id="C1"}),
					?assertEqual(ok, add(Pid, Dummy1)),
					{_Key, Call} = get_call(Pid, "C1"),
					?assertEqual(false, lists:member(testq, Call#queued_call.skills)),
					?assertEqual(ok, add_skills(Pid, "C1", ['_queue'])),
					{_Key, Call2} = get_call(Pid, "C1"),
					?debugFmt("Skills are ~p~n", [Call2#queued_call.skills]),
					?assertEqual(true, lists:member(testqueue, Call2#queued_call.skills))
				end
			}, {
				"Remove magic skills test", fun() ->
					Pid = whereis(testqueue),
					{ok, Dummy1} = dummy_media:start(#call{id="C1", skills=['_node']}),
					?assertEqual(ok, add(Pid, Dummy1)),
					{_Key, Call} = get_call(Pid, "C1"),
					?debugFmt("Queued_call:  ~p; this node:  ~p", [Call, node()]),
					?assertEqual(true, lists:member(node(), Call#queued_call.skills)),
					?assertEqual(ok, remove_skills(Pid, "C1", ['_node'])),
					{_Key, Call2} = get_call(Pid, "C1"),
					?assertEqual(false, lists:member(node(), Call2#queued_call.skills))
				end
			}, {
				"Ensure that call_skills are merged into the call's skill list on add", fun() ->
					Pid = whereis(testqueue),
					{ok, Dummy1} = dummy_media:start(#call{id="C1", skills=[madness]}),
					?assertEqual(ok, add(Pid, Dummy1)),
					{_Key, Call} = get_call(Pid, "C1"),
					?assertEqual(true, lists:member(node(), Call#queued_call.skills)),
					?assertEqual(true, lists:member(english, Call#queued_call.skills))
				end
			}
		]
	}.
	
queue_update_and_info_test_() -> 
	{
		foreach,
		fun() -> 
			test_primer(),
			queue_manager:start([node()]),
			{ok, Pid} = queue_manager:add_queue(testqueue),
			{ok, Dummy} = dummy_media:start(#call{id="testcall", skills=[english, testskill]}),
			call_queue:add(Pid, 1, Dummy),
			register(media_dummy, Dummy),
			register(testqueue, Pid),
			{Pid, Dummy}
		end,
		fun({Pid, Dummy}) -> 
			unregister(media_dummy),
			exit(Dummy, normal),
			?CONSOLE("Das pid:  ~p", [Pid]),
			try call_queue:stop(Pid)
			catch
				What1:Why1 ->
					?CONSOLE("Cleanup of call_queue caught ~p:~p", [What1, Why1])
			end,
			case whereis(queue_manager) of
				undefined ->
					?CONSOLE("queue_manager already dead.", []);
				_Else -> 
					try queue_manager:stop()
					catch
						What2:Why2 ->
							?CONSOLE("Cleanup of queue_manager caught ~p:~p", [What2, Why2])
					end
			end
		end,
		[
			{
				"Change weight test", fun() ->
					Pid = whereis(testqueue),
					?assertEqual(ok, set_weight(Pid, 2)),
					?assertEqual(2, get_weight(Pid))
				end
			}, {
				"Invalid weight change test", fun() ->
					Pid = whereis(testqueue),
					?assertEqual(error, set_weight(Pid, -1)),
					?assertEqual(?DEFAULT_WEIGHT, get_weight(Pid))
				end
			}, {
				"Call Count Test", fun() ->
					Pid = whereis(testqueue),
					?assertEqual(1, call_count(Pid)),
					?assertEqual(ok, remove(Pid, "testcall")),
					?assertEqual(0, call_count(Pid)),
					?assertEqual(none, remove(Pid, "testcall"))
				end
			}, {
				"Dump queue data to a list", fun() ->
					Pid = whereis(testqueue),
					%Dummy1 = whereis(media_dummy),
					{ok, Dummy2} = dummy_media:start(#call{id="C2"}),
					{ok, Dummy3} = dummy_media:start(#call{id="C3"}),
					add(Pid, 1, Dummy2),
					add(Pid, 1, Dummy3),
					F = fun(X) -> 
						X#queued_call.id
					end,
					?assertMatch(["testcall", "C2", "C3"], lists:map(F, to_list(Pid)))
				end
			}, {
				"Empty queue to list", fun() ->
					Pid = whereis(testqueue),
					remove(Pid, "testcall"),
					?assertMatch([], to_list(Pid))
				end
			}, {
				"Change recipe", fun() -> 
					Pid = whereis(testqueue),
					#state{recipe = ?DEFAULT_RECIPE} = print(Pid),
					NewRecipe = [{3, set_priority, 5, run_many}],
					?assertEqual(ok, set_recipe(Pid, NewRecipe)),
					?assertMatch(#state{recipe = NewRecipe}, print(Pid))				
				end
			}
		]
	}.
	
queue_manager_and_cook_test_() ->
	{
		timeout,
		60,
		{
			foreach,
			fun() ->
				queue_manager:start([node()]),
				{ok, Pid} = queue_manager:add_queue(testqueue),
				{ok, Dummy} = dummy_media:start(#call{id="testcall", skills=[english, testskill]}),
				call_queue:add(Pid, 1, Dummy),
				register(media_dummy, Dummy),
				{Pid, Dummy}
			end,
			fun({Pid, Dummy}) -> 
				unregister(media_dummy),
				exit(Dummy, normal),
				?CONSOLE("Das pid:  ~p", [Pid]),
				try call_queue:stop(Pid)
				catch
					What1:Why1 ->
						?CONSOLE("Cleanup of call_queue caught ~p:~p", [What1, Why1])
				end,
				case whereis(queue_manager) of
					undefined ->
						?CONSOLE("queue_manager already dead.", []);
					_Else -> 
						try queue_manager:stop()
						catch
							What2:Why2 ->
								?CONSOLE("Cleanup of queue_manager caught ~p:~p", [What2, Why2])
						end
				end
			end,
			[
				{
					"Slaughter the cook",
					fun() ->
						{exists, Pid} = queue_manager:add_queue(testqueue),
						Dummy1 = whereis(media_dummy),
						{_Key1, Call1} = get_call(Pid, Dummy1),
						?assertEqual(Call1#queued_call.media, Dummy1),
						?CONSOLE("Dummy1: ~p~nCall1:  ~p~nQPid:  ~p", [Dummy1, Call1, Pid]),
						gen_server:call(Call1#queued_call.cook, {stop, test_kill}),
						?assert(is_process_alive(Call1#queued_call.cook) =:= false),
						receive
						after 300 ->
							ok
						end,
						{_Key2, Call2} = get_call(Pid, Dummy1),
						?assert(is_process_alive(Call2#queued_call.cook)),
						?assert(Call2#queued_call.cook =/= Call1#queued_call.cook),
						?assert(Call1#queued_call.media =:= Call2#queued_call.media)
					end
				}
			]
		}
	}.
	
-define(MYSERVERFUNC, fun() -> {ok, Pid} = start(testq, ?DEFAULT_RECIPE, ?DEFAULT_WEIGHT), {Pid, fun() -> stop(Pid) end} end).

-include("gen_server_test.hrl").

-endif.
