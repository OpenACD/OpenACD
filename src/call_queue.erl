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

%% @doc An inplementation of priority queues, with parallel call delivery.
%% The calls in queue can be re-prioritized at any time and they can have 
%% skills added/removed to facilitate scripted/dynamic call delivery.
%% Each call can be 'bound' to by a single dispatcher from each node in a 
%% cluster.

-module(call_queue).

%% depends on util, agent, cook, queue_manager

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([fake_dispatcher/0]).
-endif.

-behaviour(gen_server).

-include("log.hrl").
-include("call.hrl").
-include("queue.hrl").

-type(key() :: {non_neg_integer(), {pos_integer(), non_neg_integer(), non_neg_integer()}}).

-type(call_queue() :: gb_tree()).

-export([
	start/2,
	start_link/2,
	set_recipe/2,
	set_weight/2,
	get_weight/1,
	add/4,
	add/3,
	add/2,
	add_at/3,
	ask/1,
	get_call/2,
	get_calls/1,
	dump/1,
	remove/2,
	bgremove/2,
	migrate/2,
	stop/1,
	grab/1,
	ungrab/2,
	set_priority/3,
	to_list/1,
	add_skills/3,
	remove_skills/3,
	call_count/1,
	call_count_by_client/2,
	selection_info/1
]).

-type(call_key() :: {pos_integer(), {pos_integer(), pos_integer(), pos_integer()}}).
-record(state, {
	queue = gb_trees:empty() :: call_queue(),
	group = "Default" :: string(),
	name = erlang:error({undefined, name}) :: string(),
	recipe = ?DEFAULT_RECIPE :: recipe(),
	weight = ?DEFAULT_WEIGHT :: pos_integer(),
	call_skills = [english, '_node'] :: [atom()],
	last_service = os:timestamp() ::{pos_integer(), pos_integer(), pos_integer()}
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

%gen_server support
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type(opt_name() :: 'weight' | 'recipe' | 'skills' | 'group').
-type(start_opt() :: {opt_name(), any()}).
-type(opts() :: [start_opt()]).
%% @doc Start a queue named `Name' with no link to the current process.  Sets the `recipe()' to `Recipe' and 
%% `Weight' to pos_integer.
-spec(start/2 :: (Name :: string(), Opts :: opts()) -> {ok, pid()}).
start(Name, Opts) -> % Start linked queue custom default recipe and weight
	gen_server:start(?MODULE, [Name, Opts], []).

%% @doc Start a queue names `Name' with a link to the current process.
%% @see start/3
-spec(start_link/2 :: (Name :: string(), Opts :: opts()) -> {ok, pid()}).
start_link(Name, Opts) -> % Start linked queue with custom recipe and weight
	gen_server:start_link(?MODULE, [Name, Opts], []).

%% @doc Set the queue at pid() `Pid''s recipe to `recipe()' `Recipe'.
-spec(set_recipe/2 :: (Pid :: pid(), Recipe :: recipe()) -> 'ok' | 'error').
set_recipe(Pid, Recipe) ->
	gen_server:call(Pid, {set_recipe, Recipe}).

%% @doc Set the queue at pid() `Pid''s weight to `pos_integer()' `Weight'.
-spec(set_weight/2 :: (Pid :: pid(), Weight :: pos_integer()) -> 'ok' | 'error').
set_weight(Pid, Weight) ->
	gen_server:call(Pid, {set_weight, Weight}).

%% @doc Return the weight of the queue at `pid()' `Pid'.
-spec(get_weight/1 :: (Pid :: pid()) -> pos_integer()).
get_weight(Pid) ->
	gen_server:call(Pid, get_weight).

%% @doc Add to the queue at `pid()' `Pid' a new call with `pos_integer()' `Priority', `pid()' Mediapid, 
%% and `#call{}' Callrec.
-spec(add/4 :: (Pid :: pid(), Priority :: non_neg_integer(), Mediapid :: pid(), Callrec :: #call{}) -> ok).
add(Pid, Priority, Mediapid, Callrec) when is_pid(Pid), is_pid(Mediapid), Priority >= 0, is_record(Callrec, call) ->
	gen_server:call(Pid, {add, Priority, Mediapid, Callrec}).

%% @doc Add to the queue at `pid()' `Pid' a call with default values.
%% Second paramter can be either `pos_integer()' `Priority' or `pid()' `Mediapid'.
%% If the second parameter is `Priority', the 3rd parameter must be the `pid()' `Mediapid'.
%% A new call is queued using the given `Priority', `Mediapid', and result of `gen_server:call(Mediapid, get_call)'.
%%
%% If the second parameter is `Mediapid', the 3rd parameter must be `#call{}' `Callrec'.  
%% A new call is queued using the default priority of 1.
%% @see add/4
-spec(add/3 :: (Pid :: pid(), Priority :: non_neg_integer(), Mediapid :: pid()) -> ok;
	(Pid :: pid(), Mediapid :: pid(), Calldata :: #call{}) -> ok).
add(Pid, Mediapid, Callrec) when is_pid(Pid), is_pid(Mediapid), is_record(Callrec, call) ->
	add(Pid, Callrec#call.priority, Mediapid, Callrec);
add(Pid, Priority, Mediapid) when is_pid(Pid), is_pid(Mediapid), Priority >= 0 ->
	Callrec = gen_media:get_call(Mediapid),
	gen_server:call(Pid, {add, Priority, Mediapid, Callrec}).

%% @doc Add to queue at `pid()' `Pid' a call taken from `pid()' `Mediapid'.
%% A call record is gather from the media server at `Mediapid', and then
%% added to the queue with a priority of 1.
%% @see add/3
%% @see add/4
-spec(add/2 :: (Pid :: pid(), Calldata :: pid()) -> ok).
add(Pid, Mediapid) when is_pid(Pid), is_pid(Mediapid) ->
	add(Pid, 1, Mediapid).

%% @doc Add to queue at `pid()' `Pid' a call taken from `pid()' `Mediapid' with
%% a given key `Key'.
-spec(add_at/3 :: (Pid :: pid(), Key :: call_key(), Mediapid :: pid()) -> ok).
add_at(Pid, Key, Mediapid) ->
	Callrec = gen_media:get_call(Mediapid),
	gen_server:cast(Pid, {add_at, Key, Mediapid, Callrec}).

%% @doc Query the queue at `pid()' `Pid' for a call with the ID `string()' or `pid()' of `Callid'.
-spec(get_call/2 :: (Pid :: pid(), Callid :: pid()) -> {key(), #queued_call{}} | 'none';
	(Pid :: pid(), Callid :: string()) -> {key(), #queued_call{}} | 'none').
get_call(Pid, Callid) when is_pid(Pid) ->
	gen_server:call(Pid, {get_call, Callid}).

%% @doc Return `[{key(), #queued_call{}]' from the queue at `pid()'.
-spec(get_calls/1 :: (Pid :: pid()) -> [{key(), #queued_call{}}]).
get_calls(Pid) when is_pid(Pid) ->
	gen_server:call(Pid, get_calls).
	
%% @doc Return the first `{key(), #queued_call{}} call in the queue at `pid()' `Pid' that doesn't 
%% have a dispatcher from this node already bound to it or `none'.
-spec(ask/1 :: (Pid :: pid()) -> 'none' | {key(), #queued_call{}}).
ask(Pid) ->
	gen_server:call(Pid, ask).

%% @doc Bind to the first `{key, #queued_call{}} in the queue at `pid()' `Pid' that doesn't have a dispatcher from this node already bound to it or `none'.
-spec(grab/1 :: (Pid :: pid()) -> 'none' | {key(), #queued_call{}}).
grab(Pid) when is_pid(Pid) ->
	gen_server:call(Pid, grab).

%% @doc Reverse of {@link grab/1}.  Releases the call identified by `pid()' or `string()' `Callid' from any 
%% bound dispatchers at queue `pid()' `Pid'.  Returns `ok'.
-spec(ungrab/2 :: (Pid :: pid(), Callid :: string() | pid()) -> 'ok').
ungrab(Pid, Mediapid) when is_pid(Mediapid), is_pid(Pid) ->
	#call{id = Cid} = gen_media:get_call(Mediapid),
	gen_server:call(Pid, {ungrab, Cid});
ungrab(Pid, Callid) when is_pid(Pid) ->
	gen_server:call(Pid, {ungrab, Callid}).

%% @doc Add `[atom(),...]' `Skills' to the call with the id of `string()' `Callid' or `pid()' `Mediapid' in the queue at `pid()' `Pid'. 
%% Returns `ok' on success, `none' on failure.
-spec(add_skills/3 :: (Pid :: pid(), Callid :: string() | pid(), Skills :: [atom(),...]) -> 'none' | 'ok').
add_skills(Pid, Mediapid, Skills) when is_pid(Mediapid) ->
	#call{id = Callid} = gen_media:get_call(Mediapid),
	gen_server:call(Pid, {add_skills, Callid, Skills});
add_skills(Pid, Callid, Skills) ->
	gen_server:call(Pid, {add_skills, Callid, Skills}).

%% @doc Remove `[atom(),...] `Skills' from the call with the id of `string()' `Callid' or `pid()' `Mediapid' in the queue at `pid()' `Pid'. 
%% Returns `ok' on success, `none' on failure.
-spec(remove_skills/3 :: (Pid :: pid(), Callid :: string() | pid(), Skills :: [atom(),...]) -> 'none' | 'ok').
remove_skills(Pid, Mediapid, Skills) when is_pid(Mediapid) ->
	#call{id = Callid} = gen_media:get_call(Mediapid),
	gen_server:call(Pid, {remove_skills, Callid, Skills});
remove_skills(Pid, Callid, Skills) ->
	gen_server:call(Pid, {remove_skills, Callid, Skills}).

%% @doc Alter the priority of the call with the id or media pid of `pid()' `Mediaid' in the queue at `pid()' `Pid' to `non_neg_integer()' `Priority'.
%% Returns `ok' on success, `none' on failure (such as invalid `Priority').
-spec(set_priority/3 :: ( Pid :: pid(), Calldata :: string() | pid(), Priority :: non_neg_integer()) -> 'none' | 'ok').
set_priority(Pid, Mediaid, Priority) when Priority >= 0 ->
	gen_server:call(Pid, {set_priority, Mediaid, Priority});
set_priority(_Pid, _Mediapid, _Priority) ->
	none.

%% @doc Returns a list of calls in queue at `pid()' `Pid'.
-spec(to_list/1 :: (Pid :: pid()) -> [#queued_call{}]).
to_list(Pid) ->
	gen_server:call(Pid, to_list).

%% @doc returns the state of the queue at `pid()' `Pid'.
-spec(dump/1 :: (Pid :: pid()) -> any()).
dump(Pid) ->
	gen_server:call(Pid, dump).

%% @doc Remove the call with id `string()' of `Calldata' or `pid()' `Callpid' from the queue at `Pid'.  
%% Returns `ok' on success, `none' on failure.
-spec(remove/2 :: (Pid :: pid(), Callid :: pid() | string()) -> 'none' | 'ok').
remove(Pid, Callpid) when is_pid(Pid), is_pid(Callpid) ->
	gen_server:call(Pid, {remove, Callpid});
remove(Pid, Callid) when is_pid(Pid) ->
	case gen_server:call(Pid, {get_call, Callid}) of
		{_Key, #queued_call{media=Mediapid}} ->
			remove(Pid, Mediapid);
		none ->
			none
	end.
	
%% @doc Non-blocking version of {@link remove/2}
%% @see remove/2
-spec(bgremove/2 :: (Pid :: pid(), Callid :: pid() | string()) -> 'ok').
bgremove(Pid, Callpid) when is_pid(Pid), is_pid(Callpid) ->
	gen_server:cast(Pid, {remove, Callpid});
bgremove(Pid, Callid) when is_pid(Pid) ->
	case gen_server:call(Pid, {get_call, Callid}) of
		{_Key, #queued_call{media = Mediapid}} ->
			bgremove(Pid, Mediapid);
		none ->
			ok
	end.

%% @doc Return the number of calls in the queue at `pid()' `Pid'.
-spec(call_count/1 :: (Pid :: pid()) -> non_neg_integer()).
call_count(Pid) ->
	gen_server:call(Pid, call_count).

%% @doc Return the number of calls in the queue at `pid()' `Pid'.
-spec(call_count_by_client/2 :: (Pid :: pid(), Client :: #client{}) -> non_neg_integer()).
call_count_by_client(Pid, Client) ->
	gen_server:call(Pid, {call_count_by_client, Client}).

%% @doc Stop the queue at `pid()' `Pid'.
-spec(stop/1 :: (Pid :: pid()) -> 'ok').
stop(Pid) ->
	gen_server:call(Pid, stop).

% find the first call in the queue that doesn't have a pid on this node
% in its bound list
%% @private
-spec(find_unbound/2 :: (GbTree :: call_queue(), From :: pid()) -> {key(), #queued_call{}} | 'none').
find_unbound(GbTree, From) when is_pid(From) ->
	find_unbound_(gb_trees:next(gb_trees:iterator(GbTree)), From).

%% @private
-spec(find_unbound_/2 :: (Iterator :: {key(), #queued_call{}, any()} | 'none', From :: pid()) -> {key(), #queued_call{}} | 'none').
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
-spec(find_key/2 :: (Needle :: string(), GbTree :: call_queue()) -> {key(), #queued_call{}} | 'none').
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
% same as find_key, only searches by pid
-spec(find_by_pid/2 :: (Needle :: pid(), GbTree :: call_queue()) -> {key(), #queued_call{}} | 'none').
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
expand_magic_skills(State, QCall, Skills) when is_record(QCall, queued_call) ->
	Call = gen_media:get_call(QCall#queued_call.media),
	expand_magic_skills(State, Call, Skills);
expand_magic_skills(State, Call, Skills) ->
	Unfiltered = [case Skill of
		'_node' -> {Skill, node(Call#call.source)};
		'_queue' -> {Skill, State#state.name};
		'_brand' -> case Call#call.client of
			Name when is_record(Name, client) -> {Skill, Name#client.label};
			_ -> {Skill, "Unknown"}
		end;
		_ -> Skill 
	end	|| Skill <- Skills],
	[X || X <- Unfiltered, X =/= []].

%% @doc Move the queue at `Pid' to `node() Node'.
-spec(migrate/2 :: (Qpid :: pid(), Node :: atom()) -> ok).
migrate(Qpid, Node) ->
	gen_server:cast(Qpid, {migrate, Node}).

%% @doc Retrieve info used to sort queues for dispatcher binding.
-spec(selection_info/1 :: (Qpid :: pid()) -> {{key(), #queued_call{}} | 'none', pos_integer(), non_neg_integer()}).
selection_info(Qpid) ->
	gen_server:call(Qpid, selection_info).

%=====
% gen_server callbacks
%=====

%% @private
init([Name, Opts]) ->
	?DEBUG("Starting queue ~p at ~p", [Name, node()]),
	process_flag(trap_exit, true),
	State = #state{
		name = Name,
		group = proplists:get_value(group, Opts, "Default"),
		recipe = proplists:get_value(recipe, Opts, ?DEFAULT_RECIPE),
		weight = proplists:get_value(weight, Opts, ?DEFAULT_WEIGHT),
		call_skills = proplists:get_value(skills, Opts, [])
	},
	set_cpx_mon(State, self()),
	{ok, State}.

%% =====
%% handle_call
%% =====

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
	set_cpx_mon(State#state{weight=Weight}),
	{reply, ok, State#state{weight=Weight}};
handle_call({set_weight, _Weight}, _From, State) -> % invalid weight
	{reply, error, State};
handle_call(get_weight, _From, State) ->
	{reply, State#state.weight, State};
handle_call({set_recipe, Recipe}, _From, State) ->
	{reply, ok, State#state{recipe=Recipe}};
handle_call({add, Priority, Callpid, Callrec}, From, State) when is_pid(Callpid) ->
	% cook is started on the same node callpid is on
	?INFO("adding call ~p to ~p request from ~p on node ~p", [Callrec#call.id, State#state.name, From, node(Callpid)]),
	Key = {Priority, now()},
	{ok, Cookpid} = cook:start_at(node(Callpid), Callpid, State#state.recipe, State#state.name, self(), Key),
	NewState = queue_call(Cookpid, Callrec, Key, State),
	{reply, ok, NewState};
handle_call({add_skills, Callid, Skills}, _From, State) ->
	case find_key(Callid, State#state.queue) of
		none ->
			{reply, none, State};
		{Key, #queued_call{skills=OldSkills} = Value} ->
			Skills2 = expand_magic_skills(State, Value, Skills),
			NewSkills = util:merge_skill_lists(OldSkills, Skills2),
			State2 = State#state{queue=gb_trees:update(Key, Value#queued_call{skills=NewSkills}, State#state.queue)},
			Telldp = fun(Pid) ->
				gen_server:cast(Pid, {update_skills, NewSkills})
			end,
			lists:foreach(Telldp, Value#queued_call.dispatchers),
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
			Telldp = fun(Pid) ->
				gen_server:cast(Pid, {update_skills, NewSkills})
			end,
			lists:foreach(Telldp, Value#queued_call.dispatchers),
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
			link(From), % to catch exits from the dispatcher so we can clean out dead pids
			State2 = State#state{queue=gb_trees:update(Key, Value#queued_call{dispatchers=lists:append(Value#queued_call.dispatchers, [From])}, State#state.queue), last_service=os:timestamp()},
			Value#queued_call.cook ! grab, % notify the cook that we grabbed
			{reply, {Key, Value}, State2}
	end;

handle_call(dump, _From, State) ->
	{reply, State, State};

handle_call({remove, Callpid}, _From, State) ->
	?INFO("Trying to remove call ~p from ~p", [Callpid, State#state.name]),
	case find_by_pid(Callpid, State#state.queue) of
		none ->
			?INFO("Did not find call ~w in ~p", [Callpid, State#state.name]),
			{reply, none, State};
		{Key, #queued_call{cook=Cookpid} = Qcall} ->
			unlink(Cookpid),
			gen_server:cast(Cookpid, stop),
			State2 = State#state{queue=gb_trees:delete(Key, State#state.queue)},
			lists:foreach(fun(D) -> exit(D, kill) end, Qcall#queued_call.dispatchers),
			set_cpx_mon(State2),
			?INFO("Removed call ~p from queue ~p", [Qcall#queued_call.id, State#state.name]),
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
	{reply, gb_trees:values(State#state.queue), State};

handle_call(get_calls, _From, #state{queue = Calls} = State) ->
	{reply, gb_trees:to_list(Calls), State};
	
handle_call(call_count, _From, State) ->
	{reply, gb_trees:size(State#state.queue), State};

handle_call({call_count_by_client, Client}, _From, State) ->
	% TODO - if this is too inefficent, micah can make cpx_monitor do it
	Count = length(lists:filter(
		fun(Qcall) ->
				#call{client = Client2} = gen_media:get_call(Qcall#queued_call.media), Client#client.id == Client2#client.id
		end, gb_trees:values(State#state.queue))),
	{reply, Count, State};

handle_call(selection_info, {From, _tag}, State) ->
	{reply, {find_unbound(State#state.queue, From), State#state.weight, State#state.last_service}, State};

handle_call(Request, _From, State) ->
	{reply, {unknown_call, Request}, State}.

%% =====
%% handle_cast
%% =====

%% @private
handle_cast({remove, Callpid}, State) ->
	?INFO("Trying to remove call ~p (cast) from ~p", [Callpid, State#state.name]),
	case find_by_pid(Callpid, State#state.queue) of
		none ->
			?INFO("Did not find ~p (cast) in ~p", [Callpid, State#state.name]),
			{noreply, State};
		{Key, #queued_call{cook=Cookpid, id=ID}} ->
			unlink(Cookpid),
			gen_server:cast(Cookpid, stop),
			State2 = State#state{queue=gb_trees:delete(Key, State#state.queue)},
			set_cpx_mon(State2),
			?INFO("Removed ~p from ~p", [ID, State#state.name]),
			{noreply, State2}
	end;
handle_cast({add_at, Key, Mediapid, Mediarec}, State) ->
	?INFO("adding call ~p to ~p on node ~p at position ~p", [Mediarec#call.id, State#state.name, node(Mediapid), Key]),
	% cook is started on the same node Mediapid is on
	{ok, Cookpid} = cook:start_at(node(Mediapid), Mediapid, State#state.recipe, State#state.name, self(), Key),
	NewState = queue_call(Cookpid, Mediarec, Key, State),
	{noreply, NewState};
handle_cast({migrate, Node}, State) when is_atom(Node) ->
	{stop, {move, Node}, State};
handle_cast({update, Opts}, State) ->
	Newstate = State#state{
		group = proplists:get_value(group, Opts, State#state.group),
		recipe = proplists:get_value(recipe, Opts, State#state.recipe),
		weight = proplists:get_value(weight, Opts, State#state.weight),
		call_skills = proplists:get_value(skills, Opts, State#state.call_skills)
	},
	{noreply, Newstate};
handle_cast(Msg, State) ->
	?DEBUG("Unhandled cast ~p for ~p", [Msg, State#state.name]),
	{noreply, State}.

%% =====
%% handle_info
%% =====

%% @private
handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
	?NOTICE("~p Handling down process ~p due to ~p", [State#state.name, Pid, Reason]),
	case find_by_pid(Pid, State#state.queue) of
		none ->
			?INFO("~p Did not find pid ~w", [State#state.name, Pid]),
			{noreply, State};
		{Key, #queued_call{cook=Cookpid, dispatchers = Dips}} ->
			gen_server:cast(Cookpid, stop),
			lists:foreach(fun(D) -> exit(D, kill) end, Dips),
			State2 = State#state{queue=gb_trees:delete(Key, State#state.queue)},
			set_cpx_mon(State2),
			{noreply, State2}
	end;
handle_info({'EXIT', From, Reason}, State) ->
	case whereis(queue_manager) of
		undefined ->
			?ERROR("~p Can't find the manager.  dying", [State#state.name]),
			{stop, {queue_manager, Reason}, State};
		From ->
			?NOTICE("~p Handling exit of queue manager with reason ~p.  Dying with it.", [State#state.name, Reason]),
			{stop, {queue_manager_died, Reason}, State};
		_Else ->
			?DEBUG("~p ~w exited due to ~p; looping through calls", [State#state.name, From, Reason]),
			Calls = gb_trees:to_list(State#state.queue),
			Cleancalls = clean_pid(From, State#state.recipe, Calls, State#state.name),
			Newtree = gb_trees:from_orddict(Cleancalls),
			set_cpx_mon(State#state{queue=Newtree}),
			{noreply, State#state{queue=Newtree}}
	end;
handle_info(Info, State) ->
	?DEBUG("~p got info ~p", [State#state.name, Info]),
	{noreply, State}.

%% =====
%% terminate
%% =====

%% @private
terminate(Reason, State) when is_atom(Reason) andalso Reason =:= normal orelse Reason =:= shutdown ->
	?NOTICE("~p terminated with reason ~p", [State#state.name, Reason]),
	lists:foreach(fun({_K,V}) when is_pid(V#call.cook) -> cook:stop(V#call.cook); (_) -> ok end, gb_trees:to_list(State#state.queue)),
	set_cpx_mon(State, delete),
	ok;
terminate(Reason, State) ->
	?NOTICE("~p unusual terminate:  ~p", [State#state.name, Reason]),
	set_cpx_mon(State, delete),
	ok.

%% =====
%% code_chnage
%% =====

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
	
%% =====
%% Internal Functions
%% =====

queue_call(Cookpid, Callrec, Key, State) ->
	Queuedrec = #queued_call{media = Callrec#call.source, cook = Cookpid, id = Callrec#call.id},
	NewSkills = lists:umerge(lists:sort(State#state.call_skills), lists:sort(Callrec#call.skills)),
	Expandedskills = expand_magic_skills(State, Callrec, NewSkills),
	Value = Queuedrec#queued_call{skills=Expandedskills},
	try gb_trees:insert(Key, Value, State#state.queue) of
		Trees ->
			link(Cookpid),
			erlang:monitor(process, Callrec#call.source),
			set_cpx_mon(State#state{queue=Trees}),
			State#state{queue = Trees}
	catch
		error:{key_exists, _} ->
			?ERROR("Call ~p is already queued in ~p at ~p", [Callrec#call.id, State#state.name, Key]),
			State
	end.

%% @private
-spec(set_cpx_mon/1 :: (State :: #state{}) -> 'ok').
set_cpx_mon(State) ->
	set_cpx_mon(State, ignore).

%% @private
-spec(set_cpx_mon/2 :: (State :: #state{}, 'delete') -> 'ok').
set_cpx_mon(State, delete) ->
	cpx_monitor:drop({queue, State#state.name});
set_cpx_mon(State, Watch) ->
	Key = {queue, State#state.name},
	MidDetails = [
		{weight, State#state.weight},
		{group, State#state.group},
		{calls, gb_trees:size(State#state.queue)}
	],
	Details = case gb_trees:is_empty(State#state.queue) of
		true ->
			MidDetails;
		false ->
			{{_, {Megsec, Sec, _}}, _} = gb_trees:smallest(State#state.queue),
			[{oldest, {timestamp, (Megsec * 1000000) + Sec}} | MidDetails]
	end,
	cpx_monitor:set(Key, Details, Watch).

%% @private
%% Cleans up both dead dispatchers and dead cooks from the calls.
-spec(clean_pid/4 :: (Deadpid :: pid(), Recipe :: recipe(), Calls :: [{key(), #queued_call{}}], QName :: string()) -> [{key(), #queued_call{}}]).
clean_pid(Deadpid, Recipe, Calls, QName) ->
	?INFO("Cleaning dead pids out...", []),
	clean_pid_(Deadpid, Recipe, QName, Calls, []).

clean_pid_(_Deadpid, _Recipe, _QName, [], Acc) ->
	lists:reverse(Acc);
clean_pid_(Deadpid, Recipe, QName, [{Key, #queued_call{cook = Deadpid} = Call} | Calls], Acc) ->
	{ok, Pid} = cook:start_at(node(Call#queued_call.media), Call#queued_call.media, Recipe, QName, self(), Key),
	?NOTICE("Cook for ~p died - respawning as ~p", [Call#queued_call.id, Pid]),
	link(Pid),
	Cleancall = Call#queued_call{cook = Pid},
	gen_media:set_cook(Call#queued_call.media, Pid),
	lists:append(lists:reverse(Acc), [{Key, Cleancall} | Calls]);
clean_pid_(Deadpid, Recipe, QName, [{Key, Call} | Calls], Acc) ->
	case lists:member(Deadpid, Call#queued_call.dispatchers) of
		false ->
			clean_pid_(Deadpid, Recipe, QName, Calls, [{Key, Call} | Acc]);
		true ->
			Cleanbound = lists:delete(Deadpid, Call#queued_call.dispatchers),
			Cleancall = Call#queued_call{dispatchers = Cleanbound},
			lists:append(lists:reverse(Acc), [{Key, Cleancall} | Calls])
	end.
	
% begin the defintions of the tests.

-ifdef(TEST).
test_primer() ->
	%["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
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
			{ok, Pid} = queue_manager:add_queue("testqueue", []),
			{ok, Dummy} = dummy_media:start([{id, "testcall"}, {skills, [english, testskill]}, {queues, none}]),
			%dummy_media:set_skills(Dummy, [english, testskill]),
			call_queue:add(Pid, 1, Dummy),
			register(media_dummy, Dummy),
			register(testqueue, Pid),
			{Pid, Dummy}
		end,
		fun({Pid, Dummy}) ->
			unregister(media_dummy),
			exit(Dummy, normal),
			?CONSOLE("Das pid:  ~p", [Pid]),
			call_queue:stop(Pid),
			queue_manager:stop()
			%try call_queue:stop(Pid)
			%catch
				%What1:Why1 ->
					%?CONSOLE("Cleanup of call_queue caught ~p:~p", [What1, Why1])
			%end,
			%case whereis(queue_manager) of
				%undefined ->
					%?CONSOLE("queue_manager already dead.", []);
				%_Else ->
					%try queue_manager:stop()
					%catch
						%What2:Why2 ->
							%?CONSOLE("Cleanup of queue_manager caught ~p:~p", [What2, Why2])
					%end
			%end
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
				"Remove by casted ID", fun() ->
					Pid = whereis(testqueue),
					?assertEqual(ok, bgremove(Pid, "testcall")),
					?assertEqual(none, remove(Pid, "testcall"))
				end
			}, {
				"Remove by non-existant casted ID", fun() ->
					Pid = whereis(testqueue),
					?assertEqual(ok, bgremove(Pid, "foo"))
				end
			}, {
				"Remove by casted pid", fun() ->
					Pid = whereis(testqueue),
					Mediapid = whereis(media_dummy),
					?assertEqual(ok, bgremove(Pid, Mediapid)),
					?assertEqual(none, remove(Pid, Mediapid))
				end
			}, {
				"Remove by non-existant casted pid", fun() ->
					Pid = whereis(testqueue),
					?assertEqual(ok, bgremove(Pid, self()))
				end
			}, {
				"Remove non-existant call by id", fun() ->
					Pid = whereis(testqueue),
					?assertEqual(none, remove(Pid, "not_an_id_or_pid"))
				end
			}, {
				"Remove non-existant call by pid", fun() ->
					Pid = whereis(testqueue),
					?assertEqual(none, remove(Pid, self()))
				end
			}, {
				"Find call by UID", fun() ->
					Pid = whereis(testqueue),
					{ok, Dummy1} = dummy_media:start([{id, "C1"}, {queues, none}]),
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
					{ok, Dummy2} = dummy_media:start([{id, "C2"}, {queues, none}]),
					{ok, Dummy3} = dummy_media:start([{id, "C3"}, {queues, none}]),
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
					{ok, Dummy2} = dummy_media:start([{id, "C2"}, {queues, none}]),
					{ok, Dummy3} = dummy_media:start([{id, "C3"}, {queues, none}]),
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
			}, {
				"Ungrab nonexistant call", fun() ->
					Pid = whereis(testqueue),
					?assertEqual(ok, ungrab(Pid, "foo"))
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
			{ok, Pid} = queue_manager:add_queue("testqueue", [{skills, [english, '_node']}]),
			{ok, Dummy} = dummy_media:start([{id, "testcall"}, {skills, [english, testskill]}, {queues, none}]),
			%dummy_media:set_skills(Dummy, [english, testskill]),
			call_queue:add(Pid, 1, Dummy),
			register(media_dummy, Dummy),
			register(testqueue, Pid),
			{Pid, Dummy}
		end,
		fun({Pid, Dummy}) ->
			unregister(media_dummy),
			exit(Dummy, normal),
			?CONSOLE("Das pid:  ~p", [Pid]),
			call_queue:stop(Pid),
			queue_manager:stop()
			%try call_queue:stop(Pid)
			%catch
				%What1:Why1 ->
					%?CONSOLE("Cleanup of call_queue caught ~p:~p", [What1, Why1])
			%end,
			%case whereis(queue_manager) of
				%undefined ->
					%?CONSOLE("queue_manager already dead.", []);
				%_Else ->
					%try queue_manager:stop()
					%catch
						%What2:Why2 ->
							%?CONSOLE("Cleanup of queue_manager caught ~p:~p", [What2, Why2])
					%end
			%end
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
				"Set priority of non-existant call by pid", fun() ->
					Pid = whereis(testqueue),
					?assertMatch(none, set_priority(Pid, self(), 5))
				end
			}, {
				"increase priority", fun() ->
					Pid = whereis(testqueue),
					Dummy = whereis(media_dummy),
					remove(Pid, Dummy),
					{ok, Dummy1} = dummy_media:start([{id, "C1"}, {queues, none}]),
					{ok, Dummy2} = dummy_media:start([{id, "C2"}, {queues, none}]),
					{ok, Dummy3} = dummy_media:start([{id, "C3"}, {queues, none}]),
					add(Pid, 1, Dummy1),
					add(Pid, 1, Dummy2),
					add(Pid, 1, Dummy3),
					{{1,_}, Call1} = ask(Pid),
					?assertEqual("C1", Call1#queued_call.id),
					set_priority(Pid, Dummy2, 0),
					{{0,_}, Call2} = ask(Pid),
					?assertEqual("C2", Call2#queued_call.id)
				end
			}, {
				"decrease priority", fun() ->
					Pid = whereis(testqueue),
					Dummy = whereis(media_dummy),
					remove(Pid, Dummy),
					{ok, Dummy1} = dummy_media:start([{id, "C1"}, {queues, none}]),
					{ok, Dummy2} = dummy_media:start([{id, "C2"}, {queues, none}]),
					{ok, Dummy3} = dummy_media:start([{id, "C3"}, {queues, none}]),
					add(Pid, 1, Dummy1),
					add(Pid, 1, Dummy2),
					add(Pid, 1, Dummy3),
					{{1,_}, Call1} = ask(Pid),
					?assertEqual("C1", Call1#queued_call.id),
					set_priority(Pid, Dummy1, 2),
					{{1,_}, Call2} = ask(Pid),
					?assertEqual("C2", Call2#queued_call.id)
				end
			}, {
				"Skill integrity test", fun() ->
					Pid = whereis(testqueue),
					{ok, Dummy1} = dummy_media:start([{id, "C1"}, {skills, [foo, bar, '_node']}, {queues, none}]),
					%dummy_media:set_skills(Dummy1, [foo, bar, '_node']),
					add(Pid, Dummy1),
					{_Key, Call} = get_call(Pid, Dummy1),
					?assertEqual(true, lists:member(foo, Call#queued_call.skills)),
					?assertEqual(false, lists:member(baz, Call#queued_call.skills)),
					?assertEqual(4, length(Call#queued_call.skills))
				end
			}, {
				"Add skills test", fun() ->
					Pid = whereis(testqueue),
					{ok, Dummy1} = dummy_media:start([{id, "C1"}, {queues, none}]),
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
					{ok, Dummy1} = dummy_media:start([{id, "C1"}, {queues, none}]),
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
					{ok, Dummy1} = dummy_media:start([{id, "C1"}, {queues, none}]),
					?assertEqual(ok, add(Pid, Dummy1)),
					?assertEqual(none, remove_skills(Pid, "C2", [english]))
				end
			}, {
				"Remove skills from call referenced by pid", fun() ->
					Pid = whereis(testqueue),
					{ok, Dummy1} = dummy_media:start([{id, "C1"}, {queues, none}]),
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
					{ok, Dummy1} = dummy_media:start([{id, "C1"}, {queues, none}]),
					?assertEqual(ok, add(Pid, Dummy1)),
					{_Key, Call} = get_call(Pid, "C1"),
					?assertEqual(false, lists:member(testq, Call#queued_call.skills)),
					?assertEqual(ok, add_skills(Pid, "C1", ['_queue'])),
					{_Key, Call2} = get_call(Pid, "C1"),
					?assertEqual(true, lists:member({'_queue', "testqueue"}, Call2#queued_call.skills))
				end
			}, {
				"Test _brand skill expansion", fun() ->
					Pid = whereis(testqueue),
					{ok, Dummy1} = dummy_media:start([{id, "C1"}, {skills, ['_brand']}, {client, #client{id="bid", label="Test Brand"}}, {queues, none}]),
					%dummy_media:set_skills(Dummy1, ['_brand']),
					%dummy_media:set_brand(Dummy1, #client{label="Test Brand"}),
					?assertEqual(ok, add(Pid, Dummy1)),
					{_Key, Call2} = get_call(Pid, "C1"),
					?DEBUG("de skillz:  ~p", [Call2#queued_call.skills]),
					%% gen_media will swallow up clients it can't confirm are real.
					?assertEqual(true, lists:member({'_brand', undefined}, Call2#queued_call.skills))
				end
			}, {
				"_brand skill should expand to 'undefined' if the call doesn't have a brand tagged", fun() ->
					Pid = whereis(testqueue),
					{ok, Dummy1} = dummy_media:start([{id, "C1"}, {skills, ['_brand']}, {queues, none}]),
					%dummy_media:set_skills(Dummy1, ['_brand']),
					?assertEqual(ok, add(Pid, Dummy1)),
					{_Key, Call2} = get_call(Pid, "C1"),
					?DEBUG("skills:  ~p", [Call2#queued_call.skills]),
					?assertEqual(true, lists:member({'_brand', undefined}, Call2#queued_call.skills))
				end
			}, {
				"Remove magic skills test", fun() ->
					Pid = whereis(testqueue),
					{ok, Dummy1} = dummy_media:start([{id, "C1"}, {skills, ['_node']}, {queues, none}]),
					%dummy_media:set_skills(Dummy1, ['_node']),
					?assertEqual(ok, add(Pid, Dummy1)),
					{_Key, Call} = get_call(Pid, "C1"),
					?assertEqual(true, lists:member({'_node', node()}, Call#queued_call.skills)),
					?assertEqual(ok, remove_skills(Pid, "C1", ['_node'])),
					{_Key, Call2} = get_call(Pid, "C1"),
					?assertEqual(false, lists:member({'_node', node()}, Call2#queued_call.skills))
				end
			}, {
				"Ensure that call_skills are merged into the call's skill list on add", fun() ->
					Pid = whereis(testqueue),
					{ok, Dummy1} = dummy_media:start([{id, "C1"}, {skills, [madness]}, {queues, none}]),
					%dummy_media:set_skills(Dummy1, [madness]),
					?assertEqual(ok, add(Pid, Dummy1)),
					{_Key, Call} = get_call(Pid, "C1"),
					?assertEqual(true, lists:member({'_node', node()}, Call#queued_call.skills)),
					?assertEqual(true, lists:member(english, Call#queued_call.skills))
				end
			}, {
				"The Media dies during cook start", fun() ->
					?CONSOLE("Media dies during cook start test begins", []),
					Pid = whereis(testqueue),
					{ok, Dummy1} = dummy_media:start([{id, "C1"}, {queues, none}]),
					add(Pid, Dummy1),
					dummy_media:stop(Dummy1, testkill),
					timer:sleep(10),
					?assertMatch(none, get_call(Pid, "C1"))
				end
			}, {
				"Media dies", fun() ->
					?CONSOLE("Media dies", []),
					Pid = whereis(testqueue),
					{ok, Dummy1} = dummy_media:start([{id, "C1"}, {queues, none}]),
					add(Pid, Dummy1),
					timer:sleep(100),
					dummy_media:stop(Dummy1, testkill),
					?assertMatch(none, get_call(Pid, "C1"))
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
			{ok, Pid} = queue_manager:add_queue("testqueue", []),
			{ok, Dummy} = dummy_media:start([{id, "testcall"}, {skills, [english, testskill]}, {queues, none}]),
			%dummy_media:set_skills(Dummy, [english, testskill]),
			call_queue:add(Pid, 1, Dummy),
			register(media_dummy, Dummy),
			register(testqueue, Pid),
			{Pid, Dummy}
		end,
		fun({Pid, Dummy}) ->
			unregister(media_dummy),
			exit(Dummy, normal),
			?CONSOLE("Das pid:  ~p", [Pid]),
			call_queue:stop(Pid),
			queue_manager:stop()
			%try call_queue:stop(Pid)
			%catch
				%What1:Why1 ->
					%?CONSOLE("Cleanup of call_queue caught ~p:~p", [What1, Why1])
			%end,
			%case whereis(queue_manager) of
				%undefined ->
					%?CONSOLE("queue_manager already dead.", []);
				%_Else ->
					%try queue_manager:stop()
					%catch
						%What2:Why2 ->
							%?CONSOLE("Cleanup of queue_manager caught ~p:~p", [What2, Why2])
					%end
			%end
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
					{ok, Dummy2} = dummy_media:start([{id, "C2"}, {queues, none}]),
					{ok, Dummy3} = dummy_media:start([{id, "C3"}, {queues, none}]),
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
					#state{recipe = ?DEFAULT_RECIPE} = dump(Pid),
					NewRecipe = [{[{ticks, 3}], set_priority, 5, run_many}],
					?assertEqual(ok, set_recipe(Pid, NewRecipe)),
					?assertMatch(#state{recipe = NewRecipe}, dump(Pid))
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
				{ok, Pid} = queue_manager:add_queue("testqueue", []),
				{ok, Dummy} = dummy_media:start([{id, "testcall"}, {skills, [english, testskill]}, {queues, none}]),
				%dummy_media:set_skills(Dummy, [english, testskill]),
				call_queue:add(Pid, 1, Dummy),
				register(media_dummy, Dummy),
				{Pid, Dummy}
			end,
			fun({Pid, Dummy}) ->
				unregister(media_dummy),
				exit(Dummy, normal),
				?CONSOLE("Das pid:  ~p", [Pid]),
				call_queue:stop(Pid),
				queue_manager:stop()
				%try call_queue:stop(Pid)
				%catch
					%What1:Why1 ->
						%?CONSOLE("Cleanup of call_queue caught ~p:~p", [What1, Why1])
				%end,
				%case whereis(queue_manager) of
					%undefined ->
						%?CONSOLE("queue_manager already dead.", []);
					%_Else ->
						%try queue_manager:stop()
						%catch
							%What2:Why2 ->
								%?CONSOLE("Cleanup of queue_manager caught ~p:~p", [What2, Why2])
						%end
				%end
			end,
			[
				{
					"Slaughter the cook",
					fun() ->
						{exists, Pid} = queue_manager:add_queue("testqueue", []),
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


get_nodes() ->
	[_Name, Host] = string:tokens(atom_to_list(node()), "@"),
	{list_to_atom(lists:append("master@", Host)), list_to_atom(lists:append("slave@", Host))}.

% TODO disabled until such time that either:
% a) rewrtten to not require actual nodes to be running or 
% b) rebar will run eunit tests on an actual node.
multi_node_test_d() ->
	%["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
	{Master, Slave} = get_nodes(),
	{
		foreach,
		fun() ->
			mnesia:stop(),
			slave:start(net_adm:localhost(), master, " -pa debug_ebin"),
			slave:start(net_adm:localhost(), slave, " -pa debug_ebin"),

			mnesia:change_config(extra_db_nodes, [Master, Slave]),
			mnesia:delete_schema([node(), Master, Slave]),
			mnesia:create_schema([node(), Master, Slave]),

			cover:start([Master, Slave]),

			rpc:call(Master, mnesia, start, []),
			rpc:call(Slave, mnesia, start, []),
			mnesia:start(),

			mnesia:change_table_copy_type(schema, Master, disc_copies),
			mnesia:change_table_copy_type(schema, Slave, disc_copies),

			{ok, _Pid} = rpc:call(Master, queue_manager, start, [[Master, Slave]]),
			{ok, _Pid2} = rpc:call(Slave, queue_manager, start, [[Master, Slave]]),

			{ok, Pid} = rpc:call(Slave, queue_manager, add_queue, ["testqueue", []]),
			Pid
		end,
		fun(Pid) ->

			rpc:call(Slave, call_queue, stop, [Pid]),
			cover:stop([Master, Slave]),

			%rpc:call(Master, mnesia, stop, []),
			%rpc:call(Slave, mnesia, stop, []),
			%rpc:call(Master, mnesia, delete_schema, [[Master]]),
			%rpc:call(Slave, mnesia, delete_schema, [[Slave]]),

			slave:stop(Master),
			slave:stop(Slave),
			mnesia:stop(),
			mnesia:delete_schema([node()]),

			ok
		end,
		[
			{ "multi node grab test", fun() ->
					% only one dispatcher per node is allowed to bind, thus we'll
					% be faking a dispatcher.
					timer:sleep(10),
					Queue = rpc:call(Slave, queue_manager, get_queue, ["testqueue"]),
					?DEBUG("queue: ~p", [Queue]),
					
					% ensure an empty queue says that it is indeed empty.
					?assertEqual(none, rpc:call(Master, call_queue, grab, [Queue])),
					?assertEqual(none, rpc:call(Slave, call_queue, grab, [Queue])),
					
					% so adding the call to teh queue on one node gets the same
					% results no matter where the ask came from.
					{ok, Dummy} = rpc:call(node(Queue), dummy_media, start, [[{id, "testcall"}, {skills, [english, testskill]}, {queues, none}]]),
					rpc:call(Master, call_queue, add, [Queue, 1, Dummy]),
					{_Key, Callrec} = rpc:call(Master, call_queue, ask, [Queue]),
					?assertEqual("testcall", Callrec#queued_call.id),
					{_Key, Callrec2} = rpc:call(Slave, call_queue, ask, [Queue]),
					?assertEqual("testcall", Callrec2#queued_call.id),					
					rpc:call(Master, gen_leader_mock, start, [agent_manager]),
					rpc:call(Slave, gen_leader_mock, start, [agent_manager]),
					ListAgents = fun(route_list_agents, _From, State, _Elec) ->
						{ok, {0, []}, State}
					end,
					rpc:call(Master, gen_leader_mock, expect_call, [agent_manager, ListAgents]),
					rpc:call(Slave, gen_leader_mock, expect_call, [agent_manager, ListAgents]),
					rpc:call(Master, gen_leader_mock, expect_call, [agent_manager, ListAgents]),
					rpc:call(Slave, gen_leader_mock, expect_call, [agent_manager, ListAgents]),
					% {K, {V, _Id, Timeavail, AgSkills}} <- gen_leader:call(?MODULE, list_agents), 
					Faked1 = spawn(Slave, ?MODULE, fake_dispatcher, []),
					Faked1 ! {grabit, Queue, self()},
					receive
						none ->
							?assert(none);
						Else ->
							?assert(true)
					after 10 ->
						?assert("faked1 timeout")
					end,
					Faked1 ! {grabit, Queue, self()},
					receive
						none ->
							?assert(true);
						Else2 ->
							?assert(Else2)
					after 10 ->
						?assert("faked1 try 2 timeout")
					end,
					Faked2 = spawn(Master, ?MODULE, fake_dispatcher, []),
					Faked2 ! {grabit, Queue, self()},
					receive
						none ->
							?assert(none);
						Else3 ->
							?assert(true)
					after 10 ->
						?assert("faked2 timeout")
					end,
					Faked2 ! {grabit, Queue, self()},
					receive
						none ->
							?assert(true);
						Else4 ->
							?assert(Else4)
					after 10 ->
						?assert("faked2 try 2 timeout")
					end
				end
			}, { "ensure cook is started on same node as call", fun() ->
					timer:sleep(10),
					Queue = rpc:call(Slave, queue_manager, get_queue, ["testqueue"]),
					{ok, Dummy} = rpc:call(Master, dummy_media, start, [[{id, "testcall"}, {queues, none}]]),
					rpc:call(Master, call_queue, add, [Queue, 1, Dummy]),
					receive after 300 -> ok end,
					{_Key, #queued_call{cook = Cook}} = rpc:call(Slave, call_queue, ask, [Queue]),
					?assertEqual(Master, node(Cook))
				end
			}, { "a respawned cook should be on the same node as its call", fun() ->
					timer:sleep(10),
					Queue = rpc:call(Slave, queue_manager, get_queue, ["testqueue"]),
					{ok, Dummy} = rpc:call(Master, dummy_media, start, [[{id, "testcall"}, {queues, none}]]),
					rpc:call(Slave, call_queue, add, [Queue, 1, Dummy]),
					receive after 300 -> ok end,
					{_Key, #queued_call{cook = Cook1}} = rpc:call(Slave, call_queue, ask, [Queue]),
					?assertEqual(Master, node(Cook1)),
					exit(Cook1, kill),
					receive after 300 -> ok end,
					{_Key, #queued_call{cook = Cook2}} = rpc:call(Slave, call_queue, ask, [Queue]),
					?assertEqual(Master, node(Cook2)),
					?assertNot(Cook1 =:= Cook2)
				end
			}
		]
	}.

fake_dispatcher() ->
	receive
		{grabit, Q, From} ->
			Out = rpc:call(node(), call_queue, grab, [Q]),
			From ! Out,
			fake_dispatcher();
		_ ->
			fake_dispatcher()
	end.
	
-define(MYSERVERFUNC, fun() -> {ok, Pid} = start("testq", []), {Pid, fun() -> stop(Pid) end} end).

-include("gen_server_test.hrl").

-endif.
