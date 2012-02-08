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

%% @doc Centralized authority on where agent_fsm's are and starting them.
%% Similar in function to the {@link queue_manager}, just oriented towards
%% agents.  There can be only one `agent_manager' per node.

-module(agent_manager).
-author(micahw).
-behaviour(gen_leader).

-ifdef(TEST).
-define(STANDARD_TEST, true).
-include_lib("eunit/include/eunit.hrl").
-endif.
-ifdef(PROFILE).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").

-type(agent_pid() :: pid()).
-type(agent_id() :: string()).
-type(time_avail() :: integer()).
-type(channels() :: [channel_category()]).
-type(endpoints() :: [atom()]). % list of media modules.
-type(skills() :: skill_list()).
-type(agent_cache() :: {agent_pid(), agent_id(), time_avail(), skills(),
	channels(), endpoints()}).

%-type(rotations() :: non_neg_integer()).
%-type(all_skill_flag() :: 'a' | 'z'). % a < z, a meaning it has the all flag,
% z lacking.  So, the gb tree will sort the a's first.
%-type(skill_list_length() :: non_neg_integer()).
%-type(went_avail_at() :: {pos_integer(), non_neg_integer(), non_neg_integer()}).
%-type(sort_key() :: {rotations(), all_skill_flag(), skill_list_length(), went_avail_at()}).

-record(state, {
	agents = dict:new() :: dict(),
	route_list = gb_trees:empty() :: any(),
	lists_requested = 0 :: integer()
}).

-type(state() :: #state{}).
-define(GEN_LEADER, true).
-include("gen_spec.hrl").

-define(has_all(Skills), case lists:member('_all', Skills) of true -> a; false -> z end).

% API exports
-export([
	start_link/1, 
	start_link/2,
	start/1,
	start/2, 
	stop/0, 
	start_agent/1, 
	query_agent/1,
	update_skill_list/2,
	find_by_skill/1,
	find_avail_agents_by_skill/1,
	sort_agents_by_elegibility/1,
	filtered_route_list/1,
	filtered_route_list/3,
	find_by_pid/1,
	blab/2,
	get_leader/0,
	list/0,
	notify/5,
	set_avail/2,
	set_ends/2
]).

% gen_leader callbacks
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

%% API

%% @doc Starts the `agent_manger' linked to the calling process.
-spec(start_link/1 :: (Nodes :: [atom()]) -> {'ok', pid()}).
start_link(Nodes) -> 
	gen_leader:start_link(?MODULE, Nodes, [{heartbeat, 1}, {vardir, util:run_dir()}], ?MODULE, [], []).

-spec(start_link/2 :: (Nodes :: [atom()], Opts :: [any()]) -> {'ok', pid()}).
start_link(Nodes, Opts) ->
	gen_leader:start_link(?MODULE, Nodes, [{heartbeat, 1}, {vardir, util:run_dir()}], ?MODULE, Opts, []).

%% @doc Starts the `agent_manager' without linking to the calling process.
-spec(start/1 :: (Nodes :: [atom()]) -> {'ok', pid()}).
start(Nodes) -> 
	gen_leader:start(?MODULE, Nodes, [{heartbeat, 1}, {vardir, util:run_dir()}], ?MODULE, [], []).

-spec(start/2 :: (Nodes :: [atom()], Opts :: [any()]) -> {'ok', pid()}).
start(Nodes, Opts) ->
	gen_leader:start(?MODULE, Nodes, [{heartbeat, 1}, {vardir, util:run_dir()}], ?MODULE, Opts, []).

%% @doc stops the `agent_manager'.
-spec(stop/0 :: () -> {'ok', pid()}).
stop() -> 
	gen_leader:call(?MODULE, stop).
	
%% @doc starts a new agent_fsm for `Agent'. Returns `{ok, pid()}', where `Pid' is the new agent_fsm pid.
-spec(start_agent/1 :: (Agent :: #agent{}) -> {'ok', pid()} | {'exists', pid()}).
start_agent(#agent{login = ALogin} = Agent) -> 
	case query_agent(ALogin) of
		false -> 
			gen_leader:call(?MODULE, {start_agent, Agent}, infinity);
		{true, Apid} -> 
			{exists, Apid}
	end.

%% @doc updates the skill-list cached here.  'Tis a case to prevent blockage.
-spec(update_skill_list/2 :: (Login :: string(), Skills :: skills()) -> 'ok').
update_skill_list(Login, Skills) ->
	gen_leader:cast(?MODULE, {update_skill_list, Login, Skills}).

%% @doc Locally find all available agents with a particular skillset that contains the subset `Skills'.
-spec(find_avail_agents_by_skill/1 :: (Skills :: [atom()]) -> [{string(), pid(), #agent{}}]).
find_avail_agents_by_skill(Skills) -> 
	%?DEBUG("skills passed:  ~p.", [Skills]),
	List = list_avail(),
	filter_avail_agents_by_skill(List, Skills).

%% @doc Locally find all available agents with a particular skillset, and
%% makes sure the server knows it's for routing.  Depricated in favor of
%% {@link filtered_route_list/3}
-spec(filtered_route_list/1 :: (Skills :: [atom()]) -> 
		{integer(), [{string(), pid(), integer(), [atom()], integer()}]}).
filtered_route_list(Skills) ->
	Agents = route_list(),
	filter_avail_agents_by_skill(Agents, Skills).

%% @doc Locally find all available agents with a particular skillset, 
%% media channel, and media callback module.  Also makes sure the server 
%% knows it's for routing.
-spec(filtered_route_list/3 :: (
	Skills :: [atom()],
	Chan :: atom(),
	Endpoint :: atom()) -> 
		{integer(), [{string(), pid(), integer(), [atom()], integer()}]}).
filtered_route_list(Skills, Chan, Endpoint) ->
	Agents = route_list(),
	[ O ||
		{_K, #agent_cache{skills = AgSkills, channels = AgChans, endpoints = AgEndpoints}} = O <- Agents,
		length(AgChans) > 0,
		(lists:member('_all', AgSkills) orelse lists:member('_all', Skills) )
		orelse
		util:list_contains_all(AgSkills, Skills),
		lists:member(Chan, AgChans),
		lists:member(Endpoint, AgEndpoints)
	].

%% @doc Filter the agents based on the skill list and availability.
-spec(filter_avail_agents_by_skill/2 :: (Agents :: [any()], Skills :: [atom()]) -> [any()]).
filter_avail_agents_by_skill(Agents, Skills) ->
	AvailSkilledAgents = [O || 
		{_K, #agent_cache{skills = AgSkills} = AgCache} = O <- Agents,
		length(AgCache#agent_cache.channels) > 0,
		( % check if either the call or the agent has the _all skill
			lists:member('_all', AgSkills) orelse
			lists:member('_all', Skills)
			% if there's no _all skill, make sure the agent has all the required skills
		) orelse util:list_contains_all(AgSkills, Skills)],
	AvailSkilledAgents.

%% @doc Sorted by idle time, then the length of the list of skills the agent has;  this means idle time is less important.
%% No un-idle agents should be in the list, otherwise it is fail.
-spec(sort_agents_by_elegibility/1 :: (Agents :: [agent_cache()]) -> [agent_cache()]).
sort_agents_by_elegibility(AvailSkilledAgents) ->
	lists:keysort(1, AvailSkilledAgents).

%help_sort(E1, E2) when size(E1) == 4, size(E2) == 4 ->
%	help_sort(erlang:append_element(E1, ignored), erlang:append_element(E2, ignored));
%help_sort({_K1, _V1, Time1, Skills1, _}, {_K2, _V2, Time2, Skills2, _}) ->
%	case {lists:member('_all', Skills1), lists:member('_all', Skills2)} of
%		{true, false} ->
%			false;
%		{false, true} ->
%			true;
%		_Else ->
%			if
%				length(Skills1) == length(Skills2) ->
%					Time1 =< Time2;
%				true ->
%					length(Skills1) =< length(Skills2)
%			end
%	end.

%% @doc Gets all the agents have have the given `[atom()] Skills'.
% TODO This is wrong, should use agent cache
-spec(find_by_skill/1 :: (Skills :: [atom()]) -> [#agent{}]).
find_by_skill(Skills) ->
	[{K, V, Timeavail, AgSkills} || 
		{K, {V, _Id, Timeavail, AgSkills, _Chan, _Ends}} <- gen_leader:call(?MODULE, list_agents), 
		lists:member('_all', AgSkills) orelse util:list_contains_all(AgSkills, Skills)].

%% @doc Gets the login associated with the passed pid().
-spec(find_by_pid/1 :: (Apid :: pid()) -> string() | 'notfound').
find_by_pid(Apid) ->
	gen_leader:leader_call(?MODULE, {get_login, Apid}).

%% @doc Get a list of agents at the node this `agent_manager' is running on.
-spec(list/0 :: () -> [any()]).
list() ->
	gen_leader:call(?MODULE, list_agents).

%% @doc List the available agents on this node.
-spec(list_avail/0 :: () -> [any()]).
list_avail() ->
	List = gen_leader:call(?MODULE, list_avail_agents),
	[X || {_, #agent_cache{channels = Chans}} = X <- List, length(Chans) > 0].
	
%% @doc Get a list of agents, tagged for how many requests of this type
%% have been made without the agent list changing in any way.  A change is
%% either an agent added, dropped, or skill list change.
-spec(route_list/0 :: () -> {integer(), [any()]}).
route_list() ->
	gen_leader:call(?MODULE, route_list_agents).

%% @doc Check if an agent idetified by agent record or login name string of `Login' exists
-spec(query_agent/1 ::	(Agent :: #agent{}) -> {'true', pid()} | 'false';
						(Login :: string()) -> {'true', pid()} | 'false').
query_agent(#agent{login=Login}) -> 
	query_agent(Login);
query_agent(Login) -> 
	gen_leader:leader_call(?MODULE, {exists, Login}).

%% @doc Notify the agent_manager of a local agent after an agent manager restart.
%% TODO update to take channels and endpoints too
-spec(notify/5 :: (Login :: string(), Id :: string(), Pid :: pid(), TimeAvail :: non_neg_integer(), Skills :: [atom()]) -> 'ok').
notify(Login, Id, Pid, TimeAvail, Skills) ->
	case gen_leader:call(?MODULE, {exists, Login}) of
		false ->
			gen_leader:call(?MODULE, {notify, Login, Id, Pid, TimeAvail, Skills});
		{true, Pid} ->
			ok;
		{true, _OtherPid} ->
			erlang:error({duplicate_registration, Login})
	end.

%% @doc Returns `{ok, pid()}' where `pid()' is the pid of the leader process.
-spec(get_leader/0 :: () -> {'ok', pid()}).
get_leader() -> 
	gen_leader:leader_call(?MODULE, get_pid).

%% @doc Send the message `string() Text' to all agents that match the given filter.
-spec(blab/2 :: (Text :: string(), {Key :: 'agent' | 'node' | 'profile', Value :: string() | atom()}) -> 'ok').
blab(Text, all) ->
	gen_leader:leader_cast(?MODULE, {blab, Text, all});
blab(Text, {Key, Value}) ->
	gen_leader:leader_cast(?MODULE, {blab, Text, {Key, Value}}).

-spec(set_avail/2 :: (AgentLogin :: string(), Channels :: [atom()]) -> 'ok').
set_avail(AgentLogin, Channels) ->
	gen_leader:cast(?MODULE, {set_avail, AgentLogin, Channels}).

-spec(set_ends/2 :: (AgentLogin :: string(), Endpoints :: [atom()]) -> 'ok').
set_ends(AgentLogin, Endpoints) ->
	gen_leader:cast(?MODULE, {set_ends, AgentLogin, Endpoints}).

%% =================================================================
%% gen_leader callbacks
%% =================================================================

%% @hidden
init(_Opts) ->
	?DEBUG("~p starting at ~p", [?MODULE, node()]),
	process_flag(trap_exit, true),
	try build_tables() of
		Whatever ->
			?DEBUG("building agent_state table:  ~p", [Whatever]),
			ok
	catch
		What:Why ->
			?WARNING("What:  ~w; Why:  ~p", [What, Why]),
			ok
	end,
	{ok, #state{}}.
	
%% @hidden
elected(State, _Election, Node) -> 
	?INFO("elected by ~w", [Node]),
	mnesia:subscribe(system),
	{ok, ok, State}.
	
%% @hidden
%% if an agent is started at two locations, the 2nd notify will be told that
%% the register failed, allowing the agent to hagurk at that point.
surrendered(#state{agents = Agents} = State, _LeaderState, _Election) -> 
	?INFO("surrendered", []),
	mnesia:unsubscribe(system),

	% clean out non-local pids
	F = fun(_Login, #agent_cache{pid = Apid}) -> 
		node() =:= node(Apid)
	end,
	Locals = dict:filter(F, Agents),
	% and tell the leader about local pids
	Notify = fun({Login, V}) -> 
		gen_leader:leader_cast(?MODULE, {update_notify, Login, V})
	end,
	Dictlist= dict:to_list(Locals),
	lists:foreach(Notify, Dictlist),
	RoutlistFilter = fun({_Key, #agent_cache{pid = Pid}}) ->
		node() =:= node(Pid)
	end,
	Routelist = gb_trees_filter(RoutlistFilter, State#state.route_list),
	{ok, State#state{agents=Locals, route_list = Routelist}}.
	
%% @hidden
handle_DOWN(Node, #state{agents = Agents} = State, _Election) -> 
	?INFO("Node ~p died", [Node]),
	% clean out the pids associated w/ the dead node
	F = fun(_Login, #agent_cache{pid = Apid}) -> 
		Node =/= node(Apid)
	end,
	Agents2 = dict:filter(F, Agents),
	Routelist = gb_trees_filter(fun({_Key, #agent_cache{pid = Pid}}) ->
		Node =/= node(Pid)
	end, State#state.route_list),
	{ok, State#state{agents = Agents2, route_list = Routelist}}.

%% @hidden
handle_leader_call({exists, Agent}, From, #state{agents = _Agents} = State, Election) when is_list(Agent) -> 
	?DEBUG("Trying to determine if ~p exists", [Agent]),
	case handle_leader_call({full_data, Agent}, From, State, Election) of
		{reply, false, _State} = O ->
			O;
		{reply, #agent_cache{pid = Apid}, NewState} ->
			{reply, {true, Apid}, NewState}
	end;
handle_leader_call({full_data, Agent}, _From, #state{agents = Agents} = State, _Election) when is_list(Agent) ->
	case dict:find(Agent, Agents) of
		error ->
			%% TODO - a nasty hack for when the agent has a @ sign in their username
			case dict:find(re:replace(Agent, "_", "@", [{return, list}]), Agents) of
				error ->
					{reply, false, State};
				{ok, Value} ->
					{reply, Value, State}
			end;
		{ok, Value} -> 
			{reply, Value, State}
	end;			
handle_leader_call(get_pid, _From, State, _Election) ->
	{reply, {ok, self()}, State};
handle_leader_call({get_login, Apid}, _From, #state{agents = Agents} = State, _Election) when is_pid(Apid) ->
	List = dict:to_list(Agents),
	Out = find_via_pid(Apid, List),
	{reply, Out, State};
handle_leader_call({get_login, Id}, _From, #state{agents = Agents} = State, _Election) ->
	List = dict:to_list(Agents),
	Out = find_via_id(Id, List),
	{reply, Out, State};
handle_leader_call(Message, From, State, _Election) ->
	?WARNING("received unexpected leader_call ~p from ~p", [Message, From]),
	{reply, ok, State}.

%% @hidden
handle_leader_cast({notify, Agent, AgentCache}, #state{agents = Agents} = State, _Election) -> 
	?INFO("Notified of ~p (~p)", [Agent, AgentCache]),
	Apid = AgentCache#agent_cache.pid,
	case dict:find(Agent, Agents) of
		error -> 
			?DEBUG("Adding agent ~p", [Agent]),
			Agents2 = dict:store(Agent, AgentCache, Agents),
			Routelist = gb_trees:enter(#agent_key{
				rotations = 0,
				has_all = ?has_all(AgentCache#agent_cache.skills),
				skill_count = length(AgentCache#agent_cache.skills),
				idle_time = AgentCache#agent_cache.time_avail},
			AgentCache, State#state.route_list),
			{noreply, State#state{agents = Agents2, route_list = Routelist}};
		{ok, #agent_cache{pid = Apid}} ->
			?DEBUG("Agent ~p already know", [Agent]),
			{noreply, State};
		_Else -> 
			?DEBUG("agent pid mismatch, telling imposter", []),
			agent:register_rejected(AgentCache#agent_cache.pid),
			{noreply, State}
	end;
handle_leader_cast({update_notify, Login, #agent_cache{pid = Pid} = Value}, #state{agents = Agents} = State, _Election) ->
	NewAgents = dict:update(Login, fun(_Old) -> Value end, Value, Agents),
	Midroutelist = gb_trees_filter(fun({_Key, #agent_cache{pid = Apid}}) ->
		Apid =/= Pid
	end, State#state.route_list),
	Skills = Value#agent_cache.skills,
	Time = Value#agent_cache.time_avail,
	Routelist = gb_trees:enter(#agent_key{ rotations = 0,
		has_all = ?has_all(Skills), skill_count = length(Skills),
		idle_time = Time}, Value, Midroutelist),
	{noreply, State#state{agents = NewAgents, route_list = Routelist}};
handle_leader_cast({notify_down, Agent}, #state{agents = Agents} = State, _Election) ->
	?NOTICE("leader notified of ~p exiting", [Agent]),
	Routelist = case dict:find(Agent, Agents) of
		error ->
			State#state.route_list;
		{ok, #agent_cache{pid = Pid}} ->
			gb_trees_filter(fun({_Key, #agent_cache{pid = Apid}}) ->
				Apid =/= Pid
			end, State#state.route_list)
	end,
	{noreply, State#state{agents = dict:erase(Agent, Agents), route_list = Routelist}};
handle_leader_cast({blab, Text, {agent, Value}}, #state{agents = Agents} = State, _Election) ->
	case dict:find(Value, Agents) of
		error ->
			% /shrug.  Meh.
			{noreply, State};
		{ok, #agent_cache{pid = Apid}} ->
			agent:blab(Apid, Text),
			{noreply, State}
	end;
handle_leader_cast({blab, Text, {node, Value}}, #state{agents = Agents} = State, _Election) ->
	Alist = dict:to_list(Agents),
	F = fun({_Aname, #agent_cache{pid = Apid}}) -> 
		case node(Apid) of
			Value ->
				agent:blab(Apid, Text);
			_Else -> ok
		end
	end,
	spawn(fun() -> lists:foreach(F, Alist) end),
	{noreply, State};
handle_leader_cast({blab, Text, {profile, Value}}, #state{agents = Agents} = State, _Election) ->
	Alist = dict:to_list(Agents),
	Foreach = fun({_Aname, #agent_cache{pid = Apid}}) ->
		try agent:dump_state(Apid) of
			#agent{profile = Value} ->
				agent:blab(Apid, Text);
			_Else ->
				ok
		catch
			What:Why ->
				?WARNING("could not blab to ~p.  ~p:~p", [Apid, What, Why]),
				ok
		end
	end,
	F = fun() ->
		lists:foreach(Foreach, Alist)
	end,
	spawn(F),
	{noreply, State};
handle_leader_cast({blab, Text, all}, #state{agents = Agents} = State, _Election) ->
	F = fun(_, #agent_cache{pid = Pid}) ->
		agent:blab(Pid, Text)
	end,
	spawn(fun() -> dict:map(F, Agents) end),
	{noreply, State};
handle_leader_cast(dump_election, State, Election) -> 
	?DEBUG("Dumping leader election.~nSelf:  ~p~nDump:  ~p", [self(), Election]),
	{noreply, State};
handle_leader_cast(Message, State, _Election) ->
	?WARNING("received unexpected leader_cast ~p", [Message]),
	{noreply, State}.

%% @hidden
from_leader(_Msg, State, _Election) -> 
	?DEBUG("Stub from leader.", []),
	{ok, State}.

%% @hidden
handle_call(list_agents, _From, #state{agents = Agents} = State, _Election) -> 
	{reply, dict:to_list(Agents), State};
handle_call(list_avail_agents, _From, State, _Election) ->
	{reply, gb_trees:to_list(State#state.route_list), State};
handle_call(route_list_agents, _From, #state{agents = _Agents, lists_requested = Count, route_list = Routelist} = State, _Election) ->		
	List = gb_trees:to_list(Routelist),
	NewRoutelist = case gb_trees:is_empty(Routelist) of
		true ->
			Routelist;
		false ->
			{#agent_key{rotations = OldCount} = Key, Val, Midroutelist} =
				gb_trees:take_smallest(Routelist),
			gb_trees:enter(Key#agent_key{rotations = OldCount + 1}, Val, Midroutelist)
	end,
	{reply, List, State#state{lists_requested = Count + 1, route_list = NewRoutelist}};
handle_call(stop, _From, State, _Election) -> 
	{stop, normal, ok, State};
handle_call({start_agent, #agent{login = ALogin, id=Aid} = InAgent}, _From, #state{agents = Agents} = State, Election) -> 
	% This should not be called directly!  use the wrapper start_agent/1
	Agent = InAgent#agent{release_data = ?DEFAULT_RELEASE},
	?INFO("Starting new agent ~p", [Agent]),
	{ok, Apid} = agent:start(Agent, [logging, gen_leader:candidates(Election)]),
	link(Apid),
	Value = #agent_cache{
		pid = Apid,
		id = Aid,
		time_avail = os:timestamp(),
		skills = Agent#agent.skills,
		channels = Agent#agent.available_channels,
		endpoints = dict:fetch_keys(Agent#agent.endpoints)
	},
	Leader = gen_leader:leader_node(Election),
	case node() of
		Leader ->
			ok;
		_ ->
			gen_leader:leader_cast(?MODULE, {update_notify, ALogin, Value})
	end,
	Agents2 = dict:store(ALogin, Value, Agents),
	gen_server:cast(dispatch_manager, {end_avail, Apid}),
	Key = #agent_key{
		has_all = ?has_all(Agent#agent.skills),
		skill_count = length(Agent#agent.skills),
		idle_time = os:timestamp()
	},
	RouteList = gb_trees:enter(Key, Value, State#state.route_list),
	{reply, {ok, Apid}, State#state{agents = Agents2, route_list = RouteList}};
handle_call({exists, Login}, _From, #state{agents = Agents} = State, Election) ->
	Leader = gen_leader:leader_node(Election),
	case dict:find(Login, Agents) of
		error when Leader =/= node() ->
			case gen_leader:leader_call(?MODULE, {full_data, Login}) of
				false ->
					{reply, false, State};
				#agent_cache{pid = Pid} = V when node(Pid) =:= node() ->
					% leader knows about a local agent, but we didn't!
					% So we update the local dict
					Agents2 = dict:store(Login, V, Agents),
					{reply, {true, Pid}, State#state{agents = Agents2}};
				#agent_cache{pid = OtherPid} ->
					{reply, {true, OtherPid}, State}
			end;
		error -> % we're the leader
			{reply, false, State};
		{ok, #agent_cache{pid = Pid}} ->
			{reply, {true, Pid}, State}
	end;
handle_call({notify, Login, #agent_cache{pid = Pid} = AgentCache}, _From, #state{agents = Agents} = State, Election) when is_pid(Pid) andalso node(Pid) =:= node() ->
	case dict:find(Login, Agents) of
		error ->
			link(Pid),
			case gen_leader:leader_node(Election) =:= node() of
				false -> 
					gen_leader:leader_cast(?MODULE, {notify, Login, AgentCache});
				_Else ->
					ok
			end,
			Agents2 = dict:store(Login, AgentCache, Agents),
			#agent_cache{skills = Skills, time_avail = TimeAvail} = AgentCache,
			Midroutelist = gb_trees:enter(#agent_key{rotations = 0,
				has_all = ?has_all(Skills), skill_count = length(Skills),
				idle_time = TimeAvail}, AgentCache, State#state.route_list),
			{reply, ok, State#state{agents = Agents2, lists_requested = 0, route_list = clear_rotates(Midroutelist)}};
		{ok, {Pid, _Id}} ->
			{reply, ok, State};
		{ok, _OtherPid} ->
			% TODO - wait, so we get notified about an agent, we have a record of the an agent with that login already,
			% and we just say 'ok'?
			{reply, ok, State}
	end;
handle_call(Message, From, State, _Election) ->
	?WARNING("received unexpected call ~p from ~p", [Message, From]),
	{reply, ok, State}.


%% @hidden
handle_cast({set_avail, Nom, Chans}, #state{agents = Agents} = State, Election) ->
	Node = node(),
	#agent_cache{pid = Pid} = AgentCache = dict:fetch(Nom, Agents),
	Midroutelist = gb_trees_filter(fun({_Key, #agent_cache{pid = Apid}}) ->
		Apid =/= Pid
	end, State#state.route_list),
	%% If the new channel list is shorter, they likely went on call
	%% if so, we can consider the 'time they have been idle' as how long
	%% it's been since they took a call.
	% TODO This can be gamed by agents by bouncing idle and released
	Time = case length(AgentCache#agent_cache.channels) > length(Chans) of
		true -> os:timestamp();
		false -> AgentCache#agent_cache.time_avail
	end,
	#agent_cache{skills = Skills} = Out = AgentCache#agent_cache{time_avail = Time, channels = Chans},
	Key = #agent_key{
		rotations = 0,
		has_all = ?has_all(Skills),
		skill_count = length(Skills),
		idle_time = Time
	},
	Routelist = gb_trees:enter(Key, Out, Midroutelist),
	F = fun(_) ->
		case gen_leader:leader_node(Election) of
			Node ->
				ok;
			_ ->
				gen_leader:leader_cast(?MODULE, {update_notify, Nom, Out})
		end,
		Out
	end,
	NewAgents = dict:update(Nom, F, Agents),
	%?INFO("Updated key ~p to ~p", [Key, Out]),
	%?DEBUG("rl:  ~p", [Routelist]),
	{noreply, State#state{agents = NewAgents, lists_requested = 0, route_list = clear_rotates(Routelist)}};

handle_cast({set_ends, Nom, Ends}, #state{agents = Agents} = State, Election) ->
	Node = node(),
	#agent_cache{ pid = Pid, id = Id, time_avail = Time, skills = Skills,
		channels = Chans, endpoints = OldEnds} = dict:fetch(Nom, Agents),
	Midroutelist = gb_trees_filter(fun({_Key, #agent_cache{pid = Apid}}) ->
		Apid =/= Pid
	end, State#state.route_list),
	Out = #agent_cache{pid = Pid, id = Id, time_avail = Time,
		skills = Skills, channels = Chans, endpoints = Ends},
	Routelist = gb_trees:enter(#agent_key{ rotations = 0,
		has_all = ?has_all(Skills), skill_count = length(Skills),
		idle_time = Time}, Out, Midroutelist),
	F = fun(_) ->
		case gen_leader:leader_node(Election) of
			Node ->
				ok;
			_ ->
				gen_leader:leader_cast(?MODULE, {update_notify, Nom, Out})
		end,
		Out
	end,
	NewAgents = dict:update(Nom, F, Agents),
	{noreply, State#state{agents = NewAgents, lists_requested = 0, route_list = clear_rotates(Routelist)}};
	
%handle_cast({end_avail, Nom}, #state{agents = Agents} = State, Election) ->
%	Node = node(),
%	{Pid, Id, _, Skills} = dict:fetch(Nom, Agents),
%	Routelist = gb_trees_filter(fun({_, {Apid, _, _}}) ->
%		Apid =/= Pid
%	end, State#state.route_list),
%	NewAgents = dict:store(Nom, {Pid, Id, 0, Skills}, Agents),
%	case gen_leader:leader_node(Election) of
%		Node ->
%			ok;
%		_ ->
%			gen_leader:leader_cast(?MODULE, {update_notify, Nom, {Pid, Id, 0, Skills}})
%	end,
%	{noreply, State#state{agents = NewAgents, lists_requested = 0, route_list = Routelist}};
handle_cast({update_skill_list, Login, Skills}, #state{agents = Agents} = State, Election) ->
	Node = node(),
	#agent_cache{pid = Pid, time_avail = Time} = AgentCache = dict:fetch(Login, Agents),
	Out = AgentCache#agent_cache{skills = Skills},
	Midroutelist = clear_rotates(gb_trees_filter(fun({_, #agent_cache{pid = Apid}}) ->
		Apid =/= Pid
	end, State#state.route_list)),
	Routelist = gb_trees:enter(#agent_key{ rotations = 0,
		has_all = ?has_all(Skills), skill_count = length(Skills),
		idle_time = Time}, Out, Midroutelist),
	F = fun(_) ->
		case gen_leader:leader_node(Election) of
			Node ->
				ok;
			_ ->
				gen_leader:leader_cast(?MODULE, {update_notify, Login, Out})
		end,
		Out
	end,	
	NewAgents = dict:update(Login, F, Agents),
	{noreply, State#state{agents = NewAgents, lists_requested = 0, route_list = Routelist}};
handle_cast(_Request, State, _Election) -> 
	?DEBUG("Stub handle_cast", []),
	{noreply, State}.

%% @hidden
handle_info({'EXIT', Pid, Reason}, #state{agents=Agents} = State) ->
	?NOTICE("Caught exit for ~p with reason ~p", [Pid, Reason]),
	F = fun(Key, #agent_cache{ pid = Value, id = Id}) ->
		case Value =/= Pid of
			true -> true;
			false ->
				?NOTICE("notifying leader of ~p exit", [{Key, Id}]),
				cpx_monitor:drop({agent, Id}),
				gen_leader:leader_cast(?MODULE, {notify_down, Key}),
				false
		end
	end,
	Routelist = clear_rotates(gb_trees_filter(fun({_, #agent_cache{pid = Apid}}) ->
		Apid =/= Pid
	end, State#state.route_list)),
	{noreply, State#state{agents=dict:filter(F, Agents), lists_requested = 0, route_list = Routelist}};
handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
	?WARNING("mnesia down at ~w", [Node]),
	{noreply, State};
handle_info(Msg, State) ->
	?DEBUG("Stub handle_info for ~p", [Msg]),
	{noreply, State}.

%% @hidden
terminate(Reason, _State) -> 
	?NOTICE("Terminating:  ~p", [Reason]),
	ok.

%% @hidden
code_change(_OldVsn, State, _Election, _Extra) ->
	{ok, State}.

%% @private
find_via_pid(_Needle, []) ->
	notfound;
find_via_pid(Needle, [{Key, #agent_cache{pid = Needle}} | _Tail]) ->
	Key;
find_via_pid(Needle, [{_Key, _NotNeedle} | Tail]) ->
	find_via_pid(Needle, Tail).

%% @private
find_via_id(_Needle, []) ->
	notfound;
find_via_id(Needle, [{Key, #agent_cache{id = Needle}} | _Tail]) ->
	Key;
find_via_id(Needle, [_Head | Tail]) ->
	find_via_id(Needle, Tail).

%% @doc returns a new tree which contians only entrys where Fun({Key, Val}) = true.
gb_trees_filter(Fun, Tree) ->
	Itor = gb_trees:iterator(Tree),
	gb_trees_filter(Fun, gb_trees:next(Itor), Tree).

gb_trees_filter(_Fun, none, Tree) ->
	Tree;
gb_trees_filter(Fun, {Key, Val, Itor}, Tree) ->
	case Fun({Key, Val}) of
		true ->
			gb_trees_filter(Fun, gb_trees:next(Itor), Tree);
		false ->
			Newtree = gb_trees:delete_any(Key, Tree),
			gb_trees_filter(Fun, gb_trees:next(Itor), Newtree)
	end.

clear_rotates({#agent_key{rotations = 0} = Key, Val, Tree}) ->
	gb_trees:enter(Key, Val, Tree);
clear_rotates({Key, Val, Tree}) ->
	Newtree = gb_trees:enter(Key#agent_key{rotations = 0}, Val, Tree),
	clear_rotates(gb_trees:take_largest(Newtree));
clear_rotates(Tree) ->
	case gb_trees:is_empty(Tree) of
		true ->
			Tree;
		false ->
			clear_rotates(gb_trees:take_largest(Tree))
	end.
	
build_tables() ->
	agent_auth:build_tables().

-ifdef(STANDARD_TEST).

handle_call_start_test_d() ->
	util:start_testnode(),
	N = util:start_testnode(agent_manager_handle_call_start_test),
	{spawn, N, [fun() -> 
		?assertMatch({ok, _Pid}, start([node()])),
		stop(),
		slave:stop(N)
	end]}.

ds() ->
	spawn(fun() -> ok end).

filter_avail_agents_by_skill_test_() ->
	[{"one in, one out",
	fun() ->
		Agents = [{{agent_key, 0, z, 1, {100, 100, 100}}, {agent_cache, ds(), "agent", 0, [skill], [dummy], []}}],
		?assertEqual(Agents, filter_avail_agents_by_skill(Agents, [skill]))
	end},
	{"two in, one out",
	fun() ->
		[Out | _] = Agents = [
			{{agent_key, 0, z, 1, {100, 100, 100}}, {agent_cache, ds(), "agent1", 0, [skill], [voice], []}},
			{{agent_key, 0, z, 0, {100, 100, 100}}, {agent_cache, ds(), "agent2", 0, [], [voice], []}}
		],
		?assertEqual([Out], filter_avail_agents_by_skill(Agents, [skill]))
	end},
	{"agent with all gets in",
	fun() ->
		Agents = [{{agent_key, 0, z, 1, {100, 100, 100}}, {agent_cache, ds(), "agent", 0, ['_all'], [dummy], []}}],
		?assertEqual(Agents, filter_avail_agents_by_skill(Agents, [skill]))
	end},
	{"agents get through when all passed in",
	fun() ->
		Agents = [
			{{agent_key, 0, z, 1, {100, 100, 100}}, {agent_cache, ds(), "agent1", 0, [skill], [dummy], []}},
			{{agent_key, 0, z, 0, {100, 100, 100}}, {agent_cache, ds(), "agent2", 0, [], [dummy], []}}
		],
		?assertEqual(Agents, filter_avail_agents_by_skill(Agents, ['_all']))
	end}].

-record(election, {
	leader = none,
	name,
	leadernode = none,
	candidate_nodes = [],
	worker_nodes = [],
	alive = [],
	down = [],
	monitored = [],
	buffered = [],
	status,
	elid,
	acks = [],
	work_down = [],
	cand_timer_int,
	cand_timer,
	pendack,
	incarn,
	nextel,
	bcast_type              %% all | one. When `all' each election event
	%% will be broadcast to all candidate nodes.
}).

handle_cast_test_() ->
	{setup,
	fun() ->
		Election = #election{
			leader = node(),
			leadernode = node()
		},
		State = #state{},
		{State, Election}
	end,
	fun(_) ->
		ok
	end,
	fun({Seedstate, Election}) ->
		[{"basic now availalble",
		fun() ->
			Agents = dict:from_list([{"agent", {agent_cache, ds(), "agent", 0, [], [], []}}]),
			State = #state{agents = Agents},
			{noreply, Newstate} = handle_cast({set_avail, "agent", [voice]}, State, Election),
			?assertNot(gb_trees:is_empty(Newstate#state.route_list)),
			?assertMatch([{{agent_key, 0, z, 0, _}, {agent_cache, _, "agent", _, [], [voice], []}}], gb_trees:to_list(Newstate#state.route_list))
		end},
		{"basic end avail",
		fun() ->
			Time = os:timestamp(),
			Pid = ds(),
			Agents = dict:from_list([{"agent", {agent_cache, Pid, "agent", Time, [], [voice], []}}]),
			Routelist = gb_trees:enter({agent_key, 0, z, 0, Time}, {agent_cache, Pid, "agent", Time, [], [voice], []}, gb_trees:empty()),
			State = Seedstate#state{agents = Agents, route_list = Routelist},
			{noreply, Newstate} = handle_cast({set_avail, "agent", []}, State, Election),
			?assertMatch([{{agent_key, 0, z, 0, _}, {agent_cache, _, "agent", _, [], [], []}}], gb_trees:to_list(Newstate#state.route_list))
		end},
		{"updatin' a skill list of an idle agent",
		fun() ->
			Pid = ds(),
			Time = os:timestamp(),
			Agents = dict:from_list([{"agent", {agent_cache, Pid, "agent", Time, [], [dummy], [dummy]}}]),
			Routelist = gb_trees:enter({agent_key, 0, z, 0, Time}, {agent_cache, Pid, "agent", Time, [], [dummy], [dummy]}, gb_trees:empty()),
			State = #state{agents = Agents, route_list = Routelist},
			{noreply, NewState} = handle_cast({update_skill_list, "agent", [skill]}, State, Election),
			?assertNot(gb_trees:is_empty(NewState#state.route_list)),
			?assertMatch([{{agent_key, 0, z, 1, _}, {agent_cache, Pid, "agent", _, [skill], [dummy], [dummy]}}], gb_trees:to_list(NewState#state.route_list))
		end}]
	end}.
	
	

%handle_cast({end_avail, Nom}, #state{agents = Agents} = State, Election) ->
%	Node = node(),
%	F = fun({Pid, Id, _Time, Skills}) ->
%	Out = {Pid, Id, 0, Skills},
%case gen_leader:leader_node(Election) of
%	Node ->
%	ok;
%_ ->
%gen_leader:leader_cast(?MODULE, {update_notify, Nom, Out})
%end,
%Out
%end,
%NewAgents = dict:update(Nom, F, Agents),
%{noreply, State#state{agents = NewAgents, lists_requested = 0}};
%handle_cast({update_skill_list, Login, Skills}, #state{agents = Agents} = State, Election) ->
%	Node = node(),
%	{Pid, Id, Time, _} = dict:fetch(Login, Agents),
%	Out = {Pid, Id, Time, Skills},
%	Midroutelist = clear_rotates(gb_trees_filter(fun({_, {Apid, _, _}}) ->
%												 Apid =/= Pid
%												 end, State#state.route_list)),
%	Routelist = gb_trees:enter({0, ?has_all(Skills), length(Skills), Time}, {Pid, Id, Skills}, Midroutelist),
%	F = fun({Pid, Id, Time, _OldSkills}) ->
%case gen_leader:leader_node(Election) of
%	Node ->
%	ok;
%_ ->
%gen_leader:leader_cast(?MODULE, {update_notify, Login, Out})
%end,
%Out
%end,	
%NewAgents = dict:update(Login, F, Agents),
%{noreply, State#state{agents = NewAgents, lists_requested = 0, route_list = Routelist}};
%handle_cast(_Request, State, _Election) -> 
%	?DEBUG("Stub handle_cast", []),
%	{noreply, State}.	
%	
%	
%	
%	
%	
%	
%	
	
external_api_test_() ->
	{setup, fun() ->
		meck:new(gen_leader)
	end,
	fun(_) ->
		meck:validate(gen_leader),
		meck:unload(gen_leader)
	end,
	fun(_) -> [

		{"start_link/1", fun() ->
			meck:expect(gen_leader, start_link, fun(?MODULE, [Node], [{heartbeat, 1}, {vardir, _RunDir}], ?MODULE, [], []) ->
				?assertEqual(node(), Node),
				{ok, util:zombie()}
			end),
			?assertMatch({ok, _Pid}, start_link([node()]))
		end},
			
		{"start_link/2", fun() ->
			meck:expect(gen_leader, start_link, fun(?MODULE, [Node], [{heartbeat, 1}, {vardir, _Rundir}], ?MODULE, opts, []) ->
				?assertEqual(node(), Node),
				{ok, util:zombie()}
			end),
			?assertMatch({ok, _Pid}, start_link([node()], opts))
		end},

		{"start/1", fun() ->
			meck:expect(gen_leader, start, fun(?MODULE, [Node], [{heartbeat, 1}, {vardir, _Rundir}], ?MODULE, [], []) ->
				?assertEqual(node(), Node),
				{ok, util:zombie()}
			end),
			?assertMatch({ok, _Pid}, start([node()]))
		end},

		{"start/2", fun() ->
			meck:expect(gen_leader, start, fun(?MODULE, [Node], [{heartbeat, 1}, {vardir, _Rundir}], ?MODULE, opts, []) ->
				?assertEqual(node(), Node),
				{ok, util:zombie()}
			end),
			?assertMatch({ok, _Pid}, start([node()], opts))
		end},

		{"stop/0", fun() ->
			meck:expect(gen_leader, call, fun(?MODULE, stop) ->
				ok
			end),
			?assertEqual(ok, stop())
		end},

		{"start_agent/1, agent exists", fun() ->
			meck:expect(gen_leader, leader_call, fun(?MODULE, {exists, "testagent"}) ->
				{true, util:zombie()}
			end),
			Agent = #agent{login = "testagent"},
			?assertMatch({exists, _Pid}, start_agent(Agent))
		end},

		{"start_agent/1, agent doesn't exist", fun() ->
			Agent = #agent{login = "testagent"},
			meck:expect(gen_leader, leader_call, fun(?MODULE, {exists, "testagent"}) ->
				false
			end),
			meck:expect(gen_leader, call, fun(?MODULE, {start_agent, InAgent}, infinity) ->
				?assertEqual(Agent, InAgent),
				{'ok', util:zombie()}
			end),
			?assertMatch({ok, _Pid}, start_agent(Agent))
		end},
			
		{"query_agent/1, given record", fun() ->
			meck:expect(gen_leader, leader_call, fun(?MODULE, {exists, "testagent"}) ->
				ok
			end),
			Agent = #agent{login = "testagent"},
			?assertEqual(ok, query_agent(Agent))
		end},

		{"query_agent/1, given login", fun() ->
			meck:expect(gen_leader, leader_call, fun(?MODULE, {exists, "testagent"}) ->
				ok
			end),
			?assertEqual(ok, query_agent("testagent"))
		end},

		{"update_skill_list/2", fun() ->
			meck:expect(gen_leader, cast, fun(?MODULE, {update_skill_list, "testagent", [english, awesome]}) ->
				ok
			end),
			?assertEqual(ok, update_skill_list("testagent", [english, awesome]))
		end},

		{"find_avail_agents_by_skill/1, 'english' skill given", fun() ->
			AgentCaches = [
				{"key1", #agent_cache{channels = [voice], skills = [], id = "skip1"}},
				{"key2", #agent_cache{channels = [], skills = [english], id = "skip2"}},
				{"key3", #agent_cache{time_avail = 1, channels = [voice], skills = [english], id = "gotit1"}},
				{"key4", #agent_cache{time_avail = 1, channels = [voice], skills = ['_all'], id = "gotit2"}}
			],
			meck:expect(gen_leader, call, fun(?MODULE, list_avail_agents) ->
				AgentCaches
			end),
			Expected = [
				{"key3", #agent_cache{time_avail = 1, channels = [voice], skills = [english], id = "gotit1"}},
				{"key4", #agent_cache{time_avail = 1, channels = [voice], skills = ['_all'], id = "gotit2"}}
			],
			?assertEqual(Expected, find_avail_agents_by_skill([english]))
		end},

		{"find_avail_agents_by_skill/1, '_all' skill given", fun() ->
			AgentCaches = [
				{"key1", #agent_cache{time_avail = 1, channels = [voice], skills = [], id = "skip1"}},
				{"key2", #agent_cache{time_avail = 1, channels = [], skills = [english], id = "skip2"}},
				{"key3", #agent_cache{time_avail = 1, channels = [voice], skills = [english], id = "gotit1"}},
				{"key4", #agent_cache{time_avail = 1, channels = [voice], skills = ['_all'], id = "gotit2"}}
			],
			meck:expect(gen_leader, call, fun(?MODULE, list_avail_agents) ->
				AgentCaches
			end),
			Expected = [
				{"key1", #agent_cache{time_avail = 1, channels = [voice], skills = [], id = "skip1"}},
				{"key3", #agent_cache{time_avail = 1, channels = [voice], skills = [english], id = "gotit1"}},
				{"key4", #agent_cache{time_avail = 1, channels = [voice], skills = ['_all'], id = "gotit2"}}
			],
			?assertEqual(Expected, find_avail_agents_by_skill(['_all']))
		end},

		{"filtered_route_list/3", fun() ->
			AgentList = [
				{"key1", #agent_cache{skills = [], channels = [], endpoints = [], id = "skip1"}},
				{"key2", #agent_cache{skills = ['_all'], channels = [], endpoints = [], id = "skip2"}},
				{"key3", #agent_cache{skills = [english], channels = [], endpoints = [], id = "skip3"}},
				{"key4", #agent_cache{skills = [], channels = [voice], endpoints = [], id = "skip4"}},
				{"key5", #agent_cache{skills = [], channels = [], endpoints = [dummy_media], id = "skip5"}},
				{"key6", #agent_cache{time_avail = 1, skills = ['_all'], channels = [voice], endpoints = [dummy_media], id = "take6"}},
				{"key7", #agent_cache{time_avail = 1, skills = [english], channels = [voice], endpoints = [dummy_media], id = "take7"}}
			],
			meck:expect(gen_leader, call, fun(?MODULE, route_list_agents) ->
				AgentList
			end),
			Expected = [
				{"key6", #agent_cache{time_avail = 1, skills = ['_all'], channels = [voice], endpoints = [dummy_media], id = "take6"}},
				{"key7", #agent_cache{time_avail = 1, skills = [english], channels = [voice], endpoints = [dummy_media], id = "take7"}}
			],
			?assertEqual(Expected, filtered_route_list([english], voice, dummy_media))
		end},

		{"find_by_pid/1", fun() ->
			Zombie = util:zombie(),
			meck:expect(gen_leader, leader_call, fun(?MODULE, {get_login, Agent}) ->
				?assertEqual(Zombie, Agent),
				"zombie_guard"
			end),
			?assertEqual("zombie_guard", find_by_pid(Zombie))
		end},

		{"blab/2, all", fun() ->
			meck:expect(gen_leader, leader_cast, fun(?MODULE, {blab, "blab", all}) ->
				ok
			end),
			?assertEqual(ok, blab("blab", all))
		end},

		{"blab/2, {key, value}", fun() ->
			meck:expect(gen_leader, leader_cast, fun(?MODULE, {blab, "blab", {node, Node}}) ->
				?assertEqual(node(), Node),
				ok
			end),
			?assertEqual(ok, blab("blab", {node, node()}))
		end},

		{"get_leader/0", fun() ->
			Zombie = util:zombie(),
			meck:expect(gen_leader, leader_call, fun(?MODULE, get_pid) ->
				{ok, Zombie}
			end),
			?assertEqual({ok, Zombie}, get_leader())
		end},

		{"list/0", fun() ->
			meck:expect(gen_leader, call, fun(?MODULE, list_agents) ->
				[]
			end),
			?assertEqual([], list())
		end},

		{"set_avail/2", fun() ->
			meck:expect(gen_leader, cast, fun(?MODULE, {set_avail, "testagent", [voice]}) ->
				ok
			end),
			?assertEqual(ok, set_avail("testagent", [voice]))
		end},

		{"set_ends/2", fun() ->
			meck:expect(gen_leader, cast, fun(?MODULE, {set_ends, "testagent", [dummy_media]}) ->
				ok
			end),
			?assertEqual(ok, set_ends("testagent", [dummy_media]))
		end}

	] end}.

internal_state_test_() -> [
	{"elected", fun() ->
		meck:new(mnesia),
		meck:expect(mnesia, subscribe, fun(system) -> ok end),
		?assertEqual({ok, ok, state}, elected(state, election, node())),
		?assertMatch([{_Pid, {mnesia, subscribe, [system]}, _}], meck:history(mnesia)),
		?assert(meck:validate(mnesia)),
		meck:unload(mnesia)
	end},

	{"surrendered", fun() ->
		meck:new(mnesia),
		meck:expect(mnesia, unsubscribe, fun(system) -> ok end),
		util:start_testnode(),
		{ok, Slave} = slave:start(net_adm:localhost(), agent_manager_surrendered),
		LocalZ = util:zombie(),
		RemoteZ = rpc:call(Slave, util, zombie, []),
		LocalA = #agent_cache{time_avail = 1, pid = LocalZ},
		RemoteA = #agent_cache{time_avail = 1, pid = RemoteZ},
		State = #state{
			agents = dict:from_list([
				{"local", LocalA},
				{"remote", RemoteA}
			]),
			route_list = gb_trees:from_orddict([
				{1, LocalA},
				{2, RemoteA}
			])
		},
		meck:new(gen_leader),
		meck:expect(gen_leader, leader_cast, fun(?MODULE, {update_notify, "local", InAgent}) ->
			?assertEqual(LocalA, InAgent)
		end),
		{ok, State0} = surrendered(State, "leaderstate", "election"),
		?assertEqual([{"local", LocalA}], dict:to_list(State0#state.agents)),
		?assertEqual([{1, LocalA}], gb_trees:to_list(State0#state.route_list)),
		?assert(meck:validate(mnesia)),
		?assert(meck:validate(gen_leader)),
		meck:unload(mnesia),
		meck:unload(gen_leader),
		slave:stop(Slave)
	end},

	{"handle_DOWN", fun() ->
		util:start_testnode(),
		{ok, Slave} = slave:start(net_adm:localhost(), agent_manager_handle_down),
		LocalZ = util:zombie(),
		RemoteZ = rpc:call(Slave, util, zombie, []),
		LocalA = #agent_cache{time_avail = 1, pid = LocalZ},
		RemoteA = #agent_cache{time_avail = 1, pid = RemoteZ},
		State = #state{
			agents = dict:from_list([
				{"local", LocalA},
				{"remote", RemoteA}
			]),
			route_list = gb_trees:from_orddict([
				{1, LocalA},
				{2, RemoteA}
			])
		},
		{ok, State0} = handle_DOWN(Slave, State, "election"),
		?assertEqual([{"local", LocalA}], dict:to_list(State0#state.agents)),
		?assertEqual([{1, LocalA}], gb_trees:to_list(State0#state.route_list)),
		slave:stop(Slave)
	end},

	{"handle_leader_call", [

		{"{exists, Agent}, agent exists", fun() ->
			State = #state{
				agents = dict:from_list([
					{"testagent", #agent_cache{pid = "pid"}}
				])
			},
			?assertEqual({reply, {true, "pid"}, State}, handle_leader_call({exists, "testagent"}, "from", State, "election"))
		end},

		{"{exists, Agent}, agent not found", fun() ->
			State = #state{ agents = dict:new() },
			?assertEqual({reply, false, State}, handle_leader_call({exists, "testagent"}, "from", State, "election"))
		end},

		{"{full_data, Agent}, agent not found", fun() ->
			State = #state{ agents = dict:new() },
			?assertEqual({reply, false, State}, handle_leader_call({full_data, "testagent"}, "from", State, "election"))
		end},

		%% the next is a hack for freeswitch.  sip registrations can't have
		%% two '@' in the name, so the sip endpoint does magic to usernames
		%% that have an @, turning it into an _.  When freesiwtch media wants
		%% to lookup the agent, it send agent_domain rather than agent@domain
		%% thus the fall back.
		{"{full_data, Agent}, agent lookup has _ instead of @", fun() ->
			State = #state{ agents = dict:from_list([
				{"testagent@example.com", "data"}
			]) },
			?assertEqual({reply, "data", State}, handle_leader_call({full_data, "testagent_example.com"}, "from", State, "election"))
		end},

		{"{full_data, Agent}, simple success", fun() ->
			State = #state{ agents = dict:from_list([ {"testagent", "data"} ]) },
			?assertEqual({reply, "data", State}, handle_leader_call({full_data, "testagent"}, "from", State, "election"))
		end},

		{"get_pid", fun() ->
			Self = self(),
			?assertEqual({reply, {ok, Self}, "state"}, handle_leader_call(get_pid, "from", "state", "election"))
		end},

		{"{get_login, Apid}, found", fun() ->
			Zombie = util:zombie(),
			Agent = #agent_cache{time_avail = 1, pid = Zombie, id = "agent"},
			State = #state{agents = dict:from_list([
				{"testagent", Agent}
			])},
			?assertEqual({reply, "testagent", State}, handle_leader_call({get_login, Zombie}, "from", State, "election"))
		end},

		{"{get_login, Apid}, not found", fun() ->
			State = #state{agents = dict:new()},
			?assertEqual({reply, notfound, State}, handle_leader_call({get_login, self()}, "from", State, "election"))
		end},

		{"{get_login, Id}, found", fun() ->
			Agent = #agent_cache{time_avail = 1, pid = self(), id = "agent"},
			State = #state{agents = dict:from_list([
				{"testagent", Agent}
			])},
			?assertEqual({reply, "testagent", State}, handle_leader_call({get_login, "agent"}, "from", State, "election"))
		end},

		{"{get_login, Id}, not found", fun() ->
			State = #state{agents = dict:new()},
			?assertEqual({reply, notfound, State}, handle_leader_call({get_login, "agent"}, "from", State, "election"))
		end}

	]},

	{"handle_leader_cast", [
		{"{notify, Agent, AgentCache}, simple success", fun() ->
			State = #state{agents = dict:new(), route_list = gb_trees:empty()},
			AgentCache = #agent_cache{pid = self(), id = "agentId",
				time_avail = 1, skills = [], channels = [], endpoints = []},
			AgentKey = #agent_key{rotations = 0, has_all = z, skill_count = 0,
				idle_time = 1},
			ExpectedState = #state{
				agents = dict:from_list([
					{"testagent", AgentCache}
				]),
				route_list = gb_trees:from_orddict([
					{AgentKey, AgentCache}
				])
			},
			?assertEqual({noreply, ExpectedState}, handle_leader_cast({notify, "testagent", AgentCache}, State, "election"))
		end},

		{"{notify, Agent, AgentCache}, agent already known", fun() ->
			AgentCache = #agent_cache{pid = self(), id = "agentId",
				time_avail = 1, skills = [], channels = [], endpoints = []},
			AgentKey = #agent_key{rotations = 0, has_all = z, skill_count = 0,
				idle_time = 1},
			ExpectedState = State = #state{
				agents = dict:from_list([
					{"testagent", AgentCache}
				]),
				route_list = gb_trees:from_orddict([
					{AgentKey, AgentCache}
				])
			},
			?assertEqual({noreply, ExpectedState}, handle_leader_cast({notify, "testagent", AgentCache}, State, "election"))
		end},

		{"{notify, Agent, AgentCache}, duplicate, pid mismatch", fun() ->
			AgentCache = #agent_cache{pid = self(), id = "agentId",
				time_avail = 1, skills = [], channels = [], endpoints = []},
			AgentKey = #agent_key{rotations = 0, has_all = z, skill_count = 0,
				idle_time = 1},
			State = #state{
				agents = dict:from_list([
					{"testagent", AgentCache}
				]),
				route_list = gb_trees:from_orddict([
					{AgentKey, AgentCache}
				])
			},
			Zombie = util:zombie(),
			BadCache = #agent_cache{pid = Zombie, id = "agentId",
				time_avail = 1, skills = [], channels = [], endpoints = []},
			meck:new(agent),
			meck:expect(agent, register_rejected, fun(InPid) ->
				?assertEqual(Zombie, InPid)
			end),
			?assertEqual({noreply, State}, handle_leader_cast({notify, "testagent", BadCache}, State, "election")),
			?assert(meck:validate(agent)),
			?assertMatch([{_Pid, {agent, register_rejected, [Zombie]}, _}],
				meck:history(agent)
			),
			meck:unload(agent)
		end},

		{"{update_notify, Login, AgentCache}", fun() ->
			AgentCache = #agent_cache{ pid = self(), id = "agentId",
				time_avail = 1, skills = [], channels = [], endpoints = []},
			Key = #agent_key{rotations = 0, has_all = z, skill_count = 0,
				idle_time = 1},
			State = #state{agents = dict:new(), route_list = gb_trees:empty()},
			Expected = #state{
				agents = dict:from_list([
					{"testagent", AgentCache}
				]),
				route_list = gb_trees:from_orddict([
					{Key, AgentCache}
				])
			},
			?assertEqual({noreply, Expected}, handle_leader_cast({update_notify, "testagent", AgentCache}, State, "election"))
		end},

		{"{notify_down, Agent}, agent not found", fun() ->
			State = #state{},
			?assertEqual({noreply, State}, handle_leader_cast({notify_down, "testagent"}, State, "election"))
		end},

		{"{notify_down, Agent}, simple success", fun() ->
			AgentCache = #agent_cache{id = "agentid", pid = self()},
			State = #state{
				agents = dict:from_list([
					{"testagent", AgentCache}
				]),
				route_list = gb_trees:from_orddict([
					{1, AgentCache}
				])
			},
			Expected = #state{},
			?assertEqual({noreply, Expected}, handle_leader_cast({notify_down, "testagent"}, State, "election"))
		end},

		{"{blab, Text, {agent, Value}}", fun() ->
			Zombie = util:zombie(),
			State = #state{agents = dict:from_list([{"testagent", #agent_cache{pid = Zombie}}])},
			meck:new(agent),
			meck:expect(agent, blab, fun(InPid, "blab blab") ->
				?assertEqual(Zombie, InPid)
			end),
			handle_leader_cast({blab, "blab blab", {agent, "testagent"}}, State, "election"),
			?assertEqual(1, length(meck:history(agent))),
			?assert(meck:validate(agent)),
			meck:unload(agent)
		end},

		{"{blab, Text, {node, Node}}", fun() ->
			util:start_testnode(),
			{ok, Slave} = slave:start(net_adm:localhost(), agent_manager_blab_node),
			RZombie = rpc:call(Slave, util, zombie, []),
			LZombie = util:zombie(),
			State = #state{agents = dict:from_list([
				{"remote_dude", #agent_cache{pid = RZombie}},
				{"local_dude", #agent_cache{pid = LZombie}}
			])},
			meck:new(agent),
			meck:expect(agent, blab, fun(InPid, "blab blab") ->
				?assertEqual(RZombie, InPid)
			end),
			handle_leader_cast({blab, "blab blab", {node, Slave}}, State, "election"),
			% spawns out the work, so wait for it to finish
			timer:sleep(10),
			?assertEqual(1, length(meck:history(agent))),
			?assert(meck:validate(agent)),
			meck:unload(agent),
			slave:stop(Slave)
		end},
		
		{"{blab, Text, {profile, Value}}", fun() ->
			State = #state{agents = dict:from_list([
				{"dude1", #agent_cache{pid = true}},
				{"dude2", #agent_cache{pid = false}}
			])},
			meck:new(agent),
			meck:expect(agent, blab, fun(true, "blab blab") -> ok end),
			meck:expect(agent, dump_state, fun
				(true) -> #agent{login = "testagent", id = "id", profile = "testprofile"};
				(false) -> #agent{login = "tnegatset", id = "di", profile = "eliforptset"}
			end),
			handle_leader_cast({blab, "blab blab", {profile, "testprofile"}}, State, "election"),
			% work done in a spawn
			timer:sleep(10),
			?assert(meck:validate(agent)),
			?assertEqual(3, length(meck:history(agent))),
			meck:unload(agent)
		end},

		{"{blab, Text, all}", fun() ->
			State = #state{agents = dict:from_list([
				{"dude1", #agent_cache{pid = pid}},
				{"dude2", #agent_cache{pid = pid}}
			])},
			meck:new(agent),
			meck:expect(agent, blab, fun(pid, "blab blab") -> ok end),
			handle_leader_cast({blab, "blab blab", all}, State, "election"),
			timer:sleep(10),
			?assert(meck:validate(agent)),
			?assertEqual(2, length(meck:history(agent))),
			meck:unload(agent)
		end}

	]},

	{"handle_call", [
		{"list_agents", ?_assert(false)},
		{"list_avail_agents, empty tree", ?_assert(false)},
		{"list_avail_agents, populated tree", ?_assert(false)},
		{"stop", ?_assert(false)},
		{"{start_agent, InAgent}", ?_assert(false)},
		{"{exists, Login}, found local", ?_assert(false)},
		{"{exists, Login}, not found", ?_assert(false)},
		{"{exists, Login}, leader found", ?_assert(false)},
		{"{exists, Login}, leader found, should be local", ?_assert(false)},
		{"{notify, Login, AgentCache}, not found, not leader", ?_assert(false)},
		{"{notify, Login, AgentCache}, not found, leader", ?_assert(false)},
		{"{notify, Login, AgentCache}, found, pid match", ?_assert(false)}
	]},

	{"handle_cast", [
		{"{set_avail, Nom, Chans}, chan list longer", ?_assert(false)},
		{"{set_avail, Nom, Chans}, chan list shorter", ?_assert(false)},
		{"{set_ends, Nom, Ends}, is leader", ?_assert(false)},
		{"{set_ends, Nom, Ends}, is not leader", ?_assert(false)},
		{"{update_skill_list, Login, Skills}, is leader", ?_assert(false)},
		{"{update_skill_list, Login, Skills}, is not leader", ?_assert(false)}
	]},

	{"handle_info", [
		{"{'EXIT', Pid, Reason}, Agent death", ?_assert(false)}
	]}

	].

single_node_test_old() -> 
	util:start_testnode(),
	N = util:start_testnode(agent_manager_single_node_tests),
	{spawn, N, {foreach,
		fun() ->
			Agent = #agent{login="testagent"},
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			mnesia:create_schema([node()]),
			mnesia:start(),
			start([node()]),
			Agent
		end,
		fun(_Agent) -> 
			stop()
		end,
		[
			fun(Agent) ->
				{"Start New Agent", 
					fun() -> 
						{ok, Pid} = start_agent(Agent),
						?assertMatch({error, query_state}, agent:query_state(Pid))
					end
				}
			end,
			fun(Agent) ->
				{"Start Existing Agent",
					fun() -> 
						{ok, Pid} = start_agent(Agent),
						?assertMatch({exists, Pid}, start_agent(Agent))
					end
				}
			end,
			fun(Agent) ->
				{"Lookup agent by name",
					fun() -> 
						{ok, Pid} = start_agent(Agent),
						Login = Agent#agent.login,
						?assertMatch({true, Pid}, query_agent(Login))
					end
				}
			end,
			fun(_Agent) ->
				{"Look for a non-existant agent",
					fun() -> 
						?assertMatch(false, query_agent("does not exist"))
					end
				}
			end,
			fun(_Agent) ->
				{"There's no available agents",
					fun() ->
						Agent1 = #agent{login="Agent1", id="A1"},
						Agent2 = #agent{login="Agent2", id="A2", skills=[english, '_agent', '_node', coolskill, otherskill]},
						Agent3 = #agent{login="Agent3", id="A3", skills=[english, '_agent', '_node', coolskill]},
						{ok, A1} = gen_leader:call(?MODULE, {start_agent, Agent1}),
						{ok, A2} = gen_leader:call(?MODULE, {start_agent, Agent2}),
						{ok, A3} = gen_leader:call(?MODULE, {start_agent, Agent3}),
						?assertEqual([], list_avail())
					end
				}
			end,
			fun(_Agent) ->
				{"There's one available agent",
					fun() ->
						Agent1 = #agent{login="Agent1", id="A1"},
						Agent2 = #agent{login="Agent2", id="A2", skills=[english, '_agent', '_node', coolskill, otherskill]},
						Agent3 = #agent{login="Agent3", id="A3", skills=[english, '_agent', '_node', coolskill]},
						{ok, A1} = gen_leader:call(?MODULE, {start_agent, Agent1}),
						{ok, A2} = gen_leader:call(?MODULE, {start_agent, Agent2}),
						{ok, A3} = gen_leader:call(?MODULE, {start_agent, Agent3}),
						gen_leader:cast(?MODULE, {set_avail, "Agent2", [dummy, voice]}),
						?assertMatch([
							{_Key, #agent_cache{pid = A2, channels = [dummy, voice]}}
						], list_avail())
					end
				}
			end,
			fun(_Agent) ->
				{"There's two available agent",
					fun() ->
						Agent1 = #agent{login="Agent1", id="A1"},
						Agent2 = #agent{login="Agent2", id="A2", skills=[english, '_agent', '_node', coolskill, otherskill]},
						Agent3 = #agent{login="Agent3", id="A3", skills=[english, '_agent', '_node', coolskill]},
						{ok, A1} = gen_leader:call(?MODULE, {start_agent, Agent1}),
						{ok, A2} = gen_leader:call(?MODULE, {start_agent, Agent2}),
						{ok, A3} = gen_leader:call(?MODULE, {start_agent, Agent3}),
						gen_leader:cast(?MODULE, {set_avail, "Agent1", [dummy, voice]}),
						gen_leader:cast(?MODULE, {set_avail, "Agent3", [dummy, voice]}),
						?assertMatch([
							{_Key, #agent_cache{pid = A1, channels = [dummy, voice]}},
							{_Key2, #agent_cache{pid = A3, channels = [dummy, voice]}}
						], list_avail())
					end
				}
			end,
			fun(_Agent) ->
				{"idle time is not updated when channels become available",
					fun() ->
						Agent = #agent{login = "agent", id="agent"},
						{ok, Apid} = gen_leader:call(?MODULE, {start_agent, Agent}),
						?assertEqual([], list_avail()),
						gen_leader:cast(?MODULE, {set_avail, "agent", [dummy]}),
						[{OldKey, _}] = list_avail(),
						gen_leader:cast(?MODULE, {set_avail, "agent", [dummy, voice]}),
						[{NewKey, _}] = list_avail(),
						?DEBUG("Old:  ~p;  New:  ~p", [OldKey, NewKey]),
						?assert(NewKey == OldKey)
					end
				}
			end,
			fun(_Agent) ->
				{"idle time is updated when channels become used",
					fun() ->
						Agent = #agent{login = "agent", id="agent"},
						{ok, Apid} = gen_leader:call(?MODULE, {start_agent, Agent}),
						?assertEqual([], list_avail()),
						gen_leader:cast(?MODULE, {set_avail, "agent", [dummy, voice]}),
						[{OldKey, _}] = list_avail(),
						gen_leader:cast(?MODULE, {set_avail, "agent", [dummy]}),
						[{NewKey, _}] = list_avail(),
						?DEBUG("Old:  ~p;  New:  ~p", [OldKey, NewKey]),
						?assert(NewKey > OldKey)
					end
				}
			end,
			fun(_Agent) ->
				{"Find available agents with a skillset that matches but is the shortest",
					fun() ->
						Agent1 = #agent{login="Agent1", id="A1"},
						Agent2 = #agent{login="Agent2", id="A2", skills=[english, '_agent', '_node', coolskill, otherskill]},
						Agent3 = #agent{login="Agent3", id="A3", skills=[english, '_agent', '_node', coolskill]},
						Agent4 = #agent{login="Agent4", id="A4", skills=[english, '_agent', '_node', coolskill, a, b, c, d, e, f]},
						{ok, Agent1Pid} = gen_leader:call(?MODULE, {start_agent, Agent1}),
						{ok, Agent2Pid} = gen_leader:call(?MODULE, {start_agent, Agent2}),
						{ok, Agent3Pid} = gen_leader:call(?MODULE, {start_agent, Agent3}),
						{ok, Agent4Pid} = gen_leader:call(?MODULE, {start_agent, Agent4}),
						agent:set_release(Agent1Pid, none),
						agent:set_release(Agent2Pid, default),
						agent:set_release(Agent3Pid, none),
						agent:set_release(Agent4Pid, default),
						?DEBUG("agent list:~n~p", [gen_leader:call(?MODULE, list_agents)]),
						?DEBUG("avail agent list:~n~p", [gen_leader:call(?MODULE, list_avail_agents)]),
						?assertMatch([{_, {agent_cache, Agent3Pid, "A3", _Time, _Skills, _Chans, []}}], find_avail_agents_by_skill([coolskill])),
						agent:set_release(Agent2Pid, none),
						agent:set_release(Agent4Pid, none),
						?assertMatch([
							{_, #agent_cache{pid = Agent3Pid, id="A3"}},
							{_, #agent_cache{pid = Agent2Pid, id="A2"}},
							{_, #agent_cache{pid = Agent4Pid, id="A4"}}
						], find_avail_agents_by_skill([coolskill]))
					end
				}
			end,
			fun(_Agent) ->
				{"Find available agents with a skillset that matches but is longest idle",
					fun() ->
						Agent1 = #agent{login="Agent1", id="Agent1"},
						Agent2 = #agent{login="Agent2", id="Agent2", skills=[english, '_agent', '_node', coolskill]},
						Agent3 = #agent{login="Agent3", id="Agent3", skills=[english, '_agent', '_node', coolskill]},
						{ok, Agent1Pid} = gen_leader:call(?MODULE, {start_agent, Agent1}),
						{ok, Agent2Pid} = gen_leader:call(?MODULE, {start_agent, Agent2}),
						{ok, Agent3Pid} = gen_leader:call(?MODULE, {start_agent, Agent3}),
						gen_leader:cast(?MODULE, {set_avail, "Agent1", [dummy]}),
						gen_leader:cast(?MODULE, {set_avail, "Agent2", [dummy]}),
						?assertMatch([{_, {agent_cache, Agent2Pid, "Agent2", _, _, _, _}}], sort_agents_by_elegibility(find_avail_agents_by_skill([coolskill]))),
						receive after 1000 -> ok end,
						gen_leader:cast(?MODULE, {set_avail, "Agent3", [dummy]}),
						?DEBUG("list avail ~p", [list_avail()]),
						?assertMatch([{_, #agent_cache{id = "Agent2"}}, {_, #agent_cache{id = "Agent3"}}], sort_agents_by_elegibility(find_avail_agents_by_skill([coolskill])))
					end
				}
			end,
			fun(_Agent) ->
				{"No agents with valid channel found",
					fun() ->
						Agent = #agent{login = "agent", id = "agent", skills = ['_all'], endpoints = dict:from_list([{dummy_media, inband}])},
						{ok, Apid} = gen_leader:call(?MODULE, {start_agent, Agent}),
						gen_leader:cast(?MODULE, {set_avail, "agent", []}),
						?assertEqual([], filtered_route_list([], dummy, dummy_media))
					end
				}
			end
		]
	}}.



get_nodes() ->
	[_Name, Host] = string:tokens(atom_to_list(node()), "@"),
	{list_to_atom(lists:append("master@", Host)), list_to_atom(lists:append("slave@", Host))}.

-record(multi_node_test_state, {
	master_node,
	slave_node,
	master_am,
	slave_am
}).

multi_node_test_() ->
	util:start_testnode(),
	Master = util:start_testnode(agent_manager_master),
	Slave = util:start_testnode(agent_manager_slave),
	mnesia:change_config(extra_db_nodes, [Master, Slave]),
	cover:start([Master, Slave]),
	{inorder, {foreach, fun() ->
		rpc:call(Master, mnesia, stop, []),
		rpc:call(Slave, mnesia, stop, []),
		mnesia:delete_schema([Master, Slave]),
		mnesia:create_schema([Master, Slave]),
		rpc:call(Master, mnesia, start, []),
		rpc:call(Slave, mnesia, start, []),
		mnesia:change_table_copy_type(schema, Master, disc_copies),
		mnesia:change_table_copy_type(schema, Slave, disc_copies),
		{ok, AMMaster} = rpc:call(Master, ?MODULE, start, [[Master, Slave]]),
		{ok, AMSlave} = rpc:call(Slave, ?MODULE, start, [[Master, Slave]]),
		#multi_node_test_state{
			master_node = Master,
			slave_node = Slave,
			master_am = AMMaster,
			slave_am = AMSlave
		}
	end,
	fun(MultinodeState) ->
		rpc:call(Master, ?MODULE, stop, []),
		rpc:call(Slave, ?MODULE, stop, []),
		rpc:call(Master, mnesia, stop, []),
		rpc:call(Slave, mnesia, stop, [])
	end,
	[fun(TestState) -> {"Slave skips added agent", fun() ->
		% only the leader knows about every agent, it seems
		% the reason not every manager needs to know about every 
		% agent is the cook will ask each dispatcher, which will ask 
		% the local manager.  The resulting lists are combined.
		Agent = #agent{id = "agent", login = "agent"},
		{ok, Apid} = rpc:call(Master, ?MODULE, start_agent, [Agent]),
		List = rpc:call(Slave, ?MODULE, list, []),
		?assertEqual([], List)
	end} end,
	fun(TestState) -> {"Master is informed of agent on slave", fun() ->
		Agent = #agent{id = "agent", login = "agent", skills = []},
		{ok, Apid} = rpc:call(Slave, ?MODULE, start_agent, [Agent]),
		receive after 100 -> ok end,
		List = rpc:call(Master, ?MODULE, list, []),
		?assertMatch([{"agent", #agent_cache{pid = Apid, id="agent", time_avail = {_T1, _T2, _T3}, skills = [], channels = _ChanList, endpoints = []}}], List)
	end} end,
	fun(TestState) -> {"Master removes agents from dead node", fun() ->
		Agent = #agent{id = "agent", login = "agent", skills = []},
		{ok, Apid} = rpc:call(Slave, ?MODULE, start_agent, [Agent]),
		List = rpc:call(Master, ?MODULE, list, []),
		?assertMatch([{"agent", #agent_cache{pid = Apid, id = "agent", time_avail = {_T1, _T2, _T3}, skills = [], channels = _ChanList, endpoints = []}}], List),
		rpc:call(Slave, erlang, exit, [TestState#multi_node_test_state.slave_am, kill]),
		receive after 100 -> ok end
		% TODO enable this at some point.
		%?assertEqual([], rpc:call(Master, ?MODULE, list, []))
	end} end]}}.


%		[
%			fun({Master, Slave, Agent, _Agent2}) ->
%				{"Slave picks up added agent",
%					fun() -> 
%						{ok, Pid} = rpc:call(Master, ?MODULE, start_agent, [Agent]),
%						?assertMatch({exists, Pid}, rpc:call(Slave, ?MODULE, start_agent, [Agent]))
%					end
%				}
%			end,
%			fun({Master, Slave, Agent, _Agent2}) ->
%				{"Slave continues after master dies",
%					fun() -> 
%						{ok, _Pid} = rpc:call(Master, ?MODULE, start_agent, [Agent]),
%						slave:stop(Master),
%						%rpc:call(Master, erlang, disconnect_node, [Slave]),
%						%rpc:call(Slave, erlang, disconnect_node, [Master]),
%						?assertMatch({ok, _NewPid}, rpc:call(Slave, ?MODULE, start_agent, [Agent]))
%					end
%				}
%			end,
%			fun({Master, Slave, _Agent, _Agent2}) ->
%				{"Slave becomes master after master dies",
%					fun() -> 
%						%% getting the pids is important for this test
%						cover:stop([Master, Slave]),
%						slave:stop(Master),
%						slave:stop(Slave),
%						
%						slave:start(net_adm:localhost(), master, " -pa debug_ebin"), 
%						slave:start(net_adm:localhost(), slave, " -pa debug_ebin"),
%						cover:start([Master, Slave]),
%
%						{ok, _MasterP} = rpc:call(Master, ?MODULE, start, [[Master, Slave]]),
%						{ok, SlaveP} = rpc:call(Slave, ?MODULE, start, [[Master, Slave]]),
%
%						%% test proper begins
%						rpc:call(Master, erlang, disconnect_node, [Slave]),
%						cover:stop([Master]),
%						slave:stop(Master),
%						
%						?assertMatch({ok, SlaveP}, rpc:call(Slave, ?MODULE, get_leader, []))
%						
%						%?assertMatch(undefined, global:whereis_name(?MODULE)),
%						%?assertMatch({ok, _Pid}, rpc:call(Slave, ?MODULE, start_agent, [Agent])),
%						%?assertMatch({true, _Pid}, rpc:call(Slave, ?MODULE, query_agent, [Agent])),
%						
%						
%						%Globalwhere = global:whereis_name(?MODULE),
%						%Slaveself = rpc:call(Slave, erlang, whereis, [?MODULE]),
%											
%						%?assertMatch(Globalwhere, Slaveself)
%					end
%				}
%			end,
%			fun({Master, Slave, Agent, Agent2}) ->
%				{"Net Split with unique agents",
%					fun() ->
%						{ok, Apid1} = rpc:call(Master, ?MODULE, start_agent, [Agent]),
%						
%						?assertMatch({exists, Apid1}, rpc:call(Slave, ?MODULE, start_agent, [Agent])),
%					
%						rpc:call(Master, erlang, disconnect_node, [Slave]),
%						rpc:call(Slave, erlang, disconnect_node, [Master]),
%						
%						{ok, Apid2} = rpc:call(Slave, ?MODULE, start_agent, [Agent2]),
%											
%						Pinged = rpc:call(Master, net_adm, ping, [Slave]),
%						Pinged = rpc:call(Slave, net_adm, ping, [Master]),
%
%						?assert(Pinged =:= pong),
%
%						?assertMatch({true, Apid1}, rpc:call(Slave, ?MODULE, query_agent, [Agent])),
%						?assertMatch({true, Apid2}, rpc:call(Master, ?MODULE, query_agent, [Agent2]))
%						
%					end
%				}
%			end,
%			fun({Master, Slave, Agent, Agent2}) ->
%				{"Master removes agents for a dead node",
%					fun() ->
%						?assertMatch({ok, _Pid}, rpc:call(Slave, ?MODULE, start_agent, [Agent])),
%						?assertMatch({ok, _Pid}, rpc:call(Master, ?MODULE, start_agent, [Agent2])),
%						?assertMatch({true, _Pid}, rpc:call(Master, ?MODULE, query_agent, [Agent])),
%						%rpc:call(Master, erlang, disconnect_node, [Slave]),
%						cover:stop(Slave),
%						slave:stop(Slave),
%						?assertEqual(false, rpc:call(Master, ?MODULE, query_agent, [Agent])),
%						?assertMatch({true, _Pid}, rpc:call(Master, ?MODULE, query_agent, [Agent2])),
%						?assertMatch({ok, _Pid}, rpc:call(Master, ?MODULE, start_agent, [Agent]))
%					end
%				}
%			end,
%			fun({Master, Slave, Agent, _Agent2}) ->
%				{"Master is notified of agent removal on slave",
%					fun() ->
%						{ok, Pid} = rpc:call(Slave, ?MODULE, start_agent, [Agent]),
%						?assertMatch({true, Pid}, rpc:call(Slave, ?MODULE, query_agent, [Agent])),
%						?assertMatch({true, Pid}, rpc:call(Master, ?MODULE, query_agent, [Agent])),
%						exit(Pid, kill),
%						timer:sleep(300),
%						?assertMatch(false, rpc:call(Slave, ?MODULE, query_agent, [Agent])),
%						?assertMatch(false, rpc:call(Master, ?MODULE, query_agent, [Agent]))
%					end
%				}
%			end
%		]
%	}.
-endif.

-ifdef(PROFILE).

avg(Times) ->
	Sum = lists:foldl(fun(E, S) -> E + S end, 0, Times),
	Sum / length(Times).

adding_agents_tc_test_() ->
	{foreach,
	fun() ->
		{ok, File} = file:open(?MODULE_STRING ++ "-profile.txt", [append]),
		{ok, AM} = agent_manager:start([node()]),
		File
	end,
	fun(File) ->
		file:close(File),
		agent_manager:stop()
	end,
	[fun(File) -> Name = "agents with same length skills", {timeout, 60, {Name, fun() ->
		Agents = [#agent{
			login = integer_to_list(X),
			skills = [X rem 5]
		} || X <- lists:seq(1, 1000)],
		Times = [begin
			{T, _} = timer:tc(agent_manager, start_agent, [A]),
			T
		end || A <- Agents],
		[case agent_manager:query_agent(A#agent.login) of
			{true, Pid} ->
				exit(Pid, kill);
			false ->
				ok
		end || A <- Agents],
		?INFO("Average for ~s:  ~f", [Name, avg(Times)]),
		?assert(true),
		io:format(File, "~p	~s:~s(~s)	~f~n", [os:timestamp(), ?MODULE, adding_agents_tc_test, Name, avg(Times)])
	end}} end,
	fun(File) -> Name = "agents with variable length skills", {timeout, 60, {Name, fun() ->
		Agents = [#agent{
			login = integer_to_list(X),
			skills = [S || S <- lists:seq(0, X rem 10)]
		} || X <- lists:seq(1, 1000)],
		Times = [begin
			{T, _} = timer:tc(agent_manager, start_agent, [A]),
			T
		end || A <- Agents],
		[case agent_manager:query_agent(A#agent.login) of
			{true, Pid} ->
				exit(Pid, kill);
			false ->
				ok
		end || A <- Agents],
		?INFO("Average:  ~f", [avg(Times)]),
		io:format(File, "~p	~s:~s(~s)	~f~n", [os:timestamp(), ?MODULE, adding_agent_tc_test, Name, avg(Times)]),
		?assert(true)
	end}} end,
	fun(File) -> Name = "agent skill list shoved in the middle", {timeout, 60, {Name, fun() ->
		LowAgents = [#agent{
			login = integer_to_list(X),
			skills = []
		} || X <- lists:seq(1, 499)],
		HighAgents = [#agent{
			login = integer_to_list(X),
			skills = [english, german]
		} || X <- lists:seq(501, 1000)],
		AllAgents = LowAgents ++ HighAgents,
		[agent_manager:start_agent(A) || A <- AllAgents],
		Times = [begin
			{T, {_, Pid}} = timer:tc(agent_manager, start_agent, [#agent{
				login = "500",
				skills = [english]
			}]),
			exit(Pid, kill),
			% give it a moment to clear it
			timer:sleep(10),
			T
		end || _X <- lists:seq(1, 1000)],
		?INFO("Average:  ~f", [avg(Times)]),
		io:format(File, "~p	~s:~s(~s)	~f~n", [os:timestamp(), ?MODULE, adding_agent_tc_test, Name, avg(Times)]),
		?assert(true)
	end}} end]}.

-ifdef(PROFILE).

avg(Times) ->
	Sum = lists:foldl(fun(E, S) -> E + S end, 0, Times),
	Sum / length(Times).

tdiff({InMeg, InSec, InMic}, {GotMeg, GotSec, GotMic}) ->
	In = InMeg * 1000000 + InSec + InMic / 1000000,
	Got = GotMeg * 1000000 + GotSec + GotMic / 1000000,
	Got - In.

adding_agents_test_() ->
	{foreach,
	fun() ->
		{ok, AM} = agent_manager:start([node()]),
		AM
	end,
	fun(_AM) ->
		agent_manager:stop()
	end,
	[fun(_) -> {timeout, 60, {"agents with same length skills", fun() ->
		Agents = [#agent{
			login = integer_to_list(X),
			skills = [X rem 5]
		} || X <- lists:seq(1, 1000)],
		Times = [begin
			Start = os:timestamp(),
			agent_manager:start_agent(A),
			End = os:timestamp(),
			tdiff(Start, End)
		end || A <- Agents],
		[case agent_manager:query_agent(A#agent.login) of
			{true, Pid} ->
				exit(Pid, kill);
			false ->
				ok
		end || A <- Agents],
		?INFO("Average:  ~f", [avg(Times)]),
		?assert(true)
	end}} end,
	fun(_) -> {timeout, 60, {"agents with variable length skills", fun() ->
		Agents = [#agent{
			login = integer_to_list(X),
			skills = [S || S <- lists:seq(0, X rem 10)]
		} || X <- lists:seq(1, 1000)],
		Times = [begin
			Start = os:timestamp(),
			agent_manager:start_agent(A),
			End = os:timestamp(),
			tdiff(Start, End)
		end || A <- Agents],
		[case agent_manager:query_agent(A#agent.login) of
			{true, Pid} ->
				exit(Pid, kill);
			false ->
				ok
		end || A <- Agents],
		?INFO("Average:  ~f", [avg(Times)]),
		?assert(true)
	end}} end]}.
		

-endif.

-endif.

