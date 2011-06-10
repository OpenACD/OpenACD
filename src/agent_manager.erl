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
-type(skill() :: atom() | {atom(), any()}).
-type(skills() :: [skill()]).
-type(channels() :: [channel_category()]).
-type(endpoints() :: [atom()]). % list of media modules.
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
	rotate_based_on_list_count/1,
	find_by_pid/1,
	blab/2,
	get_leader/0,
	list/0,
	notify/5,
	log_state/3
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
%% makes sure the server knows it's for routing.
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
		{_K, {_V, _Aid, AgSkills, AgChans, AgEndpoints}} = O <- Agents,
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
		{_K, {_V, _Aid, AgSkills}} = O <- Agents,
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


%% @doc To keep the first agent on a given node from being flooded with 
%% messages rotate the list based on how many times it's been requested
%% without the agent list being updated.
-spec(rotate_based_on_list_count/1 :: (Agents :: [{string(), pid(), integer(), [atom()], {atom(), integer()}}]) -> any()).
rotate_based_on_list_count(Agents) ->
	Nodemap = create_node_map(Agents),
	Remap = rotate_based_on_list_count(Agents, Nodemap, 1, []),
	remap(Agents, Remap).

remap(List, Remap) ->
	Sort = fun({_O1, N1}, {_O2, N2}) ->
		N1 =< N2
	end,
	remap(List, lists:sort(Sort, Remap), []).

remap(_List, [], Acc) ->
	lists:reverse(Acc);
remap(List, [{Old, _New} | Tail], Acc) ->
	E = lists:nth(Old, List),
	remap(List, Tail, [E | Acc]).

rotate_based_on_list_count([], _Nodemap, _Nth, Remap) ->
	lists:reverse(Remap);
rotate_based_on_list_count([{_Login, _Pid, _Timeavail, _Skills, {Node, Count}} | Tail], Nodemap, Nth, Remap) ->
	Map = dict:fetch(Node, Nodemap),
	OriginalIndex = util:list_index(Nth, Map),
	% clock math.  Given [1, 2, 3] :: 2 + 3 = 2; 1 + 2 = 3; 1 + 4 = 1; 2 + 5 = 3
	Newindex = case ( (OriginalIndex + Count) rem length(Map) ) of 0 -> length(Map); I -> I end,
	Newmap = lists:nth(Newindex, Map),
	rotate_based_on_list_count(Tail, Nodemap, Nth + 1, [{Nth, Newmap} | Remap]).

create_node_map(Agents) ->
	create_node_map(Agents, 1, dict:new()).

create_node_map([], _Nth, Acc) ->
	Acc;
create_node_map([{_Login, _Pid, _Timeavail, _Skills, {Node, _Count}} | Tail], Nth, Acc) ->
	case dict:is_key(Node, Acc) of
		true ->
			create_node_map(Tail, Nth + 1, dict:append(Node, Nth, Acc));
		false ->
			create_node_map(Tail, Nth + 1, dict:store(Node, [Nth], Acc))
	end.

%% @doc Gets all the agents have have the given `[atom()] Skills'.
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
	gen_leader:call(?MODULE, list_avail_agents).
	
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

%% @doc Used by agents to write to the agent states table.
-spec(log_state/3 :: (Agent :: string(), State :: atom(), Statedata :: any()) -> 'ok').
log_state(Agent, State, Statedata) ->
	gen_leader:cast(?MODULE, {log_state, Agent, State, Statedata}).
	
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

%% gen_leader callbacks
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
	F = fun(_Login, {Apid, _, _, _, _, _}) -> 
		node() =:= node(Apid)
	end,
	Locals = dict:filter(F, Agents),
	% and tell the leader about local pids
	Notify = fun({Login, V}) -> 
		gen_leader:leader_cast(?MODULE, {update_notify, Login, V})
	end,
	Dictlist= dict:to_list(Locals),
	lists:foreach(Notify, Dictlist),
	RoutlistFilter = fun({_Key, {Pid, _Id, _Skills, _Chans, _Ends}}) ->
		node() =:= node(Pid)
	end,
	Routelist = gb_trees_filter(RoutlistFilter, State#state.route_list),
	{ok, State#state{agents=Locals, route_list = Routelist}}.
	
%% @hidden
handle_DOWN(Node, #state{agents = Agents} = State, _Election) -> 
	% clean out the pids associated w/ the dead node
	F = fun(_Login, {Apid, _, _, _}) -> 
		Node =/= node(Apid)
	end,
	Agents2 = dict:filter(F, Agents),
	Routelist = gb_trees_filter(fun({_Key, {Pid, _Id, _Skills, _Chans, _Ends}}) ->
		Node =/= node(Pid)
	end, State#state.route_list),
	{ok, State#state{agents = Agents2, route_list = Routelist}}.

%% @hidden
handle_leader_call({exists, Agent}, From, #state{agents = _Agents} = State, Election) when is_list(Agent) -> 
	?DEBUG("Trying to determine if ~p exists", [Agent]),
	case handle_leader_call({full_data, Agent}, From, State, Election) of
		{reply, false, _State} = O ->
			O;
		{reply, {Apid, _Id, _Time, _Skills, _Chans, _Ends}, NewState} ->
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
handle_leader_cast({notify, Agent, Id, Apid, TimeAvail, Skills, Chans, Ends}, #state{agents = Agents} = State, _Election) -> 
	?INFO("Notified of ~p pid ~p", [Agent, Apid]),
	case dict:find(Agent, Agents) of
		error -> 
			Agents2 = dict:store(Agent, {Apid, Id, TimeAvail, Skills, Chans, Ends}, Agents),
			Routelist = case TimeAvail of
				0 ->
					State#state.route_list;
				_ ->
					gb_trees:enter({0, ?has_all(Skills), length(Skills), TimeAvail}, {Apid, Id, Skills, Chans, Ends}, State#state.route_list)
			end,
			{noreply, State#state{agents = Agents2, route_list = Routelist}};
		{ok, {Apid, _, _, _}} ->
			{noreply, State};
		_Else -> 
			agent:register_rejected(Apid),
			{noreply, State}
	end;
handle_leader_cast({update_notify, Login, {Pid, Id, Time, Skills, Chans, Ends} = Value}, #state{agents = Agents} = State, _Election) ->
	NewAgents = dict:update(Login, fun(_Old) -> Value end, Value, Agents),
	Midroutelist = gb_trees_filter(fun({_Key, {Apid, _FunId, _FunSkills, _Chans, _Ends}}) ->
		Apid =/= Pid
	end, State#state.route_list),
	Routelist = case Time of
		0 ->
			Midroutelist;
		_ ->
			gb_trees:enter({0, ?has_all(Skills), length(Skills), Time}, {Pid, Id, Skills, Chans, Ends}, Midroutelist)
	end,
	{noreply, State#state{agents = NewAgents, route_list = Routelist}};
handle_leader_cast({notify_down, Agent}, #state{agents = Agents} = State, _Election) ->
	?NOTICE("leader notified of ~p exiting", [Agent]),
	Routelist = case dict:find(Agent, Agents) of
		error ->
			State#state.route_list;
		{ok, {Pid, _Id, _Time, _, _, _}} ->
			gb_trees_filter(fun({_Key, {Apid, _FunId, _Skills, _Chans, _Ends}}) ->
				Apid =/= Pid
			end, State#state.route_list)
	end,
	{noreply, State#state{agents = dict:erase(Agent, Agents), route_list = Routelist}};
handle_leader_cast({blab, Text, {agent, Value}}, #state{agents = Agents} = State, _Election) ->
	case dict:find(Value, Agents) of
		error ->
			% /shrug.  Meh.
			{noreply, State};
		{ok, {Apid, _Id, _Time, _Skills, _Chans, _Ends}} ->
			agent:blab(Apid, Text),
			{noreply, State}
	end;
handle_leader_cast({blab, Text, {node, Value}}, #state{agents = Agents} = State, _Election) ->
	Alist = dict:to_list(Agents),
	F = fun({_Aname, {Apid, _Id, _Time, _Skills, _Chans, _Ends}}) -> 
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
	Foreach = fun({_Aname, {Apid, _Id, _Time, _Skills, _Chans, _Ends}}) ->
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
	F = fun(_, {Pid, _, _, _}) ->
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
			{{OldCount, AllSkillFlag, Len, Time}, Val, Midroutelist} = gb_trees:take_smallest(Routelist),
			gb_trees:enter({OldCount + 1, AllSkillFlag, Len, Time}, Val, Midroutelist)
	end,
	{reply, List, State#state{lists_requested = Count + 1, route_list = NewRoutelist}};
handle_call(stop, _From, State, _Election) -> 
	{stop, normal, ok, State};
handle_call({start_agent, #agent{login = ALogin, id=Aid} = Agent}, _From, #state{agents = Agents} = State, Election) -> 
	% This should not be called directly!  use the wrapper start_agent/1
	?INFO("Starting new agent ~p", [Agent]),
	{ok, Apid} = agent:start(Agent, [logging, gen_leader:candidates(Election)]),
	link(Apid),
	Value = {Apid, Aid, 0, Agent#agent.skills, Agent#agent.available_channels, dict:fetch_keys(Agent#agent.endpoints)},
	Leader = gen_leader:leader_node(Election),
	case node() of
		Leader ->
			ok;
		_ ->
			gen_leader:leader_cast(?MODULE, {update_notify, ALogin, Value})
	end,
	Agents2 = dict:store(ALogin, Value, Agents),
	gen_server:cast(dispatch_manager, {end_avail, Apid}),
	% does not go into the route_list untile they go available
	{reply, {ok, Apid}, State#state{agents = Agents2}};
handle_call({exists, Login}, _From, #state{agents = Agents} = State, Election) ->
	Leader = gen_leader:leader_node(Election),
	case dict:find(Login, Agents) of
		error when Leader =/= node() ->
			case gen_leader:leader_call(?MODULE, {full_data, Login}) of
				false ->
					{reply, false, State};
				{Pid, _Id, _Time, _Skills, _Chans, _Ends} = V when node(Pid) =:= node() ->
					% leader knows about a local agent, but we didn't!
					% So we update the local dict
					Agents2 = dict:store(Login, V, Agents),
					{reply, {true, Pid}, State#state{agents = Agents2}};
				{OtherPid, _Id, _Time, _Skills, _Chans, _Ends} ->
					{reply, {true, OtherPid}, State}
			end;
		error -> % we're the leader
			{reply, false, State};
		{ok, {Pid, _Id, _Timeavail, _Skills, _Chans, _Ends}} ->
			{reply, {true, Pid}, State}
	end;
handle_call({notify, Login, Id, Pid, TimeAvail, Skills, Chans, Ends}, _From, #state{agents = Agents} = State, Election) when is_pid(Pid) andalso node(Pid) =:= node() ->
	case dict:find(Login, Agents) of
		error ->
			link(Pid),
			case gen_leader:leader_node(Election) =:= node() of
				false -> 
					gen_leader:leader_cast(?MODULE, {notify, Login, Id, Pid, TimeAvail, Skills, Chans, Ends});
				_Else ->
					ok
			end,
			Agents2 = dict:store(Login, {Pid, Id, TimeAvail, Skills, Chans, Ends}, Agents),
			case TimeAvail of
				0 ->
					{reply, ok, State#state{agents = Agents2}};
				_ ->
					% only clear the lists_requested if the agent is actually available.
					Midroutelist = gb_trees:enter({0, ?has_all(Skills), length(Skills), TimeAvail}, {Pid, Id, Skills, Chans, Ends}, State#state.route_list),
					{reply, ok, State#state{agents = Agents2, lists_requested = 0, route_list = clear_rotates(Midroutelist)}}
			end;
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
	{Pid, Id, _Time, Skills, OldChans, Ends} = dict:fetch(Nom, Agents),
	Midroutelist = gb_trees_filter(fun({_Key, {Apid, _, _}}) ->
		Apid =/= Pid
	end, State#state.route_list),
	Time = os:timestamp(),
	Out = {Pid, Id, Time, Skills, Chans, Ends},
	Routelist = gb_trees:enter({0, ?has_all(Skills), length(Skills), Time}, {Pid, Id, Skills, Chans, Ends}, Midroutelist),
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
	{Pid, Id, Time, _, Chans, Ends} = dict:fetch(Login, Agents),
	Out = {Pid, Id, Time, Skills, Chans, Ends},
	Midroutelist = clear_rotates(gb_trees_filter(fun({_, {Apid, _, _}}) ->
		Apid =/= Pid
	end, State#state.route_list)),
	Routelist = case Time of
		0 ->
			Midroutelist;
		_ ->
			gb_trees:enter({0, ?has_all(Skills), length(Skills), Time}, {Pid, Id, Skills, Chans, Ends}, Midroutelist)
	end,
	F = fun({_FunPid, _FunId, _FunTime, _OldSkills, _FunChans, _FunEnds}) ->
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
	F = fun(Key, {Value, Id, _Time, _Skills, _Chans, _Ends}) ->
		case Value =/= Pid of
			true -> true;
			false ->
				?NOTICE("notifying leader of ~p exit", [{Key, Id}]),
				cpx_monitor:drop({agent, Id}),
				gen_leader:leader_cast(?MODULE, {notify_down, Key}),
				false
		end
	end,
	Routelist = clear_rotates(gb_trees_filter(fun({_, {Apid, _, _, _, _}}) ->
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
find_via_pid(Needle, [{Key, {Needle, _, _, _, _, _}} | _Tail]) ->
	Key;
find_via_pid(Needle, [{_Key, _NotNeedle} | Tail]) ->
	find_via_pid(Needle, Tail).

%% @private
find_via_id(_Needle, []) ->
	notfound;
find_via_id(Needle, [{Key, {_, Needle, _, _, _, _}} | _Tail]) ->
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

clear_rotates({{0, _, _, _} = Key, Val, Tree}) ->
	gb_trees:enter(Key, Val, Tree);
clear_rotates({{_, AllSkillFlag, Len, Time}, Val, Tree}) ->
	Newtree = gb_trees:enter({0, AllSkillFlag, Len, Time}, Val, Tree),
	clear_rotates(gb_trees:take_largest(Newtree));
clear_rotates(Tree) ->
	case gb_trees:is_empty(Tree) of
		true ->
			Tree;
		false ->
			clear_rotates(gb_trees:take_largest(Tree))
	end.
	
build_tables() ->
	agent_auth:build_tables(),
	util:build_table(agent_state, [
		{attributes, record_info(fields, agent_state)},
		{disc_copies, [node()]},
		{type, bag}
	]).

-ifdef(STANDARD_TEST).

handle_call_start_test() ->
	util:start_testnode(),
	N = util:start_testnode(agent_manager_handle_call_start_test),
	{spawn, N, [fun() -> 
		?assertMatch({ok, _Pid}, start([node()])),
		stop(),
		slave:stop(N)
	end]}.

sort_eligible_agents_test_() ->
	[{"only difference is the length of the skills list",
	fun() ->
		In = [{"3rd", "3rd", 3, [a, b, c, d], ignored}, {"1st", "1st", 3, [a], ignored}, {"2nd", "2nd", 3, [a, b, c], ignored}],
		Out = [{"1st", "1st", 3, [a], ignored}, {"2nd", "2nd", 3, [a, b, c], ignored}, {"3rd", "3rd", 3, [a, b, c, d], ignored}],
		?assertEqual(Out, sort_agents_by_elegibility(In))
	end},
	{"Only difference is the time went available",
	fun() ->
		In = [{"3rd", "3rd", 9, [a], ignored}, {"1st", "1st", 3, [a], ignored}, {"2nd", "2nd", 6, [a], ignored}],
		Out = [{"1st", "1st", 3, [a], ignored}, {"2nd", "2nd", 6, [a], ignored}, {"3rd", "3rd", 9, [a], ignored}],
		?assertEqual(Out, sort_agents_by_elegibility(In))
	end},
	{"'_all' loses the skill list war",
	fun() ->
		In = [{"3rd", "3rd", 3, ['_all'], ignored}, {"1st", "1st", 3, [a], ignored}, {"2nd", "2nd", 3, [a, b, c], ignored}],
		Out = [{"1st", "1st", 3, [a], ignored}, {"2nd", "2nd", 3, [a, b, c], ignored}, {"3rd", "3rd", 3, ['_all'], ignored}],
		?assertEqual(Out, sort_agents_by_elegibility(In))
	end},
	{"Big sortification",
	fun() ->
		In = [
			{"5th", "5th", 9, ['_all'], ignored},
			{"7th", "7th", 9, ['_all', a], ignored},
			{"2nd", "2nd", 9, [a, b], ignored},
			{"6th", "6th", 6, ['_all', a], ignored},
			{"3rd", "3rd", 6, [a, b, c], ignored},
			{"1st", "1st", 6, [a, b], ignored},
			{"4th", "4th", 6, ['_all'], ignored}
		],
		Out = [
			{"1st", "1st", 6, [a, b], ignored},
			{"2nd", "2nd", 9, [a, b], ignored},
			{"3rd", "3rd", 6, [a, b, c], ignored},
			{"4th", "4th", 6, ['_all'], ignored},
			{"5th", "5th", 9, ['_all'], ignored},
			{"6th", "6th", 6, ['_all', a], ignored},
			{"7th", "7th", 9, ['_all', a], ignored}
		],
		?assertEqual(Out, sort_agents_by_elegibility(In))
	end}].

remap_test() ->
	Out = [1, 2, 3, 4, 5, 6],
	In = [3, 6, 5, 4, 1, 2],
	Remap = [{1, 3}, {2, 6}, {3, 5}, {4, 4}, {5, 1}, {6, 2}],
	?assertEqual(Out, remap(In, Remap)).

rotate_based_on_list_count_test_() ->
	[{"No rotation",
	fun() ->
		In = [{"1st", "1st", 3, [], {node(), 0}}, {"2nd", "2nd", 6, [], {node(), 0}}, {"3rd", "3rd", 9, [], {node(), 0}}],
		?assertEqual(In, rotate_based_on_list_count(In))
	end},
	{"All same node, rotate one",
	fun() ->
		In = [{"1st", "1st", 3, [], {node(), 1}}, {"2nd", "2nd", 6, [], {node(), 1}}, {"3rd", "3rd", 9, [], {node(), 1}}],
		Out = [{"3rd", "3rd", 9, [], {node(), 1}}, {"1st", "1st", 3, [], {node(), 1}}, {"2nd", "2nd", 6, [], {node(), 1}}],
		?assertEqual(Out, rotate_based_on_list_count(In))
	end},
	{"Diff nodes, no rotates",
	fun() ->
		In = [
			{"1st", "1st", 3, [], {one@node, 0}},
			{"2nd", "2nd", 3, [], {two@node, 0}},
			{"3rd", "3rd", 6, [], {one@node, 0}},
			{"4th", "4th", 6, [], {two@node, 0}}
		],
		?assertEqual(In, rotate_based_on_list_count(In))
	end},
	{"Diff nodes, one@node rotates",
	fun() ->
		In = [
			{"3rd", "3rd", 3, [], {one@node, 1}},
			{"2nd", "2nd", 3, [], {two@node, 0}},
			{"1st", "1st", 6, [], {one@node, 1}},
			{"4th", "4th", 6, [], {two@node, 0}}
		],
		Out = [
			{"1st", "1st", 6, [], {one@node, 1}},
			{"2nd", "2nd", 3, [], {two@node, 0}},
			{"3rd", "3rd", 3, [], {one@node, 1}},
			{"4th", "4th", 6, [], {two@node, 0}}
		],
		?assertEqual(Out, rotate_based_on_list_count(In))
	end}].

ds() ->
	spawn(fun() -> ok end).

filter_avail_agents_by_skill_test_() ->
	[{"one in, one out",
	fun() ->
		Agents = [{{0, z, 1, {100, 100, 100}}, {ds(), "agent", [skill]}}],
		?assertEqual(Agents, filter_avail_agents_by_skill(Agents, [skill]))
	end},
	{"two in, one out",
	fun() ->
		[Out | _] = Agents = [
			{{0, z, 1, {100, 100, 100}}, {ds(), "agent1", [skill]}},
			{{0, z, 0, {100, 100, 100}}, {ds(), "agent2", []}}
		],
		?assertEqual([Out], filter_avail_agents_by_skill(Agents, [skill]))
	end},
	{"agent with all gets in",
	fun() ->
		Agents = [{{0, z, 1, {100, 100, 100}}, {ds(), "agent", ['_all']}}],
		?assertEqual(Agents, filter_avail_agents_by_skill(Agents, [skill]))
	end},
	{"agents get through when all passed in",
	fun() ->
		Agents = [
			{{0, z, 1, {100, 100, 100}}, {ds(), "agent1", [skill]}},
			{{0, z, 0, {100, 100, 100}}, {ds(), "agent2", []}}
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
			Agents = dict:from_list([{"agent", {ds(), "agent", 0, []}}]),
			State = #state{agents = Agents},
			{noreply, Newstate} = handle_cast({now_avail, "agent"}, State, Election),
			?assertNot(gb_trees:is_empty(Newstate#state.route_list)),
			?assertMatch([{{0, z, 0, _}, {_, "agent", []}}], gb_trees:to_list(Newstate#state.route_list))
		end},
		{"basic end avail",
		fun() ->
			Time = os:timestamp(),
			Pid = ds(),
			Agents = dict:from_list([{"agent", {Pid, "agent", Time, []}}]),
			Routelist = gb_trees:enter({0, z, 0, Time}, {Pid, "agent", []}, gb_trees:empty()),
			State = Seedstate#state{agents = Agents, route_list = Routelist},
			{noreply, Newstate} = handle_cast({end_avail, "agent"}, State, Election),
			?assert(gb_trees:is_empty(Newstate#state.route_list))
		end},
		{"updatin' a skill list of an idle agent",
		fun() ->
			Pid = ds(),
			Time = os:timestamp(),
			Agents = dict:from_list([{"agent", {Pid, "agent", Time, []}}]),
			Routelist = gb_trees:enter({0, z, 0, Time}, {Pid, "agent", []}, gb_trees:empty()),
			State = #state{agents = Agents, route_list = Routelist},
			{noreply, NewState} = handle_cast({update_skill_list, "agent", [skill]}, State, Election),
			?assertNot(gb_trees:is_empty(NewState#state.route_list)),
			?assertMatch([{{0, z, 1, _}, {Pid, "agent", [skill]}}], gb_trees:to_list(NewState#state.route_list))
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
	
	
	
single_node_test_() -> 
	util:start_testnode(),
	N = util:start_testnode(agent_manage_single_node_tests),
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
						?assertMatch({ok, released}, agent:query_state(Pid))
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
				{"Look for a non-existang agent",
					fun() -> 
						?assertMatch(false, query_agent("does not exist"))
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
						agent:set_state(Agent1Pid, idle),
						agent:set_state(Agent3Pid, idle),
						?DEBUG("agent list:~n~p", [gen_leader:call(?MODULE, list_agents)]),
						?DEBUG("avail agent list:~n~p", [gen_leader:call(?MODULE, list_avail_agents)]),
						?assertMatch([{_, {Agent3Pid, "A3", _Skills}}], find_avail_agents_by_skill([coolskill])),
						agent:set_state(Agent2Pid, idle),
						agent:set_state(Agent4Pid, idle),
						?assertMatch([
							{_, {Agent3Pid, "A3", _}},
							{_, {Agent2Pid, "A2", _}},
							{_, {Agent4Pid, "A4", _}}
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
						agent:set_state(Agent1Pid, idle),
						agent:set_state(Agent2Pid, idle),
						?assertMatch([{_, {Agent2Pid, "Agent2", _}}], sort_agents_by_elegibility(find_avail_agents_by_skill([coolskill]))),
						receive after 1000 -> ok end,
						agent:set_state(Agent3Pid, idle),
						?assertMatch([{_, {Agent2Pid, "Agent2", _}}, {_, {Agent3Pid, "Agent3", _}}], sort_agents_by_elegibility(find_avail_agents_by_skill([coolskill])))
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
		List = rpc:call(Master, ?MODULE, list, []),
		?assertEqual([{"agent", {Apid, "agent", 0, []}}], List)
	end} end,
	fun(TestState) -> {"Master removes agents from dead node", fun() ->
		Agent = #agent{id = "agent", login = "agent", skills = []},
		{ok, Apid} = rpc:call(Slave, ?MODULE, start_agent, [Agent]),
		List = rpc:call(Master, ?MODULE, list, []),
		?assertEqual([{"agent", {Apid, "agent", 0, []}}], List),
		rpc:call(Slave, erlang, exit, [TestState#multi_node_test_state.slave_am, kill]),
		?assertEqual([], rpc:call(Master, ?MODULE, list, []))
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

