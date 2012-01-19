-module(cpx_agent_event).
-behavior(gen_event).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("agent.hrl").
-include_lib("stdlib/include/qlc.hrl").

% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	terminate/2, code_change/3]).

% api
-export([
	start/0,
	start_link/0,
	stop/0,
	agent_init/1,
	change_profile/2,
	change_state/2,
	change_agent/2,
	agent_channel_init/4,
	change_agent_channel/3,
	truncate/1
]).

%% =====
%% API
%% =====

%% @doc starts the agent event server.
-spec(start/0 :: () -> {'ok', pid()}).
start() ->
	{ok, Nodes} = cpx:get_env(nodes, [node()]),
	start(Nodes).

%% @doc Starts the event server using the given nodes as part of the
%% mnesia cluster.
-spec(start/1 :: (Nodes :: [node()]) -> {'ok', pid()}).
start(Nodes) ->
	case build_tables(Nodes) of
		ok -> ok; 
		Else ->
			?WARNING("Some tables didn't build, this may crash later. ~p", [Else])
	end,
	gen_event:start({local, ?MODULE}).

%% @doc Starts teh agent event server linked.
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() ->
	{ok, Nodes} = cpx:get_env(nodes, [node()]),
	start_link(Nodes).

%% @doc Starts the agent event server linked using the given ndoes as part
%% of the mnesia cluster.
-spec(start_link/1 :: (Nodes :: [atom()]) -> {'ok', pid()}).
start_link(Nodes) ->
	case build_tables(Nodes) of
		ok -> ok;
		Else ->
			?WARNING("Some tables didn't build, this may crash later. ~p", [Else])
	end,
	gen_event:start_link({local, ?MODULE}).

%% @doc Stops the agent event server.
-spec(stop/0 :: () -> 'ok').
stop() ->
	gen_event:stop(?MODULE).

%% @doc Create a handler specifically for the given agent.  Handles profile
%% login, logout, profile changes, release, and idle states.
-spec(agent_init/1 :: (Agent :: #agent{}) -> 'ok' | {atom(), any()}).
agent_init(Agent) when is_record(Agent, agent) ->
	try begin
		Handlers = gen_event:which_handlers(?MODULE),
		Id = Agent#agent.id,
		Member = lists:member({?MODULE, Id}, Handlers),
		case Member of
			false ->
				ok = gen_event:add_sup_handler(?MODULE, {?MODULE, Id}, Agent);
			_ ->
				?INFO("Agent Event already initialized for ~s", [Agent#agent.login])
		end
	end catch
		What:Why ->
			?ERROR("Initializing Agent Event Failed due to ~p:~p", [What,Why]),
			{What, Why}
	end.

%% @doc An agent has changed profiles
-spec(change_profile/2 :: (OldAgent :: #agent{}, NewAgent :: #agent{}) -> 'ok').
change_profile(OldAgent, NewAgent) when is_record(OldAgent, agent),
is_record(NewAgent, agent) ->
	gen_event:notify(?MODULE, {change_profile, OldAgent, NewAgent}).

%% @doc An agent has changed state (idle <-> released)
-spec(change_state/2 :: (OldAgent :: #agent{}, NewAgent :: #agent{}) -> 'ok').
change_state(OldAgent, NewAgent) when is_record(OldAgent, agent),
is_record(NewAgent, agent) ->
	gen_event:notify(?MODULE, {change_state, OldAgent, NewAgent}).

%% @doc An agent has changed in some way.  The handler will figure out
%% how and send the correct state change or profile change
-spec(change_agent/2 :: (OldAgent :: #agent{}, NewAgent :: #agent{}) -> 'ok').
change_agent(OldAgent, NewAgent) when is_record(OldAgent, agent),
is_record(NewAgent, agent) ->
	gen_event:notify(?MODULE, {change_agent, OldAgent, NewAgent}).
	
%% @doc Create a handler specifically for the given agent channel.
-spec(agent_channel_init/4 :: (Agent :: string(), ChannelId :: pid(),
Statename :: atom(), Statedata :: any()) -> 'ok' | {atom(), any()}).
agent_channel_init(Agent, ChannelId, Statename, Statedata) ->
	try begin
		Handlers = gen_event:which_handlers(?MODULE),
		Member = lists:member({?MODULE, ChannelId}, Handlers),
		case Member of
			false ->
				ok = gen_event:add_sup_handler(?MODULE, {?MODULE, ChannelId}, [Agent, ChannelId, Statename, Statedata]);
			_ ->
				?INFO("Agent channel ~p already has event initialized", [ChannelId])
		end
	end catch
		What:Why ->
			?ERROR("Initialization failed for channel id ~s:~p due to ~p:~p", [Agent, ChannelId, What, Why]),
			{What, Why}
	end.

%% @doc Alert the appropriate handler that an agent channel has changed 
%% in some way (usually state).
-spec(change_agent_channel/3 :: (Chanid :: pid(), Statename :: atom(),
Statedata :: any()) -> 'ok').
change_agent_channel(Chanid, Statename, Statedata) ->
	gen_event:notify(?MODULE, {change_agent_channel, Chanid, Statename, Statedata}).

%% @doc Purge all state information about an agent from mnesia, both 
%% idleness and channel data.
-spec(truncate/1 :: (AgentId :: string()) -> 'ok').
truncate(AgentId) ->
	Transfun = fun() ->
		AgentQH = qlc:q([X || #agent_state{id = Agent} = X <- mnesia:table(agent_state), Agent =:= AgentId]),
		ChannelQH = qlc:q([X || #agent_channel_state{agent_id = Agent} = X <- mnesia:table(agent_channel_state), Agent =:= AgentId]),
		Agents = qlc:e(AgentQH),
		Channels = qlc:e(ChannelQH),
		[mnesia:delete_object(X) || X <- Agents],
		[mnesia:delete_object(X) || X <- Channels],
		{Agents, Channels}
	end,
	case mnesia:transaction(Transfun) of
		{atomic, {Agents, Channels}} ->
			signal_cpx_monitor_truncate(Agents, Channels);
		{aborted, Else} ->
			?WARNING("Could not truncate:  ~p", [Else])
	end.

%% =====
%% gen_event callbacks
%% =====

%% -----
%% init
%% -----

%% @private
init([Agent, ChannelId, Statename, Statedata]) when is_pid(ChannelId) ->
	Now = util:now(),
	AgentPid = Agent#agent.source,
	Chanstate = #agent_channel_state{
		agent_id = Agent#agent.id,
		id = ChannelId,
		oldstate = init,
		state = Statename,
		statedata = Statedata,
		start = Now
	},
	TransactFun = fun() ->
		mnesia:dirty_write(Chanstate),
		cpx_monitor:info({agent_channel_state, Chanstate}),
		ok
	end,
	case mnesia:async_dirty(TransactFun) of
		ok ->
			{ok, {AgentPid, ChannelId}};
		Else ->
			?WARNING("Storing initial agent channel event for ~p failed", [ChannelId]),
			{ok, {AgentPid, ChannelId}}
	end;

init(Agent) when is_record(Agent, agent) ->
	#agent{login = Agentname, skills = Skills, release_data = Statedata,
		profile = Profile, id = Id} = Agent,
	State = case Statedata of
		{_, _, _} -> released;
		_ -> idle
	end,
	TransactFun = fun() ->
		Now = util:now(),
		Login = #agent_state{
			id = Id, 
			agent = Agentname, 
			oldstate = login, 
			state = State,
			statedata = Skills,
			start = Now, 
			ended = Now, 
			profile= Profile
		},
		StateRow = #agent_state{
			id = Id,
			agent = Agentname,
			oldstate = State,
			statedata = Statedata,
			start = Now,
			profile = Profile
		},
		mnesia:dirty_write(Login),
		mnesia:dirty_write(StateRow),
		cpx_monitor:info({agent_state, Login}),
		cpx_monitor:info({agent_state, StateRow}),
		ok
	end,
	case mnesia:async_dirty(TransactFun) of
		ok ->
			{ok, Agent};
		Res ->
			?WARNING("res of agent state login:  ~p", [Res]),
			{ok, Agent}
	end.

%% -----
%% handle_event
%% -----

%% @private
handle_event({change_profile, #agent{id = Id} = OldAgent, NewAgent},
#agent{id = Id} = CurrentAgent) ->
	#agent{profile = OldProfile, skills = OldSkills} = OldAgent,
	#agent{profile = NewProfile, skills = NewSkills} = NewAgent,
	DroppedSkills = OldSkills -- NewSkills,
	GainedSkills = NewSkills -- OldSkills,
	ProfChangeRec = #agent_profile_change{
		id = NewAgent#agent.id,
		agent = NewAgent#agent.login,
		old_profile = OldProfile,
		new_profile = NewProfile,
		skills = NewSkills,
		dropped_skills = DroppedSkills,
		gained_skills = GainedSkills
	},
	cpx_monitor:info({agent_profile, ProfChangeRec}),
	{ok, NewAgent};
	
handle_event({change_state, #agent{id = Id} = OldAgent, NewAgent},
#agent{id = Id} = CurrentAgent) ->
	#agent{login = Agentname, release_data = Statedata,
		profile = Profile} = NewAgent,
	State = case Statedata of
		{_, _, _} -> released;
		_ -> idle
	end,
	Now = util:now(),
	TransactFun = fun() ->
		terminate_states(Id, Now),
%		QH = qlc:q([Rec ||
%			Rec <- mnesia:table(agent_state),
%			Rec#agent_state.id =:= Id,
%			Rec#agent_state.ended =:= undefined
%		]),
%		Recs = qlc:e(QH),
%		[begin
%			mnesia:delete_object(Untermed),
%			Termed = Untermed#agent_state{ended = Now, timestamp = Now, state = State},
%			cpx_monitor:info({agent_state, Termed}),
%			mnesia:write(Termed)
%		end || Untermed <- Recs],
		Newrec = #agent_state{id = Id, agent = Agentname, oldstate = State,
			statedata = Statedata, profile = Profile, start = Now},
		cpx_monitor:info({agent_state, Newrec}),
		mnesia:write(Newrec),
		ok
	end,
	case mnesia:async_dirty(TransactFun) of
		ok ->
			{ok, NewAgent};
		Res ->
			?WARNING("res of agent ~p state change ~p log:  ~p", [Id, State, Res]),
			{ok, NewAgent}
	end;

handle_event({change_agent, #agent{id = Id, profile = Profile} = OldAgent,
#agent{id = Id, profile = Profile} = NewAgent},
#agent{id = Id} = CurAgent) ->
	% most likely a state change.
	handle_event({change_state, OldAgent, NewAgent}, CurAgent);

handle_event({change_agent, Old, New}, Cur) ->
	handle_event({change_profile, Old, New}, Cur);

handle_event({change_agent_channel, ChanId, Statename, Statedata},
{_, ChanId} = State) ->
	Now = util:now(),
	TransactFun = fun() ->
		BaseRec = terminate_states(ChanId, Now),
%		QH = qlc:q([X ||
%			X <- mnesia:table(agent_channel_state),
%			X#agent_channel_state.id =:= ChanId,
%			X#agent_channel_state.ended =:= undefined
%		]),
%		[BaseRec | _] = Recs = qlc:e(QH),
%		[begin
%			mnesia:delete_object(Untermed),
%			Termed = Untermed#agent_channel_state{ended = Now, timestamp = Now},
%			cpx_monitor:info({agent_channel_state, Termed}),
%			mnesia:write(Termed)
%		end || Untermed <- Recs],
		#agent_channel_state{state = Oldstate} = BaseRec,
		Newrec = BaseRec#agent_channel_state{oldstate = Oldstate,
			state = Statename, statedata = Statedata, start = Now,
			ended = undefined, timestamp = Now
		},
		cpx_monitor:info({agent_channel_state, Newrec}),
		mnesia:write(Newrec),
		ok
	end,
	case mnesia:async_dirty(TransactFun) of
		ok ->
			{ok, State};
		Res ->
			?WARNING("res of the agent channel ~p state change ~p:~p log: ~p", [ChanId, Statename, Statedata, Res]),
			{ok, State}
	end;

% ignore any event we can't handle.
handle_event(Event, State) ->
	{ok, State}.

%% -----
%% handle_call
%% -----

%% @private
handle_call(Request, State) ->
	{ok, invalid, State}.

%% -----
%% handle_info
%% -----

%% @private

handle_info(Info, State) ->
	?DEBUG("Some info:  ~p", [Info]),
	{ok, State}.

%% ----
%% terminate
%% -----

%% @private
terminate({stop, Reason}, {_AgentId, ChanPid}) ->
	?DEBUG("Seems the channel ~p stopped:  ~p", [ChanPid, Reason]),
	Now = util:now(),
	Transfun = fun() ->
		BaseRec = terminate_states(ChanPid, Now),
		#agent_channel_state{state = Oldstate} = BaseRec,
		Newrec = BaseRec#agent_channel_state{oldstate = Oldstate,
			state = exit, start = Now, ended = Now, timestamp = Now,
			statedata = Reason},
		cpx_monitor:info({agent_channel_state, Newrec}),
		mnesia:write(Newrec),
		ok
	end,
	mnesia:async_dirty(Transfun);

terminate({stop, Reason}, #agent{id = AgentId} = Agent) ->
	?DEBUG("Agent ~s stopped:  ~p", [Agent#agent.login, Reason]),
	Now = util:now(),
	Transfun = fun() ->
		Basestate = terminate_states(AgentId, Now),
		Newrec = #agent_state{id = AgentId, agent = Agent#agent.login,
			oldstate = Basestate#agent_state.state, state = logout,
			statedata = Reason, start = Now, ended = Now, timestamp = Now},
		mnesia:write(Newrec),
		ok
	end,
	mnesia:async_dirty(Transfun);

terminate(Why, State) ->
	?INFO("Some other exit:  %p", [Why]),
	ok.

%% -----
%% code_change
%% -----

%% @private
code_change(OldVsn, State, Extra) ->
	{ok, State}.

%% =====
%% Internal
%% =====

terminate_states(ChanId, Now) when is_pid(ChanId) ->
	QH = qlc:q([X ||
		X <- mnesia:table(agent_channel_state),
		X#agent_channel_state.id =:= ChanId,
		X#agent_channel_state.ended =:= undefined
	]),
	[BaseRec | _] = Recs = qlc:e(QH),
	[begin
		mnesia:delete_object(Untermed),
		Termed = Untermed#agent_channel_state{ended = Now, timestamp = Now},
		cpx_monitor:info({agent_channel_state, Termed}),
		mnesia:write(Termed)
	end || Untermed <- Recs],
	BaseRec;

terminate_states(Id, Now) when is_list(Id) ->
	QH = qlc:q([Rec ||
		Rec <- mnesia:table(agent_state),
		Rec#agent_state.id =:= Id,
		Rec#agent_state.ended =:= undefined
	]),
	[Base | _] = Recs = qlc:e(QH),
	[begin
		mnesia:delete_object(Untermed),
		Termed = Untermed#agent_state{ended = Now, timestamp = Now, state = login},
		cpx_monitor:info({agent_state, Termed}),
		mnesia:write(Termed)
	end || Untermed <- Recs],
	Base.

%% -----

build_tables(Nodes) ->
	Agent = util:build_table(agent_state, [
		{attributes, record_info(fields, agent_state)},
		{disc_copies, Nodes},
		{type, bag},
		{index, [agent, profile]}
	]),
	Channel = util:build_table(agent_channel_state, [
		{attributes, record_info(fields, agent_channel_state)},
		{disc_copies, Nodes},
		{type, bag},
		{index, [id]}
	]),
	Successes = [exists, copied, {atomic, ok}],
	case {lists:member(Agent, Successes), lists:member(Channel, Successes)} of
		{true, true} -> ok;
		_ ->
			?WARNING("One of the tables didn't build.  Agent:  ~p;  Channel:  ~p", [Agent, Channel]),
			{Agent, Channel}
	end.

%% -----
signal_cpx_monitor_truncate(Agents, Channels) ->
	truncate_agents(Agents),
	truncate_channels(Channels).
%% -----
truncate_agents([]) ->
	ok;

truncate_agents([#agent_state{ended = undefined} = A | Tail]) ->
	Now = util:now(),
	NewA = A#agent_state{ended = Now, state = logoout},
	cpx_monitor:info({agent_state, NewA}),
	truncate_agents(Tail);

truncate_agents([_|Tail]) ->
	truncate_agents(Tail).
%% -----
truncate_channels([]) ->
	ok;

truncate_channels([#agent_channel_state{ended = undefined} = A | Tail]) ->
	Now = util:now(),
	NewA = A#agent_channel_state{ended = Now},
	ExitA = A#agent_channel_state{start = Now, oldstate = A#agent_channel_state.state, state = 'exit', statedata = undefined},
	cpx_monitor:info({agent_channel_state, NewA}),
	cpx_monitor:info({agent_channel_state, ExitA});

truncate_channels([_|Tail]) ->
	truncate_channels(Tail).

-ifdef(TEST).
%% =====
%% Test
%% =====

% agent tests:
% agent init produces 2 events:  login, and current state
% state change produces 2 events, termination of old event, and new
% logout/exit produces N events where N is states unterminated
% 	new state is logout

%% -----
%% utility functions
%% -----

setup_mnesia() ->
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	mnesia:start(),
	build_tables([node()]).

setup_node(Nodename) ->
	util:start_testnode(),
	util:start_testnode(Nodename).

agent_init(CpxMonPid, Agent) ->
	gen_leader_mock:expect_leader_cast(CpxMonPid, fun(_, _, _) -> ok end),
	gen_leader_mock:expect_leader_cast(CpxMonPid, fun(_,_,_) -> ok end),
	init(Agent).

agent_channel_init(CpxMonPid, Agent, ChanId, Statename, Statedata) ->
	gen_leader_mock:expect_leader_cast(CpxMonPid, fun(_,_,_) -> ok end),
	init([Agent, ChanId, Statename, Statedata]).

%% -----
%% Tests for handling agent changes
%% -----

agent_test_() ->
	Node = setup_node(agent_init_test_node),
	{spawn, Node, {foreach,
	fun() ->
		setup_mnesia(),
		case gen_leader_mock:start(cpx_monitor) of
			{ok, O} -> O;
			{error, {already_started, O}} -> O
		end
	end,

%% -----

	[fun(CpxMonPid) ->
		{"simple initialize", fun() ->
			gen_leader_mock:expect_leader_cast(CpxMonPid, fun({info, _Ts, {agent_state, Info}}, _State, _Elect) ->
				#agent_state{id = Id, oldstate = Old, state = Current,
					profile = Profile, start = Start, ended = End} = Info,
				?assertEqual("testagent", Id),
				?assertEqual(login, Old),
				?assertEqual(default, Current),
				?assertEqual("testprofile", Profile),
				?assertEqual(Start, End),
				ok
			end),
			gen_leader_mock:expect_leader_cast(CpxMonPid, fun({info, _Ts, {agent_state, Info}}, _State, _Elect) ->
				#agent_state{id = Id, oldstate = Old, state = Current,
					profile = Profile, start = Start, ended = End} = Info,
				?assertEqual("testagent", Id),
				?assertEqual(default, Old),
				?assertEqual(undefined, Current),
				?assertEqual("testprofile", Profile),
				?assertNot(undefined =:= Start),
				?assertEqual(undefined, End),
				ok
			end),
			Agent = #agent{login = "testagent", id="testagent",
				profile = "testprofile", release_data = default},
			Out = init(Agent),
			?assertEqual({ok, Agent}, Out),
			cpx_monitor:assert_mock()
		end}
	end,

%% -----

	fun(CpxMonPid) ->
		{"simple state change", fun() ->
			StateAgent = #agent{login = "testagent", id = "testagent",
				profile = "testprofile", release_data = default},
			{ok, State} = agent_init(CpxMonPid, StateAgent),
			OldAgent = StateAgent,
			NewReleaseData = {"rid", "rlabel", 0},
			NewAgent = OldAgent#agent{release_data = NewReleaseData},
			gen_leader_mock:expect_leader_cast(CpxMonPid, fun({info, _Ts, {agent_state, Info}}, _State, _Elect) ->
				?assert(is_record(Info, agent_state)),
				#agent_state{state = NewRelease, oldstate = OldRelease,
					ended = Ended} = Info,
				?assertEqual(NewReleaseData, NewRelease),
				?assertEqual(default, OldRelease),
				?assertNot(Ended == undefined),
				ok
			end),
			gen_leader_mock:expect_leader_cast(CpxMonPid, fun({info, _Ts, {agent_state, Info}}, _State, _Elect) ->
				?assert(is_record(Info, agent_state)),
				#agent_state{state = NewRelease, oldstate = OldRelease,
					ended = Ended} = Info,
				?assertEqual(undefined, NewRelease),
				?assertEqual(undefined, Ended),
				?assertEqual(NewReleaseData, OldRelease),
				ok
			end),
			Out = handle_event({change_state, OldAgent, NewAgent}, State),
			?assertEqual({ok, NewAgent}, Out),
			cpx_monitor:assert_mock()
		end}
	end,

%% -----

	fun(CpxMonPid) ->
		{"simple profile change", fun() ->
			OldSkills = ['english', 'myspace', 'yahoo', 'pub'],
			NewSkills = ['english', 'facebook', 'google', 'bar'],
			StateAgent = #agent{login = "testagent", id = "testagent",
				profile = "testprofile", skills = OldSkills, release_data = default},
			{ok, State} = agent_init(CpxMonPid, StateAgent),
			OldAgent = StateAgent,
			NewAgent = OldAgent#agent{skills = NewSkills, profile = "newprofile"},
			ExpectDropped = ['myspace', 'yahoo', 'pub'],
			ExpectGained = ['facebook', 'google', 'bar'],
			ExpectSkills = ['english', 'facebook', 'google', 'bar'],
			gen_leader_mock:expect_leader_cast(CpxMonPid, fun({info, _Ts, {agent_profile, ProfileChange}}, _State, _Elec) ->
				#agent_profile_change{old_profile = OP, new_profile = NP,
					skills = S, dropped_skills = DS, gained_skills = GS, id = Id} = 						ProfileChange,
				?assertEqual("testprofile", OP),
				?assertEqual("newprofile", NP),
				?assertEqual(ExpectSkills, S),
				?assertEqual(ExpectDropped, DS),
				?assertEqual(ExpectGained, GS),
				?assertEqual("testagent", Id),
				ok
			end),
			Out = handle_event({change_profile, OldAgent, NewAgent}, State),
			?assertEqual({ok, NewAgent}, Out),
			cpx_monitor:assert_mock()
		end}
	end,

%% -----

	fun(CpxMonPid) ->
		{"auto detect change: profile", fun() ->
			OldSkills = ['english', 'myspace', 'yahoo', 'pub'],
			NewSkills = ['english', 'facebook', 'google', 'bar'],
			StateAgent = #agent{login = "testagent", id = "testagent",
				profile = "testprofile", skills = OldSkills, release_data = default},
			{ok, State} = agent_init(CpxMonPid, StateAgent),
			OldAgent = StateAgent,
			NewAgent = OldAgent#agent{skills = NewSkills, profile = "newprofile"},
			ExpectDropped = ['myspace', 'yahoo', 'pub'],
			ExpectGained = ['facebook', 'google', 'bar'],
			ExpectSkills = ['english', 'facebook', 'google', 'bar'],
			gen_leader_mock:expect_leader_cast(CpxMonPid, fun({info, _Ts, {agent_profile, ProfileChange}}, _State, _Elec) ->
				#agent_profile_change{old_profile = OP, new_profile = NP,
					skills = S, dropped_skills = DS, gained_skills = GS, id = Id} = 						ProfileChange,
				?assertEqual("testprofile", OP),
				?assertEqual("newprofile", NP),
				?assertEqual(ExpectSkills, S),
				?assertEqual(ExpectDropped, DS),
				?assertEqual(ExpectGained, GS),
				?assertEqual("testagent", Id),
				ok
			end),
			Out = handle_event({change_agent, OldAgent, NewAgent}, State),
			?assertEqual({ok, NewAgent}, Out),
			cpx_monitor:assert_mock()
		end}
	end,

%% -----

	fun(CpxMonPid) ->
		{"auto detect change:  state", fun() ->
			StateAgent = #agent{login = "testagent", id = "testagent",
				profile = "testprofile", release_data = default},
			{ok, State} = agent_init(CpxMonPid, StateAgent),
			OldAgent = StateAgent,
			NewReleaseData = {"rid", "rlabel", 0},
			NewAgent = OldAgent#agent{release_data = NewReleaseData},
			gen_leader_mock:expect_leader_cast(CpxMonPid, fun({info, _Ts, {agent_state, Info}}, _State, _Elect) ->
				?assert(is_record(Info, agent_state)),
				#agent_state{state = NewRelease, oldstate = OldRelease,
					ended = Ended} = Info,
				?assertEqual(NewReleaseData, NewRelease),
				?assertEqual(default, OldRelease),
				?assertNot(Ended == undefined),
				ok
			end),
			gen_leader_mock:expect_leader_cast(CpxMonPid, fun({info, _Ts, {agent_state, Info}}, _State, _Elect) ->
				?assert(is_record(Info, agent_state)),
				#agent_state{state = NewRelease, oldstate = OldRelease,
					ended = Ended} = Info,
				?assertEqual(undefined, NewRelease),
				?assertEqual(undefined, Ended),
				?assertEqual(NewReleaseData, OldRelease),
				ok
			end),
			Out = handle_event({change_agent, OldAgent, NewAgent}, State),
			?assertEqual({ok, NewAgent}, Out),
			cpx_monitor:assert_mock()
		end}
	end]}}.

%% -----
%% agent channel state tests
%% -----

% agent channel tests:
% agent channel init produces 1 event: current state
% agent channel state change produces 2 events:  terminate of old and
%		creation of new
% agent channel state exit produces N events, where N is unterminated.
%		state would be exit.

agent_channel_test_() ->
	Node = setup_node(agent_channel_init_test_node),
	{spawn, Node, {foreach,
	fun() ->
		setup_mnesia(),
		case gen_leader_mock:start(cpx_monitor) of
			{ok, O} -> O;
			{error, {already_started, O}} -> O
		end
	end,

%% -----

	[fun(CpxMonPid) ->
		{"simple initialize", fun() ->
			ChannelRef = spawn(fun() -> ok end),
			Agent = #agent{login = "testagent", id = "testagent", source = "agent_pid"},
			gen_leader_mock:expect_leader_cast(CpxMonPid, fun({info, _Ts, {agent_channel_state, Info}}, _State, _Elect) ->
				#agent_channel_state{agent_id = Aid, id = Cid, oldstate = OState,
					state = State, start = Started, ended = Ended} = Info,
				?assertEqual("testagent", Aid),
				?assertEqual(ChannelRef, Cid),
				?assertEqual(init, OState),
				?assertEqual(prering, State),
				?assertNot(Started == undefined),
				?assertEqual(undefined, Ended),
				ok
			end),
			Out = init([Agent, ChannelRef, prering, call_rec]),
			?assertMatch({ok, {"agent_pid", ChannelRef}}, Out),
			cpx_monitor:assert_mock()
		end}
	end,

%% -----

	fun(CpxMonPid) ->
		{"state change", fun() ->
			ChannelRef = spawn(fun() -> ok end),
			Agent = #agent{login = "testagent", id = "testid",
				source = "agent_pid"},
			{ok, State0} = agent_channel_init(CpxMonPid, Agent, ChannelRef, prering, "call_data"),
			gen_leader_mock:expect_leader_cast(CpxMonPid, fun({info, _Ts, {agent_channel_state, Info}}, _State, _Elect) ->
				#agent_channel_state{agent_id = Aid, id = Cid, oldstate = OState,
					state = NState, start = Started, ended = Ended,
					statedata = Statedata} = Info,
				?assertEqual("testid", Aid),
				?assertEqual(ChannelRef, Cid),
				?assertEqual(init, OState),
				?assertEqual(prering, NState),
				?assertEqual("call_data", Statedata),
				?assertNot(Started == undefined),
				?assertNot(Ended == undefined),
				ok
			end),
			gen_leader_mock:expect_leader_cast(CpxMonPid, fun({info, _Ts, {agent_channel_state, Info}}, _State, _Elect) ->
				#agent_channel_state{agent_id = Aid, id = Cid, oldstate = OState,
					state = NState, start = Started, ended = Ended,
					statedata = Statedata} = Info,
				?assertEqual("testid", Aid),
				?assertEqual(ChannelRef, Cid),
				?assertEqual(prering, OState),
				?assertEqual(ringing, NState),
				?assertEqual("new_call_data", Statedata),
				?assertNot(Started == undefined),
				?assertEqual(undefined, Ended),
				ok
			end),
			Out = handle_event({change_agent_channel, ChannelRef, ringing, "new_call_data"}, State0),
			?assertEqual({ok, State0}, Out),
			cpx_monitor:assert_mock()
		end}
	end
	]}}.

-endif.
