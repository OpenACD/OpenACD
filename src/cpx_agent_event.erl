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
	agent_init/1,
	agent_channel_init/2,
	change_profile/2,
	change_state/2,
	change_agent/2
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
				ok = gen_event:add_handler(?MODULE, {?MODULE, Id}, Agent);
			_ ->
				?INFO("Agent Event already initialized for ~s", [Agent])
		end
	end catch
		What:Why ->
			?ERROR("Initializing Agent Event Failed due to ~p:~p", [What,Why]),
			{What, Why}
	end.

%% @doc Create a handler specifically for the given agent channel.
-spec(agent_channel_init/2 :: (Agent :: string(), ChannelId :: reference()) -> 'ok' | {atom(), any()}).
agent_channel_init(Agent, ChannelId) ->
	try begin
		Handlers = gen_event:which_handlers(?MODULE),
		Member = lists:member({?MODULE, ChannelId}, Handlers),
		case Member of
			false ->
				ok = gen_event:add_handler(?MODULE, {?MODULE, ChannelId}, [Agent, ChannelId]);
			_ ->
				?INFO("Agent channel ~p already has event initialized", [ChannelId])
		end
	end catch
		What:Why ->
			?ERROR("Initialization failed for channel id ~s:~p due to ~p:~p", [Agent, ChannelId, What, Why]),
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
	
%% =====
%% gen_event callbacks
%% =====

%% -----
%% init
%% -----

%% @private
init([Agent, ChannelId]) when is_reference(ChannelId) ->
	{ok, {Agent, ChannelId}};

init(Agent) when is_record(Agent, agent) ->
	#agent{login = Agentname, skills = Skills, release_data = State,
		profile = Profile, id = Id} = Agent,
	Statedata = State,
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
	#agent{login = Agentname, release_data = State,
		profile = Profile} = NewAgent,
	Statedata = State,
	TransactFun = fun() ->
		Now = util:now(),
		QH = qlc:q([Rec ||
			Rec <- mnesia:table(agent_state),
			Rec#agent_state.id =:= Id,
			Rec#agent_state.ended =:= undefined
		]),
		Recs = qlc:e(QH),
		[begin
			mnesia:delete_object(Untermed),
			Termed = Untermed#agent_state{ended = Now, timestamp = Now, state = State},
			cpx_monitor:info({agent_state, Termed}),
			mnesia:write(Termed)
		end || Untermed <- Recs],
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

handle_event({detect_change, #agent{id = Id, profile = Profile} = OldAgent,
#agent{id = Id, profile = Profile} = NewAgent},
#agent{id = Id} = CurAgent) ->
	% most likely a state change.
	handle_event({change_state, OldAgent, NewAgent}, CurAgent);

handle_event({detect_change, Old, New}, Cur) ->
	handle_event({change_profile, Old, New}, Cur);

% ignore any event we can't handle.
handle_event(Event, State) ->
	?INFO("Unhandled event:  ~p", [Event]),
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
handle_info({'EXIT', Apid, Reason}, #agent{source = Apid} = Agent) ->
	#agent{id = Id} = Agent,
	TransactFun = fun() ->
		Now = util:now(),
		QH = qlc:q([Rec || Rec <- mnesia:table(agent_state), Rec#agent_state.id =:= Id, Rec#agent_state.ended =:= undefined]),
		Recs = qlc:e(QH),
		?DEBUG("Recs to loop through:  ~p", [Recs]),
		[begin
			mnesia:delete_object(Untermed), 
			Termed = Untermed#agent_state{ended = Now, timestamp = Now, state = logout},
			cpx_monitor:info({agent_state, Termed}),
			mnesia:write(Termed)
		end || Untermed <- Recs],
		%% TODO terminate items for agent_channels
		ok
	end,
	case mnesia:async_dirty(TransactFun) of
		ok ->
			remove_handler;
		Res ->
			?WARNING("res of agent state change log:  ~p", [Res]),
			remove_handler
	end;

% exit of channel


handle_info(Info, State) ->
	{ok, State}.

%% ----
%% terminate
%% -----

%% @private
terminate(Why, State) -> ok.

%% -----
%% code_change
%% -----

%% @private
code_change(OldVsn, State, Extra) ->
	{ok, State}.

%% =====
%% Internal
%% =====

build_tables(Nodes) ->
	Agent = util:build_table(agent_state, [
		{attributes, record_info(fields, agent_state)},
		{disc_copies, Nodes}
	]),
	Channel = util:build_table(agent_channel_state, [
		{attributes, record_info(fields, agent_channel_state)},
		{disc_copies, Nodes}
	]),
	Successes = [exists, copied, {atomic, ok}],
	case {lists:member(Agent, Successes), lists:member(Channel, Successes)} of
		{true, true} -> ok;
		_ ->
			?WARNING("One of the tables didn't build.  Agent:  ~p;  Channel:  ~p", [Agent, Channel]),
			{Agent, Channel}
	end.

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
			Out = handle_event({detect_change, OldAgent, NewAgent}, State),
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
			Out = handle_event({detect_change, OldAgent, NewAgent}, State),
			?assertEqual({ok, NewAgent}, Out),
			cpx_monitor:assert_mock()
		end}
	end

	]}}.

% agent channel tests:
% agent channel init produces 1 event: current state
% agent channel state change produces 2 events:  terminate of old and
%		creation of new
% agent channel state exit produces N events, where N is unterminated.
%		state would be exit.

-endif.
