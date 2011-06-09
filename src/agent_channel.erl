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

%% @doc A gen_fsm for an agent channel.  When an agent is to go ringing
%% for a media, if the agent fsm has a channel available, a new process
%% of this module is started.  Once the agent has gone through the flow,
%% this process can die.
-module(agent_channel).
-behaviour(gen_fsm).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([
	from_idle_tests/0,
	from_ringing_tests/0,
	from_precall_tests/0,
	from_oncall_tests/0,
	from_outgoing_tests/0,
	from_released_tests/0,
	from_warmtransfer_tests/0,
	from_wrapup_tests/0
]).

-endif.

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
%-include_lib("stdlib/include/qlc.hrl").


-record(state, {
	agent_fsm :: pid(),
	agent_connection :: pid(),
	ring_fails = 0 :: non_neg_integer(),
	ringouts = 0 :: non_neg_integer(),
	max_ringouts = infinity :: 'infinity' | pos_integer(),
	ring_locked = 'unlocked' :: 'unlocked' | 'locked',
	state_data :: any()
}).

-type(state() :: #state{}).
-define(GEN_FSM, true).
-include("gen_spec.hrl").

-define(DEFAULT_REL, {"default", default, -1}).
-define(RING_FAIL_REL, {"Ring Fail", ring_fail, -1}).
-define(RING_LOCK_DURATION, 1000). % in ms
-define(WRAPUP_AUTOEND_KEY, autoend_wrapup).
-define(STATE_ATOMS, ['prering', 'ringing', 'precall', 'oncall', 
	'warmtransfer_hold', 'warmtransfer_3rd_party', 'wrapup']).

%% gen_fsm exports
-export([
	init/1,
	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3,
	code_change/4%,
	%format_status/2
]).
%% defined state exports
-export([
	prering/3,
	ringing/3,
	precall/3,
	oncall/3,
	warmtransfer_hold/3,
	warmtransfer_3rd_party/3,
	wrapup/3
]).
%% defining async state exports
-export([
	prering/2,
	ringing/2,
	precall/2,
	oncall/2,
	warmtransfer_hold/2,
	warmtransfer_3rd_party/2,
	wrapup/2
]).

%% api
-export([
	start/2,
	start_link/2,
	stop/1, 
	get_media/1,
	set_state/2, 
	set_state/3, 
	list_to_state/1, 
	set_endpoint/2, 
	set_connection/2,
	agent_transfer/2,
	queue_transfer/2,
	media_call/2,
	media_cast/2,
	media_push/2,
	spy/2,
	has_successful_ring/1,
	has_failed_ring/1
]).

% ======================================================================
% API
% ======================================================================

-type(start_opts() :: [{atom(), any()}]).
%% @doc start an fsm with the given options.
-spec(start/2 :: (AgentRec :: #agent{}, Options :: start_opts()) -> {'ok', pid()}).
start(AgentRec, Options) ->
	gen_fsm:start(?MODULE, [AgentRec, Options], []).

%% @doc Start an fsm linked to the calling process.
-spec(start_link/2 :: (AgentRec :: #agent{}, Options :: start_opts()) -> {'ok', pid()}).
start_link(AgentRec, Options) ->
	gen_fsm:start_link(?MODULE, [AgentRec, Options], []).

%% @doc Stop the passed agent fsm `Pid'.
-spec(stop/1 :: (Pid :: pid()) -> 'ok').
stop(Pid) -> 
	gen_fsm:send_all_state_event(Pid, stop).
	
%% @doc link the given agent  `Pid' to the given connection `Socket'.
-spec(set_connection/2 :: (Pid :: pid(), Socket :: pid()) -> 'ok' | 'error').
set_connection(Pid, Socket) ->
	gen_fsm:sync_send_all_state_event(Pid, {set_connection, Socket}).

-spec(set_endpoint/2 :: (Pid :: pid(), Endpoint :: {endpointtype(), string()}) -> ok).
set_endpoint(Pid, Endpoint) ->
	gen_fsm:sync_send_all_state_event(Pid, {set_endpoint, Endpoint}).

%% @doc The connection can request to call to the agent's media when oncall.
-spec(media_call/2 :: (Apid :: pid(), Request :: any()) -> any()).
media_call(Apid, Request) ->
	gen_fsm:sync_send_event(Apid, {mediacall, Request}).

%% @doc To cast to the media while oncall, use this.
-spec(media_cast/2 :: (Apid :: pid(), Request :: any()) -> 'ok').
media_cast(Apid, Request) ->
	gen_fsm:send_event(Apid, {mediacast, Request}).

%% @doc Returns the #call{} of the current state if there is on, otherwise 
%% returns `invalid'.
-spec(get_media/1 :: (Apid :: pid()) -> {ok, #call{}} | 'invalid').
get_media(Apid) ->
	gen_fsm:sync_send_event(Apid, get_media).

%% @doc Attempt to set the state of agent at `Pid' to `State'.
-spec(set_state/2 :: (Pid :: pid(), State :: atom()) -> 'ok' | 'invalid').
set_state(Pid, State) ->
	gen_fsm:sync_send_event(Pid, State, infinity).

%% @doc Attempt to set the state of the agent at `Pid' to `State' with data `Data'.  `Data' is related to the `State' the agent is going into.  
%% Often `Data' will be `#call{} or a callid of type `string()'.
-spec(set_state/3 :: (Pid :: pid(), State :: 'idle' | 'ringing' | 'precall' | 'oncall' | 'outgoing' | 'warmtransfer' | 'wrapup', Data :: any()) -> 'ok' | 'invalid';
                     (Pid :: pid(), State :: 'released', Data :: any()) -> 'ok' | 'invalid' | 'queued').
set_state(Pid, State, Data) ->
	gen_fsm:sync_send_event(Pid, {State, Data}, infinity).

%% @doc attmept to push data from the media connection to the agent.  It's up to
%% the agent connection to interpret this correctly.
-spec(media_push/2 :: (Pid :: pid(), Data :: any()) -> any()).
media_push(Pid, Data) ->
	S = self(),
	gen_fsm:send_event(Pid, {mediapush, S, Data}).

%% @doc Make the give `pid() Spy' spy on `pid() Target'.
-spec(spy/2 :: (Spy :: pid(), Target :: pid()) -> 'ok' | 'invalid').
spy(Spy, Target) ->
	gen_fsm:sync_send_event(Spy, {spy, Target}).

%% @doc Translate the state `String' into the internally used atom.  `String' can either be the human readable string or a number in string form (`"1"').
-spec(list_to_state/1 :: (String :: string()) -> atom()).
list_to_state(String) ->
	Atom = try erlang:list_existing_atom(String) of
		A -> A
	catch
		error:badarg -> badarg
	end,
	case lists:member(Atom, ?STATE_ATOMS) of
		true -> Atom;
		false -> erlang:error(badarg)
	end.

%% @doc Start the agent_transfer procedure.  Gernally the media will handle it from here.
-spec(agent_transfer/2 :: (Pid :: pid(), Target :: pid()) -> 'ok' | 'invalid').
agent_transfer(Pid, Target) ->
	gen_fsm:sync_send_event(Pid, {agent_transfer, Target}).

%% @doc Start the queue_transfer procedure.  Gernally the media will handle it from here.
-spec(queue_transfer/2 :: (Pid :: pid(), Queue :: string()) -> 'ok' | 'invalid').
queue_transfer(Pid, Queue) ->
	gen_fsm:sync_send_event(Pid, {queue_transfer, Queue}).

%% @doc Inform the agent that it's failed a ring, usually an outbound.
%% Used by gen_media, prolly not anywhere else.
-spec(has_failed_ring/1 :: (Pid :: pid()) -> 'ok').
has_failed_ring(Pid) ->
	MediaPid = self(),
	gen_fsm:send_event(Pid, {failed_ring, MediaPid}).

%% @doc Media saying the ring worked afterall; useful to confirm outband rings.
-spec(has_successful_ring/1 :: (Pid :: pid()) -> 'ok').
has_successful_ring(Pid) ->
	MediaPid = self(),
	gen_fsm:send_event(Pid, {has_successful_ring, MediaPid}).

% ======================================================================
% INIT
% ======================================================================

%% @private
%-spec(init/1 :: (Args :: [#agent{}]) -> {'ok', 'released', #agent{}}).
init([Agent, Options]) when is_record(Agent, agent) ->
	{ok, MaxRingouts} = cpx:get_env(max_ringouts, infinity),
	ProtoState = #state{
		agent_fsm = Agent#agent.source,
		agent_connection = Agent#agent.connection,
		max_ringouts = MaxRingouts
	},
	InitInfo = proplists:get_value(initial_state, Options, {prering, undefined}),
	case InitInfo of
		{prering, Call} when is_record(Call, call); Call =:= undefined ->
			State = ProtoState#state{state_data = Call},
			{ok, prering, State};
		{ringing, Call} when is_record(Call, call) ->
			State = ProtoState#state{state_data = Call},
			{ok, ringing, State};
		{precall, Client} when is_record(Client, client) ->
			State = ProtoState#state{state_data = Client},
			{ok, precall, State};
		_ ->
			{stop, badstate}
	end.
		
% ======================================================================
% PRERING
% ======================================================================

prering({ringing, Call}, From, State) ->
	% TODO check if valid
	{reply, ok, ringing, State#state{state_data = Call}};
prering(Msg, _From, State) ->
	?INFO("Msg ~p not understood", [Msg]),
	{reply, {error, invalid}, prering, State}.

prering(Msg, State) ->
	{next_state, prering, State}.

% ======================================================================
% RINGING
% ======================================================================

ringing({oncall, Call}, From, #state{state_data = Call} = State) ->
	{reply, ok, oncall, State};
ringing(Msg, From, State) ->
	{reply, {error, invalid}, ringing, State}.

ringing(Msg, State) ->
	{next_state, ringing, State}.

% ======================================================================
% PRECALL
% ======================================================================

precall({oncall, #call{client = Client} = Call}, From, #state{state_data = Client} = State) ->
	{reply, ok, oncall, State#state{state_data = Call}};
precall(Msg, From, State) ->
	{reply, {error, invalid}, precall, State}.

precall(Msg, State) ->
	{next_state, precall, State}.

% ======================================================================
% ONCALL
% ======================================================================

oncall(warmtransfer_hold, From, State) ->
	{reply, ok, warmtransfer_hold, State};
oncall({warmtransfer_3rd_party, Data}, From, State) ->
	case oncall(warmtransfer_hold, From, State) of
		{reply, ok, warmtransfer_hold, NewState} ->
			warmtransfer_hold({warmtransfer_3rd_party, Data}, From, NewState);
		Else ->
			Else
		end;
oncall(wrapup, From, #state{state_data = Call} = State) ->
	oncall({wrapup, Call}, From, State);
oncall({wrapup, Call}, From, #state{state_data = Call} = State) ->
	{reply, ok, wrapup, State#state{state_data = Call}};
oncall(Msg, From, State) ->
	{reply, {error, invalid}, oncall, State}.

oncall(Msg, State) ->
	{next_state, oncall, State}.

% ======================================================================
% WARMTRANSFER_HOLD
% ======================================================================

warmtransfer_hold(oncall, From, State) ->
	{reply, ok, oncall, State};
warmtransfer_hold({warmtransfer_3rd_party, Data}, From, #state{state_data = Call} = State) ->
	{reply, ok, warmtransfer_3rd_party, State#state{state_data = {Call, Data}}};
warmtransfer_hold(wrapup, From, State) ->
	{reply, ok, wrapup, State};
warmtransfer_hold(Msg, From, State) ->
	{reply, {error, invalid}, warmtransfer_hold, State}.

warmtransfer_hold(Msg, State) ->
	{next_state, warmtransfer_hold, State}.

% ======================================================================
% WARMTRANSFER_3RD_PARTY
% ======================================================================

warmtransfer_3rd_party(warmtransfer_hold, From, #state{state_data = {Call, _}} = State) ->
	{reply, ok, warmtransfer_hold, State#state{state_data = Call}};
warmtransfer_3rd_party(oncall, From, #state{state_data = {Call, _}} = State) ->
	{reply, ok, oncall, State#state{state_data = Call}};
warmtransfer_3rd_party(wrapup, From, #state{state_data = {Call, _}} = State) ->
	{reply, ok, wrapup, State#state{state_data = Call}};
warmtransfer_3rd_party(Msg, From, State) ->
	{reply, {error, invalid}, State}.

warmtransfer_3rd_party(Msg, State) ->
	{next_state, warmtransfer_3rd_party, State}.

% ======================================================================
% WRAPUP
% ======================================================================

wrapup(stop, From, State) ->
	{stop, normal, ok, State};
wrapup(Msg, From, State) ->
	{reply, ok, wrapup, State}.

wrapup(stop, State) ->
	{stop, normal, State};
wrapup(Msg, State) ->
	{next_state, wrapup, State}.


% ======================================================================
% HANDLE_EVENT
% ======================================================================

handle_event(Event, StateName, State) ->
	{next_state, StateName, State}.

% ======================================================================
% HANDLE_SYNC_EVENT
% ======================================================================

handle_sync_event(query_state, _From, StateName, State) ->
	{reply, {ok, StateName}, StateName, State};
handle_sync_event({set_connection, Pid}, _From, StateName, #state{agent_connection = AgentConn} = State) ->
	gen_server:cast(Pid, {change_state, StateName, State#state.state_data}),
	case cpx_supervisor:get_value(motd) of
		{ok, Motd} ->
			gen_server:cast(Pid, {blab, Motd});
		_ ->
			ok
	end,
	{reply, ok, StateName, State#state{agent_connection = Pid}};
handle_sync_event({url_pop, URL, Name}, _From, StateName, #state{agent_connection = Connection} = State) when is_pid(Connection) ->
	gen_server:cast(Connection, {url_pop, URL, Name}),
	{reply, ok, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
	{reply, ok, StateName, State}.

% ======================================================================
% HANDLE_INFO
% ======================================================================

handle_info(end_wrapup, wrapup, State) ->
	{stop, normal, State};
handle_info(_Info, StateName, State) ->
	{next_state, StateName, State}.

% ======================================================================
% TERMINATE
% ======================================================================

terminate(Reason, StateName, _State) ->
	ok.

% ======================================================================
% CODE_CHANGE
% ======================================================================

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

% ======================================================================
% CODE_CHANGE
% ======================================================================

%format_status(normal, [PDict, State]) ->
%	[{data, [{"State", format_status(terminate, [PDict, State])}]}];
%format_status(terminate, [_PDict, #state{agent_rec = Agent} = _State]) ->
%	% prevent client data from being dumped
%	Newagent = case Agent#agent.statedata of
%		#call{client = Client} = Call when is_record(Call#call.client, client) ->
%			Client = Call#call.client,
%			Agent#agent{statedata = Call#call{client = Client#client{options = []}}};
%		{onhold, #call{client = Client} = Call, calling, ID} when is_record(Client, client) ->
%			Agent#agent{statedata = {onhold, Call#call{client = Client#client{options = []}}, calling, ID}};
%		_ ->
%			Agent
%	end,
%	[Newagent].


% ======================================================================
% INTERNAL FUNCTIONS
% ======================================================================


% ======================================================================
% TESTS
% ======================================================================
	
-ifdef(TEST).

start_arbitrary_state_test() ->
	{ok, Pid} = start(#agent{login = "testagent", state = idle}),
	?assertEqual({ok, idle}, query_state(Pid)),
	agent:stop(Pid).		
	
ring_oncall_mismatch_test() ->
	{_, Pid} = start(#agent{login="testagent"}),
	Goodcall = #call{id="Goodcall", source=self()},
	Badcall = #call{id="Badcall", source=self()},
	application:set_env('OpenACD', ring_manager, freeswitch_media_manager),
	{ok, Fsmmm} = gen_server_mock:named({local, freeswitch_media_manager}),
	gen_server_mock:expect_call(freeswitch_media_manager, fun(_, _, State) ->
		{ok, {ok, spawn(fun() -> ok end)}, State}
	end),
	?assertMatch(ok, set_state(Pid, idle)),
	?assertMatch(ok, set_state(Pid, ringing, Goodcall)),
	?assertMatch(invalid, set_state(Pid, oncall, Badcall)).

expand_magic_skills_test_() ->
	Agent = #agent{login = "testagent", profile = "testprofile", skills = ['_agent', '_node', '_profile', english, {'_brand', "testbrand"}]},
	Newskills = expand_magic_skills(Agent, Agent#agent.skills),
	[?_assert(lists:member({'_agent', "testagent"}, Newskills)),
	?_assert(lists:member({'_node', node()}, Newskills)),
	?_assert(lists:member(english, Newskills)),
	?_assert(lists:member({'_profile', "testprofile"}, Newskills)),
	?_assert(lists:member({'_brand', "testbrand"}, Newskills))].

start_change_test_() ->
	util:start_testnode(),
	NodeNames = [
		from_idle_tests,
		from_ringing_tests,
		from_precall_tests,
		from_oncall_tests,
		from_outgoing_tests,
		from_released_tests,
		from_warmtransfer_tests,
		from_wrapup_tests
	],
	NodesList = [{?MODULE:Name(), util:start_testnode(Name)} || Name <- NodeNames],
	[{setup, {spawn, Node}, fun() -> ok end, Tests} || {Tests, Node} <- NodesList].

from_idle_tests() ->
	{foreach,
	fun() ->
		{ok, Dmock} = gen_server_mock:named({local, dispatch_manager}),
		{ok, Monmock} = cpx_monitor:make_mock(),
		{ok, AMmock} = gen_leader_mock:start(agent_manager),
		{ok, Connmock} = gen_server_mock:new(),
		{ok, Logpid} = gen_server_mock:new(),
		Agent = #agent{id = "testid", login = "testagent", connection = Connmock, log_pid = Logpid},
		Assertmocks = fun() ->
			gen_server_mock:assert_expectations(Dmock),
			%gen_leader_mock:assert_expectations(Monmock),
			cpx_monitor:assert_mock(),
			gen_server_mock:assert_expectations(Connmock),
			gen_server_mock:assert_expectations(Logpid),
			gen_leader_mock:assert_expectations(AMmock),
			ok
		end,
		%?CONSOLE("Test args:  ~p", [[Agent, Dmock, Monmock, Connmock, Logpid, Assertmocks]]),
		{#state{agent_rec = Agent}, AMmock, Dmock, Monmock, Connmock, Assertmocks}
	end,
	fun({_Agent, AMmock, Dmock, Monmock, Connmock, _Assertmocks}) ->
		gen_server_mock:stop(Dmock),
		%gen_leader_mock:stop(Monmock),
		cpx_monitor:stop_mock(),
		gen_server_mock:stop(Connmock),
		gen_leader_mock:stop(AMmock),
		timer:sleep(10), % because the mock dispatch manager isn't dying quickly enough 
		% before the next test runs.
		ok
	end,
	[fun({#state{agent_rec = Agent} = State, AMmock, Dmock, Monmock, Connmock, Assertmocks} = _Testargs) ->
		{"to precall",
		fun() ->
			Client = #client{label = "testclient"},
			Aself = self(),
			gen_server_mock:expect_cast(Dmock, fun({end_avail, Self}, _State) ->
				Self = Aself,
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_cast(Connmock, fun({change_state, precall, Inclient}, _State) -> 
				Inclient = Client,
				ok
			end),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", precall, idle, _Client}, _State) -> ok end),
			gen_leader_mock:expect_cast(AMmock, fun({end_avail, Nom}, _State, _Elec) ->
				Nom = Agent#agent.login,
				ok
			end),
			?assertMatch({reply, ok, precall, _State}, idle({precall, Client}, {"ref", "pid"}, State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, AMmock, _Dmock, Monmock, Connmock, Assertmocks} = _Testargs) ->
		{"to ringing",
		fun() ->
			{ok, Fsmmm} = gen_server_mock:named({local, freeswitch_media_manager}),
			application:set_env('OpenACD', ring_manager, freeswitch_media_manager),
			gen_server_mock:expect_call(Fsmmm, fun({ring, {undefined, transient, sip_registration}, _EndPointData, _Callback, _Options}, _From, State) ->
				{ok, {ok, spawn(fun() -> ok end)}, State}
			end),
			Self = self(),
			Call = #call{
				id = "testcall",
				source = Self
			},
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_cast(Connmock, fun({change_state, ringing, Incall}, _State) -> 
				Call = Incall,
				ok
			end),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", ringing, idle, _Call}, _State) -> ok end),
			gen_leader_mock:expect_cast(AMmock, fun({end_avail, Nom}, _State, _Elec) ->
				Nom = Agent#agent.login,
				ok
			end),
			?assertMatch({reply, ok, ringing, _State}, idle({ringing, Call}, "from", State)),
			gen_server_mock:assert_expectations(whereis(freeswitch_media_manager)),
			Assertmocks(),
			gen_server_mock:stop(whereis(freeswitch_media_manager))
		end}
	end,
	fun({Seedstate, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks} = _Testargs) ->
		{"to ringing with a ring lock in effect",
		fun() ->
			Self = self(),
			Call = #call{
				id = "testcall",
				source = Self
			},
			State = Seedstate#state{ring_locked = locked},
			?assertEqual({reply, invalid, idle, State}, idle({ringing, Call}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = SeedAgent} = Seedstate, AMmock, _Dmmock, _Monmock, Connmock, Assertmocks}) ->
		{"to ringing with down perisistant ring",
		fun() ->
			#state{agent_rec = Agent} = State = Seedstate#state{agent_rec = SeedAgent#agent{endpointtype = {undefined, persistant, sip_registration}}}, 
			Self = self(),
			Call = #call{
				id = "testcall",
				source = Self
			},
			?assertMatch({reply, invalid, idle, _State}, idle({ringing, Call}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to released with invalid format",
		fun() ->
			?assertMatch({reply, invalid, idle, _State}, idle({released, "not a valid data"}, "from", Agent)), Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, AMmock, Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to released with default",
		fun() ->
			Self = self(),
			gen_server_mock:expect_cast(Dmock, fun({end_avail, Apid}, _State) ->
				Self = Apid,
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_cast(Connmock, fun({change_state, released, ?DEFAULT_REL}, _State) ->
				ok
			end),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", released, idle, ?DEFAULT_REL}, _State) -> ok end),
			gen_leader_mock:expect_cast(AMmock, fun({end_avail, Nom}, _State, _Elec) ->
				Nom = Agent#agent.login,
				ok
			end),
			?assertMatch({reply, ok, released, _State}, idle({released, default}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, AMmock, Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to released",
		fun() ->
			Self = self(),
			gen_server_mock:expect_cast(Dmock, fun({end_avail, Apid}, _State) ->
				Self = Apid,
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_cast(Connmock, fun({change_state, released, {"id", "just 'cause", 0}}, _State) ->
				ok
			end),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", released, idle, {"id", "just 'cause", 0}}, _State) -> ok end),
			gen_leader_mock:expect_cast(AMmock, fun({end_avail, Nom}, _State, _Elec) ->
				Nom = Agent#agent.login,
				ok
			end),
			?assertMatch({reply, ok, released, _State}, idle({released, {"id", "just 'cause", 0}}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to oncall",
		fun() ->
			?assertMatch({reply, invalid, idle, _State}, idle({oncall, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to ringing with invalid call",
		fun() ->
			?assertMatch({reply, invalid, idle, _State}, idle({ringing, "not a call rec"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to outgoing",
		fun() ->
			?assertMatch({reply, invalid, idle, _State}, idle({outgoing, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to warmtransfer",
		fun() ->
			?assertMatch({reply, invalid, idle, _State}, idle({warmtransfer, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to wrapup",
		fun() ->
			?assertMatch({reply, invalid, idle, _State}, idle({wrapup, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end]}.

from_ringing_tests() ->
	{foreach,
	fun() ->
		{ok, Dmock} = gen_server_mock:named({local, dispatch_manager}),
		%gen_leader_mock:expect_leader_cast(Monmock, fun({set, _}, _, _) -> ok end),
		{ok, AMmock} = gen_leader_mock:start(agent_manager),
		{ok, Connmock} = gen_server_mock:new(),
		{ok, Mpid} = dummy_media:start([{id, "testcall"}, {queues, none}]),
		{ok, Logpid} = gen_server_mock:new(),
		ProtoCallrec = gen_media:get_call(Mpid),
		exit(Mpid, kill),
		{ok, Mediamock} = gen_server_mock:new(),
		Callrec = ProtoCallrec#call{source = Mediamock},
		Agent = #agent{id = "testid", login = "testagent", connection = Connmock, statedata = Callrec, state = ringing, log_pid = Logpid},
		{ok, Monmock} = cpx_monitor:make_mock(),
		Assertmocks = fun() ->
			gen_server_mock:assert_expectations(Dmock),
			%gen_leader_mock:assert_expectations(Monmock),
			cpx_monitor:assert_mock(),
			gen_server_mock:assert_expectations(Connmock),
			gen_server_mock:assert_expectations(Logpid),
			ok
		end,
		{#state{agent_rec = Agent}, AMmock, Dmock, Monmock, Connmock, Assertmocks}
	end,
	fun({#state{agent_rec = #agent{statedata = Callrec}}, AMmock, Dmock, Monmock, Connmock, _Assertmocks}) ->
		gen_server_mock:stop(Dmock),
		%gen_leader_mock:stop(Monmock),
		cpx_monitor:stop_mock(),
		gen_server_mock:stop(Connmock),
		gen_server_mock:stop(Callrec#call.source),
		gen_leader_mock:stop(AMmock),
		timer:sleep(10), % because the mock dispatch manager isn't dying quickly enough 
		% before the next test runs.
		ok
	end,
	[fun({#state{agent_rec = Agent} = State, AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to idle",
		fun() ->
			%Aself = self(),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_cast(Connmock, fun({change_state, idle}, _State) ->
				ok
			end),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", idle, ringing, {}}, _State) -> ok end),
			gen_leader_mock:expect_cast(AMmock, fun({now_avail, Nom}, _State, _Elec) ->
				Nom = Agent#agent.login,
				ok
			end),
			?assertMatch({reply, ok, idle, _State}, ringing(idle, "from", State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to idle with a ring lock between rings",
		fun() ->
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_cast(Connmock, fun({change_state, idle}, _State) ->
				ok
			end),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", idle, ringing, {}}, _State) -> ok end),
			gen_leader_mock:expect_cast(AMmock, fun({now_avail, Nom}, _State, _Elec) ->
				Nom = Agent#agent.login,
				ok
			end),
			application:set_env('OpenACD', agent_ringout_lock, 10),
			{reply, ok, idle, #state{ringouts = Ringouts, ring_locked = Newlocked} = Newstate} = ringing(idle, "from", State),
			?assertEqual(locked, Newlocked),
			?assertEqual(1, Ringouts),
			receive
				ring_unlock ->
					ok
			after 15 ->
				?assert("ring_unlock on recieved")
			end,
			Assertmocks(),
			application:set_env('OpenACD', agent_ringout_lock, 0)
	 end}
	end,
	fun({#state{agent_rec = Agent} = InState, AMmock, _Dmock, _Monmock, Connmock, Assertmocks}) ->
		{"to idle while waiting for an outband ring response",
		fun() ->
			State = InState#state{ring_locked = wait},
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_cast(Connmock, fun({change_state, idle}, _State) ->
				ok
			end),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", idle, ringing, {}}, _State) -> ok end),
			gen_leader_mock:expect_cast(AMmock, fun({now_avail, Nom}, _State, _Elec) ->
				Nom = Agent#agent.login,
				ok
			end),
			{reply, ok, idle, #state{ringouts = Ringouts, ring_locked = Newlocked} = Newstate} = ringing(idle, "from", State),
			?assertEqual(unlocked, Newlocked),
			?assertEqual(1, Ringouts),
			Assertmocks()
	 end}
	end,
	fun({State, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"max_ringouts met/exceeded", fun() ->
			TestState = State#state{max_ringouts = 3, ringouts = 3},
			Out = ringing(idle, "from", TestState),
			?assertEqual({stop, max_ringouts, TestState}, Out),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to ringing",
		fun() ->
			?assertMatch({reply, invalid, ringing, _State}, ringing({ringing, "doens't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to precall",
		fun() ->
			?assertMatch({reply, invalid, ringing, _State}, ringing({precall, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, AMmock, Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to oncall with inband media path",
		fun() ->
			Callrec = Agent#agent.statedata, 
			gen_server_mock:expect_cast(Dmock, fun({end_avail, _Apid}, _State) -> ok end),
			gen_leader_mock:expect_cast(AMmock, fun({end_avail, _Aglogin}, _State, _Elec) -> ok end),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_cast(Connmock, fun({change_state, oncall, Inrec}, _State) ->
				Inrec = Agent#agent.statedata,
				ok
			end),
			gen_server_mock:expect_call(Callrec#call.source, fun(_Message, _From, _State) ->
				ok
			end),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", oncall, ringing, _Callrec}, _State) -> ok end),
			Out = ringing(oncall, "from", State),
			?assertMatch({reply, ok, oncall, _State}, Out),
			Assertmocks(),
			NewState = element(4, Out),
			?assertEqual(0, NewState#state.ringouts)
		end}
	end,
	fun({#state{agent_rec = Agent} = State, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to oncall with failing inband media",
		fun() ->
			Callrec = Agent#agent.statedata,
			gen_server_mock:expect_call(Callrec#call.source, fun(_Message, _From, State) ->
				{ok, invalid, State}
			end),
			?assertMatch({reply, invalid, ringing, _State}, ringing(oncall, "from", State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Oldagent} = State, AMmock, Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to oncall with outband media path",
		fun() ->
			Oldcall = Oldagent#agent.statedata, 
			Callrec = Oldcall#call{ring_path = outband, media_path = outband},
			Agent = Oldagent#agent{statedata = Callrec},
			gen_server_mock:expect_cast(Dmock, fun({end_avail, _Apid}, _State) -> ok end),
			gen_leader_mock:expect_cast(AMmock, fun({end_avail, _Nom}, _State, _Elec) -> ok end),
			gen_server_mock:expect_cast(Connmock, fun({change_state, oncall, Inrec}, _State) ->
				Inrec = Agent#agent.statedata,
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", oncall, ringing, _Callrec}, _State) -> ok end),
			Out = ringing({oncall, Callrec}, "from", State),
			?assertMatch({reply, ok, oncall, _State}, Out),
			Assertmocks(),
			NewState= element(4, Out),
			?assertEqual(0, NewState#state.ringouts)
		end}
	end,
	fun({#state{agent_rec = Agent} = State, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to oncall with call id mismatch",
		fun() ->
			Oldcall = Agent#agent.statedata,
			Callrec = Oldcall#call{id = "invalid"},
			?assertMatch({reply, invalid, ringing, _State}, ringing({oncall, Callrec}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = InAgentRec} = InAgent, AMmock, Dmock, _Monmock, Connmock, Assertmocks}) ->
		{"to oncall from media while waiting on outband ring success/fail with outband media path",
		fun() ->
			InCall = InAgentRec#agent.statedata,
			Call = InCall#call{ring_path = outband, media_path = outband},
			AgentRec = InAgentRec#agent{statedata = Call},
			Agent = InAgent#state{ring_locked = wait, agent_rec = AgentRec},
			gen_server_mock:expect_cast(Dmock, fun({end_avail, _Apid}, _State) -> ok end),
			gen_leader_mock:expect_cast(AMmock, fun({end_avail, _Nom}, _State, _Elec) -> ok end),
			gen_server_mock:expect_cast(Connmock, fun({change_state, oncall, Inrec}, _State) ->
				Inrec = AgentRec#agent.statedata,
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_info(AgentRec#agent.log_pid, fun({"testagent", oncall, ringing, _Callrec}, _State) -> ok end),
			?assertMatch({reply, ok, oncall, _State}, ringing({oncall, AgentRec#agent.statedata}, {Call#call.source, "tag"}, Agent)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = InAgentRec} = InAgent, _AMmock, _Dmock, _Monmock, Connmock, Assertmocks}) ->
		{"to oncall from connection while waiting on outband ring success/fail with outband media path",
		fun() ->
			InCall = InAgentRec#agent.statedata,
			Call = InCall#call{ring_path = outband, media_path = outband},
			AgentRec = InAgentRec#agent{statedata = Call},
			Agent = InAgent#state{ring_locked = wait},
			?assertMatch({reply, invalid, ringing, _State}, ringing({oncall, AgentRec#agent.statedata}, {Connmock, "tag"}, Agent)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = InAgentRec} = InAgent, _AMmock, _Dmock, _Monmock, Connmock, Assertmocks}) ->
		{"to oncall from connection while waiting on outband ring success/fail with inband media path",
		fun() ->
			InCall = InAgentRec#agent.statedata,
			Call = InCall#call{ring_path = outband, media_path = inband},
			AgentRec = InAgentRec#agent{statedata = Call},
			Agent = InAgent#state{ring_locked = wait},
			?assertMatch({reply, invalid, ringing, _State}, ringing({oncall, AgentRec#agent.statedata}, {Connmock, "tag"}, Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to outgoing",
		fun() ->
			?assertMatch({reply, invalid, ringing, _State}, ringing({outgoing, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _, _, _, _, _}) ->
		{"to released with bad format",
		fun() ->
			?assertMatch({reply, invalid, ringing, _State}, ringing({released, "not valid"}, "from", Agent))
		end}
	end,
	fun({#state{agent_rec = Agent} = State, AMmock, Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to released default",
		fun() ->
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_cast(Dmock, fun({end_avail, _Apid}, _State) -> ok end),
			gen_leader_mock:expect_cast(AMmock, fun({end_avail, _Nom}, _State, _Elec) -> ok end),
			gen_server_mock:expect_cast(Connmock, fun({change_state, released, ?DEFAULT_REL}, _State) ->
				ok
			end),
			Callrec = Agent#agent.statedata,
			Self = self(),
			gen_server_mock:expect_info(Callrec#call.source, fun({'$gen_media_stop_ring', Inpid}, _State) ->
				Inpid = Self,
				ok
			end),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", released, ringing, ?DEFAULT_REL}, _State) -> ok end),
			?assertMatch({reply, ok, released, _State}, ringing({released, default}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, AMmock, Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to released",
		fun() ->
			gen_server_mock:expect_cast(Dmock, fun({end_avail, _Apid}, _State) -> ok end),
			gen_leader_mock:expect_cast(AMmock, fun({end_avail, _Nom}, _State, _Elec) -> ok end),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_cast(Connmock, fun({change_state, released, {"id", "res_reason", 0}}, _State) ->
				ok
			end),
			Callrec = Agent#agent.statedata,
			Self = self(),
			gen_server_mock:expect_info(Callrec#call.source, fun({'$gen_media_stop_ring', Inpid}, _State) ->
				Inpid = Self,
				ok
			end),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", released, ringing, {"id", "res_reason", 0}}, _State) -> ok end),
			?assertMatch({reply, ok, released, _State}, ringing({released, {"id", "res_reason", 0}}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to warmtransfer",
		fun() ->
			?assertMatch({reply, invalid, ringing, _State}, ringing({warmtransfer, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to wrapup",
		fun() ->
			?assertMatch({reply, invalid, ringing, _State}, ringing({wrapup, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = #agent{statedata = Callrec} = _Agent} = Seedstate, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"Ring fails are reset on a has_successful_ring message",
		fun() ->
			State = Seedstate#state{ring_fails = 2},
			?assertEqual({next_state, ringing, Seedstate}, ringing({has_successful_ring, Callrec#call.source}, State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = #agent{statedata = Callrec} = _Agent} = Seedstate, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"Ring success clears the wait state",
		fun() ->
			State = Seedstate#state{ring_locked = wait},
			?assertEqual({next_state, ringing, Seedstate}, ringing({has_successful_ring, Callrec#call.source}, State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = #agent{statedata = Callrec} = Agent} = Seedstate, AMmock, Dmock, Monmock, Connmock, Assertmocks}) ->
		{"gets a ring_failed message with < 3 ring fails",
		fun() ->
			Aself = self(),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_cast(Connmock, fun({change_state, idle}, _State) ->
				ok
			end),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", idle, ringing, {}}, _State) -> ok end),
			gen_leader_mock:expect_cast(AMmock, fun({now_avail, Nom}, _State, _Elec) ->
				Nom = Agent#agent.login,
				ok
			end),
			?assertMatch({next_state, idle, #state{ring_fails = 1} = _State}, ringing({failed_ring, Callrec#call.source}, Seedstate)),
			Gotmsg = receive
				ring_unlock ->
					ok
			after (?RING_LOCK_DURATION + 50) ->
				error
			end,
			?assertEqual(ok, Gotmsg),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = #agent{statedata = Callrec} = Agent} = InSeedstate, AMmock, Dmock, Monmock, Connmock, Assertmocks}) ->
		{"gets a ring_failed message with < 3 ring fails and wait lock state",
		fun() ->
			Seedstate = InSeedstate#state{ring_locked = wait},
			Aself = self(),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_cast(Connmock, fun({change_state, idle}, _State) ->
				ok
			end),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", idle, ringing, {}}, _State) -> ok end),
			gen_leader_mock:expect_cast(AMmock, fun({now_avail, Nom}, _State, _Elec) ->
				Nom = Agent#agent.login,
				ok
			end),
			?assertMatch({next_state, idle, #state{ring_fails = 1} = _State}, ringing({failed_ring, Callrec#call.source}, Seedstate)),
			Gotmsg = receive
				ring_unlock ->
					ok
			after (?RING_LOCK_DURATION + 50) ->
				error
			end,
			?assertEqual(ok, Gotmsg),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = #agent{statedata = Callrec} = Agent} = Seedstate, AMmock, Dmock, Monmock, Connmock, Assertmocks}) ->
		{"gets a ring_failed message with = 3 ring fails",
		fun() ->
			State = Seedstate#state{ring_fails = 3},
			gen_server_mock:expect_cast(Dmock, fun({end_avail, _Apid}, _State) -> ok end),
			gen_leader_mock:expect_cast(AMmock, fun({end_avail, _Nom}, _State, _Elec) -> ok end),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_cast(Connmock, fun({change_state, released, ?RING_FAIL_REL}, _State) ->
				ok
			end),
			Self = self(),
			gen_server_mock:expect_info(Callrec#call.source, fun({'$gen_media_stop_ring', Inpid}, _State) ->
				Inpid = Self,
				ok
			end),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", released, ringing, ?RING_FAIL_REL}, _State) -> ok end),
			?assertMatch({next_state, released, #state{ring_fails = 0}}, ringing({failed_ring, Callrec#call.source}, State)),
			Assertmocks()
		end}
	end]}.

from_precall_tests() ->
	{foreach,
	fun() ->
		{ok, Dmock} = gen_server_mock:named({local, dispatch_manager}),
		{ok, Monmock} = cpx_monitor:make_mock(),
		%{ok, Monmock} = gen_leader_mock:start(cpx_monitor),
		{ok, Connmock} = gen_server_mock:new(),
		{ok, Logpid} = gen_server_mock:new(),
		{ok, AMmock} = gen_leader_mock:start(agent_manager),
		Client = #client{label = "testclient", id = "testclient"},
		Call = #call{id = "testcall", client = Client, source = spawn(fun() -> ok end)},
		Agent = #agent{id = "testid", login = "testagent", connection = Connmock, state = precall, statedata = Call, log_pid = Logpid},
		Assertmocks = fun() ->
			gen_server_mock:assert_expectations(Dmock),
			%gen_leader_mock:assert_expectations(Monmock),
			cpx_monitor:assert_mock(),
			gen_leader_mock:assert_expectations(AMmock),
			gen_server_mock:assert_expectations(Connmock),
			gen_server_mock:assert_expectations(Logpid),
			ok
		end,
		{#state{agent_rec = Agent}, AMmock, Dmock, Monmock, Connmock, Assertmocks}
	end,
	fun({_Agent, AMmock, Dmock, Monmock, Connmock, _Assertmocks}) ->
		gen_server_mock:stop(Dmock),
		cpx_monitor:stop_mock(),
		%gen_leader_mock:stop(Monmock),
		gen_server_mock:stop(Connmock),
		gen_leader_mock:stop(AMmock),
		timer:sleep(10), % because the mock dispatch manager isn't dying quickly enough 
		% before the next test runs.
		ok
	end,
	[fun({#state{agent_rec = Agent} = State, AMmock, Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to idle",
		fun() ->
			Apid = self(),
			gen_server_mock:expect_cast(Dmock, fun({now_avail, Pid}, _State) ->
				Apid = Pid,
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_cast(Connmock, fun({change_state, idle}, _State) ->
				ok
			end),
			gen_leader_mock:expect_cast(AMmock, fun({now_avail, Nom}, _State, _Elec) ->
				Nom = Agent#agent.login,
				ok
			end),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", idle, precall, {}}, _State) -> ok end),
			?assertMatch({reply, ok, idle, _State}, precall(idle, "from", State)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to ringing",
		fun() ->
			?assertMatch({reply, invalid, precall, _State}, precall({ringing, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to precall",
		fun() ->
			?assertMatch({reply, invalid, precall, _State}, precall({precall, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to oncall",
		fun() ->
			?assertMatch({reply, invalid, precall, _State}, precall({oncall, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to outgoing",
		fun() ->
			gen_server_mock:expect_cast(Connmock, fun({change_state, outgoing, _Data}, _State) ->
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", outgoing, precall, _}, _State) -> ok end),
			?assertMatch({reply, ok, outgoing, _State}, precall({outgoing, #call{id = "testcall", source = spawn(fun() -> ok end)} }, "from", State)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to released with bad info",
		fun() ->
			?assertMatch({reply, invalid, precall, _State}, precall({released, "reason"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to released with default",
		fun() ->
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_cast(Connmock, fun({change_state, released, ?DEFAULT_REL}, _State) ->
				ok
			end),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", released, precall, ?DEFAULT_REL}, _State) -> ok end),
			?assertMatch({reply, ok, released, _State}, precall({released, default}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to released",
		fun() ->
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_cast(Connmock, fun({change_state, released, {"id", "reason", 0}}, _State) ->
				ok
			end),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", released, precall, {"id", "reason", 0}}, _State) -> ok end),
			?assertMatch({reply, ok, released, _State}, precall({released, {"id", "reason", 0}}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to warmtransfer",
		fun() ->
			?assertMatch({reply, invalid, precall, _State}, precall({warmtransfer, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to wrapup",
		fun() ->
			?assertMatch({reply, invalid, precall, _State}, precall({wrapup, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end]}.

from_oncall_tests() ->
	{foreach,
	fun() ->
		{ok, Dmock} = gen_server_mock:named({local, dispatch_manager}),
		{ok, Monmock} = cpx_monitor:make_mock(),
		%{ok, Monmock} = gen_leader_mock:start(cpx_monitor),
		{ok, Connmock} = gen_server_mock:new(),
		{ok, AMmock} = gen_leader_mock:start(agent_manager),
		Client = #client{label = "testclient"},
		{ok, Mediapid} = gen_server_mock:new(),
		{ok, Logpid} = gen_server_mock:new(),
		{ok, RingChanMock} = gen_server_mock:new(),
		Callrec = #call{
			id = "testcall",
			source = Mediapid,
			client = Client
		},
		Agent = #agent{id = "testid", login = "testagent", connection = Connmock, state = oncall, statedata = Callrec, log_pid = Logpid, endpointtype = {RingChanMock, transient, sip_registration}},
		Assertmocks = fun() ->
			gen_server_mock:assert_expectations(Dmock),
			cpx_monitor:assert_mock(),
			%gen_leader_mock:assert_expectations(Monmock),
			gen_server_mock:assert_expectations(Connmock),
			gen_server_mock:assert_expectations(Logpid),
			gen_leader_mock:assert_expectations(AMmock),
			ok
		end,
		{#state{agent_rec = Agent}, AMmock, Dmock, Monmock, Connmock, Assertmocks}
	end,
	fun({_Agent, AMmock, Dmock, Monmock, Connmock, _Assertmocks}) ->
		gen_server_mock:stop(Dmock),
		cpx_monitor:stop_mock(),
		%gen_leader_mock:stop(Monmock),
		gen_server_mock:stop(Connmock),
		gen_leader_mock:stop(AMmock),
		timer:sleep(10), % because the mock dispatch manager isn't dying quickly enough 
		% before the next test runs.
		ok
	end,
	[fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to idle",
		fun() ->
			?assertMatch({reply, invalid, oncall, _State}, oncall(idle, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to ringing",
		fun() ->
			?assertMatch({reply, invalid, oncall, _State}, oncall({ringing, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to precall",
		fun() ->
			?assertMatch({reply, invalid, oncall, _State}, oncall({precall, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to oncall",
		fun() ->
			?assertMatch({reply, invalid, oncall, _State}, oncall({oncall, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to outgoing",
		fun() ->
			?assertMatch({reply, invalid, oncall, _State}, oncall({outgoing, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to released when no release queued with default reason",
		fun() ->
			?assertMatch({reply, queued, oncall, _State}, oncall({released, default}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to released when no release queued with invalid reason",
		fun() ->
			?assertMatch({reply, invalid, oncall, _State}, oncall({released, "goober"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to released when no release queued with valid reason",
		fun() ->
			?assertMatch({reply, queued, oncall, _State}, oncall({released, {"id", "goober", 0}}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Oldagent} = State, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to released when a release is queued",
		fun() ->
			Agent = Oldagent#agent{queuedrelease = "oldreason"},
			?assertMatch({reply, queued, oncall, _State}, oncall({released, {"id", "new reason", 0}}, "from", State#state{agent_rec = Agent})),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Oldagent} = State, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to released (default) when a release is queued",
		fun() ->
			Agent = Oldagent#agent{queuedrelease = {"oid", "oldreason", 0}},
			?assertMatch({reply, queued, oncall, _State}, oncall({released, default}, "from", State#state{agent_rec = Agent})),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Oldagent} = State, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to released (invalid) when a release is queued",
		fun() ->
			Agent = Oldagent#agent{queuedrelease = {"oid", "oldreason", 0}},
			?assertMatch({reply, invalid, oncall, _State}, oncall({released, "new reason"}, "from", State#state{agent_rec = Agent})),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Oldagent} = State, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to released undefined",
		fun() ->
			Agent = Oldagent#agent{queuedrelease = {"oid", "oldreason", 0}},
			?assertMatch({reply, ok, oncall, _State}, oncall({released, undefined}, "from", State#state{agent_rec = Agent})),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to warmtransfer",
		fun() ->
			%Callrec = Agent#agent.statedata,
			gen_server_mock:expect_cast(Connmock, fun({change_state, warmtransfer, {onhold, _Callrec, calling, "transferto"}}, _State) ->
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, 
				fun({"testagent", warmtransfer, oncall, {onhold, Call, calling, "transferto"}}, _State) -> 
					Call = Agent#agent.statedata,
					ok 
				end),
			?assertMatch({reply, ok, warmtransfer, _State}, oncall({warmtransfer, "transferto"}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = BaseAgent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to wrapup request from agent connection and inband media",
		fun() ->
			Basecall = BaseAgent#agent.statedata,
			Callrec = Basecall#call{media_path = inband},
			Agent = BaseAgent#agent{statedata = Callrec},
			gen_server_mock:expect_cast(Connmock, fun({change_state, wrapup, Incall}, _State) ->
				Incall = Agent#agent.statedata,
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", wrapup, oncall, Incall}, _State) ->
				Incall = Agent#agent.statedata,
				ok
			end),
			gen_server_mock:expect_call(Callrec#call.source, fun('$gen_media_wrapup', _From, _State) ->
				ok
			end),
			?assertMatch({reply, ok, wrapup, #state{agent_rec = #agent{endpointtype = {undefined, transient, _}}} = _State}, oncall({wrapup, Agent#agent.statedata}, {Connmock, make_ref()}, State#state{agent_rec = Agent})),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = BaseAgent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to wrapup request from media and inband media",
		fun() ->
			Basecall = BaseAgent#agent.statedata,
			Callrec = Basecall#call{media_path = inband},
			Agent = BaseAgent#agent{statedata = Callrec},
			gen_server_mock:expect_cast(Connmock, fun({change_state, wrapup, Incall}, _State) ->
				Incall = Agent#agent.statedata,
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", wrapup, oncall, Incall}, _State) ->
				Incall = Agent#agent.statedata,
				ok
			end),
			?assertMatch({reply, ok, wrapup, #state{agent_rec = #agent{endpointtype = {undefined, transient, _}}} = _State}, oncall({wrapup, Agent#agent.statedata}, {Callrec#call.source, make_ref()}, State#state{agent_rec = Agent})),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = BaseAgent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to wrapup request from some other pid and inband media",
		fun() ->
			Basecall = BaseAgent#agent.statedata,
			Client = #client{label = "testclient", options = [{?WRAPUP_AUTOEND_KEY, 1}]},
			Callrec = Basecall#call{client = Client},
			Agent = BaseAgent#agent{statedata = Callrec},
			Out = oncall({wrapup, Agent#agent.statedata}, {spawn(fun() -> ok end), make_ref()}, State),
			?assertMatch({reply, invalid, oncall, #state{agent_rec = #agent{endpointtype = {_, transient, _}}} = _State}, Out),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to wrapup request from agent connection with outband media",
		fun() ->
			?assertMatch({reply, invalid, oncall, #state{agent_rec = #agent{endpointtype = {_, transient, _}}} = _State}, oncall({wrapup, Agent#agent.statedata}, {Connmock, make_ref()}, State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to wrapup request from media with outband media",
		fun() ->
			Call = Agent#agent.statedata,
			gen_server_mock:expect_cast(Connmock, fun({change_state, wrapup, Incall}, _State) ->
				Incall = Agent#agent.statedata,
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", wrapup, oncall, Incall}, _State) -> Incall = Agent#agent.statedata, ok end),
			?assertMatch({reply, ok, wrapup, #state{agent_rec = #agent{endpointtype = {undefined, transient, _}}} = _State}, oncall({wrapup, Agent#agent.statedata}, {Call#call.source, make_ref()}, State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to wrapup request from some other pid with outband media",
		fun() ->
			?assertMatch({reply, invalid, oncall, _State}, oncall({wrapup, Agent#agent.statedata}, {spawn(fun() -> ok end), make_ref()}, State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = #agent{endpointtype = {RingChanPid, _, _}} = BaseAgent} = BaseState, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to wrapup with persistant ring channel",
		fun() ->
			Agent = BaseAgent#agent{endpointtype = {RingChanPid, persistant, sip_registration}},
			State = BaseState#state{agent_rec = Agent},
			Call = Agent#agent.statedata,
			gen_server_mock:expect_cast(Connmock, fun({change_state, wrapup, Incall}, _State) ->
				Incall = Agent#agent.statedata,
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", wrapup, oncall, Incall}, _State) -> Incall = Agent#agent.statedata, ok end),
			Out = oncall({wrapup, Agent#agent.statedata}, {Call#call.source, make_ref()}, State),

			?DEBUG("Das out:  ~p", [Out]),
			?assertMatch({reply, ok, wrapup, #state{agent_rec = #agent{endpointtype = {RingChanPid, persistant, _}}} = _State}, Out),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = BaseAgent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to wrapup with a hard end",
		fun() ->
			Basecall = BaseAgent#agent.statedata,
			Client = #client{label = "testclient", options = [{?WRAPUP_AUTOEND_KEY, 1}]},
			Callrec = Basecall#call{client = Client},
			Agent = BaseAgent#agent{statedata = Callrec},
			gen_server_mock:expect_cast(Connmock, fun({change_state, wrapup, Incall}, _State) ->
				Incall = Agent#agent.statedata,
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", wrapup, oncall, Incall}, _State) -> Incall = Agent#agent.statedata, ok end),
			?assertMatch({reply, ok, wrapup, _State}, oncall({wrapup, Agent#agent.statedata}, {Callrec#call.source, make_ref()}, State#state{agent_rec = Agent})),
			Gotend = receive
				end_wrapup ->
					ok
			after 1500 ->
				error
			end,
			?assertEqual(ok, Gotend),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = BaseAgent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to wrapup with hard end and inband media",
		fun() ->
			Basecall = BaseAgent#agent.statedata,
			Client = #client{label = "testclient", options = [{?WRAPUP_AUTOEND_KEY, 1}]},
			Callrec = Basecall#call{client = Client, media_path = inband},
			Agent = BaseAgent#agent{statedata = Callrec},
			gen_server_mock:expect_call(Callrec#call.source, fun('$gen_media_wrapup', _From, _State) -> ok end),
			gen_server_mock:expect_cast(Connmock, fun({change_state, wrapup, Incall}, _State) ->
				Incall = Agent#agent.statedata,
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", wrapup, oncall, Incall}, _State) -> Incall = Agent#agent.statedata, ok end),
			?assertMatch({reply, ok, wrapup, _State}, oncall(wrapup, {Connmock, make_ref()}, State#state{agent_rec = Agent})),
			Gotend = receive
				end_wrapup ->
					ok
			after 1500 ->
				error
			end,
			?assertEqual(ok, Gotend),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = #agent{statedata = Callrec} = _Agent} = Seedstate, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"gets successful ring message, so resets ring fails",
		fun() ->
			State = Seedstate#state{ring_fails = 2},
			?assertMatch({next_state, oncall, #state{ring_fails = 0} = _Newstate}, oncall({has_successful_ring, Callrec#call.source}, State)),
			Assertmocks()
		end}
	end]}.

from_outgoing_tests() ->
	{foreach,
	fun() ->
		{ok, Dmock} = gen_server_mock:named({local, dispatch_manager}),
		{ok, Monmock} = gen_leader_mock:start(cpx_monitor),
		{ok, Connmock} = gen_server_mock:new(),
		{ok, AMmock} = gen_leader_mock:start(agent_manager),
		Client = #client{label = "testclient"},
		{ok, Mediapid} = gen_server_mock:new(),
		{ok, Logpid} = gen_server_mock:new(),
		Callrec = #call{
			id = "testcall",
			source = Mediapid,
			client = Client
		},
		Agent = #agent{id = "testid", login = "testagent", connection = Connmock, state = oncall, statedata = Callrec, log_pid = Logpid},
		Assertmocks = fun() ->
			gen_server_mock:assert_expectations(Dmock),
			cpx_monitor:assert_mock(),
			%gen_leader_mock:assert_expectations(Monmock),
			gen_server_mock:assert_expectations(Connmock),
			gen_server_mock:assert_expectations(Logpid),
			gen_leader_mock:assert_expectations(AMmock),
			ok
		end,
		{#state{agent_rec = Agent}, AMmock, Dmock, Monmock, Connmock, Assertmocks}
	end,
	fun({_Agent, AMmock, Dmock, Monmock, Connmock, _Assertmocks}) ->
		gen_server_mock:stop(Dmock),
		%gen_leader_mock:stop(Monmock),
		cpx_monitor:stop_mock(),
		gen_server_mock:stop(Connmock),
		gen_leader_mock:stop(AMmock),
		timer:sleep(10), % because the mock dispatch manager isn't dying quickly enough 
		% before the next test runs.
		ok
	end,
	[fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to idle",
		fun() ->
			?assertMatch({reply, invalid, outgoing, _State}, outgoing(idle, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to ringing",
		fun() ->
			?assertMatch({reply, invalid, outgoing, _State}, outgoing({ringing, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to precall",
		fun() ->
			?assertMatch({reply, invalid, outgoing, _State}, outgoing({precall, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to oncall",
		fun() ->
			?assertMatch({reply, invalid, outgoing, _State}, outgoing({oncall, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to outgoing",
		fun() ->
			?assertMatch({reply, invalid, outgoing, _State}, outgoing({outgoing, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to unqueuing a release",
		fun() ->
			?assertMatch({reply, ok, outgoing, _State}, outgoing({released, undefined}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to queue a default release",
		fun() ->
			?assertMatch({reply, queued, outgoing, _State}, outgoing({released, default}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to queue a valid release",
		fun() ->
			?assertMatch({reply, queued, outgoing, _State}, outgoing({released, {"id", "reason", 0}}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to queue an invalid release",
		fun() ->
			?assertMatch({reply, invalid, outgoing, _State}, outgoing({released, "bad bad juju"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to requeue a release",
		fun() ->
			?assertMatch({reply, queued, outgoing, _State}, outgoing({released, default}, "from", Agent)),
			?assertMatch({reply, queued, outgoing, _State}, outgoing({released, {"id", "new reason", 0}}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to warmtransfer",
		fun() ->
			gen_server_mock:expect_cast(Connmock, fun({change_state, warmtransfer, {onhold, #call{id = "testcall"}, calling, "transferto"}}, _State) ->
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [{node, node()}], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, 
				fun({"testagent", warmtransfer, outgoing, {onhold, Call, calling, "transferto"}}, _State) ->
					Call = Agent#agent.statedata,
					ok
				end),
			?assertMatch({reply, ok, warmtransfer, _State}, outgoing({warmtransfer, "transferto"}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to wrapup",
		fun() ->
			gen_server_mock:expect_cast(Connmock, fun({change_state, wrapup, _Calldata}, _State) ->
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [{node, node()}], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", wrapup, outgoing, Call}, _State) ->
				Call = Agent#agent.statedata, 
				ok
			end),
			?assertMatch({reply, ok, wrapup, _State}, outgoing({wrapup, Agent#agent.statedata}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to wrapup with call id mismatch",
		fun() ->
			Oldrec = Agent#agent.statedata,
			Invalidrec = Oldrec#call{id = "bad bad leroy brown"},
			?assertMatch({reply, invalid, outgoing, _State}, outgoing({wrapup, Invalidrec}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = BaseAgent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to wrapup with a hard end",
		fun() ->
			Basecall = BaseAgent#agent.statedata,
			Client = #client{label = "testclient", options = [{?WRAPUP_AUTOEND_KEY, 1}]},
			Callrec = Basecall#call{client = Client},
			Agent = BaseAgent#agent{statedata = Callrec},
			gen_server_mock:expect_cast(Connmock, fun({change_state, wrapup, _Calldata}, _State) ->
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [{node, node()}], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", wrapup, outgoing, Call}, _State) ->
				Call = Agent#agent.statedata, 
				ok
			end),
			?assertMatch({reply, ok, wrapup, _State}, outgoing({wrapup, Agent#agent.statedata}, "from", State#state{agent_rec = Agent})),
			Gotend = receive
				end_wrapup ->
					ok
			after 1500 ->
				error
			end,
			?assertEqual(ok, Gotend),
			Assertmocks()
		end}
	end]}.

from_released_tests() ->
	{foreach,
	fun() ->
		{ok, Dmock} = gen_server_mock:named({local, dispatch_manager}),
		{ok, Monmock} = cpx_monitor:make_mock(),
		{ok, Connmock} = gen_server_mock:new(),
		{ok, AMmock} = gen_leader_mock:start(agent_manager),
		{ok, Logpid} = gen_server_mock:new(),
		Agent = #agent{id = "testid", login = "testagent", connection = Connmock, state = released, statedata = "testrelease", log_pid = Logpid},
		Assertmocks = fun() ->
			gen_server_mock:assert_expectations(Dmock),
			cpx_monitor:assert_mock(),
			gen_server_mock:assert_expectations(Connmock),
			gen_server_mock:assert_expectations(Logpid),
			gen_leader_mock:assert_expectations(AMmock),
			ok
		end,
		{#state{agent_rec = Agent}, AMmock, Dmock, Monmock, Connmock, Assertmocks}
	end,
	fun({_Agent, AMmock, Dmock, Monmock, Connmock, _Assertmocks}) ->
		gen_server_mock:stop(Dmock),
		cpx_monitor:stop_mock(),
		gen_server_mock:stop(Connmock),
		gen_leader_mock:stop(AMmock),
		timer:sleep(10), % because the mock dispatch manager isn't dying quickly enough 
		% before the next test runs.
		ok
	end,
	[fun({#state{agent_rec = Agent} = State, AMmock, Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to idle",
		fun() ->
			gen_server_mock:expect_cast(Connmock, fun({change_state, idle}, _State) ->
				ok
			end),
			Self = self(),
			gen_server_mock:expect_cast(Dmock, fun({now_avail, Apid}, _State) ->
				Self = Apid,
				ok
			end),
			gen_leader_mock:expect_cast(AMmock, fun({now_avail, Nom}, _State, _Elec) ->
				Nom = Agent#agent.login,
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [{node, node()}], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", idle, released, {}}, _State) -> ok end),
			?assertMatch({reply, ok, idle, _State}, released(idle, "from", State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to ringing",
		fun() ->
			gen_server_mock:expect_cast(Connmock, fun({change_state, ringing, "callrec"}, _State) ->
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [{node, node()}], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", ringing, released, "callrec"}, _State) -> ok end),
			?assertMatch({reply, ok, ringing, _State}, released({ringing, "callrec"}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to precall",
		fun() ->
			gen_server_mock:expect_cast(Connmock, fun({change_state, precall, "client"}, _State) ->
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [{node, node()}], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", precall, released, "client"}, _State) -> ok end),
			?assertMatch({reply, ok, precall, _State}, released({precall, "client"}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to oncall",
		fun() ->
			?assertMatch({reply, invalid, released, _State}, released({oncall, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to outgoing",
		fun() ->
			?assertMatch({reply, invalid, released, _State}, released({outgoing, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, _AMmock, _Dmock, _Monmock, Connmock, Assertmocks}) ->
		{"to released default",
		fun() ->
			gen_server_mock:expect_cast(Connmock, fun({change_state, released, ?DEFAULT_REL}, _State) ->
				ok
			end),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", released, released, ?DEFAULT_REL}, _State) -> ok end),
			?assertMatch({reply, ok, released, _State}, released({released, default}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, _AMmock, _Dmock, _Monmock, Connmock, Assertmocks}) ->
		{"to released valid",
		fun() ->
			gen_server_mock:expect_cast(Connmock, fun({change_state, released, {"id", "reason", 0}}, _State) ->
				ok
			end),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", released, released, {"id", "reason", 0}}, _State) -> ok end),
			?assertMatch({reply, ok, released, _State}, released({released, {"id", "reason", 0}}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to released invalid",
		fun() ->
			?assertMatch({reply, invalid, released, _State}, released({released, "not gonna work"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to warmtransfer",
		fun() ->
			?assertMatch({reply, invalid, released, _State}, released({warmtransfer, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to wrapup",
		fun() ->
			?assertMatch({reply, invalid, released, _State}, released({wrapup, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end]}.

from_warmtransfer_tests() ->
	{foreach,
	fun() ->
		{ok, Dmock} = gen_server_mock:named({local, dispatch_manager}),
		{ok, Monmock} = cpx_monitor:make_mock(),
		{ok, Connmock} = gen_server_mock:new(),
		{ok, Logpid} = gen_server_mock:new(),
		{ok, AMmock} = gen_leader_mock:start(agent_manager),
		Client = #client{label = "testclient"},
		{ok, Mediapid} = gen_server_mock:new(),
		Callrec = #call{
			id = "testcall",
			source = Mediapid,
			client = Client
		},
		Agent = #agent{id = "testid", login = "testagent", connection = Connmock, state = warmtransfer, statedata = {onhold, Callrec, calling, "testtarget"}, log_pid = Logpid},
		Assertmocks = fun() ->
			gen_server_mock:assert_expectations(Dmock),
			cpx_monitor:assert_mock(),
			gen_server_mock:assert_expectations(Connmock),
			gen_server_mock:assert_expectations(Logpid),
			gen_leader_mock:assert_expectations(AMmock),
			ok
		end,
		{#state{agent_rec = Agent}, AMmock, Dmock, Monmock, Connmock, Assertmocks}
	end,
	fun({_Agent, AMmock, Dmock, Monmock, Connmock, _Assertmocks}) ->
		gen_server_mock:stop(Dmock),
		cpx_monitor:stop_mock(),
		gen_server_mock:stop(Connmock),
		gen_leader_mock:stop(AMmock),
		timer:sleep(10), % because the mock dispatch manager isn't dying quickly enough 
		% before the next test runs.
		ok
	end,
	[fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to idle",
		fun() ->
			?assertMatch({reply, invalid, warmtransfer, _State}, warmtransfer(idle, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to ringing",
		fun() ->
			?assertMatch({reply, invalid, warmtransfer, _State}, warmtransfer({ringing, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to precall",
		fun() ->
			?assertMatch({reply, invalid, warmtransfer, _State}, warmtransfer({precall, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to oncall",
		fun() ->
			{onhold, Callrec, calling, _Whoever} = Agent#agent.statedata,
			gen_server_mock:expect_cast(Connmock, fun({change_state, oncall, Inrec}, _State) ->
				Callrec = Inrec,
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [{node, node()}], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", oncall, warmtransfer, _Callrec}, _State) -> ok end),
			?assertMatch({reply, ok, oncall, _State}, warmtransfer({oncall, Callrec}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to oncall with id mismatch",
		fun() ->
			Badcall = #call{id = "hagurk", source= self()},
			?assertMatch({reply, invalid, warmtransfer, _State}, warmtransfer({oncall, Badcall}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to outgoing",
		fun() ->
			gen_server_mock:expect_cast(Connmock, fun({change_state, outgoing, _Inrec}, _State) ->
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [{node, node()}], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", outgoing, warmtransfer, Callrec}, _State) ->
				Callrec = element(2, Agent#agent.statedata),
				ok
			end),
			?assertMatch({reply, ok, outgoing, _State}, warmtransfer({outgoing, element(2, Agent#agent.statedata)}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = Agent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to wrapup",
		fun() ->
			{onhold, Callrec, calling, _Whoever} = Agent#agent.statedata,
			gen_server_mock:expect_cast(Connmock, fun({change_state, wrapup, Inrec}, _State) ->
				Callrec = Inrec,
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [{node, node()}], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", wrapup, warmtransfer, _Callrec}, _State) -> ok end),
			?assertMatch({reply, ok, wrapup, _State}, warmtransfer({wrapup, Callrec}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to oncall with id mismatch",
		fun() ->
			Badcall = #call{id = "hagurk", source= self()},
			?assertMatch({reply, invalid, warmtransfer, _State}, warmtransfer({wrapup, Badcall}, "from", Agent)),
			Assertmocks()
		end}
	end]}.

from_wrapup_tests() ->
	{foreach,
	fun() ->
		{ok, Dmock} = gen_server_mock:named({local, dispatch_manager}),
		{ok, Monmock} = cpx_monitor:make_mock(),
		{ok, Connmock} = gen_server_mock:new(),
		{ok, AMmock} = gen_leader_mock:start(agent_manager),
		Client = #client{label = "testclient"},
		{ok, Mediapid} = gen_server_mock:new(),
		Callrec = #call{
			id = "testcall",
			source = Mediapid,
			client = Client
		},
		{ok, Logpid} = gen_server_mock:new(),
		Agent = #agent{id = "testid", login = "testagent", connection = Connmock, state = warmtransfer, statedata = Callrec, log_pid = Logpid},
		Assertmocks = fun() ->
			gen_server_mock:assert_expectations(Dmock),
			cpx_monitor:assert_mock(),
			gen_server_mock:assert_expectations(Connmock),
			gen_leader_mock:assert_expectations(AMmock),
			ok
		end,
		{#state{agent_rec = Agent}, AMmock, Dmock, Monmock, Connmock, Assertmocks}
	end,
	fun({_Agent, AMmock, Dmock, Monmock, Connmock, _Assertmocks}) ->
		gen_server_mock:stop(Dmock),
		cpx_monitor:stop_mock(),
		gen_server_mock:stop(Connmock),
		gen_leader_mock:stop(AMmock),
		timer:sleep(10), % because the mock dispatch manager isn't dying quickly enough 
		% before the next test runs.
		ok
	end,
	[fun({#state{agent_rec = Agent} = State, AMmock, Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to idle",
		fun() ->
			Self = self(),
			gen_server_mock:expect_cast(Dmock, fun({now_avail, Apid}, _State) ->
				Self = Apid,
				ok
			end),
			gen_leader_mock:expect_cast(AMmock, fun({now_avail, Nom}, _State, _Elec) ->
				Nom = Agent#agent.login,
				ok
			end),
			gen_server_mock:expect_cast(Connmock, fun({change_state, idle}, _State) ->
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [{node, node()}], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", idle, wrapup, {}}, _State) -> ok end),
			?assertMatch({reply, ok, idle, _State}, wrapup(idle, "from", State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = OldAgent} = State, _AMmock, _Dmock, Monmock, Connmock, Assertmocks}) ->
		{"to idle with a release queued",
		fun() ->
			Agent = OldAgent#agent{queuedrelease = ?DEFAULT_REL},
			gen_server_mock:expect_cast(Connmock, fun({change_state, released, ?DEFAULT_REL}, _State) ->
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [{node, node()}], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", released, wrapup, ?DEFAULT_REL}, _State) -> ok end),
			?assertMatch({reply, ok, released, _State}, wrapup(idle, "from", State#state{agent_rec = Agent})),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to precall",
		fun() ->
			?assertMatch({reply, invalid, wrapup, _State}, wrapup({precall, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to ringing",
		fun() ->
			?assertMatch({reply, invalid, wrapup, _State}, wrapup({ringing, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to precall",
		fun() ->
			?assertMatch({reply, invalid, wrapup, _State}, wrapup({precall, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to oncall",
		fun() ->
			?assertMatch({reply, invalid, wrapup, _State}, wrapup({oncall, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to outgoing",
		fun() ->
			?assertMatch({reply, invalid, wrapup, _State}, wrapup({outgoing, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to released with reason undefined",
		fun() ->
			?assertMatch({reply, ok, wrapup, #state{agent_rec = #agent{queuedrelease = undefined}}}, wrapup({released, undefined}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to released with default reason",
		fun() ->
			?assertMatch({reply, queued, wrapup, #state{agent_rec = #agent{queuedrelease = ?DEFAULT_REL}}}, wrapup({released, default}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to released with a valid reason",
		fun() ->
			?assertMatch({reply, queued, wrapup, #state{agent_rec = #agent{queuedrelease = {"id", "reason", 0}}}}, wrapup({released, {"id", "reason", 0}}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to released with a bad reason",
		fun() ->
			?assertMatch({reply, invalid, wrapup, #state{agent_rec = #agent{queuedrelease = undefined}}}, wrapup({released, "reason"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to warmtransfer",
		fun() ->
			?assertMatch({reply, invalid, wrapup, _State}, wrapup({warmtransfer, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end,
	fun({Agent, _AMmock, _Dmock, _Monmock, _Connmock, Assertmocks}) ->
		{"to wrapup",
		fun() ->
			?assertMatch({reply, invalid, wrapup, _State}, wrapup({wrapup, "doesn't matter"}, "from", Agent)),
			Assertmocks()
		end}
	end]}.

init_test_() ->
	[{"agent_auth's mnesia tables not available",
	fun() ->
		catch agent_auth:stop(),
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		Agent = #agent{
			login = "testagent",
			skills = [],
			profile = error
		},
		?assertEqual({ok, released, #state{agent_rec = Agent}}, init([Agent, []]))
	end},
	{"agent should do logging of state changes",
	fun() ->
		Agent = #agent{
			login = "testagent",
			skills = [english]
		},
		{ok, released, #state{agent_rec = Newagent}} = init([Agent, [logging]]),
		?assert(is_pid(Newagent#agent.log_pid))
	end},
	{"agent has some magic skills that allow multiples.",
	fun() ->
		Agent = #agent{
			login = "testagent",
			skills = [{'_queue', "queue1"}, {'_queue', "queue2"}, {'_brand', "brandx"}, {'_brand', "brandy"}]
		},
		?assertMatch({ok, released, _NewAgent}, init([Agent, []])),
		{ok, released, #state{agent_rec = #agent{skills = Skills}}} = init([Agent, []]),
		lists:foreach(fun(E) ->
			?assert(lists:member(E, Agent#agent.skills))
		end, Skills)
	end},
	{"agent has some magic skills that should not be multiples of",
	fun() ->
		Agent = #agent{
			login = "testagent",
			skills = [{'_profile', "prof1"}, {'_profile', "prof2"}]
		},
		?assertError(badarg, init([Agent, []]))
	end}].
	
generate_state() ->
	generate_state([{2, idle, "Idle"}, {3, ringing, "Ringing"}, {4, precall, "Precall"}, {5, oncall, "Oncall"}, {6, outgoing, "Outgoing"}, {7, released, "Released"}, {8, warmtransfer, "WarmTransfer"}, {9, wrapup, "Wrapup"}]).
generate_state([{Int, Atom, String}|T]) -> 
	[{"Automated test for " ++ integer_to_list(Int) ++ ", " ++ atom_to_list(Atom), 
		fun() -> 
			?assertEqual(Int, state_to_integer(Atom)),
			?assertEqual(Atom, integer_to_state(Int)),
			?assertEqual(Atom, list_to_state(String))
		end
		}|generate_state(T)];
generate_state([]) -> 
	[].

cross_check_state_test_() ->
	generate_state().

handle_sync_event_test_() ->
	{foreach,
	fun() ->
		ok
	end,
	fun(_) ->
		ok
	end,
	[fun(_) ->
		{"query state",
		fun() ->
			?assertMatch({reply, {ok, statename}, statename, "state"}, handle_sync_event(query_state, "from", statename, "state"))
		end}
	end,
	fun(_) ->
		{"set connectoin when connection is not defined",
		fun() ->
			{ok, Connmock} = gen_server_mock:new(),
			gen_server_mock:expect_cast(Connmock, fun({change_state, idle, undefined}, _State) -> ok end),
			Agent = #agent{
				login = "testagent",
				state = idle,
				statedata = undefined
			},
			?assertMatch({reply, ok, idle, #state{agent_rec = #agent{connection = Connmock}}}, handle_sync_event({set_connection, Connmock}, "from", idle, #state{agent_rec = Agent})),
			gen_server_mock:assert_expectations(Connmock)
		end}
	end,
	fun(_) ->
		{"set connection when connection is already defined",
		fun() ->
			{ok, Curmock} = gen_server_mock:new(),
			{ok, Newmock} = gen_server_mock:new(),
			Agent = #agent{
				login = "testagent",
				state = idle,
				statedata = undefined,
				connection = Curmock
			},
			?assertMatch({reply, error, idle, #state{ agent_rec = #agent{connection = Curmock}}}, handle_sync_event({set_connection, Newmock}, "from", idle, #state{agent_rec = Agent})),
			gen_server_mock:assert_expectations(Curmock),
			gen_server_mock:assert_expectations(Newmock)
		end}
	end,
	fun(_) ->
		{"pop url",
		fun() ->
			{ok, Connmock} = gen_server_mock:new(),
			Agent = #agent{
				login = "testagent",
				state = idle,
				connection = Connmock
			},
			gen_server_mock:expect_cast(Connmock, fun({url_pop, "localhost", "ring"}, _State) -> ok end),
			?assertMatch({reply, ok, idle, _State}, handle_sync_event({url_pop, "localhost", "ring"}, "from", idle, #state{agent_rec = Agent})),
			gen_server_mock:assert_expectations(Connmock)
		end}
	end,
	fun(_) ->
		{"garbage data",
		fun() ->
			?assertMatch({reply, ok, state, "state"}, handle_sync_event(<<"garbage data">>, "from", state, "state"))
		end}
	end,
	fun(_) ->
		{"setting end point doesn't corrupt state",
		fun() ->
			Seedstate = #state{agent_rec = #agent{login = "test"}},
			?assertMatch({reply, ok, state, #state{agent_rec = #agent{endpointtype = {undefined, transient, "end point type"}, endpointdata = "end point data"} = _Agent} = _State}, handle_sync_event({set_endpoint, {"end point type", "end point data"}}, "from", state, Seedstate))
		end}
	end]}.

handle_conn_exit_inband_test_() ->
	{foreach,
	fun() ->
		{ok, Callmock} = gen_server_mock:new(),
		{ok, Connmock} = gen_server_mock:new(),
		Call = #call{id = "test", source = Callmock, media_path = inband},
		{#state{agent_rec = #agent{login = "test", connection = Connmock, statedata = Call}}, Connmock}
	end,
	fun(_) ->
		ok
	end,
	[fun({A, P}) ->
		{"Death in idle",
		fun() ->
			Res = handle_info({'EXIT', P, "fail"}, idle, A),
			?assertEqual({stop, {error, conn_exit, "fail"}, A}, Res)
		end}
	end,
	fun({A, P}) ->
		{"Death in ringing",
		fun() ->
			Res = handle_info({'EXIT', P, "fail"}, ringing, A),
			?assertEqual({stop, {error, conn_exit, "fail"}, A}, Res)
		end}
	end,
	fun({A, P}) ->
		{"Death in precall",
		fun() ->
			Res = handle_info({'EXIT', P, "fail"}, precall, A),
			?assertEqual({stop, {error, conn_exit, "fail"}, A}, Res)
		end}
	end,
	fun({#state{agent_rec = AA} = A, P}) ->
		{"Death in oncall",
		fun() ->
			#call{source = Callmock} = AA#agent.statedata,
			gen_server_mock:expect_call(Callmock, fun('$gen_media_wrapup', _From, _State) -> ok end),
			Res = handle_info({'EXIT', P, "fail"}, oncall, A),
			?assertMatch({stop, {error, conn_exit, "fail"}, _State}, Res),
			{stop, {error, conn_exit, "fail"}, #state{agent_rec = Newa}} = Res,
			?assertEqual(undefined, Newa#agent.connection),
			gen_server_mock:assert_expectations(Callmock)
		end}
	end,
	fun({#state{agent_rec = AA} = A, P}) ->
		{"Death in outgoing",
		fun() ->
			#call{source = Callmock} = AA#agent.statedata,
			gen_server_mock:expect_call(Callmock, fun('$gen_media_wrapup', _From, _State) -> ok end),
			Res = handle_info({'EXIT', P, "fail"}, outgoing, A),
			?assertEqual({stop, {error, conn_exit, "fail"}, A#state{agent_rec = AA#agent{connection = undefined}}}, Res),
			gen_server_mock:assert_expectations(Callmock)
		end}
	end,
	fun({A, P}) ->
		{"Death in released",
		fun() ->
			Res = handle_info({'EXIT', P, "fail"}, released, A),
			?assertEqual({stop, {error, conn_exit, "fail"}, A}, Res)
		end}
	end,
	fun({#state{agent_rec = AA} = A, P}) ->
		{"Death in warm transfer",
		fun() ->
			#call{source = Callmock} = Call = AA#agent.statedata,
			Agent = AA#agent{statedata = {onhold, Call, calling, "target"}},
			State = A#state{agent_rec = Agent},
			gen_server_mock:expect_call(Callmock, fun('$gen_media_wrapup', _From, _State) -> ok end),
			Res = handle_info({'EXIT', P, "fail"}, warmtransfer, State),
			?assertEqual({stop, {error, conn_exit, "fail"}, #state{agent_rec = Agent#agent{connection = undefined}}}, Res),
			gen_server_mock:assert_expectations(Callmock)
		end}
	end,
	fun({A, P}) ->
		{"Death in wrapup",
		fun() ->
			Res = handle_info({'EXIT', P, "fail"}, wrapup, A),
			?assertEqual({stop, {error, conn_exit, "fail"}, A}, Res)
		end}
	end]}.

handle_conn_exit_outband_test_() ->
	{foreach,
	fun() ->
		{ok, Callpid} = dummy_media:start([{id, "test"}, {queues, none}]),
		Callrec = #call{id = "test", source = Callpid, media_path = outband},
		{#state{agent_rec = #agent{login = "test", connection = self()}}, self(), Callpid, Callrec}
	end,
	fun(_) ->
		ok
	end,
	[fun({A, P, _Mp, _C}) ->
		{"Death in idle",
		fun() ->
			Res = handle_info({'EXIT', P, "fail"}, idle, A),
			?assertEqual({stop, {error, conn_exit, "fail"}, A}, Res)
		end}
	end,
	fun({A, P, _Mp, _C}) ->
		{"Death in ringing",
		fun() ->
			Res = handle_info({'EXIT', P, "fail"}, ringing, A),
			?assertEqual({stop, {error, conn_exit, "fail"}, A}, Res)
		end}
	end,
	fun({A, P, _Mp, _C}) ->
		{"Death in precall",
		fun() ->
			Res = handle_info({'EXIT', P, "fail"}, precall, A),
			?assertEqual({stop, {error, conn_exit, "fail"}, A}, Res)
		end}
	end,
	fun({#state{agent_rec = AA} = A, P, _Mp, C}) ->
		{"Death in oncall",
		fun() ->
			Agent = AA#agent{statedata = C},
			Res = handle_info({'EXIT', P, "fail"}, oncall, A#state{agent_rec = Agent}),
			?assertEqual({next_state, oncall, #state{agent_rec = Agent#agent{connection = undefined}}}, Res)
		end}
	end,
	fun({#state{agent_rec = AA} = _A, P, _Mp, C}) ->
		{"Death in outgoing",
		fun() ->
			Agent = AA#agent{statedata = C},
			Res = handle_info({'EXIT', P, "fail"}, outgoing, #state{agent_rec = Agent}),
			?assertEqual({next_state, outgoing, #state{agent_rec = Agent#agent{connection = undefined}}}, Res)
		end}
	end,
	fun({A, P, _Mp, _C}) ->
		{"Death in released",
		fun() ->
			Res = handle_info({'EXIT', P, "fail"}, released, A),
			?assertEqual({stop, {error, conn_exit, "fail"}, A}, Res)
		end}
	end,
	fun({#state{agent_rec = AA} = _A, P, _Mp, C}) ->
		{"Death in warm transfer",
		fun() ->
			Agent = AA#agent{statedata = {onhold, C, calling, "target"}},
			Res = handle_info({'EXIT', P, "fail"}, warmtransfer, #state{agent_rec = Agent}),
			?assertEqual({next_state, warmtransfer, #state{agent_rec = Agent#agent{connection = undefined}}}, Res)
		end}
	end,
	fun({A, P, _Mp, _C}) ->
		{"Death in wrapup",
		fun() ->
			Res = handle_info({'EXIT', P, "fail"}, wrapup, A),
			?assertEqual({stop, {error, conn_exit, "fail"}, A}, Res)
		end}
	end]}.

handle_info_test_() ->
	{foreach,
	fun() ->
		{ok, Dmock} = gen_server_mock:named({local, dispatch_manager}),
		{ok, Monmock} = cpx_monitor:make_mock(),
		{ok, Connmock} = gen_server_mock:new(),
		{ok, AMmock} = gen_leader_mock:start(agent_manager),
		Client = #client{label = "testclient"},
		{ok, Mediapid} = gen_server_mock:new(),
		Callrec = #call{
			id = "testcall",
			source = Mediapid,
			client = Client
		},
		{ok, Logpid} = gen_server_mock:new(),
		Agent = #agent{id = "testid", login = "testagent", connection = Connmock, state = wrapup, statedata = Callrec, log_pid = Logpid},
		Assertmocks = fun() ->
			gen_server_mock:assert_expectations(Dmock),
			cpx_monitor:assert_mock(),
			gen_server_mock:assert_expectations(Connmock),
			gen_leader_mock:assert_expectations(AMmock),
			ok
		end,
		{#state{agent_rec = Agent}, Assertmocks}
	end,
	fun({#state{agent_rec = Agent}, _Assertmocks}) ->
		gen_server_mock:stop(whereis(dispatch_manager)),
		cpx_monitor:stop_mock(),
		gen_server_mock:stop(Agent#agent.connection),
		gen_leader_mock:stop(agent_manager),
		timer:sleep(10), % because the mock dispatch manager isn't dying quickly enough 
		% before the next test runs.
		ok
	end,
	[fun({#state{agent_rec = Agent} = State, Assertmocks}) ->
		{"Hard end to idle",
		fun() ->
			Self = self(),
			gen_server_mock:expect_cast(dispatch_manager, fun({now_avail, Apid}, _State) ->
				Self = Apid,
				ok
			end),
			gen_leader_mock:expect_cast(agent_manager, fun({now_avail, Nom}, _State, _Elec) ->
				Nom = Agent#agent.login,
				ok
			end),
			gen_server_mock:expect_cast(Agent#agent.connection, fun({change_state, idle}, _State) ->
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [{node, node()}], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", idle, wrapup, {}}, _State) -> ok end),
			?assertMatch({next_state, idle, _State}, handle_info(end_wrapup, wrapup, State)),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = OldAgent} = State, Assertmocks}) ->
		{"Hard end with queued release",
		fun() ->
			Agent = OldAgent#agent{queuedrelease = ?DEFAULT_REL},
			gen_server_mock:expect_cast(Agent#agent.connection, fun({change_state, released, ?DEFAULT_REL}, _State) ->
				ok
			end),
			cpx_monitor:add_set({{agent, "testid"}, [{node, node()}], ignore}),
			gen_server_mock:expect_info(Agent#agent.log_pid, fun({"testagent", released, wrapup, ?DEFAULT_REL}, _State) -> ok end),
			?assertMatch({next_state, released, _State}, handle_info(end_wrapup, wrapup, State#state{agent_rec = Agent})),
			Assertmocks()
		end}
	end,
	fun({#state{agent_rec = OldAgent} = State, Assertmocks}) ->
		{"hard wrapup comes in at wrong time",
		fun() ->
			Agent = OldAgent#agent{state = oncall},
			?assertEqual({next_state, oncall, State#state{agent_rec = Agent}}, handle_info(end_wrapup, oncall, State#state{agent_rec = Agent})),
			Assertmocks()
		end}
	end,
	fun({Seedstate, Assertmocks}) ->
		{"getting the unlock message unlocks ringability",
		fun() ->
			State = Seedstate#state{ring_locked = locked},
			?assertEqual({next_state, gooberstate, Seedstate}, handle_info(ring_unlock, gooberstate, State)),
			Assertmocks()
		end}
	end]}.

-endif.
