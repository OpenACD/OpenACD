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
%%  Chris Case	<chris dot case at g33xnexus dot com>
%%

%% @doc A gen_fsm for an agent channel.  When an agent is to go ringing
%% for a media, if the agent fsm has a channel available, a new process
%% of this module is started.  Once the agent has gone through the flow,
%% this process can die.
-module(agent_channel).
-behaviour(gen_fsm).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
%-include_lib("stdlib/include/qlc.hrl").


-record(state, {
	agent_fsm :: pid(),
	agent_connection :: pid(),
	endpoint = inband :: any(),
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
	start/4,
	start_link/2,
	start_link/4,
	stop/1, 
	get_media/1,
	set_state/2, 
	set_state/3,
	end_wrapup/1,
	list_to_state/1, 
	set_connection/2,
	agent_transfer/2,
	queue_transfer/2,
	media_call/2, % conn asking the media stuff
	media_cast/2, % conn telling media stuff
	media_push/3, % media telling conn stuff
	spy/2,
	has_successful_ring/1,
	has_failed_ring/1,
	url_pop/3
]).

% ======================================================================
% API
% ======================================================================

-type(start_opts() :: [{atom(), any()}]).
%% @doc start an fsm with the given options.
-spec(start/2 :: (AgentRec :: #agent{}, Options :: start_opts()) -> {'ok', pid()}).
start(AgentRec, Options) ->
	gen_fsm:start(?MODULE, [AgentRec, Options], []).

start(AgentRec, CallRec, EndpointData, InitState) ->
	gen_fsm:start(?MODULE, [AgentRec, CallRec, EndpointData, InitState]).

%% @doc Start an fsm linked to the calling process.
-spec(start_link/2 :: (AgentRec :: #agent{}, Options :: start_opts()) -> {'ok', pid()}).
start_link(AgentRec, Options) ->
	gen_fsm:start_link(?MODULE, [AgentRec, Options], []).

start_link(AgentRec, CallRec, EndpointData, InitState) ->
	gen_fsm:start_link(?MODULE, [AgentRec, CallRec, EndpointData, InitState], []).

%% @doc Stop the passed agent fsm `Pid'.
-spec(stop/1 :: (Pid :: pid()) -> 'ok').
stop(Pid) -> 
	gen_fsm:send_all_state_event(Pid, stop).
	
%% @doc link the given agent  `Pid' to the given connection `Socket'.
-spec(set_connection/2 :: (Pid :: pid(), Socket :: pid()) -> 'ok' | 'error').
set_connection(Pid, Socket) ->
	gen_fsm:sync_send_all_state_event(Pid, {set_connection, Socket}).

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

%% @doc End the channel while in wrapup.
-spec(end_wrapup/1 :: (Pid :: pid()) -> 'ok' | 'invalid').
end_wrapup(Pid) ->
	gen_fsm:sync_send_event(Pid, stop).

%% @doc attmept to push data from the media connection to the agent.  It's up to
%% the agent connection to interpret this correctly.
-spec(media_push/3 :: (Pid :: pid(), Callrec :: #call{}, Data :: any()) -> any()).
media_push(Pid, Callrec, Data) ->
	S = self(),
	gen_fsm:send_event(Pid, {mediapush, S, Callrec, Data}).

%% @doc Make the give `pid() Spy' spy on `pid() Target'.
-spec(spy/2 :: (Spy :: pid(), Target :: pid()) -> 'ok' | 'invalid').
spy(Spy, Target) ->
	gen_fsm:sync_send_event(Spy, {spy, Target}).

%% @doc Make the agent connection try to pop a given url.
-spec(url_pop/3 :: (Pid :: pid(), Url :: string(), Name :: string()) -> 'ok').
url_pop(Pid, Url, Name) ->
	gen_fsm:send_all_state_event(Pid, {url_pop, Url, Name}).

%% @doc Translate the state `String' into the internally used atom.  `String' can either be the human readable string or a number in string form (`"1"').
-spec(list_to_state/1 :: (String :: string()) -> atom()).
list_to_state(String) ->
	Atom = try erlang:list_to_existing_atom(String) of
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
%init([Agent, Options]) when is_record(Agent, agent) ->
%	%{ok, MaxRingouts} = cpx:get_env(max_ringouts, infinity),
%	ProtoState = #state{
%		agent_fsm = Agent#agent.source,
%		agent_connection = Agent#agent.connection
%	},
%	InitInfo = proplists:get_value(initial_state, Options, {prering, undefined}),
%	case InitInfo of
%		{prering, Call} when is_record(Call, call); Call =:= undefined ->
%			State = ProtoState#state{state_data = Call},
%			?DEBUG("Starting in prering", []),
%			{ok, prering, State};
%		{ringing, Call} when is_record(Call, call) ->
%			State = ProtoState#state{state_data = Call},
%			?DEBUG("Starting in ring directly", []),
%			{ok, ringing, State};
%		{precall, Client} when is_record(Client, client) ->
%			State = ProtoState#state{state_data = Client},
%			?DEBUG("Starting in precall", []),
%			{ok, precall, State};
%		_ ->
%			?WARNING("Failed start:  ~p", [InitInfo]),
%			{stop, badstate}
%	end;

init([Agent, Call, Endpoint, StateName]) ->
	process_flag(trap_exit, true),
	State = #state{
		agent_fsm = Agent#agent.source,
		agent_connection = Agent#agent.connection,
		endpoint = Endpoint,
		state_data = Call
	},
	case StateName of
		prering when is_record(Call, call); Call =:= undefined ->
			case start_endpoint(Endpoint, Agent, Call) of
				{ok, Pid} ->
					?DEBUG("Starting in prering", []),
					conn_cast(Agent, {set_channel, self(), prering, Call}),
					cpx_agent_event:agent_channel_init(Agent,self(),prering,Call),
					{ok, prering, State#state{endpoint = Pid}};
				{error, Error} ->
					{stop, {error, Error}}
			end;
		precall when is_record(Call, client) ->
			?DEBUG("Starting in precall", []),
			conn_cast(Agent, {set_channel, self(), precall, Call}),
			cpx_agent_event:agent_channel_init(Agent,self(),precall,Call),
			{ok, precall, State};
		precall when is_record(Call, call) ->
			?DEBUG("Starting in precall with media rather than client", []),
			conn_cast(Agent, {set_channel, self(), precall, Call}),
			cpx_agent_event:agent_channel_init(Agent, self(), precall, Call),
			{ok, precall, State};
		ringing when is_record(Call, call) ->
			% TODO tell media to ring
			?DEBUG("Starting in ringing", []),
			conn_cast(Agent, {set_channel, self(), ringing, Call}),
			cpx_agent_event:agent_channel_init(Agent,self(),ringing, Call),
			{ok, ringing, State};
		_ ->
			?WARNING("Failed start:  ~p", [{StateName, Call}]),
			{stop, badstate}
	end.
	
		
% ======================================================================
% PRERING
% ======================================================================

prering({ringing, Call}, From, State) ->
	% TODO check if valid
	?DEBUG("Moving from prering to ringing state request from ~p", [From]),
	conn_cast(State#state.agent_connection, {set_channel, self(), ringing, Call}),
	cpx_agent_event:change_agent_channel(self(), ringing, Call),
	{reply, ok, ringing, State#state{state_data = Call}};
prering(Msg, _From, State) ->
	?INFO("Msg ~p not understood", [Msg]),
	{reply, {error, invalid}, prering, State}.

%% -----

prering(_Msg, State) ->
	{next_state, prering, State}.

% ======================================================================
% RINGING
% ======================================================================

ringing(oncall, {Conn, _}, #state{agent_connection = Conn, endpoint = inband} = State) ->
	#call{source = Media} = Call = State#state.state_data,
	case gen_media:oncall(Media) of
		ok ->
			conn_cast(Conn, {set_channel, self(), oncall, Call}),
			cpx_agent_event:change_agent_channel(self(), oncall, Call),
			?DEBUG("Moving from ringing to oncall state", []),
			{reply, ok, oncall, State};
		Else ->
			?WARNING("Didn't go oncall:  ~p", [Else]),
			{reply, {error, Else}, ringing, State}
	end;

ringing(oncall, {Conn, _}, #state{agent_connection = Conn, endpoint = Pid, state_data = #call{ring_path = inband}} = State) ->
	#call{source = Media} = Call = State#state.state_data,
	case gen_media:oncall(Media) of
		ok ->
			conn_cast(Conn, {set_channel, self(), oncall, Call}),
			cpx_agent_event:change_agent_channel(self(), oncall, Call),
			NewEndpoint = case Call#call.media_path of
				inband ->
					erlang:exit(Pid, normal),
					undefined;
				_ ->
					Pid
			end,
			NewState = State#state{endpoint = NewEndpoint},
			?DEBUG("Moving from ringing to oncall state", []),
			{reply, ok, oncall, NewState};
		Else ->
			?WARNING("Didn't go oncall:  ~p", [Else]),
			{reply, {error, Else}, ringing, State}
	end;

ringing({oncall, Call}, _From, #state{state_data = Call} = State) ->
	?DEBUG("Moving from ringing to oncall state", []),
	conn_cast(State#state.agent_connection, {set_channel, self(), oncall, Call}),
	cpx_agent_event:change_agent_channel(self(), oncall, Call),
	{reply, ok, oncall, State};

ringing(_Msg, _From, State) ->
	{reply, {error, invalid}, ringing, State}.

%% -----

ringing(_Msg, State) ->
	{next_state, ringing, State}.

% ======================================================================
% PRECALL
% ======================================================================

precall({oncall, #call{client = Client} = Call}, _From, #state{state_data = Client} = State) ->
	?DEBUG("Moving from precall to oncall state", []),
	conn_cast(State#state.agent_connection, {set_channel, self(), oncall, Call}),
	cpx_agent_event:change_agent_channel(self(), oncall, Call),
	{reply, ok, oncall, State#state{state_data = Call}};

precall({oncall, #call{id = Id} = Call}, _From, #state{state_data = #call{id = Id}} = State) ->
	?DEBUG("Moving from precall to oncall", []),
	conn_cast(State#state.agent_connection, {set_channel, self(), oncall, Call}),
	cpx_agent_event:change_agent_channel(self(), oncall, Call),
	{reply, ok, oncall, State#state{state_data = Call}};

precall(_Msg, _From, State) ->
	{reply, {error, invalid}, precall, State}.

%% -----

precall({mediapush, From, Callrec, Data}, #state{state_data = #call{source = From}, agent_connection = Conn} = State) when is_pid(Conn) ->
	Self = self(),
	gen_server:cast(Conn, {mediapush, Self, Callrec, Data}),
	{next_state, precall, State};

precall(_Msg, State) ->
	{next_state, precall, State}.

% ======================================================================
% ONCALL
% ======================================================================

% TODO the two clauses below are no longer used as warmtransfer has been
% moved to a media specific set-up.
oncall(warmtransfer_hold, _From, State) ->
	?DEBUG("Moving from oncall to warmtransfer_hold", []),
	conn_cast(State#state.agent_connection, {set_channel, self(), warmtransfer_hold, State#state.state_data}),
	{reply, ok, warmtransfer_hold, State};
oncall({warmtransfer_3rd_party, Data}, From, State) ->
	case oncall(warmtransfer_hold, From, State) of
		{reply, ok, warmtransfer_hold, NewState} ->
			warmtransfer_hold({warmtransfer_3rd_party, Data}, From, NewState);
		Else ->
			Else
		end;

%% -----

oncall(wrapup, From, #state{state_data = Call} = State) ->
	oncall({wrapup, Call}, From, State);

oncall({wrapup, Call}, {From, _Tag}, #state{state_data = Call} = State) ->
	case Call#call.source of
		From ->
			?DEBUG("Moving from oncall to wrapup", []),
			conn_cast(State#state.agent_connection, {set_channel, self(), wrapup, Call}),
			cpx_agent_event:change_agent_channel(self(), wrapup, Call),
			{reply, ok, wrapup, State#state{state_data = Call}};
		CallSource ->
			case gen_media:wrapup(CallSource) of
				ok ->
					?DEBUG("Moving from oncall to wrapup", []),
					conn_cast(State#state.agent_connection, {set_channel, self(), wrapup, Call}),
					cpx_agent_event:change_agent_channel(self(), wrapup, Call),
					{reply, ok, wrapup, State#state{state_data = Call}};
				Else ->
					{reply, Else, oncall, State}
			end
	end;

oncall(_Msg, _From, State) ->
	{reply, {error, invalid}, oncall, State}.

%% -----

oncall({mediapush, From, Callrec, Data}, #state{state_data = #call{source = From}, agent_connection = Conn} = State) when is_pid(Conn) ->
	Self = self(),
	gen_server:cast(Conn, {mediapush, Self, Callrec, Data}),
	{next_state, oncall, State};

oncall(_Msg, State) ->
	{next_state, oncall, State}.

% ======================================================================
% WARMTRANSFER_HOLD
% ======================================================================

% TODO depricated state
warmtransfer_hold(oncall, _From, #state{state_data = Call} = State) ->
	?DEBUG("Moving from warmtransfer_hold to oncall", []),
	conn_cast(State#state.agent_connection, {set_channel, self(), oncall, Call}),
	{reply, ok, oncall, State};
warmtransfer_hold({warmtransfer_3rd_party, Data}, _From, #state{state_data = Call} = State) ->
	?DEBUG("Moving from warmtransfer_hold to warmtransfer_3rd_party", []),
	conn_cast(State#state.agent_connection, {set_channel, self(), warmtransfer_3rd_party, {Call, Data}}),
	{reply, ok, warmtransfer_3rd_party, State#state{state_data = {Call, Data}}};
warmtransfer_hold(wrapup, _From, State) ->
	?DEBUG("Moving from warmtransfer_hold to wrapup", []),
	conn_cast(State#state.agent_connection, {set_channel, self(), wrapup, State#state.state_data}),
	{reply, ok, wrapup, State};
warmtransfer_hold(_Msg, _From, State) ->
	{reply, {error, invalid}, warmtransfer_hold, State}.

warmtransfer_hold(_Msg, State) ->
	{next_state, warmtransfer_hold, State}.

% ======================================================================
% WARMTRANSFER_3RD_PARTY
% ======================================================================

% TODO depricated state
warmtransfer_3rd_party(warmtransfer_hold, _From, #state{state_data = {Call, _}} = State) ->
	?DEBUG("Moving from warmtransfer_3rd_party to warmtransfer_hold", []),
	conn_cast(State#state.agent_connection, {set_channel, self(), warmtransfer_hold, Call}),
	{reply, ok, warmtransfer_hold, State#state{state_data = Call}};
warmtransfer_3rd_party(oncall, _From, #state{state_data = {Call, _}} = State) ->
	?DEBUG("Moving from warmtransfer_3rd_party to oncall", []),
	conn_cast(State#state.agent_connection, {set_channel, self(), oncall, Call}),
	{reply, ok, oncall, State#state{state_data = Call}};
warmtransfer_3rd_party(wrapup, _From, #state{state_data = {Call, _}} = State) ->
	?DEBUG("Moving from warmtransfer_3rd_party to wrapup", []),
	conn_cast(State#state.agent_connection, {set_channel, self(), wrapup, Call}),
	{reply, ok, wrapup, State#state{state_data = Call}};
warmtransfer_3rd_party(_Msg, _From, State) ->
	{reply, {error, invalid}, State}.

warmtransfer_3rd_party(_Msg, State) ->
	{next_state, warmtransfer_3rd_party, State}.

% ======================================================================
% WRAPUP
% ======================================================================

% no calls to the cpx_agent_event as monitoring should be enough.
wrapup(stop, _From, State) ->
	{stop, normal, ok, State};
wrapup(_Msg, _From, State) ->
	{reply, ok, wrapup, State}.

wrapup(stop, State) ->
	{stop, normal, State};
wrapup(_Msg, State) ->
	{next_state, wrapup, State}.


% ======================================================================
% HANDLE_EVENT
% ======================================================================

handle_event(_Event, StateName, State) ->
	{next_state, StateName, State}.

% ======================================================================
% HANDLE_SYNC_EVENT
% ======================================================================

handle_sync_event(query_state, _From, StateName, State) ->
	{reply, {ok, StateName}, StateName, State};

handle_sync_event({set_connection, Pid}, _From, StateName, #state{agent_connection = _AgentConn} = State) ->
	gen_server:cast(Pid, {set_channel, self(), StateName, State#state.state_data}),
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

handle_info({'EXIT', Pid, Why}, wrapup, #state{endpoint = Pid} = State) ->
	?INFO("Exit of endpoint ~p due to ~p while in wrapup; ignorable", [Pid, Why]),
	{next_state, wrapup, State};

handle_info({'EXIT', Pid, Why}, oncall, #state{endpoint = Pid} = State) ->
	?INFO("Exit of endpoint ~p due to ~p while oncall; moving to wrapup.", [Pid, Why]),
	Callrec = State#state.state_data,
	conn_cast(State#state.agent_connection, {set_channel, self(), wrapup, Callrec}),
	cpx_agent_event:change_agent_channel(self(), wrapup, Callrec),
	CallPid = Callrec#call.source,
	case gen_media:wrapup(CallPid) of
		ok ->
			{next_state, wrapup, State};
		Else ->
			?WARNING("could not set gen_media to wrapup:  ~p", [Else]),
			{next_state, oncall, State}
	end;
	
handle_info({'EXIT', Pid, Why}, StateName, #state{endpoint = Pid} = State) ->
	?INFO("Exit of endpoint ~p due to ~p in state ~s", [Pid, Why, StateName]),
	{stop, Why, State};

handle_info({'EXIT', Pid, Why}, _StateName, #state{agent_fsm = Pid} = State) ->
	?INFO("Exit of agent fsm due to ~p", [Why]),
	{stop, Why, State};

handle_info(end_wrapup, wrapup, State) ->
	{stop, normal, State};

handle_info(_Info, StateName, State) ->
	{next_state, StateName, State}.

% ======================================================================
% TERMINATE
% ======================================================================

terminate(_Reason, _StateName, _State) ->
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

conn_cast(Agent, Msg) when is_record(Agent, agent) ->
	conn_cast(Agent#agent.connection, Msg);
conn_cast(undefined, _Msg) ->
	ok;
conn_cast(Conn, Msg) when is_pid(Conn) ->
	gen_server:cast(Conn, Msg).

start_endpoint(Pid, Agent, Call) when is_pid(Pid) ->
	link(Pid),
	Pid ! {prering, {Agent, self()}, Call},
	{ok, Pid};
start_endpoint({Mod, Func, XtraArgs}, Agent, Call) ->
	case apply(Mod, Func, [Agent, self(), Call | XtraArgs]) of
		{ok, Pid} ->
			link(Pid),
			{ok, Pid};
		Else ->
			{error, Else}
	end;
start_endpoint(E, _, _) ->
	{error, {badendpoint, E}}.

% ======================================================================
% TESTS
% ======================================================================
	
-ifdef(TEST).

public_api_test_() ->
	{foreach, fun() ->
		meck:new(gen_fsm, [unstick])
	end,
	fun(_) ->
		meck:unload(gen_fsm)
	end, [
	
	fun(_) -> {"start/2, simple_sucess", fun() ->
		meck:expect(gen_fsm, start, fun(?MODULE, [agentrecord, options], []) ->
			?assert(true)
		end),

		start(agentrecord, options),
		?assertEqual(1, length(meck:history(gen_fsm))),
		?assert(meck:validate(gen_fsm))	
	end} end,

	fun(_) -> {"start/4, simple_sucess", fun() ->
		meck:expect(gen_fsm, start, fun(?MODULE, [agentrecord, callrecord,
			endpointdata, initstate]) ->
			?assert(true)
		end),

		start(agentrecord, callrecord, endpointdata, initstate),
		?assertEqual(1, length(meck:history(gen_fsm))),
		?assert(meck:validate(gen_fsm))	
	end} end,

	fun(_) -> {"start_link/2, simple_sucess", fun() ->
		meck:expect(gen_fsm, start_link, fun(?MODULE, [agentrecord, options], []) ->
			?assert(true)
		end),

		start_link(agentrecord, options),
		?assertEqual(1, length(meck:history(gen_fsm))),
		?assert(meck:validate(gen_fsm))	
	end} end,

	fun(_) -> {"start_link/4, simple_sucess", fun() ->
		meck:expect(gen_fsm, start_link, fun(?MODULE, [agentrecord,
			callrecord, endpointdata, initstate], []) ->
			?assert(true)
		end),

		start_link(agentrecord, callrecord, endpointdata, initstate),
		?assertEqual(1, length(meck:history(gen_fsm))),
		?assert(meck:validate(gen_fsm))	
	end} end,

	fun(_) -> {"stop/1, simple_sucess", fun() ->
		meck:expect(gen_fsm, send_all_state_event, fun(pid, stop) ->
			?assert(true)
		end),

		stop(pid),
		?assertEqual(1, length(meck:history(gen_fsm))),
		?assert(meck:validate(gen_fsm))
	end} end

	]}.

-endif.
