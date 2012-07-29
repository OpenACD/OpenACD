-module(freeswitch_busy_agent).
-author("Micah").

-behaviour(gen_media).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("queue.hrl").
-include("call.hrl").
-include("agent.hrl").

% gen_media
-export([
	init/1,
	%urlpop_getvars/1,
	handle_ring/3,
	handle_ring_stop/2,
	handle_answer/3,
	handle_oncall_transition_accept/5,
%% TODO added for testing only (implemented with focus on real Calls - no other media)
	handle_end_call/2,
	handle_agent_transfer/4, % always fail.
	handle_queue_transfer/2, % TODO can't fail this.  If this pops up, crash.
	handle_wrapup/2,
	handle_call/4, 
	handle_cast/3, 
	handle_info/3,
	terminate/3,
	code_change/4]).

% api
-export([
	start/2, start_link/2,
	ring_agent/2, ring_agent/3,
	cancel/1,
	transfer/2,
	transfer/3,
	get_other/1,
	set_other/2,

	% exported as a cede control helper
	set_state_agent/5
]).

-record(state, {
	fsnode,
	other_media,
	agent_info,
	ring_info,
	recording_path
}).

%% =====================================================================
%% API
%% =====================================================================

start(FsNode, BaseCall) ->
	gen_media:start(?MODULE, [FsNode, BaseCall]).

start_link(FsNode, BaseCall) ->
	gen_media:start_link(?MODULE, [FsNode, BaseCall]).

ring_agent(Media, Apid) ->
	ring_agent(Media, Apid, 5000).

ring_agent(Media, Apid, Timeout) ->
	Self = self(),
	Call = gen_media:get_call(Media),
	QCallFake = #queued_call{id = Call#call.id, media = Media, cook = Self},
	gen_media:ring(Media, Apid, QCallFake, Timeout).

cancel(Media) ->
	gen_media:cast(Media, cancel).

transfer(Media, Endpoint) ->
	transfer(Media, Endpoint, undefined).

transfer(Media, Endpoint, SendEvent) ->
	gen_media:cast(Media, {transfer, Endpoint, SendEvent}).

set_state_agent(Login, Apid, RingChanPid, RingChanUUID, State) ->
	State#state{agent_info = {Apid, Login}, ring_info = {RingChanPid, RingChanUUID}}.

get_other(Pid) ->
	gen_media:call(Pid, get_other).

set_other(Pid, OtherPid) ->
	gen_media:cast(Pid, {set_other, OtherPid}).

%% =====================================================================
%% Gen_media callbacks
%% =====================================================================

%% ---------------------------------------------------------------------
%% Init
%% ---------------------------------------------------------------------

init([FsNode, BaseCall]) ->
	process_flag(trap_exit, true),
	link(BaseCall#call.source),
	{ok, {#state{fsnode = FsNode, other_media = BaseCall#call.source}, BaseCall}}.

%% ---------------------------------------------------------------------
%% handle_ring
%% ---------------------------------------------------------------------

handle_ring(Agent, Call, State) when is_pid(Agent) ->
	AgentRec = agent:dump_state(Agent),
	handle_ring({Agent, AgentRec}, Call, State);

handle_ring({_Apid, #agent{endpointtype = {undefined, persistent, _EdnPointType}} = AgentRec}, _Call, State) ->
	?WARNING("agent ~s doesn't have thier persistent ring up yet", [AgentRec#agent.login]),
	{invalid, State};

handle_ring({Apid, #agent{endpointtype = {EndpointPid, persistent, _EndpointType}} = Agent}, Call, State) ->
	?INFO("Letting persistent ring channel handle actuall oncall", []),
	Call0 = Call#call{ring_path = inband, media_path = inband},
	Ruuid = freeswitch_ring:get_uuid(EndpointPid),
	State0 = State#state{agent_info = {Apid, Agent}, ring_info = {EndpointPid, Ruuid}},
	{ok, Call0, State0};

handle_ring({Apid, #agent{endpointtype = {Rpid, transient, _}} = Agent}, _Call, State) ->
	?DEBUG("linking to existing transient ring pid ~p", [Rpid]),
	link(Rpid),
	Ruuid = freeswitch_ring:get_uuid(Rpid),
	State0 = State#state{agent_info = {Apid, Agent}, ring_info = {Rpid, Ruuid}},
	{ok, State0}.

%% ---------------------------------------------------------------------
%% handle_ring_stop
%% ---------------------------------------------------------------------

handle_ring_stop(_Call, State) ->
	?ERROR("NYI", []),
	{ok, State}.

%% ---------------------------------------------------------------------
%% handle_answer
%% ---------------------------------------------------------------------

handle_answer(_Agent, Call, State) ->
	#state{other_media = OtherMedia, ring_info = {_Rpid, Ruuid} = RingInfo, agent_info = {_Apid, Agent} } = State,
	Login = Agent#agent.login,
	RecPath = case cpx_supervisor:get_archive_path(Call) of
		none ->
			?DEBUG("archiving is not configured for ~p", [Call#call.id]),
			undefined;
		{error, _Reason, Path} ->
			?WARNING("Unable to create requested call archiving directory for recording ~p for ~p", [Path, Call#call.id]),
			undefined;
		Path ->
			%% get_archive_path ensures the directory is writeable by us and exists, so this
			%% should be safe to do (the call will be hungup if creating the recording file fails)
			?DEBUG("archiving ~p to ~s.wav", [Call#call.id, Path]),
			freeswitch:bgapi(State#state.fsnode, uuid_setvar, Ruuid ++ " RECORD_APPEND true"),
			freeswitch:bgapi(State#state.fsnode, uuid_record, Ruuid ++ " start " ++ Path ++ "." ++ Login ++ "." ++ ".wav"),
			Path
	end,
	?INFO("handling answer, telling other media:  ~p", [OtherMedia]),
	gen_media:cast(OtherMedia, {?MODULE, answer, Call, RingInfo}),
	{ok, State#state{recording_path = RecPath}}.

%% ---------------------------------------------------------------------
%% handle_oncall_transition_accept
%% ---------------------------------------------------------------------

handle_oncall_transition_accept(freeswitch_media, FsMediaState, {From, _Tag}, Call, #state{other_media = From} = State) ->
	#state{agent_info = {Login, Apid}, ring_info = {RingChan, RingUUID}} = State,
	FsState0 = freeswitch_media:set_state_agent(Login, Apid, RingChan, RingUUID, FsMediaState),
	FsState1 = freeswitch_media:set_3rd_party(From, FsState0),
	freeswitch:handlecall(State#state.fsnode, Call#call.id),
	{ok, FsState1};

handle_oncall_transition_accept(Mod, ModState, {From, _Tag}, _Call, State) ->
	?WARNING("One of Mod, ModState, or From unacceptable:  ~p, ~p, ~p", [Mod, ModState, From]),
	{error, invalid, State}.

%% ---------------------------------------------------------------------
%% handle_end_call
%% ---------------------------------------------------------------------

handle_end_call(_Call, State) ->
	{error, nyi, State}.

%% ---------------------------------------------------------------------
%% handle_agent_transfer
%% ---------------------------------------------------------------------

handle_agent_transfer(_Apid, _Timeout, _Call, State) ->
	{error, nyi, State}.

%% ---------------------------------------------------------------------
%% handle_queue_transfer
%% ---------------------------------------------------------------------

handle_queue_transfer(_Call, _State) ->
	throw(not_queueable).

%% ---------------------------------------------------------------------
%% handle_wrapup
%% ---------------------------------------------------------------------

handle_wrapup(_Call, State) ->
	{hangup, State}.

%% ---------------------------------------------------------------------
%% handle_call
%% ---------------------------------------------------------------------

handle_call(get_other, _From, _Call, State) ->
	{reply, State#state.other_media, State};

handle_call(Msg, _From, _Call, State) ->
	?WARNING("Unhandled call ~p", [Msg]),
	{reply, {error, invalid}, State}.

%% ---------------------------------------------------------------------
%% handle_cast
%% ---------------------------------------------------------------------

handle_cast({set_other, Other}, _Call, State) ->
	{noreply, State#state{other_media = Other}};

handle_cast({transfer, Endpoint}, Call, State) ->
	handle_cast({transfer, Endpoint, undefined}, Call, State);

handle_cast({transfer, Endpoint, SendToAgent}, Call, State) ->
	#state{ring_info = {_Pid, UUID}, fsnode = Fnode} = State,
	?DEBUG("Got call to do transfer to ~s", [Endpoint]),
	%Any = freeswitch_ring:block_until(Pid, any),
	%?DEBUG("unblocked on ~p", [Any]),
	freeswitch:api(Fnode, uuid_transfer, UUID ++ " " ++ Endpoint),
	case SendToAgent of
		undefined ->
			{noreply, State};
		_ ->
			{{mediapush, SendToAgent}, State}
	end;

handle_cast(cancel, _Call, State) ->
	?INFO("Stopping due to cancel request", []),
	{stop, normal, State};

handle_cast(Msg, _Call, State) ->
	?WARNING("Unhandled cast ~p", [Msg]),
	{noreply, State}.

%% ---------------------------------------------------------------------
%% handle_info
%% ---------------------------------------------------------------------

handle_info({'EXIT', Rpid, Cause}, _Call, #state{ring_info = {Rpid, _Ruuid}} = State) ->
	?DEBUG("Hangup of ring channel due to ~p indicates unbusy agent", [Cause]),
	{stop, {hangup, agent}, State};

handle_info(Msg, _Call, State) ->
	?DEBUG("unhandled info:  ~p", [Msg]),
	{noreply, State}.

%% ---------------------------------------------------------------------
%% termiante
%% ---------------------------------------------------------------------

terminate(Cause, _Call, _State) ->
	?INFO("Exit due to ~p", [Cause]).

%% ---------------------------------------------------------------------
%% code_change
%% ---------------------------------------------------------------------

code_change(_OldVsn, _Call, State, _Extra) ->
	{ok, State}.

%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% TESTS
%% =====================================================================
