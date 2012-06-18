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
	ring_agent/2, ring_agent/3
]).

-record(state, {
	fsnode,
	agent_info,
	ring_info
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

%% =====================================================================
%% Gen_media callbacks
%% =====================================================================

%% ---------------------------------------------------------------------
%% Init
%% ---------------------------------------------------------------------

init([FsNode, BaseCall]) ->
	process_flag(trap_exit, true),
	{ok, {#state{fsnode = FsNode}, BaseCall}}.

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

handle_answer(_Agent, _Call, State) ->
	{ok, State}.

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

handle_call(Msg, _From, _Call, State) ->
	?WARNING("Unhandled call ~p", [Msg]),
	{reply, {error, invalid}, State}.

%% ---------------------------------------------------------------------
%% handle_cast
%% ---------------------------------------------------------------------

handle_cast(Msg, _Call, State) ->
	?WARNING("Unhandled cast ~p", [Msg]),
	{noreply, State}.

%% ---------------------------------------------------------------------
%% handle_info
%% ---------------------------------------------------------------------

handle_info({'EXIT', Rpid, Cause}, _Call, #state{ring_info = {Rpid, _Ruuid}} = State) ->
	?DEBUG("Hangup of ring channel due to ~p indicates unbusy agent", [Cause]),
	{{hangup, agent}, State};

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
