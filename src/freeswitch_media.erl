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

%% @doc The gen_media callback module for voice calls through freeswitch.  
%% @see freeswitch_media_manager

-module(freeswitch_media).
-author("Micah").

-behaviour(gen_media).

-ifdef(TEST).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("queue.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("cpx_freeswitch_pb.hrl").

-define(TIMEOUT, 10000).

-define(DEFAULT_PRIORITY, 10).
-define(DEFAULT_VM_PRIORITY_DIFF, 10).

%% API
-export([
	start/3,
	start_link/3,
	get_call/1,
	%get_queue/1,
	%get_agent/1,
	%unqueue/1,
	%set_agent/3,
	dump_state/1,
	statename/1,
	statedata/1,
	'3rd_party_pickup'/1,
	spy_observe_only/1,
	spy_whisper/2,
	spy_whisper_caller/1,
	spy_whisper_agent/1,
	spy_barge/1,
	spy_single_step/3,

	toggle_hold/1,
	contact_3rd_party/2,
	contact_3rd_party/3,
	contact_3rd_party/4,
	contact_agent/2,
	contact_agent/3,
	retrieve_conference/1,
	retrieve_3rd_party/1,
	hangup_3rd_party/1,
	merge_3rd_party/2,
	merge_all/1,
	merge_only_3rd_party/1,
	end_conference/1,

	conference_status/1,
	conference_mute/2,
	conference_deaf/2,
	conference_unmute/2,
	conference_undeaf/2,
	conference_kick/2,
	conference_command/2,

	complete_agent_transfer/1,
	cancel_agent_transfer/1,
	cede_control/2,

	% helper for the cede control:
	set_state_agent/5
	]).

%% gen_media callbacks
-export([
	init/1,
	urlpop_getvars/1,
	handle_ring/3,
	handle_ring_stop/2,
	handle_answer/3,
	handle_voicemail/3,
	handle_spy/3,
	handle_announce/3,
%% TODO added for testing only (implemented with focus on real Calls - no other media)
	handle_end_call/2,
	handle_agent_transfer/4,
	handle_queue_transfer/2,
	handle_wrapup/2,
	handle_call/4, 
	handle_cast/3, 
	handle_info/3,
	handle_warm_transfer_begin/3,
	handle_warm_transfer_cancel/2,
	handle_warm_transfer_complete/2,
	handle_oncall_transition_accepted/4,
	terminate/3,
	code_change/4]).

-type(internal_statename() ::
	'inivr' | % never been in queue.
	'inqueue' | % waiting for an agent to call
	'inqueue_ringing' | % got an agent, waiting for the anser
	'oncall' | % oncall with an agent.
	'oncall_ringing' | % starting an agent transfer
	'oncall_hold' | % simple hold
	'oncall_hold_ringing' | % doing an agent transfer while caller on hold
	'hold_conference' | % has a conference, but agent (self) not a member
	'hold_conference_3rdparty' | % a conference is up, as is a call to a
		% 3rd party, but agent is member of neither
	'in_conference_3rdparty' | % agent is a member of the conference, 
		% but there is a 3rd party in limbo.
	'3rd_party' | % there is a conference, but agent is talking w/ 3rd party.
	'in_conference' | % agent is a member of the conference, no limbo party
	'wrapup_conference' | % there is no agent
	'blind_transfered'
).

-define(cdr_states, ['oncall', 'oncall_hold', 'oncall_hold_ringing',
	'hold_conference', 'hold_conference_3rdparty', 'in_conference_3rdparty', 
	'3rd_party', 'agent_contact', 'in_conference', 'wrapup_conference',
	'blind_transfered', 'inqueue', 'inqueue_ringing', 'inivr']).

-record(state, {
	statename :: internal_statename(),
	uuid :: string(),
	cook :: pid() | 'undefined',
	queue :: string() | 'undefined',
	cnode :: atom(),
	dialstring :: string(),
	agent :: string() | 'undefined',
	agent_pid :: pid() | 'undefined',
	ringchannel :: pid() | 'undefined',
	ringuuid :: string() | 'undefined',
	manager_pid :: 'undefined' | any(),
	voicemail = false :: 'false' | string(),
	xferchannel :: pid() | 'undefined',
	xferuuid :: string() | 'undefined',
	in_control = false :: boolean(),
	queued = false :: boolean(),
	allow_voicemail = false :: boolean(),
	vm_priority_diff = ?DEFAULT_VM_PRIORITY_DIFF :: integer(),
	warm_transfer_uuid = undefined :: string() | 'undefined',
	ivroption :: string() | 'undefined',
	caseid :: string() | 'undefined',
	moh = "moh" :: string() | 'none',
	record_path :: 'undefined' | string(),
	dial_vars :: list(),
	hold :: 'undefined' | 'hold',
	spawn_oncall_mon :: 'undefined' | {pid(), reference()},
	conference_id :: 'undefined' | string(),
	'3rd_party_id' :: 'undefined' | string(),
	'3rd_party_mon' :: 'undefined' | {pid(), reference()},
	spy_channel :: 'undefined' | {pid(), pid(), string()},
		% {agent_pid(), agent_connection_pid(), channel_uuid()}
	next_state :: 'undefined' | internal_statename()
}).

-type(state() :: #state{}).
-define(GEN_MEDIA, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================
%% @doc starts the freeswitch media gen_server.  `Cnode' is the C node the communicates directly with freeswitch.
-spec(start/3 :: (Cnode :: atom(), DialString :: string(), UUID :: string()) -> {'ok', pid()}).
start(Cnode, DialString, UUID) ->
	gen_media:start(?MODULE, [Cnode, DialString, UUID]).

-spec(start_link/3 :: (Cnode :: atom(), DialString :: string(), UUID :: string()) -> {'ok', pid()}).
start_link(Cnode, DialString, UUID) ->
	gen_media:start_link(?MODULE, [Cnode, DialString, UUID]).

%% @doc returns the record of the call freeswitch media `MPid' is in charge of.
-spec(get_call/1 :: (MPid :: pid()) -> #call{}).
get_call(MPid) ->
	gen_media:get_call(MPid).

-spec(dump_state/1 :: (Mpid :: pid()) -> #state{}).
dump_state(Mpid) when is_pid(Mpid) ->
	gen_media:call(Mpid, dump_state).

-spec(statename/1 :: (Mpid :: pid()) -> atom()).
statename(Mpid) when is_pid(Mpid) ->
	State = dump_state(Mpid),
	State#state.statename.

-spec(statedata/1 :: (Mpid :: pid()) -> #state{}).
statedata(Mpid) when is_pid(Mpid) ->
	case dump_state(Mpid) of
		State when is_record(State, state) ->
			Fields = record_info(fields, state),
			[{lists:nth(N, Fields), element(N + 1, State)} || N <- lists:seq(1, length(Fields))];
		State when is_list(State) ->
			State;
		Else ->
			Else
	end.

'3rd_party_pickup'(Mpid) ->
	Self = self(),
	gen_media:cast(Mpid, {'3rd_party_pickup', Self}).

%% @doc When a supervisor has started a spy, this can be used instead of
%% the dtmf on the softphone to whisper.  `agent' means only the agent
%% will hear the supervisor.  `caller' does only the caller.  `both' and
%% `none' do what is said on the tin.
-spec(spy_whisper/2 :: (MPid :: pid(), Who :: 'agent' | 'caller' | 'both' | 'none') -> 'ok' | {'error', 'not_spy'}).
spy_whisper(MPid, Who) ->
	gen_media:call(MPid, {modify_spy, Who}).

%% @doc same as spy_whisper(MPid, none).
-spec(spy_observe_only/1 :: (MPid :: pid()) -> 'ok' | {'error', 'not_spy'}).
spy_observe_only(MPid) ->
	spy_whisper(MPid, none).

%% @doc same as spy_whisper(MPid, caller).
-spec(spy_whisper_caller/1 :: (MPid :: pid()) -> 'ok' | {'error', 'not_spy'}).
spy_whisper_caller(MPid) ->
	spy_whisper(MPid, caller).

%% @doc same as spy_whisper(MPid, agent).
-spec(spy_whisper_agent/1 :: (MPid :: pid()) -> 'ok' | {'error', 'not_spy'}).
spy_whisper_agent(MPid) ->
	spy_whisper(MPid, agent).

%% @doc same as spy_whisper(MPid, both).
-spec(spy_barge/1 :: (MPid :: pid()) -> 'ok' | {'error', 'not_spy'}).
spy_barge(MPid) ->
	spy_whisper(MPid, both).

%% @doc Skips past gen_media:spy/3 to do an eavesdrop on it's own.  The
%% other spy functions will still work to modify the spy.  This allows the
%% spy to start in whisper_caller, whisper_agent, observe_only, or
%% barge rather then the default observe_only.  This is set by the `Who'
%% option
-spec(spy_single_step/3 :: (MPid :: pid(), SpyerInfo :: #agent{}, Who :: 'both' | 'none' | 'caller' | 'agent') -> 'ok').
spy_single_step(MPid, SpyerInfo, Who) ->
	gen_media:call(MPid, {spy, SpyerInfo, Who}, infinity).

%% @doc Puts the current channel on hold.  The exact nature depends on the
%% state the call was in.  If it's simply just the agent and caller, can
%% be used to hold and retrieve the caller.  If a conference is in
%% progress, can only be used to take the agent out of the conference
%% temporarily, or put the 3rd party on hold.
%%
%% Conferencing can be used as an alternative to a warm transfer.  When
%% the conference is started, the agent can hangup without completely
%% killing the call.
-spec(toggle_hold/1 :: (MPid :: pid()) -> 'ok').
toggle_hold(MPid) ->
	gen_media:cast(MPid, toggle_hold).

%% @doc If the caller has been put on hold, this implicitly starts a
%% conference.  Calls out to the 3rd party and switches the agent to that
%% call.
-spec(contact_3rd_party/2 :: (MPid :: pid(), Destination :: string()) -> 'ok').
contact_3rd_party(MPid, Destination) ->
	contact_3rd_party(MPid, Destination, '3rd_party').

%% @doc From the states oncall_hold, in_conference, and hold_conference, 
%% contact the 3rd party.  Afterwards, either go in_conference, 
%% 3rd_party, hold_conference-3rdparty, or hold_conference.
-spec(contact_3rd_party/3 :: (MPid :: pid(), Destination :: string(), NextState :: 'in_conference' | '3rd_party' | 'hold_conference_3rdparty' | 'hold_conference' | string()) -> 'ok').
contact_3rd_party(MPid, Destination, NextState) when NextState =:= 'in_conference'; NextState =:= '3rd_party'; NextState =:= 'hold_conference_3rdparty'; NextState =:= 'hold_conference' ->
	{ok, ConfProf} = cpx:get_env(freeswitch_conference_profile, "default"),
	contact_3rd_party(MPid, Destination, NextState, ConfProf);
contact_3rd_party(MPid, Destination, Profile) when is_list(Profile) ->
	contact_3rd_party(MPid, Destination, '3rd_party', Profile).

%% @doc And now even the conference profile can be defined.  Note that if
%% a conference is already underway, the profile is ignored.
contact_3rd_party(MPid, Destination, NextState, ConfProf) when NextState =:= 'in_conference'; NextState =:= '3rd_party'; NextState =:= 'hold_conference_3rdparty'; NextState =:= 'hold_conference', is_list(ConfProf) ->
	gen_media:cast(MPid, {contact_3rd_party, Destination, NextState, ConfProf}).

%% @doc To ensure agents are put in proper states, use this to call other
%% agents, getting them involved in the conference.
-spec(contact_agent/2 :: (MPid :: pid(), Agent :: pid() | string() | #agent{}) -> 'ok' | {'error', any()}).
contact_agent(MPid, Agent) ->
	{ok, ConfProf} = cpx:get_env(freeswitch_conference_profile, "default"),
	contact_agent(MPid, Agent, ConfProf).

%% @doc like contact_agent/2, but allows a custom profile to be used for
%% the conference in case the conference needs to be started.
-spec(contact_agent/3 :: (MPid :: pid(), Agent :: pid() | string() | #agent{}, ConfProf :: string()) -> 'ok' | {'error', any()}).
contact_agent(MPid, Agent, ConfProf) when is_list(Agent) ->
	case agent_manager:query_agent(Agent) of
		{true, Apid} ->
			contact_agent(MPid, Apid, ConfProf);
		false ->
			{error, no_agent}
	end;

contact_agent(MPid, Agent, ConfProf) when is_pid(Agent) ->
	gen_media:cast(MPid, {contact_agent, Agent, ConfProf}).

%% @doc While a conference is active, if the agent is talking to a 3rd
%% party or just sitting in limbo, this puts the agent in the conference.
-spec(retrieve_conference/1 :: (MPid :: pid()) -> 'ok').
retrieve_conference(MPid) ->
	gen_media:cast(MPid, retrieve_conference).

%% @doc While a conference is active, and a 3rd party is on hold, this
%% bridges the 3rd party and agent together.  Agent can be in the
%% conference or in limbo.
-spec(retrieve_3rd_party/1 :: (MPid :: pid()) -> 'ok').
retrieve_3rd_party(MPid) ->
	gen_media:cast(MPid, retrieve_3rd_party).

%% @doc Drops the 3rd party.  If the agent was talking to them, the agent
%% is put in limbo.
-spec(hangup_3rd_party/1 :: (MPid :: pid()) -> 'ok').
hangup_3rd_party(MPid) ->
	gen_media:cast(MPid, hangup_3rd_party).

%% @doc Merges the 3rd party into an existing confernce.  The agent has
%% the option not to join them.
-spec(merge_3rd_party/2 :: (MPid :: pid(), IncludeSelf :: boolean()) -> 'ok').
merge_3rd_party(MPid, IncludeSelf) ->
	gen_media:cast(MPid, {merge_3rd_party, IncludeSelf}).

%% @doc Same as merge_3rd_party(MPid, true).
-spec(merge_all/1 :: (MPid :: pid()) -> 'ok').
merge_all(MPid) ->
	merge_3rd_party(MPid, true).

%% @doc Same as merge_3rd_party(MPid, false).
-spec(merge_only_3rd_party/1 :: (MPid :: pid()) -> 'ok').
merge_only_3rd_party(MPid) ->
	merge_3rd_party(MPid, false).

%% @doc Removes all members from a conference (if they are in one), 
%% hanging up the channels.  This is uncerimonious and a bit brutal, but
%% sometimes needed.
-spec(end_conference/1 :: (MPid :: pid()) -> 'ok' | {'error', any()}).
end_conference(MPid) ->
	gen_media:call(MPid, end_conference).

%% @doc List the information available for the media's conference, if
%% there is one.
conference_status(Mpid) ->
	case conference_command(Mpid, "list") of
		{ok, {ConfId, ConfData}} ->
			InfoLines = string:tokens(ConfData, "\n"),
			ConfData0 = parse_conference_info(InfoLines),
			{ok, {ConfId, ConfData0}};
		Else ->
			Else
	end.

parse_conference_info(InfoLines) ->
	parse_conference_info(InfoLines, []).

parse_conference_info([], Acc) ->
	Acc;
parse_conference_info([Line | Tail], Acc) ->
	Tokens = util:string_split(Line, ";"),
	Keys = ["id","dialstring","uuid","cid_number", "cid_name", "statuses", "volume_in", "volume_out","energy","unknown"],
	Props = lists:zip(Keys, Tokens),
	parse_conference_info(Tail, [Props | Acc]).
	
%% @doc Mute a conference member
conference_mute(Mpid, MemberId) ->
	conference_command(Mpid, "mute " ++ MemberId).

%% @doc Deafen a conference member
conference_deaf(Mpid, MemberId) ->
	conference_command(Mpid, "deaf " ++ MemberId).

%% @doc Unmute a conference member
conference_unmute(Mpid, MemberId) ->
	conference_command(Mpid, "unmute " ++ MemberId).

%% @doc Undeaf a conference member
conference_undeaf(Mpid, MemberId) ->
	conference_command(Mpid, "undeaf " ++ MemberId).

%% @doc Kick a conference member
conference_kick(Mpid, MemberId) ->
	conference_command(Mpid, "kick " ++ MemberId).

%% @doc Run an arbitrary conference command (if the media is in a
%% conference).
conference_command(Mpid, Command) ->
	gen_media:call(Mpid, {conference_command, Command}).

%% @doc If an agent_transfer occurs while the caller is on hold, this
%% needs to be used to complete it.
-spec(complete_agent_transfer/1 :: (Mpid :: pid()) -> 'ok').
complete_agent_transfer(Mpid) ->
	gen_media:cast(Mpid, complete_agent_transfer).

%% @doc Cancels an inprogress agent warm transfer.
-spec(cancel_agent_transfer/1 :: (Mpid :: pid()) -> 'ok').
cancel_agent_transfer(Mpid) ->
	gen_media:cast(Mpid, cancel_agent_transfer).

%% @doc While in a conference with the passing agent, give control of this call
%% to the agent.
-spec(cede_control/2 :: (MPid :: pid(), NewController :: string()) -> 'ok').
cede_control(MPid, NewController) ->
	case agent_manager:query_agent(NewController) of
		false ->
			?INFO("~s agent doesn't exist", [NewController]),
			{error, noagent};
		{true, Apid} ->
			case agent:dump_state(Apid) of
				#agent{statedata = #call{source = MPid}} ->
					{error, self_oncall_transition};
				#agent{statedata = #call{source = OtherMedia}} ->
					gen_media:oncall_transition(MPid, OtherMedia);
				_ ->
					{error, invalid_agent_state}
			end
	end.

%% @hidden
set_state_agent(Login, Pid, RingChan, RingUUID, State) ->
	State#state{agent = Login, agent_pid = Pid, ringchannel = RingChan, ringuuid = RingUUID}.

%%====================================================================
%% gen_media callbacks
%%====================================================================
%% @private
init([Cnode, DialString, UUID]) ->
	process_flag(trap_exit, true),
	Manager = whereis(freeswitch_media_manager),
	{DNIS, Client, Priority, CidName, CidNum, SIPFrom, ExportVars} = get_info(Cnode, UUID),
	Call = #call{id = UUID, source = self(), client = Client, priority = Priority, callerid={CidName, CidNum}, dnis=DNIS, media_path = inband},
	{ok, {#state{statename = inivr, cnode=Cnode, manager_pid = Manager, dialstring = DialString, dial_vars = ["sip_h_X-FromData='"++SIPFrom++"'" | ExportVars]}, Call, {inivr, [DNIS]}}}.

-spec(urlpop_getvars/1 :: (State :: #state{}) -> [{binary(), binary()}]).
urlpop_getvars(#state{ivroption = Ivropt} = _State) ->
	[{"itxt", Ivropt}].

-spec(handle_announce/3 :: (Announcement :: string(), Callrec :: #call{}, State :: #state{}) -> {'ok', #state{}}).
handle_announce(Announcement, Callrec, State) ->
	freeswitch:sendmsg(State#state.cnode, Callrec#call.id,
		[{"call-command", "execute"},
			{"execute-app-name", "playback"},
			{"execute-app-arg", Announcement}]),
	{ok, State}.

handle_oncall_transition_accepted(freeswitch_busy_agent, BusyAgentState, Call, State) ->
	#state{agent_pid = Apid, agent = Agent, ringchannel = RingChan, ringuuid = RingUUID} = State,
	AgentNom = if
		is_record(Agent, agent) ->
			Agent#agent.login;
		true ->
			Agent
	end,
	BusyAgentState0 = freeswitch_busy_agent:set_state_agent(AgentNom, Apid, RingChan, RingUUID, BusyAgentState),
	{ok, BusyAgentState0}.

handle_answer(Apid, Callrec, #state{xferchannel = XferChannel, xferuuid = XferUUID} = State) when is_pid(XferChannel) ->
	link(XferChannel),
	?INFO("intercepting ~s from channel ~s", [XferUUID, Callrec#call.id]),
	Result = freeswitch:api(State#state.cnode, uuid_bridge, Callrec#call.id ++ " " ++ XferUUID),
	?DEBUG("result of bridge:  ~p", [Result]),
	freeswitch:api(State#state.cnode, uuid_setvar_multi, Callrec#call.id ++ " hangup_after_bridge=true;park_after_bridge=false"),
	case State#state.record_path of
		undefined ->
			ok;
		Path ->
			?DEBUG("resuming recording for ~p", [Callrec#call.id]),
			freeswitch:api(State#state.cnode, uuid_record, Callrec#call.id ++ " start " ++ Path ++ ".wav"),
			Agent = agent:dump_state(Apid),
			freeswitch:bgapi(State#state.cnode, uuid_record, XferUUID ++ " start " ++ Path ++ ".agent." ++ Agent#agent.login ++ ".wav")
	end,
	agent:conn_cast(Apid, {mediaload, Callrec, [{<<"width">>, <<"800px">>}, {<<"height">>, <<"600px">>}, {<<"title">>, <<"Server Boosts">>}]}),
	cdr:media_custom(Callrec, oncall, self, []),
	{ok, State#state{agent_pid = Apid, ringchannel = XferChannel, statename = oncall,
		ringuuid = XferUUID,
			xferchannel = undefined, xferuuid = undefined, queued = false}};

handle_answer(Apid, Callrec, #state{statename = inqueue_ringing} = State) ->
	UUID = freeswitch_ring:get_uuid(State#state.ringchannel),
	Agent = agent:dump_state(Apid),
	case freeswitch:api(State#state.cnode, uuid_bridge, Callrec#call.id ++ " " ++ UUID) of
		{ok, _} ->
			RecPath = case cpx_supervisor:get_archive_path(Callrec) of
				none ->
					?DEBUG("archiving is not configured for ~p", [Callrec#call.id]),
					undefined;
				{error, _Reason, Path} ->
					?WARNING("Unable to create requested call archiving directory for recording ~p for ~p", [Path, Callrec#call.id]),
					undefined;
				Path ->
					%% get_archive_path ensures the directory is writeable by us and exists, so this
					%% should be safe to do (the call will be hungup if creating the recording file fails)
					?DEBUG("archiving ~p to ~s.wav", [Callrec#call.id, Path]),
					freeswitch:api(State#state.cnode, uuid_setvar, Callrec#call.id ++ " RECORD_APPEND true"),
					freeswitch:api(State#state.cnode, uuid_record, Callrec#call.id ++ " start "++Path++".wav"),
					freeswitch:bgapi(State#state.cnode, uuid_setvar, UUID ++ " RECORD_APPEND true"),
					freeswitch:bgapi(State#state.cnode, uuid_record, UUID ++ " start " ++ Path ++ ".agent." ++ Agent#agent.login ++ ".wav"),
					Path
			end,
			agent:conn_cast(Apid, {mediaload, Callrec, [{<<"width">>, <<"800px">>}, {<<"height">>, <<"600px">>}, {<<"title">>, <<"Server Boosts">>}]}),
			{ok, State#state{statename = oncall, agent_pid = Apid, ringuuid = UUID, record_path = RecPath, queued = false}};
		{error, "-ERR Invalid uuid\n"} ->
			proc_lib:spawn(fun() ->
				case freeswitch:api(State#state.cnode, uuid_exists, Callrec#call.id) of
					{ok, "true"} ->
						ok;
					NotTrue ->
						?WARNING("freeswitch can't find call for ~s; exiting pid ~p", [Callrec#call.id, Callrec#call.source]),
						exit(Callrec#call.source, kill)
				end
			end),
			{error, "-ERR Invalid uuid\n", State};
		{error, Error} ->
			?WARNING("Could not do answer:  ~p", [Error]),
			{error, Error, State}
	end.

%% TODO added for testing only (implemented with focus on real Calls - no other media)
-spec(handle_end_call/2 :: (Callrec :: #call{}, State :: #state{}) -> {'ok', #state{}}).
handle_end_call(Callrec, State) ->
	freeswitch:sendmsg(State#state.cnode, Callrec#call.id,
		[{"call-command", "hangup"},
			{"hangup-cause", "SUBSCRIBER_ABSENT"}]),
	{deferred, State}.

handle_ring(Apid, Callrec, State) when is_pid(Apid) ->
	?INFO("ring to agent ~p for call ~s", [Apid, Callrec#call.id]),
	AgentRec = agent:dump_state(Apid), % TODO - we could avoid this if we had the agent's login,
	handle_ring({Apid, AgentRec}, Callrec, State);
handle_ring({_Apid, #agent{endpointtype = {undefined, persistent, _}} = Agent}, _Callrec, State) ->
	?WARNING("Agent (~p) does not have it's persistent channel up yet", [Agent#agent.login]),
	{invalid, State};
handle_ring({Apid, #agent{endpointtype = {EndpointPid, persistent, _EndPointType}}} = Agent, Callrec, State) ->
	%% a persisitant ring does the hard work for us
	%% go right to the okay.
	?INFO("Ring channel made things happy, I assume", []),
	NewStatename = case State#state.statename of
		inqueue -> inqueue_ringing;
		oncall -> oncall_ringing
	end,
	{ok, [{"itext", State#state.ivroption}], Callrec#call{ring_path = inband, media_path = inband}, State#state{statename = NewStatename, ringchannel = EndpointPid, agent_pid = Apid}};
handle_ring({Apid, #agent{endpointtype = {RPid, transient, _}} = AgentRec}, Callrec, State) ->
	NewStatename = case State#state.statename of
		inqueue -> inqueue_ringing;
		oncall -> oncall_ringing
	end,
	link(RPid),
	{ok, [{"itxt", State#state.ivroption}], State#state{statename = NewStatename, agent_pid = Apid, ringchannel = RPid}}.

handle_ring_stop(Callrec, #state{xferchannel = RingChannel} = State) when is_pid(RingChannel) ->
	?DEBUG("hanging up transfer channel for ~p", [Callrec#call.id]),
	freeswitch_ring:hangup(RingChannel),
	NextState = case State#state.statename of
		oncall_hold_ringing ->
			oncall_hold;
		oncall_ringing ->
			oncall
	end,
	cdr:media_custom(Callrec, NextState, self, []),
	{ok, State#state{statename = NextState, xferchannel = undefined, xferuuid = undefined}};
handle_ring_stop(Callrec, State) ->
	?DEBUG("hanging up ring channel for ~p", [Callrec#call.id]),
	case State#state.ringchannel of
		undefined ->
			ok;
		RingChannel ->
			% TODO - make sure the call didn't get bridged in the interim?
			% the ring channel might have bridged and the message is sitting in our mailbox
			freeswitch_ring:hangup(RingChannel)
	end,
	NewStatename = case State#state.statename of
		inqueue_ringing -> inqueue;
		oncall_ringing -> oncall
	end,
	{ok, State#state{statename = NewStatename, ringchannel=undefined}}.

-spec(handle_voicemail/3 :: (Agent :: pid() | 'undefined', Call :: #call{}, State :: #state{}) -> {'ok', #state{}}).
handle_voicemail(Agent, Callrec, State) when is_pid(Agent) ->
	{ok, Midstate} = handle_ring_stop(Callrec, State),
	handle_voicemail(undefined, Callrec, Midstate);
handle_voicemail(undefined, Call, State) ->
	UUID = Call#call.id,
	freeswitch:bgapi(State#state.cnode, uuid_transfer, UUID ++ " 'playback:IVR/prrec.wav,gentones:%(500\\,0\\,500),sleep:600,record:/tmp/${uuid}.wav' inline"),
	{ok, State#state{statename = inivr, voicemail = "/tmp/"++UUID++".wav"}}.

-spec(handle_spy/3 :: (Agent :: {pid(), #agent{}}, Call :: #call{}, State :: #state{}) -> {'error', 'bad_agent', #state{}} | {'ok', #state{}}).
handle_spy({Agent, AgentRec}, Call, #state{cnode = Fnode, ringchannel = Chan} = State) when is_pid(Chan) ->
	Dialstring = freeswitch_media_manager:get_agent_dial_string(AgentRec, []),
	{ok, JobId} = freeswitch:bgapi(Fnode, originate, Dialstring ++ " &eavesdrop(" ++ Call#call.id ++ ")"),
	#agent{connection = ConnPid} = AgentRec,
	{ok, State#state{spy_channel = {Agent, ConnPid, JobId}}};
handle_spy(_Agent, _Call, State) ->
	{invalid, State}.


handle_agent_transfer(AgentPid, Timeout, Call, State) when is_pid(AgentPid) ->
	AgentRec = agent:dump_state(AgentPid), % TODO - avoid this
	handle_agent_transfer({AgentPid, AgentRec}, Timeout, Call, State);

handle_agent_transfer({AgentPid, #agent{endpointtype = {undefined, persistent, _}} = AgentRec}, Timeout, Call, State) ->
	?WARNING("Agent ~p does not have it's persistent channel up yet", [AgentRec#agent.login]),
	{error, {bad_endpoint, AgentRec}, State};

handle_agent_transfer({AgentPid, #agent{endpointtype = {EndpointPid, persistent, _EndPointType}} = Agent}, Timeout, Call, State) ->
	?INFO("Persistent ring channel, assuming success", []),
	{ok, [{"ivropts", State#state.ivroption}, {"caseid", State#state.caseid}], State#state{statename = oncall_ringing, xferchannel = EndpointPid, xferuuid = freeswitch_ring:get_uuid(EndpointPid)}};

handle_agent_transfer({AgentPid, #agent{endpointtype = {undefined, transient, _}} = AgentRec}, Timeout, Call, State) ->
	?WARNING("Couldn't do agent transfer as the agent doesn't seem ready to ring", []),
	{error, {bad_endpoint, AgentRec}, State};

handle_agent_transfer({AgentPid, #agent{endpointtype = {RPid, transient, _}} = AgentRec}, Timeout, Call, State) ->
	?INFO("using exising transient ring channel ~p", [RPid]),
	link(RPid),
	XferUUID = freeswitch_ring:get_uuid(RPid),
	NextStatename = case State#state.statename of
		oncall -> oncall_ringing;
		oncall_hold -> oncall_hold_ringing;
		oncall_hold_ringing -> oncall_hold_ringing
	end,
	case NextStatename of
		oncall_hold_ringing ->
			cdr:media_custom(Call, oncall_hold_ringing, ?cdr_states, AgentRec#agent.login);
		_ ->
			ok
	end,
	{ok, [{"itxt", State#state.ivroption}], State#state{statename = NextStatename, xferchannel = RPid, xferuuid = XferUUID}}.

-spec(handle_warm_transfer_begin/3 :: (Number :: pos_integer(), Call :: #call{}, State :: #state{}) -> {'ok', string(), #state{}} | {'error', string(), #state{}}).
handle_warm_transfer_begin(Number, Call, #state{agent_pid = AgentPid, cnode = Node, ringchannel = undefined} = State) when is_pid(AgentPid) ->
	case freeswitch:api(Node, create_uuid) of
		{ok, NewUUID} ->
			?NOTICE("warmxfer UUID for ~p is ~p", [Call#call.id, NewUUID]),
			F = fun(RingUUID) ->
					fun(ok, _Reply) ->
							Client = Call#call.client,
							CalleridArgs = case proplists:get_value(<<"callerid">>, Client#client.options) of
								undefined ->
									["origination_privacy=hide_namehide_number"];
								CalleridNum ->
									["origination_caller_id_name='"++Client#client.label++"'", "origination_caller_id_number='"++binary_to_list(CalleridNum)++"'"]
							end,

							freeswitch:bgapi(Node, uuid_setvar, RingUUID ++ " ringback %(2000,4000,440.0,480.0)"),
							freeswitch:sendmsg(Node, RingUUID,
								[{"call-command", "execute"},
									{"execute-app-name", "bridge"},
									{"execute-app-arg",
										freeswitch_media_manager:do_dial_string(State#state.dialstring, Number, ["origination_uuid="++NewUUID | CalleridArgs])}]);
						(error, Reply) ->
							?WARNING("originate failed for ~p with ~p", [Call#call.id, Reply]),
							ok
					end
			end,

			Self = self(),

			F2 = fun(_RingUUID, EventName, _Event) ->
					case EventName of
						"CHANNEL_BRIDGE" ->
							case State#state.record_path of
								undefined ->
									ok;
								Path ->
									?DEBUG("switching to recording the 3rd party leg for ~p", [Call#call.id]),
									freeswitch:api(Node, uuid_record, Call#call.id ++ " stop " ++ Path ++ ".wav"),
									freeswitch:api(Node, uuid_record, NewUUID ++ " start " ++ Path ++ ".wav")
							end,
							Self ! warm_transfer_succeeded;
						_ ->
							ok
					end,
					true
			end,

			AgentState = agent:dump_state(AgentPid), % TODO - avoid

			case freeswitch_ring:start(Node, AgentState, AgentPid, Call, ?getRingout, F, [no_oncall_on_bridge, {eventfun, F2}, {needed_events, ['CHANNEL_BRIDGE']}, {dial_vars, State#state.dial_vars}]) of
				{ok, Pid} ->
					link(Pid),
					{ok, NewUUID, State#state{ringchannel = Pid, warm_transfer_uuid = NewUUID}};
				{error, Error} ->
					?ERROR("error when starting ring channel for ~p :  ~p", [Call#call.id, Error]),
					{error, Error, State}
			end;
		Else ->
			{error, Else, State}
	end;
handle_warm_transfer_begin(Number, Call, #state{agent_pid = AgentPid, cnode = Node} = State) when is_pid(AgentPid) ->
	case freeswitch:api(Node, create_uuid) of
		{ok, NewUUID} ->
			?NOTICE("warmxfer UUID for ~p is ~p", [Call#call.id, NewUUID]),
			freeswitch:api(Node, uuid_setvar, Call#call.id++" park_after_bridge true"),

			case State#state.record_path of
				undefined ->
					ok;
				Path ->
					?DEBUG("switching to recording the 3rd party leg for ~p", [Call#call.id]),
					freeswitch:api(Node, uuid_record, Call#call.id ++ " stop " ++ Path ++ ".wav"),
					freeswitch:api(Node, uuid_record, NewUUID ++ " start " ++ Path ++ ".wav")
			end,

			Client = Call#call.client,

			CalleridArgs = case proplists:get_value(<<"callerid">>, Client#client.options) of
				undefined ->
					["origination_privacy=hide_namehide_number"];
				CalleridNum ->
					["origination_caller_id_name=\\\\'"++Client#client.label++"\\\\'", "origination_caller_id_number=\\\\'"++binary_to_list(CalleridNum)++"\\\\'"]
			end,

			Dialplan = " 'm:^:bridge:"++ re:replace(freeswitch_media_manager:do_dial_string(State#state.dialstring, Number, ["origination_uuid="++NewUUID | CalleridArgs]), ",", ",", [{return, list}, global]) ++ "' inline",
			?NOTICE("~s", [Dialplan]),

			freeswitch:bgapi(Node, uuid_setvar, freeswitch_ring:get_uuid(State#state.ringchannel) ++ " ringback %(2000,4000,440.0,480.0)"),

			freeswitch:bgapi(State#state.cnode, uuid_transfer,
				freeswitch_ring:get_uuid(State#state.ringchannel) ++ Dialplan), 

			% play musique d'attente 
			case State#state.moh of
				none ->
					ok;
				_MohMusic ->
					freeswitch:sendmsg(Node, Call#call.id,
						[{"call-command", "execute"},
							{"execute-app-name", "playback"},
							{"execute-app-arg", "local_stream://" ++ State#state.moh}])
			end,
			{ok, NewUUID, State#state{warm_transfer_uuid = NewUUID}};
		Else ->
			?ERROR("bgapi call failed for ~p with ~p", [Call#call.id, Else]),
			{error, "create_uuid failed", State}
	end;
handle_warm_transfer_begin(_Number, Call, #state{agent_pid = AgentPid} = State) ->
	?WARNING("wtf?! agent pid is ~p for ~p", [AgentPid, Call#call.id]),
	{error, "error: no agent bridged to this call", State}.

-spec(handle_warm_transfer_cancel/2 :: (Call :: #call{}, State :: #state{}) -> 'ok' | {'error', string(), #state{}}).
handle_warm_transfer_cancel(Call, #state{warm_transfer_uuid = WUUID, cnode = Node, ringchannel = Ring} = State) when is_list(WUUID), is_pid(Ring) ->
	RUUID = freeswitch_ring:get_uuid(Ring),
	%?INFO("intercepting ~s from channel ~s", [RUUID, Call#call.id]),
	case State#state.record_path of
		undefined ->
			ok;
		Path ->
			?DEBUG("switching back to recording the original leg for ~p", [Call#call.id]),
			freeswitch:api(Node, uuid_record, WUUID ++ " stop " ++ Path ++ ".wav"),
			freeswitch:api(Node, uuid_record, Call#call.id ++ " start " ++ Path ++ ".wav")
	end,

	%Result = freeswitch:sendmsg(State#state.cnode, RUUID,
		%[{"call-command", "execute"}, {"execute-app-name", "intercept"}, {"execute-app-arg", Call#call.id}]),
	%?NOTICE("intercept result: ~p", [Result]),
	Result = freeswitch:api(State#state.cnode, uuid_bridge,  RUUID ++" " ++Call#call.id),
	?INFO("uuid_bridge result for ~p: ~p", [Call#call.id, Result]),
	{ok, State#state{warm_transfer_uuid = undefined}};
handle_warm_transfer_cancel(Call, #state{warm_transfer_uuid = WUUID, cnode = Node, agent_pid = AgentPid} = State) when is_list(WUUID) ->
	case freeswitch:api(Node, create_uuid) of
		{ok, NewUUID} ->
			?NOTICE("warmxfer UUID for ~p is ~p", [Call#call.id, NewUUID]),
			F = fun(RingUUID) ->
					fun(ok, _Reply) ->
							case State#state.record_path of
								undefined ->
									ok;
								Path ->
									?DEBUG("switching back to recording the original leg for ~p", [Call#call.id]),
									freeswitch:api(Node, uuid_record, WUUID ++ " stop " ++ Path ++ ".wav"),
									freeswitch:api(Node, uuid_record, Call#call.id ++ " start " ++ Path ++ ".wav")
							end,
							freeswitch:api(Node, uuid_bridge, RingUUID++" "++Call#call.id);
						(error, Reply) ->
							?WARNING("originate failed for ~p : ~p", [Call#call.id, Reply]),
							ok
					end
			end,

			AgentState = agent:dump_state(AgentPid), % TODO - avoid

			case freeswitch_ring:start(Node, AgentState, AgentPid, Call, ?getRingout, F, [{dial_vars, State#state.dial_vars}]) of
				{ok, Pid} ->
					link(Pid),
					{ok, State#state{ringchannel = Pid, warm_transfer_uuid = undefined}};
				{error, Error} ->
					?ERROR("error:  ~p", [Error]),
					{error, Error, State}
			end;
		Else ->
			{error, Else, State}
	end;
handle_warm_transfer_cancel(_Call, State) ->
	{error, "Not in warm transfer", State}.

-spec(handle_warm_transfer_complete/2 :: (Call :: #call{}, State :: #state{}) -> 'ok' | {'error', string(), #state{}}).
handle_warm_transfer_complete(Call, #state{warm_transfer_uuid = WUUID, cnode = Node} = State) when is_list(WUUID) ->
	%?INFO("intercepting ~s from channel ~s", [WUUID, Call#call.id]),
	case State#state.record_path of
		undefined ->
			ok;
		Path ->
			?DEBUG("stopping recording due to warm transfer complete ~p", [Call#call.id]),
			freeswitch:api(Node, uuid_record, WUUID ++ " stop " ++ Path ++ ".wav")
	end,

	%Result = freeswitch:sendmsg(State#state.cnode, WUUID,
		%[{"call-command", "execute"}, {"execute-app-name", "intercept"}, {"execute-app-arg", Call#call.id}]),
	%?INFO("intercept result: ~p", [Result]),
	Result = freeswitch:api(State#state.cnode, uuid_bridge,  WUUID ++" " ++Call#call.id),
	?INFO("uuid_bridge result: ~p", [Result]),
	{ok, State#state{warm_transfer_uuid = undefined}};
handle_warm_transfer_complete(_Call, State) ->
	{error, "Not in warm transfer", State}.

handle_wrapup(#call{media_path = inband} = Call, State) ->
	% TODO This could prolly stand to be a bit more elegant.
	%freeswitch:api(State#state.cnode, uuid_kill, Call#call.id),
	freeswitch:api(State#state.cnode, uuid_kill, State#state.ringuuid),
	Oncall = lists:member(State#state.statename, ['oncall', 'oncall_hold', 'oncall_ringing', 'oncall_hold_ringing']),
	if
		Oncall ->
			{hangup, State};
		true ->
			cdr:media_custom(Call, wrapup_conference, self, []),
			{ok, State#state{statename = wrapup_conference}}
	end;

handle_wrapup(_Call, State) ->
	% This intentionally left blank; media is out of band, so there's
	% no direct hangup by the agent
	{ok, State}.
	
handle_queue_transfer(Call, #state{cnode = Fnode} = State) ->
	case State#state.record_path of
		undefined ->
			ok;
		Path ->
			?DEBUG("stopping recording due to queue transfer for ~p", [Call#call.id]),
			freeswitch:api(Fnode, uuid_record, Call#call.id ++ " stop " ++ Path ++ ".wav")
	end,
	freeswitch:api(Fnode, uuid_park, Call#call.id),
	freeswitch:api(Fnode, uuid_setvar, Call#call.id ++ " park_after_bridge false"),
	% play musique d'attente
	% TODO this can generate an annoying warning in FS, but I don't care right now
	case State#state.moh of
		none ->
			ok;
		_MohMusak ->
			freeswitch:sendmsg(Fnode, Call#call.id,
				[{"call-command", "execute"},
					{"execute-app-name", "playback"},
					{"execute-app-arg", "local_stream://" ++ State#state.moh}])
	end,
	cdr:media_custom(Call, inqueue, self, []),
	{ok, State#state{statename = inqueue, queued = true, agent_pid = undefined}}.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
handle_call(get_call, _From, Call, State) ->
	{reply, Call, State};
handle_call(get_agent, _From, _Call, State) ->
	{reply, State#state.agent_pid, State};
handle_call({set_agent, Agent, Apid}, _From, _Call, State) ->
	{reply, ok, State#state{agent = Agent, agent_pid = Apid}};
handle_call(dump_state, _From, _Call, State) ->
	{reply, State, State};

handle_call({modify_spy, _Who}, _From, _Call, #state{spy_channel = undefined} = State) ->
	{reply, {error, not_spy}, State};

handle_call({modify_spy, Who}, {From, Tag}, Call, #state{spy_channel = {Agent, From, _Channel}} = State) ->
	handle_call({modify_spy, Who}, {Agent, Tag}, Call, State);

handle_call({modify_spy, Who}, {From, _Tag}, _Call, #state{spy_channel = {From, _AgentConnPid, SpyChannel}} = State) ->
	Key = case Who of
		agent -> "1";
		caller -> "2";
		both -> "3";
		none -> "4"
	end,
	freeswitch:bgapi(State#state.cnode, uuid_recv_dtmf, SpyChannel ++ " " ++ Key ++ "@960"),
	{reply, ok, State};

handle_call({modify_spy, _Who}, _From, _Call, State) ->
	{reply, {error, not_spy}, State};

handle_call({spy, SpyerInfo, Who}, From, Call, #state{cnode = Fnode, ringchannel = Chan} = State) when is_pid(Chan) ->
	Dialstring = freeswitch_media_manager:get_agent_dial_string(SpyerInfo, []),
	DTMF = case Who of
		agent -> "1";
		caller -> "2";
		both -> "3";
		none -> "4"
	end,
	{ok, JobId} = freeswitch:bgapi(Fnode, originate, Dialstring ++ " 'queue_dtmf:w" ++ DTMF ++ "@500,eavesdrop:" ++ Call#call.id ++ "' inline"),
	#agent{connection = ConnPid} = SpyerInfo,
	{noreply, State#state{spy_channel = {ConnPid, SpyerInfo, {From, JobId}}}};

handle_call(end_conference, _From, _Call, #state{conference_id = undefined} = State) ->
	{reply, {error, no_conference}, State};

handle_call(end_conference, _From, _Call, #state{conference_id = {ending, _JobId, _From, _ConfId}} = State) ->
	{reply, {error, ending}, State};

handle_call(end_conference, From, _Call, State) ->
	#state{conference_id = ConfId, cnode = Fnode} = State,
	{ok, JobId} = freeswitch:bgapi(Fnode, conference, ConfId ++ " hup all"),
	NewConf = {ending, JobId, From, ConfId},
	State0 = State#state{conference_id = NewConf},
	{noreply, State0};

handle_call({conference_command, _}, _From, _Call, #state{conference_id = undefined} = State) ->
	{reply, {error, no_conference}, State};

handle_call({conference_command, Command}, _From, _Call, #state{conference_id = ConfId} = State) ->
	#state{cnode = Fsnode} = State,
	?DEBUG("Conference command issued:  ~s", [Command]),
	case freeswitch:api(Fsnode, conference, ConfId ++ " " ++ Command) of
		{ok, Data} ->
			{reply, {ok, {ConfId, Data}}, State};
		Else ->
			{reply, Else, State}
	end;

handle_call({<<"conference_status">>, _Post}, From, Call, State) ->
	{reply, Reply, State0} = handle_call({conference_command, "list"}, From, Call, State),
	case Reply of
		{error, no_conference} ->
			Json = {struct, [{success, false},{<<"message">>, no_conference}, {<<"errcode">>, no_conferece}]},
			{reply, iolist_to_binary(mochijson2:encode(Json)), State0};
		{ok, {ConfId,ConfData}} ->
			InfoLines = string:tokens(ConfData, "\n"),
			ConfData0 = parse_conference_info(InfoLines),
			ConfData1 = [{struct, [{list_to_binary(K),list_to_binary(V)} || {K,V} <- InfoProplist]}
				|| InfoProplist <- ConfData0],
			Json = {struct, [{success,true},{<<"result">>,ConfData1}]},
			{reply, iolist_to_binary(mochijson2:encode(Json)),State0};
		Else ->
			Json = {struct, [{success,false},{<<"message">>, iolist_to_binary(io_lib:format("some error: ~p", [Else]))},{<<"errcode">>, <<"UNKNOWN_ERROR">>}]},
			{reply, iolist_to_binary(mochijson2:encode(Json)), State0}
	end;

handle_call({<<"conference_command">>, Post}, From, Call, State) ->
	case catch begin
		Args = proplists:get_value("args", Post, []),
		Args0 = case Args of
			_ when is_binary(Args) ->
				[binary_to_list(Args)];
			_ when is_list(Args) ->
				[binary_to_list(A) || A <- Args]
		end,
		Command = string:join(Args0, " "),
		case handle_call({conference_command, Command}, From, Call, State) of
			{reply, {ok, {ConfId,ConfData}}, State0} ->
				Struct = {struct, [{success, true}, {<<"result">>, list_to_binary(ConfData)}]},
				{ok, Struct, State0};
			{reply, Else, State0} ->
				Struct = {struct, [{success, false}, {<<"message">>, iolist_to_binary(io_lib:format("Command no workie:  ~p", [Else]))}]},
				{ok, Struct, State0}
		end end of
			{ok, Json, State1} ->
				{reply, {[], iolist_to_binary(mochijson2:encode(Json))}, State1};
			Error ->
				Json = {struct, [{success, false}, {<<"message">>, iolist_to_binary(io_lib:format("some error:  ~p", [Error]))},{<<"errcode">>, <<"UNKNOWN_ERROR">>}]},
				{reply, {[], iolist_to_binary(mochijson2:encode(Json))}, State}
		end;

handle_call(Msg, _From, Call, State) ->
	?INFO("unhandled mesage ~p for ~p", [Msg, Call#call.id]),
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private

handle_cast(call_bridged, _Call, #state{statename = oncall_hold_ringing} = State) ->
	{{mediapush, call_bridged}, State};

handle_cast(toggle_hold, Call, #state{statename = Statename} = State)
		when Statename == oncall; Statename == oncall_ringing ->
	?DEBUG("toggle hold while oncall", []),
	#state{cnode = Fnode, moh = Muzak, ringuuid = Ringid} = State,
	#call{id = Callid} = Call,
	?INFO("Gonna try to set ~s on hold", [Call#call.id]),
	%ok = fs_send_execute(Fnode, Callid, "set", "hangup_after_bridge=false"),
	%ok = fs_send_execute(Fnode, Callid, "set", "park_after_bridge=true"),
	freeswitch:api(Fnode, uuid_setvar_multi, Callid ++ " hangup_after_bridge=false;park_after_bridge=true"),
	ok = fs_send_execute(Fnode, Ringid, "set", "hangup_after_bridge=false"),
	ok = fs_send_execute(Fnode, Ringid, "set", "park_after_bridge=true"),
	Res = freeswitch:api(Fnode, uuid_transfer, Ringid ++ " park inline"),
	%?ERROR("Res of the api:  ~p", [Res]),
	case Muzak of
		none -> ok;
		_ ->
			fs_send_execute(Fnode, Callid, "playback", "local_stream://" ++ Muzak)
	end,
	Statename0 = case Statename of
		oncall -> oncall_hold;
		oncall_ringing -> oncall_hold_ringing
	end,
	case State#state.record_path of
		undefined ->
			ok;
		Path ->
			?DEBUG("Stopping recording due to going on hold for ~p", [Callid]),
			freeswitch:api(Fnode, uuid_record, Callid ++ " stop " ++ State#state.record_path ++ ".wav")
	end,
	{noreply, State#state{statename = Statename0}};

%% oncall hold -> next_state
handle_cast(toggle_hold, Call, #state{statename = oncall_hold} = State) ->
	?INFO("Gonna try to pick up the holder dude for ~s", [Call#call.id]),
	#state{cnode = Fnode, ringuuid = Ringid} = State,
	#call{id = Callid} = Call,
	freeswitch:api(Fnode, uuid_bridge, Callid ++ " " ++ Ringid),
	freeswitch:api(Fnode, uuid_setvar_multi, Callid ++ " hangup_after_bridge=true;park_after_bridge=false"),
	case State#state.record_path of
		undefined -> ok;
		Path ->
			?DEBUG("Starting recording for ~p", [Callid]),
			freeswitch:api(Fnode, uuid_record, Callid ++ " start " ++ Path ++ ".wav")
	end,
	{noreply, State#state{statename = oncall}};

handle_cast(toggle_hold, _Call, #state{statename = oncall_hold_ringing} = State) ->
	?DEBUG("Cannot back out of oncall_hold_ringing until ringing ends", []),
	{noreply, State};

handle_cast({contact_agent, _Agent, ConfProf} = Cast, Call, #state{statename = oncall_hold, cnode = Fnode} = State) ->
	% first step, get into hold_conference state, ie: make the conference
	case make_conference(Fnode, Call#call.id, ConfProf) of
		{ok, ConfId} ->
			Newstate = State#state{conference_id = ConfId, statename = hold_conference},
			handle_cast(Cast, Call, Newstate);
		Else ->
			?WARNING("Could not make the conference:  ~p", [Else]),
			{noreply, State}
	end;

handle_cast({contact_3rd_party, _Args, _NextState, ConfProfile} = Cast, Call, #state{statename = oncall_hold, cnode = Fnode} = State) ->
	% first step is to move to hold_conference state, which means 
	% creating the conference.
	case make_conference(Fnode, Call#call.id, ConfProfile) of
		{ok, ConfId} ->
			% okay, solidify the conference state change, and go on.
			Newstate = State#state{conference_id = ConfId, statename = hold_conference},
			handle_cast(Cast, Call, Newstate);
		Else ->
			?ERROR("Could not create conference:  ~p", [Else]),
			{noreply, State}
	end;

handle_cast(complete_agent_transfer, Call, #state{statename = oncall_hold_ringing} = State) ->
	#state{xferchannel = Rpid} = State,
	% TODO This will fail hilariously horribly for non-transient pids.
	gen_server:cast(Rpid, complete_agent_transfer),
	{noreply, State};

handle_cast(cancel_agent_transfer, Call, #state{statename = oncall_hold_ringing} = State) ->
	#state{xferchannel = Rpid} = State,
	gen_server:cast(Rpid, cancel_agent_transfer),
	{{stop_ring, agent_transfer_cancel}, State};

handle_cast({cede_control, NewController}, Call, State) ->
	Self = self(),
	proc_lib:spawn(?MODULE, cede_control, [Self, NewController]),
	{noreply, State};

%% hold_conference -> 3rd_party
handle_cast({contact_agent, AgentPid, _ConfProf}, Call, #state{statename = hold_conference} = State) ->
	#state{cnode = Fnode} = State,
	case freeswitch_busy_agent:start(Fnode, Call) of
		{ok, Pid} ->
			case freeswitch_busy_agent:ring_agent(Pid, AgentPid) of
				invalid ->
					?WARNING("Could not contact agent", []),
					freeswitch_busy_agent:cancel(Pid),
					{noreply, State};
				_DeferredOrOk ->
					link(Pid),
					{noreply, State#state{'3rd_party_id' = Pid, statename = hold_conference_3rdparty, next_state = '3rd_party'}}
			end;
		Error ->
			?WARNING("freeswitch_busy_agent could not start:  ~p", [Error]),
			{noreply, State}
	end;

handle_cast({freeswitch_busy_agent, answer, #call{source = BusyPid} = OtherCall, {_OPid, Ouuid}}, Call, #state{statename = hold_conference_3rdparty, '3rd_party_id' = BusyPid, ringuuid = Ruuid} = State) ->
	#state{cnode = Fnode} = State,
	% TODO too much control here?  Maybe freeswitch_busy_agent should do
	% this?
	freeswitch:api(Fnode, uuid_setvar_multi, Ouuid ++ " hangup_after_bridge=false;park_after_bridge=true"),
	freeswitch:api(Fnode, uuid_setvar_multi, Ruuid ++ " hangup_after_bridge=false;park_after_bridge=true"),
	freeswitch:bgapi(Fnode, uuid_bridge,  Ruuid ++ " " ++ Ouuid),
	{{mediapush, agent_contact}, State#state{statename = '3rd_party'}};
	
%% hold_conference -> 3rd_party | in_conference
handle_cast({contact_3rd_party, Destination, NextState, _ConfProf}, Call, #state{statename = hold_conference, cnode = Fnode} = State) ->
	% start a ring chan to 3rd party
	% play a ringing sound to agent to be nice
	% on any error, we just kill the playback
	% otherwise if the 3rd party picks up, we send a message here to move 
	% to the new state.
	#call{client = Client} = Call,
	#client{options = ClientOpts} = Client,
	{CallerNameOpt, CallerNumberOpt} = case proplists:get_value(<<"caller_id">>, ClientOpts) of
		undefined -> {"", ""};
		{BinName, BinNumber} when is_binary(BinName), is_binary(BinNumber) ->
			{binary_to_list(BinName), binary_to_list(BinNumber)};
		CidOut -> CidOut
	end,
	%Destination = binary_to_list(proplists:get_value("args", Args)),
	BaseDs = freeswitch_media_manager:get_default_dial_string(),
	RingOps = [CallerNameOpt, CallerNumberOpt, "park_after_bridge=true"],
	case originate(Fnode, BaseDs, Destination, RingOps) of
		{ok, UUID} ->
			{noreply, State#state{'3rd_party_id' = UUID, statename = hold_conference_3rdparty, next_state = NextState}};
		Else ->
			?WARNING("Failed to contact 3rd party ~s due to:  ~p", [Destination, Else]),
			{noreply, State}
	end;

handle_cast(retrieve_conference, Call, #state{
		statename = Statename} = State) when Statename =:= 'hold_conference';
		Statename =:= 'hold_conference_3rdparty' ->
	#state{cnode = Fnode, ringuuid = Ringid, conference_id = Confid} = State,
	freeswitch:bgapi(Fnode, uuid_transfer, Ringid ++ " conference:" ++ Confid ++ " inline"),
	NewState = case Statename of
		'hold_conference' -> 'in_conference';
		'hold_conference_3rdparty' -> 'in_conference_3rdparty'
	end,
	cdr:media_custom(Call, NewState, ?cdr_states, []),
	{{mediapush, NewState}, State#state{statename = NewState}};

% 3rd_party -> hold_conference_3rdparty | in_conference | hold_conference
handle_cast(toggle_hold, Call, #state{statename = '3rd_party'} = State) ->
	?INFO("Place 3rd party on hold", []),
	#state{cnode = Fnode, moh = Muzak, ringuuid = Ringid, '3rd_party_id' = ThirdPId} = State,
	ok = fs_send_execute(Fnode, Ringid, "set", "hangup_after_bridge=false"),
	ok = fs_send_execute(Fnode, Ringid, "set", "park_after_bridge=true"),
	freeswitch:api(Fnode, uuid_transfer, Ringid ++ " park inline"),
	Helddp = case Muzak of
		none -> "park";
		_ -> "park:,playback:local_stream://" ++ Muzak
	end,
	if
		is_pid(ThirdPId) ->
			% assuming it's freeswitch_busy_agent.
			freeswitch_busy_agent:transfer(ThirdPId, Helddp);
		true ->
			freeswitch:bgapi(Fnode, uuid_transfer, ThirdPId ++ " " ++ Helddp ++ " inline")
	end,
	{noreply, State#state{statename = hold_conference_3rdparty}};

handle_cast(retrieve_conference, Call, #state{statename = '3rd_party'} = State) ->
	?INFO("Place 3rd party on hold, and go to the conference", []),
	{noreply, MidState} = handle_cast(toggle_hold, Call, State),
	handle_cast(retrieve_conference, Call, MidState);

handle_cast(hangup_3rd_party, Call, #state{statename = '3rd_party'} = State) ->
	?INFO("killing 3rd party channel while talking w/ them", []),
	{noreply, MidState} = handle_cast(toggle_hold, Call, State),
	handle_cast(hangup_3rd_party, Call, MidState);

handle_cast({merge_3rd_party, _IncludeSelf}, Call, #state{'3rd_party_id' = undefined} = State) ->
	{noreply, State};

handle_cast({merge_3rd_party, IncludeAgent}, Call, State) ->
	#state{cnode = Fnode, ringuuid = Ringid, '3rd_party_id' = Thirdid, conference_id = Confid} = State,
	NextState = case {IncludeAgent, Thirdid} of
		{true, _} when is_pid(Thirdid) ->
			TransFun = fun() ->
				freeswitch:api(Fnode, uuid_transfer, Ringid ++ " 'conference:" ++ Confid ++ "' inline"),
				freeswitch_ring:block_until(State#state.ringchannel, ["CHANNEL_UNBRIDGE"]),
				freeswitch_busy_agent:transfer(Thirdid, "'conference:" ++ Confid ++ "' inline")
			end,
			proc_lib:spawn(TransFun),
			'in_conference';
		{true, _} ->
			freeswitch:api(Fnode, uuid_transfer, Thirdid ++ " 'conference:" ++ Confid ++ "' inline"),
			'in_conference';
		{_, _} when is_pid(Thirdid) ->
			TransFun = fun() ->
				freeswitch:api(Fnode, uuid_transfer, Ringid ++ " park inline"),
				freeswitch_ring:block_until(State#state.ringchannel, ["CHANNEL_UNBRIDGE"]),
				freeswitch_busy_agent:transfer(Thirdid, "'conference:" ++ Confid ++ "' inline")
			end,
			proc_lib:spawn(TransFun),
			'hold_conference';
		{_, _} ->
			freeswitch:api(Fnode, uuid_transfer, Thirdid ++ " 'conference:" ++ Confid ++ "' inline")
	end,
	{noreply, State#state{statename = NextState}};

% retrieve conference also works here.
handle_cast(retrieve_3rd_party, Call, #state{statename = hold_conference_3rdparty} = State) ->
	#state{cnode= Fnode, ringuuid = Ringid, '3rd_party_id' = Thirdpid} = State,
	?INFO("Picking up help 3rd party:  ~s", [Thirdpid]),
	freeswitch:bgapi(Fnode, uuid_bridge, Thirdpid ++ " " ++ Ringid),
	{noreply, State#state{statename = '3rd_party'}};

handle_cast(hangup_3rd_party, Call, #state{statename = 'hold_conference_3rdparty'} = State) ->
	?INFO("hangling up on 3rd party", []),
	#state{cnode = Fnode, '3rd_party_id' = Thirdid} = State,
	freeswitch:bgapi(Fnode, uuid_kill, Thirdid),
	{noreply, State#state{statename = 'hold_conference'}};

% in_conference -> '3rdparty' | 'conference_hold'
handle_cast(toggle_hold, Call, #state{statename = 'in_conference'} = State) ->
	?INFO("Place conference on hold", []),
	#state{cnode = Fnode, ringuuid = Ringid} = State,
	ok = fs_send_execute(Fnode, Ringid, "set", "hangup_after_bridge=false"),
	ok = fs_send_execute(Fnode, Ringid, "set", "park_after_bridge=true"),
	freeswitch:api(Fnode, uuid_transfer, Ringid ++ " park inline"),
	{noreply, State#state{statename = hold_conference}};

handle_cast({contact_3rd_party, T, N}, Call, State) ->
	{ok, ConfProf} = cpx:get_env(freeswitch_conference_profile, "default"),
	handle_cast({contact_3rd_party, T, N,ConfProf}, Call, State);

handle_cast({contact_3rd_party, _Targ, _NextState, _ConfProf} = Cast, Call, #state{statename = 'in_conference'} = State) ->
	?INFO("contact 3rd party, means place conference on hold first", []),
	{noreply, MidState} = handle_cast(toggle_hold, Call, State),
	handle_cast(Cast, Call, MidState);

handle_cast({contact_agent, _Apid, _ConfProf} = Cast, Call, #state{statename = in_conference} = State) ->
	?INFO("contact agent, placing conference on hold first", []),
	{noreply, MidState} = handle_cast(toggle_hold, Call, State),
	handle_cast(Cast, Call, MidState);

handle_cast(retrieve_3rd_party, Call, #state{statename = 'in_conference_3rdparty'} = State) ->
	% put conference on hold and swap to 3rd party.
	{noreply, State0} = handle_cast(toggle_hold, Call, State),
	{noreply, State1} = handle_cast(toggle_hold, Call, State0),
	handle_cast(retrieve_3rd_party, Call, State1);

handle_cast(toggle_hold, Call, #state{statename = 'in_conference_3rdparty'} = State) ->
	FakeState = State#state{statename = 'in_conference'},
	{noreply, State0} = handle_cast(toggle_hold, Call, FakeState),
	{noreply, State0#state{statename = 'hold_conference_3rdparty'}};

% any state.
handle_cast({audio_level, Target, Level}, Call, #state{statename = Statename} =
		State) when Statename == oncall; Statename == oncall_ringing ->
	%[Target, Level] = proplists:get_value("args", Arguments, [<<"read">>, 0]),
	?INFO("uuid_audio for ~s with direction ~s set to ~p", [Call#call.id, Target, Level]),
	ApiStr = Call#call.id ++ " start " ++ binary_to_list(Target) ++ " level "
		++ integer_to_list(Level),
	freeswitch:bgapi(State#state.cnode, uuid_audio, ApiStr),
	{noreply, State};

handle_cast({blind_transfer, Destination}, Call, #state{statename = oncall} = State) ->
	#state{cnode = Fnode} = State,
	#call{client = Client, id = UUID} = Call,
	#client{options= ClientOpts} = Client,
	{CallerNameOpt,CallerNumberOpt} = case proplists:get_value(<<"caller_id">>, ClientOpts) of
		undefined -> {"",""};
		{BinName,BinNumber} when is_binary(BinName),is_binary(BinNumber) ->
			{binary_to_list(BinName),binary_to_list(BinNumber)};
		CidOut -> CidOut
	end,
	BaseDS = freeswitch_media_manager:get_default_dial_string(),
	RingOpts = [CallerNameOpt, CallerNumberOpt, "hangup_after_bridge=false"],
	Dialstring = freeswitch_media_manager:do_dial_string(BaseDS, Destination, RingOpts),
	?DEBUG("Transfering ~s to ~s blindly.", [UUID, Dialstring]),
	freeswitch:api(Fnode, uuid_setvar, UUID ++ " park_after_bridge true"),
	freeswitch:bgapi(Fnode, uuid_transfer, UUID ++ " 'm:^:bridge:" ++ Dialstring ++ "' inline"),
	{wrapup, State#state{statename = 'blind_transfered'}};

handle_cast({play_dtmf, []}, Call, State) ->
	?DEBUG("Dtmf not played due to no digits", []),
	{noreply, State};

handle_cast({play_dtmf, Digits}, Call, State) when is_binary(Digits) ->
	handle_cast({play_dtmf, binary_to_list(Digits)}, Call, State);

handle_cast({play_dtmf, Digits}, Call, #state{statename = oncall} = State) ->
	#state{cnode = Fnode} = State,
	#call{id = UUID} = Call,
	freeswitch:bgapi(Fnode, uuid_send_dtmf, UUID ++ " " ++ Digits),
	{noreply, State};

%% web api's
handle_cast({<<"toggle_hold">>, _}, Call, State) ->
	handle_cast(toggle_hold, Call, State);

handle_cast({<<"complete_agent_transfer">>, _}, Call, State) ->
	handle_cast(complete_agent_transfer, Call, State);

handle_cast({<<"cancel_agent_transfer">>, _}, Call, State) ->
	handle_cast(cancel_agent_transfer, Call, State);

handle_cast({<<"retrieve_3rd_party">>, _}, Call, State) ->
	handle_cast(retrieve_3rd_party, Call, State);

handle_cast({<<"contact_agent">>, Args}, Call, State) ->
	Destination = binary_to_list(proplists:get_value("args", Args)),
	handle_cast({contact_agent, Destination, "default"}, Call, State);

handle_cast({<<"cede_control">>, Args}, Call, State) ->
	OtherAgent = binary_to_list(proplists:get_value("args", Args)),
	handle_cast({cede_control, OtherAgent}, Call, State);

handle_cast({<<"contact_3rd_party">>, Args}, Call, State) ->
	Destination = binary_to_list(proplists:get_value("args", Args)),
	handle_cast({contact_3rd_party, Destination, '3rd_party'}, Call, State);

handle_cast({<<"retrieve_conference">>, _Args}, Call, State) ->
	handle_cast(retrieve_conference, Call, State);

handle_cast({<<"merge_3rd_party">>, Args}, Call, State) ->
	IncludeAgent = case proplists:get_value("args", Args) of
		<<"true">> ->
			true;
		_ ->
			false
	end,
	handle_cast({merge_3rd_party, IncludeAgent},Call,State);

handle_cast({<<"hangup_3rd_party">>, _}, Call, State) ->
	handle_cast(hangup_3rd_party, Call, State);

handle_cast({<<"audio_level">>, Arguments}, Call, State) ->
	[Target, Level] = case proplists:get_value("args", Arguments, [<<"read">>, 0]) of
		[<<"write">>, N] -> [write, N];
		[_,N] -> [read,N];
		_ -> [read,0]
	end,
	handle_cast({audio_level, Target, Level}, Call, State);

handle_cast({<<"blind_transfer">>, Args}, Call, State) ->
	Dest = proplists:get_value("args", Args),
	handle_cast({blind_transfer, Dest}, Call, State);

handle_cast({<<"play_dtmf">>, Args}, Call, State) ->
	Digits = case proplists:get_value("args", Args, []) of
		X when is_integer(X) -> integer_to_list(X);
		X -> X
	end,
	handle_cast({play_dtmf, Digits}, Call, State);

%% tcp api's
handle_cast(Request, Call, State) when is_record(Request, mediacommandrequest) ->
	FixedRequest = cpx_freeswitch_pb:decode_extensions(Request),
	Hint = case cpx_freeswitch_pb:get_extension(FixedRequest, freeswitch_request_hint) of
		{ok, O} -> O;
		_ -> undefined
	end,
	case Hint of
		'SET_AUDIO_LEVEL' ->
			case cpx_freeswitch_pb:get_extension(audio_level_request,FixedRequest) of
				#audiolevelrequest{channel = X, value = Y} when X == undefined; Y == undefined ->
					{noreply, State};
				#audiolevelrequest{channel = Chan, value = Val} ->
					Target = case Chan of
						'SPEAKER' -> write;
						'MIC' -> read
					end,
					handle_cast({audio_level, Target, Val}, Call, State);
				_ ->
					{noreply, State}
			end;
		'TOGGLE_HOLD' ->
			handle_cast(toggle_hold, Call, State);
		'CONTACT_3RD_PARTY' ->
			case cpx_freeswitch_pb:get_extension(FixedRequest, contact_3rd_party) of
				undefined -> handle_cast(contact_3rd_party, Call, State);
				{ok, #contact3rdpartyrequest{target = Target}} ->
					handle_cast({contact_3rd_party, Target, '3rd_party'}, Call, State)
			end;
		'RETRIEVE_CONFERENCE' -> handle_cast(retrieve_conference, Call, State);
		'MERGE_3RD_PARTY' ->
			case cpx_freeswitch_pb:get_extension(merge_3rd_party, FixedRequest) of
				undefined -> handle_cast({merge_3rd_party, false},Call,State);
				{ok, #merge3rdpartyrequest{include_self = AndSelf}} ->
					handle_cast({merge_3rd_party, AndSelf},Call,State)
			end;
		'RETRIEVE_3RD_PARTY' -> handle_cast(retrieve_3rd_party, Call, State);
		'HANGUP_3RD_PARTY' -> handle_cast(hangup_3rd_party, Call, State);
		'BLIND_TRANSFER' ->
			case cpx_freeswitch_pb:get_extension(FixedRequest,blind_transfer) of
				{ok, #blindtransferrequest{target = Dest}} ->
					handle_cast({blind_transfer, Dest}, Call, State);
				BTIgnored ->
					?DEBUG("blind transfer ignored:  ~p", [BTIgnored]),
					{noreply, State}
			end;
		'PLAY_DTMF' ->
			case cpx_freeswitch_pb:get_extension(FixedRequest, dtmf_string) of
				{ok, []} ->
					?DEBUG("dtmf request ignored due to lack of digits", []),
					{noreply, State};
				{ok, Dtmf} ->
					handle_cast({send_dtmf, Dtmf}, Call, State);
				DtmfIgnored ->
					?DEBUG("dtmf request ignored:  ~p", [DtmfIgnored]),
					{noreply, State}
			end;
		FullReqIgnored ->
			?DEBUG("Request fully ignored:  ~p", [FullReqIgnored]),
			{noreply, State}
	end;

handle_cast({'3rd_party_pickup', ChanPid}, Call, #state{'3rd_party_mon' = {ChanPid, ChanMon}} = State) ->
	#state{cnode = Fnode, '3rd_party_id' = OtherParty, ringuuid = AgentChan} = State,
	freeswitch:bgapi(Fnode, uuid_bridge, OtherParty ++ " " ++ AgentChan),
	{noreply, State#state{statename = '3rd_party'}};
	
handle_cast({set_caseid, CaseID}, Call, State) ->
	?INFO("setting caseid for ~p to ~p", [Call#call.id, CaseID]),
	{noreply, State#state{caseid = CaseID}};

handle_cast(Msg, _Call, State) ->
	?DEBUG("unhandled cast while in state ~p:  ~p", [State#state.statename, Msg]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info(check_recovery, Call, State) ->
	case whereis(freeswitch_media_manager) of
		Pid when is_pid(Pid) ->
			link(Pid),
			gen_server:cast(freeswitch_media_manager, {notify, Call#call.id, self()}),
			{noreply, State#state{manager_pid = Pid}};
		_Else ->
			{ok, Tref} = timer:send_after(1000, check_recovery),
			{noreply, State#state{manager_pid = Tref}}
	end;
handle_info({'EXIT', Pid, Reason}, Call, #state{xferchannel = Pid} = State) ->
	?WARNING("Handling transfer channel ~w exit ~p for ~p", [Pid, Reason, Call#call.id]),
	NextState = case State#state.statename of
		oncall_ringing -> oncall;
		oncall_hold_ringing -> oncall_hold;
		Other -> Other
	end,
	SendInfo = binary_to_list(iolist_to_binary(io_lib:format("~s ~s", [State#state.ringuuid, NextState]))),
	if
		is_pid(State#state.agent_pid) ->
			agent:media_push(State#state.agent_pid, NextState);
		true ->
			ok
	end,
	freeswitch:api(State#state.cnode, uuid_send_info, SendInfo),
	{stop_ring, State#state{xferchannel = undefined, statename = NextState}};
handle_info({'EXIT', Pid, Reason}, Call, #state{ringchannel = Pid, warm_transfer_uuid = W} = State) when is_list(W) ->
	?WARNING("Handling ring channel ~w exit ~p while in warm transfer for ~p", [Pid, Reason, Call#call.id]),
	agent:media_push(State#state.agent_pid, warm_transfer_failed),
	cdr:warmxfer_fail(Call, State#state.agent_pid),
	{noreply, State#state{ringchannel = undefined}};
handle_info(warm_transfer_succeeded, Call, #state{warm_transfer_uuid = W} = State) when is_list(W) ->
	?DEBUG("Got warm transfer success notification from ring channel for ~p", [Call#call.id]),
	agent:media_push(State#state.agent_pid, warm_transfer_succeeded),
	{noreply, State};
handle_info({'EXIT', Pid, Reason}, Call, #state{statename = Statename, ringchannel = Pid} = State) when Statename =:= inqueue_ringing; Statename =:= oncall_ringing ->
	?WARNING("Handling ring channel ~w exit ~p for ~p", [Pid, Reason, Call#call.id]),
	NextState = case State#state.statename of
		inqueue_ringing -> inqueue;
		oncall_ringing -> oncall
	end,
	{stop_ring, State#state{statename = NextState, ringchannel = undefined}};

handle_info({'EXIT', Pid, normal}, Call, #state{ringchannel = Pid} = State) ->
	Statename = State#state.statename,
	NoopStates = [wrapup_conference, inqueue],
	Noop = lists:member(Statename, NoopStates),
	ReQueueStates = [oncall_hold, oncall_hold_ringing],
	Requeue = lists:member(Statename, ReQueueStates),
	?INFO("ring channel exit while in ~p state", [Statename]),
	if
		Noop ->
			{noreply, State};
		Statename =:= wrapup_conference ->
			{noreply, State};
		Requeue ->
			QueueNam = case State#state.queue of
				undefined ->
					?INFO("Requeue back to default_queue", []),
					"default_queue";
				QElse ->
					?INFO("Attempt to requeue back to ~p", [QElse]),
					QElse
			end,
			proc_lib:spawn(fun() ->
				gen_media:queue(Call#call.source, QueueNam)
			end),
			if
				is_pid(State#state.xferchannel) ->
					freeswitch:bgapi(State#state.cnode, uuid_kill, State#state.xferuuid);
				true ->
					ok
			end,
			{noreply, State};
		true ->
			{wrapup, State#state{ringchannel = undefined, ringuuid = undefined}}
	end;

handle_info({'EXIT', Pid, "CHANNEL_HANGUP"}, _Call, #state{ringchannel = Pid} = State) ->
	?INFO("ring channel exit while in ~p state", [State#state.statename]),
	{wrapup, State#state{ringchannel = undefined, ringuuid = undefined}};

handle_info({'EXIT', Pid, Reason}, _Call, #state{'3rd_party_id' = Pid} = State) ->
	?INFO("3rd party pid exit (fs busy agent):  ~p", [Pid]),
	State0 = State#state{'3rd_party_id' = undefined, '3rd_party_mon' = undefined},
	StateName = case State#state.statename of
		'3rd_party' ->
			'hold_conference';
		'hold_conference_3rdparty' ->
			'hold_conference';
		'in_conference_3rdparty' ->
			'in_conference';
		OtherState ->
			OtherState
	end,
	State1 = State0#state{statename = StateName},
	{{mediapush, StateName}, State1};

handle_info({'EXIT', Pid, noconnection}, _Call, State) ->
	?WARNING("Exit of ~p due to noconnection; this normally indicates a fatal freeswitch failure, so going down too.", [Pid]),
	{stop, noconnection, State};

handle_info({'EXIT', Pid, Reason}, Call, #state{manager_pid = Pid} = State) ->
	?WARNING("Handling manager exit from ~w due to ~p for ~p", [Pid, Reason, Call#call.id]),
	{ok, Tref} = timer:send_after(1000, check_recovery),
	{noreply, State#state{manager_pid = Tref}};

handle_info({'EXIT', Pid, Reason}, Call, #state{'3rd_party_id' = Pid} = State) ->
	?INFO("Contacted agent likely hung up; other call pid ~p exited due to ~p", [Pid, Reason]),
	NextState = case State#state.statename of
		hold_conference_3rdparty ->
			hold_conference;
		in_conference_3rdparty ->
			in_conference;
		agent_contact ->
			hold_conference;
		'3rd_party' ->
			hold_conference;
		OtherState ->
			OtherState
	end,
	{{mediapush, NextState}, State#state{'3rd_party_id' = undefined, statename = NextState}};

handle_info({call, {event, [UUID | Rest]}} = Event, Call, #state{uuid = undefined} = State) when is_list(UUID) ->
	?DEBUG("reporting new call ~p when uuid not set", [UUID]),
	handle_info(Event, Call, State#state{uuid = UUID});

handle_info({call, {event, [UUID | Rest]}}, Call, State) when is_list(UUID) ->
	SetSess = freeswitch:session_setevent(State#state.cnode, [
		'CHANNEL_BRIDGE', 'CHANNEL_PARK', 'CHANNEL_HANGUP',
		'CHANNEL_HANGUP_COMPLETE', 'CHANNEL_DESTROY', 'DTMF',
		'CHANNEL_ANSWER', 'CUSTOM', 'conference::maintenance']),
	?DEBUG("reporting new call ~p (eventage:  ~p).", [UUID, SetSess]),
	case State#state.uuid of
		UUID -> freeswitch_media_manager:notify(UUID, self());
		_ -> ok
	end,
	case_event_name([UUID | Rest], Call, State#state{in_control = true});
handle_info({call_event, {event, [UUID | Rest]}}, Call, State) when is_list(UUID) ->
	case_event_name([ UUID | Rest], Call, State);
handle_info({set_agent, Login, Apid}, _Call, State) ->
	{noreply, State#state{agent = Login, agent_pid = Apid}};

handle_info({bgok, JobId, Data} = Msg, _Call, #state{conference_id = {ending, JobId, From, ConfId}} = State) ->
	?INFO("Confirmation ~p of killing conference ~p", [Data, ConfId]),
	gen_server:reply(From, ok),
	{noreply, State#state{conference_id = undefined}};

handle_info({bgok, JobId, Data} = Msg, Call, #state{spy_channel = {Apid, Spyerinfo, {From, JobId}}} = State) ->
	State0 = State#state{spy_channel = {Apid, Spyerinfo, JobId}},
	case Data of
		"+OK " ++ UUIDandNL ->
			{_, State1} = handle_info(Msg, Call, State0),
			gen_server:reply(From, ok),
			{noreply, State1};
		Error ->
			{_, State1} = handle_info(Msg, Call, State0),
			gen_server:reply(From, {error, Error}),
			{noreply, State1}
	end;
	
handle_info({bgok, JobId, Data}, _Call, #state{spy_channel = {Apid, ConnPid, JobId}} = State) ->
	case Data of
		"+OK " ++ UUIDandNL ->
			agent:blab(Apid, "While spying, you have the following options:\n"++
				"* To whisper to the agent; press 1\n"++
				"* To whisper to the caller; press 2\n"++
				"* To talk to both parties; press 3\n"++
				"* To resume spying; press 0"),
			Length = length(UUIDandNL) - 2,
			UUID = lists:sublist(UUIDandNL, Length),
			SpyChannel = {Apid, ConnPid, UUID},
			{noreply, State#state{spy_channel = SpyChannel}};
		_What ->
			?NOTICE("Spy didn't seem to go so well:  ~p", [Data]),
			{noreply, State#state{spy_channel = undefined}}
	end;

handle_info({bgok, _JobId, Reply}, Call, State) ->
	?DEBUG("bgok:  ~p for ~p", [Reply, Call#call.id]),
	{noreply, State};
handle_info({bgerror, "-ERR NO_ANSWER\n"}, Call, State) ->
	?INFO("Potential ringout.  Statecook:  ~p for ~p", [State#state.cook, Call#call.id]),
	%% the apid is known by gen_media, let it handle if it is not not.
	{stop_ring, State};
handle_info({bgerror, "-ERR USER_BUSY\n"}, Call, State) ->
	?NOTICE("Agent rejected the call ~p", [Call#call.id]),
	{stop_ring, State};
handle_info({bgerror, Reply}, Call, State) ->
	?WARNING("unhandled bgerror: ~p for ~p", [Reply, Call#call.id]),
	{noreply, State};
handle_info(channel_destroy, Call, #state{in_control = InControl} = State) when not InControl ->
	?NOTICE("Hangup in IVR for ~p", [Call#call.id]),
	{stop, hangup, State};
%handle_info(channel_destroy, Call, State) ->
%	Stoppy = [oncall, oncall_hold, oncall_hold_ringing, oncall_ringing,
%		blind_transfered, inqueue, inqueue_ringing, inivr],
%	case lists:member(State#state.statename, Stoppy) of
%		true ->
%			?NOTICE("stopping due to channel_destroy while in state ~p", [State#state.statename]),
%			{stop, hangup, State};
%		_ ->
%			?INFO("Channel may be dead, but state ~p indicates more work to be done.", [State#state.statename]),
%			{noreply, State}
%	end;

% TODO This had a use at some point, but was cuasing ramdom hangups.
% need to find what was sending :(
%handle_info(call_hangup, Call, State) ->
%	?NOTICE("Call hangup info, terminating ~p", [Call#call.id]),
%	catch freeswitch_ring:hangup(State#state.ringchannel),
%	{stop, normal, State};

handle_info({'DOWN', Ref, process, Pid, Cause}, Call, #state{statename = 
		oncall, spawn_oncall_mon = {Pid, Ref}} = State) ->
	?DEBUG("Oncaller pid termination cause:  ~p", [Cause]),
	cdr:media_custom(Call, oncall, ?cdr_states, []),
	{{mediapush, caller_offhold}, State#state{spawn_oncall_mon = undefined}};

handle_info(channel_destroy, Call, State) ->
	?INFO("channel destroy (likely from manager) for ~s.", [Call#call.id]),
	Stopples = [inqueue, inivr, inqueue_ringing],
	Hangups = [oncall, oncall_ringing, oncall_hold, oncall_hold_ringing],
	Stopple = lists:member(State#state.statename, Stopples),
	Hang = lists:member(State#state.statename, Hangups),
	if
		Stopple ->
			?DEBUG("Stopping as inqueue, inqueue_ringing, or inivr", []),
			{stop, normal, State};
		Hang ->
			?DEBUG("hangup as oncall of some variant", []),
			{{hangup, caller}, State};
		true ->
			?DEBUG("noop:  ~p", [State#state.statename]),
			{noreply, State}
	end;

handle_info(Info, Call, State) ->
	?INFO("unhandled info ~p for ~p", [Info, Call#call.id]),
	?DEBUG("unhandled state:  ~p", [State]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @private
terminate(Reason, Call, State) ->
	#state{statename = Statename, cnode = Fs} = State,
	?NOTICE("terminating: ~p ~p", [Reason, Call#call.id]),
	OncallStates = [oncall, oncall_hold],
	TransferStates = [oncall_ringing, oncall_hold_ringing],
	Oncall = lists:member(Statename, OncallStates),
	Transfer = lists:member(Statename, TransferStates),
	if
		Oncall ->
			freeswitch:api(Fs, uuid_kill, Call#call.id);
		Transfer ->
			% tear down the transfer channel if there is one
			gen_server:cast(State#state.xferchannel, cancel_agent_transfer),
			freeswitch:api(Fs, uuid_kill, Call#call.id),
			freeswitch:api(Fs, uuid_kill, State#state.xferuuid);
		true ->
			ok
	end,
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
%% @private
code_change(_OldVsn, _Call, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

make_conference(Fnode, Callid, ConfProfile) ->
	{ok, ConfId} = freeswitch:api(Fnode, create_uuid),
	%{ok, ConfProfile} = cpx:get_env(freeswitch_conference_profile, "default"),
	case freeswitch:api(Fnode, uuid_transfer, Callid ++ " conference:" ++ 
		ConfId ++ "@" ++ ConfProfile ++ "++flags{mintwo} inline") of
		{ok, Res} ->
			?INFO("Success result creating conferance and transfering call to it:  ~p", [Res]),
			{ok, ConfId};
		Else ->
			?ERROR("Could not create conference:  ~p", [Else]),
			{error, Else}
	end.

%% @private
fs_send_execute(Node, Callid, Name, Arg) ->
	freeswitch:sendmsg(Node, Callid, [
		{"call-command", "execute"},
		{"execute-app-name", "set"},
		{"execute-app-arg", Arg}
	]).

originate(Node, BaseDialstring, Destination, InOpts) ->
	{ok, UUID} = freeswitch:api(Node, create_uuid),
	Opts = ["origination_uuid=" ++ UUID | InOpts],
	Dialstring = freeswitch_media_manager:do_dial_string(BaseDialstring, Destination, Opts),
	case freeswitch:bgapi(Node, originate, Dialstring ++ " &park()") of
		{ok, _BgApiId} ->
			case originate_gethandle(Node, UUID) of
				{error, Err} ->
					?WARNING("Cound not originate ~s due to ~p", [UUID, Err]),
					{error, Err};
				_ ->
					{ok, UUID}
			end;
		Bgerr ->
			?WARNING("Could not start originate:  ~p", [Bgerr]),
			{error, Bgerr}
	end.

originate_gethandle(Node, UUID) ->
	originate_gethandle(Node, UUID, 0).

originate_gethandle(Node, UUID, Count) ->
	case freeswitch:handlecall(Node, UUID) of
		{error, badsession} when Count > 10 ->
			{error, badsession};
		{error, badsession} ->
			timer:sleep(100),
			originate_gethandle(Node, UUID, Count + 1);
		{error, Other} ->
			{error, Other};
		Else ->
			Else
	end.

%% @private
case_event_name([UUID | Rawcall], Callrec, State) ->
	Ename = case proplists:get_value("Event-Name", Rawcall) of
		"CUSTOM" -> {"CUSTOM", proplists:get_value("Event-Subclass", Rawcall)};
		Else -> Else
	end,
	case_event_name(Ename, UUID, Rawcall, Callrec, State).

%% @private
case_event_name({"CUSTOM", "conference::maintenance"}, UUID, _Rawcall, Callrec, #state{statename = Statename, '3rd_party_id' = UUID} = State) when 
		Statename =:= 'in_conference'; Statename =:= 'hold_conference' ->
	cdr:media_custom(Callrec, Statename, ?cdr_states, []),
	{{mediapush, Statename}, State#state{'3rd_party_id' = undefined}};

case_event_name({"CUSTOM", "conference::maintenance"}, UUID, Rawcall, Callrec, State) ->
	#state{cnode = Fsnode, conference_id = ConfId} = State,
	Action = proplists:get_value("Action", Rawcall),
	ConfList = if
		Action =:= "add-member"; Action =:= "del-member" ->
			{ok, Data} = freeswitch:api(Fsnode, conference, ConfId ++ " list"),
			InfoLines = string:tokens(Data, "\n"),
			InfoLines0 = parse_conference_info(InfoLines),
			[{struct, [{list_to_binary(K), list_to_binary(V)} || {K,V} <- InfoProplist]}
				|| InfoProplist <- InfoLines0];
		true ->
			undefined
	end,
	MakeJson = fun() ->
		MemberId = proplists:get_value("Member-ID", Rawcall),
		{struct, [
			{<<"member_id">>, list_to_binary(MemberId)},
			{<<"conference_id">>, list_to_binary(ConfId)},
			{<<"conference_members">>, ConfList}
		]}
	end,
	case Action of
		"add-member" ->
			JsonData = MakeJson(),
			{{mediapush, {<<"conference_add_member">>, JsonData}}, State};
		"del-member" ->
			JsonData = MakeJson(),
			{{mediapush, {<<"conference_del_member">>, JsonData}}, State};
		_Other ->
			?DEBUG("Conference maintaince action ~s ignored", [Action]),
			{noreply, State}
	end;

case_event_name("CHANNEL_ANSWER", UUID, _Rawcall, Callrec, #state{
		statename = hold_conference_3rdparty, '3rd_party_id' = UUID} = State) ->
	#state{cnode = Fnode, ringuuid = Ruuid, next_state = NextState} = State,
	freeswitch:api(Fnode, uuid_setvar_multi, Ruuid ++ " hangup_after_bridge=false;park_after_bridge=true"),
	case NextState of
		in_conference ->
			?MODULE:merge_all(self()),
			?DEBUG("Time to merge all together", []),
			{noreply, State#state{next_state = undefined}};
		hold_conference ->
			?DEBUG("Only merging the 3rd party now", []),
			?MODULE:merge_only_3rd_party(self()),
			{noreply, State#state{next_state = undefined}};
		hold_conference_3rdparty ->
			?DEBUG("Doing nothing", []),
			{noreply, State#state{next_state = undefined}};
		_Whatever ->
			?DEBUG("Connecting to 3rd party", []),
			freeswitch:bgapi(Fnode, uuid_bridge, UUID ++ " " ++ Ruuid),
			cdr:media_custom(Callrec, '3rd_party', ?cdr_states, []),
			{{mediapush, '3rd_party'}, State#state{statename = '3rd_party'}}
	end;

case_event_name("CHANNEL_BRIDGE", UUID, _Rawcall, Callrec, #state{'3rd_party_id' = UUID, statename = '3rd_party'} = State) ->
	?DEBUG("Telling agent we're now oncall w/ the 3rd party", []),
	cdr:media_custom(Callrec, '3rd_party', ?cdr_states, []),
	{{mediapush, '3rd_party'}, State};

case_event_name("CHANNEL_BRIDGE", _UUID, _Rawcall, Call, #state{'3rd_party_id' = APid, statename = '3rd_party'} = State) ->
	?DEBUG("Telling agent we're now oncall...with another agent!", []),
	cdr:media_custom(Call, 'agent_contact', ?cdr_states, []),
	{{mediapush, 'agent_contact'}, State};

case_event_name(EventName, UUID, _Rawcall, _Callrec, #state{statename =  blind_transfered} = State) ->
	?DEBUG("Blind transfer state doing nothing for event event ~s of uuid ~s", [EventName, UUID]),
	{noreply, State};

case_event_name("CHANNEL_BRIDGE", UUID, _Rawcall, Callrec, #state{ringchannel = Rpid, uuid = UUID} = State) when is_pid(Rpid) ->
	% TODO fix when this can return an {oncall, State}
	RingUUID = freeswitch_ring:get_uuid(State#state.ringchannel),
	SpawnOncall = spawn_monitor(fun() ->
		Oot = gen_media:oncall(Callrec#call.source),
		?DEBUG("Result of oncall:  ~p", [Oot])
	end),
	{noreply, State#state{statename = oncall, spawn_oncall_mon = SpawnOncall, ringuuid = RingUUID}};

case_event_name("CHANNEL_PARK", UUID, Rawcall, Callrec, #state{statename = hold_conference_3rdparty, '3rd_party_id' = UUID} = State) ->
	?DEBUG("park of the 3rd party, proll a hold", []),
	cdr:media_custom(Callrec, State#state.statename, ?cdr_states, []),
	{{mediapush, State#state.statename}, State};

case_event_name("CHANNEL_PARK", UUID, Rawcall, Callrec, #state{
		statename = HoldState, uuid = UUID} = State) when HoldState =:= oncall_hold; HoldState =:= oncall_hold_ringing ->
	Moh = case proplists:get_value("variable_queue_moh", Rawcall, "moh") of
		"silence" ->
			none;
		MohMusak ->
			MohMusak
	end,
	case Moh of
		none ->
			freeswitch:sendmsg(State#state.cnode, UUID,
				[{"call-command", "execute"},
					{"execute-app-name", "playback"},
					{"execute-app-arg", "silence"}]);
		_MohMusak ->
			freeswitch:sendmsg(State#state.cnode, UUID,
				[{"call-command", "execute"},
					{"execute-app-name", "playback"},
					{"execute-app-arg", "local_stream://"++Moh}])
	end,
	cdr:media_custom(Callrec, HoldState, ?cdr_states, []),
	{{mediapush, caller_hold}, State};

case_event_name("CHANNEL_DESTROY", UUID, Rawcall, Callrec, #state{
		'3rd_party_id' = UUID, statename = Statename} = State) ->
	{{mediapush, Statename}, State#state{'3rd_party_id' = undefined}};

case_event_name("CHANNEL_PARK", UUID, Rawcall, Callrec, #state{
		uuid = UUID, queued = false, warm_transfer_uuid = undefined,
		statename = Statename} = State) when
		Statename == inqueue; Statename =/= inqueue_ringing ->

	?DEBUG("Channel park rawcall: ~p", [Rawcall]),

	Queue = proplists:get_value("variable_queue", Rawcall, "default_queue"),
	Client = proplists:get_value("variable_brand", Rawcall),
	AllowVM = proplists:get_value("variable_allow_voicemail", Rawcall, false),
	Moh = case proplists:get_value("variable_queue_moh", Rawcall, "moh") of
		"silence" ->
			none;
		MohMusak ->
			MohMusak
	end,
	Priority = get_rawcall_int("variable_queue_priority", 
		Rawcall, ?DEFAULT_PRIORITY),

	VMPriorityDiff = 
		case get_rawcall_int("variable_vm_priority_diff",
				Rawcall, undefined) of
			undefined ->
				ClientRec = Callrec#call.client,
				proplists:get_value(vm_priority_diff, ClientRec#client.options,
				 	?DEFAULT_VM_PRIORITY_DIFF);
			Val ->
				Val
		end,

	Ivropt = proplists:get_value("variable_ivropt", Rawcall),
	SkillList = proplists:get_value("variable_skills", Rawcall, ""),
	Skills = lists:foldl(fun(X, Acc) ->
		try list_to_existing_atom(X) of
			Atom ->
				[Atom | Acc]
		catch
			error:badarg ->
				?WARNING("Freeswitch requested unknown skill ~s~n", [X]),
				Acc
		end
	end, [], util:string_split(SkillList, ",")),
	
	{Calleridname, Calleridnum} = get_caller_id(Rawcall),
	Doanswer = proplists:get_value("variable_erlang_answer", Rawcall, true),
	NewCall = Callrec#call{client=Client, callerid={Calleridname, Calleridnum}, priority = Priority, skills = Skills},
	case Doanswer of
		"false" ->
			ok;
		_ ->
			freeswitch:sendmsg(State#state.cnode, UUID,
				[{"call-command", "execute"},
					{"execute-app-name", "answer"}])
	end,
	% reset park after bridge in case this was ever on hold.
	freeswitch:bgapi(State#state.cnode, uuid_setvar, UUID ++ " hangup_after_bridge true"),
	freeswitch:bgapi(State#state.cnode, uuid_setvar, UUID ++ " park_after_bridge false"),
	% play musique d'attente
	case Moh of
		none ->
			freeswitch:sendmsg(State#state.cnode, UUID,
				[{"call-command", "execute"},
					{"execute-app-name", "playback"},
					{"execute-app-arg", "silence"}]);
		_MohMusak ->
			freeswitch:sendmsg(State#state.cnode, UUID,
				[{"call-command", "execute"},
					{"execute-app-name", "playback"},
					{"execute-app-arg", "local_stream://"++Moh}])
	end,
	%% tell gen_media to (finally) queue the media
	cdr:media_custom(NewCall, inqueue, self, Queue),
	{queue, Queue, NewCall, State#state{queue = Queue, queued=true, allow_voicemail=AllowVM, vm_priority_diff = VMPriorityDiff, moh=Moh, ivroption = Ivropt, statename = inqueue}};

case_event_name("CHANNEL_HANGUP", UUID, _Rawcall, Callrec, #state{uuid = UUID} = State)  when is_list(State#state.warm_transfer_uuid) and is_pid(State#state.ringchannel) ->
	?NOTICE("caller hung up while agent was talking to third party ~p", [Callrec#call.id]),
	RUUID = freeswitch_ring:get_uuid(State#state.ringchannel),
	% notify the agent that the caller hung up via some beeping
	freeswitch:bgapi(State#state.cnode, uuid_displace,
		RUUID ++ " start tone_stream://v=-7;%(100,0,941.0,1477.0);v=-7;>=2;+=.1;%(1400,0,350,440) mux"),
	agent:blab(State#state.agent_pid, "Caller hung up, sorry."),
	cdr:warmxfer_fail(Callrec, State#state.agent_pid),
	{{hangup, "caller"}, State};

case_event_name("CHANNEL_HANGUP_COMPLETE", UUID, Rawcall, Callrec, #state{uuid = UUID} = State) ->
	?DEBUG("Channel hangup ~p", [Callrec#call.id]),
	Apid = State#state.agent_pid,
	case Apid of
		undefined ->
			?WARNING("Agent undefined ~p", [Callrec#call.id]),
			State2 = State#state{agent = undefined, agent_pid = undefined};
		_Other ->
			try agent:query_state(Apid) of
				{ok, ringing} ->
					?NOTICE("caller hung up while we were ringing an agent ~p", [Callrec#call.id]),
					case State#state.ringchannel of
						undefined ->
							ok;
						RingChannel ->
							freeswitch_ring:hangup(RingChannel)
					end;
				_Whatever ->
					ok
			catch
				exit:{noproc, _} ->
					?WARNING("agent ~p is a dead pid ~p", [Apid, Callrec#call.id])
			end,
			State2 = State#state{agent = undefined, agent_pid = undefined, ringchannel = undefined}
	end,
	case State#state.voicemail of
		false -> % no voicemail
			ok;
		FileName ->
			case filelib:is_regular(FileName) of
				true ->
					?NOTICE("~s left a voicemail", [UUID]),
					Client = Callrec#call.client,

					VMPriority = Callrec#call.priority +
						State#state.vm_priority_diff,

					freeswitch_media_manager:new_voicemail(UUID, FileName, State#state.queue, VMPriority, Client#client.id);
				false ->
					?NOTICE("~s hungup without leaving a voicemail", [UUID])
			end
	end,
	% TODO - this is protocol specific and we only handle SIP right now
	% TODO - this should go in the CDR
	Cause = proplists:get_value("variable_hangup_cause", Rawcall),
	Who = case proplists:get_value("variable_sip_hangup_disposition", Rawcall) of
		"recv_bye" ->
			?DEBUG("Caller hungup ~p, cause ~p", [UUID, Cause]),
			"caller";
		"send_bye" ->
			?DEBUG("Agent hungup ~p, cause ~p", [UUID, Cause]),
			"agent";
		_ ->
			?DEBUG("I don't know who hung up ~p, cause ~p", [UUID, Cause]),
			undefined
		end,
	%{noreply, State};
	{{hangup, Who}, State2};

case_event_name("CHANNEL_DESTROY", UUID, _Rawcall, Callrec, #state{uuid = UUID} = State) ->
	?DEBUG("Last message this will recieve, channel destroy ~p", [Callrec#call.id]),
	{stop, normal, State};

case_event_name("DTMF", UUID, Rawcall, Callrec, #state{allow_voicemail = VmAllowed, queued = true, uuid = UUID} = State) when VmAllowed =/= false ->
	case proplists:get_value("DTMF-Digit", Rawcall) of
		"*" ->
			% allow the media to go to voicemail
			?NOTICE("caller requested to go to voicemail ~p", [Callrec#call.id]),
			freeswitch:bgapi(State#state.cnode, uuid_transfer, UUID ++ " 'playback:IVR/prrec.wav,gentones:%(500\\,0\\,500),sleep:600,record:/tmp/${uuid}.wav' inline"),
			case State#state.ringchannel of
				undefined ->
					ok;
				RingChannel ->
					freeswitch_ring:hangup(RingChannel)
			end,
			{voicemail, State#state{voicemail = "/tmp/"++UUID++".wav"}};
		_ ->
			{noreply, State}
	end;

case_event_name({error, notfound}, UUID, Rawcall, Callrec, State) ->
	?WARNING("event name not found: ~p for ~p", [proplists:get_value("Content-Type", Rawcall), UUID]),
	{noreply, State};

case_event_name(Ename, UUID, _, _, #state{statename = Statename} = State) ->
	?DEBUG("Event ~p for ~s unhandled while in state ~p", [Ename, UUID, Statename]),
	{noreply, State}.

get_exported_variables(Proplist) ->
	ExportVars = string:tokens(proplists:get_value("variable_export_vars", Proplist, ""), ","),
	VarNames = ["variable_" ++ V || V <- ExportVars],
	VarValues = [proplists:get_value(N, Proplist, "") || N <- VarNames],
	lists:zipwith(fun (K,V) -> K ++ "=" ++ V end, ExportVars, VarValues).

get_caller_id(Proplist) ->
 InitCallerIdName = proplists:get_value("Caller-Caller-ID-Name", Proplist),
 InitCallerIdNumber = proplists:get_value("Caller-Caller-ID-Number", Proplist),
 ?DEBUG("Originat cid/cname:  ~p, ~p", [InitCallerIdName, InitCallerIdNumber]),
 {CallerIdName, CallerIdNumber} = case {InitCallerIdName, InitCallerIdNumber} of
	 {NixCidName, _} when NixCidName =:= "unknown"; NixCidName =:= undefined ->
		 {proplists:get_value("variable_sip_from_user_stripped", Proplist, "Unknown"),
		 proplists:get_value("variable_sip_from_uri", Proplist, "Unknown")};
	 _ ->
		 {InitCallerIdName, InitCallerIdNumber}
 end,
 ?DEBUG("And what shall be used:  ~p  ~p", [CallerIdName,CallerIdNumber]),
 {CallerIdName, CallerIdNumber}.

get_info(Cnode, UUID) ->
	get_info(Cnode, UUID, 0).

get_info(Cnode, UUID, Retries) when Retries < 2 ->
	case freeswitch:api(Cnode, uuid_dump, UUID) of
		{ok, Result} ->
			Proplist = lists:foldl(
				fun([], Acc) ->
						Acc;
					(String, Acc) ->
						[Key, Value] = util:string_split(String, ": ", 2),
						[{Key, Value} | Acc]
				end, [], util:string_split(Result, "\n")),

			Priority = try list_to_integer(proplists:get_value("variable_queue_priority", Proplist, "")) of
				Pri -> Pri
			catch
				error:badarg ->
					?DEFAULT_PRIORITY
			end,
			{CallerIdName, CallerIdNumber} = get_caller_id(Proplist),
			{proplists:get_value("Caller-Destination-Number", Proplist, ""),
				proplists:get_value("variable_brand", Proplist, ""), Priority,
				CallerIdName, CallerIdNumber,
				proplists:get_value("variable_sip_from_display", Proplist, ""),
				get_exported_variables(Proplist)
			};
		timeout ->
			?WARNING("uuid_dump for ~s timed out. Retrying", [UUID]),
			%{"", "", 10, "Unknown", "Unknown"};
			get_info(Cnode, UUID, Retries + 1);
		{error, Error} ->
			?WARNING("uuid_dump for ~s errored:  ~p. Retrying", [UUID, Error]),
			%{"", "", 10, "Unknown", "Unknown"}
			get_info(Cnode, UUID, Retries + 1)
	end;
get_info(_, UUID, _) ->
	?WARNING("Too many failures doing uuid_dump for ~p", [UUID]),
	{"", "", ?DEFAULT_PRIORITY, "Unknown", "Unknown", "", []}.

get_rawcall_int(Key, Rawcall, Default) ->
	case proplists:get_value(Key, Rawcall) of
		undefined ->
			Default;
		ValStr ->
			try list_to_integer(ValStr) of
				ValInt -> ValInt
			catch
				error:badarg ->
					?WARNING("Invalid value for ~s: ~p", [Key, ValStr]),
					Default
			end
	end.
