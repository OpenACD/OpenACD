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
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("OpenACD/include/log.hrl").
-include_lib("OpenACD/include/queue.hrl").
-include_lib("OpenACD/include/call.hrl").
-include_lib("OpenACD/include/agent.hrl").
-include("cpx_freeswitch_pb.hrl").
%-include_lib("OpenACD/include/gen_media.hrl").

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
	'3rd_party_pickup'/1
	]).

%% gen_media callbacks
-export([
	init/1,
	urlpop_getvars/1,
	prepare_endpoint/2,
	handle_ring/4,
	handle_ring_stop/4,
	handle_answer/5,
	handle_voicemail/3,
	handle_spy/3,
	handle_announce/3,
%% TODO added for testing only (implemented with focus on real Calls - no other media)
	handle_end_call/4,
	handle_agent_transfer/4,
	handle_queue_transfer/5,
	handle_wrapup/5,
	handle_call/6,
	handle_cast/5, 
	handle_info/5,
	terminate/5,
	code_change/4]).

-type(internal_statename() ::
	'inivr' | % never been in queue.
	'inqueue' | % waiting for an agent to call
	'inqueue_ringing' | % got an agent, waiting for the anser
	'oncall' | % oncall with an agent.
	'oncall_ringing' | % starting an agent transfer
	'oncall_hold' | % simple hold
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

-define(cdr_states, ['oncall', 'oncall_hold', 'hold_conference',
	'hold_conference_3rdparty', 'in_conference_3rdparty', '3rd_party',
	'in_conference', 'wrapup_conference', 'blind_transfered']).

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
	ivroption :: string() | 'undefined',
	caseid :: string() | 'undefined',
	moh = "moh" :: string() | 'none',
	record_path :: 'undefined' | string(),
	dial_vars :: list(),
	hold :: 'undefined' | 'hold',
	spawn_oncall_mon :: 'undefined' | {pid(), reference()},
	conference_id :: 'undefined' | string(),
	'3rd_party_id' :: 'undefined' | string(),
	'3rd_party_mon' :: 'undefined' | {pid(), reference()}
}).

-type(state() :: #state{}).
-define(GEN_MEDIA, true).
-include_lib("OpenACD/include/gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================

%% @doc starts the freeswitch media gen_media.  `Cnode' is the C node the
%% communicates directly with freeswitch.  Dialstring is the data used to
%% connect to an outside line.  UUID is the id of the channel on
%% freeswith, and therefore the id of the call in OpenACD.
-spec(start/3 :: (Cnode :: atom(), DialString :: string(), UUID :: string()) -> {'ok', pid()}).
start(Cnode, DialString, UUID) ->
	gen_media:start(?MODULE, [Cnode, DialString, UUID]).

%% @doc Starts the freeswitch gen_media linked to the calling process.
%% @see start/3
-spec(start_link/3 :: (Cnode :: atom(), DialString :: string(), UUID :: string()) -> {'ok', pid()}).
start_link(Cnode, DialString, UUID) ->
	gen_media:start_link(?MODULE, [Cnode, DialString, UUID]).

%% @doc returns the record of the call freeswitch media `MPid' is in
%% charge of.
-spec(get_call/1 :: (MPid :: pid()) -> #call{}).
get_call(MPid) ->
	gen_media:get_call(MPid).

%% @doc A debugging function that returns the state record.
-spec(dump_state/1 :: (Mpid :: pid()) -> #state{}).
dump_state(Mpid) when is_pid(Mpid) ->
	gen_media:call(Mpid, dump_state).

%% @hidden
'3rd_party_pickup'(Mpid) ->
	Self = self(),
	gen_media:cast(Mpid, {'3rd_party_pickup', Self}).
	
%%====================================================================
%% gen_media callbacks
%%====================================================================

%% @private
init([Cnode, DialString, UUID]) ->
	process_flag(trap_exit, true),
	Manager = whereis(freeswitch_media_manager),
	{DNIS, Client, Priority, CidName, CidNum, SIPFrom, ExportVars} = get_info(Cnode, UUID),
	Call = #call{id = UUID, source = self(), client = Client, priority = Priority, callerid={CidName, CidNum}, dnis=DNIS, media_path = inband},
	{ok, {#state{statename = inivr, cnode=Cnode, manager_pid = Manager, dialstring = DialString, dial_vars = ["sip_h_X-FromData='"++SIPFrom++"'" | ExportVars], uuid = UUID}, Call, {inivr, [DNIS]}}}.

%% @private
-spec(urlpop_getvars/1 :: (State :: #state{}) -> [{binary(), binary()}]).
urlpop_getvars(#state{ivroption = Ivropt} = _State) ->
	[{"itxt", Ivropt}].

%% @private
-spec(handle_announce/3 :: (Announcement :: string(), Callrec :: #call{}, State :: #state{}) -> {'ok', #state{}}).
handle_announce(Announcement, Callrec, State) ->
	freeswitch:sendmsg(State#state.cnode, Callrec#call.id,
		[{"call-command", "execute"},
			{"execute-app-name", "playback"},
			{"execute-app-arg", Announcement}]),
	{ok, State}.

%%--------------------------------------------------------------------
%% perpare_endpoint
%%--------------------------------------------------------------------

-type(agent_registration_type() :: 'sip' | 'sip_registration' | 'h323' |
	'iax2' | 'pstn').
-type(agent_registration_data_opt() :: {'date', string()}).
-type(agent_registration_type_opt() :: {'type', agent_registration_type()}).
-type(persistant_ring_opt() :: 'persistant').
-type(endpoint_opt() :: persistant_ring_opt() |
	agent_registration_type_opt() | agent_registration_data_opt()).
-type(endpoint_opts() :: [endpoint_opt()]).
-type(freeswitch_ring_endpoint() :: pid() | {'freeswitch_ring','start',[_]}).
%% @doc Handle an agent being setup with a freeswitch_media endpoint.
%% Options uses defaults of [{type,sip_registration},{data,agent_login()}].
-spec(prepare_endpoint/2 :: (Agent :: #agent{}, Options :: endpoint_opts())
	-> {'ok', freeswitch_ring_endpoint()}).
prepare_endpoint(Agent, Options) ->
	{Node, Dialstring, Dest} = freeswitch_media_manager:get_ring_data(Agent, Options),
	case proplists:get_value(persistant, Options) of
		undefined ->
			{ok, {freeswitch_ring, start, [Node, freeswitch_ring_transient, [
				{destination, Dest},
				{dialstring, Dialstring}]]}};
		true ->
			freeswitch_ring:start(Node, freeswitch_persistant_ring, [
				{destination, Dest},
				{dialstring, Dialstring},
				persistant
			])
	end.

%%--------------------------------------------------------------------
%% handle_answer
%%--------------------------------------------------------------------

%% @hidden
handle_answer(Apid, StateName, Callrec, GenMediaState, State) when
		StateName =:= inqueue_ringing; StateName =:= oncall_ringing ->
	{RingUUID, RingPid} = case GenMediaState of
		#inqueue_ringing_state{outband_ring_pid = undefined} ->
			{"", undefined};
		#inqueue_ringing_state{outband_ring_pid = P} ->
			{freeswitch_ring:get_uuid(P), P};
		#oncall_ringing_state{outband_ring_pid = undefined} ->
			{"", undefined};
		#oncall_ringing_state{outband_ring_pid = P} ->
			{freeswitch_ring:get_uuid(P), P}
	end,
	case freeswitch:api(State#state.cnode, uuid_bridge, Callrec#call.id ++ " " ++ RingUUID) of
		{ok, _} ->
			RecPath = case cpx_supervisor:get_archive_path(Callrec) of
				none ->
					?DEBUG("archiving is not configured for ~p", [Callrec#call.id]),
					undefined;
				{error, _Reason, Path} ->
					?WARNING("Unable to create requested call archiving directory for recording ~p for ~p", [Path, Callrec#call.id]),
					undefined;
				Path ->
					% get_archive_path ensures the directory is writeable by us and
					% exists, so this should be safe to do (the call will be hungup
					% if creating the recording file fails)
					?DEBUG("archiving ~p to ~s.wav", [Callrec#call.id, Path]),
					freeswitch:api(State#state.cnode, uuid_setvar, Callrec#call.id ++ " RECORD_APPEND true"),
					freeswitch:api(State#state.cnode, uuid_record, Callrec#call.id ++ " start "++Path++".wav"),
					Path++".wav"
			end,
			agent_channel:media_push(Apid, Callrec, {mediaload, Callrec, [{<<"height">>, <<"300px">>}, {<<"title">>, <<"Server Boosts">>}]}),
			{ok, State#state{agent_pid = Apid, record_path = RecPath, queued = false, statename = oncall, ringchannel = RingPid, ringuuid = RingUUID}};
		{error, Error} ->
			?WARNING("Could not do answer:  ~p", [Error]),
			{error, Error, State}
	end.


% TODO added for testing only (implemented with focus on real Calls - no other media)
%% @hidden
-spec(handle_end_call/4 :: (GMState :: atom(), Callrec :: #call{},
GMStateData :: any(), State :: #state{}) -> {'ok', #state{}}).
handle_end_call(_Gmstate, Callrec, _Gmstatedata, State) ->
	freeswitch:sendmsg(State#state.cnode, Callrec#call.id,
		[{"call-command", "hangup"},
			{"hangup-cause", "SUBSCRIBER_ABSENT"}]),
	{deferred, State}.

%% @hidden
handle_ring(Apid, RingData, Callrec, State) when is_pid(Apid) ->
	?INFO("ring to agent ~p for call ~s", [Apid, Callrec#call.id]),
	% TODO - we could avoid this if we had the agent's login,
	AgentRec = agent:dump_state(Apid),
	handle_ring({Apid, AgentRec}, RingData, Callrec, State);
handle_ring({_Apid, #agent{ring_channel = {undefined, persistant, _}} = Agent}, _RingData, _Callrec, State) ->
	?WARNING("Agent (~p) does not have it's persistant channel up yet", [Agent#agent.login]),
	{invalid, State};
handle_ring({Apid, #agent{ring_channel = {EndpointPid, persistant, _EndPointType}}} = Agent, _RingData, Callrec, State) ->
	%% a persisitant ring does the hard work for us
	%% go right to the okay.
	?INFO("Ring channel made things happy, I assume", []),
	{ok, [{"itxt", State#state.ivroption}], Callrec#call{ring_path = inband, media_path = inband}, State#state{ringchannel = EndpointPid, agent_pid = Apid}};
handle_ring({Apid, #agent{ring_channel = {RPid, transient, _}} = AgentRec}, _RingData, Callrec, State) ->
	NewStatename = case State#state.statename of
		inqueue -> inqueue_ringing;
		oncall -> oncall_ringing
	end,
	{ok, [{"itxt", State#state.ivroption}], State#state{statename = NewStatename, agent_pid = Apid, ringchannel = RPid}}.

% TODO This needs to be updated when conferencing is fixed.
%% @hidden
handle_ring_stop(_StateName, Callrec, _GenMedia, #state{xferchannel = RingChannel} = State) when is_pid(RingChannel) ->
	?DEBUG("hanging up transfer channel for ~p", [Callrec#call.id]),
	freeswitch_ring:hangup(RingChannel),
	{ok, State#state{xferchannel = undefined, xferuuid = undefined}};

handle_ring_stop(StateName, Callrec, _GenMedia, State) ->
	?DEBUG("hanging up ring channel for ~p", [Callrec#call.id]),
	case State#state.ringchannel of
		undefined ->
			ok;
		RingChannel ->
			% TODO - make sure the call didn't get bridged in the interim?
			% the ring channel might have bridged and the message is sitting
			% in our mailbox
			freeswitch_ring:hangup(RingChannel)
	end,
	NewStatename = case StateName of
		inqueue_ringing -> inqueue;
		oncall_ringing -> oncall
	end,
	{ok, State#state{statename = NewStatename, ringchannel=undefined}}.

%% @hidden
-spec(handle_voicemail/3 :: (Agent :: pid() | 'undefined', Call :: #call{}, State :: #state{}) -> {'ok', #state{}}).
handle_voicemail(Agent, Callrec, State) when is_pid(Agent) ->
	{ok, Midstate} = handle_ring_stop(undefined, Callrec, undefined, State),
	handle_voicemail(undefined, Callrec, Midstate);
handle_voicemail(undefined, Call, State) ->
	UUID = Call#call.id,
	freeswitch:bgapi(State#state.cnode, uuid_transfer, UUID ++ " 'playback:IVR/prrec.wav,gentones:%(500\\,0\\,500),sleep:600,record:/tmp/${uuid}.wav' inline"),
	{ok, State#state{statename = inivr, voicemail = "/tmp/"++UUID++".wav"}}.

%% @hidden
-spec(handle_spy/3 :: (Agent :: {pid(), #agent{}}, Call :: #call{}, State :: #state{}) -> {'error', 'bad_agent', #state{}} | {'ok', #state{}}).
handle_spy({Agent, AgentRec}, Call, #state{cnode = Fnode, ringchannel = Chan} = State) when is_pid(Chan) ->
	agent:blab(Agent, "While spying, you have the following options:\n"++
		"* To whisper to the agent; press 1\n"++
		"* To whisper to the caller; press 2\n"++
		"* To talk to both parties; press 3\n"++
		"* To resume spying; press 0"),
	Dialstring = freeswitch_media_manager:get_agent_dial_string(AgentRec, []),
	freeswitch:bgapi(Fnode, originate, Dialstring ++ " &eavesdrop(" ++ Call#call.id ++ ")"),
	{ok, State};
handle_spy(_Agent, _Call, State) ->
	{invalid, State}.

%% @hidden
handle_agent_transfer(AgentPid, Timeout, Call, State) ->
	AgentRec = agent:dump_state(AgentPid), % TODO - avoid this
	?INFO("transfer_agent to ~p for call ~p", [AgentRec#agent.login, Call#call.id]),
	% fun that returns another fun when passed the UUID of the new channel
	% (what fun!)
	F = fun(_UUID) ->
		fun(ok, _Reply) ->
			% agent picked up?
				?INFO("Agent transfer picked up? ~p", [Call#call.id]);
		(error, Reply) ->
			?WARNING("originate failed for ~p with  ~p", [Call#call.id, Reply])
		end
	end,
	case freeswitch_ring:start_link(State#state.cnode, AgentRec, AgentPid, Call, Timeout, F, [single_leg, no_oncall_on_bridge, {dial_vars, State#state.dial_vars}]) of
		{ok, Pid} ->
			{ok, [{"ivropt", State#state.ivroption}, {"caseid", State#state.caseid}], State#state{statename = oncall_ringing, xferchannel = Pid, xferuuid = freeswitch_ring:get_uuid(Pid)}};
		{error, Error} ->
			?ERROR("error:  ~p", [Error]),
			{error, Error, State}
	end.

%% @hidden
handle_wrapup(_From, _StateName, #call{media_path = inband} = Call, _GenMediaState, State) ->
	?DEBUG("Handling wrapup request", []),
	% TODO This could prolly stand to be a bit more elegant.
	freeswitch:api(State#state.cnode, uuid_kill, Call#call.id),
	{hangup, State};
handle_wrapup(_From, _StateName, _Call, _GenMediaState, State) ->
	% This intentionally left blank; media is out of band, so there's
	% no direct hangup by the agent
	?DEBUG("Not doing wrapup request", []),
	{ok, State}.

%% @hidden
handle_queue_transfer(_Queue, _StateName, Call, _GenMediaState, #state{cnode = Fnode} = State) ->
	case State#state.record_path of
		undefined ->
			ok;
		Path ->
			?DEBUG("stopping recording due to queue transfer for ~p", [Call#call.id]),
			freeswitch:api(Fnode, uuid_record, Call#call.id ++ " stop " ++ Path)
	end,
	freeswitch:api(Fnode, uuid_park, Call#call.id),
	% play musique d'attente
	% TODO this can generate an annoying warning in FS, but I don't care
	% right now
	case State#state.moh of
		none ->
			ok;
		_MohMusak ->
			freeswitch:sendmsg(Fnode, Call#call.id,
				[{"call-command", "execute"},
					{"execute-app-name", "playback"},
					{"execute-app-arg", "local_stream://" ++ State#state.moh}])
	end,
	{ok, State#state{statename = inqueue, queued = true, agent_pid = undefined}}.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------

%% @private
handle_call(get_call, _From, _, Call, _, State) ->
	{reply, Call, State};
handle_call(get_agent, _From, _, _Call, _, State) ->
	{reply, State#state.agent_pid, State};
handle_call({set_agent, Agent, Apid}, _From, _, _Call, _, State) ->
	{reply, ok, State#state{agent = Agent, agent_pid = Apid}};
handle_call(dump_state, _From, _, _Call, _, State) ->
	{reply, State, State};

handle_call(Msg, _From, _, Call, _, State) ->
	?INFO("unhandled mesage ~p for ~p", [Msg, Call#call.id]),
	{reply, ok, State}.




%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private

handle_cast(toggle_hold, _Statename, Call, _GenMediaState, #state{statename = Statename} = State)
		when Statename == oncall; Statename == oncall_ringing ->
	?DEBUG("toggle hold while oncall", []),
	#state{cnode = Fnode, moh = Muzak, ringuuid = Ringid} = State,
	#call{id = Callid} = Call,
	?INFO("Gonna try to set ~s on hold", [Call#call.id]),
	freeswitch:api(Fnode, uuid_setvar_multi, Callid ++ " hangup_after_bridge=false;park_after_bridge=true"),
	?DEBUG("Ringid:  ~p", [Ringid]),
	ok = fs_send_execute(Fnode, Ringid, "set", "hangup_after_bridge=false"),
	ok = fs_send_execute(Fnode, Ringid, "set", "park_after_bridge=true"),
	Res = freeswitch:api(Fnode, uuid_transfer, Ringid ++ " park inline"),
	?ERROR("Res of the api:  ~p", [Res]),
	case Muzak of
		none -> ok;
		_ ->
			?ERROR("5 ~p", [fs_send_execute(Fnode, Callid, "playback", "local_stream://" ++ Muzak)])
	end,
	{noreply, State#state{statename = oncall_hold}};

%% oncall hold -> next_state
handle_cast(toggle_hold, _Statename, Call, _GenMediaState, #state{statename = oncall_hold} = State) ->
	?INFO("Gonna try to pick up the holder dude for ~s", [Call#call.id]),
	#state{cnode = Fnode, ringuuid = Ringid} = State,
	#call{id = Callid} = Call,
	freeswitch:api(Fnode, uuid_bridge, Callid ++ " " ++ Ringid),
	freeswitch:api(Fnode, uuid_setvar_multi, Callid ++ " hangup_after_bridge=true;park_after_bridge=false"),
	{noreply, State#state{statename = oncall}};

handle_cast({contact_3rd_party, _Args} = Cast, Statename, Call, GenMediaState, #state{statename = oncall_hold, cnode = Fnode} = State) ->
	% first step is to move to hold_conference state, which means 
	% creating the conference.
	{ok, ConfId} = freeswitch:api(Fnode, create_uuid),
	case freeswitch:api(Fnode, uuid_transfer, Call#call.id ++ " conference:" ++ 
		ConfId ++ " inline") of
		{ok, Res} ->
			?INFO("Success result creating conferance and transfering call to it:  ~p", [Res]),
			% okay, solidify the conference state change, and go on.
			Newstate = State#state{conference_id = ConfId, statename = hold_conference},
			handle_cast(Cast, Statename, Call, GenMediaState, Newstate);
		Else ->
			?ERROR("Could not create conference:  ~p", [Else]),
			{noreply, State}
	end;

%% hold_conference -> 3rd_party | in_conference
handle_cast({contact_3rd_party, Destination}, _Statename, Call, _GenMediaState, #state{statename = hold_conference, cnode = Fnode} = State) ->
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
			{noreply, State#state{'3rd_party_id' = UUID, statename = hold_conference_3rdparty}};
		Else ->
			?WARNING("Failed to contact 3rd party ~s due to:  ~p", [Destination, Else]),
			{noreply, State}
	end;

handle_cast(retrieve_conference, _Statename, Call, _GenMediaState, #state{
		statename = Statename} = State) when Statename =:= 'hold_conference';
		Statename =:= 'hold_conference_3rdparty' ->
	#state{cnode = Fnode, ringuuid = Ringid, conference_id = Confid} = State,
	freeswitch:api(Fnode, uuid_transfer, Ringid ++ " 'conference:" ++ Confid ++ "' inline"),
	NewState = case Statename of
		'hold_conference' -> 'in_conference';
		'hold_conference_3rdparty' -> 'in_conference_3rdparty'
	end,
	cdr:media_custom(Call, NewState, ?cdr_states, []),
	{{mediapush, NewState}, State#state{statename = NewState}};

% 3rd_party -> hold_conference_3rdparty | in_conference | hold_conference
handle_cast(toggle_hold, _Statename, Call, _GenMediaState, #state{statename = '3rd_party'} = State) ->
	?INFO("Place 3rd party on hold", []),
	#state{cnode = Fnode, moh = Muzak, ringuuid = Ringid, '3rd_party_id' = ThirdPId} = State,
	ok = fs_send_execute(Fnode, Ringid, "set", "hangup_after_bridge=false"),
	ok = fs_send_execute(Fnode, Ringid, "set", "park_after_bridge=true"),
	freeswitch:api(Fnode, uuid_transfer, Ringid ++ " park inline"),
	% RETRIEVE_CONFERENCE breaks with this code; just specifying "park"
	% works, though.
	Helddp = "park",
	freeswitch:api(Fnode, uuid_transfer, ThirdPId ++ " " ++ Helddp ++ " inline"),
	{noreply, State#state{statename = hold_conference_3rdparty}};

handle_cast(retrieve_conference, Statename, Call, GenMediaState, #state{statename = '3rd_party'} = State) ->
	?INFO("Place 3rd party on hold, and go to the conference", []),
	{noreply, MidState} = handle_cast(toggle_hold, Statename, Call, GenMediaState, State),
	{noreply, MidState#state{statename = in_conference_3rdparty}};

handle_cast(retrieve_3rd_party, _Statename, Call, _GenMediaState, #state{statename = 'in_conference_3rdparty'} = State) ->
	?INFO("Taking agent out of conference, briding to 3rdparyt",[]),
	#state{cnode = Fnode, ringuuid = Ringid, '3rd_party_id' = Thirdy} = State,
	freeswitch:api(Fnode, uuid_bridge, Thirdy ++ " " ++ Ringid),
	{noreply, State#state{statename = '3rd_party'}};

handle_cast(hangup_3rd_party, Statename, Call, GenMediaState, #state{statename = '3rd_party'} = State) ->
	?INFO("killing 3rd party channel while talking w/ them", []),
	{noreply, MidState} = handle_cast(toggle_hold, Statename, Call, GenMediaState, State),
	handle_cast(hangup_3rd_party, Statename, Call, GenMediaState, MidState);

handle_cast({merge_3rd_party, _IncludeSelf}, _Statename, Call, _GenMediaState, #state{'3rd_party_id' = undefined} = State) ->
	{noreply, State};

handle_cast({merge_3rd_party, IncludeAgent}, _Statename, Call, _GenMediaState, State) ->
	#state{cnode = Fnode, ringuuid = Ringid, '3rd_party_id' = Thirdid, conference_id = Confid} = State,
	NextState = case IncludeAgent of
		true -> 'in_conference';
		_ -> 'hold_conference'
	end,
	freeswitch:api(Fnode, uuid_transfer, Ringid ++ " park inline"),
	freeswitch:bgapi(Fnode, uuid_transfer, Thirdid ++ " conference:" ++ Confid ++ " inline"),
	{noreply, State#state{statename = NextState}};

% retrieve conference also works here.
handle_cast(retrieve_3rd_party, _Statename, Call, _GenMediaState, #state{statename = hold_conference_3rdparty} = State) ->
	#state{cnode= Fnode, ringuuid = Ringid, '3rd_party_id' = Thirdpid} = State,
	?INFO("Picking up help 3rd party:  ~s", [Thirdpid]),
	freeswitch:bgapi(Fnode, uuid_bridge, Thirdpid ++ " " ++ Ringid),
	{noreply, State#state{statename = '3rd_party'}};

handle_cast(hangup_3rd_party, _Statename, Call, _GenMediaState, #state{statename = 'hold_conference_3rdparty'} = State) ->
	?INFO("hangling up on 3rd party", []),
	#state{cnode = Fnode, '3rd_party_id' = Thirdid} = State,
	freeswitch:bgapi(Fnode, uuid_kill, Thirdid),
	{noreply, State#state{statename = 'hold_conference'}};

% in_conference -> '3rdparty' | 'conference_hold'
handle_cast(toggle_hold, _Statename, Call, _GenMediaState, #state{statename = 'in_conference'} = State) ->
	?INFO("Place conference on hold", []),
	#state{cnode = Fnode, ringuuid = Ringid} = State,
	ok = fs_send_execute(Fnode, Ringid, "set", "hangup_after_bridge=false"),
	ok = fs_send_execute(Fnode, Ringid, "set", "park_after_bridge=true"),
	freeswitch:api(Fnode, uuid_transfer, Ringid ++ " park inline"),
	{{mediapush, hold_conference}, State#state{statename = hold_conference}};

handle_cast({contact_3rd_party, _Targ} = Cast, Statename, Call, GenMediaState, #state{statename = 'in_conference'} = State) ->
	?INFO("contact 3rd party, means place conference on hold first", []),
	{noreply, MidState} = handle_cast(toggle_hold, Statename, Call, GenMediaState, State),
	handle_cast(Cast, Statename, Call, GenMediaState, MidState);

% any state.
handle_cast({audio_level, Target, Level}, _Statename, Call, _GenMediaState, #state{statename = Statename} =
		State) when Statename == oncall; Statename == oncall_ringing ->
	?INFO("uuid_audio for ~s with direction ~s set to ~p", [Call#call.id, Target, Level]),
	ApiStr = Call#call.id ++ " start " ++ binary_to_list(Target) ++ " level "
		++ integer_to_list(Level),
	freeswitch:bgapi(State#state.cnode, uuid_audio, ApiStr),
	{noreply, State};

handle_cast({blind_transfer, Destination}, _Statename, Call, _GenMediaState, #state{statename = oncall} = State) ->
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

handle_cast({play_dtmf, []}, _Statename, _Call, _GenMediaState, State) ->
	?DEBUG("Dtmf not played due to no digits", []),
	{noreply, State};

handle_cast({play_dtmf, Digits}, Statename, Call, GenMediaState, State) when is_binary(Digits) ->
	handle_cast({play_dtmf, binary_to_list(Digits)}, Statename, Call, GenMediaState, State);

handle_cast({play_dtmf, Digits}, _Statename, Call, _GenMediaState, #state{statename = oncall} = State) ->
	#state{cnode = Fnode} = State,
	#call{id = UUID} = Call,
	freeswitch:bgapi(Fnode, uuid_send_dtmf, UUID ++ " " ++ Digits),
	{noreply, State};

%% web api's
handle_cast({agent_web_connection, <<"toggle_hold">>, _}, Statename, Call, GenMediaState, State) ->
	handle_cast(toggle_hold, Statename, Call, GenMediaState, State);

handle_cast({agent_web_connection, <<"retrieve_3rd_party">>, _}, Statename, Call, GenMediaState, State) ->
	handle_cast(retrieve_3rd_party, Statename, Call, GenMediaState, State);

handle_cast({agent_web_connection, <<"contact_3rd_party">>, Args}, Statename, Call, GenMediaState, State) ->
	Destination = binary_to_list(proplists:get_value("args", Args)),
	handle_cast({contact_3rd_party, Destination}, Statename, Call, GenMediaState, State);

handle_cast({agent_web_connection, <<"retrieve_conference">>, _Args}, Statename, Call, GenMediaState, State) ->
	handle_cast(retrieve_conference, Statename, Call, GenMediaState, State);

handle_cast({agent_web_connection, <<"merge_3rd_party">>, Args}, Statename, Call, GenMediaState, State) ->
	IncludeAgent = case proplists:get_value("args", Args) of
		<<"true">> ->
			true;
		_ ->
			false
	end,
	handle_cast({merge_3rd_party, IncludeAgent}, Statename, Call, GenMediaState, State);

handle_cast({agent_web_connection, <<"hangup_3rd_party">>, _}, Statename, Call, GenMediaState, State) ->
	handle_cast(hangup_3rd_party, Statename, Call, GenMediaState, State);

handle_cast({agent_web_connection, <<"audio_level">>, Arguments}, Statename, Call, GenMediaState, State) ->
	[Target, Level] = case proplists:get_value("args", Arguments, [<<"read">>, 0]) of
		[<<"write">>, N] -> [write, N];
		[_,N] -> [read,N];
		_ -> [read,0]
	end,
	handle_cast({audio_level, Target, Level}, Statename, Call, GenMediaState, State);

handle_cast({agent_web_connection, <<"blind_transfer">>, Args}, Statename, Call, GenMediaState, State) ->
	Dest = proplists:get_value("args", Args),
	handle_cast({blind_transfer, Dest}, Statename, Call, GenMediaState, State);

handle_cast({agent_web_connection, <<"play_dtmf">>, Args}, Statename, Call, GMState, State) ->
	Digits = case proplists:get_value("args", Args, []) of
		X when is_integer(X) -> integer_to_list(X);
		X -> X
	end,
	handle_cast({play_dtmf, Digits}, Statename, Call, GMState, State);

%% tcp api's
handle_cast(Request, Statename, Call, GenMediaState, State) when is_record(Request, mediacommandrequest) ->
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
					handle_cast({audio_level, Target, Val}, Statename, Call, GenMediaState, State);
				_ ->
					{noreply, State}
			end;
		'TOGGLE_HOLD' ->
			handle_cast(toggle_hold, Statename, Call, GenMediaState, State);
		'CONTACT_3RD_PARTY' ->
			case cpx_freeswitch_pb:get_extension(FixedRequest, contact_3rd_party) of
				undefined -> handle_cast(contact_3rd_party, Statename, Call, GenMediaState, State);
				{ok, #contact3rdpartyrequest{target = Target}} ->
					handle_cast({contact_3rd_party, Target}, Statename, Call, GenMediaState, State)
			end;
		'RETRIEVE_CONFERENCE' -> handle_cast(retrieve_conference, Statename, Call, GenMediaState, State);
		'MERGE_3RD_PARTY' ->
			case cpx_freeswitch_pb:get_extension(FixedRequest, merge_3rd_party) of
				undefined ->
					handle_cast({merge_3rd_party, false}, Statename, Call, GenMediaState, State);
				{ok, #merge3rdpartyrequest{include_self = AndSelf}} ->
					handle_cast({merge_3rd_party, AndSelf}, Statename, Call, GenMediaState, State)
			end;
		'RETRIEVE_3RD_PARTY' ->
			handle_cast(retrieve_3rd_party, Statename, Call, GenMediaState, State);
		'HANGUP_3RD_PARTY' ->
			handle_cast(hangup_3rd_party, Statename, Call, GenMediaState, State);
		'BLIND_TRANSFER' ->
			case cpx_freeswitch_pb:get_extension(FixedRequest,blind_transfer) of
				{ok, #blindtransferrequest{target = Dest}} ->
					handle_cast({blind_transfer, Dest}, Statename, Call, GenMediaState, State);
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
					handle_cast({send_dtmf, Dtmf}, Statename, Call, GenMediaState, State);
				DtmfIgnored ->
					?DEBUG("dtmf request ignored:  ~p", [DtmfIgnored]),
					{noreply, State}
			end;
		FullReqIgnored ->
			?DEBUG("Request fully ignored:  ~p", [FullReqIgnored]),
			{noreply, State}
	end;

handle_cast({'3rd_party_pickup', ChanPid}, _Statename, Call, _GenMediaState, #state{'3rd_party_mon' = {ChanPid, ChanMon}} = State) ->
	#state{cnode = Fnode, '3rd_party_id' = OtherParty, ringuuid = AgentChan} = State,
	freeswitch:bgapi(Fnode, uuid_bridge, OtherParty ++ " " ++ AgentChan),
	{noreply, State#state{statename = '3rd_party'}};
	
handle_cast({set_caseid, CaseID}, _Statename, Call, _GenMediaState, State) ->
	?INFO("setting caseid for ~p to ~p", [Call#call.id, CaseID]),
	{noreply, State#state{caseid = CaseID}};

handle_cast(Msg, _, _Call, _, State) ->
	?DEBUG("unhandled cast while in state ~p:  ~p", [State#state.statename, Msg]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info(check_recovery, _StateName, Call, _Internal, State) ->
	case whereis(freeswitch_media_manager) of
		Pid when is_pid(Pid) ->
			link(Pid),
			gen_server:cast(freeswitch_media_manager, {notify, Call#call.id, self()}),
			{noreply, State#state{manager_pid = Pid}};
		_Else ->
			{ok, Tref} = timer:send_after(1000, check_recovery),
			{noreply, State#state{manager_pid = Tref}}
	end;

handle_info({'EXIT', Pid, Reason}, StateName, Call, _Internal, 
		#state{ringchannel = Pid} = State) when StateName =:= oncall_ringing;
		StateName =:= inqueue_ringing ->
	?WARNING("Handling ring channel ~w exit ~p for ~p", [Pid, Reason, Call#call.id]),
	{stop_ring, State#state{ringchannel = undefined}};

handle_info({'EXIT', Pid, noconnection}, _StateName, _Call, _Internal,
		State) ->
	?WARNING("Exit of ~p due to noconnection; this normally indicates a fatal freeswitch failure, so going down too.", [Pid]),
	{stop, noconnection, State};

handle_info({'EXIT', Pid, Reason}, _StateName, Call, _Internal, #state{manager_pid = Pid} = State) ->
	?WARNING("Handling manager exit from ~w due to ~p for ~p", [Pid, Reason, Call#call.id]),
	{ok, Tref} = timer:send_after(1000, check_recovery),
	{noreply, State#state{manager_pid = Tref}};

%% TODO make awesome by taking advantage of the state machine-ness.
handle_info({call, {event, [UUID | Rest]}}, _StateName, Call, _Internal, State) when is_list(UUID) ->
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

handle_info({call_event, {event, [UUID | Rest]}}, _StateName, Call, _Internal, State) when is_list(UUID) ->
	case_event_name([ UUID | Rest], Call, State);

handle_info({set_agent, Login, Apid}, _StateName, _Call, _Intenral, State) ->
	{noreply, State#state{agent = Login, agent_pid = Apid}};

handle_info({bgok, Reply}, _StateName, Call, _Internal, State) ->
	?DEBUG("bgok:  ~p for ~p", [Reply, Call#call.id]),
	{noreply, State};

handle_info({bgerror, "-ERR NO_ANSWER\n"}, _StateName, Call, _Internal, State) ->
	?INFO("Potential ringout.  Statecook:  ~p for ~p", [State#state.cook, Call#call.id]),
	%% the apid is known by gen_media, let it handle if it is not not.
	{stop_ring, State};

handle_info({bgerror, "-ERR USER_BUSY\n"}, _StateName, Call, _Internal, State) ->
	?NOTICE("Agent rejected the call ~p", [Call#call.id]),
	{stop_ring, State};

handle_info({bgerror, Reply}, _StateName, Call, _Internal, State) ->
	?WARNING("unhandled bgerror: ~p for ~p", [Reply, Call#call.id]),
	{noreply, State};

handle_info(channel_destroy, _StateName, Call, _Internal, #state{in_control = InControl} = State) when not InControl ->
	?NOTICE("Hangup in IVR for ~p", [Call#call.id]),
	{stop, hangup, State};
% TODO This had a use at some point, but was cuasing ramdom hangups.
% need to find what was sending :(
%handle_info(call_hangup, Call, State) ->
%	?NOTICE("Call hangup info, terminating ~p", [Call#call.id]),
%	catch freeswitch_ring:hangup(State#state.ringchannel),
%	{stop, normal, State};

handle_info({'DOWN', Ref, process, Pid, Cause}, _Statename, Call,
		_Internal, #state{statename = oncall,
		spawn_oncall_mon = {Pid, Ref}} = State) ->
	?DEBUG("Oncaller pid termination cause:  ~p", [Cause]),
	cdr:media_custom(Call, oncall, ?cdr_states, []),
	{{mediapush, caller_offhold}, State#state{spawn_oncall_mon = undefined}};

handle_info(call_hangup, _StateName, Call, _Internal, State) ->
	?NOTICE("Call hangup info, terminating ~p", [Call#call.id]),
	catch freeswitch_ring:hangup(State#state.ringchannel),
	{stop, normal, State};

handle_info(Info, _StateName, Call, _Internal, State) ->
	?INFO("unhandled info ~p for ~p", [Info, Call#call.id]),
	?DEBUG("unhandled state:  ~p", [State]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @private
terminate(Reason, _StateName, Call, _Internal, _State) ->
	?NOTICE("terminating: ~p ~p", [Reason, Call#call.id]),
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
	?DEBUG("Event ~p for ~s while in state ~p", [Ename, UUID, State#state.statename]),
	case_event_name(Ename, UUID, Rawcall, Callrec, State).

%% @private
case_event_name({"CUSTOM", "conference::maintenance"}, UUID, _Rawcall, Callrec, #state{statename = Statename, '3rd_party_id' = UUID} = State) when 
	Statename =:= 'in_conference'; Statename =:= 'hold_conference' ->
		?INFO("finishing up a 3rd paryt merge",[]),
		case Statename of
			in_conference ->
				#state{cnode = Fnode, conference_id = Confid, ringuuid = Ringid} = State,
				freeswitch:api(Fnode, uuid_transfer, Ringid ++ " 'conference:" ++ Confid ++ "' inline");
			_ ->
				ok
		end,
		cdr:media_custom(Callrec, Statename, ?cdr_states, []),
		{{mediapush, Statename}, State#state{'3rd_party_id' = undefined}};

case_event_name("CHANNEL_ANSWER", UUID, _Rawcall, Callrec, #state{
		statename = hold_conference_3rdparty, '3rd_party_id' = UUID} = State) ->
	#state{cnode = Fnode, ringuuid = Ruuid} = State,
	freeswitch:api(Fnode, uuid_setvar_multi, Ruuid ++ " hangup_after_bridge=false;park_after_bridge=true"),
	freeswitch:bgapi(Fnode, uuid_bridge, UUID ++ " " ++ Ruuid),
	cdr:media_custom(Callrec, '3rd_party', ?cdr_states, []),
	{{mediapush, '3rd_party'}, State#state{statename = '3rd_party'}};

case_event_name("CHANNEL_BRIDGE", UUID, _Rawcall, Callrec, #state{'3rd_party_id' = UUID, statename = '3rd_party'} = State) ->
	?DEBUG("Telling agent we're now oncall w/ the 3rd party", []),
	cdr:media_custom(Callrec, '3rd_party', ?cdr_states, []),
	{{mediapush, '3rd_party'}, State};

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

case_event_name("CHANNEL_PARK", Thirdy, Rawcall, Callrec, #state{
	statename = 'in_conference_3rdparty', '3rd_party_id' = Thirdy} = State) ->
		#state{conference_id = Confid, cnode = Fnode, ringuuid = Ringid} = State,
		?DEBUG("Most likely in the middle of putting the agent in a conference", []),
	freeswitch:api(Fnode, uuid_transfer, Ringid ++ " 'conference:" ++ Confid ++ "' inline"),
	cdr:media_custom(Callrec, State#state.statename, ?cdr_states, []),
	{{mediapush, 'in_conference_3rdparty'}, State};
	
case_event_name("CHANNEL_PARK", UUID, Rawcall, Callrec, #state{statename = hold_conference_3rdparty, '3rd_party_id' = UUID} = State) ->
	?DEBUG("park of the 3rd party, proll a hold", []),
	cdr:media_custom(Callrec, State#state.statename, ?cdr_states, []),
	{{mediapush, State#state.statename}, State};

case_event_name("CHANNEL_PARK", UUID, Rawcall, Callrec, #state{
		statename = oncall_hold, uuid = UUID} = State) ->
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
	cdr:media_custom(Callrec, 'oncall_hold', ?cdr_states, []),
	{{mediapush, caller_hold}, State};

case_event_name("CHANNEL_DESTROY", UUID, Rawcall, Callrec, #state{
		'3rd_party_id' = UUID, statename = Statename} = State) ->
	{{mediapush, Statename}, State#state{'3rd_party_id' = undefined}};

case_event_name("CHANNEL_PARK", UUID, Rawcall, Callrec, #state{
		uuid = UUID, queued = false, statename = Statename} = State) when
		Statename == inivr ->
	Queue = proplists:get_value("variable_queue", Rawcall, "default_queue"),
	Client = {proplists:get_value("variable_brand", Rawcall),[{"dial_vars", State#state.dial_vars} | get_client_options(Rawcall)]},
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
	freeswitch:bgapi(State#state.cnode, uuid_setvar, UUID ++ " hangup_after_bridge true"),
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
	{queue, Queue, NewCall, State#state{queue = Queue, queued=true, allow_voicemail=AllowVM, vm_priority_diff = VMPriorityDiff, moh=Moh, ivroption = Ivropt, statename = inqueue}};

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
%	{hangup, State2};
%"CHANNEL_HANGUP_COMPLETE" ->
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
	Hupstates = [oncall, oncall_hold, inqueue, inqueue_ringing, inivr],
	Statename = State#state.statename,
	case lists:member(Statename, Hupstates) of
		true -> {{hangup, Who}, State2};
		_ -> {noreply, State2}
	end;

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

get_client_options(Proplist) ->
	ExportVars = string:tokens(proplists:get_value("variable_export_vars", Proplist, ""), ","),
	VarNames = ["variable_" ++ V || V <- ExportVars],
	VarValues = [proplists:get_value(N, Proplist, "null") || N <- VarNames,
		proplists:get_value(N, Proplist) =/= undefined],
	VarValues1 = lists:map(fun(Elem) ->
		try mochijson2:decode(Elem) of
			null -> undefined;
			Else when is_binary(Else) -> Else;
			{struct, Else} -> Else;
			Else -> Else
		catch
			error:{case_clause,_} -> Elem;
			error:{badmatch,any} -> undefined
		end
	end, VarValues),
	lists:zip(ExportVars,VarValues1).

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
			get_info(Cnode, UUID, Retries + 1);
		{error, Error} ->
			?WARNING("uuid_dump for ~s errored:  ~p. Retrying", [UUID, Error]),
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
