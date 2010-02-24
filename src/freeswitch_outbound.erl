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
%%	Micah Warren <micahw at fusedsolutions dot com>
%%

%% @doc Helper module for freeswitch media to make an outbound call.
-module(freeswitch_outbound).
-author("Micah").

-behaviour(gen_media).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("queue.hrl").
-include("call.hrl").
-include("agent.hrl").


%% API
-export([
	start_link/6,
	start/6,
	hangup/1
	]).

%% gen_server callbacks
-export([
	init/1,
	handle_announce/3,
	handle_answer/3,
	handle_ring/3,
	handle_voicemail/3,
	handle_spy/3,
	handle_ring_stop/2,
	handle_agent_transfer/4,
	handle_queue_transfer/2,
	handle_wrapup/2,
	handle_call/4,
	handle_cast/3,
	handle_info/3,
	handle_warm_transfer_begin/3,
	handle_warm_transfer_cancel/2,
	handle_warm_transfer_complete/2,
	terminate/3,
	code_change/4]).

-record(state, {
	cnode :: atom(),
	agent_pid :: pid(),
	agent :: string(),
	ringchannel :: pid(),
	xferchannel :: pid(),
	xferuuid :: string(),
	voicemail = false :: 'false' | string(),
	dialstring :: string(),
	caseid :: string() | 'undefined',
	record_path :: 'undefined' | string(),
	warm_transfer_uuid = undefined :: string() | 'undefined'
	}).

-type(state() :: #state{}).
-define(GEN_MEDIA, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================

-spec(start/6 :: (Fnode :: atom(), AgentRec :: #agent{}, Apid :: pid(), Number :: any(), DialString :: string(), Ringout :: pos_integer()) -> {'ok', pid()}).
start(Fnode, AgentRec, Apid, Number, DialString, Ringout) when is_pid(Apid) ->
	gen_media:start(?MODULE, [Fnode, AgentRec, Apid, Number, DialString, Ringout]).

-spec(start_link/6 :: (Fnode :: atom(), AgentRec :: #agent{}, Apid :: pid(), Number :: any(), DialString :: string(), Ringout :: pos_integer()) -> {'ok', pid()}).
start_link(Fnode, AgentRec, Apid, Number, DialString, Ringout) when is_pid(Apid) ->
	gen_media:start_link(?MODULE, [Fnode, AgentRec, Apid, Number, DialString, Ringout]).

-spec(hangup/1 :: (Pid :: pid()) -> 'ok').
hangup(Pid) ->
	gen_media:cast(Pid, hangup).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Fnode, AgentRec, Apid, Client, DialString, _Ringout]) ->
	process_flag(trap_exit, true),
	case freeswitch:api(Fnode, create_uuid) of
		{ok, UUID} ->
			Call = #call{id=UUID, source=self(), type=voice, direction=outbound, client = Client, priority = 10},
			{ok, {#state{cnode = Fnode, agent_pid = Apid, dialstring = DialString, agent = AgentRec#agent.login}, Call, {precall, [Client]}}};
		Else ->
			?ERROR("create_uuid failed: ~p", [Else]),
			{stop, {error, Else}}
	end.

%%--------------------------------------------------------------------
%% Description: gen_media
%%--------------------------------------------------------------------

-spec(handle_announce/3 :: (Announcement :: string(), Callrec :: #call{}, State :: #state{}) -> {'ok', #state{}}).
handle_announce(Announcement, Callrec, State) ->
	freeswitch:sendmsg(State#state.cnode, Callrec#call.id,
		[{"call-command", "execute"},
			{"execute-app-name", "playback"},
			{"execute-app-arg", Announcement}]),
	{ok, State}.

handle_answer(Apid, Callrec, #state{xferchannel = XferChannel, xferuuid = XferUUID} = State) when is_pid(XferChannel) ->
	link(XferChannel),
	?INFO("intercepting ~s from channel ~s", [XferUUID, Callrec#call.id]),
	freeswitch:sendmsg(State#state.cnode, XferUUID,
		[{"call-command", "execute"}, {"execute-app-name", "intercept"}, {"execute-app-arg", Callrec#call.id}]),
	agent:conn_cast(Apid, {mediaload, Callrec, [{<<"height">>, <<"300px">>}]}),
	{ok, State#state{agent_pid = Apid, ringchannel = XferChannel,
			xferchannel = undefined, xferuuid = undefined}};
handle_answer(_Apid, _Call, State) ->
	{error, outgoing_only, State}.

handle_ring(_Apid, _Call, State) ->
	{invalid, State}.
	
handle_ring_stop(_Call, State) ->
	{ok, State}.

-spec(handle_voicemail/3 :: ('undefined', Call :: #call{}, State :: #state{}) -> {'ok', #state{}}).
handle_voicemail(undefined, Call, State) ->
	UUID = Call#call.id,
	freeswitch:bgapi(State#state.cnode, uuid_transfer, UUID ++ " 'playback:voicemail/vm-record_message.wav,record:/tmp/${uuid}.wav' inline"),
	{ok, State#state{voicemail = "/tmp/"++UUID++".wav"}}.

-spec(handle_spy/3 :: (Agent :: pid(), Call :: #call{}, State :: #state{}) -> {'error', 'bad_agent', #state{}} | {'ok', #state{}}).
handle_spy(Agent, Call, #state{cnode = Fnode, ringchannel = Chan} = State) when is_pid(Chan) ->
	case agent_manager:find_by_pid(Agent) of
		notfound ->
			{error, bad_agent, State};
		AgentName ->
			agent:blab(Agent, "While spying, you have the following options:\n"++
				"* To whisper to the agent; press 1\n"++
				"* To whisper to the caller; press 2\n"++
				"* To talk to both parties; press 3\n"++
				"* To resume spying; press 0"),
			freeswitch:bgapi(Fnode, originate, "user/" ++ re:replace(AgentName, "@", "_", [{return, list}]) ++ " &eavesdrop(" ++ Call#call.id ++ ")"),
			{ok, State}
	end;
handle_spy(_Agent, _Call, State) ->
	{invalid, State}.

handle_agent_transfer(AgentPid, Timeout, Call, State) ->
	?INFO("transfer_agent to ~p for call ~p", [AgentPid, Call#call.id]),
	AgentRec = agent:dump_state(AgentPid),
	% fun that returns another fun when passed the UUID of the new channel
	% (what fun!)
	F = fun(_UUID) ->
		fun(ok, _Reply) ->
			% agent picked up?
				?INFO("Agent transfer picked up ~p?~n", [Call#call.id]);
		(error, Reply) ->
			?WARNING("originate agent_transfer for ~p failed: ~p", [Call#call.id, Reply])
		end
	end,
	case freeswitch_ring:start_link(State#state.cnode, AgentRec, AgentPid, Call, Timeout, F, [single_leg, no_oncall_on_bridge]) of
		{ok, Pid} ->
			{ok, [{"caseid", State#state.caseid}], State#state{xferchannel = Pid, xferuuid = freeswitch_ring:get_uuid(Pid)}};
		{error, Error} ->
			?ERROR("error starting ring channel for ~p:  ~p", [Call#call.id, Error]),
			{error, Error, State}
	end.

handle_queue_transfer(Call, #state{cnode = Fnode} = State) ->
	freeswitch:api(Fnode, uuid_park, Call#call.id),
	% play musique d'attente
	freeswitch:sendmsg(Fnode, Call#call.id,
		[{"call-command", "execute"},
			{"execute-app-name", "playback"},
			{"execute-app-arg", "local_stream://moh"}]),
	{ok, State}.

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
									freeswitch:api(Node, uuid_record, Call#call.id ++ " stop " ++ Path),
									freeswitch:api(Node, uuid_record, NewUUID ++ " start " ++ Path)
							end,
							Self ! warm_transfer_succeeded;
						_ ->
							ok
					end,
					true
			end,

			AgentState = agent:dump_state(AgentPid),

			case freeswitch_ring:start(Node, AgentState, AgentPid, Call, 600, F, [no_oncall_on_bridge, {eventfun, F2}]) of
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
					freeswitch:api(Node, uuid_record, Call#call.id ++ " stop " ++ Path),
					freeswitch:api(Node, uuid_record, NewUUID ++ " start " ++ Path)
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

			freeswitch:bgapi(State#state.cnode, uuid_transfer,
				freeswitch_ring:get_uuid(State#state.ringchannel) ++ Dialplan), 

			% play musique d'attente 
			freeswitch:sendmsg(Node, Call#call.id,
				[{"call-command", "execute"},
					{"execute-app-name", "playback"},
					{"execute-app-arg", "local_stream://moh"}]),
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
			freeswitch:api(Node, uuid_record, WUUID ++ " stop " ++ Path),
			freeswitch:api(Node, uuid_record, Call#call.id ++ " start " ++ Path)
	end,

	%Result = freeswitch:sendmsg(State#state.cnode, RUUID,
		%[{"call-command", "execute"}, {"execute-app-name", "intercept"}, {"execute-app-arg", Call#call.id}]),
	%?NOTICE("intercept result: ~p", [Result]),
	Result = freeswitch:api(State#state.cnode, uuid_bridge,  RUUID ++" " ++Call#call.id),
	?INFO("uuid_bridge result for ~p to ~p: ~p", [RUUID, Call#call.id, Result]),
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
									freeswitch:api(Node, uuid_record, WUUID ++ " stop " ++ Path),
									freeswitch:api(Node, uuid_record, Call#call.id ++ " start " ++ Path)
							end,
							freeswitch:api(Node, uuid_bridge, RingUUID++" "++Call#call.id);
						(error, Reply) ->
							?WARNING("originate failed for ~p : ~p", [Call#call.id, Reply]),
							ok
					end
			end,

			AgentState = agent:dump_state(AgentPid),

			case freeswitch_ring:start(Node, AgentState, AgentPid, Call, 600, F, []) of
				{ok, Pid} ->
					link(Pid),
					{ok, State#state{ringchannel = Pid, warm_transfer_uuid = undefined}};
				{error, Error} ->
					?ERROR("error starting ring channel for ~p:  ~p", [Call#call.id, Error]),
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
			freeswitch:api(Node, uuid_record, WUUID ++ " stop " ++ Path)
	end,

	%Result = freeswitch:sendmsg(State#state.cnode, WUUID,
		%[{"call-command", "execute"}, {"execute-app-name", "intercept"}, {"execute-app-arg", Call#call.id}]),
	%?INFO("intercept result: ~p", [Result]),
	Result = freeswitch:api(State#state.cnode, uuid_bridge,  WUUID ++" " ++Call#call.id),
	?INFO("uuid_bridge result for ~p: ~p", [Call#call.id, Result]),
	{ok, State#state{warm_transfer_uuid = undefined}};
handle_warm_transfer_complete(_Call, State) ->
	{error, "Not in warm transfer", State}.


handle_wrapup(_Call, State) ->
	{ok, State}.
	
%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({dial, Number}, _From, Call, #state{cnode = Fnode, dialstring = DialString, agent_pid = Apid} = State) ->
	?NOTICE("I'm supposed to dial ~p for ~p", [Number, Call#call.id]),
	Self = self(),
	AgentRec = agent:dump_state(Apid),
	F = fun(RingUUID) ->
			fun(ok, _Reply) ->
					Client = Call#call.client,
					CalleridArgs = case proplists:get_value(<<"callerid">>, Client#client.options) of
						undefined ->
							["origination_privacy=hide_namehide_number"];
						CalleridNum ->
							["origination_caller_id_name='"++Client#client.label++"'", "origination_caller_id_number='"++binary_to_list(CalleridNum)++"'"]
					end,

					freeswitch:bgapi(Fnode, uuid_setvar, RingUUID ++ " ringback %(2000,4000,440.0,480.0)"),
					%freeswitch:bgapi(Fnode, uuid_setvar, RingUUID ++ " ringback tone_stream://path=/usr/local/freeswitch/conf/tetris.ttml;loops=10"),

					freeswitch:sendmsg(Fnode, RingUUID,
						[{"call-command", "execute"},
							{"execute-app-name", "bridge"},
							{"execute-app-arg", freeswitch_media_manager:do_dial_string(DialString, Number, ["origination_uuid="++Call#call.id | CalleridArgs])}]),
					Self ! {connect_uuid, Number};
				(error, Reply) ->
					?WARNING("originate failed for ~p: ~p; agent:  ~s", [Call#call.id, Reply, AgentRec#agent.login]),
					ok
			end
	end,
	RecPath = case cpx_supervisor:get_archive_path(Call) of
		none ->
			?DEBUG("archiving is not configured for ~p", [Call#call.id]),
			undefined;
		{error, _Reason, Path} ->
			?WARNING("Unable to create requested call archiving directory for recording ~p for ~p", [Path, Call#call.id]),
			undefined;
		Path ->
			Path++".wav"
	end,

	F2 = fun(_RingUUID, EventName, _Event) ->
			case EventName of
				"CHANNEL_BRIDGE" ->
					agent:conn_cast(Apid, {mediaload, Call, [{<<"height">>, <<"300px">>}]}),
					?DEBUG("archiving ~p to ~s.wav", [Call#call.id, RecPath]),
					freeswitch:api(State#state.cnode, uuid_setvar, Call#call.id ++ " RECORD_APPEND true"),
					freeswitch:api(Fnode, uuid_record, Call#call.id ++ " start "++RecPath),
					Self ! bridge;
				_ ->
					ok
			end,
			true
	end,
	case freeswitch_ring:start(Fnode, AgentRec, Apid, Call, 600, F, [no_oncall_on_bridge, {eventfun, F2}]) of
		{ok, Pid} ->
			link(Pid),
			cdr:dialoutgoing(Call, Number),
			{reply, ok, State#state{ringchannel = Pid, record_path = RecPath}};
		{error, Error} ->
			?ERROR("error creating ring channel for ~p:  ~p; agent:  ~s", [Call#call.id, Error, AgentRec#agent.login]),
			{reply, {error, Error}, State}
	end;
handle_call(Msg, _From, Call, State) ->
	?INFO("unhandled mesage ~p for ~p", [Msg, Call#call.id]),
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({"audiolevel", Arguments}, Call, State) ->
	?INFO("uuid_audio ~s", [Call#call.id++" start "++proplists:get_value("target", Arguments)++" level "++proplists:get_value("value", Arguments)]),
	freeswitch:bgapi(State#state.cnode, uuid_audio, Call#call.id++" start "++proplists:get_value("target", Arguments)++" level "++proplists:get_value("value", Arguments)),
	{noreply, State};
handle_cast({set_caseid, CaseID}, _Call, State) ->
	{noreply, State#state{caseid = CaseID}};
handle_cast(hangup, Call, State) ->
	freeswitch:sendmsg(State#state.cnode, Call#call.id,
		[{"call-command", "hangup"},
			{"hangup-cause", "NORMAL_CLEARING"}]),
	{noreply, State};
handle_cast(_Msg, _Call, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({call, {event, [UUID | _Rest]}}, #call{id = UUID}, State) ->
	?DEBUG("call ~p", [UUID]),
	{noreply, State};
handle_info({call_event, {event, [UUID | Rest]}}, #call{id = UUID}, State) ->
	Event = proplists:get_value("Event-Name", Rest),
	case Event of
		"CHANNEL_HANGUP" ->
			Elem1 = case proplists:get_value("variable_hangup_cause", Rest) of
				"NO_ROUTE_DESTINATION" ->
					?ERROR("No route to destination for outbound call ~p", [UUID]),
					noreply;
				"NORMAL_CLEARING" ->
					?INFO("Normal clearing ~p", [UUID]),
					wrapup;
				"USER_BUSY" ->
					?WARNING("Agent's phone rejected call ~p", [UUID]),
					noreply;
				"NO_ANSWER" ->
					?NOTICE("Agent rangout on outbound call ~p", [UUID]),
					noreply;
				Else ->
					?INFO("Hangup cause for ~p: ~p", [UUID, Else]),
					noreply
			end,
			{Elem1, State};
		"CHANNEL_HANGUP_COMPLETE" ->
			% TODO - this is protocol specific and we only handle SIP right now
			% TODO - this should go in the CDR
			case proplists:get_value("variable_sip_hangup_disposition", Rest) of
				"recv_bye" ->
					?DEBUG("Caller hungup ~p", [UUID]);
				"send_bye" ->
					?DEBUG("Agent hungup ~p", [UUID]);
				_ ->
					?DEBUG("I don't know who hung up ~p", [UUID])
				end,
			{noreply, State};
		_Else ->
			?DEBUG("call_event ~p for ~p", [Event, UUID]),
			{noreply, State}
	end;
handle_info(call_hangup, Call, State) ->
	?DEBUG("Call hangup info for ~p", [Call#call.id]),
	catch freeswitch_ring:hangup(State#state.ringchannel),
	{stop, normal, State};
handle_info({connect_uuid, Number}, #call{id = UUID} = Call, #state{cnode = Fnode, agent = Agent} = State) ->
	Gethandle = fun(Recusef, Count) ->
			?DEBUG("Counted ~p for ~p", [Count, Call#call.id]),
			case freeswitch:handlecall(Fnode, Call#call.id) of
				{error, badsession} when Count > 4 ->
					{error, badsession};
				{error, badsession} ->
					timer:sleep(100),
					Recusef(Recusef, Count+1);
				{error, Other} ->
					{error, Other};
				Else ->
					Else
			end
	end,
	case Gethandle(Gethandle, 0) of
		{error, badsession} ->
			?ERROR("bad uuid ~p", [UUID]),
			{stop, {error, session}, State};
		{error, Other} ->
			?ERROR("other error starting; ~p", [Other]),
			{stop, {error, Other}, State};
		_Else ->
			?NOTICE("starting for ~p", [UUID]),
			Client = Call#call.client,
			%% callerid internally is set to the brandname/dialed number
			{outbound, Agent, Call#call{callerid = {Client#client.label, Number}}, State}
	end;
handle_info(warm_transfer_succeeded, Call, #state{warm_transfer_uuid = W} = State) when is_list(W) ->
	?DEBUG("Got warm transfer success notification from ring channel for ~p", [Call#call.id]),
	agent:media_push(State#state.agent_pid, warm_transfer_succeeded),
	{noreply, State};
handle_info({'EXIT', Pid, Reason}, Call, #state{xferchannel = Pid} = State) ->
	?WARNING("Handling transfer channel ~w exit ~p for ~p", [Pid, Reason, Call#call.id]),
	{stop_ring, State#state{xferchannel = undefined}};
handle_info({'EXIT', Pid, Reason}, Call, #state{ringchannel = Pid, warm_transfer_uuid = W} = State) when is_list(W) ->
	?WARNING("Handling ring channel ~w exit ~p while in warm transfer for ~p", [Pid, Reason, Call#call.id]),
	agent:media_push(State#state.agent_pid, warm_transfer_failed),
	cdr:warmxfer_fail(Call, State#state.agent_pid),
	{noreply, State#state{ringchannel = undefined}};
handle_info({'EXIT', Pid, Reason}, Call, #state{ringchannel = Pid} = State) ->
	?WARNING("Handling ring channel ~w exit ~p for ~p", [Pid, Reason, Call#call.id]),
	{stop_ring, State#state{ringchannel = undefined}};
%handle_info({'EXIT', Pid, Reason}, Call, #state{manager_pid = Pid} = State) ->
	%?WARNING("Handling manager exit from ~w due to ~p for ~p", [Pid, Reason, Call#call.id]),
	%{ok, Tref} = timer:send_after(1000, check_recovery),
	%{noreply, State#state{manager_pid = Tref}};
handle_info(bridge, Call, #state{ringchannel = Pid, warm_transfer_uuid = W} = State) when is_list(W) ->
	?INFO("bridged when warm transfer... ~p", [Call#call.id]),
	agent:media_push(State#state.agent_pid, warm_transfer_succeeded),
	{noreply, State};
handle_info(Info, Call, State) ->
	?DEBUG("unhandled info ~p for ~p", [Info, Call#call.id]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(Reason, Call, _State) ->
	?NOTICE("FreeSWITCH outbound channel ~p teminating ~p", [Call#call.id, Reason]),
	ok.

%%--------------------------------------------------------------------
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, _Call, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
