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
	dialstring :: string()
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
	case freeswitch:api(Fnode, create_uuid) of
		{ok, UUID} ->
			Call = #call{id=UUID, source=self(), type=voice, direction=outbound, client = Client, priority = 10},
			{ok, {#state{cnode = Fnode, agent_pid = Apid, dialstring = DialString, agent = AgentRec#agent.login}, Call, {precall, [Client]}}};
		Else ->
			?ERROR("create_uuid failed: ~p", [Else]),
			{stop, {error, Else}}
	end.

	
	%case freeswitch:api(Fnode, create_uuid) of
		%{ok, UUID} ->
			%Call = #call{id=UUID, source=self(), type=voice, direction=outbound},
			%Self = self(),

			%F = fun(RingUUID) ->
				%fun(ok, _Reply) ->
					%freeswitch:sendmsg(Fnode, RingUUID,
						%[{"call-command", "execute"},
							%{"execute-app-name", "bridge"},
							%{"execute-app-arg", "[ignore_early_media=true,origination_uuid="++UUID++"]sofia/gateway/"++Gateway++"/"++Number}]),
					%Self ! connect_uuid;
				%(error, Reply) ->
					%?WARNING("originate failed: ~p", [Reply]),
					%ok
				%end
			%end,
			%F2 = fun(RingUUID, EventName, Event) ->
					%case EventName of
						%"CHANNEL_BRIDGE" ->
								%case cpx_supervisor:get_archive_path(Call) of
									%none ->
										%?DEBUG("archiving is not configured", []);
									%{error, Reason, Path} ->
										%?WARNING("Unable to create requested call archiving directory for recording ~p", [Path]);
									%Path ->
										%?DEBUG("archiving to ~s.wav", [Path]),
										%freeswitch:api(Fnode, uuid_record, UUID ++ " start "++Path++".wav")
								%end;
						%_ ->
							%ok
					%end,
					%true
			%end,

			%case freeswitch_ring:start(Fnode, AgentRec, Apid, Call, 600, F, [no_oncall_on_bridge, {eventfun, F2}]) of
				%{ok, Pid} ->
					%link(Pid),
					%{ok, {#state{cnode = Fnode, uuid = UUID, ringchannel = Pid, agent_pid = Apid, agent = AgentRec#agent.login}, Call}};
				%{error, Error} ->
					%?ERROR("error:  ~p", [Error]),
					%{stop, {error, Error}}
			%end;
		%Else ->
			%?ERROR("bgapi call failed ~p", [Else]),
			%{stop, {error, Else}}
	%end.

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
				?INFO("Agent transfer picked up?~n", []);
		(error, Reply) ->
			?WARNING("originate failed: ~p", [Reply])
		end
	end,
	case freeswitch_ring:start_link(State#state.cnode, AgentRec, AgentPid, Call, Timeout, F, [single_leg, no_oncall_on_bridge]) of
		{ok, Pid} ->
			{ok, State#state{xferchannel = Pid, xferuuid = freeswitch_ring:get_uuid(Pid)}};
		{error, Error} ->
			?ERROR("error:  ~p", [Error]),
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

handle_wrapup(_Call, State) ->
	{ok, State}.
	
%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({dial, Number}, _From, Call, #state{cnode = Fnode, dialstring = DialString, agent_pid = Apid} = State) ->
	?NOTICE("I'm supposed to dial ~p", [Number]),
	Self = self(),
	F = fun(RingUUID) ->
			fun(ok, _Reply) ->
					Client = Call#call.client,
					CalleridArgs = case proplists:get_value(<<"callerid">>, Client#client.options) of
						undefined ->
							["origination_privacy=hide_namehide_number"];
						CalleridNum ->
							["effective_caller_id_name="++Client#client.label, "effective_caller_id_name="++CalleridNum]
					end,
					freeswitch:sendmsg(Fnode, RingUUID,
						[{"call-command", "execute"},
							{"execute-app-name", "bridge"},
							{"execute-app-arg", freeswitch_media_manager:do_dial_string(DialString, Number, ["origination_uuid="++Call#call.id | CalleridArgs])}]),
					Self ! {connect_uuid, Number};
				(error, Reply) ->
					?WARNING("originate failed: ~p", [Reply]),
					ok
			end
	end,
	F2 = fun(_RingUUID, EventName, _Event) ->
			case EventName of
				"CHANNEL_BRIDGE" ->
					case cpx_supervisor:get_archive_path(Call) of
						none ->
							?DEBUG("archiving is not configured", []);
						{error, _Reason, Path} ->
							?WARNING("Unable to create requested call archiving directory for recording ~p", [Path]);
						Path ->
							?DEBUG("archiving to ~s.wav", [Path]),
							freeswitch:api(Fnode, uuid_record, Call#call.id ++ " start "++Path++".wav")
					end;
				_ ->
					ok
			end,
			true
	end,

	AgentRec = agent:dump_state(Apid),

	case freeswitch_ring:start(Fnode, AgentRec, Apid, Call, 600, F, [no_oncall_on_bridge, {eventfun, F2}]) of
		{ok, Pid} ->
			link(Pid),
			cdr:dialoutgoing(Call, Number),
			{reply, ok, State#state{ringchannel = Pid}};
		{error, Error} ->
			?ERROR("error:  ~p", [Error]),
			{stop, bad_call, {error, Error}, State}
	end;
handle_call(Msg, _From, _Call, State) ->
	?INFO("unhandled mesage ~p", [Msg]),
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
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
	?DEBUG("call", []),
	{noreply, State};
handle_info({call_event, {event, [UUID | Rest]}}, #call{id = UUID}, State) ->
	Event = freeswitch:get_event_name(Rest),
	case Event of
		"CHANNEL_HANGUP" ->
			Elem1 = case proplists:get_value("variable_hangup_cause", Rest) of
				"NO_ROUTE_DESTINATION" ->
					?ERROR("No route to destination for outbound call", []),
					noreply;
				"NORMAL_CLEARING" ->
					?INFO("Normal clearing", []),
					wrapup;
				"USER_BUSY" ->
					?WARNING("Agent's phone rejected the call", []),
					noreply;
				"NO_ANSWER" ->
					?NOTICE("Agent rangout on outbound call", []),
					noreply;
				Else ->
					?INFO("Hangup cause: ~p", [Else]),
					noreply
			end,
			{Elem1, State};
		_Else ->
			?DEBUG("call_event ~p", [Event]),
			{noreply, State}
	end;
handle_info(call_hangup, _Call, State) ->
	?DEBUG("Call hangup info", []),
	catch freeswitch_ring:hangup(State#state.ringchannel),
	{stop, normal, State};
handle_info({connect_uuid, Number}, #call{id = UUID} = Call, #state{cnode = Fnode, agent = Agent} = State) ->
	Gethandle = fun(Recusef, Count) ->
			?DEBUG("Counted ~p", [Count]),
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
handle_info(Info, _Call, State) ->
	?DEBUG("unhandled info ~p", [Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(Reason, _Call, _State) ->
	?NOTICE("FreeSWITCH outbound channel teminating ~p", [Reason]),
	ok.

%%--------------------------------------------------------------------
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, _Call, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
