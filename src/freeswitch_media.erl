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
%%	The Original Code is Spice Telephony.
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
%%	Andrew Thompson <athompson at spicecsm dot com>
%%	Micah Warren <mwarren at spicecsm dot com>
%%

%% @doc An on demand gen_server for watching a freeswitch call.
%% This is started by freeswitch_media_manager when a new call id is found.
%% This is responsible for:
%% <ul>
%% <li>Connecting an agent to a call</li>
%% <li>Moving a call into queue.</li>
%% <li>Removing a call from queue.</li>
%% <li>Signalling when a call has hung up.</li>
%% </ul>
%% @see freeswitch_media_manager

-module(freeswitch_media).
-author("Micah").

-behaviour(gen_media).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("queue.hrl").
-include("call.hrl").
-include("agent.hrl").

-define(TIMEOUT, 10000).

-define(DEFAULT_PRIORITY, 10).

% TODO hangup occurs in reverse order.  eg:  if the agent hangs up, it looks
% like the the caller did, and vice versa.

%% API
-export([
	start/2,
	start_link/2,
	get_call/1,
	%get_queue/1,
	%get_agent/1,
	%unqueue/1,
	%set_agent/3,
	dump_state/1
	]).

%% gen_media callbacks
-export([
	init/1, 
	handle_ring/3,
	handle_ring_stop/2,
	handle_answer/3,
	handle_voicemail/3,
	handle_spy/3,
	handle_announce/3,
	handle_agent_transfer/4,
	handle_queue_transfer/2,
	handle_wrapup/2,
	handle_call/4, 
	handle_cast/3, 
	handle_info/3,
	handle_warm_transfer_begin/3,
	terminate/3,
	code_change/4]).

-record(state, {
	%callrec = undefined :: #call{} | 'undefined',
	cook :: pid() | 'undefined',
	queue :: string() | 'undefined',
	cnode :: atom(),
	agent :: string() | 'undefined',
	agent_pid :: pid() | 'undefined',
	ringchannel :: pid() | 'undefined',
	manager_pid :: 'undefined' | any(),
	voicemail = false :: 'false' | string(),
	xferchannel :: pid() | 'undefined',
	xferuuid :: string() | 'undefined',
	in_control = false :: bool(),
	queued = false :: bool()
	}).

-type(state() :: #state{}).
-define(GEN_MEDIA, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================
%% @doc starts the freeswitch media gen_server.  `Cnode' is the C node the communicates directly with freeswitch.
%-spec(start/1 :: (Cnode :: atom()) -> {'ok', pid()}).
%start(Cnode) ->
	%gen_media:start(?MODULE, [Cnode]).

%-spec(start_link/1 :: (Cnode :: atom()) -> {'ok', pid()}).
%start_link(Cnode) ->
	%gen_media:start_link(?MODULE, [Cnode]).

-spec(start/2 :: (Cnode :: atom(), UUID :: string()) -> {'ok', pid()}).
start(Cnode, UUID) ->
	gen_media:start(?MODULE, [Cnode, UUID]).

-spec(start_link/2 :: (Cnode :: atom(), UUID :: string()) -> {'ok', pid()}).
start_link(Cnode, UUID) ->
	gen_media:start_link(?MODULE, [Cnode, UUID]).

%% @doc returns the record of the call freeswitch media `MPid' is in charge of.
-spec(get_call/1 :: (MPid :: pid()) -> #call{}).
get_call(MPid) ->
	gen_media:get_call(MPid).

%-spec(get_queue/1 :: (MPid :: pid()) -> pid()).
%get_queue(MPid) ->
%	gen_media:call(MPid, get_queue).
%
%-spec(get_agent/1 :: (MPid :: pid()) -> pid()).
%get_agent(MPid) ->
%	gen_media:call(MPid, get_agent).

-spec(dump_state/1 :: (Mpid :: pid()) -> #state{}).
dump_state(Mpid) when is_pid(Mpid) ->
	gen_media:call(Mpid, dump_state).
	
%%====================================================================
%% gen_media callbacks
%%====================================================================
%% @private
init([Cnode, UUID]) ->
	process_flag(trap_exit, true),
	Manager = whereis(freeswitch_media_manager),
	{DNIS, Client, Priority, CidName, CidNum} = get_info(Cnode, UUID),
	Call = #call{id = UUID, source = self(), client = Client, priority = Priority, callerid={CidName, CidNum}},
	{ok, {#state{cnode=Cnode, manager_pid = Manager}, Call, {inivr, [DNIS]}}}.

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
	%freeswitch_ring:hangup(State#state.ringchannel),
	{ok, State#state{agent_pid = Apid, ringchannel = XferChannel,
			xferchannel = undefined, xferuuid = undefined}};
handle_answer(Apid, Callrec, State) ->
	case cpx_supervisor:get_archive_path(Callrec) of
		none ->
			?DEBUG("archiving is not configured", []);
		{error, Reason, Path} ->
			?WARNING("Unable to create requested call archiving directory for recording ~p", [Path]);
		Path ->
			% TODO - if Freeswitch can't create this file, the call gets aborted!
			?DEBUG("archiving to ~s.wav", [Path]),
			freeswitch:api(State#state.cnode, uuid_record, Callrec#call.id ++ " start "++Path++".wav")
	end,
	{ok, State#state{agent_pid = Apid}}.

handle_ring(Apid, Callrec, State) ->
	?INFO("ring to agent ~p for call ~s", [Apid, Callrec#call.id]),
	F = fun(UUID) ->
		fun(ok, _Reply) ->
			freeswitch:api(State#state.cnode, uuid_bridge, UUID ++ " " ++ Callrec#call.id);
		(error, Reply) ->
			?WARNING("originate failed: ~p", [Reply]),
			ok
		end
	end,
	AgentRec = agent:dump_state(Apid),
	case freeswitch_ring:start(State#state.cnode, AgentRec, Apid, Callrec, 600, F) of
		{ok, Pid} ->
			link(Pid),
			{ok, State#state{ringchannel = Pid, agent_pid = Apid}};
		{error, Error} ->
			?ERROR("error:  ~p", [Error]),
			{invalid, State}
	end.

handle_ring_stop(_Callrec, State) ->
	?DEBUG("hanging up ring channel", []),
	case State#state.ringchannel of
		undefined ->
			ok;
		RingChannel ->
			freeswitch_ring:hangup(RingChannel)
	end,
	{ok, State#state{ringchannel=undefined}}.

handle_voicemail(Agent, Callrec, State) when is_pid(Agent) ->
	{ok, Midstate} = handle_ring_stop(Callrec, State),
	handle_voicemail(undefined, Callrec, Midstate);
handle_voicemail(undefined, Call, State) ->
	UUID = Call#call.id,
	freeswitch:bgapi(State#state.cnode, uuid_transfer, UUID ++ " 'playback:voicemail/vm-record_message.wav,record:/tmp/${uuid}.wav' inline"),
	{ok, State#state{voicemail = "/tmp/"++UUID++".wav"}}.

handle_spy(Agent, Call, #state{cnode = Fnode, ringchannel = Chan} = State) when is_pid(Chan) ->
	case agent_manager:find_by_pid(Agent) of
		AgentName ->
			agent:blab(Agent, "While spying, you have the following options:\n"++
				"* To whisper to the agent; press 1\n"++
				"* To whisper to the caller; press 2\n"++
				"* To talk to both parties; press 3\n"++
				"* To resume spying; press 0"),
			freeswitch:bgapi(Fnode, originate, "user/" ++ re:replace(AgentName, "@", "_", [{return, list}]) ++ " &eavesdrop(" ++ Call#call.id ++ ")"),
			{ok, State};
		notfound ->
			{error, bad_agent, State}
	end;
handle_spy(_Agent, _Call, State) ->
	{invalid, State}.

handle_agent_transfer(AgentPid, Timeout, Call, State) ->
	?INFO("transfer_agent to ~p for call ~p", [AgentPid, Call#call.id]),
	AgentRec = agent:dump_state(AgentPid),
	%#agent{login = Offerer} = agent:dump_state(Offererpid),
	%Ringout = Timeout div 1000,
	%?DEBUG("ringout ~p", [Ringout]),
	%cdr:agent_transfer(Call, {Offerer, Recipient}),
	% fun that returns another fun when passed the UUID of the new channel
	% (what fun!)
	F = fun(UUID) ->
		fun(ok, _Reply) ->
			% agent picked up?
			%unlink(State#state.ringchannel),
			%freeswitch:sendmsg(State#state.cnode, UUID,
				%[{"call-command", "execute"}, {"execute-app-name", "intercept"}, {"execute-app-arg", Call#call.id}]),
				%gen_media:oncall(Call#call.source),
				?INFO("Agent transfer picked up?~n", []);
		(error, Reply) ->
			?WARNING("originate failed: ~p", [Reply])
			%agent:set_state(AgentPid, idle)
		end
	end,
	case freeswitch_ring:start_link(State#state.cnode, AgentRec, AgentPid, Call, Timeout, F, [single_leg, no_oncall_on_bridge]) of
		{ok, Pid} ->
			%{ok, State#state{agent_pid = AgentPid, ringchannel=Pid}};
			{ok, State#state{xferchannel = Pid, xferuuid = freeswitch_ring:get_uuid(Pid)}};
		{error, Error} ->
			?ERROR("error:  ~p", [Error]),
			{error, Error, State}
	end.

handle_warm_transfer_begin(Number, Call, #state{agent_pid = AgentPid, cnode = Node} = State) when is_pid(AgentPid) ->
	case freeswitch:api(Node, uuid_transfer, lists:flatten(io_lib:format("~s -both 'conference:~s+flags{mintwo}' inline", [Call#call.id, Call#call.id]))) of
		{error, Error} ->
			?WARNING("transferring into a conference failed: ~s", [Error]),
			{error, Error, State};
		{ok, _Whatever} ->
			% okay, now figure out the member IDs
			NF = io_lib:format("Conference ~s not found", [Call#call.id]),
			timer:sleep(100),
			case freeswitch:api(Node, conference, lists:flatten(io_lib:format("~s list", [Call#call.id]))) of
				{ok, NF} ->
					% TODO uh-oh!
					?WARNING("newly created conference not found", []),
					{ok, State};
				{ok, Output} ->
					Members = lists:map(fun(Y) -> util:string_split(Y, ";") end, util:string_split(Output, "\n")),
					?NOTICE("members ~p", [Members]),
					[[Id | _Rest]] = lists:filter(fun(X) -> lists:nth(3, X) =:= Call#call.id end, Members),
					freeswitch:api(Node, conference, Call#call.id ++ " play local_stream://moh " ++ Id),
					freeswitch:api(Node, conference, Call#call.id ++ " mute " ++ Id),
					?NOTICE("Muting ~s in conference", [Id]),
					case freeswitch:api(Node, create_uuid) of
						{ok, UUID} ->
							DialResult = freeswitch:api(Node, conference, lists:flatten(io_lib:format("~s dial {origination_uuid=~s,originate_timeout=30}sofia/gateway/cpxvgw.fusedsolutions.com/~s 1234567890 FreeSWITCH_Conference", [Call#call.id, UUID, Number]))),
							?NOTICE("warmxfer dial result: ~p, UUID requested: ~s", [DialResult, UUID]),
							{ok, UUID, State};
						_ ->
							{error, "allocating UUID failed", State}
					end
			end
	end;
handle_warm_transfer_begin(_Number, _Call, #state{agent_pid = AgentPid} = State) ->
	?WARNING("wtf?! agent pid is ~p", [AgentPid]),
	{error, "error: no agent bridged to this call~n", State}.


handle_wrapup(_Call, State) ->
	% This intentionally left blank; media is out of band, so there's
	% no direct hangup by the agent
	{ok, State}.
	
handle_queue_transfer(Call, #state{cnode = Fnode} = State) ->
	freeswitch:api(Fnode, uuid_park, Call#call.id),
	% play musique d'attente (actually, don't because it's already running from before)
	%freeswitch:sendmsg(Fnode, Call#call.id,
		%[{"call-command", "execute"},
			%{"execute-app-name", "playback"},
			%{"execute-app-arg", "local_stream://moh"}]),
	{ok, State}.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
%handle_call({transfer_agent, AgentPid, Timeout}, _From, #state{callrec = Call, agent_pid = Offererpid} = State) ->
%	?INFO("transfer_agent to ~p for call ~p", [AgentPid, Call#call.id]),
%	#agent{login = Recipient} = AgentRec = agent:dump_state(AgentPid),
%	#agent{login = Offerer} = agent:dump_state(Offererpid),
%	Ringout = Timeout div 1000,
%	?DEBUG("ringout ~p", [Ringout]),
%	cdr:agent_transfer(Call, {Offerer, Recipient}),
%	case agent:set_state(AgentPid, ringing, Call) of
%		ok ->
%			% fun that returns another fun when passed the UUID of the new channel
%			% (what fun!)
%			F = fun(UUID) ->
%				fun(ok, _Reply) ->
%					% agent picked up?
%					freeswitch:sendmsg(State#state.cnode, UUID,
%						[{"call-command", "execute"}, {"execute-app-name", "intercept"}, {"execute-app-arg", Call#call.id}]);
%				(error, Reply) ->
%					?WARNING("originate failed: ~p", [Reply]),
%					agent:set_state(AgentPid, idle)
%				end
%			end,
%			case freeswitch_ring:start(State#state.cnode, AgentRec, AgentPid, Call, Ringout, F) of
%				{ok, Pid} ->
%					{reply, ok, State#state{agent_pid = AgentPid, ringchannel=Pid}};
%				{error, Error} ->
%					?ERROR("error:  ~p", [Error]),
%					agent:set_state(AgentPid, released, "badring"),
%					{reply, invalid, State}
%			end;
%		Else ->
%			?INFO("Agent ringing response:  ~p", [Else]),
%			{reply, invalid, State}
%	end;
handle_call(get_call, _From, Call, State) ->
	{reply, Call, State};
handle_call(get_agent, _From, _Call, State) ->
	{reply, State#state.agent_pid, State};
handle_call({set_agent, Agent, Apid}, _From, _Call, State) ->
	{reply, ok, State#state{agent = Agent, agent_pid = Apid}};
handle_call(dump_state, _From, _Call, State) ->
	{reply, State, State};
handle_call(_Request, _From, _Call, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast(_Msg, _Call, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info(check_recovery, Call, State) ->
	case whereis(freeswitch_media_manager) of
		Pid when is_pid(Pid) ->
			link(Pid),
			%Call = State#state.callrec,
			gen_server:cast(freeswitch_media_manager, {notify, Call#call.id, self()}),
			{noreply, State#state{manager_pid = Pid}};
		_Else ->
			{ok, Tref} = timer:send_after(1000, check_recovery),
			{noreply, State#state{manager_pid = Tref}}
	end;
handle_info({'EXIT', Pid, Reason}, _Call, #state{xferchannel = Pid} = State) ->
	?WARNING("Handling transfer channel ~w exit ~p", [Pid, Reason]),
	{stop_ring, State#state{ringchannel = undefined}};
handle_info({'EXIT', Pid, Reason}, _Call, #state{ringchannel = Pid} = State) ->
	?WARNING("Handling ring channel ~w exit ~p", [Pid, Reason]),
	{stop_ring, State#state{ringchannel = undefined}};
handle_info({'EXIT', Pid, Reason}, _Call, #state{manager_pid = Pid} = State) ->
	?WARNING("Handling manager exit from ~w due to ~p", [Pid, Reason]),
	{ok, Tref} = timer:send_after(1000, check_recovery),
	{noreply, State#state{manager_pid = Tref}};
handle_info({call, {event, [UUID | Rest]}}, Call, State) when is_list(UUID) ->
	?DEBUG("reporting new call ~p.", [UUID]),
	%Callrec = #call{id = UUID, source = self()},
	%cdr:cdrinit(Callrec),
	freeswitch_media_manager:notify(UUID, self()),
	%State2 = State#state{callrec = Callrec},
	case_event_name([UUID | Rest], Call, State#state{in_control = true});
handle_info({call_event, {event, [UUID | Rest]}}, Call, State) when is_list(UUID) ->
	?DEBUG("reporting existing call progess ~p.", [UUID]),
	% TODO flesh out for all call events.
	case_event_name([ UUID | Rest], Call, State);
handle_info({set_agent, Login, Apid}, _Call, State) ->
	{noreply, State#state{agent = Login, agent_pid = Apid}};
handle_info({bgok, Reply}, _Call, State) ->
	?DEBUG("bgok:  ~p", [Reply]),
	{noreply, State};
handle_info({bgerror, "-ERR NO_ANSWER\n"}, _Call, State) ->
	?INFO("Potential ringout.  Statecook:  ~p", [State#state.cook]),
	%% the apid is known by gen_media, let it handle if it is not not.
	{stop_ring, State};
handle_info({bgerror, "-ERR USER_BUSY\n"}, _Call, State) ->
	?NOTICE("Agent rejected the call", []),
	{stop_ring, State};
handle_info({bgerror, Reply}, _Call, State) ->
	?WARNING("unhandled bgerror: ~p", [Reply]),
	{noreply, State};
handle_info(channel_destroy, _Call, #state{in_control = InControl} = State) when not InControl ->
	?NOTICE("Hangup in IVR", []),
	{hangup, State};
handle_info(call_hangup, _Call, State) ->
	?NOTICE("Call hangup info, terminating", []),
	{stop, normal, State};
handle_info(Info, _Call, State) ->
	?INFO("unhandled info ~p", [Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @private
terminate(Reason, _Call, State) ->
	?NOTICE("terminating: ~p", [Reason]),
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
case_event_name([UUID | Rawcall], Callrec, State) ->
	Ename = freeswitch:get_event_name(Rawcall),
	?DEBUG("Event:  ~p;  UUID:  ~p", [Ename, UUID]),
	case Ename of
		"CHANNEL_PARK" ->
			case State#state.queued of
				false ->
					Queue = proplists:get_value("variable_queue", Rawcall, "default_queue"),
					Client = proplists:get_value("variable_brand", Rawcall),
					P = proplists:get_value("variable_queue_priority", Rawcall, integer_to_list(?DEFAULT_PRIORITY)),
					Priority = try list_to_integer(P) of
						Pri -> Pri
					catch
						error:badarg -> ?DEFAULT_PRIORITY
					end,
					?WARNING("proplist: ~p", [Rawcall]),
					Calleridname = proplists:get_value("Caller-Caller-ID-Name", Rawcall, "Unknown"),
					Calleridnum = proplists:get_value("Caller-Caller-ID-Number", Rawcall, "Unknown"),
					NewCall = Callrec#call{client=Client, callerid={Calleridname, Calleridnum}, priority = Priority},
					freeswitch:sendmsg(State#state.cnode, UUID,
						[{"call-command", "execute"},
							{"execute-app-name", "answer"}]),
					% play musique d'attente
					freeswitch:sendmsg(State#state.cnode, UUID,
						[{"call-command", "execute"},
							{"execute-app-name", "playback"},
							{"execute-app-arg", "local_stream://moh"}]),
						%% tell gen_media to (finally) queue the media
					{queue, Queue, NewCall, State#state{queue = Queue, queued=true}};
				_Otherwise ->
					{noreply, State}
			end;
		"CHANNEL_HANGUP" ->
			?DEBUG("Channel hangup", []),
			Apid = State#state.agent_pid,
			case Apid of
				undefined ->
					?WARNING("Agent undefined", []),
					State2 = State#state{agent = undefined, agent_pid = undefined};
				_Other ->
					case agent:query_state(Apid) of
						{ok, ringing} ->
							?NOTICE("caller hung up while we were ringing an agent", []),
							case State#state.ringchannel of
								undefined ->
									ok;
								RingChannel ->
									freeswitch_ring:hangup(RingChannel)
							end;
						_Whatever ->
							ok
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
							freeswitch_media_manager:new_voicemail(UUID, FileName, State#state.queue, Callrec#call.priority + 10);
						false ->
							?NOTICE("~s hungup without leaving a voicemail", [UUID])
					end
			end,
			{hangup, State2};
		"CHANNEL_DESTROY" ->
			?DEBUG("Last message this will recieve, channel destroy", []),
			{stop, normal, State};
		"DTMF" ->
			case proplists:get_value("DTMF-Digit", Rawcall) of
				"*" ->
					% allow the media to go to voicemail
					?NOTICE("caller requested to go to voicemail", []),
					freeswitch:bgapi(State#state.cnode, uuid_transfer, UUID ++ " 'playback:voicemail/vm-record_message.wav,record:/tmp/${uuid}.wav' inline"),
					{voicemail, State#state{voicemail = "/tmp/"++UUID++".wav"}};
				_ ->
					{noreply, State}
			end;
		{error, notfound} ->
			?WARNING("event name not found: ~p", [freeswitch:get_event_header(Rawcall, "Content-Type")]),
			{noreply, State};
		Else ->
			?DEBUG("Event unhandled ~p", [Else]),
			{noreply, State}
	end.

%get_info(Cnode, UUID) ->
	%{ok, Result} = freeswitch:api(Cnode, uuid_getvar_multi, UUID++" destination_number;brand;queue_priority"),
	%[DNIS, Brand, P] = re:split(Result, ";", [{return, list}]),
	%Priority = try list_to_integer(P) of
		%Pri ->
			%Pri
	%catch
		%error:badarg ->
			%?DEFAULT_PRIORITY
	%end,
	%{DNIS, Brand, Priority}.
	
get_info(Cnode, UUID) ->
	{ok, Result} = freeswitch:api(Cnode, uuid_dump, UUID),
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

	{proplists:get_value("Caller-Destination-Number", Proplist, ""),
		proplists:get_value("variable_brand", Proplist, ""), Priority,
		proplists:get_value("Caller-Caller-ID-Name", Proplist, "Unknown"),
		proplists:get_value("Caller-Caller-ID-Number", Proplist, "Unknown")
	}.
