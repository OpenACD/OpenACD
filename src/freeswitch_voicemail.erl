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

-module(freeswitch_voicemail).

-behaviour(gen_media).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("queue.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("cpx_freeswitch_pb.hrl").

-define(TIMEOUT, 10000).

%% API
-export([
	start/6,
	start_link/6,
	get_call/1,
	dump_state/1
	]).

%% gen_media callbacks
-export([
	init/1, 
	handle_ring/3,
	handle_ring_stop/2,
	handle_answer/3,
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
	cook :: pid() | 'undefined',
	queue :: string() | 'undefined',
	queue_pid :: pid() | 'undefined',
	cnode :: atom(),
	agent :: string() | 'undefined',
	agent_pid :: pid() | 'undefined',
	ringchannel :: pid() | 'undefined',
	ringuuid :: string() | 'undefined',
	xferchannel :: pid() | 'undefined',
	xferuuid :: string() | 'undefined',
	manager_pid :: 'undefined' | any(),
	file = erlang:error({undefined, file}):: string(),
	answered = false :: boolean(),
	caseid :: string() | 'undefined',
	time = util:now() :: integer()
}).

-type(state() :: #state{}).
-define(GEN_MEDIA, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================
%% @doc starts the freeswitch media gen_server.  `Cnode' is the C node the communicates directly with freeswitch.
-spec(start/6 :: (Cnode :: atom(), UUID :: string(), File :: string(), Queue :: string(), Priority :: pos_integer(), Client :: #client{} | string()) -> {'ok', pid()}).
start(Cnode, UUID, File, Queue, Priority, Client) ->
	gen_media:start(?MODULE, [Cnode, UUID, File, Queue, Priority, Client]).

-spec(start_link/6 :: (Cnode :: atom(), UUID :: string(), File :: string(), Queue :: string(), Priority :: pos_integer(), Client :: #client{} | string()) -> {'ok', pid()}).
start_link(Cnode, UUID, File, Queue, Priority, Client) ->
	gen_media:start_link(?MODULE, [Cnode, UUID, File, Queue, Priority, Client]).

%% @doc returns the record of the call freeswitch media `MPid' is in charge of.
-spec(get_call/1 :: (MPid :: pid()) -> #call{}).
get_call(MPid) ->
	gen_media:get_call(MPid).

-spec(dump_state/1 :: (Mpid :: pid()) -> #state{}).
dump_state(Mpid) when is_pid(Mpid) ->
	gen_media:call(Mpid, dump_state).
	
%%====================================================================
%% gen_media callbacks
%%====================================================================
%% @private
init([Cnode, UUID, File, Queue, Priority, Client]) ->
	process_flag(trap_exit, true),
	Manager = whereis(freeswitch_media_manager),
	Callrec = #call{id=UUID++"-vm", type=voicemail, source=self(), priority = Priority, client = Client, media_path = inband},
	case cpx_supervisor:get_archive_path(Callrec) of
		none ->
			?DEBUG("archiving is not configured", []);
		{error, _Reason, Path} ->
			?WARNING("Unable to create requested call archiving directory for recording ~p", [Path]);
		Path ->
			Ext = filename:extension(File),
			?DEBUG("archiving to ~s~s", [Path, Ext]),
			file:copy(File, Path++Ext)
	end,
	{ok, {#state{cnode=Cnode, manager_pid = Manager, file=File}, {Queue, Callrec}}}.

handle_answer(Apid, Callrec, #state{file=File, xferchannel = XferChannel} = State) when is_pid(XferChannel) ->
	link(XferChannel),
	%freeswitch_ring:hangup(State#state.ringchannel),
	agent:conn_cast(Apid, {mediaload, Callrec, [{<<"width">>, <<"300px">>},{<<"height">>, <<"180px">>},{<<"title">>,<<>>}]}),
	?NOTICE("Voicemail ~s successfully transferred! Time to play ~s", [Callrec#call.id, File]),
	freeswitch:sendmsg(State#state.cnode, State#state.xferuuid,
		[{"call-command", "execute"},
			{"event-lock", "true"},
			{"execute-app-name", "phrase"},
			{"execute-app-arg", "voicemail_say_date,"++integer_to_list(State#state.time)}]),
	freeswitch:sendmsg(State#state.cnode, State#state.xferuuid,
		[{"call-command", "execute"},
			{"event-lock", "true"},
			{"execute-app-name", "playback"},
			{"execute-app-arg", File}]),
	{ok, State#state{agent_pid = Apid, ringchannel = State#state.xferchannel,
			ringuuid = State#state.xferuuid, xferuuid = undefined, xferchannel = undefined, answered = true}};
handle_answer(Apid, Callrec, #state{file=File} = State) ->
	?NOTICE("Voicemail ~s successfully answered! Time to play ~s", [Callrec#call.id, File]),
	agent:conn_cast(Apid, {mediaload, Callrec, [{<<"width">>, <<"300px">>},{<<"height">>, <<"180px">>},{<<"title">>,<<>>}]}),
	freeswitch:sendmsg(State#state.cnode, State#state.ringuuid,
		[{"call-command", "execute"},
			{"event-lock", "true"},
			{"execute-app-name", "phrase"},
			{"execute-app-arg", "voicemail_say_date,"++integer_to_list(State#state.time)}]),
	freeswitch:sendmsg(State#state.cnode, State#state.ringuuid,
		[{"call-command", "execute"},
			{"event-lock", "true"},
			{"execute-app-name", "playback"},
			{"execute-app-arg", File}]),
	{ok, State#state{agent_pid = Apid, answered = true}}.

handle_ring(Apid, Callrec, State) when is_pid(Apid) ->
	AgentRec = agent:dump_state(Apid),
	handle_ring({Apid, AgentRec}, Callrec, State);
handle_ring({_Apid, #agent{endpointtype = {undefined, persistent, _}} = Agent}, _Callrec, State) ->
	?WARNING("Agent (~p) does not have it's persistent channel up yet", [Agent#agent.login]),
	{invalid, State};

handle_ring({Apid, #agent{endpointtype = {EndpointPid, persistent, _EndpointType}} = Agent}, Callrec, State) ->
	?INFO("Ring channel made things happy, I assume", []),
	{ok, [{"caseid", State#state.caseid}], State#state{ringchannel = EndpointPid, ringuuid = freeswitch_ring:get_uuid(EndpointPid), agent_pid = Apid}};

handle_ring({Apid, #agent{endpointtype = {EndpointPid, transient, _EndpintType}} = Agent}, Callrec, State) when is_pid(EndpointPid) ->
	?INFO("Agent already has transient ring pid up:  ~p", [EndpointPid]),
	{ok, [{"caseid", State#state.caseid}], State#state{ringchannel = EndpointPid, ringuuid = freeswitch_ring:get_uuid(EndpointPid), agent_pid = Apid}};

%% ===== old stuff ======
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
	F2 = fun(UUID, EventName, Event) ->
			case EventName of
				"DTMF" ->
					case proplists:get_value("DTMF-Digit", Event) of
						"5" ->
							freeswitch:sendmsg(State#state.cnode, UUID,
								[{"call-command", "execute"},
									{"event-lock", "true"},
									{"execute-app-name", "playback"},
									{"execute-app-arg", State#state.file}]);
						_ ->
							ok
					end;
				"CHANNEL_EXECUTE_COMPLETE" ->
					File = State#state.file,
					case proplists:get_value("Application-Data", Event) of
						File ->
							?NOTICE("Finished playing voicemail recording", []),
							freeswitch:sendmsg(State#state.cnode, UUID,
								[{"call-command", "execute"},
									{"event-lock", "true"},
									{"execute-app-name", "playback"},
									{"execute-app-arg", "file_string://voicemail/8000/vm-press.wav!digits/8000/5.wav!voicemail/8000/vm-repeat_message.wav"}]);
						_ ->
							ok
					end;
				_ ->
					ok
			end,
			true
	end,
	AgentRec = agent:dump_state(Apid),
	case freeswitch_ring:start(State#state.cnode, AgentRec, Apid, Callrec, ?getRingout, F, [single_leg, {eventfun, F2}, {needed_events, ['DTMF', 'CHANNEL_EXECUTE_COMPLETE']}]) of
		{ok, Pid} ->
			link(Pid),
			{ok, [{"caseid", State#state.caseid}], State#state{ringchannel = Pid, ringuuid = freeswitch_ring:get_uuid(Pid), agent_pid = Apid}};
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

handle_agent_transfer(AgentPid, Timeout, Call, State) ->
	?INFO("transfer_agent to ~p for call ~p", [AgentPid, Call#call.id]),
	AgentRec = agent:dump_state(AgentPid),
	% fun that returns another fun when passed the UUID of the new channel
	% (what fun!)
	F = fun(_UUID) ->
		fun(ok, _Reply) ->
			% agent picked up?
			ok;
		(error, Reply) ->
			?WARNING("originate failed: ~p", [Reply])
			%agent:set_state(AgentPid, idle)
		end
	end,

	F2 = fun(UUID, EventName, Event) ->
			case EventName of
				"DTMF" ->
					case proplists:get_value("DTMF-Digit", Event) of
						"5" ->
							freeswitch:sendmsg(State#state.cnode, UUID,
								[{"call-command", "execute"},
									{"execute-app-name", "playback"},
									{"execute-app-arg", State#state.file}]);
						_ ->
							ok
					end;
				"CHANNEL_EXECUTE_COMPLETE" ->
					File = State#state.file,
					case proplists:get_value("Application-Data", Event) of
						File ->
							?NOTICE("Finished playing voicemail recording", []);
						_ ->
							ok
					end;
				_ ->
					ok
			end,
			true
	end,
	case freeswitch_ring:start(State#state.cnode, AgentRec, AgentPid, Call, Timeout, F, [single_leg, {eventfun, F2}, {needed_events, ['DTMF', 'CHANNEL_EXECUTE_COMPLETE']}]) of
		{ok, Pid} ->
			{ok, [{"caseid", State#state.caseid}], State#state{agent_pid = AgentPid, xferchannel = Pid, xferuuid = freeswitch_ring:get_uuid(Pid)}};
		{error, Error} ->
			?ERROR("error:  ~p", [Error]),
			{error, Error, State}
	end.

-spec(handle_warm_transfer_begin/3 :: (Number :: string(), Call :: #call{}, State :: any()) -> {'invalid', #state{}}).
handle_warm_transfer_begin(_Number, _Call, State) ->
	{invalid, State}.

handle_wrapup(_Call, State) ->
	% This intentionally left blank; media is out of band, so there's
	% no direct hangup by the agent
	{ok, State}.
	
handle_queue_transfer(_Call, #state{ringchannel = Channel} = State) when is_pid(Channel) ->
	freeswitch_ring:hangup(Channel),
	{ok, State#state{ringchannel = undefined, xferchannel=undefined, xferuuid=undefined, answered = false}};
handle_queue_transfer(_Call, State) ->
	{ok, State#state{answered = false}}.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
handle_call(get_call, _From, Call, State) ->
	{reply, Call, State};
handle_call(get_queue, _From, _Call, State) ->
	{reply, State#state.queue_pid, State};
handle_call(get_agent, _From, _Call, State) ->
	{reply, State#state.agent_pid, State};
handle_call({set_agent, Agent, Apid}, _From, _Call, State) ->
	{reply, ok, State#state{agent = Agent, agent_pid = Apid}};
handle_call(dump_state, _From, _Call, State) ->
	{reply, State, State};
handle_call(Msg, _From, _Call, State) ->
	?INFO("unhandled mesage ~p", [Msg]),
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast({set_caseid, CaseID}, _Call, State) ->
	{noreply, State#state{caseid = CaseID}};

handle_cast(replay, _Call, #state{file = File} = State) ->
	freeswitch:sendmsg(State#state.cnode, State#state.ringuuid,
		[{"call-command", "execute"},
			{"event-lock", "true"},
			{"execute-app-name", "playback"},
			{"execute-app-arg", File}]),
	{noreply, State};

%% web api
handle_cast({<<"replay">>, _}, Call, State) ->
	handle_cast(replay, Call, State);

%% tcp api
handle_cast(Request, Call, State) when is_record(Request, mediacommandrequest) ->
	FixedRequest = cpx_freeswitch_pb:decode_extensions(Request),
	Hint = case cpx_freeswitch_pb:get_extension(FixedRequest, freeswitch_voicemail_request_hint) of
		{ok, O} -> O;
		_ -> undefined
	end,
	case Hint of
		'REPLAY' ->
			handle_cast(replay, Call, State);
		Else ->
			?INFO("Invalid request hint:  ~p", [Else]),
			{noreply, State}
	end;

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
			gen_server:cast(freeswitch_media_manager, {notify, Call#call.id, self()}),
			{noreply, State#state{manager_pid = Pid}};
		_Else ->
			{ok, Tref} = timer:send_after(1000, check_recovery),
			{noreply, State#state{manager_pid = Tref}}
	end;
handle_info({'EXIT', Pid, Reason}, _Call, #state{xferchannel = Pid} = State) ->
	?WARNING("Handling transfer channel ~w exit ~p", [Pid, Reason]),
	{stop_ring, State#state{ringchannel = undefined}};
handle_info({'EXIT', Pid, Reason}, _Call, #state{ringchannel = Pid, answered = true, xferchannel = undefined} = State) ->
	?WARNING("Handling ring channel ~w exit ~p after answered, no transfer", [Pid, Reason]),
	{stop, {hangup, "agent"}, State};
handle_info({'EXIT', Pid, Reason}, _Call, #state{ringchannel = Pid} = State) ->
	?WARNING("Handling ring channel ~w exit ~p", [Pid, Reason]),
	{stop_ring, State#state{ringchannel = undefined}};
handle_info({'EXIT', Pid, Reason}, _Call, #state{manager_pid = Pid} = State) ->
	?WARNING("Handling manager exit from ~w due to ~p", [Pid, Reason]),
	{ok, Tref} = timer:send_after(1000, check_recovery),
	{noreply, State#state{manager_pid = Tref}};
handle_info(call_hangup, _Call, State) ->
	catch freeswitch_ring:hangup(State#state.ringchannel),
	{stop, normal, State};
handle_info(Info, _Call, State) ->
	?INFO("unhandled info ~p", [Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @private
terminate(Reason, _Call, _State) ->
	% TODO - delete the recording or archive it somehow
	?NOTICE("terminating: ~p", [Reason]),
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
%% @private
code_change(_OldVsn, _Call, State, _Extra) ->
	{ok, State}.

