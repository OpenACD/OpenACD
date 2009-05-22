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
	handle_ring_stop/1,
	handle_answer/3,
	handle_voicemail/1,
	handle_announce/2,
	handle_call/3, 
	handle_cast/2, 
	handle_info/2,
	terminate/2,
	code_change/3]).

-record(state, {
	callrec = undefined :: #call{} | 'undefined',
	cook :: pid() | 'undefined',
	queue :: string() | 'undefined',
	queue_pid :: pid() | 'undefined',
	cnode :: atom(),
	domain :: string(),
	agent :: string() | 'undefined',
	agent_pid :: pid() | 'undefined',
	ringchannel :: pid() | 'undefined'
	}).

-type(state() :: #state{}).
-define(GEN_MEDIA, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================
%% @doc starts the freeswitch media gen_server.  `Cnode' is the C node the communicates directly with freeswitch.
start(Cnode, Domain) ->
	gen_media:start(?MODULE, [Cnode, Domain]).

start_link(Cnode, Domain) ->
	gen_media:start_link(?MODULE, [Cnode, Domain]).

%% @doc returns the record of the call freeswitch media `MPid' is in charge of.
-spec(get_call/1 :: (MPid :: pid()) -> #call{}).
get_call(MPid) ->
	gen_media:get_call(MPid).

-spec(get_queue/1 :: (MPid :: pid()) -> pid()).
get_queue(MPid) ->
	gen_media:call(MPid, get_queue).

-spec(get_agent/1 :: (MPid :: pid()) -> pid()).
get_agent(MPid) ->
	gen_media:call(MPid, get_agent).

-spec(dump_state/1 :: (Mpid :: pid()) -> #state{}).
dump_state(Mpid) when is_pid(Mpid) ->
	gen_media:call(Mpid, dump_state).
	
%%====================================================================
%% gen_media callbacks
%%====================================================================
%% @private
init([Cnode, Domain]) ->
	{ok, {#state{cnode=Cnode, domain=Domain}, undefined}}.

handle_announce(Announcement, State) ->
	{ok, State}.

handle_answer(Apid, Callrec, State) ->
	{ok, State}.

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
			{ok, State#state{ringchannel = Pid}};
		{error, Error} ->
			?ERROR("error:  ~p", [Error]),
			{invalid, State}
	end.

handle_ring_stop(State) ->
	?DEBUG("hanging up ring channel", []),
	case State#state.ringchannel of
		undefined ->
			ok;
		RingChannel ->
			freeswitch_ring:hangup(RingChannel)
	end,
	{ok, State#state{ringchannel=undefined}}.

handle_voicemail(State) ->
	{invalid, State}.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
handle_call({transfer_agent, AgentPid, Timeout}, _From, #state{callrec = Call, agent_pid = Offererpid} = State) ->
	?INFO("transfer_agent to ~p for call ~p", [AgentPid, Call#call.id]),
	#agent{login = Recipient} = AgentRec = agent:dump_state(AgentPid),
	#agent{login = Offerer} = agent:dump_state(Offererpid),
	Ringout = Timeout div 1000,
	?DEBUG("ringout ~p", [Ringout]),
	cdr:agent_transfer(Call, {Offerer, Recipient}),
	case agent:set_state(AgentPid, ringing, Call) of
		ok ->
			% fun that returns another fun when passed the UUID of the new channel
			% (what fun!)
			F = fun(UUID) ->
				fun(ok, _Reply) ->
					% agent picked up?
					freeswitch:sendmsg(State#state.cnode, UUID,
						[{"call-command", "execute"}, {"execute-app-name", "intercept"}, {"execute-app-arg", Call#call.id}]);
				(error, Reply) ->
					?WARNING("originate failed: ~p", [Reply]),
					agent:set_state(AgentPid, idle)
				end
			end,
			case freeswitch_ring:start(State#state.cnode, AgentRec, AgentPid, Call, Ringout, F) of
				{ok, Pid} ->
					{reply, ok, State#state{agent_pid = AgentPid, ringchannel=Pid}};
				{error, Error} ->
					?ERROR("error:  ~p", [Error]),
					agent:set_state(AgentPid, released, "badring"),
					{reply, invalid, State}
			end;
		Else ->
			?INFO("Agent ringing response:  ~p", [Else]),
			{reply, invalid, State}
	end;
handle_call(get_call, _From, State) ->
	{reply, State#state.callrec, State};
handle_call(get_queue, _From, State) ->
	{reply, State#state.queue_pid, State};
handle_call(get_agent, _From, State) ->
	{reply, State#state.agent_pid, State};
%handle_call(unqueue, _From, #state{queue_pid = undefined} = State) ->
%	?WARNING("Cannot unqueue because there is no queue pid defined", []),
%	{reply, ok, State};
%handle_call(unqueue, _From, #state{queue_pid = Qpid, callrec = Callrec} = State) when is_pid(Qpid) ->
%	%% using a try in case the Qpid is dead.
%	try call_queue:remove(Qpid, Callrec#call.id) of
%		_Any ->
%			{reply, ok, State#state{queue_pid = undefined, queue = undefined}}
%	catch
%		_:_ ->
%			?WARNING("Cannot unqueue from ~p because it's dead.", [Qpid]),
%			{reply, ok, State#state{queue_pid = undefined, queue = undefined}}
%	end;
handle_call({set_agent, Agent, Apid}, _From, State) ->
	{reply, ok, State#state{agent = Agent, agent_pid = Apid}};
handle_call(dump_state, _From, State) ->
	{reply, State, State};
handle_call({announce, Announcement}, _From, #state{callrec = Callrec} = State) ->
	freeswitch:sendmsg(State#state.cnode, Callrec#call.id,
		[{"call-command", "execute"},
			{"execute-app-name", "playback"},
			{"execute-app-arg", Announcement}]),
	{reply, ok, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info({call, {event, [UUID | Rest]}}, State) ->
	?DEBUG("reporting new call ~p.", [UUID]),
	Callrec = #call{id = UUID, source = self()},
	cdr:cdrinit(Callrec),
	freeswitch_media_manager:notify(UUID, self()),
	State2 = State#state{callrec = Callrec},
	case_event_name([UUID | Rest], State2);
handle_info({call_event, {event, [UUID | Rest]}}, State) ->
	?DEBUG("reporting existing call progess ~p.", [UUID]),
	% TODO flesh out for all call events.
	case_event_name([ UUID | Rest], State);
handle_info({set_agent, Login, Apid}, State) ->
	{noreply, State#state{agent = Login, agent_pid = Apid}};
handle_info({bgok, Reply}, State) ->
	?DEBUG("bgok:  ~p", [Reply]),
	{noreply, State};
handle_info({bgerror, "-ERR NO_ANSWER\n"}, State) ->
	?INFO("Potential ringout.  Statecook:  ~p", [State#state.cook]),
	%% the apid is known by gen_media, let it handle if it is not not.
	{stop_ring, State};
handle_info({bgerror, "-ERR USER_BUSY\n"}, State) ->
	?NOTICE("Agent rejected the call", []),
	{stop_ring, State};
handle_info({bgerror, Reply}, State) ->
	?WARNING("unhandled bgerror: ~p", [Reply]),
	{noreply, State};
handle_info(call_hangup, State) ->
	?NOTICE("Call hangup info, terminating", []),
	{stop, normal, State};
handle_info(Info, State) ->
	?INFO("unhandled info ~p", [Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @private
terminate(Reason, _State) ->
	?NOTICE("terminating: ~p", [Reason]),
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @private
case_event_name([UUID | Rawcall], #state{callrec = Callrec} = State) ->
	Ename = freeswitch:get_event_name(Rawcall),
	?DEBUG("Event:  ~p;  UUID:  ~p", [Ename, UUID]),
	case Ename of
		"CHANNEL_PARK" ->
			case State#state.queue_pid of
				undefined ->
					Queue = freeswitch:get_event_header(Rawcall, "variable_queue"),
					Brand = freeswitch:get_event_header(Rawcall, "variable_brand"),
					case call_queue_config:get_client(Brand) of
						none ->
							Clientrec = #client{label="Unknown", tenant=0, brand=0};
						Clientrec ->
							ok
					end,
					Callerid = freeswitch:get_event_header(Rawcall, "Caller-Caller-ID-Name"),
					NewCall = Callrec#call{id=UUID, client=Clientrec, callerid=Callerid, source=self()},
					freeswitch:sendmsg(State#state.cnode, UUID,
						[{"call-command", "execute"},
							{"execute-app-name", "answer"}]),
					% play musique d'attente
					freeswitch:sendmsg(State#state.cnode, UUID,
						[{"call-command", "execute"},
							{"execute-app-name", "playback"},
							{"execute-app-arg", "local_stream://moh"}]),
						%% tell gen_media to (finally) queue the media
					{queue, Queue, Callrec, State#state{queue = Queue}};
				_Otherwise ->
					{noreply, State}
			end;
		"CHANNEL_HANGUP" ->
			?DEBUG("Channel hangup", []),
			Qpid = State#state.queue_pid,
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
						{ok, oncall} ->
							ok;
						{ok, released} ->
							ok
					end,
					State2 = State#state{agent = undefined, agent_pid = undefined, ringchannel = undefined}
			end,
			case Qpid of
				undefined ->
					?WARNING("Queue undefined", []),
					State3 = State2#state{agent = undefined, agent_pid = undefined};
				_Else ->
					call_queue:remove(Qpid, self()),
					State3 = State2#state{queue = undefined, queue_pid = undefined}
			end,
			{hangup, State3};
		"CHANNEL_DESTROY" ->
			?DEBUG("Last message this will recieve, channel destroy", []),
			{stop, normal, State};
		{error, notfound} ->
			?WARNING("event name not found: ~p", [freeswitch:get_event_header(Rawcall, "Content-Type")]),
			{noreply, State};
		Else ->
			?DEBUG("Event unhandled ~p", [Else]),
			{noreply, State}
	end.
