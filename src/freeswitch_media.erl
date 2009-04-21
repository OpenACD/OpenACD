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

-behaviour(gen_server).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("queue.hrl").
-include("call.hrl").
-include("agent.hrl").

-define(TIMEOUT, 10000).


%% API
-export([
	start/2,
	start_link/2,
	get_call/1,
	get_queue/1,
	get_agent/1,
	unqueue/1,
	set_agent/3,
	dump_state/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	callrec = undefined,
	cook,
	queue,
	queue_pid,
	cnode,
	domain,
	agent,
	agent_pid,
	ringchannel
	}).

%%====================================================================
%% API
%%====================================================================
%% @doc starts the freeswitch media gen_server.  `Cnode' is the C node the communicates directly with freeswitch.
start(Cnode, Domain) ->
	gen_server:start(?MODULE, [Cnode, Domain], []).

start_link(Cnode, Domain) ->
	gen_server:start_link(?MODULE, [Cnode, Domain], []).

%% @doc returns the record of the call freeswitch media `MPid' is in charge of.
-spec(get_call/1 :: (MPid :: pid()) -> #call{}).
get_call(MPid) ->
	gen_server:call(MPid, get_call).

-spec(get_queue/1 :: (MPid :: pid()) -> pid()).
get_queue(MPid) ->
	gen_server:call(MPid, get_queue).

-spec(get_agent/1 :: (MPid :: pid()) -> pid()).
get_agent(MPid) ->
	gen_server:call(MPid, get_agent).

-spec(unqueue/1 :: (Mpid :: pid()) -> 'ok').
unqueue(MPid) ->
	gen_server:call(MPid, unqueue).

-spec(set_agent/3 :: (Mpid :: pid(), Agent :: any(), Apid :: pid()) -> 'ok').
set_agent(MPid, Agent, Apid) when is_pid(MPid), is_pid(Apid) ->
	gen_server:call(MPid, {set_agent, Agent, Apid}).

-spec(dump_state/1 :: (Mpid :: pid()) -> #state{}).
dump_state(Mpid) when is_pid(Mpid) ->
	gen_server:call(Mpid, dump_state).
	
%%====================================================================
%% gen_server callbacks
%%====================================================================
%% @private
init([Cnode, Domain]) ->
	{ok, #state{cnode=Cnode, domain=Domain}}.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
% TODO If this module's state has the call, why does this need the call passed in?  (part of the spec for a generic media_manager, change spec?)
handle_call({ring_agent, AgentPid, QCall, Timeout}, _From, #state{callrec = Call} = State) ->
	?INFO("ring_agent to ~p for call ~p with cook ~p", [AgentPid, QCall#queued_call.id, QCall#queued_call.cook]),
	AgentRec = agent:dump_state(AgentPid),
	Ringout = Timeout div 1000,
	?DEBUG("ringout ~p", [Ringout]),
	case agent:set_state(AgentPid, ringing, Call#call{cook=QCall#queued_call.cook}) of
		ok ->
			case freeswitch_ring:start(State#state.cnode, AgentRec, AgentPid, Call#call{cook=QCall#queued_call.cook}, Ringout, State#state.domain) of
				{ok, Pid} ->
					{reply, ok, State#state{agent_pid = AgentPid, cook=QCall#queued_call.cook, ringchannel=Pid}};
				{error, Error} ->
					?ERROR("error:  ~p", [Error]),
					agent:set_state(AgentPid, released, "badring"),
					{reply, invalid, State#state{cook = QCall#queued_call.cook}}
			end;
		
		
		
		%	{ok, UUID} = freeswitch:api(State#state.cnode, create_uuid),
%			Args = "{origination_uuid=" ++ UUID ++ ",originate_timeout=" ++ integer_to_list(Ringout) ++ "}sofia/default/" ++ AgentRec#agent.login ++ "%" ++ State#state.domain ++ " &park()",
%			F = fun(ok, _Reply) ->
%					% agent picked up?
%					freeswitch:api(State#state.cnode, uuid_bridge, UUID ++ " " ++ Call#call.id);
%				(error, Reply) ->
%					?CONSOLE("originate failed: ~p", [Reply]),
%					gen_server:cast(QCall#queued_call.cook, {stop_ringing, AgentPid})
%			end,
%			case freeswitch:bgapi(State#state.cnode, originate, Args, F) of
%			%	{ok, _Msg} ->
%				ok ->
%					case freeswitch_ring:start(State#state.cnode, UUID, AgentPid, Call) of
%						{ok, Pid} when is_pid(Pid) ->
%							?CONSOLE("Started the freeswitch_ring got ~p", [Pid]),
%							{reply, ok, State#state{agent_pid = AgentPid, cook=QCall#queued_call.cook}};
%						{error, {error, baduuid}} ->
%							?CONSOLE("Bad uuid (~p)got passed in, resetting this for another tick pass", [UUID]),
%							agent:set_state(AgentPid, idle),
%							{reply, invalid, State#state{cook = QCall#queued_call.cook}}
%					end;
%					%{ok, Pid} = freeswitch_ring:start(State#state.cnode, UUID, AgentPid, Call),
%					%F2 = fun(F2) ->
%					%		receive
%					%			X ->
%					%				%io:format("got message ~p~n", [X]),
%					%				F2(F2)
%					%		end
%					%end,
%					%spawn(fun() ->
%					%	freeswitch:handlecall(State#state.cnode, UUID),
%					%	F2(F2)
%					%end),
%						
%					%{reply, ok, State#state{agent_pid = AgentPid, cook=QCall#queued_call.cook}};
%				{error, Msg} ->
%					?CONSOLE("Failed to ring agent ~p with error ~p", [AgentRec#agent.login, Msg]),
%					X = agent:set_state(AgentPid, released, "reason"),
%					?CONSOLE("------- ~p", [X]),
%					{reply, invalid, State#state{cook = QCall#queued_call.cook}};
%				timeout ->
%					?CONSOLE("timed out waiting for bgapi response", []),
%					agent:set_state(AgentPid, released, "failure!"),
%					{reply, invalid, State#state{cook = QCall#queued_call.cook}}
%			end;
			%	Else ->
			%		?CONSOLE("Freeswitch api response: ~p",[Else]),
			%		{reply, {invalid, Else}, State}
			%end;
		Else ->
			?INFO("Agent ringing response:  ~p", [Else]),
			{reply, invalid, State#state{cook = QCall#queued_call.cook}}
	end;
	%Reply = freeswitch_media_manager:ring_agent(AgentPid, Call),
	%{reply, Reply, State};
handle_call(get_call, _From, State) ->
	{reply, State#state.callrec, State};
handle_call(get_queue, _From, State) ->
	{reply, State#state.queue_pid, State};
handle_call(get_agent, _From, State) ->
	{reply, State#state.agent_pid, State};
handle_call(unqueue, _From, #state{queue_pid = undefined} = State) ->
	?WARNING("Cannot unqueue because there is no queue pid defined", []),
	{reply, ok, State};
handle_call(unqueue, _From, #state{queue_pid = Qpid, callrec = Callrec} = State) when is_pid(Qpid) ->
	%% using a try in case the Qpid is dead.
	try call_queue:remove(Qpid, Callrec#call.id) of
		_Any ->
			{reply, ok, State#state{queue_pid = undefined, queue = undefined}}
	catch
		_:_ ->
			?WARNING("Cannot unqueue from ~p because it's dead.", [Qpid]),
			{reply, ok, State#state{queue_pid = undefined, queue = undefined}}
	end;
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
handle_cast(unqueue, #state{callrec = Callrec} = State) ->
	case State#state.queue_pid of
		undefined -> 
			?ERROR("not queued, something's borked", []),
			{noreply, State};
		QPid ->
			?DEBUG("queue removal result:  ~p", [call_queue:remove(QPid, Callrec#call.id)]),
			{noreply, State}
	end;
handle_cast(agent_oncall, State) ->
	case State#state.agent_pid of
		undefined ->
			?WARNING("No agent to set state to",[]),
			{noreply, State};
		Apid ->
			agent:set_state(Apid, oncall, State#state.callrec),
			{noreply, State}
	end;
handle_cast(stop_ringing, State) ->
	?DEBUG("hanging up ring channel", []),
	case State#state.ringchannel of
		undefined ->
			ok;
		RingChannel ->
			freeswitch_ring:hangup(RingChannel)
	end,
	{noreply, State#state{ringchannel=undefined, agent=undefined, agent_pid=undefined}};
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info({call, {event, [UUID | Rest]}}, State) ->
	?DEBUG("reporting new call ~p.", [UUID]),
	Callrec = #call{id = UUID, source = self()},
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
	case State#state.agent_pid of
		Apid when is_pid(Apid) ->
			gen_server:cast(State#state.cook, {stop_ringing, Apid}),
			?INFO("potential should be fulfilled", []),
			{noreply, State#state{agent = undefined, agent_pid = undefined, ringchannel = undefined}};
		_Else ->
			?INFO("disregarding potential ringout, not ringing to an agent", []),
			{noreply, State}
	end;
handle_info({bgerror, "-ERR USER_BUSY\n"}, State) ->
	?NOTICE("Agent rejected the call", []),
	case State#state.agent_pid of
		Apid when is_pid(Apid) ->
			gen_server:cast(State#state.cook, {stop_ringing, Apid}),
			{noreply, State#state{agent = undefined, agent_pid = undefined, ringchannel = undefined}};
		_Else ->
			{noreply, State}
	end;
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
					case queue_manager:get_queue(Queue) of
						undefined ->
							% TODO what to do w/ the call w/ no queue?
							?WARNING("Uh oh, no queue of ~p", [Queue]),
							{noreply, State};
						Qpid ->
							?DEBUG("Trying to add to queue...", []),
							R = call_queue:add(Qpid, self(), NewCall),
							?DEBUG("q response:  ~p", [R]),
							%freeswitch_media_manager:queued_call(UUID, Qpid),
							{noreply, State#state{queue_pid=Qpid, queue=Queue, callrec=NewCall}}
					end;
				_Otherwise ->
					{noreply, State}
			end;
		"CHANNEL_HANGUP" ->
			% on a channel destroy, set the agent to wrap-up.
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
							end,
							agent:set_state(Apid, idle);
						{ok, oncall} ->
							agent:set_state(Apid, wrapup, State#state.callrec);
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
			{noreply, State3};
		"CHANNEL_DESTROY" ->
			?DEBUG("Last message this will recieve, channel destroy", []),
			{stop, normal, State};
		%"PRESENCE_IN" ->
		%	?CONSOLE("Lets see if presence in will let me get out of queue...", []),
		%	Qpid = State#state.queue_pid,
		%	case Qpid of
		%		undefined ->
		%			?CONSOLE("I'm not sure if this is even possible, undefined queue during ~p", [Ename]),
		%			{noreply, State};
		%		_Else ->
		%			call_queue:remove(Qpid, self()),
		%			State2 = State#state{queue = undefined},
		%			{noreply, State2}
		%	end;
		{error, notfound} ->
			?WARNING("event name not found: ~p", [freeswitch:get_event_header(Rawcall, "Content-Type")]),
			{noreply, State};
		Else ->
			?DEBUG("Event unhandled ~p", [Else]),
			{noreply, State}
	end.
