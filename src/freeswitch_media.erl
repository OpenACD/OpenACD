%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Original Code is Spice Telphony.
%% 
%% The Initial Developer of the Original Code is 
%% Andrew Thompson and Micah Warren.
%% Portions created by the Initial Developers are Copyright (C) 
%% SpiceCSM. All Rights Reserved.

%% Contributor(s): 

%% Andrew Thompson <athompson at spicecsm dot com>
%% Micah Warren <mwarren at spicecsm dot com>
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

-define(TIMEOUT, 10000).

%% API
-export([
	start/1,
	get_call/1,
	get_queue/1,
	get_agent/1,
	unqueue/1,
	set_agent/3
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {callrec = #call{}, queue, queue_pid, cnode, agent, agent_pid}).

%%====================================================================
%% API
%%====================================================================
%% @doc starts the freeswitch media gen_server.  `Cnode' is the C node the communicates directly with freeswitch.
start(Cnode) -> 
	gen_server:start(?MODULE, [Cnode], []).

%% @doc returns the record of the call freeswitch media `MPid' is in charge of.
-spec(get_call(MPid :: pid()) -> #call{}).
get_call(MPid) -> 
	gen_server:call(MPid, get_call).

-spec(get_queue(MPid :: pid()) -> pid()).
get_queue(MPid) -> 
	gen_server:call(MPid, get_queue).

-spec(get_agent(MPid :: pid()) -> pid()).
get_agent(MPid) -> 
	gen_server:call(MPid, get_agent).

-spec(unqueue(Mpid :: pid()) -> 'ok').
unqueue(MPid) -> 
	gen_server:call(MPid, unqueue).

-spec(set_agent(Mpid :: pid(), Agent :: any(), Apid :: pid()) -> 'ok').
set_agent(MPid, Agent, Apid) when is_pid(MPid), is_pid(Apid) ->
	gen_server:call(MPid, {set_agent, Agent, Apid}).
	
%%====================================================================
%% gen_server callbacks
%%====================================================================
%% @private
init([Cnode]) ->
    {ok, #state{cnode=Cnode}}.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
% TODO If this module's state has the call, why does this need the call passed in?  (part of the spec for a generic media_manager, change spec?)
handle_call({ring_agent, AgentPid, Call}, _From, State) -> 
	Reply = freeswitch_media_manager:ring_agent(AgentPid, Call),
	{reply, Reply, State};
handle_call(get_call, _From, State) -> 
	{reply, State#state.callrec, State};
handle_call(get_queue, _From, State) -> 
	{reply, State#state.queue_pid, State};
handle_call(get_agent, _From, State) -> 
	{reply, State#state.agent_pid, State};
handle_call(unqueue, _From, #state{queue_pid = undefined} = State) -> 
	?CONSOLE("Cannot unqueue because there is no queue pid defined", []),
	{reply, ok, State};
handle_call(unqueue, _From, #state{queue_pid = Qpid, callrec = Callrec} = State) when is_pid(Qpid) -> 
	%% using a try in case the Qpid is dead.
	try call_queue:remove(Qpid, Callrec#call.id) of
		_Any -> 
			{reply, ok, State#state{queue_pid = undefined, queue = undefined}}
	catch
		_:_ -> 
			?CONSOLE("Cannot unqueue from ~p because it's dead.", [Qpid]),
			{reply, ok, State#state{queue_pid = undefined, queue = undefined}}
	end;
handle_call({set_agent, Agent, Apid}, _From, State) -> 
	{reply, ok, State#state{agent = Agent, agent_pid = Apid}};
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
	?CONSOLE("~p reporting new call ~p.", [self(), UUID]),
	case_event_name([UUID | Rest], State);
handle_info({call_event, {event, [UUID | Rest]}}, State) -> 
	?CONSOLE("~p reporting existing call progess ~p.", [self(), UUID]),
	% TODO flesh out for all call events.
	case_event_name([ UUID | Rest], State);
handle_info({set_agent, Login, Apid}, State) -> 
	{noreply, State#state{agent = Login, agent_pid = Apid}};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @private
terminate(_Reason, _State) ->
	?CONSOLE("terminating...", []),
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
has_dst_chan(Rawcall) -> 
	case freeswitch:get_event_header(Rawcall, "variable_dstchan") of
		{error, notfound} -> 
			false;
		Else -> 
			Else
	end.

%% @private
case_event_name([UUID | Rawcall], #state{callrec = Callrec} = State) ->
	Ename = freeswitch:get_event_name(Rawcall),
%	?CONSOLE("Event:  ~p;  UUID:  ~p", [Ename, UUID]),
	case Ename of
		"CHANNEL_PARK" ->
			case has_dst_chan(Rawcall) of
				false -> 
					case State#state.queue_pid of
						undefined ->
							Queue = freeswitch:get_event_header(Rawcall, "variable_queue"),
							Brand = freeswitch:get_event_header(Rawcall, "variable_brand"),
							Callerid = freeswitch:get_event_header(Rawcall, "Caller-Caller-ID-Name"),
							NewCall = Callrec#call{id=UUID, client=Brand, callerid=Callerid, source=self()},
							case queue_manager:get_queue(Queue) of
								undefined ->
									% TODO what to do w/ the call w/ no queue?
									?CONSOLE("Uh oh, no queue of ~p", [Queue]),
									{noreply, State};
								Qpid -> 
									?CONSOLE("Trying to add to queue...", []),
									R = call_queue:add(Qpid, self()),
									?CONSOLE("q response:  ~p", [R]),
									%freeswitch_media_manager:queued_call(UUID, Qpid),
									{noreply, State#state{queue_pid=Qpid, queue=Queue, callrec=NewCall}}
							end;
						 _Otherwise -> 
							 {noreply, State} 
					end;
				Else ->
					% TODO - this isn't safe
					{ok, Elsepid} = freeswitch_media_manager:get_handler(Else),
					?CONSOLE("Bridging UUID ~p to ~p", [UUID, Else]),
					X = freeswitch:api(State#state.cnode, uuid_bridge, lists:append([Else, " ", UUID])),
					?CONSOLE("result of bridging to node ~p was ~p", [State#state.cnode, X]),
					% remove the call from queue, in this case Else.
					freeswitch_media:unqueue(Elsepid),
					% now to tell the agent that it is no longer ringing, but in call.
					case freeswitch:get_event_header(Rawcall, "variable_agent") of
						{error, notfound} -> 
							?CONSOLE("Could not find agent in headers for ~p.", [Else]),
							{noreply, State};
						Agent ->
							{true, Apid} = agent_manager:query_agent(Agent),
							agent:set_state(Apid, oncall, Callrec),
							freeswitch_media:set_agent(Elsepid, Agent, Apid),
							{noreply, State}
					end
			end;
		"CHANNEL_HANGUP" -> 
			% on a channel destroy, set the agent to wrap-up.
			Qpid = State#state.queue_pid,
			Apid = State#state.queue_pid,
			case Apid of
				undefined -> 
					State2 = State#state{agent = undefined, agent_pid = undefined};
				_Other -> 
					agent:set_state(Apid, wrapup, State#state.callrec),
					State2 = State#state{agent = undefined, agent_pid = undefined}
			end,
			case Qpid of
				undefined -> 
					State3 = State2#state{agent = undefined, agent_pid = undefined};
				_Else -> 
					call_queue:remove(Qpid, State#state.callrec),
					State3 = State2#state{queue = undefined, queue_pid = undefined}
			end,
			{noreply, State3};
		Else -> 
		%	?CONSOLE("Event unhandled", []),
			{noreply, State}
	end.
