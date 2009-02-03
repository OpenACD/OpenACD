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
-export([start/1]).

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
fifo_parse(Rawcall, State) -> 
	?CONSOLE("fifo::info needs more processing.", []),
	case freeswitch:get_event_header(Rawcall, "FIFO-Action") of
		"push" -> 
			?CONSOLE("put into queue", []),
				case queue_manager:get_queue(State#state.queue) of
					undefined ->
						% TODO if we start using fifo again, what do we really want to do here?
						?CONSOLE("Uh oh, no queue of ~p", [State#state.queue]),
						{noreply, State};
					Qpid -> 
						?CONSOLE("Trying to add to queue...", []),
						% TODO and if this failed?
						R = call_queue:add(Qpid, State#state.callrec),
						?CONSOLE("q response:  ~p", [R]),
						{noreply, State#state{queue_pid=Qpid}}
				end;
		"abort" ->
			?CONSOLE("happy hangup", []),
			{noreply, State};
		Else -> 
			% TODO uh indeed.
			?CONSOLE("Uh...~p", [Else]),
			{noreply, State}
	end.

case_event_name([UUID | Rawcall], State) ->
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
							% TODO condense the next 2 lines into one
							Protocall = State#state.callrec,
							Protocall2 = Protocall#call{id=UUID, client=Brand, callerid=Callerid, source=self()},
							case queue_manager:get_queue(Queue) of
								undefined ->
									% TODO what to do w/ the call w/ no queue?
									?CONSOLE("Uh oh, no queue of ~p", [Queue]),
									{noreply, State};
								Qpid -> 
									?CONSOLE("Trying to add to queue...", []),
									R = call_queue:add(Qpid, Protocall2),
									?CONSOLE("q response:  ~p", [R]),
									freeswitch_media_manager:queued_call(UUID, Qpid),
									{noreply, State#state{queue_pid=Qpid, queue=Queue, callrec=Protocall2}}
							end;
						 _Otherwise -> 
							 {noreply, State} 
					end;
				Else -> 
					?CONSOLE("Bridging UUID ~p to ~p", [UUID, Else]),
					X = freeswitch:api(State#state.cnode, uuid_bridge, lists:append([Else, " ", UUID])),
					?CONSOLE("result of bridging to node ~p was ~p", [State#state.cnode, X]),
					% remove the call from queue, in this case Else.
					{ok, Qpid} = freeswitch_media_manager:find_queued(Else),
					?CONSOLE("Time to remove ~p from ~p", [Else, Qpid]),
					{_Key, Callrec} = call_queue:get_call(Qpid, Else),
					call_queue:remove(Qpid, Else),
					freeswitch_media_manager:unqueue_call(Else),
					% now to tell the agent that it is no longer ringing, but in call.
					case freeswitch:get_event_header(Rawcall, "variable_agent") of
						{error, notfound} -> 
							?CONSOLE("Could not find agent in headers for ~p.", [Else]),
							{noreply, State};
						Agent ->
							{true, Apid} = agent_manager:query_agent(Agent),
							agent:set_state(Apid, oncall, Callrec),
							freeswitch_media_manager:agent_call(Else, Apid),
							{noreply, State}
					end
			end;
		"CHANNEL_HANGUP" -> 
			% on a channel destroy, set the agent to wrap-up.
			case freeswitch_media_manager:find_agent(UUID) of
				{ok, Apid} -> 
					agent:set_state(Apid, wrapup, State#state.callrec),
					freeswitch_media_manager:unagent_call(UUID),
					{noreply, State};
				Else -> 
					{noreply, State}
			end;
		Else -> 
		%	?CONSOLE("Event unhandled", []),
			{noreply, State}
	end.