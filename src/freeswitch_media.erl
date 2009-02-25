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
	set_agent/3
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	callrec = #call{},
	queue,
	queue_pid,
	cnode,
	domain,
	agent,
	agent_pid,
	dstchan = undefined
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
init([Cnode, Domain]) ->
	{ok, #state{cnode=Cnode, domain=Domain}}.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
% TODO If this module's state has the call, why does this need the call passed in?  (part of the spec for a generic media_manager, change spec?)
handle_call({ring_agent, AgentPid, QCall}, _From, #state{callrec = Call} = State) ->
	?CONSOLE("ring_agent to ~p for call ~p", [AgentPid, QCall#queued_call.id]),
	AgentRec = agent:dump_state(AgentPid),
	case agent:set_state(AgentPid, ringing, Call) of
		ok ->
			Args = "{dstchan=" ++ Call#call.id ++ ",agent="++ AgentRec#agent.login ++"}sofia/default/" ++ AgentRec#agent.login ++ "%" ++ State#state.domain ++ " '&erlang(freeswitch_media_manager:! "++atom_to_list(node())++")'",
			X = freeswitch:bgapi(State#state.cnode, originate, Args),
			?CONSOLE("Bgapi call res:  ~p;  With args: ~p", [X, Args]),
			{reply, ok, State};
		Else ->
			?CONSOLE("Agent ringing response:  ~p", [Else]),
			{reply, invalid, State}
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
	?CONSOLE("reporting new call ~p.  dstchan:  ~p", [UUID, has_dst_chan(Rest)]),
	case has_dst_chan(Rest) of
		false ->
			State2 = State#state{dstchan = undefined};
		Dstchan ->
			State2 = State#state{dstchan = Dstchan}
	end,
	case_event_name([UUID | Rest], State2);
handle_info({call_event, {event, [UUID | Rest]}}, State) ->
	?CONSOLE("reporting existing call progess ~p.", [UUID]),
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
terminate(Reason, _State) ->
	?CONSOLE("terminating: ~p", [Reason]),
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
	%?CONSOLE("Rawcall for has_dst_chan check: ~p", [Rawcall]),
	case freeswitch:get_event_header(Rawcall, "variable_dstchan") of
		{error, notfound} ->
			false;
		Else ->
			Else
	end.

%% @private
case_event_name([UUID | Rawcall], #state{callrec = Callrec, dstchan = Dstchan} = State) ->
	Ename = freeswitch:get_event_name(Rawcall),
	?CONSOLE("Event:  ~p;  UUID:  ~p", [Ename, UUID]),
	case Dstchan of
		undefined ->
			case Ename of
				"CHANNEL_PARK" ->
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
									R = call_queue:add(Qpid, 1, self(), NewCall),
									?CONSOLE("q response:  ~p", [R]),
									%freeswitch_media_manager:queued_call(UUID, Qpid),
									{noreply, State#state{queue_pid=Qpid, queue=Queue, callrec=NewCall}}
							end;
						 _Otherwise ->
							 {noreply, State}
					end;
				"CHANNEL_HANGUP" ->
					% on a channel destroy, set the agent to wrap-up.
					?CONSOLE("Channel hangup", []),
					Qpid = State#state.queue_pid,
					Apid = State#state.agent_pid,
					case Apid of
						undefined ->
							?CONSOLE("Agent undefined", []),
							State2 = State#state{agent = undefined, agent_pid = undefined};
						_Other ->
							agent:set_state(Apid, wrapup, State#state.callrec),
							State2 = State#state{agent = undefined, agent_pid = undefined}
					end,
					case Qpid of
						undefined ->
							?CONSOLE("Queue undefined", []),
							State3 = State2#state{agent = undefined, agent_pid = undefined};
						_Else ->
							call_queue:remove(Qpid, self()),
							State3 = State2#state{queue = undefined, queue_pid = undefined}
					end,
					{noreply, State3};
				"CHANNEL_DESTROY" ->
					?CONSOLE("Last message this will recieve, channel destroy", []),
					{stop, normal, State};
				"PRESENCE_IN" ->
					?CONSOLE("Lets see if presence in will let me get out of queue...", []),
					Qpid = State#state.queue_pid,
					case Qpid of
						undefined ->
							?CONSOLE("I'm not sure if this is even possible, undefined queue during ~p", [Ename]),
							{noreply, State};
						_Else ->
							call_queue:remove(Qpid, self()),
							State2 = State#state{queue = undefined},
							{noreply, State2}
					end;
				{error, notfound} ->
					?CONSOLE("event name not found: ~p", [freeswitch:get_event_header(Rawcall, "Content-Type")]),
					{noreply, State};
				Else ->
					?CONSOLE("Event unhandled ~p", [Else]),
					{noreply, State}
			end;
		Dstchan ->
			?CONSOLE("dstchan mode Ename:  ~p", [Ename]),
			{noreply, State}
	end.
