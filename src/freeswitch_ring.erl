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

%% @doc Helper module for freeswitch media to ring to an agent.
-module(freeswitch_ring).
-author("Micah").

-behaviour(gen_server).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("queue.hrl").
-include("call.hrl").
-include("agent.hrl").


%% API
-export([
	start_link/4,
	start/6
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	cnode :: node(),
	uuid :: any(),
	agent_pid :: pid(),
	callrec :: #call{}
	}).

%%====================================================================
%% API
%%====================================================================
start(Fnode, AgentRec, Apid, Qcall, Ringout, Domain) when is_pid(Apid), is_record(Qcall, call) ->
	gen_server:start(?MODULE, [Fnode, AgentRec, Apid, Qcall, Ringout, Domain], []).
	
start_link(Fnode, UUID, Apid, Callrec) when is_pid(Apid), is_record(Callrec, call) ->
    gen_server:start_link(?MODULE, [Fnode, UUID, Apid, Callrec], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Fnode, AgentRec, Apid, Qcall, Ringout, Domain]) ->
	case freeswitch:api(Fnode, create_uuid) of
		{ok, UUID} ->
			Args = "[origination_uuid=" ++ UUID ++ ",originate_timeout=" ++ integer_to_list(Ringout) ++ "]sofia/default/" ++ AgentRec#agent.login ++ "%" ++ Domain ++ " &park()",
			F = fun(ok, _Reply) ->
					% agent picked up?
					freeswitch:api(Fnode, uuid_bridge, UUID ++ " " ++ Qcall#call.id);
				(error, Reply) ->
					?CONSOLE("originate failed: ~p", [Reply]),
					gen_server:cast(Qcall#call.cook, {stop_ringing, Apid})
			end,
			case freeswitch:bgapi(Fnode, originate, Args, F) of
				ok ->
					Gethandle = fun(Recusef, Count) ->
						?CONSOLE("Counted ~p", [Count]),
						case freeswitch:handlecall(Fnode, UUID) of
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
							?CONSOLE("bad uuid ~p", [UUID]),
							{stop, {error, session}};
						{error, Other} ->
							?CONSOLE("other error starting; ~p", [Other]),
							{stop, {error, Other}};
						_Else ->
							?CONSOLE("starting for ~p", [UUID]),
							{ok, #state{cnode = Fnode, uuid = UUID, agent_pid = Apid, callrec = Qcall}}
					end;
				Else ->
					?CONSOLE("bgapi call failed ~p", [Else]),
					{stop, {error, Else}}
			end
	end.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
    Reply = {unknown, Request},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({call, {event, [UUID | _Rest]}}, #state{uuid = UUID} = State) ->
	?CONSOLE("call", []),
	{noreply, State};
handle_info({call_event, {event, [UUID | Rest]}}, #state{uuid = UUID} = State) ->
	Event = freeswitch:get_event_name(Rest),
	case Event of
		"CHANNEL_BRIDGE" ->
			?CONSOLE("Call bridged", []),
			Call = State#state.callrec,
			case agent:set_state(State#state.agent_pid, oncall, Call) of
				ok ->
					gen_server:call(Call#call.source, unqueue),
					{noreply, State};
				invalid ->
					?CONSOLE("Cannot set agent ~p to oncall with media ~p", [State#state.agent_pid, Call#call.id]),
					{noreply, State}
			end;
		%"CHANNEL_UNBRIDGE" ->
		%	% if this event happens, it means the remote side (caller) has hungup.  For now, we do nothing but console log it.
		%	?CONSOLE("agent should be in wrapup, seems like remote dc'ed", []),
		%	{noreply, State};
		"CHANNEL_UNBRIDGE" -> 
			% if the agent is in wrap-up, set them to idle (or at least attempt).
			% this is for niftyness
			case agent:query_state(State#state.agent_pid) of
				{ok, wrapup} ->
					agent:set_state(State#state.agent_pid, idle),
					{noreply, State};
				{ok, _Other} ->
					{noreply, State}
			end;
		_Else ->
			?CONSOLE("call_event ~p", [Event]),
			{noreply, State}
	end;
handle_info(Info, State) ->
	?CONSOLE("unhandled info ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
	?CONSOLE("Teminationg ~p", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
