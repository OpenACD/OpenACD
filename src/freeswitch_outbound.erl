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

%% @doc Helper module for freeswitch media to make an outbound call.
-module(freeswitch_outbound).
-author("Micah").

-behaviour(gen_media).

-ifdef(EUNIT).
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
	handle_announce/2,
	handle_answer/3,
	handle_ring/3,
	handle_voicemail/1,
	handle_ring_stop/1,
	handle_agent_transfer/4,
	handle_queue_transfer/1,
	handle_wrapup/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-record(state, {
	cnode :: atom(),
	uuid :: any(),
	agent_pid :: pid(),
	agent :: string(),
	callrec :: #call{}
	}).

-type(state() :: #state{}).
-define(GEN_MEDIA, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================
start(Fnode, AgentRec, Apid, Number, Ringout, Domain) when is_pid(Apid) ->
	gen_media:start(?MODULE, [Fnode, AgentRec, Apid, Number, Ringout, Domain]).

start_link(Fnode, AgentRec, Apid, Number, Ringout, Domain) when is_pid(Apid) ->
	gen_media:start_link(?MODULE, [Fnode, AgentRec, Apid, Number, Ringout, Domain]).

hangup(Pid) ->
	gen_media:cast(Pid, hangup).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Fnode, AgentRec, Apid, Number, Ringout, _Domain]) ->
	case freeswitch:api(Fnode, create_uuid) of
		{ok, UUID} ->
			Call = #call{id=UUID, source=self(), type=voice},
			Args = "[hangup_after_bridge=true,origination_uuid=" ++ UUID ++ ",originate_timeout=" ++ integer_to_list(Ringout) ++ "]user/" ++ AgentRec#agent.login ++ " " ++ Number ++ " xml outbound",
			?INFO("Originating outbound call with args: ~p", [Args]),
			F = fun(ok, _Reply) ->
					% agent picked up?
					ok;
				(error, Reply) ->
					?WARNING("originate failed: ~p", [Reply]),
					ok
			end,
			case freeswitch:bgapi(Fnode, originate, Args, F) of
				ok ->
					Gethandle = fun(Recusef, Count) ->
						?DEBUG("Counted ~p", [Count]),
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
							?ERROR("bad uuid ~p", [UUID]),
							{stop, {error, session}};
						{error, Other} ->
							?ERROR("other error starting; ~p", [Other]),
							{stop, {error, Other}};
						_Else ->
							?DEBUG("starting for ~p", [UUID]),
							{ok, {#state{cnode = Fnode, uuid = UUID, agent_pid = Apid, agent = AgentRec#agent.login, callrec = Call}, Call}}
					end;
				Else ->
					?ERROR("bgapi call failed ~p", [Else]),
					{stop, {error, Else}}
			end
	end.
%%--------------------------------------------------------------------
%% Description: gen_media
%%--------------------------------------------------------------------

handle_announce(_Announce, State) ->
	{ok, State}.
	
handle_answer(_Apid, _Call, State) ->
	{error, outgoing_only, State}.

handle_ring(_Apid, _Call, State) ->
	{error, outgoing_only, State}.
	
handle_ring_stop(State) ->
	{ok, State}.

handle_voicemail(State) ->
	{invalid, State}.

handle_agent_transfer(_Agent, _Call, _Timeout, State) ->
	{error, outgoing_only, State}.

handle_queue_transfer(State) ->
	% TODO flesh this out.
	{ok, State}.

handle_wrapup(State) ->
	{ok, State}.
	
%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
	Reply = {unknown, Request},
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(hangup, #state{uuid = UUID} = State) ->
	freeswitch:sendmsg(State#state.cnode, UUID,
		[{"call-command", "hangup"},
			{"hangup-cause", "NORMAL_CLEARING"}]),
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({call, {event, [UUID | _Rest]}}, #state{uuid = UUID} = State) ->
	?DEBUG("call", []),
	{noreply, State};
handle_info({call_event, {event, [UUID | Rest]}}, #state{uuid = UUID} = State) ->
	Event = freeswitch:get_event_name(Rest),
	case Event of
		"CHANNEL_BRIDGE" ->
			?INFO("Call bridged", []),
			{outbound, State#state.agent, State};
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
handle_info(call_hangup, State) ->
	?DEBUG("Call hangup info", []),
	{stop, normal, State};
handle_info(Info, State) ->
	?DEBUG("unhandled info ~p", [Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
	?NOTICE("FreeSWITCH outbound channel teminating ~p", [Reason]),
	ok.

%%--------------------------------------------------------------------
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
