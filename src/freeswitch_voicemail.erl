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

-module(freeswitch_voicemail).

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
	start/4,
	start_link/4,
	get_call/1,
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
	handle_agent_transfer/4,
	handle_queue_transfer/1,
	handle_wrapup/1,
	handle_call/3, 
	handle_cast/2, 
	handle_info/2,
	handle_warm_transfer_begin/2,
	terminate/2,
	code_change/3]).

-record(state, {
	callrec = undefined :: #call{} | 'undefined',
	cook :: pid() | 'undefined',
	queue :: string() | 'undefined',
	queue_pid :: pid() | 'undefined',
	cnode :: atom(),
	agent :: string() | 'undefined',
	agent_pid :: pid() | 'undefined',
	ringchannel :: pid() | 'undefined',
	ringuuid :: string() | 'undefined',
	manager_pid :: 'undefined' | any(),
	file = erlang:error({undefined, file}):: string()
	}).

-type(state() :: #state{}).
-define(GEN_MEDIA, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================
%% @doc starts the freeswitch media gen_server.  `Cnode' is the C node the communicates directly with freeswitch.
%-spec(start/1 :: (Cnode :: atom()) -> {'ok', pid()}).
start(Cnode, UUID, File, Queue) ->
	gen_media:start(?MODULE, [Cnode, UUID, File, Queue]).

%-spec(start_link/1 :: (Cnode :: atom()) -> {'ok', pid()}).
start_link(Cnode, UUID, File, Queue) ->
	gen_media:start_link(?MODULE, [Cnode, UUID, File, Queue]).

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
init([Cnode, UUID, File, Queue]) ->
	process_flag(trap_exit, true),
	Manager = whereis(freeswitch_media_manager),
	{ok, {#state{cnode=Cnode, manager_pid = Manager, file=File}, {Queue, #call{id=UUID++"-vm", type=voice, source=self()}}}}.

handle_announce(Announcement, #state{callrec = Callrec} = State) ->
	{invalid, State}.

handle_answer(Apid, Callrec, #state{file=File} = State) ->
	?NOTICE("Voicemail ~s answered! Time to play ~s", [Callrec#call.id, File]),
	freeswitch:sendmsg(State#state.cnode, State#state.ringuuid,
		[{"call-command", "execute"},
			{"execute-app-name", "playback"},
			{"execute-app-arg", File}]),
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
	AgentRec = agent:dump_state(Apid),
	case freeswitch_ring:start(State#state.cnode, AgentRec, Apid, Callrec, 600, F, [single_leg, {eventfun, F2}]) of
		{ok, Pid} ->
			link(Pid),
			{ok, State#state{ringchannel = Pid, ringuuid = freeswitch_ring:get_uuid(Pid), agent_pid = Apid}};
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

handle_voicemail(#state{callrec = Call} = State) ->
	{invalid, State}.

handle_agent_transfer(AgentPid, Call, Timeout, State) ->
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
	% TODO - test that handle_answer gets called again...
	case freeswitch_ring:start(State#state.cnode, AgentRec, AgentPid, Call, Timeout, F, [single_leg]) of
		{ok, Pid} ->
			{ok, State#state{agent_pid = AgentPid, ringchannel=Pid}};
		{error, Error} ->
			?ERROR("error:  ~p", [Error]),
			{error, Error, State}
	end.

handle_warm_transfer_begin(Number, #state{agent_pid = AgentPid, callrec = Call, cnode = Node} = State) when is_pid(AgentPid) ->
	{invalid, State};
handle_warm_transfer_begin(_Number, #state{agent_pid = AgentPid} = State) ->
	?WARNING("wtf?! agent pid is ~p", [AgentPid]),
	{error, "error: no agent bridged to this call~n", State}.


handle_wrapup(State) ->
	% This intentionally left blank; media is out of band, so there's
	% no direct hangup by the agent
	{ok, State}.
	
handle_queue_transfer(State) ->
	% TODO fully implement this.
	{ok, State}.
%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
handle_call(get_call, _From, State) ->
	{reply, State#state.callrec, State};
handle_call(get_queue, _From, State) ->
	{reply, State#state.queue_pid, State};
handle_call(get_agent, _From, State) ->
	{reply, State#state.agent_pid, State};
handle_call({set_agent, Agent, Apid}, _From, State) ->
	{reply, ok, State#state{agent = Agent, agent_pid = Apid}};
handle_call(dump_state, _From, State) ->
	{reply, State, State};
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
handle_info(check_recovery, State) ->
	case whereis(freeswitch_media_manager) of
		Pid when is_pid(Pid) ->
			link(Pid),
			Call = State#state.callrec,
			gen_server:cast(freeswitch_media_manager, {notify, Call#call.id, self()}),
			{noreply, State#state{manager_pid = Pid}};
		_Else ->
			{ok, Tref} = timer:send_after(1000, check_recovery),
			{noreply, State#state{manager_pid = Tref}}
	end;
handle_info({'EXIT', Pid, Reason}, #state{ringchannel = Pid} = State) ->
	?WARNING("Handling ring channel ~w exit ~p", [Pid, Reason]),
	{wrapup, State};
handle_info({'EXIT', Pid, Reason}, #state{manager_pid = Pid} = State) ->
	?WARNING("Handling manager exit from ~w due to ~p", [Pid, Reason]),
	{ok, Tref} = timer:send_after(1000, check_recovery),
	{noreply, State#state{manager_pid = Tref}};
handle_info(Info, State) ->
	?INFO("unhandled info ~p", [Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @private
terminate(Reason, State) ->
	?NOTICE("terminating: ~p", [Reason]),
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

