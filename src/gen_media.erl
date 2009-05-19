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

-module(gen_media).
-author(micahw).

-behaviour(gen_server).

-include("log.hrl").
-include("call.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
	behaviour_info/1,
	start_link/2,
	start/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% gen_media api
-export([
	ring/4,
	get_call/1,
	voicemail/1,
	announce/2,
	stop_ringing/1,
	oncall/1,
	call/2,
	call/3,
	cast/2
]).

-record(state, {
	callback,
	substate,
	callrec,
	agent_pid,
	ringout = false
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

-define(MEDIA_ACTIONS, [ring_agent, get_call, start_cook, voicemail, announce, stop_cook]).

-spec(behaviour_info/1 :: 
	(Info :: 'callbacks') -> [{atom(), non_neg_integer()}];
	(Info :: any()) -> 'undefined').
behaviour_info(callbacks) ->
	GS = gen_server:behaviour_info(callbacks),
	lists:append([{handle_ring, 3}, {handle_ring_stop, 1}, {handle_answer, 3}, {handle_voicemail, 1}, {handle_annouce, 2}], GS);
behaviour_info(_Other) ->
    undefined.

ring(Genmedia, Agent, Qcall, Timeout) ->
	gen_server:call(Genmedia, {'$gen_media_ring', Agent, Qcall, Timeout}).

get_call(Genmedia) ->
	gen_server:call(Genmedia, '$gen_media_get_call').

voicemail(Genmedia) ->
	gen_server:call(Genmedia, '$gen_media_voicemail').

announce(Genmedia, Annouce) ->
	gen_server:call(Genmedia, {'$gen_media_annouce', Annouce}).

stop_ringing(Genmedia) ->
	Genmedia ! '$gen_media_stop_ring'.

oncall(Genmedia) ->
	gen_server:call(Genmedia, '$gen_media_agent_oncall').

call(Genmedia, Request) ->
	gen_server:call(Genmedia, Request).

call(Genmedia, Request, Timeout) ->
	gen_server:call(Genmedia, Request, Timeout).
	
cast(Genmedia, Request) ->
	gen_server:cast(Genmedia, Request).
%
%handle_call({ring_agent, AgentPid, QCall, Timeout}, _From, #state{callrec = Call} = State) ->
%handle_call(get_call, _From, State) ->
%handle_call(get_queue, _From, State) ->
%handle_call(get_agent, _From, State) ->
%handle_call(unqueue, _From, #state{queue_pid = undefined} = State) ->
%handle_call(unqueue, _From, #state{queue_pid = Qpid, callrec = Callrec} = State) when is_pid(Qpid) ->
%handle_call({set_agent, Agent, Apid}, _From, State) ->
%handle_call(dump_state, _From, State) ->
%handle_call({announce, Announcement}, _From, #state{callrec = Callrec} = State) ->
%handle_cast(unqueue, #state{callrec = Callrec} = State) ->
%handle_cast(agent_oncall, State) ->
%handle_cast(stop_ringing, State) ->
%
%
%handle_call({ring_agent, AgentPid, Queuedcall, Ringout}, _From, #state{fail = Fail} = State) -> 
%handle_call(get_call, _From, #state{fail = Fail} = State) -> 
%handle_call({start_cook, Recipe, Queuename}, _From, #state{callrec = Call, fail = Fail} = State) -> 
%handle_call({stop, Reason}, _From, State) ->
%handle_call(stop_cook, _From, #state{callrec = Call, fail = Fail} = State) -> 
%handle_call(voicemail, _From, #state{fail = Fail} = State) ->
%handle_call({announce, _Args}, _From, #state{fail = Fail} = State) ->
%	
%	
%	
	
	
%%====================================================================
%% API
%%====================================================================

start_link(Callback, Args) ->
    gen_server:start_link(?MODULE, [Callback, Args], []).

start(Callback, Args) ->
	gen_server:start(?MODULE, [Callback, Args], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Callback, Args]) ->
	{ok, {Substate, Callrec}} = Callback:init(Args),
    {ok, #state{callback = Callback, substate = Substate, callrec = Callrec}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------

handle_call('$gen_media_get_call', From, State) ->
	{reply, State#state.callrec, State};
handle_call({'$gen_media_ring', Agent, QCall, Timeout}, From, #state{callrec = Call, callback = Callback} = State) ->
	% TODO set the cook to the call directly
	?INFO("Trying to ring ~p with timeout ~p", [Agent, Timeout]),
	case agent:set_state(Agent, ringing, Call#call{cook=QCall#queued_call.cook}) of
		ok ->
			case Callback:handle_ring(Agent, State#state.callrec, State#state.substate) of
				{ok, Substate} ->
					{ok, _Tref} = timer:send_after(Timeout, {'$gen_media_stop_ring', QCall#queued_call.cook}),
					{reply, ok, State#state{substate = Substate, agent_pid = Agent, ringout=true}};
				{invalid, Substate} ->
					agent:set_state(Agent, idle),
					{reply, invalid, State#state{substate = Substate}}
			end;
		Else ->
			?INFO("Agent ringing response:  ~p", [Else]),
			{reply, invalid, State}
	end;
handle_call({'$gen_media_annouce', Annouce}, From, #state{callback = Callback} = State) ->
	?INFO("Doing announce", []),
	{ok, Substate} = Callback:handle_announce(Annouce, State),
	{reply, ok, State#state{substate = Substate}};
handle_call('$gen_media_voicemail', From, #state{callback = Callback} = State) ->
	?INFO("trying to send media to voicemail", []),
	{Res, Substate} = Callback:handle_voicemail(State#state.substate),
	{reply, Res, State#state{substate = Substate}};
handle_call('$gen_media_agent_oncall', {Apid, _Tag}, #state{callback = Callback, agent_pid = Apid, callrec = #call{ring_path = inband} = Callrec} = State) ->
	?INFO("oncall request from agent ~p", [Apid]),
	case Callback:handle_answer(Apid, State#state.callrec, State#state.substate) of
		{ok, NewState} ->
			{reply, ok, State#state{substate = NewState, ringout = false}};
		{error, Reason, NewState} ->
			{reply, invalid, State#state{substate = NewState}}
	end;
handle_call('$gen_media_agent_oncall', {Apid, _Tag}, #state{agent_pid = Apid} = State) ->
	?INFO("Cannot accept on call requests from agent (~p) unless ring_path is inband", [Apid]),
	{reply, invalid, State};
handle_call('$gen_media_agent_oncall', From, #state{agent_pid = Apid, callback = Callback} = State) ->
	?INFO("oncall request from ~p; agent to set on call is ~p", [From, Apid]),
	case Callback:handle_answer(Apid, State#state.callrec, State#state.substate) of
		{ok, NewState} ->
			agent:set_state(Apid, oncall, State#state.callrec),
			{reply, ok, State#state{substate = NewState, ringout = false}};
		{error, Reason, NewState} ->
			{reply, invalid, State#state{substate = NewState}}
	end;
handle_call(Request, From, #state{callback = Callback} = State) ->
	case Callback:handle_call(Request, From, State#state.substate) of
		{reply, Reply, NewState} ->
			{reply, Reply, State#state{substate = NewState}};
		{reply, Reply, Newstate, Timeout}  ->
			{reply, Reply, State#state{substate = Newstate}, Timeout};
		{noreply, NewState} ->
			{noreply, State#state{substate = NewState}};
		{noreply, NewState, Timeout} ->
			{noreply, State#state{substate = NewState}, Timeout};
		{stop, Reason, Reply, NewState} ->
			{stop, Reason, Reply, State#state{substate = NewState}};
		{stop, Reason, NewState} ->
			{stop, Reason, State#state{substate = NewState}}
	end.
	
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%--------------------------------------------------------------------

handle_cast('$gen_media_agent_oncall', #state{callback = Callback} = State) ->
	?INFO("Agent going oncall", []),
	case State#state.agent_pid of
		undefined ->
			?WARNING("No agent to set state to",[]),
			{noreply, State};
		Apid when is_pid(Apid) ->
			case Callback:handle_answer(Apid, State#state.callrec, State#state.substate) of
				{ok, Newstate} ->
					agent:set_state(State#state.agent_pid, oncall, State#state.callrec),
					{noreply, State#state{substate = Newstate, ringout=false}};
				{error, Reason, Newstate} ->
					?WARNING("Counld not set agent to on call due to ~p", [Reason]),
					{noreply, State#state{substate = Newstate}}
			end
	end;
handle_cast(Msg, #state{callback = Callback} = State) ->
	case Callback:handle_cast(Msg, State#state.substate) of
		{noreply, NewState} ->
			{noreply, State#state{substate = NewState}};
		{noreply, NewState, Timeout} ->
			{noreply, State#state{substate = NewState}, Timeout};
		{stop, Reason, NewState} ->
			{stop, Reason, State#state{substate = NewState}}
	end.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_info({'$gen_media_stop_ring', _Cook}, #state{agent_pid = undefined} = State) ->
	?NOTICE("No agent is ringing for this call", []),
	{noreply, State};
handle_info({'$gen_media_stop_ring', _Cook}, #state{ringout = false} = State) ->
	?NOTICE("Ringout is set not to be handled", []),
	{noreply, State};
handle_info({'$gen_media_stop_ring', Cook}, #state{agent_pid = Apid, callback = Callback, ringout=true} = State) when is_pid(Apid) ->
	?INFO("Handling ringout...", []),
	agent:set_state(Apid, idle),
	gen_server:cast(Cook, stop_ringing),
	{ok, Newsub} = Callback:handle_ring_stop(State#state.substate),
	{noreply, State#state{substate = Newsub, ringout = false}};
handle_info(Info, #state{callback = Callback} = State) ->
	case Callback:handle_info(Info, State#state.substate) of
		{noreply, NewState} ->
			{noreply, State#state{substate = NewState}};
		{noreply, NewState,  Timeout} ->
			{noreply, State#state{substate = NewState}, Timeout};
		{stop, Reason, NewState} ->
			{stop, Reason, State#state{substate = NewState}}
	end.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(Reason, #state{callback = Callback} = State) ->
	Callback:terminate(Reason, State#state.substate).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, #state{callback = Callback} = State, Extra) ->
	{ok, Newsub} = Callback:code_change(OldVsn, State#state.substate, Extra),
    {ok, State#state{substate = Newsub}}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-ifdef(EUNIT).



-endif.
