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
%%	Micah Warren <micahw at fusedsolutions dot com>
%%

%% @doc Behaviour module for media types in the Spice-Telephony software.
%% Gen_media uses gen_server as the underlying framework for what it does.
%% It has a few specific call, cast, and info callbacks it implements, 
%% everything else it passes to it's callback module to process.  Replies from 
%% handle_call, handle_cast, and handle_info are extended to allow for specific 
%% events only media would need.
%%
%%	Callback functions:
%%
%%	<b>init(Args) -> {ok, {State, Route_hint}}</b>
%%		types:  Args = any()
%%				State = any()
%%				Route_hint = {Queue, #call{}} | undefined | #call{}
%%					Queue = string()
%%
%%		When gen_media starts, this function is called.  It should initialize
%%		all required data.
%%
%%		Some media may not be able to provide a call record on start-up, thus
%%		allowing the media to finish prepping and then queue later.
%%
%%	<b>handle_ring(Agent, Call, State) -> Result</b>
%%		types:	Agent = pid()
%%				Call = #call{}
%%				State = any()
%%				Result = {ok, NewState} | {ok, UrlOptions, NewState} | 
%%					{invalid, NewState}
%%					UrlOptions = [{string(), string()}]
%%					NewState = any()
%%
%%		When a call must ring to an agent (either due to out of queue or the 
%%		start of a transfer), this is called.
%%
%%		Agent is the pid of the agent that will be set to ringing if Result is
%%		{ok, NewState}.
%%
%%		Call is the #call{} maintained by the gen_media and passed in for 
%%		Reference.
%%		
%%		State is the internal state of the gen_media callbacks.
%%
%%		If Result is {ok, NewState} or {ok, UrlOptions, NewState}, Agent is set 
%%		to ringing, and execution continues with NewState.  A url_pop is sent to
%%		the agent is the client for the media is set to have one.  In the case
%%		of {ok, UrlOptions, NewState}, the UrlOptions are appened to the url as
%%		a query (get) string.
%%
%%		If Result is {invalid, NewState}, Agent is set to idle, and execution
%%		continues with NewState.
%%
%%	<b>handle_ring_stop(Call, State) -> Result</b>
%%		types:	Call = #call{}
%%				State = any()
%%				Result = {ok, NewState}
%%
%%		When an agent should no longer be ringing, such as due to ringout, this
%%		function is called.
%%
%%		State is the internal state of the gen_media callbacks.
%%
%%		Execution will continue with NewState.
%%
%%	<b>handle_answer(Agent, Call, State) -> Result</b>
%%		types:	Agent = pid()
%%				Call = #call{}
%%				State = any()
%%				Result = {ok, NewState} | {error, Error, NewState}
%%					Error = any()
%%
%%		When an agent should be placed on call after ringing, this function
%%		is called.
%%
%%		Agent is the agent that will be set oncall if Result is {ok, NewState}.
%%
%%		Call is the #call{} the agent will be answering.
%%
%%		State is the internal state of the gen_media callbacks.
%%
%%		If Result is {ok, NewState} and the callpath is inband, it is assumed 
%%		the agent has already set themselves oncall.  If it is out of band,
%%		the agent is set to oncall.  The callback module can always safely
%%		assume the agent is oncall.  Execution then continues with NewState.
%%
%%		If Result is {error, Error, NewState}, the agent's state is not changed
%%		And execution continues with NewState.
%%
%%	<b>handle_voicemail(Ringing, Call, State) -> Result</b>
%%		types:	Ringing = undefined | pid()
%%				Call = #call{}
%%				State = any()
%%				Result = {ok, NewState} | {invalid, NewState}
%%
%%		When a media should be removed from queue and moved to voicemail, this
%%		is called.
%%
%%		State is the internal state of the gen_media callbacks.
%%
%%		Ringing is the pid of the agent ringing with the media, or undefined if
%%		there is no agent.
%%
%%		If Result is {ok, NewState}, the call is removed from queue and 
%%		execution continues with NewState.
%%
%%		If Result is {invalid, NewState} execution continues with NewState.
%%
%%	<b>handle_annouce(Announce, Call, State) -> {ok, NewState}</b>
%%		types:	Announce = any()
%%				Call = any()
%%				State = NewState = any()
%%
%%		When a recipe calls for a call in queue to play an announcement, this
%%		function is called.  Execution then continues with NewState.
%%
%%	<b>handle_agent_transfer(Agent, Call, Timeout, State) -> Result</b>
%%		types:	Agent = pid()
%%				Call = #call{}
%%				Timeout = pos_integer()
%%				State = any()
%%				Result = {ok, NewState} | {error, Error, NewState}
%%					NewState = State = any()
%%					Error = any()
%%
%%		When a media should be transfered to another agent, this is one of the
%%		first step.  The target agent is set to ringing, then this callback
%%		is used to verify that.  If the callback returns {ok, NewState}, 
%%		execution continues with NewState, and gen_media handles with oncall or
%%		a ringout.
%%
%%	<b>handle_queue_transfer(Call, State) -> {ok, NewState}</b>
%%		types:	Call = #call{}
%%				State = NewState = any()
%%
%%		When a media is placed back into queue from an agent, this is called
%%		To allow the media to do any required clean up or unbridging.  The 
%%		Call is requeued at the priority it was initially queued at.
%%		Execution then continues with NewState.
%%
%%	<b>handle_wrapup(Call, State) -> {Finality, NewState}</b>
%%		types:	Call = #call{}
%%				State = NewState = any()
%%				Finality = ok | hangup
%%
%%		This callback is only used if the call record's media path is inband.
%%		When an agent goes to wrapup, this gives the callback a chance to do any
%%		clean-up needed.  
%%
%%		If the media determines this is a hang-up (ie, no more
%%		can be done with the media), it can return {hangup, NewState}.  The
%%		gen_media then terminates with state NewState.
%%
%%		If {ok, NewState} is returned, execution continues with state NewState.
%%
%%	<b>handle_spy(Spy, Call, State) -> {ok, NewState} | {invalid, NewState} | {error, Error, NewState}</b>
%%		types:  Call = #call{}
%%				State = NewState = any()
%%				Spy = pid()
%%
%%		This callback is only valid when the Spy is released and there is an
%%		Agent oncall with the call.  This signals the callback that a supervisor
%%		is attempting to observe the agent that is oncall.  The other callbacks
%%		Should take into account the possibility of a spy if 'ok' is returned.
%%		
%%		Be aware that when calling this, gen_media does not have a reliable 
%%		to determine an agent's security level.  The agent connecitons, however,
%%		do.
%%
%%	<b>Extended gen_server Callbacks</b>
%%
%%	In addition to the usual replies gen_server expects from it's callbacks of
%%	handle_call/3, handle_cast/2, and handle_info/2, gen_media will take some 
%%	action based on the following Returns.
%%
%%	{queue, Queue, Callrec, NewState}
%%		types:  Queue = string()
%%				Callrec = #call{}
%%				NewState = any()
%%
%%		This result is only valid if the callbacks init/1 returned undefined
%%		for the call record.  This sets the call record and queues the call.
%%		Execution then continues on with NewState.  If this is replied to a 
%%		call, ok is set as the reply.
%%
%%	{outbound, Agent, NewState}
%%	{outbound, Agent, Call, NewState}
%%		types:  Agent = pid()
%%				Call = #call{}
%%				NewState = any()
%%
%%		This result is valid only if the call is not queued.  The second form
%%		is only valid if init/1 retuned an undefined call.  This also assumes 
%%		the agent at pid() is already in precall state.  If The agent can be set
%%		to outgoing, it will be.  Execution continues on with NewState.
%%
%%	{voicemail, NewState}
%%		types:	NewState = any()
%%
%%		This result is valid only if the call is queued.  Removes the media from
%%		queue and stops ringing to an agent it is.  Assumes the media has already
%%		done what it needs to for a voicemail.  If done in a handle_call, the
%%		reply is 'ok'.
%%
%%	{Agentaction, NewState}
%%	{Agentaction, Reply, NewState}
%%		types:	Agentaction = stop_ring | {stop_ring, Data} | wrapup | hangup | 
%%					{mediapush, Data, Mode}
%%				Reply = any()
%%				NewState = any()
%%				Data = any()
%%				Mode = replace | append
%%
%%		This result is only valid if an agent has been associated with this 
%%		media by ringing.  The second form is only valid if the request came in
%%		as a gen_media:call.  This attempts to take the specified action on the 
%%		agent, the continues execution with NewState.
%%
%%		{stop_ring, Data} is used to stop the gen_media from handling a ringout.  It 
%%		does not change the agent's state.  Execution will continue with 
%%		NewState.  This is useful if there is an error ringing to an agent that
%%		only becomes apparent at a later time.  A return of stop_ring is
%%		Equivalent to {stop_ring, undefined}.
%%
%%		wrapup is only valid if there is an agent associated with a media, and
%%		that agent is oncall or outgoing.  This sets the agent to wrapup and
%%		continues execution with NewState.
%%
%%		hangup is valid at any time an agent is (or was) associated a call.
%%		It is useful when the remote party of a media disconnects, as the 
%%		callback does not have any knowledge of what state an agent is in.
%%		This determine's the agent's state, then set's the agent's state 
%%		apropriately.  Execution then coninues with NewState.
%%
%%		mediapush is only valid if there is an agent oncall with the media, and
%%		the media is inband.  The given Data is casted to the associaed agent 
%%		as a media push.
%%
%%	{stop, hangup, NewState}
%%		types:  NewState = any()
%%
%%		This causes the media to take any action is would from an
%%		Agentaction return tuple of hangup, then stop.

-module(gen_media).
-author(micahw).

-behaviour(gen_server).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").

-ifdef(TEST).
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
	agent_transfer/3,
	warm_transfer_begin/2,
	warm_transfer_cancel/1,
	warm_transfer_complete/1,
	queue/2,
	call/2,
	call/3,
	cast/2,
	wrapup/1,
	spy/2
]).

% TODO - add these to a global .hrl, cpx perhaps?
-type(tref() :: any()).
-type(proplist_item() :: atom() | {any(), any()}).
-type(proplist() :: [proplist_item()]).

-record(state, {
	callback :: atom(),
	substate :: any(),
	callrec :: #call{},
	ring_pid :: 'undefined' | pid(),
	oncall_pid :: 'undefined' | pid(),
	queue_failover = true :: 'true' | 'false',
	queue_pid :: 'undefined' | pid(),
	ringout = false:: tref() | 'false',
	outband_ring_pid :: 'undefined' | pid(),
	warm_transfer = false :: boolean()
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

-define(MEDIA_ACTIONS, [ring_agent, get_call, start_cook, voicemail, announce, stop_cook]).

-spec(behaviour_info/1 :: 
	(Info :: 'callbacks' | any()) -> [{atom(), non_neg_integer()}] | 'undefined').
behaviour_info(callbacks) ->
	[
		{handle_ring, 3},
		{handle_ring_stop, 2},
		{handle_answer, 3}, 
		%{handle_voicemail, 3}, 
		%{handle_announce, 3}, 
		{handle_agent_transfer, 4},
		{handle_queue_transfer, 2},
%		{handle_warm_transfer_begin, 3},
%		{handle_warm_transfer_cancel, 2},
%		{handle_warm_transfer_complete, 2},
		{handle_wrapup, 2},
		{handle_call, 4},
		{handle_cast, 3},
		{handle_info, 3},
		{terminate, 3},
		{code_change, 4}
	];
behaviour_info(_Other) ->
    undefined.

%% @doc Make the `pid() Genmedia' ring to `pid() Agent' based off of
%% `#queued_call{} Qcall' with a ringout of `pos_integer() Timeout' miliseconds.
-spec(ring/4 :: (Genmedia :: pid(), Agent :: pid(), Qcall :: #queued_call{}, Timeout :: pos_integer())  -> 'ok' | 'invalid').
ring(Genmedia, Agent, Qcall, Timeout) ->
	gen_server:call(Genmedia, {'$gen_media_ring', Agent, Qcall, Timeout}).

%% @doc Get the call record associated with `pid() Genmedia'.
-spec(get_call/1 :: (Genmedia :: pid()) -> #call{}).
get_call(Genmedia) ->
	gen_server:call(Genmedia, '$gen_media_get_call').

%% @doc Send the passed `pid() Genmedia' to voicemail.
-spec(voicemail/1 :: (Genmedia :: pid()) -> 'ok' | 'invalid').
voicemail(Genmedia) ->
	gen_server:call(Genmedia, '$gen_media_voicemail').

%% @doc Pass `any() Annouce' message to `pid() Genmedia'.
-spec(announce/2 :: (Genmedia :: pid(), Annouce :: any()) -> 'ok').
announce(Genmedia, Annouce) ->
	gen_server:call(Genmedia, {'$gen_media_announce', Annouce}).

%% @doc Sends the oncall agent associated with the call to wrapup; or, if it's
%% the oncall agent making the request, gives the callback module a chance to
%% handle it.
-spec(wrapup/1 :: (Genmedia :: pid()) -> 'ok' | 'invalid').
wrapup(Genmedia) ->
	gen_server:call(Genmedia, '$gen_media_wrapup').

%% @doc Send a stop ringing message to `pid() Genmedia'.
-spec(stop_ringing/1 :: (Genmedia :: pid()) -> 'ok').
stop_ringing(Genmedia) ->
	Self = self(),
	Genmedia ! {'$gen_media_stop_ring', Self},
	ok.

%% @doc Set the agent associated with `pid() Genmedia' to oncall.
-spec(oncall/1 :: (Genmedia :: pid()) -> 'ok' | 'invalid').
oncall(Genmedia) ->
	gen_server:call(Genmedia, '$gen_media_agent_oncall').

%% @doc Transfer the call from the agent it is associated with to a new agent.
-spec(agent_transfer/3 :: (Genmedia :: pid(), Apid :: pid(), Timeout :: pos_integer()) -> 'ok' | 'invalid').
agent_transfer(Genmedia, Apid, Timeout) ->
	gen_server:call(Genmedia, {'$gen_media_agent_transfer', Apid, Timeout}).

-spec(warm_transfer_begin/2 :: (Genmedia :: pid(), Number :: string()) -> 'ok' | 'invalid').
warm_transfer_begin(Genmedia, Number) ->
	gen_server:call(Genmedia, {'$gen_media_warm_transfer_begin', Number}).

-spec(warm_transfer_cancel/1 :: (Genmedia :: pid()) -> 'ok' | 'invalid').
warm_transfer_cancel(Genmedia) ->
	gen_server:call(Genmedia, '$gen_media_warm_transfer_cancel').

-spec(warm_transfer_complete/1 :: (Genmedia :: pid()) -> 'ok' | 'invalid').
warm_transfer_complete(Genmedia) ->
	gen_server:call(Genmedia, '$gen_media_warm_transfer_complete').

%% @doc Transfer the passed media into the given queue.
-spec(queue/2 :: (Genmedia :: pid(), Queue :: string()) -> 'ok' | 'invalid').
queue(Genmedia, Queue) ->
	gen_server:call(Genmedia, {'$gen_media_queue', Queue}).
	
%% @doc Attempt to spy on the agent oncall with the given media.
-spec(spy/2 :: (Genmedia :: pid(), Spy :: pid()) -> 'ok' | 'invalid' | {'error', any()}).
spy(Genmedia, Spy) ->
	gen_server:call(Genmedia, {'$gen_media_spy', Spy}).

%% @doc Do the equivalent of a `gen_server:call/2'.
-spec(call/2 :: (Genmedia :: pid(), Request :: any()) -> any()).
call(Genmedia, Request) ->
	gen_server:call(Genmedia, Request).

%% @doc Do the equivalent of `gen_server:call/3'.
-spec(call/3 :: (Genmedia :: pid(), Request :: any(), Timeout :: pos_integer()) -> any()).
call(Genmedia, Request, Timeout) ->
	gen_server:call(Genmedia, Request, Timeout).

%% @doc Do the equivalent of `gen_server:cast/2'.
-spec(cast/2 :: (Genmedia :: pid(), Request:: any()) -> 'ok').
cast(Genmedia, Request) ->
	gen_server:cast(Genmedia, Request).

%%====================================================================
%% API
%%====================================================================

%% @doc Start a gen_media linked to the calling process.
-spec(start_link/2 :: (Callback :: atom(), Args :: any()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start_link(Callback, Args) ->
	gen_server:start_link(?MODULE, [Callback, Args], []).

-spec(start/2 :: (Callback :: atom(), Args :: any()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start(Callback, Args) ->
	gen_server:start(?MODULE, [Callback, Args], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init([Callback, Args]) ->
	case Callback:init(Args) of
		{ok, {Substate, undefined}} ->
		    {ok, #state{callback = Callback, substate = Substate, callrec = undefined}};
		{ok, {Substate, {Queue, PCallrec}}} when is_record(PCallrec, call) ->
			Callrec = correct_client(PCallrec),
			cdr:cdrinit(Callrec),
			Qpid = case priv_queue(Queue, Callrec, true) of
				invalid when Queue =/= "default_queue" ->
					priv_queue("default_queue", Callrec, true),
					set_cpx_mon(#state{callrec = Callrec}, [{queue, "default_queue"}]),
					cdr:inqueue(Callrec, "default_queue");
				{default, Pid} ->
					set_cpx_mon(#state{callrec = Callrec}, [{queue, "default_queue"}]),
					cdr:inqueue(Callrec, "default_queue"),
					Pid;
				Else ->
					cdr:inqueue(Callrec, Queue),
					set_cpx_mon(#state{callrec = Callrec}, [{queue, Queue}]),
					Else
			end,
			{ok, #state{callback = Callback, substate = Substate, callrec = Callrec#call{source = self()}, queue_pid = Qpid}};
		{ok, {Substate, PCallrec, {CDRState, CDRArgs}}} when is_record(PCallrec, call) ->
			Callrec = correct_client(PCallrec),
			cdr:cdrinit(Callrec),
			apply(cdr, CDRState, [Callrec | CDRArgs]),
			set_cpx_mon(#state{callrec = Callrec}, []),
			{ok, #state{callback = Callback, substate = Substate, callrec = Callrec#call{source = self()}}};
		{ok, {Substate, PCallrec}} when is_record(PCallrec, call) ->
			Callrec = correct_client(PCallrec),
			cdr:cdrinit(Callrec),
			set_cpx_mon(#state{callrec = Callrec}, []),
			{ok, #state{callback = Callback, substate = Substate, callrec = Callrec#call{source = self()}}};
		{stop, Reason} = O ->
			?WARNING("init aborted due to ~p", [Reason]),
			O;
		ignore ->
			?WARNING("init told to ignore", []),
			ignore
	end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------

%% @private
handle_call({'$gen_media_spy', Spy}, _From, #state{oncall_pid = Spy} = State) ->
	%% Can't spy on yourself.
	?DEBUG("Can't spy on yourself", []),
	{reply, invalid, State};
handle_call({'$gen_media_spy', Spy}, {Spy, _Tag}, #state{callback = Callback, oncall_pid = Ocpid, callrec = Call} = State) when is_pid(Ocpid) ->
	case erlang:function_exported(Callback, handle_spy, 3) of
		false ->
			?DEBUG("Callback ~p doesn't support spy for ~p", [Callback, Call#call.id]),
			{reply, invalid, State};
		true ->
			case Callback:handle_spy(Spy, Call, State#state.substate) of
				{ok, Newstate} ->
					{reply, ok, State#state{substate = Newstate}};
				{invalid, Newstate} ->
					{reply, invalid, State#state{substate = Newstate}};
				{error, Error, Newstate} ->
					?INFO("Callback ~p errored ~p on spy for ~p", [Callback, Error, Call#call.id]),
					{reply, {error, Error}, State#state{substate = Newstate}}
			end
	end;
handle_call({'$gen_media_spy', _Spy}, _From, State) ->
	{reply, invalid, State};
handle_call('$gen_media_wrapup', {Ocpid, _Tag}, #state{callback = Callback, oncall_pid = Ocpid, callrec = Call} = State) when Call#call.media_path =:= inband ->
	?INFO("Request to end call ~p from agent", [Call#call.id]),
	cdr:wrapup(State#state.callrec, Ocpid),
	case Callback:handle_wrapup(State#state.callrec, State#state.substate) of
		{ok, NewState} ->
			{reply, ok, State#state{oncall_pid = undefined, substate = NewState}};
		{hangup, NewState} ->
			cdr:hangup(State#state.callrec, agent),
			{stop, normal, ok, State#state{oncall_pid = undefined, substate = NewState}}
	end;
handle_call('$gen_media_wrapup', {Ocpid, _Tag}, #state{oncall_pid = Ocpid, callrec = Call} = State) ->
	?ERROR("Cannot do a wrapup directly unless mediapath is inband, and request is from agent oncall. ~p", [Call#call.id]),
	{reply, invalid, State};
handle_call({'$gen_media_queue', Queue}, {Ocpid, _Tag}, #state{callback = Callback, callrec = Call, oncall_pid = Ocpid} = State) ->
	?INFO("request to queue call ~p from agent", [Call#call.id]),
	% TODO calls that were previously handled by an agent should get their priority bumped?
	case priv_queue(Queue, State#state.callrec, State#state.queue_failover) of
		invalid ->
			{reply, invalid, State};
		{default, Qpid} ->
			{ok, NewState} = Callback:handle_queue_transfer(State#state.callrec, State#state.substate),
			cdr:queue_transfer(State#state.callrec, "default_queue"),
			cdr:inqueue(State#state.callrec, "default_queue"),
			cdr:wrapup(State#state.callrec, Ocpid),
			set_cpx_mon(State#state{substate = NewState, oncall_pid = undefined}, [{queue, "default_queue"}]),
			{reply, ok, State#state{substate = NewState, oncall_pid = undefined, queue_pid = Qpid}};
		Qpid when is_pid(Qpid) ->
			{ok, NewState} = Callback:handle_queue_transfer(State#state.callrec, State#state.substate),
			cdr:queue_transfer(State#state.callrec, Queue),
			cdr:inqueue(State#state.callrec, Queue),
			cdr:wrapup(State#state.callrec, Ocpid),
			set_cpx_mon(State#state{substate = NewState, oncall_pid = undefined}, [{queue, Queue}]),
			{reply, ok, State#state{substate = NewState, oncall_pid = undefined, queue_pid = Qpid}}
	end;
handle_call({'$gen_media_queue', Queue}, From, #state{callback = Callback, callrec = Call} = State) when is_pid(State#state.oncall_pid) ->
	% TODO calls that were previously handled by an agent should get their priority bumped?
	?INFO("Request to queue ~p from ~p", [Call#call.id, From]),
	case priv_queue(Queue, State#state.callrec, State#state.queue_failover) of
		invalid ->
			{reply, invalid, State};
		{default, Qpid} ->
			set_agent_state(State#state.oncall_pid, [wrapup, State#state.callrec]),
			{ok, NewState} = Callback:handle_queue_transfer(State#state.callrec, State#state.substate),
			cdr:queue_transfer(State#state.callrec, "default_queue"),
			cdr:inqueue(State#state.callrec, "default_queue"),
			cdr:wrapup(State#state.callrec, State#state.oncall_pid),
			set_cpx_mon(State#state{substate = NewState, oncall_pid = undefined}, [{queue, "default_queue"}]),
			{reply, ok, State#state{substate = NewState, oncall_pid = undefined, queue_pid = Qpid}};
		Qpid when is_pid(Qpid) ->
			set_agent_state(State#state.oncall_pid, [wrapup, State#state.callrec]),
			{ok, NewState} = Callback:handle_queue_transfer(State#state.callrec, State#state.substate),
			cdr:queue_transfer(State#state.callrec, Queue),
			cdr:inqueue(State#state.callrec, Queue),
			cdr:wrapup(State#state.callrec, State#state.oncall_pid),
			set_cpx_mon(State#state{substate = NewState, oncall_pid = undefined}, [{queue, Queue}]),
			{reply, ok, State#state{substate = NewState, oncall_pid = undefined, queue_pid = Qpid}}
	end;
handle_call('$gen_media_get_call', _From, State) ->
	{reply, State#state.callrec, State};
handle_call({'$gen_media_ring', Agent, #queued_call{cook = Requester} = QCall, Timeout}, {Requester, _Tag}, #state{callrec = Call, callback = Callback, ring_pid = undefined} = State) ->
	?INFO("Trying to ring ~p with ~p with timeout ~p", [Agent, Call#call.id, Timeout]),
	case set_agent_state(Agent, [ringing, Call#call{cook=QCall#queued_call.cook}]) of
		ok ->
			case Callback:handle_ring(Agent, State#state.callrec, State#state.substate) of
				Success when element(1, Success) == ok ->
					Popopts = case Success of
						{ok, Substate} ->
							[];
						{ok, Opts, Substate} ->
							Opts
					end,
					{ok, Tref} = timer:send_after(Timeout, {'$gen_media_stop_ring', QCall#queued_call.cook}),
					cdr:ringing(Call, Agent),
					url_pop(Call, Agent, Popopts),
					Newcall = Call#call{cook = QCall#queued_call.cook},
					Arec = agent:dump_state(Agent),
					Outbandringpid = case {Arec#agent.defaultringpath, Call#call.ring_path, whereis(freeswitch_media_manager)} of
						{outband, inband, Pid} when is_pid(Pid) ->
							case freeswitch_media_manager:ring_agent(Agent, Arec, Call, Timeout) of
								{ok, RingChanPid} ->
									RingChanPid;
								Else ->
									?WARNING("Failed to do out of band ring:  ~p for ~p", [Else, Call#call.id]),
									undefined
							end;
						_ ->
							undefined
					end,
					{reply, ok, State#state{substate = Substate, ring_pid = Agent, ringout=Tref, callrec = Newcall, outband_ring_pid = Outbandringpid}};
				{invalid, Substate} ->
					set_agent_state(Agent, [released, {"Ring Fail", "Ring Fail", -1}]),
					{reply, invalid, State#state{substate = Substate}}
			end;
		Else ->
			?INFO("Agent ~p ringing response:  ~p for ~p", [Agent, Else, Call#call.id]),
			{reply, invalid, State}
	end;
handle_call({'$gen_media_ring', Agent, QCall, Timeout}, From, #state{callrec = Call, callback = Callback, ring_pid = undefined} = State) ->
	gen_server:cast(QCall#queued_call.cook, {ring_to, Agent, QCall}),
	{reply, deferred, State};
handle_call({'$gen_media_ring', Agent, QCall, Timeout}, From, #state{callrec = Call, callback = Callback, ring_pid = Rpid} = State) ->
	?NOTICE("Changing ring from ~p to ~p for ~p", [Rpid, Agent, Call#call.id]),
	Cook = QCall#queued_call.cook,
	{noreply, Midstate} = handle_info({'$gen_media_stop_ring', Cook}, State),
	handle_call({'$gen_media_ring', Agent, QCall, Timeout}, From, Midstate);

	
handle_call({'$gen_media_agent_transfer', Apid}, _From, #state{oncall_pid = Apid, callrec = Call} = State) ->
	?NOTICE("Can't transfer to yourself, silly ~p! ~p", [Apid, Call#call.id]),
	{reply, invalid, State};
handle_call({'$gen_media_agent_transfer', Apid, Timeout}, _From, #state{callrec = Call, callback = Callback, ring_pid = undefined, oncall_pid = Ocpid} = State) when is_pid(Ocpid) ->
	case set_agent_state(Apid, [ringing, State#state.callrec]) of
		ok ->
			case Callback:handle_agent_transfer(Apid, Timeout, State#state.callrec, State#state.substate) of
				Success when element(1, Success) == ok ->
					Popopts = case Success of
						{ok, Substate} ->
							[];
						{ok, Opts, Substate} ->
							Opts
					end,
					{ok, Tref} = timer:send_after(Timeout, {'$gen_media_stop_ring', dummy}),
					cdr:agent_transfer(State#state.callrec, {Ocpid, Apid}),
					cdr:ringing(State#state.callrec, Apid),
					url_pop(Call, Apid, Popopts),
					{reply, ok, State#state{ring_pid = Apid, ringout = Tref, substate = Substate}};
				{error, Error, NewState} ->
					?NOTICE("Could not set agent ringing for transfer ~p due to ~p", [Error, Call#call.id]),
					set_agent_state(Apid, [idle]),
					{reply, invalid, State#state{substate = NewState}}
			end;
		invalid ->
			?NOTICE("Could not ring ~p to target agent ~p", [Call#call.id, Apid]),
			{reply, invalid, State}
	end;
handle_call({'$gen_media_agent_transfer', _Apid, _Timeout}, _From, #state{callrec = Call, ring_pid = Rpid} = State) ->
	?ERROR("Invalid agent transfer for ~p sent when no agent oncall (ring_pid:  ~p)", [Call#call.id, Rpid]),
	{reply, invalid, State};
handle_call({'$gen_media_warm_transfer_begin', Number}, _From, #state{callback = Callback, oncall_pid = Apid, callrec = Call} = State) when is_pid(Apid) ->
	case erlang:function_exported(Callback, handle_warm_transfer_begin, 3) of
		true ->
			case Callback:handle_warm_transfer_begin(Number, Call, State#state.substate) of
				{ok, UUID, NewState} ->
					Res = set_agent_state(Apid, [warmtransfer, UUID]),
					cdr:warmxfer_begin(State#state.callrec, {Apid, Number}),
					{reply, Res, State#state{substate = NewState, warm_transfer = true}};
				{error, Error, NewState} ->
					?DEBUG("Callback module ~w errored for warm transfer begin:  ~p for ~p", [Callback, Error, Call#call.id]),
					{reply, invalid, State#state{substate = NewState}}
			end;
		false ->
			{reply, invalid, State}
	end;
handle_call('$gen_media_warm_transfer_cancel', _From, #state{callback = Callback, oncall_pid = Apid, callrec = Call} = State) when is_pid(Apid) ->
	case erlang:function_exported(Callback, handle_warm_transfer_cancel, 2) of
		true ->
			case Callback:handle_warm_transfer_cancel(Call, State#state.substate) of
				{ok, NewState} ->
					Res = set_agent_state(Apid, [oncall, Call]),
					cdr:warmxfer_cancel(Call, Apid),
					#agent{login = Agent} = agent:dump_state(Apid),
					cdr:oncall(Call, Agent),
					{reply, Res, State#state{substate = NewState, warm_transfer = false}};
				{error, Error, NewState} ->
					?DEBUG("Callback module ~w errored for warm transfer cancel:  ~p for ~p", [Callback, Error, Call#call.id]),
					{reply, invalid, State#state{substate = NewState}}
			end;
		false ->
			{reply, invalid, State}
	end;
handle_call('$gen_media_warm_transfer_complete', _From, #state{callback = Callback, oncall_pid = Apid, callrec = Call} = State) when is_pid(Apid) ->
	case erlang:function_exported(Callback, handle_warm_transfer_complete, 2) of
		true ->
			case Callback:handle_warm_transfer_complete(Call, State#state.substate) of
				{ok, NewState} ->
					Res = set_agent_state(Apid, [wrapup, Call]),
					cdr:warmxfer_complete(Call, Apid),
					#agent{login = Agent} = agent:dump_state(Apid),
					cdr:wrapup(Call, Agent),
					{reply, Res, State#state{substate = NewState, oncall_pid = undefined}};
				{error, Error, NewState} ->
					?DEBUG("Callback module ~w errored for warm transfer complete:  ~p for ~p", [Callback, Error, Call#call.id]),
					{reply, invalid, State#state{substate = NewState}}
			end;
		false ->
			{reply, invalid, State}
	end;
handle_call({'$gen_media_announce', Annouce}, _From, #state{callback = Callback, substate = InSubstate, callrec = Call} = State) ->
	?INFO("Doing announce for ~p", [Call#call.id]),
	Substate = case erlang:function_exported(Callback, handle_announce, 3) of
		true ->
			{ok, N} = Callback:handle_announce(Annouce, Call, InSubstate),
			N;
		false ->
			State#state.substate
	end,
	{reply, ok, State#state{substate = Substate}};
handle_call('$gen_media_voicemail', _From, #state{callback = Callback, callrec = Call} = State) when is_pid(State#state.queue_pid) ->
	?INFO("trying to send media ~p to voicemail", [Call#call.id]),
	case erlang:function_exported(Callback, handle_voicemail, 3) of
		false ->
			{reply, invalid, State};
		true ->
			case  Callback:handle_voicemail(State#state.ring_pid, Call, State#state.substate) of
				{ok, Substate} ->
					priv_voicemail(State),
					{reply, ok, State#state{substate = Substate, queue_pid = undefined, ring_pid = undefined}};
				{invalid, Substate} ->
					{reply, invalid, State#state{substate = Substate}}
			end
	end;
handle_call('$gen_media_voicemail', _From, #state{queue_pid = undefined, callrec = Call} = State) ->
	?ERROR("voicemail only valid when the media ~p is queued", [Call#call.id]),
	{reply, invalid, State};
handle_call('$gen_media_agent_oncall', {Apid, _Tag}, #state{ring_pid = Apid, callrec = #call{ring_path = outband} = Call} = State) ->
	?INFO("Cannot accept on call requests from agent (~p) unless ring_path is inband for ~p", [Apid, Call#call.id]),
	{reply, invalid, State};
handle_call('$gen_media_agent_oncall', {Rpid, _Tag}, #state{ring_pid = Rpid, callback = Callback, oncall_pid = Ocpid, callrec = #call{ring_path = inband} = Call} = State) when is_pid(Ocpid) ->
	?INFO("oncall request during what looks like an agent transfer (inband) for ~p", [Call#call.id]),
	case Callback:handle_answer(Rpid, Call, State#state.substate) of
		{ok, NewState} ->
			kill_outband_ring(State),
			cdr:oncall(Call, Rpid),
			timer:cancel(State#state.ringout),
			set_agent_state(Ocpid, [wrapup, Call]),
			cdr:wrapup(Call, Ocpid),
			Agent = agent_manager:find_by_pid(Rpid),
			set_cpx_mon(State#state{substate = NewState, ringout = false, oncall_pid = Rpid, ring_pid = undefined}, [{agent, Agent}]),
			{reply, ok, State#state{substate = NewState, ringout = false, oncall_pid = Rpid, ring_pid = undefined, outband_ring_pid = undefined}};
		{error, Reason, NewState} ->
			?ERROR("Cannot set ~p for ~p to oncall due to ~p", [Rpid, Call#call.id, Reason]),
			{reply, invalid, State#state{substate = NewState}}
	end;
handle_call('$gen_media_agent_oncall', _From, #state{warm_transfer = true, callrec = Call, oncall_pid = undefined} = State) ->
	?INFO("stray oncall request during what looks like a warm transfer complete (outofband) for ~p", [Call#call.id]),
	{reply, ok, State};
handle_call('$gen_media_agent_oncall', _From, #state{warm_transfer = true, callrec = Call} = State) ->
	?INFO("oncall request during what looks like a warm transfer (outofband) for ~p", [Call#call.id]),
	agent:media_push(State#state.oncall_pid, warm_transfer_succeeded),
	{reply, ok, State};
handle_call('$gen_media_agent_oncall', _From, #state{ring_pid = Rpid, callback = Callback, oncall_pid = Ocpid, callrec = Call} = State) when is_pid(Ocpid), is_pid(Rpid) ->
	?INFO("oncall request during what looks like an agent transfer (outofband) to ~p for ~p", [Rpid, Call#call.id]),
	case set_agent_state(Rpid, [oncall, State#state.callrec]) of
		invalid ->
			{reply, invalid, State};
		ok ->
			case Callback:handle_answer(Rpid, State#state.callrec, State#state.substate) of
				{ok, NewState} ->
					kill_outband_ring(State),
					cdr:oncall(State#state.callrec, Rpid),
					timer:cancel(State#state.ringout),
					set_agent_state(Ocpid, [wrapup, State#state.callrec]),
					cdr:wrapup(State#state.callrec, Ocpid),
					Agent = agent_manager:find_by_pid(Rpid),
					set_cpx_mon(State#state{substate = NewState, ringout = false, oncall_pid = Rpid, ring_pid = undefined}, [{agent, Agent}]),
					{reply, ok, State#state{substate = NewState, ringout = false, oncall_pid = Rpid, ring_pid = undefined, outband_ring_pid = undefined}};
				{error, Reason, NewState} ->
					?ERROR("Cannot set ~p to oncall due to ~p for ~p", [Rpid, Reason, Call#call.id]),
					{reply, invalid, State#state{substate = NewState}}
			end
	end;
handle_call('$gen_media_agent_oncall', {Apid, _Tag}, #state{callback = Callback, ring_pid = Apid, callrec = #call{ring_path = inband} = Call} = State) ->
	?INFO("oncall request from agent ~p for ~p", [Apid, Call#call.id]),
	case Callback:handle_answer(Apid, State#state.callrec, State#state.substate) of
		{ok, NewState} ->
			kill_outband_ring(State),
			timer:cancel(State#state.ringout),
			unqueue(State#state.queue_pid, self()),
			cdr:oncall(State#state.callrec, Apid),
			Agent = agent_manager:find_by_pid(Apid),
			set_cpx_mon(State#state{substate = NewState, ringout = false, queue_pid = undefined, ring_pid = undefined, oncall_pid = Apid}, [{agent, Agent}]),
			{reply, ok, State#state{substate = NewState, ringout = false, queue_pid = undefined, ring_pid = undefined, oncall_pid = Apid, outband_ring_pid = undefined}};
		{error, Reason, NewState} ->
			?ERROR("Could not set ~p on call due to ~p for ~p", [Apid, Reason, Call#call.id]),
			{reply, invalid, State#state{substate = NewState}}
	end;

handle_call('$gen_media_agent_oncall', From, #state{ring_pid = Apid, callback = Callback, callrec = Call} = State) when is_pid(Apid) ->
	?INFO("oncall request from ~p; agent to set on call is ~p for ~p", [From, Apid, Call#call.id]),
	case set_agent_state(Apid, [oncall, Call]) of
		invalid ->
			{reply, invalid, State};
		ok ->
			case Callback:handle_answer(Apid, Call, State#state.substate) of
				{ok, NewState} ->
					kill_outband_ring(State),
					cdr:oncall(Call, Apid),
					timer:cancel(State#state.ringout),
					call_queue:remove(State#state.queue_pid, self()),
					Agent = agent_manager:find_by_pid(Apid),
					set_cpx_mon(State#state{substate = NewState, ringout = false, queue_pid = undefined, oncall_pid = Apid, ring_pid = undefined}, [{agent, Agent}]),
					{reply, ok, State#state{substate = NewState, ringout = false, queue_pid = undefined, oncall_pid = Apid, ring_pid = undefined, outband_ring_pid = undefined}};
				{error, Reason, NewState} ->
					?ERROR("Could not set ~p on call with ~p due to ~p", [Apid, Call#call.id, Reason]),
					{reply, invalid, State#state{substate = NewState}}
			end;
		badagent ->
			{ok, NewSubstate} = Callback:handle_ring_stop(State#state.callrec, State#state.substate),
			kill_outband_ring(State),
			cdr:ringout(State#state.callrec, {badagent, Apid}),
			timer:cancel(State#state.ringout),
			Newstate = State#state{substate = NewSubstate, ringout = false, ring_pid = undefined, outband_ring_pid = undefined},
			Callrec = State#state.callrec,
			gen_server:cast(Callrec#call.cook, stop_ringing),
			{reply, invalid, Newstate}
	end;
handle_call('$gen_media_agent_oncall', From, #state{oncall_pid = OcPid, callrec = Call} = State) when is_pid(OcPid) ->
	?INFO("oncall request from ~p for ~p when already oncall, ignoring", [From, Call#call.id]),
	% TODO - is this a bad thing to do? Micah -- please review this clause.
	{reply, ok, State};
handle_call('$gen_media_agent_oncall', From, #state{ring_pid = undefined, callrec = Call} = State) ->
	?INFO("oncall request from ~p for ~p when no ring_pid (probobly a late request)", [From, Call#call.id]),
	{reply, invalid, State};
handle_call(Request, From, #state{callback = Callback} = State) ->
	Reply = Callback:handle_call(Request, From, State#state.callrec, State#state.substate),
	handle_custom_return(Reply, State, reply).
	
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%--------------------------------------------------------------------

%% @private
handle_cast(Msg, #state{callback = Callback} = State) ->
	Reply = Callback:handle_cast(Msg, State#state.callrec, State#state.substate),
	handle_custom_return(Reply, State, noreply).

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%--------------------------------------------------------------------

%% @private
handle_info({'$gen_media_stop_ring', Apid}, #state{ring_pid = Apid, callback = Callback, callrec = Callrec} = State) ->
	?INFO("ring agent ~w requested a stop to ringing for ~p", [Apid, Callrec#call.id]),
	{ok, Newsub} = Callback:handle_ring_stop(State#state.callrec, State#state.substate),
	gen_server:cast(Callrec#call.cook, stop_ringing),
	kill_outband_ring(State),
	cdr:ringout(Callrec, {agent_request, Apid}),
	{noreply, State#state{substate = Newsub, ringout = false, ring_pid = undefined, outband_ring_pid = undefined}};
handle_info({'$gen_media_stop_ring', _Cook}, #state{ring_pid = undefined, callrec = Call} = State) ->
	?NOTICE("No agent is ringing for this call ~p", [Call#call.id]),
	{noreply, State};
handle_info({'$gen_media_stop_ring', _Cook}, #state{ringout = false, callrec = Call} = State) ->
	?NOTICE("Ringout is set not to be handled for ~p", [Call#call.id]),
	{noreply, State};
handle_info({'$gen_media_stop_ring', Cook}, #state{ring_pid = Apid, callback = Callback, callrec = Call} = State) when is_pid(Apid) ->
	?INFO("Handling ringout... for ~p", [Call#call.id]),
	Reason = try agent:query_state(Apid) of
		{ok, ringing} ->
			set_agent_state(Apid, [idle]),
			ringout;
		{ok, released} ->
			released;
		_Else ->
			ringout
	catch
		What:Why ->
			?INFO("getting agent info failed due to ~p:~p for ~p", [What, Why, Call#call.id]),
			ringout
	end,
	gen_server:cast(Cook, stop_ringing),
	{ok, Newsub} = Callback:handle_ring_stop(State#state.callrec, State#state.substate),
	kill_outband_ring(State),
	cdr:ringout(State#state.callrec, {Reason, Apid}),
	{noreply, State#state{substate = Newsub, ringout = false, ring_pid = undefined}};
handle_info(Info, #state{callback = Callback} = State) ->
	%?DEBUG("Other info message, going directly to callback.  ~p", [Info]),
	Return = Callback:handle_info(Info, State#state.callrec, State#state.substate),
	handle_custom_return(Return, State, noreply).
	
%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------

%% @private
terminate(Reason, #state{callback = Callback} = State) ->
	set_cpx_mon(State, delete),
	Callback:terminate(Reason, State#state.callrec, State#state.substate).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------

%% @private
code_change(OldVsn, #state{callback = Callback} = State, Extra) ->
	{ok, Newsub} = Callback:code_change(OldVsn, State#state.callrec, State#state.substate, Extra),
    {ok, State#state{substate = Newsub}}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

set_agent_state(Apid, Args) ->
	try apply(agent, set_state, [Apid | Args]) of
		Res ->
			Res
	catch
		exit:{noproc, {gen_fsm, sync_send_event, _TheArgs}} ->
			?WARNING("Agent ~p is a dead pid", [Apid]),
			badagent
	end.

handle_stop(Reason, #state{queue_pid = Qpid, oncall_pid = Ocpid, ring_pid = Rpid} = State) ->
	Call = State#state.callrec,
	case {Qpid, Ocpid, Rpid} of
		{undefined, undefined, undefined} ->
			cdr:hangup(State#state.callrec, string:join(tuple_to_list(Call#call.callerid), " ")),
			set_cpx_mon(State, delete);
		_ ->
			set_cpx_mon(State, delete),
			agent_interact(hangup, State)
	end,
	case Reason of
		hangup ->
			normal;
		_ ->
			Reason
	end.

handle_custom_return(Return, State, noreply) ->
	case Return of
		{noreply, NewState} ->
			{noreply, State#state{substate = NewState}};
		{noreply, NewState,  Timeout} ->
			{noreply, State#state{substate = NewState}, Timeout};
		{stop, Reason, NewState} ->
			Newstop = handle_stop(Reason, State),
			{stop, Newstop, State#state{substate = NewState}};
		{queue, Queue, PCallrec, NewState} ->
			Callrec = correct_client(PCallrec),
			case priv_queue(Queue, Callrec, State#state.queue_failover) of
				{default, Qpid} ->
					cdr:cdrinit(Callrec),
					cdr:inqueue(Callrec, "default_queue"),
					set_cpx_mon(State#state{callrec = Callrec, substate = NewState, queue_pid = Qpid}, [{queue, "default_queue"}]),
					{noreply, State#state{callrec = Callrec, substate = NewState, queue_pid = Qpid}};
				invalid ->
					?WARNING("Could not queue ~p into ~p (failover ~p)", [Callrec#call.id, Queue, State#state.queue_failover]),
					{noreply, State#state{callrec = Callrec, substate = NewState}};
				Qpid ->
					cdr:cdrinit(Callrec),
					cdr:inqueue(Callrec, Queue),
					set_cpx_mon(State#state{callrec = Callrec, substate = NewState, queue_pid = Qpid}, [{queue, Queue}]),
					{noreply, State#state{callrec = Callrec, substate = NewState, queue_pid = Qpid}}
			end;
		{voicemail, NewState} when is_pid(State#state.queue_pid)  orelse State#state.oncall_pid == undefined ->
			priv_voicemail(State),
			{noreply, State#state{substate = NewState, queue_pid = undefined, ring_pid = undefined}};
		Tuple when element(1, Tuple) =:= outbound ->
			{_Reply, NewState} = outgoing(Tuple, State),
			{noreply, NewState};
		{Interact, NewState} ->
			Midstate = agent_interact(Interact, State),
			{noreply, Midstate#state{substate = NewState}}
	end;
handle_custom_return(Return, State, reply) ->
	case Return of
		{reply, Reply, NewState} ->
			{reply, Reply, State#state{substate = NewState}};
		{reply, Reply, Newstate, Timeout}  ->
			{reply, Reply, State#state{substate = Newstate}, Timeout};
		{noreply, NewState} ->
			{noreply, State#state{substate = NewState}};
		{noreply, NewState, Timeout} ->
			{noreply, State#state{substate = NewState}, Timeout};
		{stop, Reason, Reply, NewState} ->
			Newreason = handle_stop(Reason, State),
			{stop, Newreason, Reply, State#state{substate = NewState}};
		{stop, Reason, NewState} ->
			Newreason = handle_stop(Reason, State),
			{stop, Newreason, State#state{substate = NewState}};
		{queue, Queue, PCallrec, NewState} ->
			Callrec = correct_client(PCallrec),
			case priv_queue(Queue, Callrec, State#state.queue_failover) of
				invalid ->
					?WARNING("Could not queue ~p into ~p (failover ~p)", [Callrec#call.id, Queue, State#state.queue_failover]),
					{reply, {error, {noqueue, Queue}}, State#state{callrec = Callrec, substate = NewState}};
				{default, Qpid} ->
					cdr:cdrinit(Callrec),
					cdr:inqueue(Callrec, "default_queue"),
					set_cpx_mon(State#state{callrec = Callrec, substate = NewState, queue_pid = Qpid}, [{queue, "default_queue"}]),
					{reply, ok, State#state{callrec = Callrec, substate = NewState, queue_pid = Qpid}};
				Qpid ->
					cdr:cdrinit(Callrec),
					cdr:inqueue(Callrec, Queue),
					set_cpx_mon(State#state{callrec = Callrec, substate = NewState, queue_pid = Qpid}, [{queue, Queue}]),
					{reply, ok, State#state{callrec = Callrec, substate = NewState, queue_pid = Qpid}}
			end;
		{voicemail, NewState} when is_pid(State#state.queue_pid) orelse State#state.oncall_pid == undefined ->
			priv_voicemail(State),
			{reply, ok, State#state{substate = NewState, queue_pid = undefined, ring_pid = undefined}};
		Tuple when element(1, Tuple) =:= outbound ->
			{Reply, NewState} = outgoing(Tuple, State),
			{reply, Reply, NewState};
		{Agentact, Reply, NewState} ->
			Midstate = agent_interact(Agentact, State),
			{reply, Reply, Midstate#state{substate = NewState}}
	end.

url_pop(#call{client = Client} = Call, Agent, Addedopts) ->
	#client{options = DefaultOptions} = correct_client_sub(undefined),
	String = case {proplists:get_value(url_pop, Client#client.options), proplists:get_value(url_pop, DefaultOptions)} of
		{undefined, undefined} ->
			undefined;
		{undefined, []} ->
			undefined;
		{undefined, L} ->
			L;
		{[], []} ->
			undefined;
		{[], L} ->
			L;
		{L, _} ->
			L
	end,
	case String of
		undefined ->
			ok;
		_ ->
			Words = [
				{"label", (case is_atom(Client#client.label) of true -> atom_to_list(Client#client.label); false -> Client#client.label end)},
				{"clientid", (case is_atom(Client#client.id) of true -> atom_to_list(Client#client.id); 
					false -> Client#client.id end)},
				{"callerid", element(1, Call#call.callerid) ++ " " ++ element(2, Call#call.callerid)},
				{"calleridname", element(1, Call#call.callerid)},
				{"calleridnum", element(2, Call#call.callerid)},
				{"callid", Call#call.id},
				{"destination", ""},
				{"ivroption", ""},
				{"media_type", atom_to_list(Call#call.type)},
				{"direction", atom_to_list(Call#call.direction)}
			],
			BaseUrl = util:string_interpolate(String, Words),
			Appender = fun({_Key, undefined}, Midurl) ->
					Midurl;
				({Key, Value}, Midurl) ->
				lists:append([Midurl, [$& | Key], [$= | Value]])
			end,
			Url = lists:foldl(Appender, BaseUrl, Addedopts),
			agent:url_pop(Agent, Url, "ring")
	end.

%% @doc Set the client record to an actual client record.
-spec(correct_client/1 :: (Callid :: #call{}) -> #call{}).
correct_client(#call{client = Client} = Callrec) ->
	Newclient = case Client of
		#client{id = Id} ->
			correct_client_sub(Id);
		undefined ->
			correct_client_sub(undefined);
		String ->
			% if given the client id; so a media is not burndened with checking
			% mnesia or the client itself.
			% basically, see the next function call:
			correct_client_sub(String)
	end,
	Callrec#call{client = Newclient}.
	
correct_client_sub(undefined) ->
	Client = try call_queue_config:get_client(id, undefined) of
		Whatever ->
			Whatever
	catch
		error:{case_clause, {aborted, {node_not_running, _Node}}} ->
			#client{}
	end,
	Client;
correct_client_sub(Id) ->
	Client = try call_queue_config:get_client(id, Id) of
		none ->
			correct_client_sub(undefined);
		Else ->
			Else
	catch
		error:{case_clause, {aborted, {node_not_running, _Node}}} ->
			#client{}
	end,
	Client.

-spec(set_cpx_mon/2 :: (State :: #state{}, Action :: proplist() | 'delete') -> 'ok').
set_cpx_mon(#state{callrec = Call} = _State, delete) ->
	cpx_monitor:drop({media, Call#call.id});
set_cpx_mon(#state{callrec = Call} = _State, Details) ->
	Client = Call#call.client,
	MidBasedet = [
		{type, Call#call.type},
		{callerid, Call#call.callerid},
		{client, Client},
		{ring_path, Call#call.ring_path},
		{media_path, Call#call.media_path},
		{direction, Call#call.direction}
	],
	{Hp, Basedet} = case {proplists:get_value(queue, Details), proplists:get_value(agent, Details)} of
		{undefined, undefined} ->
			{[], MidBasedet};
		{undefined, _A} ->
			{[{agent_link, {0, 60 * 5, 60 * 15, {time, util:now()}}}], MidBasedet};
		{_Q, _} ->
			{[{inqueue, {0, 60 * 5, 60 * 10, {time, util:now()}}}], [{queued_at, {timestamp, util:now()}}, {priority, Call#call.priority} | MidBasedet]}
	end,
	Fulldet = lists:append([Basedet, Details]),
	cpx_monitor:set({media, Call#call.id}, Hp, Fulldet).

priv_queue(Queue, Callrec, Failover) ->
	case queue_manager:get_queue(Queue) of
		undefined ->
			?WARNING("Uh oh, no queue of ~p, failover:  ~w for ~p", [Queue, Failover, Callrec#call.id]),
			case Failover of
				true ->
					Dqpid = queue_manager:get_queue("default_queue"),
					call_queue:add(Dqpid, self(), Callrec),
					%% yes, we do want this to die if the default queue can't be found
					{default, Dqpid};
				false ->
					invalid
			end;
		Qpid ->
			?DEBUG("Trying to add ~p to queue...", [Callrec#call.id]),
			R = call_queue:add(Qpid, self(), Callrec),
			?DEBUG("q response:  ~p for ~p", [R, Callrec#call.id]),
			?INFO("Queueing call ~s into ~s", [Callrec#call.id, Queue]),
			Qpid
	end.

priv_voicemail(State) ->
	case State#state.queue_pid of
		undefined ->
			% meh
			ok;
		Qpid ->
			call_queue:remove(State#state.queue_pid, self())
	end,
	cdr:voicemail(State#state.callrec, State#state.queue_pid),
	case State#state.ring_pid of
		undefined ->
			ok;
		Apid when is_pid(Apid) ->
			set_agent_state(Apid, [idle])
	end,
	ok.

unqueue(undefined, _Callpid) ->
	ok;
unqueue(Qpid, Callpid) when is_pid(Qpid) ->
	call_queue:remove(Qpid, Callpid),
	ok.

kill_outband_ring(State) ->
	case State#state.outband_ring_pid of
		undefined ->
			ok;
		Pid when is_pid(Pid) ->
			freeswitch_ring:hangup(Pid)
	end.

agent_interact({mediapush, Data}, #state{oncall_pid = Ocpid, callrec = Call} = State) when is_pid(Ocpid), Call#call.media_path =:= inband ->
	?DEBUG("Shoving ~p from ~p", [Data, Call#call.id]),
	agent:media_push(Ocpid, Data),
	State;
agent_interact({mediapush, _Data}, #state{callrec = Call} = State) ->
	?INFO("Cannot do a media push from ~p in current state:  ~p", [Call#call.id, State]),
	State;
agent_interact(stop_ring, State) ->
	agent_interact({stop_ring, undefined}, State);
agent_interact({stop_ring, Reason}, #state{callrec = Call, ring_pid = Apid} = State)  ->
	case Call#call.cook of
		CookPid when is_pid(CookPid) ->
			gen_server:cast(CookPid, stop_ringing);
		_ ->
			ok
	end,
	Midstate = case {State#state.ringout, Apid} of
		{false, undefined} ->
			% Nothin' doing.
			?INFO("stop_ring for ~p when there's not much of a ring to handle", [Call#call.id]),
			State;
		{false, Apid} ->
			?INFO("stop_ring for ~p with an agent ringing but no timer", [Call#call.id]),
			set_agent_state(Apid, [idle]),
			cdr:ringout(State#state.callrec, {Reason, Apid}),
			State#state{ring_pid = undefined};
		{Tref, undefined} ->
			timer:cancel(Tref),
			?WARNING("stop_ring for ~p with only a timer", [Call#call.id]),
			State#state{ringout = false};
		{Tref, Apid} ->
			?INFO("stop_ring for ~p with a timer and agent", [Call#call.id]),
			timer:cancel(Tref),
			set_agent_state(Apid, [idle]),
			cdr:ringout(State#state.callrec, {Reason, Apid}),
			State#state{ring_pid = undefined, ringout = false}
	end,
	kill_outband_ring(Midstate),
	Midstate#state{outband_ring_pid = undefined};
agent_interact(wrapup, #state{oncall_pid = Apid, callrec = Call} = State) ->
	?INFO("Attempting to set agent at ~p to wrapup for ~p", [Apid, Call#call.id]),
	set_agent_state(Apid, [wrapup, State#state.callrec]),
	cdr:wrapup(State#state.callrec, Apid),
	State#state{oncall_pid = undefined};
agent_interact(hangup, #state{oncall_pid = Oncallpid, ring_pid = Ringpid, callrec = Call} = State) when is_pid(Oncallpid), is_pid(Ringpid) ->
	?INFO("hangup for ~p when both oncall and ring are pids", [Call#call.id]),
	set_agent_state(Ringpid, [idle]),
	set_agent_state(Oncallpid, [wrapup, State#state.callrec]),
	cdr:wrapup(State#state.callrec, Oncallpid),
	Callrec = State#state.callrec,
	cdr:hangup(Callrec, string:join(tuple_to_list(Callrec#call.callerid), " ")),
	kill_outband_ring(State),
	State#state{oncall_pid = undefined, ring_pid = undefined, outband_ring_pid = undefined};
agent_interact(hangup, #state{oncall_pid = Apid, callrec = Call} = State) when is_pid(Apid) ->
	?INFO("hangup for ~p when only oncall is a pid", [Call#call.id]),
	set_agent_state(Apid, [wrapup, State#state.callrec]),
	cdr:wrapup(State#state.callrec, Apid),
	Callrec = State#state.callrec,
	cdr:hangup(Callrec, string:join(tuple_to_list(Callrec#call.callerid), " ")),
	State#state{oncall_pid = undefined};
agent_interact(hangup, #state{ring_pid = Apid, callrec = Call} = State) when is_pid(Apid) ->
	?INFO("hangup for ~p when only ringing is a pid", [Call#call.id]),
	set_agent_state(Apid, [idle]),
	cdr:hangup(Call, string:join(tuple_to_list(Call#call.callerid), " ")),
	kill_outband_ring(State),
	State#state{ring_pid = undefined, outband_ring_pid = undefined};
agent_interact(hangup, #state{queue_pid = Qpid, callrec = Call} = State) when is_pid(Qpid) ->
	?INFO("hang for ~p up when only queue is a pid", [Call#call.id]),
	unqueue(Qpid, self()),
	cdr:hangup(State#state.callrec, string:join(tuple_to_list(Call#call.callerid), " ")),
	State#state{queue_pid = undefined};
agent_interact(hangup, #state{callrec = Call, queue_pid = undefined, oncall_pid = undefined, ring_pid = undefined} = State) when is_record(Call, call) ->
	?INFO("orphaned call ~p, no queue or agents at all", [Call#call.id]),
	cdr:hangup(State#state.callrec, string:join(tuple_to_list(Call#call.callerid), " ")),
	State;
agent_interact(hangup, #state{queue_pid = undefined, oncall_pid = undefined, ring_pid = undefined} = State) ->
	?INFO("Truely orphaned call, no queue, agents, or even a call record", []),
	State.

%% These should crash the media if the agent doesn't exist.
outgoing({outbound, Agent, NewState}, #state{callrec = Call} = State) when is_record(State#state.callrec, call) ->
	?INFO("Told to set ~s to outbound for ~p", [Agent, Call#call.id]),
	case agent_manager:query_agent(Agent) of
		{true, Apid} ->
			agent:set_state(Apid, outgoing, State#state.callrec),
			cdr:oncall(State#state.callrec, Agent),
			{ok, State#state{oncall_pid = Apid, substate = NewState}};
		false ->
			?ERROR("Agent ~s doesn't exists; can't set outgoing for ~p", [Agent, Call#call.id]),
			{{error, {noagent, Agent}}, State#state{substate = NewState}}
	end;
outgoing({outbound, Agent, Call, NewState}, State) when is_record(Call, call) ->
	?INFO("Told to set ~s to outbound and call ~p", [Agent, Call#call.id]),
	case agent_manager:query_agent(Agent) of
		{true, Apid} ->
			agent:set_state(Apid, outgoing, Call),
			cdr:oncall(Call, Agent),
			{ok, State#state{callrec = Call, substate = NewState, oncall_pid = Apid}};
		false ->
			?ERROR("Agent ~s doesn't exists; can't set outgoing for ~p", [Agent, Call#call.id]),
			{{error, {noagent, Agent}}, State#state{substate = NewState, callrec = Call}}
	end.

-ifdef(TEST).

dead_spawn() ->
	spawn(fun() -> ok end).

url_pop_test_() ->
	{setup,
	fun() ->
		{ok, Agent} = agent:start(#agent{login = "testagent"}),
		{ok, Conn} = gen_server_mock:new(),
		gen_server_mock:expect_cast(Conn, fun(_, _) -> ok end),
		agent:set_connection(Agent, Conn),
		Call = #call{id = "testcall", source = dead_spawn()},
		{Call, Agent, Conn}
	end,
	fun({_, Agent, Conn}) ->
		gen_server_mock:stop(Conn),
		agent:stop(Agent)
	end,
	fun({BaseCall, Agent, Conn}) ->
		[{"no url pop defined in client",
		fun() ->
			Call = BaseCall#call{client = #client{label = "client", id = "client", options = []}},
			% if the mock (Conn) gets a cast, it'll error; that's the test.
			% if it error's, it's a fail.
			url_pop(Call, Agent, [])
		end},
		{"url is an empty list",
		fun() ->
			Call = BaseCall#call{client = #client{label = "client", id = "client", options = [{url_pop, []}]}},
			% same as above.
			url_pop(Call, Agent, [])
		end},
		{"url is set",
		fun() ->
			Call = BaseCall#call{client = #client{label = "client", id = "client", options = [{url_pop, "example.com"}]}},
			gen_server_mock:expect_cast(Conn, fun({url_pop, "example.com", "ring"}, _) -> ok end),
			url_pop(Call, Agent, []),
			gen_server_mock:assert_expectations(Conn)
		end},
		{"url is set with some additional options",
		fun() ->
			Call = BaseCall#call{client = #client{label = "client", id = "client", options = [{url_pop, "example.com?a=b"}]}},
			gen_server_mock:expect_cast(Conn, fun({url_pop, "example.com?a=b&addkey=addval", "ring"}, _) -> ok end),
			url_pop(Call, Agent, [{"addkey", "addval"}]),
			gen_server_mock:assert_expectations(Conn)
		end},
		{"url is set with some additional options, some of which are blank",
		fun() ->
			Call = BaseCall#call{client = #client{label = "client", id = "client", options = [{url_pop, "example.com?a=b"}]}},
			gen_server_mock:expect_cast(Conn, fun({url_pop, "example.com?a=b&addkey=addval", "ring"}, _) -> ok end),
			url_pop(Call, Agent, [{"addkey", "addval"}, {"foo", undefined}]),
			gen_server_mock:assert_expectations(Conn)
		end}
	]
	end}.
	
init_test_() ->
	{foreach,
	fun() ->
		{ok, QMmock} = gen_leader_mock:start(queue_manager),
		{ok, Qpid} = gen_server_mock:new(),
		Assertmocks = fun() ->
			gen_server_mock:assert_expectations(Qpid),
			gen_leader_mock:assert_expectations(QMmock)
		end,
		{QMmock, Qpid, Assertmocks}
	end,
	fun({QMmock, Qpid, _Assertmocks}) ->
		gen_leader_mock:stop(QMmock),
		gen_server_mock:stop(Qpid),
		timer:sleep(10)
	end,
	[fun({_, _, Assertmocks}) ->
		{"call rec returned, but no queue",
		fun() ->
			Args = [[{id, "dummy"}, {queues, none}], success],
			Res = init([dummy_media, Args]),
			?assertMatch({ok, #state{callback = dummy_media, callrec = #call{id = "dummy"}}}, Res),
			Assertmocks()
		end}
	end,
	fun({QMmock, Qpid, Assertmocks}) ->
		{"call rec and queue name returned",
		fun() ->
			Args = [[{queues, ["testqueue"]}, {id, "dummy"}], success],
			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
				{ok, Qpid, State}
			end),
			gen_server_mock:expect_call(Qpid, fun({add, 40, Inpid, Callrec}, _From, _State) -> ok end),
			Res = init([dummy_media, Args]),
			?assertMatch({ok, #state{callback = dummy_media, callrec = #call{id = "dummy"}, queue_pid = Qpid}}, Res),
			Assertmocks()
		end}
	end,
	fun({QMmock, Qpid, Assertmocks}) ->
		{"call rec and queue name returned, but queue doesn't exist",
		fun() ->
			Args = [[{queues, ["testqueue"]}, {id, "dummy"}], success],
			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
				{ok, undefined, State}
			end),
			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "default_queue"}, _From, State, _Elec) ->
				{ok, Qpid, State}
			end),
			gen_server_mock:expect_call(Qpid, fun({add, 40, Inpid, Callrec}, _From, _State) -> ok end),
			Res = init([dummy_media, Args]),
			?assertMatch({ok, #state{callback = dummy_media, callrec = #call{id = "dummy"}, queue_pid = Qpid}}, Res),
			Assertmocks()
		end}
	end]}.

handle_call_test_() ->
	{foreach,
	fun() ->
		{ok, Seedstate} = init([dummy_media, [[{queues, none}], success]]),
		{ok, QMmock} = gen_leader_mock:start(queue_manager),
		{ok, Qpid} = gen_server_mock:new(),
		{ok, Ammock} = gen_leader_mock:start(agent_manager),
		gen_event:start({local, cdr}),
		Assertmocks = fun() ->
			gen_server_mock:assert_expectations(Qpid),
			gen_leader_mock:assert_expectations(QMmock),
			gen_leader_mock:assert_expectations(Ammock),
			gen_event_mock:assert_expectations(cdr)
		end,
		Makestate = fun() ->
			{ok, #state{callrec = Callrec} = Out} = init([dummy_media, [[{queues, none}], success]]),
			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}), 
			Out
		end,
		{Makestate, QMmock, Qpid, Ammock, Assertmocks}
	end,
	fun({_Makestate, QMmock, Qpid, Ammock, _Assertmocks}) ->
		gen_server_mock:stop(Qpid),
		gen_leader_mock:stop(QMmock),
		gen_leader_mock:stop(Ammock),
		gen_event:stop(cdr),
		timer:sleep(10)
	end,
	[fun({Makestate, _QMock, _Qpid, _Ammock, Assertmocks}) ->
		{"spying when there's no agent oncall fails",
		fun() ->
			Seedstate = Makestate(),
			?assertMatch({reply, invalid, Seedstate}, handle_call({'$gen_media_spy', "Pid"}, "from", Seedstate)),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMock, _Qpid, _Ammock, Assertmocks}) ->
		{"Spy is not the pid making the request",
		fun() ->
			{ok, Spy} = agent:start(#agent{login = "testagent", state = released, statedata = "default"}),
			Pid = spawn(fun() -> ok end),
			Seedstate = Makestate(),
			State = Seedstate#state{oncall_pid = Spy},
			?assertMatch({reply, invalid, State}, handle_call({'$gen_media_spy', Spy}, {self(), "tag"}, State)),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMock, _Qpid, _Ammock, Assertmocks}) ->
		{"Can't spy on yourself", 
		fun() ->
			Seedstate = Makestate(),
			Spy = dead_spawn(),
			State = Seedstate#state{oncall_pid = Spy},
			?assertMatch({reply, invalid, State}, handle_call({'$gen_media_spy', Spy}, {Spy, "tag"}, State))
		end}
	end,
	fun({Makestate, _QMock, _Qpid, _Ammock, Assertmocks}) ->
		{"Spy valid, callback says no",
		fun() ->
			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
			Callrec = Seedstate#state.callrec,
			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
			Ocpid = dead_spawn(),
			{ok, Spy} = agent:start(#agent{login = "testagent", state = released, statedata = "defaut"}),
			?assertMatch({reply, invalid, _Newstate}, handle_call({'$gen_media_spy', Spy}, {Spy, "tag"}, Seedstate#state{oncall_pid = Ocpid})),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMock, Qpid, _Ammock, Assertmocks}) ->
		{"Spy valid, callback says ok",
		fun() ->
			Seedstate = Makestate(),
			Ocpid = dead_spawn(),
			State = Seedstate#state{oncall_pid = Ocpid},
			{ok, Spy} = agent:start(#agent{login = "testagent", state= released, statedata = "default"}),
			?assertMatch({reply, ok, _Newstate}, handle_call({'$gen_media_spy', Spy}, {Spy, "tag"}, State)),
			Assertmocks()
		end}
	end,	
	fun({Makestate, _, _, Ammock, Assertmocks}) ->
		{"oncall_pid requests wrapup",
		fun() ->
			Seedstate = Makestate(),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Seedstate#state.callrec}),
			gen_leader_mock:expect_leader_call(Ammock, fun({get_login, Apid}, _From, State, _Elec) ->
				Apid = Agent,
				{ok, "testagent", State}
			end),
			gen_event_mock:expect_event(cdr, fun({wrapup, Callrec, _Time, "testagent"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({hangup, Callrer, _Time, agent}, _State) -> ok end),
			State = Seedstate#state{oncall_pid = Agent},
			?assertMatch({stop, normal, ok, _State}, handle_call('$gen_media_wrapup', {Agent, "tag"}, State)),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMmock, _Qpid, _Ammock, Assertmocks}) ->
		{"oncall_pid can't request wrapup when media_path is outband",
		fun() ->
			#state{callrec = Oldcall} = Seedstate = Makestate(),
			Callrec = Oldcall#call{media_path = outband},
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Callrec}),
			State = Seedstate#state{oncall_pid = Agent, callrec = Callrec},
			?assertMatch({reply, invalid, _State}, handle_call('$gen_media_wrapup', {Agent, "tag"}, State)),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"sending to queue requested by oncall pid, all works",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Seedstate#state.callrec}),
			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
				{ok, Qpid, State}
			end),
			gen_server_mock:expect_call(Qpid, fun({add, 40, Mpid, Rec}, _From, _State) ->
				Mpid = Callrec#call.source,
				Rec = Callrec,
				ok
			end),
			gen_event_mock:expect_event(cdr, fun({queue_transfer, Callrec, _Time, "testqueue"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({inqueue, _Callrec, _Time, "testqueue"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({wrapup, Callrec, _Time, Agent}, _State) -> ok end),
			gen_leader_mock:expect_leader_call(Ammock, fun({get_login, Agent}, _From, State, _Elec) ->
				{ok, "testagent", State}
			end),
			State = Seedstate#state{oncall_pid = Agent},
			{reply, ok, Newstate} = handle_call({'$gen_media_queue', "testqueue"}, {Agent, "tag"}, State),
			?assertEqual(undefined, Newstate#state.oncall_pid),
			?assertEqual(Qpid, Newstate#state.queue_pid),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"oncall pid sends call to queue, but falls back to default queue",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Seedstate#state.callrec}),
			State = Seedstate#state{oncall_pid = Agent},
			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
				{ok, undefined, State}
			end),
			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "default_queue"}, _From, State, _Elec) ->
				{ok, Qpid, State}
			end),
			gen_server_mock:expect_call(Qpid, fun({add, 40, Mpid, Rec}, _From, _State) ->
				Mpid = Callrec#call.source,
				Rec = Callrec,
				ok
			end),
			gen_event_mock:expect_event(cdr, fun({queue_transfer, Callrec, _Time, "default_queue"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({inqueue, Callrec, _Time, "default_queue"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({wrapup, Callrec, _Time, Agent}, _State) -> ok end),
			gen_leader_mock:expect_leader_call(Ammock, fun({get_login, Agent}, _From, State, _Elec) ->
				{ok, "testagent", State}
			end),
			{reply, ok, Newstate} = handle_call({'$gen_media_queue', "testqueue"}, {Agent, "tag"}, State),
			?assertEqual(undefined, Newstate#state.oncall_pid),
			?assertEqual(Qpid, Newstate#state.queue_pid),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"oncall pid sends call to queue, but falls back to nowhere w/ fallback set to false",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Seedstate#state.callrec}),
			State = Seedstate#state{oncall_pid = Agent, queue_failover = false},
			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
				{ok, undefined, State}
			end),
			?assertMatch({reply, invalid, _State}, handle_call({'$gen_media_queue', "testqueue"}, {Agent, "tag"}, State)),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"sent to queue by something else, and alls well.",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Callrec}),
			gen_server_mock:expect_call(Qpid, fun({add, 40, Mpid, Rec}, _From, _State) ->
				Mpid = Callrec#call.source, 
				Rec = Callrec,
				ok
			end),
			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
				{ok, Qpid, State}
			end),
			gen_leader_mock:expect_leader_call(Ammock, fun({get_login, Agent}, _From, State, _Elec) ->
				{ok, "testagent", State}
			end),
			gen_event_mock:expect_event(cdr, fun({queue_transfer, Callrec, _Time, "testqueue"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({inqueue, Callrec, _Time, "testqueue"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({wrapup, Callrec, _Time, Agent}, _State) -> ok end),
			State = Seedstate#state{oncall_pid = Agent},
			{reply, ok, Newstate} = handle_call({'$gen_media_queue', "testqueue"}, "from", State),
			?assertEqual(undefined, Newstate#state.oncall_pid),
			?assertEqual(Qpid, Newstate#state.queue_pid),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"sent to queue by something else, but falling back",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Callrec}),
			gen_server_mock:expect_call(Qpid, fun({add, 40, Mpid, Rec}, _From, _state) ->
				Mpid = Callrec#call.source,
				Rec = Callrec,
				ok
			end),
			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
				{ok, undefined, State}
			end),
			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "default_queue"}, _From, State, _Elec) ->
				{ok, Qpid, State}
			end),
			gen_event_mock:expect_event(cdr, fun({queue_transfer, Callrec, _Time, "default_queue"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({inqueue, Callrec, _Time, "default_queue"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({wrapup, Callrec, _Time, Agent}, _State) -> ok end),
			gen_leader_mock:expect_leader_call(Ammock, fun({get_login, Agent}, _From, State, _Elec) ->
				{ok, "testagent", State}
			end),
			State = Seedstate#state{oncall_pid = Agent},
			{reply, ok, Newstate} = handle_call({'$gen_media_queue', "testqueue"}, "from", State),
			?assertEqual(undefined, Newstate#state.oncall_pid),
			?assertEqual(Qpid, Newstate#state.queue_pid),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"gen_media_ring setting agent successful, as is the callback module.",
		fun() ->
			{ok, Agent} = agent:start(#agent{login = "testagent", state = idle, statedata = {}}),
			gen_leader_mock:expect_leader_call(Ammock, fun(_, _, State, _) ->
				%% cdr sends a call here.
				{ok, "testagent", State}
			end),
			#state{callrec = Callrec} = Seedstate = Makestate(),
			gen_event_mock:expect_event(cdr, fun({ringing, Callrec, _Time, "testagent"}, _State) -> ok end),
			#queued_call{cook = Cook} = Qcall = #queued_call{media = Callrec#call.source, id = "testcall"},
			{reply, ok, Newstate} = handle_call({'$gen_media_ring', Agent, Qcall, 100}, {Cook, "tag"}, Seedstate),
			receive
				{'$gen_media_stop_ring', Cook} ->
					ok
			after 150 ->
				erlang:error(timer_timeout)
			end,
			?assertEqual(Agent, Newstate#state.ring_pid),
			?assertNot(false =:= Newstate#state.ringout),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"gen_media_ring setting the agent fails.",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			#queued_call{cook = Cook} = Qcall = #queued_call{media = Callrec#call.source, id = "testcall"},
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = "whatever"}),
			?assertMatch({reply, invalid, _State}, handle_call({'$gen_media_ring', Agent, Qcall, 100}, {Cook, "tag"}, Seedstate)),
			receive
				{'$gen_media_stop_ring', Cook} ->
					erlang:error(timer_lives)
			after 150 ->
				ok
			end,
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"gen_media_ring callback module fails",
		fun() ->
			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
			Callrec = Seedstate#state.callrec,
			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
			#queued_call{cook = Cook} = Qcall = #queued_call{media = Callrec#call.source, id = "testcall"},
			{ok, Agent} = agent:start(#agent{login = "testagent", state = idle, statedata = {}}),
			{reply, invalid, Newstate} = handle_call({'$gen_media_ring', Agent, Qcall, 150}, {Cook, "tag"}, Seedstate),
			receive
				{'$gen_media_stop_ring', Cook} ->
					erlang:error(timer_lives)
			after 150 ->
				ok
			end,
			?assertEqual(undefined, Newstate#state.ring_pid),
			?assertEqual({ok, released}, agent:query_state(Agent)),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"can't transfer to yourself, silly!",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Callrec}),
			State = Seedstate#state{oncall_pid = Agent},
			?assertEqual({reply, invalid, State}, handle_call({'$gen_media_agent_transfer', Agent}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"agent transfer, target agent can't change state",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Callrec}),
			{ok, Target} = agent:start(#agent{login = "targetagent", state = wrapup, statedata = "doesn't matter"}),
			State = Seedstate#state{oncall_pid = Agent},
			?assertEqual({reply, invalid, State}, handle_call({'$gen_media_agent_transfer', Target, 100}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"agent transfer, all is well",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			%% cdr makes 2 call outs to this, but that will be tested in cdr
			gen_leader_mock:expect_leader_call(Ammock, fun(_, _, S, _) -> {ok, "testagent", S} end),
			gen_leader_mock:expect_leader_call(Ammock, fun(_, _, S, _) -> {ok, "targetagent", S} end),
			gen_leader_mock:expect_leader_call(Ammock, fun(_, _, S, _) -> {ok, "targetagent", S} end),
			gen_event_mock:expect_event(cdr, fun({agent_transfer, Callrec, _Time, {"testagent", "targetagent"}}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({ringing, Callrec, _Time, "targetagent"}, _State) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Callrec}),
			{ok, Target} = agent:start(#agent{login = "targetagent", state = idle, statedata = {}}),
			State = Seedstate#state{oncall_pid = Agent},
			{reply, ok, Newstate} = handle_call({'$gen_media_agent_transfer', Target, 100}, "from", State),
			receive
				{'$gen_media_stop_ring', Cook} ->
					ok
			after 150 ->
				erlang:error(timer_nolives)
			end,
			?assertEqual(Target, Newstate#state.ring_pid),
			?assertEqual(Agent, Newstate#state.oncall_pid),
			?assertNot(false =:= Newstate#state.ringout),
			?assertEqual({ok, ringing}, agent:query_state(Target)),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"agent transfer, callback says no",
		fun() ->
			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
			Callrec = Seedstate#state.callrec,
			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
			{ok, Target} = agent:start(#agent{login = "testagent", state = idle, statedata = {}}),
			Agent = spawn(fun() -> ok end),
			State = Seedstate#state{oncall_pid = Agent},
			{reply, invalid, Newstate} = handle_call({'$gen_media_agent_transfer', Target, 100}, "from", State),
			receive
				{'$gen_media_ring_stop', _Cook} ->
					erlang:error(timer_lives)
			after 150 ->
				ok
			end,
			?assertNot(Newstate#state.ringout),
			?assertEqual(undefined, Newstate#state.ring_pid),
			?assertEqual({ok, idle}, agent:query_state(Target)),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"gen_media_announce",
		fun() ->
			{ok, #state{callrec = Call} = Seedstate} = init([dummy_media, [[{queues, none}], success]]),
			gen_event_mock:supplant(cdr, {{cdr, Call#call.id}, []}),
			{reply, ok, Newstate} = handle_call({'$gen_media_announce', "doesn't matter"}, "from", Seedstate),
			?CONSOLE("~p", [Seedstate]),
			?CONSOLE("~p", [Newstate]),
			?assertEqual({reply, ok, Seedstate}, handle_call({'$gen_media_announce', "doesn't matter"}, "from", Seedstate)),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"gen_media_voicemail works",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			State = Seedstate#state{queue_pid = Qpid},
			% this expect is because the cdr is going to want it.
			gen_leader_mock:expect_leader_call(QMmock, fun(queues_as_list, _From, State, _Elec) ->
				{ok, [{"default_queue", Qpid}], State}
			end),
			gen_server_mock:expect_call(Qpid, fun({remove, Inpid}, _From, _State) ->
				Inpid = Callrec#call.source,
				ok
			end),
			gen_event_mock:expect_event(cdr, fun({voicemail, Callrec, _Time, "default_queue"}, _State) -> ok end),
			?assertMatch({reply, ok, _State}, handle_call('$gen_media_voicemail', "from", State)),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"gen_media_voicemail while an agent's ringing",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = ringing, statedata = Callrec}),
			State = Seedstate#state{queue_pid = Qpid, ring_pid = Agent},
			gen_leader_mock:expect_leader_call(QMmock, fun(queues_as_list, _From, State, _Elec) ->
				{ok, [{"default_queue", Qpid}], State}
			end),
			gen_server_mock:expect_call(Qpid, fun({remove, Inpid}, _From, _State) ->
				Inpid = Callrec#call.source,
				ok
			end),
			gen_event_mock:expect_event(cdr, fun({voicemail, Callrec, _Time, "default_queue"}, _State) -> ok end),
			{reply, ok, NewState} = handle_call('$gen_media_voicemail', "from", State),
			?assertEqual(undefined, NewState#state.ring_pid),
			?assertEqual({ok, idle}, agent:query_state(Agent)),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"gen_media_voicemail callback says no",
		fun() ->
			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
			#state{callrec = Call} = State = Seedstate#state{queue_pid = Qpid},
			gen_event_mock:supplant(cdr, {{cdr, Call#call.id}, []}),
			?assertMatch({reply, invalid, _State}, handle_call('$gen_media_voicemail', "from", State)),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"Agent can't request oncall if ring_path is outband",
		fun() ->
			#state{callrec = Seedcall} = Seedstate = Makestate(),
			Agent = spawn(fun() -> ok end),
			Callrec = Seedcall#call{ring_path = outband},
			State = Seedstate#state{callrec = Callrec, ring_pid = Agent},
			?assertEqual({reply, invalid, State}, handle_call('$gen_media_agent_oncall', {Agent, "tag"}, State)),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"agent oncall request when both a ring pid and oncall pid are set and media path is inband",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			{ok, Oncall} = agent:start(#agent{login = "oncall", state = oncall, statedata = Callrec}),
			{ok, Ring} = agent:start(#agent{login = "ringing", state = oncall, statedata = Callrec}),
			gen_leader_mock:expect_leader_call(Ammock, fun(_Msg, _From, State, _Elec) -> {ok, "ringing", State} end),
			gen_leader_mock:expect_leader_call(Ammock, fun(_Msg, _From, State, _Elec) -> {ok, "oncall", State} end),
			gen_leader_mock:expect_leader_call(Ammock, fun(_Msg, _From, _State, _Elec) -> ok end),
			gen_event_mock:expect_event(cdr, fun({oncall, Callrec, _Time, "ringing"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({wrapup, Callrec, _Time, "oncall"}, _State) -> ok end),
			{ok, Tref} = timer:send_after(100, timer_lives),
			State = Seedstate#state{oncall_pid = Oncall, ring_pid = Ring, ringout = Tref},
			{reply, ok, Newstate} = handle_call('$gen_media_agent_oncall', {Ring, "tag"}, State),
			receive
				timer_lives ->
					erlang:error(timer_lives)
			after 150 ->
				ok
			end,
			?assertNot(Newstate#state.ringout),
			?assertEqual(Ring, Newstate#state.oncall_pid),
			?assertEqual(undefined, Newstate#state.ring_pid),
			?assertEqual({ok, wrapup}, agent:query_state(Oncall)),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"agent oncall request when both a ring pid and oncall pid are set and media path is inband, but callback says no",
		fun() ->
			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
			Callrec = Seedstate#state.callrec,
			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
			{ok, Oncall} = agent:start(#agent{login = "oncall", state = oncall, statedata = Callrec}),
			{ok, Ring} = agent:start(#agent{login = "ring", state = ringing, statedata = Callrec}),
			{ok, Tref} = timer:send_after(100, timer_lives),
			State = Seedstate#state{oncall_pid = Oncall, ring_pid = Ring, ringout = Tref},
			{reply, invalid, Newstate} = handle_call('$gen_media_agent_oncall', {Ring, "tag"}, State),
			receive
				timer_lives ->
					ok
			after 150 ->
				erlang:error(timer_nolives)
			end,
			?assertEqual(State, Newstate),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"oncall during transfer with outband media",
		fun() ->
			#state{callrec = Seedcall} = Seedstate = Makestate(),
			Callrec = Seedcall#call{ring_path = outband},
			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
			{ok, Oncall} = agent:start(#agent{login = "oncall", state = oncall, statedata = Callrec}),
			{ok, Ring} = agent:start(#agent{login = "ring", state = ringing, statedata = Callrec}),
			gen_leader_mock:expect_leader_call(Ammock, fun(_Msg, _From, State, _Elec) -> {ok, "ring", State} end),
			gen_leader_mock:expect_leader_call(Ammock, fun(_Msg, _From, State, _Elec) -> {ok, "oncall", State} end),
			gen_leader_mock:expect_leader_call(Ammock, fun(_Msg, _From, _State, _Elec) -> ok end),
			gen_event_mock:expect_event(cdr, fun({oncall, Callrec, _Time, "ring"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({wrapup, Callrec, _Time, "oncall"}, _State) -> ok end),
			{ok, Tref} = timer:send_after(100, timer_lives),
			State = Seedstate#state{oncall_pid = Oncall, ringout = Tref, ring_pid = Ring, callrec = Callrec},
			{reply, ok, Newstate} = handle_call('$gen_media_agent_oncall', "from", State),
			receive
				timer_lives ->
					erlang:error(timer_lives)
			after 150 ->
				ok
			end,
			?assertEqual({ok, oncall}, agent:query_state(Ring)),
			?assertEqual({ok, wrapup}, agent:query_state(Oncall)),
			?assertEqual(undefined, Newstate#state.ring_pid),
			?assertNot(Newstate#state.ringout),
			?assertEqual(Ring, Newstate#state.oncall_pid),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"oncall during transfer with outband media, but callback says no",
		fun() ->
			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
			Seedcall = Seedstate#state.callrec,
			gen_event_mock:supplant(cdr, {{cdr, Seedcall#call.id}, []}),
			Callrec = Seedcall#call{ring_path = outband},
			{ok, Oncall} = agent:start(#agent{login = "oncall", state = oncall, statedata = Callrec}),
			{ok, Ring} = agent:start(#agent{login = "ring", state = ringing, statedata = Callrec}),
			{ok, Tref} = timer:send_after(100, timer_lives),
			State = Seedstate#state{oncall_pid = Oncall, ringout = Tref, ring_pid = Ring, callrec = Callrec},
			{reply, invalid, Newstate} = handle_call('$gen_media_agent_oncall', "from", State),
			receive
				timer_lives ->
					ok
			after 150 ->
				erlang:error(timer_nolives)
			end,
			% TODO Two agents oncall due to why going on call is handled :/
			% since there's no way to roll back an oncall, if the handle_answer
			% callback fails, we have a f*cked state.
			?assertEqual({ok, oncall}, agent:query_state(Ring)),
			?assertEqual({ok, oncall}, agent:query_state(Oncall)),
			?assertEqual(Ring, Newstate#state.ring_pid),
			?assertEqual(Tref, Newstate#state.ringout),
			?assertEqual(Oncall, Newstate#state.oncall_pid),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"oncall queue to agent requested by agent with inband media",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = ringing, statedata = Callrec}),
			gen_leader_mock:expect_leader_call(Ammock, fun(_Msg, _From, State, _Elec) -> {ok, "testagent", State} end),
			gen_leader_mock:expect_leader_call(Ammock, fun(_Msg, _From, _State, _Elec) -> ok end),
			gen_server_mock:expect_call(Qpid, fun({remove, Inpid}, _From, _State) ->
				Inpid = Callrec#call.source,
				ok
			end),
			gen_event_mock:expect_event(cdr, fun({oncall, Callrec, _Time, "testagent"}, _State) -> ok end),
			{ok, Tref} = timer:send_after(100, timer_lives),
			State = Seedstate#state{queue_pid = Qpid, ring_pid = Agent, ringout = Tref},
			{reply, ok, Newstate} = handle_call('$gen_media_agent_oncall', {Agent, "tag"}, State),
			receive
				timer_lives ->
					erlang:error(timer_lives)
			after 150 ->
				ok
			end,
			?assertNot(Newstate#state.ringout),
			?assertEqual(Agent, Newstate#state.oncall_pid),
			?assertEqual(undefined, Newstate#state.ring_pid),
			?assertEqual(undefined, Newstate#state.queue_pid),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"oncall quee to agent request by agent, but callback says no",
		fun() ->
			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
			Callrec = Seedstate#state.callrec,
			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = ringing, statedata = Callrec}),
			{ok, Tref} = timer:send_after(100, timer_lives),
			State = Seedstate#state{ringout = Tref, ring_pid = Agent, queue_pid = Qpid},
			{reply, invalid, Newstate} = handle_call('$gen_media_agent_oncall', {Agent, "tag"}, State),
			receive
				timer_lives ->
					ok
			after 150 ->
				erlang:error(timer_nolives)
			end,
			?assertEqual(Agent, Newstate#state.ring_pid),
			?assertEqual(Qpid, Newstate#state.queue_pid),
			?assertEqual(Tref, Newstate#state.ringout),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"oncall queue to agent requst by whoever with outband media",
		fun() ->
			#state{callrec = Seedcall} = Seedstate = Makestate(),
			Callrec = Seedcall#call{ring_path = outband, media_path = outband},
			{ok, Agent} = agent:start(#agent{login = "testagent", state = ringing, statedata = Callrec}),
			gen_server_mock:expect_call(Qpid, fun({remove, Inpid}, _From, _State) ->
				Inpid = Callrec#call.source,
				ok
			end),
			gen_leader_mock:expect_leader_call(Ammock, fun(_, _, State, _) -> {ok, "testagent", State} end),
			gen_leader_mock:expect_leader_call(Ammock, fun(_, _, _, _) -> ok end),
			gen_event_mock:expect_event(cdr, fun({oncall, Callrec, _Time, "testagent"}, _State) -> ok end),
			{ok, Tref} = timer:send_after(100, timer_lives),
			State = Seedstate#state{callrec = Callrec, ring_pid = Agent, queue_pid = Qpid, ringout = Tref},
			{reply, ok, Newstate} = handle_call('$gen_media_agent_oncall', "from", State),
			receive
				timer_lives -> 
					erlang:error(timer_lives)
			after 150 ->
				ok
			end,
			?assertNot(Newstate#state.ringout),
			?assertEqual(Agent, Newstate#state.oncall_pid),
			?assertEqual(undefined, Newstate#state.queue_pid),
			?assertEqual(undefined, Newstate#state.ring_pid),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"oncall queue to agent request by whoever with outband media, but callback says no",
		fun() ->
			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
			Seedcall = Seedstate#state.callrec,
			gen_event_mock:supplant(cdr, {{cdr, Seedcall#call.id}, []}),
			Callrec = Seedcall#call{ring_path = outband, media_path = outband},
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Callrec}),
			{ok, Tref} = timer:send_after(100, timer_lives),
			State = Seedstate#state{callrec = Callrec, ring_pid = Agent, ringout = Tref, queue_pid = Qpid},
			{reply, invalid, Newstate} = handle_call('$gen_media_agent_oncall', "from", State),
			receive
				timer_lives -> 
					ok
			after 150 ->
				erlang:error(timer_nolives)
			end,
			?assertNot(false =:= State#state.ringout),
			?assertEqual(Agent, State#state.ring_pid),
			?assertEqual(Qpid, State#state.queue_pid),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"late oncall request (ring_pid is undefined)",
		fun() ->
			Seedstate = Makestate(),
			State = Seedstate#state{ring_pid = undefined},
			?assertMatch({reply, invalid, State}, handle_call('$gen_media_agent_oncall', "from", State))
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"oncall request to agent that is no longer running",
		fun() ->
			{ok, Seedstate} = init([dummy_media, [[{queues, none}], success]]),
			Seedcall = Seedstate#state.callrec,
			gen_event_mock:supplant(cdr, {{cdr, Seedcall#call.id}, []}),
			Callrec = Seedcall#call{ring_path = outband, media_path = outband},
			Agent = spawn(fun() -> ok end),
			{ok, Tref} = timer:send_after(100, timer_lives),
			State = Seedstate#state{callrec = Callrec, ring_pid = Agent, ringout = Tref, queue_pid = Qpid},
			gen_event_mock:expect_event(cdr, fun({ringout, Callrec, _Time, _Data}, _) -> ok end),
			gen_leader_mock:expect_leader_call(Ammock, fun(_, _, S, _) -> {ok, notfound, S} end),
			Out = handle_call('$gen_media_agent_oncall', "from", State),
			?assertMatch({reply, invalid, _}, Out),
			{reply, invalid, Newstate} = Out,
			receive
				timer_lives ->
					erlang:error(timer_lives)
			after 150 ->
				ok
			end,
			?assert(false == Newstate#state.ringout),
			?assertEqual(undefined, Newstate#state.ring_pid),
			?assertEqual(Qpid, State#state.queue_pid),
			Assertmocks()
		end}
	end]}.

handle_info_test_() ->
	{foreach,
	fun() ->
		{ok, Seedstate} = init([dummy_media, [[{queues, none}], success]]),
		{Seedstate}
	end,
	fun(_) ->
		ok
	end,
	[fun({#state{callrec = Oldcall} = Seedstate}) ->
		{"agent requests a ring stop",
		fun() ->
			{ok, Apid} = agent:start(#agent{login = "testagent", state = ringing, statedata = Seedstate#state.callrec}),
			{ok, Cook} = gen_server_mock:new(),
			gen_server_mock:expect_cast(Cook, fun(stop_ringing, _State) -> ok end),
			{ok, Am} = gen_leader_mock:start(agent_manager),
			gen_leader_mock:expect_leader_call(Am, fun(_, _, State, _) -> {ok, "testagent", State} end),
			Callrec = Oldcall#call{cook = Cook},
			State = Seedstate#state{ring_pid = Apid, callrec = Callrec},
			{noreply, Newstate} = handle_info({'$gen_media_stop_ring', Apid}, State),
			?assertNot(Newstate#state.ringout),
			?assertEqual(undefined, Newstate#state.ring_pid),
			gen_server_mock:assert_expectations(Cook),
			gen_server_mock:stop(Cook),
			gen_leader_mock:stop(Am)
		end}
	end,
	fun({Seedstate}) ->
		{"ring stop request with no ring_pid defined",
		fun() ->
			{noreply, Newstate} = handle_info({'$gen_media_stop_ring', "doesn't matter"}, Seedstate),
			?assertEqual(Seedstate, Newstate)
		end}
	end,
	fun({Seedstate}) ->
		{"ring stop request with no ringout handled",
		fun() ->
			Pid = spawn(fun() -> ok end),
			State = Seedstate#state{ring_pid = Pid},
			{noreply, Newstate} = handle_info({'$gen_media_stop_ring', "doesn't matter"}, State),
			?assertEqual(State, Newstate)
		end}
	end,
	fun({Seedstate}) ->
		{"ring stop request with a live agent ringing",
		fun() ->
			{ok, Cook} = gen_server_mock:new(),
			gen_server_mock:expect_cast(Cook, fun(stop_ringing, _State) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = ringing, statedata = Seedstate#state.callrec}),
			{ok, Am} = gen_leader_mock:start(agent_manager),
			gen_leader_mock:expect_leader_call(Am, fun(_, _, State, _) -> {ok, "testagent", State} end),
			State = Seedstate#state{ring_pid = Agent, ringout = true},
			{noreply, Newstate} = handle_info({'$gen_media_stop_ring', Cook}, State),
			gen_server_mock:assert_expectations(Cook),
			?assertEqual({ok, idle}, agent:query_state(Agent)),
			?assertNot(Newstate#state.ringout),
			?assertEqual(undefined, Newstate#state.ring_pid),
			gen_leader_mock:stop(Am)
		end}
	end,
	fun({Seedstate}) ->
		{"ring stop request with a live agent in wrong state",
		fun() ->
			{ok, Cook} = gen_server_mock:new(),
			gen_server_mock:expect_cast(Cook, fun(stop_ringing, _State) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Seedstate#state.callrec}),
			{ok, Am} = gen_leader_mock:start(agent_manager),
			gen_leader_mock:expect_leader_call(Am, fun(_, _, State, _) -> {ok, "testagent", State} end),
			State = Seedstate#state{ring_pid = Agent, ringout = true},
			{noreply, Newstate} = handle_info({'$gen_media_stop_ring', Cook}, State),
			gen_server_mock:assert_expectations(Cook),
			?assertEqual({ok, oncall}, agent:query_state(Agent)),
			?assertNot(Newstate#state.ringout),
			?assertEqual(undefined, Newstate#state.ring_pid),
			gen_leader_mock:stop(Am)
		end}
	end,
	fun({Seedstate}) ->
		{"ring stop request with a dead agent",
		fun() ->
			{ok, Cook} = gen_server_mock:new(),
			gen_server_mock:expect_cast(Cook, fun(stop_ringing, _State) -> ok end),
			{ok, Am} = gen_leader_mock:start(agent_manager),
			gen_leader_mock:expect_leader_call(Am, fun(_, _, State, _) -> {ok, "doesn't matter", State} end),
			Agent = spawn(fun() -> ok end),
			State = Seedstate#state{ring_pid = Agent, ringout = true},
			{noreply, Newstate} = handle_info({'$gen_media_stop_ring', Cook}, State),
			gen_server_mock:assert_expectations(Cook),
			?assertNot(Newstate#state.ringout),
			?assertEqual(undefined, Newstate#state.ring_pid),
			gen_leader_mock:stop(Am)
		end}
	end]}.
	
agent_interact_test_() ->
	{foreach,
	fun() ->
		Callrec = #call{id = "testcall", source = self()},
		{ok, Mock} = gen_leader_mock:start(agent_manager),
		gen_leader_mock:expect_leader_call(Mock, fun(_Data, _From, State, _Elec) -> {ok, "testagent", State} end),
		gen_event:start({local, cdr}),
		gen_event:add_handler(cdr, gen_event_mock, []),
		{#agent{login = "testagent"}, Callrec}
	end,
	fun({_Arec, _Callrec}) ->
		Mock = whereis(agent_manager),
		gen_leader_mock:stop(Mock),
		gen_event:stop(cdr),
		timer:sleep(10), % because mocks don't like to die quickly.
		ok
	end,
	[%fun({Arec, Callrecbase}) ->
%		{"mediapush",
%		fun() ->
%			Callrec = Callrecbase#call{media_path = inband},
%			{ok, Apid} = agent:start(Arec#agent{statedata = Callrec, state = oncall}),
%			State = #state{oncall_pid = Apid, callrec = Callrec},
%			Expected = State,
%			?assertEqual(Expected, agent_interact({mediapush, "data", append}, State)),
%			agent:stop(Apid),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
	%fun({Arec, Callrec}) ->
		%{"media push when media_path doesn't match",
		%fun() ->
			%{ok, Apid} = agent:start(Arec#agent{statedata = Callrec, state = oncall}),
			%State = #state{oncall_pid = Apid, callrec = Callrec},
			%Expected = State,
			%agent:stop(Apid),
			%?assertEqual(Expected, agent_interact({mediapush, "data", append}, State)),
			%gen_event_mock:assert_expectations(cdr)
			%ok
		%end}
	%end,
	fun({Arec, Callrec}) ->
		{"stop_ring with a ringout timer going",
		fun() ->
			{ok, Apid} = agent:start(Arec#agent{statedata = Callrec, state = ringing}),
			{ok, Tref} = timer:send_interval(1000, <<"timer">>),
			State = #state{ring_pid = Apid, ringout = Tref, callrec = Callrec},
			gen_event_mock:expect_event(cdr, fun({ringout, Callrec, _Time, {undefined, "testagent"}}, _State) ->
				ok
			end),
			Res = agent_interact(stop_ring, State),
			agent:stop(Apid),
			receive
				<<"timer">> ->
					 erlang:error(timer_lives)
			after 1500 ->
				ok
			end,
			?assertEqual(false, Res#state.ringout),
			?assertEqual(undefined, Res#state.ring_pid),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({Arec, Callrec}) ->
		{"stop_ring with no ringout or ring_pid defined",
		fun() ->
			State = #state{ring_pid = undefined, ringout = false, callrec = Callrec},
			Res = agent_interact(stop_ring, State),
			?assertEqual(State, Res),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({Arec, Callrec}) ->
		{"stop_ring with only ring_pid defined",
		fun() ->
			{ok, Apid} = agent:start(Arec#agent{state = ringing, statedata = Callrec}),
			State = #state{ring_pid = Apid, ringout = false, callrec = Callrec},
			gen_event_mock:expect_event(cdr, fun({ringout, Callrec, _Time, {undefined, "testagent"}}, _State) -> ok end),
			Res = agent_interact(stop_ring, State),
			agent:stop(Apid),
			?assertEqual(undefined, Res#state.ring_pid),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({Arec, Callrec}) ->
		{"stop_ring with only ringout defined",
		fun() ->
			{ok, Tref} = timer:send_interval(1000, <<"timer">>),
			State = #state{ringout = Tref, callrec = Callrec},
			Res = agent_interact(stop_ring, State),
			receive
				<<"timer">>	->
					 erlang:error(timer_lives)
			after 1500 ->
				ok
			end,
			?assertEqual(false, Res#state.ringout),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({Arec, Callrec}) ->
		{"wrapup",
		fun() ->
			{ok, Apid} = agent:start(Arec#agent{state = oncall, statedata = Callrec}),
			State = #state{oncall_pid = Apid, callrec = Callrec},
			gen_event_mock:expect_event(cdr, fun({wrapup, Callrec, _Time, "testagent"}, _State) -> ok end),
			Res = agent_interact(wrapup, State),
			agent:stop(Apid),
			?assertEqual(undefined, Res#state.oncall_pid),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({Arec, Callrec}) ->
		{"hangup when both oncall and ring are pids",
		fun() ->
			{ok, Oncall} = agent:start(Arec#agent{state = oncall, statedata = Callrec}),
			{ok, Ringing} = agent:start(Arec#agent{state = ringing, statedata = Callrec, login = "ringing"}),
			State = #state{oncall_pid = Oncall, ring_pid = Ringing, callrec = Callrec},
			gen_event_mock:expect_event(cdr, fun({wrapup, Callrec, _Time, "testagent"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({hangup, Callrec, _Time, "Unknown Unknown"}, _State) -> ok end),
			Res = agent_interact(hangup, State),
			agent:stop(Oncall),
			agent:stop(Ringing),
			?assertEqual(undefined, Res#state.oncall_pid),
			?assertEqual(undefined, Res#state.ring_pid),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({Arec, Callrec}) ->
		{"hang up when only oncall is a pid",
		fun() ->
			{ok, Apid} = agent:start(Arec#agent{state = oncall, statedata = Callrec}),
			State = #state{oncall_pid = Apid, callrec = Callrec},
			gen_event_mock:expect_event(cdr, fun({wrapup, Callrec, _Time, "testagent"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({hangup, Callrec, _Time, "Unknown Unknown"}, _State) -> ok end),
			Res = agent_interact(hangup, State),
			agent:stop(Apid),
			?assertEqual(undefined, Res#state.oncall_pid),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({Arec, Callrec}) ->
		{"hang up when only ringing is a pid",
		fun() ->
			{ok, Apid} = agent:start(Arec#agent{state = ringing, statedata = Callrec}),
			gen_event_mock:expect_event(cdr, fun({hangup, Callrec, _Time, "Unknown Unknown"}, _State) -> ok end),
			State = #state{ring_pid = Apid, callrec = Callrec},
			Res = agent_interact(hangup, State),
			agent:stop(Apid),
			?assertEqual(undefined, Res#state.ring_pid),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({Arec, Callrec}) ->
		{"hang up when only queue is a pid",
		fun() ->
			{ok, Qpid} = gen_server_mock:new(),
			gen_server_mock:expect_call(Qpid, fun({remove, Incpid}, _From, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({hangup, Callrec, _Time, "Unknown Unknown"}, _State) -> ok end),
			State = #state{queue_pid = Qpid, callrec = Callrec},
			Res = agent_interact(hangup, State),
			?assertEqual(undefined, Res#state.queue_pid),
			gen_server_mock:assert_expectations(Qpid),
			gen_server_mock:stop(Qpid),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({_Arec, _Callrec}) ->
		{"orphaned call, or just not yet queued",
		fun() ->
			Res = agent_interact(hangup, #state{}),
			?assertEqual(#state{}, Res),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({_Arec, Callrec}) ->
		{"dead agent pid doesn't cause crash",
		fun() ->
			State = #state{ring_pid = spawn(fun() -> ok end), callrec = Callrec},
			gen_event_mock:expect_event(cdr, fun({ringout, Callrec, _Time, {undefined, "testagent"}}, _) -> ok end),
			Res = agent_interact(stop_ring, State),
			?assertEqual(undefined, Res#state.ring_pid),
			gen_event_mock:assert_expectations(cdr)
		end}
	end]}.

outgoing_test_() ->
	{foreach,
	fun() ->
		{ok, Apid} = agent:start(#agent{login = "testagent", state = precall, statedata = "clientrec"}),
		{ok, Ammock} = gen_leader_mock:start(agent_manager),
		gen_event:start({local, cdr}),
		gen_event:add_handler(cdr, gen_event_mock, []),
		{Apid, Ammock}
	end,
	fun({Apid, Ammock}) ->
		agent:stop(Apid),
		gen_leader_mock:stop(Ammock),
		gen_event:stop(cdr),
		timer:sleep(10)
	end,
	[fun({Apid, Ammock}) ->
		{"set agent outbound with known call, and agent exists",
		fun() ->
			gen_leader_mock:expect_leader_call(Ammock, fun({exists, "testagent"}, _From, State, _Elec) ->
				{ok, {true, Apid}, State}
			end),
			gen_event_mock:expect_event(cdr, fun({oncall, Callrec, _Time, "testagent"}, _State) -> ok end),
			State = #state{callrec = #call{id = "testcall", source = self()}},
			{ok, Res} = outgoing({outbound, "testagent", "newsubstate"}, State),
			?assertEqual(Apid, Res#state.oncall_pid),
			?assertEqual("newsubstate", Res#state.substate),
			gen_leader_mock:assert_expectations(Ammock),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({_Apid, Ammock}) ->
		{"set agent outbound with known call, but agent doesn't exist",
		fun() ->
			gen_leader_mock:expect_leader_call(Ammock, fun({exists, "testagent"}, _From, State, _Elec) ->
				{ok, false, State}
			end),
			State = #state{callrec = #call{id = "testcall", source = self()}},
			Res = outgoing({outbound, "testagent", "newsubstate"}, State),
			?assertMatch({{error, {noagent, "testagent"}}, _Newstate}, Res),
			{_, Newstate} = Res,
			?assertEqual("newsubstate", Newstate#state.substate),
			gen_leader_mock:assert_expectations(Ammock),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({Apid, Ammock}) ->
		{"set agent outbound with a new callrec",
		fun() ->
			gen_leader_mock:expect_leader_call(Ammock, fun({exists, "testagent"}, _From, State, _Elec) ->
				{ok, {true, Apid}, State}
			end),
			gen_event_mock:expect_event(cdr, fun({oncall, Callrec, _Time, "testagent"}, _State) -> ok end),
			Callrec = #call{id = "testcall", source = self()},
			State = #state{},
			{ok, Res} = outgoing({outbound, "testagent", Callrec, "newsubstate"}, State),
			?assertEqual(Apid, Res#state.oncall_pid),
			?assertEqual(Callrec, Res#state.callrec),
			?assertEqual("newsubstate", Res#state.substate),
			gen_leader_mock:assert_expectations(Ammock),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({Apid, Ammock}) ->
		{"set agent outbound iwth a new call rec, but agent doesn't exist",
		fun() ->
			gen_leader_mock:expect_leader_call(Ammock, fun({exists, "testagent"}, _From, State, _Elec) ->
				{ok, false, State}
			end),
			Callrec = #call{id = "testcall", source = self()},
			State = #state{},
			Res = outgoing({outbound, "testagent", Callrec, "newsubstate"}, State),
			?assertMatch({{error, {noagent, "testagent"}}, _State}, Res),
			{_, Newstate} = Res,
			?assertEqual("newsubstate", Newstate#state.substate),
			?assertEqual(Callrec, Newstate#state.callrec),
			gen_leader_mock:assert_expectations(Ammock),
			gen_event_mock:assert_expectations(cdr)
		end}
	end]}.

priv_queue_test_() ->
	{foreach,
	fun() ->
		{ok, QMpid} = gen_leader_mock:start(queue_manager),
		{ok, Qpid} = gen_server_mock:new(),
		Callrec = #call{id = "testcall", source = self()},
		Assertmocks = fun() ->
			gen_leader_mock:assert_expectations(QMpid),
			gen_server_mock:assert_expectations(Qpid)
		end,
		{QMpid, Qpid, Callrec, Assertmocks}
	end,
	fun({QMpid, Qpid, _Callrec, _Mocks}) ->
		gen_leader_mock:stop(QMpid),
		gen_server_mock:stop(Qpid),
		timer:sleep(10),
		ok
	end,
	[fun({QMpid, Qpid, Callrec, Mocks}) ->
		{"All is well",
		fun() ->
			gen_leader_mock:expect_leader_call(QMpid, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
				{ok, Qpid, State}
			end),
			gen_server_mock:expect_call(Qpid, fun({add, 40, Inpid, Callrec}, _From, _State) -> ok end),
			?assertEqual(Qpid, priv_queue("testqueue", Callrec, "doesn't matter")),
			Mocks()
		end}
	end,
	fun({QMpid, _Qpid, Callrec, Mocks}) ->
		{"failover is false",
		fun() ->
			gen_leader_mock:expect_leader_call(QMpid, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
				{ok, undefined, State}
			end),
			?assertEqual(invalid, priv_queue("testqueue", Callrec, false)),
			Mocks()
		end}
	end,
	fun({QMpid, Qpid, Callrec, Mocks}) ->
		{"failover is true",
		fun() ->
			gen_leader_mock:expect_leader_call(QMpid, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
				{ok, undefined, State}
			end),
			gen_leader_mock:expect_leader_call(QMpid, fun({get_queue, "default_queue"}, _From, State, _Elec) ->
				{ok, Qpid, State}
			end),
			gen_server_mock:expect_call(Qpid, fun({add, 40, Inpid, Callrec}, _From, _State) -> ok end),
			?assertEqual({default, Qpid}, priv_queue("testqueue", Callrec, true)),
			Mocks()
		end}
	end]}.

-endif.
