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
%%		All required data.
%%
%%		Some media may not be able to provide a call record on start-up, thus
%%		allowing the media to finish prepping and then queue later.
%%
%%	<b>handle_ring(Agent, Call, State) -> Result</b>
%%		types:	Agent = pid()
%%				Call = #call{}
%%				State = any()
%%				Result = {ok, NewState} | {invalid, NewState}
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
%%		If Result is {ok, NewState}, Agent is set to ringing, and execution 
%%		continues with NewState.
%%
%%		If Result is {invalid, NewState}, Agent is set to idle, and execution
%%		continues with NewState.
%%
%%	<b>handle_ring_stop(State) -> Result</b>
%%		types:	State = any()
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
%%		the agent is set to oncall.  Execution then continues with NewState.
%%
%%		If Result is {error, Error, NewState}, the agent's state is not changed
%%		And execution continues with NewState.
%%
%%	<b>handle_voicemail(State) -> Result</b>
%%		types:	State = any()
%%				Result = {ok, NewState} | {invalid, NewState}
%%
%%		When a media should be removed from queue and moved to voicemail, this
%%		is called.
%%
%%		State is the internal state of the gen_media callbacks.
%%
%%		If Result is {ok, NewState}, the call is removed from queue and 
%%		execution continues with NewState.
%%
%%		If Result is {invalid, NewState} execution continues with NewState.
%%
%%	<b>handle_annouce(Announce, State) -> {ok, NewState}</b>
%%		types:	Announce = any()
%%				State = NewState = any()
%%
%%		When a recipe calls for a call in queue to play an announcement, this
%%		function is called.  Execution then continues with NewState.
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
%%	<b>handle_queue_transfer(State) -> {ok, NewState}</b>
%%		types:	State = NewState = any()
%%
%%		When a media is placed back into queue from an agent, this is called
%%		To allow the media to do any required clean up or unbridging.
%%		Execution then continues with NewState.
%%
%%	<b>handle_wrapup(State) -> {Finality, NewState}</b>
%%		types:	State = NewState = any()
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
%%	{Agentaction, NewState}
%%	{Agentaction, Reply, NewState}
%%		types:	Agentaction = stop_ring | wrapup | hangup
%%				Reply = any()
%%				NewState = any()
%%
%%		This result is only valid if an agent has been associated with this 
%%		media by ringing.  The second form is only valid if the request came in
%%		as a gen_media:call.  This attempts to take the specified action on the 
%%		agent, the continues execution with NewState.
%%
%%		stop_ring is used to stop the gen_media from handling a ringout.  It 
%%		does not change the agent's state.  Execution will continue with 
%%		NewState.  This is useful if there is an error ringing to an agent that
%%		only becomes apparent at a later time.
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
	agent_transfer/3,
	queue/2,
	call/2,
	call/3,
	cast/2,
	wrapup/1
]).

-record(state, {
	callback,
	substate,
	callrec,
	ring_pid,
	oncall_pid,
	%agent_pid,
	queue_pid,
	ringout
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
	lists:append(GS, [
		{handle_ring, 3},
		{handle_ring_stop, 1},
		{handle_answer, 3}, 
		{handle_voicemail, 1}, 
		{handle_announce, 2}, 
		{handle_agent_transfer, 4},
		{handle_queue_transfer, 1},
		{handle_wrapup, 1}
	]);
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
	gen_server:call(Genmedia, {'$gen_media_annouce', Annouce}).

%% @doc Sends the oncall agent associated with the call to wrapup; or, if it's
%% the oncall agent making the request, gives the callback module a chance to
%% handle it.
-spec(wrapup/1 :: (Genmedia :: pid()) -> 'ok' | 'invalid').
wrapup(Genmedia) ->
	gen_server:call(Genmedia, '$gen_media_wrapup').

%% @doc Send a stop ringing message to `pid() Genmedia'.
-spec(stop_ringing/1 :: (Genmedia :: pid()) -> 'ok').
stop_ringing(Genmedia) ->
	Genmedia ! '$gen_media_stop_ring',
	ok.

%% @doc Set the agent associated with `pid() Genmedia' to oncall.
-spec(oncall/1 :: (Genmedia :: pid()) -> 'ok' | 'invalid').
oncall(Genmedia) ->
	gen_server:call(Genmedia, '$gen_media_agent_oncall').

%% @doc Transfer the call from the agent it is associated with to a new agent.
-spec(agent_transfer/3 :: (Genmedia :: pid(), Apid :: pid(), Timeout :: pos_integer()) -> 'ok' | 'invalid').
agent_transfer(Genmedia, Apid, Timeout) ->
	gen_server:call(Genmedia, {'$gen_media_agent_transfer', Apid, Timeout}).

%% @doc Transfer the passed media into the given queue.
-spec(queue/2 :: (Genmedia :: pid(), Queue :: string()) -> 'ok' | 'invalid').
queue(Genmedia, Queue) ->
	gen_server:call(Genmedia, {'$gen_media_queue', Queue}).

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
-spec(start_link/2 :: (Callback :: atom(), Args :: any()) -> {'ok', pid()} | {'error', any()}).
start_link(Callback, Args) ->
    gen_server:start_link(?MODULE, [Callback, Args], []).

-spec(start/2 :: (Callback :: atom(), Args :: any()) -> {'ok', pid()} | {'error', any()}).
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
		{ok, {Substate, {Queue, Callrec}}} when is_record(Callrec, call) ->
			cdr:cdrinit(Callrec),
			Qpid = case priv_queue(Queue, Callrec) of
				invalid when Queue =/= "default_queue" ->
					priv_queue("default_queue", Callrec),
					cdr:inqueue(Callrec, "default_queue");
				Else ->
					Else
			end,
			{ok, #state{callback = Callback, substate = Substate, callrec = Callrec#call{source = self()}, queue_pid = Qpid}};
		{ok, {Substate, Callrec}} when is_record(Callrec, call) ->
			cdr:cdrinit(Callrec),
			{ok, #state{callback = Callback, substate = Substate, callrec = Callrec#call{source = self()}}}
	end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------

%% @private
handle_call('$gen_media_wrapup', {Ocpid, _Tag}, #state{callback = Callback, oncall_pid = Ocpid, callrec = Call} = State) when Call#call.media_path =:= inband ->
	?INFO("Request to end call from agent", []),
	cdr:wrapup(State#state.callrec, Ocpid),
	case Callback:handle_wrapup(State#state.substate) of
		{ok, NewState} ->
			{reply, ok, State#state{oncall_pid = undefined, substate = NewState}};
		{hangup, NewState} ->
			cdr:hangup(State#state.callrec, agent),
			{stop, normal, ok, State#state{oncall_pid = undefined, substate = NewState}}
	end;
handle_call('$gen_media_wrapup', {Ocpid, _Tag}, #state{oncall_pid = Ocpid} = State) ->
	?ERROR("Cannot do a wrapup directly unless mediapath is inband, and request is from agent oncall.", []),
	{reply, invalid, State};
%handle_call('$gen_media_wrapup', _From, #state{callback = Callback, oncall_pid = Ocpid} = State) when is_pid(Ocpid) ->
%	case agent:set_state(Ocpid, wrapup, State#state.callrec) of
%		ok ->
%			{ok, NewState} = Callback:handle_wrapup(State#state.substate),
%			cdr:wrapup(State#state.callrec, Ocpid),
%			{reply, ok, State#state{oncall_pid = undefined, substate = NewState}};
%		Else ->
%			{reply, invalid, State}
%	end;
handle_call({'$gen_media_queue', Queue}, {Ocpid, _Tag}, #state{callback = Callback, oncall_pid = Ocpid} = State) ->
	?INFO("requat to queue call from agent", []),
	case priv_queue(Queue, State#state.callrec) of
		invalid ->
			{reply, invalid, State};
		Qpid when is_pid(Qpid) ->
			{ok, NewState} = Callback:handle_queue_transfer(State#state.substate),
			cdr:inqueue(State#state.callrec, Queue),
			{reply, ok, State#state{substate = NewState, oncall_pid = undefined}}
	end;
handle_call({'$gen_media_queue', Queue}, From, #state{callback = Callback} = State) when is_pid(State#state.oncall_pid) ->
	?INFO("Request to queue from ~p", [From]),
	case priv_queue(Queue, State#state.callrec) of
		invalid ->
			{reply, invalid, State};
		Qpid when is_pid(Qpid) ->
			agent:set_state(State#state.oncall_pid, wrapup, State#state.callrec),
			{ok, NewState} = Callback:handle_queue_transfer(State#state.substate),
			cdr:inqueue(State#state.callrec, Queue),
			{reply, ok, State#state{substate = NewState, oncall_pid = undefined}}
	end;
handle_call('$gen_media_get_call', _From, State) ->
	{reply, State#state.callrec, State};
handle_call({'$gen_media_ring', Agent, QCall, Timeout}, _From, #state{callrec = Call, callback = Callback} = State) ->
	% TODO set the cook to the call directly
	?INFO("Trying to ring ~p with timeout ~p", [Agent, Timeout]),
	case agent:set_state(Agent, ringing, Call#call{cook=QCall#queued_call.cook}) of
		ok ->
			case Callback:handle_ring(Agent, State#state.callrec, State#state.substate) of
				{ok, Substate} ->
					{ok, Tref} = timer:send_after(Timeout, {'$gen_media_stop_ring', QCall#queued_call.cook}),
					cdr:ringing(Call, Agent),
					{reply, ok, State#state{substate = Substate, ring_pid = Agent, ringout=Tref}};
				{invalid, Substate} ->
					agent:set_state(Agent, idle),
					{reply, invalid, State#state{substate = Substate}}
			end;
		Else ->
			?INFO("Agent ringing response:  ~p", [Else]),
			{reply, invalid, State}
	end;
handle_call({'$gen_media_agent_transfer', Apid}, _From, #state{oncall_pid = Apid} = State) ->
	?NOTICE("Can't transfer to yourself, silly ~p!", [Apid]),
	{reply, invalid, State};
handle_call({'$gen_media_agent_transfer', Apid, Timeout}, _From, #state{callback = Callback, ring_pid = undefined, oncall_pid = Ocpid} = State) when is_pid(Ocpid) ->
	case agent:set_state(Apid, ringing, State#state.callrec) of
		ok ->
			case Callback:handle_agent_transfer(Apid, State#state.callrec, Timeout, State#state.substate) of
				{ok, NewState} ->
					{ok, Tref} = timer:send_after(Timeout, {'$gen_media_stop_ring', dummy}),
					cdr:agent_transfer(State#state.callrec, {Ocpid, Apid}),
					cdr:ringing(State#state.callrec, Apid),
					{reply, ok, State#state{ring_pid = Apid, ringout = Tref, substate = NewState}};
				{error, Error, NewState} ->
					?NOTICE("Could not set agent ringing for transfer due to ~p", [Error]),
					agent:set_state(Apid, idle),
					{reply, invalid, State#state{substate = NewState}}
			end;
		invalid ->
			?NOTICE("Could not ring to target agent ~p", [Apid]),
			{reply, invalid, State}
	end;
handle_call({'$gen_media_agent_transfer', _Apid, _Timeout}, _From, State) ->
	?ERROR("Invalid agent transfer sent when state is ~p.", [State]),
	{reply, invalid, State};
handle_call({'$gen_media_annouce', Annouce}, _From, #state{callback = Callback} = State) ->
	?INFO("Doing announce", []),
	{ok, Substate} = Callback:handle_announce(Annouce, State),
	{reply, ok, State#state{substate = Substate}};
handle_call('$gen_media_voicemail', _From, #state{callback = Callback} = State) when is_pid(State#state.queue_pid) ->
	?INFO("trying to send media to voicemail", []),
	{Res, Substate} = Callback:handle_voicemail(State#state.substate),
	call_queue:remove(State#state.queue_pid, self()),
	{reply, Res, State#state{substate = Substate}};
handle_call('$gen_media_voicemail', _From, #state{queue_pid = undefined} = State) ->
	?ERROR("voicemail only valid when the media is queued", []),
	{reply, invalid, State};
handle_call('$gen_media_agent_oncall', {Apid, _Tag}, #state{ring_pid = Apid, callrec = #call{ring_path = outband}} = State) ->
	?INFO("Cannot accept on call requests from agent (~p) unless ring_path is inband", [Apid]),
	{reply, invalid, State};
handle_call('$gen_media_agent_oncall', {Rpid, _Tag}, #state{ring_pid = Rpid, callback = Callback, oncall_pid = Ocpid, callrec = #call{ring_path = inband}} = State) when is_pid(Ocpid) ->
	?INFO("oncall request during what looks like an agent transfer (inband)", []),
	case Callback:handle_answer(Rpid, State#state.callrec, State#state.substate) of
		{ok, NewState} ->
			%agent:set_state(Rpid, oncall, State#state.callrec),
			cdr:oncall(State#state.callrec, Rpid),
			timer:cancel(State#state.ringout),
			agent:set_state(Ocpid, wrapup, State#state.callrec),
			cdr:wrapup(State#state.callrec, Ocpid),
			{reply, ok, State#state{substate = NewState, ringout = false, oncall_pid = Rpid, ring_pid = undefined}};
		{error, Reason, NewState} ->
			?ERROR("Cannot set ~p to oncall due to ~p", [Rpid, Reason]),
			{reply, invalid, State#state{substate = NewState}}
	end;
handle_call('$gen_media_agent_oncall', _From, #state{ring_pid = Rpid, callback = Callback, oncall_pid = Ocpid} = State) when is_pid(Ocpid) ->
	?INFO("oncall request during what looks like an agent transfer (outofband)", []),
	case Callback:handle_answer(Rpid, State#state.callrec, State#state.substate) of
		{ok, NewState} ->
			agent:set_state(Rpid, oncall, State#state.callrec),
			cdr:oncall(State#state.callrec, Rpid),
			timer:cancel(State#state.ringout),
			agent:set_state(Ocpid, wrapup, State#state.callrec),
			cdr:wrapup(State#state.callrec, Ocpid),
			{reply, ok, State#state{substate = NewState, ringout = false, oncall_pid = Rpid, ring_pid = undefined}};
		{error, Reason, NewState} ->
			?ERROR("Cannot set ~p to oncall due to ~p", [Rpid, Reason]),
			{reply, invalid, State#state{substate = NewState}}
	end;
handle_call('$gen_media_agent_oncall', {Apid, _Tag}, #state{callback = Callback, ring_pid = Apid, callrec = #call{ring_path = inband}} = State) ->
	?INFO("oncall request from agent ~p", [Apid]),
	case Callback:handle_answer(Apid, State#state.callrec, State#state.substate) of
		{ok, NewState} ->
			timer:cancel(State#state.ringout),
			unqueue(State#state.queue_pid, self()),
			cdr:oncall(State#state.callrec, Apid),
			{reply, ok, State#state{substate = NewState, ringout = false, queue_pid = undefined, ring_pid = undefined, oncall_pid = Apid}};
		{error, Reason, NewState} ->
			?ERROR("Could not set ~p on call due to ~p", [Apid, Reason]),
			{reply, invalid, State#state{substate = NewState}}
	end;

handle_call('$gen_media_agent_oncall', From, #state{ring_pid = Apid, callback = Callback} = State) ->
	?INFO("oncall request from ~p; agent to set on call is ~p", [From, Apid]),
	case Callback:handle_answer(Apid, State#state.callrec, State#state.substate) of
		{ok, NewState} ->
			agent:set_state(Apid, oncall, State#state.callrec),
			cdr:oncall(State#state.callrec, Apid),
			timer:cancel(State#state.ringout),
			call_queue:remove(State#state.queue_pid, self()),
			{reply, ok, State#state{substate = NewState, ringout = false, queue_pid = undefined, oncall_pid = Apid, ring_pid = undefined}};
		{error, Reason, NewState} ->
			?ERROR("Could not set ~p on call due to ~p", [Apid, Reason]),
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
			{stop, Reason, State#state{substate = NewState}};
		{queue, Queue, Callrec, NewState} ->
			case priv_queue(Queue, Callrec) of
				invalid ->
					{reply, {error, {noqueue, Queue}}, State#state{callrec = Callrec, substate = NewState}};
				Qpid ->
					cdr:cdrinit(Callrec),
					cdr:inqueue(Callrec, Queue),
					{reply, ok, State#state{callrec = Callrec, substate = NewState, queue_pid = Qpid}}
			end;
		Tuple when element(1, Tuple) =:= outbound ->
			{Reply, NewState} = outgoing(Tuple, State),
			{reply, Reply, NewState};
		{Agentact, Reply, NewState} when is_pid(State#state.oncall_pid) or is_pid(State#state.ring_pid) ->
			Midstate = agent_interact(Agentact, State),
			{reply, Reply, Midstate#state{substate = NewState}}
	end.
	
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%--------------------------------------------------------------------

%% @private
handle_cast(Msg, #state{callback = Callback} = State) ->
	case Callback:handle_cast(Msg, State#state.substate) of
		{noreply, NewState} ->
			{noreply, State#state{substate = NewState}};
		{noreply, NewState, Timeout} ->
			{noreply, State#state{substate = NewState}, Timeout};
		{stop, Reason, NewState} ->
			{stop, Reason, State#state{substate = NewState}};
		{queue, Queue, Callrec, NewState} ->
			case priv_queue(Queue, Callrec) of
				invalid ->
					{noreply, State#state{callrec = Callrec, substate = NewState}};
				Qpid ->
					cdr:cdrinit(Callrec),
					cdr:inqueeu(Callrec, Queue),
					{noreply, State#state{callrec = Callrec, substate = NewState, queue_pid = Qpid}}
			end;
		Tuple when element(1, Tuple) =:= outbound ->
			{_Reply, NewState} = outgoing(Tuple, State),
			{noreply, NewState};
		{Agentact, NewState} when is_pid(State#state.oncall_pid) or is_pid(State#state.ring_pid) ->
			Midstate = agent_interact(Agentact, State),
			{noreply, Midstate#state{substate = NewState}}
	end.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%--------------------------------------------------------------------

%% @private
handle_info({'$gen_media_stop_ring', _Cook}, #state{ring_pid = undefined} = State) ->
	?NOTICE("No agent is ringing for this call", []),
	{noreply, State};
handle_info({'$gen_media_stop_ring', _Cook}, #state{ringout = false} = State) ->
	?NOTICE("Ringout is set not to be handled", []),
	{noreply, State};
handle_info({'$gen_media_stop_ring', Cook}, #state{ring_pid = Apid, callback = Callback} = State) when is_pid(Apid) ->
	?INFO("Handling ringout...", []),
	agent:set_state(Apid, idle),
	gen_server:cast(Cook, stop_ringing),
	{ok, Newsub} = Callback:handle_ring_stop(State#state.substate),
	{noreply, State#state{substate = Newsub, ringout = false, ring_pid = undefined}};
handle_info(Info, #state{callback = Callback} = State) ->
	?DEBUG("Other info message, going directly to callback.  ~p", [Info]),
	case Callback:handle_info(Info, State#state.substate) of
		{noreply, NewState} ->
			{noreply, State#state{substate = NewState}};
		{noreply, NewState,  Timeout} ->
			{noreply, State#state{substate = NewState}, Timeout};
		{stop, Reason, NewState} ->
			{stop, Reason, State#state{substate = NewState}};
		{queue, Queue, Callrec, NewState} ->
			case priv_queue(Queue, Callrec) of
				invalid ->
					{noreply, State#state{callrec = Callrec, substate = NewState}};
				Qpid ->
					cdr:cdrinit(Callrec),
					cdr:inqueue(Callrec, Queue),
					{noreply, State#state{callrec = Callrec, substate = NewState, queue_pid = Qpid}}
			end;
		Tuple when element(1, Tuple) =:= outbound ->
			{_Reply, NewState} = outgoing(Tuple, State),
			{noreply, NewState};
		{Interact, NewState} when is_pid(State#state.oncall_pid) or is_pid(State#state.ring_pid) ->
			Midstate = agent_interact(Interact, State),
			{noreply, Midstate#state{substate = NewState}}
	end.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------

%% @private
terminate(Reason, #state{callback = Callback, queue_pid = undefined, oncall_pid = undefined, ring_pid = undefined} = State) ->
	Callback:terminate(Reason, State#state.substate);
terminate(Reason, #state{callback = Callback, queue_pid = Qpid, oncall_pid = Ocpid, ring_pid = Rpid} = State) ->
	?NOTICE("gen_media termination due to ~p", [Reason]),
	?INFO("Qpid ~p  oncall ~p  ring ~p", [Qpid, Ocpid, Rpid]),
	agent_interact(hangup, State),
	Callback:terminate(Reason, State#state.substate).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------

%% @private
code_change(OldVsn, #state{callback = Callback} = State, Extra) ->
	{ok, Newsub} = Callback:code_change(OldVsn, State#state.substate, Extra),
    {ok, State#state{substate = Newsub}}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

priv_queue(Queue, Callrec) ->
	case queue_manager:get_queue(Queue) of
		undefined ->
			% TODO what to do w/ the call w/ no queue?
			?WARNING("Uh oh, no queue of ~p", [Queue]),
			invalid;
		Qpid ->
			?DEBUG("Trying to add to queue...", []),
			R = call_queue:add(Qpid, self(), Callrec),
			?DEBUG("q response:  ~p", [R]),
			?INFO("Queueing call ~s into ~s", [Callrec#call.id, Queue]),
			Qpid
	end.

unqueue(undefined, _Callpid) ->
	ok;
unqueue(Qpid, Callpid) when is_pid(Qpid) ->
	call_queue:remove(Qpid, Callpid),
	ok.

agent_interact(stop_ring, #state{ring_pid = Apid} = State) when State#state.ringout =/= false ->
	?INFO("stop_ring", []),
	timer:cancel(State#state.ringout),
	agent:set_state(Apid, idle),
	State#state{ringout = false, ring_pid = undefined};
agent_interact(wrapup, #state{oncall_pid = Apid} = State) ->
	?INFO("Attempting to set agent at ~p to wrapup", [Apid]),
	agent:set_state(Apid, wrapup, State#state.callrec),
	cdr:wrapup(State#state.callrec, Apid),
	State#state{oncall_pid = undefined};
agent_interact(hangup, #state{oncall_pid = Oncallpid, ring_pid = Ringpid} = State) when is_pid(Oncallpid), is_pid(Ringpid) ->
	?INFO("hangup when both oncall and ring are pids", []),
	agent:set_state(Ringpid, idle),
	agent:set_state(Oncallpid, wrapup, State#state.callrec),
	cdr:wrapup(State#state.callrec, Oncallpid),
	State#state{oncall_pid = undefined, ring_pid = undefined};
agent_interact(hangup, #state{oncall_pid = Apid} = State) when is_pid(Apid) ->
	?INFO("hangup when only oncall is a pid", []),
	agent:set_state(Apid, wrapup, State#state.callrec),
	cdr:wrapup(State#state.callrec, Apid),
	State#state{oncall_pid = undefined};
agent_interact(hangup, #state{ring_pid = Apid} = State) when is_pid(Apid) ->
	?INFO("hangup when only ringing is a pid", []),
	agent:set_state(Apid, idle),
	State#state{ring_pid = undefined};
agent_interact(hangup, #state{queue_pid = Qpid, callrec = Call} = State) when is_pid(Qpid) ->
	?INFO("hang up when only queue is a pid", []),
	unqueue(Qpid, self()),
	cdr:hangup(State#state.callrec, Call#call.callerid),
	State#state{queue_pid = undefined}.

outgoing({outbound, Agent, NewState}, State) when is_record(State#state.callrec, call) ->
	?INFO("Told to set ~s to outbound", [Agent]),
	case agent_manager:query_agent(Agent) of
		{true, Apid} ->
			agent:set_state(Apid, outgoing, State#state.callrec),
			{ok, State#state{oncall_pid = Apid, substate = NewState}};
		false ->
			?ERROR("Agent ~s doesn't exists; can't set outgoing", [Agent]),
			{{error, {noagent, Agent}}, State#state{substate = NewState}}
	end;
outgoing({outbound, Agent, Call, NewState}, State) when is_record(Call, call), State#state.callrec =:= undefined ->
	?INFO("Told to set ~s to outbound and call ~p", [Agent, Call]),
	case agent_manager:query_agent(Agent) of
		{true, Apid} ->
			agent:set_state(Apid, outgoing, Call),
			{ok, State#state{callrec = Call, substate = NewState, oncall_pid = Apid}};
		false ->
			?ERROR("Agent ~s doesn't exists; can't set outgoing", [Agent]),
			{{error, {noagent, Agent}}, State#state{substate = NewState, callrec = Call}}
	end.

-ifdef(EUNIT).



-endif.
