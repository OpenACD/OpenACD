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
%%	Micah Warren <micahw at lordnull dot com>
%%

%% @doc Behaviour module for media types.  Gen_media uses gen_fsm as the
%% underlying framework for what it does.  It exposes a gen_server-esque
%% behavior for it's callback modules, however.  Any time gen_media 
%% recieves an event it cannot handle wholly intnerally, it will call a
%% specific funciton of the callback module.
%%
%% Replies from handle_call, handle_cast, and handle_info are extended to
%% allow for specific events only media would need.
%%
%%	Callback functions:
%%
%%	The callback functions follow a general pattern for thier arguments
%%	(aside from init).  It is:
%%		[Arg1, Arg2, Arg3, ..., ArgN, StateName, Call, InternalState, State]
%%
%%	Arg1 ... ArgN are arbitrary terms defined in the documenation for
%%	each callback.
%%
%%	StateName is the current state of the gen_media fsm.  The states most
%%	commonly used are inivr, inqueue, inqueue_ringing, oncall,
%%	oncall_ringing, and wrapup.
%%
%%	Call is the most recently #call{}.
%%
%%	InternalState is the internal state record of the gen_media fsm with
%%	the most pertinant data.  These are defined in gen_media.hrl.
%%
%%	State is the state the callback module last returned from a callback
%%	function.  It is used for implementation specific data for medias.
%%
%%	<b>init(Args) -> {ok, {State, Route_hint}}</b>
%%		types:  Args = any()
%%				State = any()
%%				Route_hint = {Queue, #call{}} | undefined | #call{}
%%					Queue = string()
%%
%%		When gen_media starts, this function is called.  It should 
%%		initialize all required data.
%%
%%		Some media may not be able to provide a call record on start-up, 
%%		thus allowing the media to finish prepping and then queue later.
%%
%%	<b>urlpop_getvars(State) -> UrlOptions</b>
%%		types:	State = any()
%%				UrlOptions = [{string(), string()}]
%%
%%		When a call rings to an agent, if a pop url is configured, get 
%%		variables are appended to the end.  If this is set, 
%%		get_url_getvars/1 will call it and merge the results to what will 
%%		be used when ringing an agent.  Options set via set_url_getvars/2 
%%		super-ceede those returned by the callback module.
%%
%%		At the time of this doc, the agent web interface prompts the agent
%%		to adjust the url pop options when doing a queue transfer.
%%
%%	<b>prepare_endpoint(Agent, Data) -> Result</b>
%%		types:	Agent = #agent{}
%%				Data = 'inband' | any()
%%				Result = {ok, NewData} | {error, Error}
%%					NewData = any()
%%					Error = any()
%%
%%		When an agent is given a new endpoint for the callback module, this
%%		function is called.  If the callback module returns {error, Error} 
%% 		The agent does not store the given endpoint data.  If {ok, NewData}
%% 		is returned, NewData is stored for the endpoint.
%%
%%		The atom 'inband' indicates an agent will go ringing despite the
%%		presense or absence of a ring pid.  If this behavior is not desired,
%%		Module:prepare_endpoint/2 should return {error, any()}, preserving
%%		any settings in place already.  If there were no settings, the
%%		endpoint is no longer used, and any media requiring it will fail
%%		to ring to the agent.
%%
%%	<b>handle_ring(RingData, Agent, Call, State) -> Result</b>
%%		types:	RingData = any()
%%				Agent = pid()
%%				Call = #call{}
%%				State = any()
%%				Result = {ok, NewState} | {ok, UrlOptions, NewState} | 
%%					{invalid, NewState}
%%					UrlOptions = [{string(), string()}]
%%					NewState = any()
%%
%%		When a call must ring to an agent (either due to out of queue or 
%%		the start of a transfer), this is called.
%%
%%		RingData is the data the ring channel returned when confirming it is
%%		able to function.  Gen media does not alter or cache it.
%%
%%		Agent is the pid of the agent that will be set to ringing if 
%%		Result is {ok, NewState} or {ok, UrlOptions, NewState}.
%%
%%		Call is the #call{} maintained by the gen_media and passed in for 
%%		Reference.
%%		
%%		State is the internal state of the gen_media callbacks.
%%
%%		If Result is {ok, NewState} or {ok, UrlOptions, NewState}, Agent 
%%		is set to ringing, and execution continues with NewState.  A 
%%		url_pop is sent to the agent is the client for the media is set to 
%%		have one.  In the case of {ok, UrlOptions, NewState}, the 
%%		UrlOptions are appened to the url as a query (get) string.
%%
%%		Note that UrlOptions can be set by the agent before a queue 
%%		transfer occurs.  In this case, before the transfer is made, 
%%		urlpop_getvars/1 should be used to present the agent with a chance 
%%		to adjust the url pop.
%%
%%		If Result is {invalid, NewState}, Agent is set to idle, and 
%%		execution continues with NewState.
%%
%%	<b>handle_ring_stop(StateName, Call, Internal, State) -> Result</b>
%%		types:	StateName = state_name()
%%				Call = #call{}
%%				Internal = internal_state()
%%				State = any()
%%				Result = {ok, NewState}
%%					NewState = any()
%%
%%		When an agent should no longer be ringing, such as due to ringout, 
%%		this function is called.
%%
%%		State is the internal state of the gen_media callbacks.
%%
%%		Execution will continue with NewState.
%%
%%	<b>handle_answer({Agent, Apid}, StateName, Call, Internal, State) ->
%%		Result</b>
%%		types:	Agent = string()
%%				Apid = pid()
%%				StateName = state_name()
%%				Call = #call{}
%%				Internal = internal_state()
%%				State = any()
%%				Result = {ok, NewState} | {error, Error, NewState}
%%					Error = NewState = any()
%%
%%		When an agent should be placed on call after ringing, this function
%%		is called.
%%
%%		Agent is the agent that will be set oncall if Result is 
%%		{ok, NewState}.
%%
%%		Call is the #call{} the agent will be answering.
%%
%%		State is the internal state of the gen_media callbacks.
%%
%%		If Result is {ok, NewState} and the callpath is inband, it is 
%%		assumed the agent has already set themselves oncall.  If it is out 
%%		of band, the agent is set to oncall.  The callback module can 
%%		always safely assume the agent is oncall.  Execution then 
%%		continues with NewState.
%%
%%		If Result is {error, Error, NewState}, the agent's state is not 
%%		changed and execution continues with NewState.
%%
%%	<b>handle_voicemail(StateName, Call, Internal, State) -> Result</b>
%%		types:	StateName = state_name()
%%				Call = #call{}
%%				Internal = internal_state()
%%				State = any()
%%				Result = {ok, NewState} | {invalid, NewState}
%%
%%		This is an optional callback.
%%
%%		When a media should be removed from queue and moved to voicemail, 
%%		this is called.
%%
%%		State is the internal state of the gen_media callbacks.
%%
%%		If Result is {ok, NewState}, the call is removed from queue and 
%%		execution continues with NewState.
%%
%%		If Result is {invalid, NewState} execution continues with NewState.
%%
%%	<b>handle_annouce(Announce, StateName, Call, Internal, State) ->
%%		{ok, NewState}</b>
%%		types:	Announce = any()
%%				StateName = state_name()
%%				Call = any()
%%				Internal = internal_state()
%%				State = NewState = any()
%%
%%		This is an optional callback.
%%
%%		When a recipe calls for a call in queue to play an announcement, if
%%		this function is defined, it is called.  Execution then continues
%%		with NewState.
%%
%%	<b>handle_agent_transfer(Agent, Timeout, StateName, Call, Internal,
%%		State) -> Result</b>
%%		types:	Agent = pid()
%%				Timeout = pos_integer()
%%				StateName = state_name()
%%				Call = #call{}
%%				Internal = internal_state()
%%				State = any()
%%				Result = {ok, NewState} | {error, Error, NewState}
%%					NewState = State = any()
%%					Error = any()
%%
%%		When a media should be transfered to another agent, this is the
%%		first step.  The target agent is set to prering, then this 
%%		callback is used to verify that.  If the callback returns 
%%		{ok, NewState}, execution continues with NewState, and gen_media 
%%		handles with oncall or a ringout.
%%
%%		In the case of an outband ring, that process will send a takeover
%%		message to gen_media.
%%
%%	<b>handle_queue_transfer({Queue, Qpid}, StateName, Call, Internal,
%%		State) -> {ok, NewState}</b>
%%		types:	Queue = string()
%%				Qpid = pid
%%				StateName = state_name()
%%				Call = #call{}
%%				Internal = internal_state()
%%				State = NewState = any()
%%
%%		When a media is placed back into queue from an agent, this is 
%%		called to allow the media to do any required clean up or 
%%		unbridging.  The Call is requeued at the priority it was initially 
%%		queued at.  Execution then continues with NewState.
%%
%%		Queue is the name of the queue the media will be placed in; Qpid is
%%		the pid of said queue.
%%
%%	<b>handle_wrapup(From, StateName, Call, Internal, State) -> {Finality,
%%		NewState}</b>
%%		types:	From = {pid(), reference()}
%%				StateName = state_name()
%%				Call = #call{}
%%				Internal = internal_state()
%%				State = NewState = any()
%%				Finality = ok | hangup
%%
%%		This callback is only used if the call record's media path is inband.
%%		When an agent goes to wrapup, this gives the callback a chance to 
%%		do any clean-up needed.  
%%
%%		If the media determines this is a hang-up (ie, no more
%%		can be done with the media), it can return {hangup, NewState}.  The
%%		gen_media then terminates with state NewState.
%%
%%		If {ok, NewState} is returned, execution continues with state
%%		NewState.
%%
%%	<b>handle_spy(Spy, StateName, Call, Internal, State) -> {ok, NewState}
%%		| {invalid, NewState} | {error, Error, NewState}</b>
%%		types:  Spy = {Spypid, AgentRec}
%%				Spypid = pid() | any()
%%				AgentRec = 'undefined' | #agent{}
%%				StateName = state_name()
%%				Call = #call{}
%%				Internal = internal_state()
%%				State = NewState = any()
%%
%%		This callback is optional.
%%
%%		Spy can be a pid of an agent acting as a spy, or a generic term to
%%		be used by the media to allow spying.  When spy is an active, agent,
%%		They must be released.
%%
%%		This signals the callback that a supervisor is attempting to observe
%%		the agent that is oncall.  The other callbacks should take into
%%		account the possibility of a spy if 'ok' is returned.
%%		
%%		Be aware that when calling this, gen_media does not have a 
%%		reliable method to determine an agent's security level.  The agent 
%%		connections, however, do.
%%
%%	<b>Extended gen_server Callbacks</b>
%%
%%	In addition to the usual replies gen_server expects from it's callback
%%	of handle_call/3, handle_cast/2, and handle_info/2, gen_media will 
%%	take some action based on the following Returns.
%%
%%	{queue, Queue, Callrec, NewState}
%%		types:  Queue = string()
%%				Callrec = #call{}
%%				NewState = any()
%%
%%		This result is only valid if the callbacks init/1 returned 
%%		undefined for the call record.  This sets the call record and 
%%		queues the call.  Execution then continues on with NewState.  If 
%%		this is replied to a call, ok is set as the reply.
%%
%%	{outbound, Agent, NewState}
%%	{outbound, Agent, Call, NewState}
%%		types:  Agent = pid()
%%				Call = #call{}
%%				NewState = any()
%%
%%		This result is valid only if the call is not queued.  The second 
%%		form is only valid if init/1 retuned an undefined call.  This also 
%%		assumes the agent at pid() is already in precall state.  If The 
%%		agent can be set to outgoing, it will be.  Execution continues on 
%%		with NewState.
%%
%%	{voicemail, NewState}
%%		types:	NewState = any()
%%
%%		This result is valid only if the call is queued.  Removes the media
%%		from queue and stops ringing to an agent it is.  Assumes the media 
%%		has already	done what it needs to for a voicemail.  If done in a 
%%		handle_call, the reply is 'ok'.
%%
%%	{Agentaction, NewState}
%%	{Agentaction, Reply, NewState}
%%		types:	Agentaction = stop_ring | {stop_ring, Data} | wrapup | hangup | 
%%					{hangup, Data} | {mediapush, Data, Mode}
%%				Reply = any()
%%				NewState = any()
%%				Data = any()
%%				Mode = replace | append
%%
%%		This result is only valid if an agent has been associated with this
%%		media by ringing.  The second form is only valid if the request 
%%		came in	as a gen_media:call.  This attempts to take the specified 
%%		action on the agent, then continues execution with NewState.
%%
%%		{stop_ring, Data} is used to stop the gen_media from handling a 
%%		ringout.  It does not change the agent's state.  Execution will 
%%		continue with NewState.  This is useful if there is an error 
%%		ringing to an agent that only becomes apparent at a later time.  A 
%%		return of `stop_ring' is Equivalent to {stop_ring, undefined}.
%%
%%		wrapup is only valid if there is an agent associated with a media, 
%%		and	that agent is oncall or outgoing.  This sets the agent to 
%%		wrapup and continues execution with NewState.
%%
%%		{hangup, Data} is valid at any time.  This will unqueue the media, 
%%		and set the appropriate state for any agents.  The cdr record will 
%%		record Data as who hung up the call.  A return of hangup is 
%%		equivalent to {hangup, undefined}.  Execution then coninues with 
%%		NewState.
%%
%%		mediapush is only valid if there is an agent oncall with the media,
%%		and the media is inband.  The given Data is casted to the 
%%		associaed agent as a media push.
%%
%%	{stop, hangup, NewState}
%%	{stop, {hangup, Data}, NewState}
%%		types:  NewState = any()
%%				Data = any()
%%
%%		This causes the media to take any action it would from an
%%		Agentaction return tuple of hangup, then stop.

% TODO Less agent oriented and more agent channel oriented.
-module(gen_media).
-author(micahw).

-behaviour(gen_fsm).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("gen_media.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
	behaviour_info/1,
	start_link/2,
	start/2
]).

%% gen_fsm callbacks
-export([
	init/1,  terminate/3, code_change/4, 
	handle_event/3, handle_sync_event/4, handle_info/3,
	inivr/2, inivr/3,
	inqueue/2, inqueue/3,
	inqueue_ringing/2, inqueue_ringing/3,
	oncall/2, oncall/3,
	oncall_ringing/2, oncall_ringing/3,
	wrapup/2, wrapup/3
]).

%% gen_media api
-export([
	ring/4,
	takeover_ring/2,
	get_call/1,
	voicemail/1,
	announce/2,
	%% TODO added for testing only (implemented with focus on real Calls - no other media)
	end_call/1,
	stop_ringing/1,
	oncall/1,
	agent_transfer/3,
	queue/2,
	call/2,
	call/3,
	cast/2,
	wrapup/1,
	spy/3,
	set_cook/2,
	set_queue/2,
	set_url_getvars/2,
	get_url_getvars/1,
	add_skills/2
]).

% TODO - add these to a global .hrl, cpx perhaps?
-type(tref() :: any()).
-type(proplist_item() :: atom() | {any(), any()}).
-type(proplist() :: [proplist_item()]).

%% gen_media states
-define(states, [
	inivr, inqueue, inqueue_ringing, oncall, oncall_ringing, wrapup
]).

%% state changes
%% init -> inivr, inqueue, oncall (in case of outbound)
%% inivr -> inqueue
%% inqueue -> inqueue_ringing
%% inqueue_ringing -> inqueue, oncall
%% oncall -> oncall_ringing, wrapup, warm_transfer_hold, inqueue
%% oncall_ringing -> oncall (same state), oncall (new agent)
%% wrapup -> *
%% warm_transfer_hold -> warm_transfer_3rd_party, oncall, wrapup
%% warm_transfer_3rd_party -> warm_transfer_merged, warm_transfer_hold
%% warm_transfer_merged -> oncall, wrapup

-record(base_state, {
	callback :: atom(),
	substate :: any(),
	callrec :: 'undefined' | #call{},
	queue_failover,
	url_pop_get_vars = []
}).

-spec(behaviour_info/1 :: 
	(Info :: 'callbacks' | any()) -> [{atom(), non_neg_integer()}] | 'undefined').
behaviour_info(callbacks) ->
	[
		{prepare_endpoint, 2},
		{init, 1},
		{handle_ring, 4},
		{handle_ring_stop, 4},
		{handle_answer, 5}, 
		%{handle_voicemail, 3}, 
		%{handle_announce, 3}, 
		{handle_agent_transfer, 4},
		{handle_queue_transfer, 5},
		{handle_wrapup, 5},
		{handle_call, 6},
		{handle_cast, 5},
		{handle_info, 5},
		{terminate, 5},
		{code_change, 4}
	];
behaviour_info(_Other) ->
    undefined.

%% @doc Make the `pid() Genmedia' ring to `pid() Agent' based off of
%% `#queued_call{} Qcall' with a ringout of `pos_integer() Timeout'
%% miliseconds.
%% @deprecated Use ring/3 instead as timout is ignored.  The ringout is
%% determined by the client option "ringout", the default value being
%% 60000.
-spec(ring/4 :: (Genmedia :: pid(), Agent :: pid() | string() | {string(), pid()}, Qcall :: #queued_call{}, Timeout :: pos_integer())  -> 'ok' | 'invalid' | 'deferred').
ring(Genmedia, {_Agent, Apid} = A, Qcall, Timeout) when is_pid(Apid) ->
	gen_fsm:sync_send_event(Genmedia, {{'$gen_media', ring}, {A, Qcall, Timeout}}, infinity);

ring(Genmedia, Apid, Qcall, Timeout) when is_pid(Apid) ->
	case agent_manager:find_by_pid(Apid) of
		notfound ->
			invalid;
		Agent ->
			ring(Genmedia, {Agent, Apid}, Qcall, Timeout)
	end;
ring(Genmedia, Agent, Qcall, Timeout) ->
	case agent_manager:query_agent(Agent) of
		{true, Apid} ->
			ring(Genmedia, {Agent, Apid}, Qcall, Timeout);
		false ->
			invalid
	end.

%% @doc Have the given gen_media ring the given agent based on the given
%% queued call.
-spec ring(Genmedia :: pid(),
	Agent :: pid() | string() | {string(), pid()},
	Qcall :: #queued_call{}) -> 'ok' | 'invalid' | 'deferred'.
ring(Genmedia, {_Agent, Apid}=A, Qcall) ->
	gen_fsm:sync_send_event(Genmedia, {{'$gen_media', ring}, {A, Qcall, undefined}}, infinity);

ring(Genmedia, Apid, Qcall) when is_pid(Apid) ->
	case agent_manager:find_by_pid(Apid) of
		notfound ->
			invalid;
		Agent ->
			ring(Genmedia, {Agent, Apid}, Qcall)
	end;

ring(Genmedia, Agent, Qcall) ->
	case agent_manager:query_agent(Agent) of
		{true, Apid} ->
			ring(Genmedia, {Agent, Apid}, Qcall);
		false ->
			invalid
	end.


-spec(takeover_ring/2 :: (Genmedia :: pid(), Agent :: pid() | string() | {string(), pid()}) -> 'ok' | 'invalid').
takeover_ring(Genmedia, {_, Apid} = Agent) when is_pid(Apid) ->
	Self = self(),
	gen_fsm:send_event(Genmedia, {{'$gen_media', takeover_ring}, {Agent, Self}});

takeover_ring(Genmedia, Apid) when is_pid(Apid) ->
	case agent_manager:find_by_pid(Apid) of
		notfound -> invalid;
		Agent -> takeover_ring(Genmedia, {Agent, Apid})
	end;

takeover_ring(Genmedia, Agent) ->
	case agent_manager:query_agent(Agent) of
		{true, Apid} -> takeover_ring(Genmedia, {Agent, Apid});
		false -> invalid
	end.

%% @doc Get the call record associated with `pid() Genmedia'.
-spec(get_call/1 :: (Genmedia :: pid()) -> #call{}).
get_call(Genmedia) ->
	gen_fsm:sync_send_all_state_event(Genmedia, {{'$gen_media', get_call}, undefined}, infinity).

%% @doc Send the passed `pid() Genmedia' to voicemail.
-spec(voicemail/1 :: (Genmedia :: pid()) -> 'ok' | 'invalid').
voicemail(Genmedia) ->
	gen_fsm:sync_send_event(Genmedia, {{'$gen_media', voicemail}, undefined}).

%% @doc Pass `any() Annouce' message to `pid() Genmedia'.
-spec(announce/2 :: (Genmedia :: pid(), Annouce :: any()) -> 'ok').
announce(Genmedia, Annouce) ->
	gen_fsm:sync_send_event(Genmedia, {{'$gen_media', announce}, Annouce}).

%% TODO added for testing only (implemented with focus on real Calls - no other media)
%% @doc End the Call for `pid() Genmedia'.
-spec(end_call/1 :: (Genmedia :: pid()) -> 'ok').
end_call(Genmedia) ->
	gen_server:call(Genmedia, '$gen_media_end_call').

%% @doc Sends the oncall agent associated with the call to wrapup; or, if it's
%% the oncall agent making the request, gives the callback module a chance to
%% handle it.
-spec(wrapup/1 :: (Genmedia :: pid()) -> 'ok' | 'invalid').
wrapup(Genmedia) ->
	gen_fsm:sync_send_event(Genmedia, {{'$gen_media', wrapup}, undefined}).

%% @doc Send a stop ringing message to `pid() Genmedia'.
-spec(stop_ringing/1 :: (Genmedia :: pid()) -> 'ok').
stop_ringing(Genmedia) ->
	Self = self(),
	gen_fsm:send_event(Genmedia, {{'$gen_media', stop_ringing}, Self}),
	ok.

%% @doc Set the agent associated with `pid() Genmedia' to oncall.
-spec(oncall/1 :: (Genmedia :: pid()) -> 'ok' | 'invalid').
oncall(Genmedia) ->
	gen_fsm:sync_send_event(Genmedia, {{'$gen_media', agent_oncall}, undefined}, infinity).

%% @doc Transfer the call from the agent it is associated with to a new agent.
-spec(agent_transfer/3 :: (Genmedia :: pid(), Apid :: pid() | string() | {string(), pid()}, Timeout :: pos_integer()) -> 'ok' | 'invalid').
agent_transfer(Genmedia, {_Login, Apid} = Agent, Timeout) when is_pid(Apid) ->
	gen_fms:sync_send_event(Genmedia, {{'$gen_media', agent_transfer}, {Agent, Timeout}});
agent_transfer(Genmedia, Apid, Timeout) when is_pid(Apid) ->
	case agent_manager:find_by_pid(Apid) of
		notfound ->
			invalid;
		Agent ->
			agent_transfer(Genmedia, {Agent, Apid}, Timeout)
	end;
agent_transfer(Genmedia, Agent, Timeout) ->
	case agent_transfer:query_agent(Agent) of
		false ->
			invalid;
		{true, Apid} ->
			agent_transfer(Genmedia, {Agent, Apid}, Timeout)
	end.

%% @doc Transfer the passed media into the given queue.
-spec(queue/2 :: (Genmedia :: pid(), Queue :: string()) -> 'ok' | 'invalid').
queue(Genmedia, Queue) ->
	gen_fsm:syn_send_event(Genmedia, {'$gen_media', queue, Queue}).
	
%% @doc Attempt to spy on the agent oncall with the given media.  `Spy' is
%% the pid to send media events/load data to, and `AgentRec' is an 
%% `#agent{}' used to hold the end point data.
-spec(spy/3 :: (Genmedia :: pid(), Spy :: pid(), AgentRec :: #agent{}) -> 'ok' | 'invalid' | {'error', any()}).
spy(Genmedia, Spy, AgentRec) ->
	gen_fsm:sync_send_event(Genmedia, {{'$gen_media', spy}, {Spy, AgentRec}}).

-spec(set_cook/2 :: (Genmedia :: pid(), CookPid :: pid()) -> 'ok').
set_cook(Genmedia, CookPid) ->
	gen_fsm:send_event(Genmedia, {{'$gen_media', set_cook}, CookPid}).

-spec(set_queue/2 :: (Genmedia :: pid(), Qpid :: pid()) -> 'ok').
set_queue(Genmedia, Qpid) ->
	gen_fsm:sync_send_event(Genmedia, {{'$gen_media', set_queue}, Qpid}).

-spec(set_url_getvars/2 :: (Genmedia :: pid(), Vars :: [{string(), string()}]) -> 'ok').
set_url_getvars(Genmedia, Vars) ->
	gen_fsm:sync_send_all_state_event(Genmedia, {{'$gen_media', set_url_getvars}, Vars}).

-spec(get_url_getvars/1 :: (Genmedia :: pid()) -> {'ok', [{string(), string()}]}).
get_url_getvars(Genmedia) ->
	gen_fsm:sync_send_all_state_event(Genmedia, {{'$gen_media', get_url_vars}, undefined}).

-spec(add_skills/2 :: (Genmedia :: pid(), Skills :: [atom() | {atom(), any()}]) -> 'ok').
add_skills(Genmedia, Skills) ->
	gen_fsm:send_all_state_event(Genmedia, {{'$gen_media', add_skills}, Skills}).
	
%% @doc Do the equivalent of a `gen_server:call/2'.
-spec(call/2 :: (Genmedia :: pid(), Request :: any()) -> any()).
call(Genmedia, Request) ->
	gen_fsm:sync_send_all_state_event(Genmedia, Request).

%% @doc Do the equivalent of `gen_server:call/3'.
-spec(call/3 :: (Genmedia :: pid(), Request :: any(), Timeout :: pos_integer()) -> any()).
call(Genmedia, Request, Timeout) ->
	gen_fsm:sync_send_all_state_event(Genmedia, Request, Timeout).

%% @doc Do the equivalent of `gen_server:cast/2'.
-spec(cast/2 :: (Genmedia :: pid(), Request:: any()) -> 'ok').
cast(Genmedia, Request) ->
	gen_fsm:send_all_state_event(Genmedia, Request).

%%====================================================================
%% API
%%====================================================================

%% @doc Start a gen_media linked to the calling process.
-spec(start_link/2 :: (Callback :: atom(), Args :: any()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start_link(Callback, Args) ->
	gen_fsm:start_link(?MODULE, [Callback, Args], []).

-spec(start/2 :: (Callback :: atom(), Args :: any()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start(Callback, Args) ->
	gen_fsm:start(?MODULE, [Callback, Args], []).

%%====================================================================
%% init callbacks
%%====================================================================

%% @private
init([Callback, Args]) ->
	case Callback:init(Args) of
		{ok, {Substate, undefined}} ->
				BaseState = #base_state{
					callback = Callback,
					substate = Substate,
					callrec = undefined
				},
				{ok, inivr, {BaseState, #inivr_state{}}};
		{ok, {Substate, {Queue, PCallrec}}} when is_record(PCallrec, call) ->
			Callrec = correct_client(PCallrec),
			cdr:cdrinit(Callrec),
			cpx_monitor:set({media, Callrec#call.id}, [], self()),
			BaseState = #base_state{
				callback = Callback,
				substate = Substate,
				callrec = Callrec
			},
			{Qnom, Qpid} = case priv_queue(Queue, Callrec, true) of
				{default, Pid} ->
					set_cpx_mon({#base_state{callrec = Callrec}, #inqueue_state{queue_pid = {"default_queue", Pid}}}, [{queue, "default_queue"}]),
					cdr:inqueue(Callrec, "default_queue"),
					{"default_queue", Pid};
				Else when is_pid(Else) ->
					cdr:inqueue(Callrec, Queue),
					Qmon = erlang:monitor(process, Else),
					set_cpx_mon({#base_state{callrec = Callrec}, #inqueue_state{queue_mon = Qmon, queue_pid = {Queue, Else}}}, [{queue, Queue}]),
					{Queue, Else}
			end,
			InqState = #inqueue_state{
				queue_mon = erlang:monitor(process, Qpid)
			},
			{ok, inqueue, {BaseState, InqState}};
		{ok, {Substate, PCallrec, {CDRState, CDRArgs}}} when is_record(PCallrec, call) ->
			Callrec = correct_client(PCallrec),
			cdr:cdrinit(Callrec),
			apply(cdr, CDRState, [Callrec | CDRArgs]),
			BaseState = #base_state{
				callback = Callback,
				substate = Substate,
				callrec = Callrec
			},
			set_cpx_mon({BaseState, #inivr_state{}}, [], self()),
			{ok, inivr, {BaseState, #inivr_state{}}};
		{ok, {Substate, PCallrec}} when is_record(PCallrec, call) ->
			Callrec = correct_client(PCallrec),
			cdr:cdrinit(Callrec),
			set_cpx_mon({#base_state{callrec = Callrec}, #inivr_state{}}, [], self()),
			BaseState = #base_state{
				callback = Callback,
				substate = Substate,
				callrec = Callrec
			},
			{ok, inivr, {BaseState, #inivr_state{}}};
		{stop, Reason} = O ->
			?WARNING("init aborted due to ~p", [Reason]),
			O;
		ignore ->
			?WARNING("init told to ignore", []),
			ignore
	end.

%%--------------------------------------------------------------------
%% inivr -> inqueue
%%--------------------------------------------------------------------

inivr({{'$gen_media', Command}, _Args}, _From, State) ->
	?DEBUG("Invalid sync event ~s while inivr", [Command]),
	{reply, invalid, inivr, State};

inivr(Msg, From, {#base_state{callback = Callback} = BaseState, _} = State) ->
	Return = Callback:handle_call(Msg, From, inivr, BaseState#base_state.callrec, #inivr_state{}, BaseState#base_state.substate),
	handle_custom_return(Return, inivr, reply, State).

inivr(Msg, {#base_state{ callback = Callback, callrec = Call} = BaseState,
		_} = State) ->
	Return = Callback:handle_cast(Msg, inivr, Call, #inivr_state{}, BaseState#base_state.substate),
	handle_custom_return(Return, inivr, noreply, State).

%%--------------------------------------------------------------------
%% inqueue -> inqueue_ringing
%%--------------------------------------------------------------------

inqueue({{'$gen_media', ring}, {{Agent, Apid}, #queued_call{
		cook = Requester} = QCall, _Timeout}}, {Requester, _Tag}, {
		#base_state{callrec = Call, callback = Callback} = BaseState,
		Internal}) ->
	ClientOpts = Call#call.client#client.options,
	TimeoutSec = proplists:get_value("ringout", ClientOpts, 60),
	Timeout = TimeoutSec * 1000,
	?INFO("Trying to ring ~p with ~p with timeout ~p", [Agent, Call#call.id, Timeout]),
	try agent:prering(Apid, Call) of
		{ok, RPid} ->
			Rmon = erlang:monitor(process, RPid),
			Tref = gen_fsm:send_event_after(Timeout, {{'$gen_media', ringout}, undefined}),
			#inqueue_state{ queue_pid = Qpid, queue_mon = Qmon,
				cook_mon = CookMon} = Internal,
			NewInternal = #inqueue_ringing_state{
				queue_pid = Qpid, queue_mon = Qmon, ring_pid = {Agent, RPid},
				ring_mon = Rmon, cook = Requester, cook_mon = CookMon,
				ringout = Tref
			},
			{reply, ok, inqueue_ringing, {BaseState, NewInternal}};
		RingErr ->
			?INFO("Agent ~p prering response:  ~p for ~p", [Agent, RingErr, Call#call.id]),
			{reply, invalid, inqueue, {BaseState, Internal}}
	catch
		exit:{noproc, {gen_fsm, sync_send_event, _TheArgs}} ->
			?WARNING("Agent ~p is a dead pid", [Apid]),
			{reply, invalid, inqueue, {BaseState, Internal}};
		exit:{max_ringouts, {gen_fsm, sync_send_event, _TheArgs}} ->
			?DEBUG("Max ringouts reached for agent ~p", [Apid]),
			{reply, invalid, inqueue, {BaseState, Internal}}
	end;

inqueue({{'$gen_media', ring}, {{_Agent, Apid}, QCall, _Timeout}}, _From, State) ->
	gen_server:cast(QCall#queued_call.cook, {ring_to, Apid, QCall}),
	{reply, deferred, inqueue, State};

inqueue({{'$gen_media', announce}, Announce}, _From, {#base_state{
		callback = Callback, substate = InSubstate, callrec = Call} =
		BaseState, Internal}) ->
	?INFO("Doing announce for ~p", [Call#call.id]),
	Substate = case erlang:function_exported(Callback, handle_announce, 3) of
		true ->
			{ok, N} = Callback:handle_announce(Announce, inqueue, Call, Internal, InSubstate),
			N;
		false ->
			InSubstate
	end,
	{reply, ok, inqueue, {BaseState#base_state{substate = Substate}, Internal}};

inqueue({{'$gen_meida', voicemail}, undefined}, _From, {BaseState, Internal}) ->
	#base_state{callback = Callback, callrec = Call} = BaseState,
	?INFO("trying to send media ~p to voicemail", [Call#call.id]),
	case erlang:function_exported(Callback, handle_voicemail, 3) of
		false ->
			{reply, invalid, inqueue, {BaseState, Internal}};
		true ->
			case  Callback:handle_voicemail(inqueue, Call, Internal, BaseState#base_state.substate) of
				{ok, Substate} ->
					priv_voicemail({BaseState, Internal}),
					{reply, ok, inqueue, {BaseState#base_state{substate = Substate}, Internal}};
				{invalid, Substate} ->
					{reply, invalid, inqueue, {BaseState#base_state{substate = Substate}, Internal}}
			end
	end;

inqueue({{'$gen_media', end_call}, _}, {Cook, _}, {#base_state{
		callrec = #call{cook = Cook}} = BaseState, InqueueState}) ->
	#base_state{callback = Callback, substate = InSubstate,
		callrec = Call} = BaseState,
	case erlang:function_exported(Callback, handle_end_call, 4) of
		true ->
			case Callback:handle_end_call(inqueue, Call, InqueueState, InSubstate) of
				{ok, Substate} ->
					% stop agent ringing, kill self
					?INFO("Ending Call for ~p", [Call#call.id]),
					NewState0 = BaseState#base_state{substate = Substate},
					{Out, NewState} = handle_stop(hangup, inqueue, NewState0, InqueueState),
					{stop, Out, ok, NewState};
				{deferred, Substate} ->
					?INFO("Ending Call deferred for ~p", [Call#call.id]),
					% up to the media to kill self.
					NewBase = BaseState#base_state{substate = Substate},
					{reply, ok, {NewBase, InqueueState}};
				{error, Err, Substate} ->
					?INFO("Ending Call for ~p errored:  ~p", [Call#call.id, Err]),
					NewBase = BaseState#base_state{substate = Substate},
					{reply, invalid, {NewBase, InqueueState}}
			end;
		false ->
			{reply, invalid, {BaseState, InqueueState}}
	end;

inqueue({{'$gen_media', set_queue}, Qpid}, _From, {BaseState, 
		#inqueue_state{queue_pid = {Queue, _}} = Internal}) ->
	#base_state{callrec = Call} = BaseState,
	?NOTICE("Updating queue pid for ~p to ~p", [Call#call.id, Qpid]),
	case Internal#inqueue_state.queue_mon of
		undefined ->
			ok;
		M ->
			erlang:demonitor(M)
	end,
	Newmon = erlang:monitor(process, Qpid),
	NewInternal = Internal#inqueue_state{
		queue_mon = Newmon,
		queue_pid = {Queue, Qpid}
	},
	{reply, ok, inqueue, {BaseState, NewInternal}};

inqueue({{'$gen_media', get_url_vars}, undefined}, _From, {BaseState, Internal}) ->
	#base_state{url_pop_get_vars = GenPopopts, substate = Substate,
		callback = Callback} = BaseState,
	Cbopts = case erlang:function_exported(Callback, urlpop_getvars, 1) of
		true ->
			Callback:urlpop_getvars(Substate);
		false ->
			[]
	end,
	Out = lists:ukeymerge(1, lists:ukeysort(1, GenPopopts), lists:ukeysort(1, Cbopts)),
	{reply, {ok, Out}, inqueue, {BaseState, Internal}};

inqueue({{'$gen_media', Command}, _Args}, _From, State) ->
	?DEBUG("Invalid sync event ~s while inqueue", [Command]),
	{reply, invalid, inqueue, State};

inqueue(Msg, From, {#base_state{callback = Callback, callrec = Call} = BaseState, InQueueState} = State) ->
	Return = Callback:handle_call(Msg, From, inqueue, Call, InQueueState, BaseState#base_state.substate),
	handle_custom_return(Return, inqueue, reply, State).

inqueue({{'$gen_media', set_outband_ring_pid}, Pid}, {BaseState, Internal}) ->
	NewInternal = Internal#inqueue_state{outband_ring_pid = Pid},
	{next_state, inqueue, {BaseState, NewInternal}};

inqueue({{'$gen_media', set_cook}, CookPid}, {BaseState, Internal}) ->
	#base_state{callrec = Call} = BaseState,
	?NOTICE("Updating cook pid for ~p to ~p", [Call#call.id, CookPid]),
	case Internal#inqueue_state.cook_mon of
		undefined ->
			ok;
		M ->
			erlang:demonitor(M)
	end,
	Newmon = erlang:monitor(process, CookPid),
	NewCall = Call#call{cook = CookPid},
	NewInternal = Internal#inqueue_state{cook_mon = Newmon, cook = CookPid},
	NewBase = BaseState#base_state{callrec = NewCall},
	{next_state, inqueue, {NewBase, NewInternal}};

inqueue({{'$gen_media', Command}, _}, State) ->
	?DEBUG("Invalid event ~s while inqueue", [Command]),
	{next_state, inqueue, State};

inqueue(Msg, {#base_state{callback = Callback, callrec = Call} = BaseState,
		InQueueState} = State) ->
	Return = Callback:handle_cast(Msg, inqueue, Call, InQueueState, BaseState#base_state.substate),
	handle_custom_return(Return, inqueue, noreply, State).

%%--------------------------------------------------------------------
%% inqueue_ringing -> inqueue, oncall
%%--------------------------------------------------------------------

inqueue_ringing({{'$gen_media', announce}, Announce}, _From, {BaseState, Internal}) ->
	#base_state{callback = Callback, substate = InSubstate,
		callrec = Call} = BaseState,
	?INFO("Doing announce for ~p", [Call#call.id]),
	Substate = case erlang:function_exported(Callback, handle_announce, 3) of
		true ->
			{ok, N} = Callback:handle_announce(Announce, inqueue_ringing, Call, Internal, InSubstate),
			N;
		false ->
			InSubstate
	end,
	{reply, ok, inqueue_ringing, {BaseState#base_state{substate = Substate}, Internal}};

inqueue_ringing({{'$gen_media', voicemail}, undefined}, _From, {BaseState, Internal}) ->
	#base_state{callback = Callback, callrec = Call} = BaseState,
	?INFO("trying to send media ~p to voicemail", [Call#call.id]),
	case erlang:function_exported(Callback, handle_voicemail, 3) of
		false ->
			{reply, invalid, inqueue_ringing, {BaseState, Internal}};
		true ->
			case  Callback:handle_voicemail(inqueue_ringing, Call, Internal, BaseState#base_state.substate) of
				{ok, Substate} ->
					priv_voicemail({BaseState, Internal}),
					NewInternal = #inqueue_state{
						queue_mon = Internal#inqueue_ringing_state.queue_mon,
						queue_pid = Internal#inqueue_ringing_state.queue_pid,
						cook = Internal#inqueue_ringing_state.cook,
						cook_mon = Internal#inqueue_ringing_state.cook_mon
					},
					NewBase = BaseState#base_state{substate = Substate},
					{reply, ok, inqueue, {NewBase, NewInternal}};
				{invalid, Substate} ->
					{reply, invalid, inqueue_ringing, {BaseState#base_state{substate = Substate}, Internal}}
			end
	end;

inqueue_ringing({{'$gen_media', agent_oncall}, undefined}, {Apid, _Tag},
		{#base_state{callrec = #call{ring_path = outband} = Call} = BaseState,
		#inqueue_ringing_state{ring_pid = {_, Apid}} = Internal}) ->
	?INFO("Cannot accept on call requests from agent (~p) unless ring_path is inband for ~p", [Apid, Call#call.id]),
	{reply, invalid, inqueue_ringing, {BaseState, Internal}};

inqueue_ringing({{'$gen_media', agent_oncall}, undefined}, {Apid, _Tag},
		{#base_state{callrec = #call{ring_path = inband} = Call} = BaseState,
		#inqueue_ringing_state{ring_pid = {Agent, Apid}} = Internal}) ->
	#base_state{callback = Callback} = BaseState,
	?INFO("oncall request from agent ~p for ~p", [Apid, Call#call.id]),
	case Callback:handle_answer(Apid, inqueue_ringing, Call, Internal, BaseState#base_state.substate) of
		{ok, NewState} ->
			kill_outband_ring({BaseState, Internal}),
			case Internal#inqueue_ringing_state.ringout of
				undefined -> ok;
				TimerRef -> gen_fsm:cancel_timer(TimerRef)
			end,
			unqueue(Internal#inqueue_ringing_state.queue_pid, self()),
			cdr:oncall(Call, Agent),
			NewBase = BaseState#base_state{substate = NewState},
			NewInternal = #oncall_state{
				oncall_pid = {Agent, Apid},
				oncall_mon = Internal#inqueue_ringing_state.ring_mon
			},
			set_cpx_mon({NewBase, NewInternal}, [{agent, Agent}]),
			erlang:demonitor(Internal#inqueue_ringing_state.queue_mon),
			{reply, ok, oncall, {NewBase, NewInternal}};
		{error, Reason, NewState} ->
			?ERROR("Could not set ~p on call due to ~p for ~p", [Apid, Reason, Call#call.id]),
			NewBase = BaseState#base_state{substate = NewState},
			{reply, invalid, inqueue_ringing, {NewBase, Internal}}
	end;

inqueue_ringing({{'$gen_media', agent_oncall}, undefined}, From, {BaseState, Internal}) ->
	#base_state{callback = Callback, callrec = Call} = BaseState,
	#inqueue_ringing_state{ring_pid = {Agent, Apid}, ring_mon = Mon} = Internal,
	?INFO("oncall request from ~p; agent to set on call is ~p for ~p", [From, Apid, Call#call.id]),
	%% TODO this will break w/ merge for multichannel; that uses a pre-ring
	%% state.
	case set_agent_state(Apid, [oncall, Call]) of
		{error, invalid} ->
			{reply, invalid, inqueue_ringing, {BaseState, Internal}};
		ok ->
			case Callback:handle_answer(Apid, inqueue_ringing, Call, Internal, BaseState#base_state.substate) of
				{ok, NewState} ->
					kill_outband_ring({BaseState, Internal}),
					cdr:oncall(Call, Agent),
					case Internal#inqueue_ringing_state.ringout of
						undefined -> ok;
						_ -> gen_fsm:cancel_timer(Internal#inqueue_ringing_state.ringout)
					end,
					{_, Qpid} = Internal#inqueue_ringing_state.queue_pid,
					call_queue:remove(Qpid, self()),
					NewBase = BaseState#base_state{substate = NewState},
					NewInternal = #oncall_state{
						oncall_pid = {Agent, Apid},
						oncall_mon = Mon
					},
					set_cpx_mon({NewBase, NewInternal}, []),
					{reply, ok, oncall, {NewBase, NewInternal}};
				{error, Reason, NewState} ->
					?ERROR("Could not set ~p on call with ~p due to ~p", [Apid, Call#call.id, Reason]),
					NewBase = BaseState#base_state{substate = NewState},
					{reply, invalid, oncall, {NewBase, Internal}}
			end;
		badagent ->
			{ok, NewSubstate} = Callback:handle_ring_stop(inqueue_ringing, Call, Internal, BaseState#base_state.substate),
			kill_outband_ring({BaseState, Internal}),
			cdr:ringout(Call, {badagent, Agent}),
			gen_fsm:cancel_timer(Internal#inqueue_ringing_state.ringout),
			erlang:demonitor(Internal#inqueue_ringing_state.ring_mon),
			NewBase = BaseState#base_state{substate = NewSubstate},
			#inqueue_ringing_state{ queue_mon = Qmon, queue_pid = Qpid, cook = Cook, cook_mon = CookMon} = Internal,
			NewInternal = #inqueue_state{
				queue_mon = Qmon,
				queue_pid = Qpid,
				cook = Cook,
				cook_mon = CookMon
			},
			gen_server:cast(Cook, stop_ringing),
			{reply, invalid, inqueue, {NewBase, NewInternal}}
	end;

inqueue_ringing({{'$gen_media', set_queue}, Qpid}, _From, State) ->
	{BaseState, Internal} = State,
	#base_state{callrec = Call} = BaseState,
	#inqueue_ringing_state{queue_mon = Mon, queue_pid = {Queue, _}} = Internal,
	?NOTICE("Updating queue pid for ~p to ~p", [Call#call.id, Qpid]),
	case Mon of
		undefined ->
			ok;
		M ->
			erlang:demonitor(M)
	end,
	NewMon = erlang:monitor(process, Qpid),
	NewInternal = Internal#inqueue_ringing_state{
		queue_mon = NewMon,
		queue_pid = {Queue, Qpid}
	},
	{reply, ok, inqueue_ringing, {BaseState, NewInternal}};

inqueue_ringing({{'$gen_media', ring}, {{Agent, Apid}, ChanType, takeover}},
		From, {_, #inqueue_ringing_state{ring_pid = {Agent, Apid}}} = State) ->
	?DEBUG("~p said it's taking over ring", [From]),
	{BaseState, Internal} = State,
	gen_fsm:cancel_timer(Internal#inqueue_ringing_state.ringout),
	agent_channel:set_state(Apid, ringing, BaseState#base_state.callrec),
	NewInternal = Internal#inqueue_ringing_state{ringout = undefined},
	{reply, ok, inqueue_ringing, {BaseState, NewInternal}};

inqueue_ringing({{'$gen_media', ring}, {{Agent, Apid}, QCall, Timeout}} = Req, From, State) ->
	Cook = QCall#queued_call.cook,
	{next_state, inqueue, MidState} = inqueue_ringing({{'$gen_media', stop_ring}, Cook}, State),
	inqueue(Req, From, MidState);

inqueue_ringing({{'$gen_media', end_call}, _}, {Cook, _}, {#base_state{
		callrec = #call{cook = Cook}} = BaseState, InternalState}) ->
	#base_state{callback = Callback, substate = InSubstate,
		callrec = Call} = BaseState,
	case erlang:function_exported(Callback, handle_end_call, 4) of
		true ->
			case Callback:handle_end_call(inqueue_ringing, Call, InternalState, InSubstate) of
				{ok, Substate} ->
					% stop agent ringing, kill self
					?INFO("Ending Call for ~p", [Call#call.id]),
					NewState0 = BaseState#base_state{substate = Substate},
					{Out, NewState} = handle_stop(hangup, inqueue_ringing, NewState0, InternalState),
					{stop, Out, ok, NewState};
				{deferred, Substate} ->
					?INFO("Ending Call deferred for ~p", [Call#call.id]),
					% up to the media to kill self.
					NewBase = BaseState#base_state{substate = Substate},
					{reply, ok, {NewBase, InternalState}};
				{error, Err, Substate} ->
					?INFO("Ending Call for ~p errored:  ~p", [Call#call.id, Err]),
					NewBase = BaseState#base_state{substate = Substate},
					{reply, invalid, {NewBase, InternalState}}
			end;
		false ->
			{reply, invalid, {BaseState, InternalState}}
	end;

inqueue_ringing({{'$gen_media', Command}, _}, _From, State) ->
	?DEBUG("Invalid command ~s while inqueue_ringing", [Command]),
	{reply, invalid, inqueue_ringing, State};
	
inqueue_ringing(Msg, From, {#base_state{callback = Callback, 
		callrec = Call} = BaseState, Extra} = State) ->
	Return = Callback:handle_call(Msg, From, inqueue_ringing, Call, Extra, BaseState#base_state.substate),
	handle_custom_return(Return, inqueue_ringing, reply, State).

inqueue_ringing({{'$gen_media', set_outband_ring_pid}, Pid}, {BaseState, Internal}) ->
	NewInternal = Internal#inqueue_ringing_state{outband_ring_pid = Pid},
	{next_state, inqueue_ringing, {BaseState, NewInternal}};

inqueue_ringing({{'$gen_media', set_cook}, CookPid}, {BaseState, Internal}) ->
	Call = BaseState#base_state.callrec,
	Mon = Internal#inqueue_ringing_state.cook_mon,
	?NOTICE("Updating cook pid for ~p to ~p", [Call#call.id, CookPid]),
	case Mon of
		undefined ->
			ok;
		M ->
			erlang:demonitor(M)
	end,
	NewMon = erlang:monitor(process, CookPid),
	NewInternal = Internal#inqueue_ringing_state{
		cook = CookPid, cook_mon = NewMon
	},
	NewCall = Call#call{cook = CookPid},
	NewBase = BaseState#base_state{callrec = NewCall},
	{next_state, inqueue_ringing, {NewBase, NewInternal}};

inqueue_ringing({{'$gen_media', takeover_ring}, {{Agent, Apid}, OutbandRinger}}, {BaseState,
		#inqueue_ringing_state{ring_pid = {Agent, Apid}} = Internal}) ->
	gen_fsm:cancel_timer(Internal#inqueue_ringing_state.ringout),
	agent_channel:set_state(Apid, ringing, BaseState#base_state.callrec),
	NewInternal = Internal#inqueue_ringing_state{ringout = undefined, outband_ring_pid = OutbandRinger},
	{next_state, inqueue_ringing, {BaseState, NewInternal}};
	
inqueue_ringing({{'$gen_media', Command}, _}, State) ->
	?DEBUG("Invalid command event ~s while inqueue_ringing", [Command]),
	{next_state, inqueue_ringing, State};

inqueue_ringing(Msg, {#base_state{callback = Callback, callrec = Call} = 
		BaseState, Extra} = State) ->
	Return = Callback:handle_cast(Msg, inqueue_ringing, Call, Extra, BaseState#base_state.substate),
	handle_custom_return(Return, inqueue_ringing, noreply, State).

%%--------------------------------------------------------------------
%% oncall -> oncall_ringing, wrapup, warm_transfer_hold, inqueue
%%--------------------------------------------------------------------

oncall({{'$gen_media', queue}, Queue}, From, {BaseState, Internal}) ->
	#base_state{callback = Callback, callrec = Call} = BaseState,
	#oncall_state{oncall_pid = {Ocagent, Apid}, oncall_mon = Mon} = Internal,
	?INFO("Request to queue ~p from ~p", [Call#call.id, From]),
	% Decrement the call's priority by 5 when requeueing
	case priv_queue(Queue, reprioritize_for_requeue(Call), BaseState#base_state.queue_failover) of
		invalid ->
			{reply, invalid, {BaseState, Internal}};
		{default, Qpid} ->
			set_agent_state(Apid, [wrapup, Call]),
			{ok, NewState} = Callback:handle_queue_transfer({"default_queue", Qpid}, oncall, BaseState#base_state.callrec, Internal, BaseState#base_state.substate),
			cdr:queue_transfer(Call, "default_queue"),
			cdr:inqueue(Call, "default_queue"),
			cdr:wrapup(Call, Ocagent),
			erlang:demonitor(Mon),
			NewMon = erlang:monitor(process, Qpid),
			NewInternal = #inqueue_state{
				queue_mon = NewMon, queue_pid = {Queue, Qpid}
			},
			NewBase = BaseState#base_state{substate = NewState},
			set_cpx_mon({NewBase, NewInternal}, []),
			{reply, ok, inqueue, {NewBase, NewInternal}};
		Qpid when is_pid(Qpid) ->
			set_agent_state(Apid, [wrapup, Call]),
			{ok, NewState} = Callback:handle_queue_transfer({Queue, Qpid}, oncall, Call, Internal, BaseState#base_state.substate),
			cdr:queue_transfer(Call, Queue),
			cdr:inqueue(Call, Queue),
			cdr:wrapup(Call, Ocagent),
			erlang:demonitor(Mon),
			NewMon = erlang:monitor(process, Qpid),
			NewBase = BaseState#base_state{substate = NewState},
			NewInternal = #inqueue_state{
				queue_mon = NewMon, queue_pid = {Queue, Qpid}
			},
			set_cpx_mon({NewBase, NewInternal}, [{queue, Queue}]),
			{reply, ok, inqueue, {NewBase, NewInternal}}
	end;

oncall({{'$gen_media', agent_transfer}, {{Agent, Apid}, Timeout}}, _From, {BaseState, #oncall_state{oncall_pid = {_, Apid}} = Internal} = State) ->
	Call = BaseState#base_state.callrec,
	?NOTICE("Can't transfer to yourself, silly ~p! ~p", [Apid, Call#call.id]),
	{reply, invalid, oncall, State};

oncall({{'$gen_media', agent_transfer}, {{Agent, Apid}, Timeout}}, _From, {BaseState, Internal}) ->
	#base_state{callrec = Call, callback = Callback, url_pop_get_vars = GenPopopts} = BaseState,
	#oncall_state{oncall_pid = {OcAgent, Ocpid}, oncall_mon = Mon} = Internal,
	case set_agent_state(Apid, [ringing, Call]) of
		ok ->
			case Callback:handle_agent_transfer(Apid, Timeout, oncall, BaseState#base_state.callrec, Internal, BaseState#base_state.substate) of
				Success when element(1, Success) == ok ->
					Popopts = case Success of
						{ok, Substate} ->
							[];
						{ok, Opts, Substate} ->
							lists:ukeymerge(1, lists:ukeysort(1, GenPopopts), lists:ukeysort(1, Opts))
					end,
					% dummy is there because the structure of internally handled
					% messages is {{'$gen_media', command}, CommandData}.  
					% Even if the command doesn't take any arguments, something
					% needs to be there to make it a 2 element tuple, thus dummy
					% here.
					Tref = gen_fsm:send_event_after(Timeout, {{'$gen_media', stop_ring}, dummy}),
					cdr:agent_transfer(Call, {OcAgent, Agent}),
					cdr:ringing(Call, Agent),
					url_pop(Call, Apid, Popopts),
					RingMon = erlang:monitor(process, Apid),
					NewBase = BaseState#base_state{substate = Substate},
					NewInternal = #oncall_ringing_state{
						ring_pid = {Agent, Apid},
						ringout = Tref,
						ring_mon = RingMon,
						oncall_pid = {OcAgent, Ocpid},
						oncall_mon = Mon
					},
					{reply, ok, oncall_ringing, {NewBase, NewInternal}};
				{error, Error, NewState} ->
					?NOTICE("Could not set agent ringing for transfer ~p due to ~p", [Error, Call#call.id]),
					set_agent_state(Apid, [idle]),
					NewBase = BaseState#base_state{substate = NewState},
					{reply, invalid, oncall, {NewBase, Internal}}
			end;
		invalid ->
			?NOTICE("Could not ring ~p to target agent ~p", [Call#call.id, Apid]),
			{reply, invalid, oncall, {BaseState, Internal}}
	end;

oncall({{'$gen_media', warm_transfer_hold}, undefined}, _From, {BaseState, Internal}) ->
	#base_state{callback = Callback, callrec = Call, substate = Substate} = BaseState,
	#oncall_state{oncall_pid = {_, Apid}} = Internal,
	case erlang:function_exported(Callback, handle_warm_transfer_hold, 4) of
		true ->
			case Callback:handle_warm_transfer_hold(oncall, Call, Internal, Substate) of
				{ok, CallerRef, NewState} ->
					Res = set_agent_state(Apid, [warm_transfer_hold]),
					cdr:warm_transfer_hold(Call, Apid),
					NewBase = BaseState#base_state{ substate = NewState },
					NewInternal = #warm_transfer_hold_state{
						oncall_pid = Internal#oncall_state.oncall_pid,
						oncall_mon = Internal#oncall_state.oncall_mon,
						caller_ref = CallerRef
					},
					{reply, ok, warm_transfer_hold, {NewBase, NewInternal}};
				{error, Error, NewState} ->
					?DEBUG("Callback module ~w errored for warm transfer hold from oncall:  ~p for ~p", [Callback, Error, Call#call.id]),
					NewBase = BaseState#base_state{ substate = NewState},
					{reply, invalid, oncall, {NewBase, Internal}}
			end;
		_ ->
			{reply, invalid, oncall, {BaseState, Internal}}
	end;

oncall({{'$gen_media', spy}, {Spy, _}}, _From, {_, #oncall_state{oncall_pid = {_Nom, Spy}}} = State) ->
	?DEBUG("Can't spy on yourself", []),
	{reply, invalid, oncall, State};

oncall({{'$gen_media', spy}, {Spy, AgentRec}}, _From, {BaseState, Oncall} = State) ->
	Callback = BaseState#base_state.callback,
	Call = BaseState#base_state.callrec,
	case erlang:function_exported(Callback, handle_spy, 4) of
		false ->
			?DEBUG("Callback ~p doesn't support spy for ~p", [Callback, Call#call.id]),
			{reply, invalid, oncall, State};
		true ->
			case Callback:handle_spy({Spy, AgentRec}, oncall, Call, Oncall, BaseState#base_state.substate) of
				{ok, Newstate} ->
					{reply, ok, oncall, {BaseState#base_state{substate = Newstate}, Oncall}};
				{invalid, Newstate} ->
					{reply, invalid, oncall, {BaseState#base_state{substate = Newstate}, Oncall}};
				{error, Error, Newstate} ->
					?INFO("Callback ~p errored ~p on spy for ~p", [Callback, Error, Call#call.id]),
					{reply, {error, Error}, oncall, {BaseState#base_state{substate = Newstate}, Oncall}}
			end
	end;

oncall({{'$gen_media', wrapup}, undefined}, {Ocpid, _Tag} = From,
		{#base_state{callrec = Call} = BaseState,
		#oncall_state{oncall_pid = {Ocagent, Ocpid}} = Oncall})
		when Call#call.media_path =:= inband ->
	?INFO("Request to end call ~p from agent", [Call#call.id]),
	Callback = BaseState#base_state.callback,
	cdr:wrapup(Call, Ocagent),
	case Callback:handle_wrapup(From, oncall, Call, Oncall, BaseState#base_state.substate) of
		{ok, NewState} ->
			erlang:demonitor(Oncall#oncall_state.oncall_mon),
			{reply, ok, wrapup, {BaseState#base_state{substate = NewState}, #wrapup_state{}}};
		{hangup, NewState} ->
			cdr:hangup(BaseState#base_state.callrec, "agent"),
			erlang:demonitor(Oncall#oncall_state.oncall_mon),
			{stop, normal, ok, {BaseState#base_state{substate = NewState}, Oncall#oncall_state{oncall_mon = undefined}}}
	end;

oncall({{'$gen_media', wrapup}, undefined}, {Ocpid, _Tag}, {#base_state{callrec = Call} = BaseState, #oncall_state{oncall_pid = {_Agent, Ocpid}} = Internal} = State) ->
	?ERROR("Cannot do a wrapup directly unless mediapath is inband, and request is from agent oncall. ~p", [Call#call.id]),
	{reply, invalid, oncall, State};

oncall({{'$gen_media', queue}, Queue}, {Ocpid, _},
		{#base_state{callback = Callback, callrec = Call} = BaseState,
		#oncall_state{oncall_mon = Mons, oncall_pid = {Ocagent, Ocpid}} = Oncall} = State) ->
	Internal = Oncall,
	?INFO("request to queue call ~p from agent", [Call#call.id]),
	% Decrement the call's priority by 5 when requeueing
	case priv_queue(Queue, reprioritize_for_requeue(Call), BaseState#base_state.queue_failover) of
		invalid ->
			{reply, invalid, State};
		{default, Qpid} ->
			{ok, NewState} = Callback:handle_queue_transfer({Queue, Qpid}, oncall, Call, Oncall, BaseState#base_state.substate),
			cdr:queue_transfer(Call, "default_queue"),
			cdr:inqueue(Call, "default_queue"),
			cdr:wrapup(Call, Ocagent),
			set_cpx_mon({BaseState#base_state{substate = NewState},#wrapup_state{}}, [{queue, "default_queue"}]),
			erlang:demonitor(Internal#oncall_state.oncall_mon),
			{reply, ok, {BaseState#base_state{substate = NewState}, #wrapup_state{}}};
		Qpid when is_pid(Qpid) ->
			{ok, NewState} = Callback:handle_queue_transfer({Queue, Qpid}, oncall, Call, Oncall, BaseState#base_state.substate),
			cdr:queue_transfer(Call, Queue),
			cdr:inqueue(Call, Queue),
			cdr:wrapup(Call, Ocagent),
			erlang:demonitor(Internal#oncall_state.oncall_mon),
			NewInternal = #inqueue_state{
				queue_pid = {Queue, Qpid},
				queue_mon = erlang:monitor(process, Qpid)
			},
			set_cpx_mon({BaseState#base_state{substate = NewState}, NewInternal}, [{queue, Queue}]),
			{reply, ok, inqueue, {BaseState#base_state{substate = NewState}, NewInternal}}
	end;

oncall({{'$gen_media', Command}, _}, _, State) ->
	?DEBUG("Invalid command sync event ~s while oncall", [Command]),
	{reply, invalid, oncall, State};

oncall(Request, From, 
		{#base_state{callback = Callback, callrec = Call} = BaseState,
		#oncall_state{oncall_pid = OcPid} = Oncall} = State) ->
	?DEBUG("Forwarding request to callback", []),
	Out = Callback:handle_call(Request, Call, oncall, OcPid),
	handle_custom_return(Out, oncall, reply, State).


oncall({{'$gen_media', Command}, _}, State) ->
	?DEBUG("Invalid command event ~s while oncall", [Command]),
	{next_state, oncall, State};

oncall(Msg, {#base_state{callback = Callback, callrec = Call} = BaseState,
		Extra} = State) ->
	Return = Callback:handle_cast(Msg, oncall, Call, Extra, BaseState#base_state.substate),
	handle_custom_return(Return, oncall, noreply, State).

%%--------------------------------------------------------------------
%% oncall_ringing -> oncall (same agent), oncall (new agent)
%%--------------------------------------------------------------------

oncall_ringing({{'$gen_media', agent_oncall}, undefined}, {Apid, _}, 
		{#base_state{callrec = #call{ring_path = outband} = Call},
		#oncall_ringing_state{ring_pid = {_, Apid}}} = State) ->
	?INFO("Cannot accept on call requests from agent (~p) unless ring_path is inband for ~p", [Apid, Call#call.id]),
	{reply, invalid, oncall_ringing, State};

oncall_ringing({{'$gen_media', agent_oncall}, undefined}, {Rpid, _},
		{#base_state{callrec = #call{ring_path = inband} = Call} = BaseState,
		#oncall_ringing_state{ring_pid = {Ragent, Rpid}} = Internal}) ->
	#base_state{callback = Callback} = BaseState,
	#oncall_ringing_state{oncall_pid = {OcAgent, Ocpid}, ring_mon = Rmon, 
		oncall_mon = Ocmon} = Internal,
	?INFO("oncall request during what looks like an agent transfer (inband) for ~p", [Call#call.id]),
	case Callback:handle_answer(Rpid, oncall_ringing, Call, Internal, BaseState#base_state.substate) of
		{ok, NewState} ->
			kill_outband_ring({BaseState, Internal}),
			cdr:oncall(Call, Ragent),
			gen_fsm:cancel_timer(Internal#oncall_ringing_state.ringout),
			set_agent_state(Ocpid, [wrapup, Call]),
			cdr:wrapup(Call, OcAgent),
			erlang:demonitor(Ocmon),
			NewBase = BaseState#base_state{substate = NewState},
			NewInternal = #oncall_state{
				oncall_pid = Internal#oncall_ringing_state.ring_pid,
				oncall_mon = Internal#oncall_ringing_state.ring_mon
			},
			set_cpx_mon({NewBase, NewInternal}, [{agent, Ragent}]),
			{reply, ok, oncall, {NewBase, NewInternal}};
		{error, Reason, NewState} ->
			?ERROR("Cannot set ~p for ~p to oncall due to ~p", [Rpid, Call#call.id, Reason]),
			NewBase = BaseState#base_state{ substate = NewState},
			{reply, invalid, oncall_ringing, {NewBase, Internal}}
	end;

oncall_ringing({{'$gen_media', agent_oncall}, undefined}, _, State) ->
	{BaseState, Internal} = State,
	#base_state{callback = Callback, callrec = Call} = BaseState,
	#oncall_ringing_state{ring_pid = {Ragent, Rpid},
		oncall_pid = {OcAgent, Ocpid}, oncall_mon = Ocmon} = Internal,
	?INFO("oncall request during what looks like an agent transfer (outofband) to ~p for ~p", [Ragent, Call#call.id]),
	case set_agent_state(Rpid, [oncall, Call]) of
		invalid ->
			{reply, invalid, oncall_ringing, State};
		ok ->
			case Callback:handle_answer(Rpid, oncall_ringing, Call, Internal, BaseState#base_state.substate) of
				{ok, NewState} ->
					kill_outband_ring(State),
					cdr:oncall(Call, Ragent),
					gen_fsm:cancel_timer(Internal#oncall_ringing_state.ringout),
					set_agent_state(Ocpid, [wrapup, Call]),
					cdr:wrapup(Call, OcAgent),
					NewBase = BaseState#base_state{ substate = NewState},
					NewInternal = #oncall_state{
						oncall_pid = {Ragent, Rpid},
						oncall_mon = Internal#oncall_ringing_state.ring_mon
					},
					set_cpx_mon({NewBase, NewInternal}, [{agent, Ragent}]),
					erlang:demonitor(Ocmon),
					{reply, ok, oncall, {NewBase, NewInternal}};
				{error, Reason, NewState} ->
					?ERROR("Cannot set ~p to oncall due to ~p for ~p", [Rpid, Reason, Call#call.id]),
					NewBase = BaseState#base_state{ substate = NewState},
					{reply, invalid, oncall_ringing, {NewBase, Internal}}
			end
	end;

oncall_ringing({{'$gen_media', Command}, _}, _From, State) ->
	?DEBUG("Invalid sync event command ~s while oncall_ringing", [Command]),
	{reply, invalid, oncall_ringing, State};

oncall_ringing(Msg, From, {#base_state{callback = Callback, callrec = Call}
		= BaseState, Extra} = State) ->
	Return = Callback:handle_call(Msg, From, oncall_ringing, Extra, BaseState#base_state.substate),
	handle_custom_return(Return, oncall_ringing, reply, State).

oncall_ringing({{'$gen_media', Command}, _}, State) ->
	?DEBUG("invalid event command ~s while oncall_ringing", [Command]),
	{next_state, oncall_ringing, State};

oncall_ringing(Msg, {#base_state{callback = Callback, callrec = Call} = 
		BaseState, Extra} = State) ->
	Return = Callback:handle_cast(Msg, oncall_ringing, Call, Extra, BaseState#base_state.substate),
	handle_custom_return(Return, oncall_ringing, noreply, State).

%%--------------------------------------------------------------------
%% wrapup
%%--------------------------------------------------------------------

wrapup({{'$gen_media', Command}, _}, _From, State) ->
	?DEBUG("Invalid sync command ~p Command whiel in wrapup", [Command]),
	{reply, invalid, wrapup, State};

wrapup(Msg, From, {#base_state{callback = Callback, callrec = Call} = 
		BaseState, Extra} = State) ->
	Return = Callback:handle_call(Msg, From, wrapup, Call, Extra, BaseState#base_state.substate),
	handle_custom_return(Return, wrapup, reply, State).

wrapup({{'$gen_media', Command}, _}, State) ->
	?DEBUG("Invalid command ~p while wrapup", [Command]),
	{next_state, wrapup, State};

wrapup(Msg, {#base_state{callback = Callback, callrec = Call} =
		BaseState, Extra} = State) ->
	Return = Callback:handle_cast(Msg, wrapup, Call, Extra, BaseState#base_state.substate),
	handle_custom_return(Return, wrapup, noreply, State).

%%--------------------------------------------------------------------
%% handle_sync_event
%%--------------------------------------------------------------------

handle_sync_event({{'$gen_media', get_url_vars}, _}, _From, StateName, State) ->
	{BaseState, _Internal} = State,
	#base_state{
		substate = Substate,
		url_pop_get_vars = GenPopopts,
		callback = Callback
	} = BaseState,
	Cbopts = case erlang:function_exported(Callback, urlpop_getvars, 1) of
		true ->
			Callback:urlpop_getvars(Substate);
		false ->
			[]
	end,
	Out = lists:ukeymerge(1, lists:ukeysort(1, GenPopopts), lists:ukeysort(1, Cbopts)),
	{reply, {ok, Out}, StateName, State};

handle_sync_event({{'$gen_media', get_call}, undefined}, _From, StateName, State) ->
	{BaseState, _} = State,
	#base_state{callrec = Reply} = BaseState,
	{reply, Reply, StateName, State};

handle_sync_event({{'$gen_media', Command}, _}, _From, StateName, State) ->
	?DEBUG("Invalid generic sync command ~p while in ~s", [Command, StateName]),
	{reply, invalid, StateName, State};

handle_sync_event(Msg, From, StateName, {#base_state{callback = Callback,
		callrec = Call} = BaseState, Extra} = State) ->
	Return = Callback:handle_call(Msg, From, StateName, Call, Extra, BaseState#base_state.substate),
	handle_custom_return(Return, StateName, reply, State).

%%--------------------------------------------------------------------
%% handle_event
%%--------------------------------------------------------------------

handle_event({{'$gen_media', set_url_getvars}, Vars}, StateName, State) ->
	{BaseState, Internal} = State,
	#base_state{url_pop_get_vars = Oldvars} = BaseState,
	Newvars = lists:ukeymerge(1, lists:ukeysort(1, Vars), lists:ukeysort(1, Oldvars)),
	?DEBUG("Input:  ~p;  Old:  ~p;  new:  ~p", [Vars, Oldvars, Newvars]),
	NewBase = BaseState#base_state{url_pop_get_vars = Newvars},
	{next_state, StateName, {NewBase, Internal}};

handle_event({{'$gen_media', add_skills}, Skills}, StateName, State) ->
	{BaseState, Internal} = State,
	Call = BaseState#base_state.callrec,
	Newskills = lists:umerge(Call#call.skills, Skills),
	Newcall = Call#call{skills = Newskills},
	{next_state, StateName, {BaseState#base_state{callrec = Newcall}, Internal}};

handle_event({{'$gen_media', Command}, _}, StateName, State) ->
	?DEBUG("Invalid generic event command ~p while in state ~p", [Command, StateName]),
	{next_state, StateName, State};

handle_event(Msg, StateName, {BaseState, Internal} = State) ->
	#base_state{callback = Callback, callrec = Call, substate = Sub} = BaseState,
	Reply = Callback:handle_cast(Msg, StateName, Call, Internal, Sub),
	handle_custom_return(Reply, StateName, noreply, State).

%%--------------------------------------------------------------------
%% handle_info
%%--------------------------------------------------------------------

handle_info({'DOWN', Ref, process, Pid, Info}, inqueue, {BaseState,
		#inqueue_state{ queue_pid = {Q, Pid}, queue_mon = Ref} = Internal}) ->
	#base_state{callrec = Call} = BaseState,
	?WARNING("Queue ~p died due to ~p (I'm ~p)", [Q, Info, Call#call.id]),
	%% in theory, the cook will tell us when the queue is back up.
	NewInternal = Internal#inqueue_state{
		queue_pid = {Q, undefined},
		queue_mon = undefined
	},
	{next_state, inqueue, {BaseState, NewInternal}};

handle_info({'DOWN', Ref, process, Pid, Info}, inqueue_ringing, {BaseState,
		#inqueue_ringing_state{ queue_pid = {Q, Pid}, queue_mon = Ref} =
		Internal}) ->
	#base_state{callrec = Call} = BaseState,
	?WARNING("Queue ~p died due to ~p (I'm ~p)", [Q, Info, Call#call.id]),
	%% in theory, the cook will tell us when the queue is back up.
	NewInternal = Internal#inqueue_ringing_state{
		queue_pid = {Q, undefined},
		queue_mon = undefined
	},
	{next_state, inqueue_ringing, {BaseState, NewInternal}};

handle_info({'DOWN', Ref, process, Pid, Info}, inqueue_ringing, {BaseState,
		#inqueue_ringing_state{cook_mon = Ref, cook = Ref} = Internal}) ->
	#base_state{callrec = Call, callback = Callback, substate = Sub} = BaseState,
	#inqueue_ringing_state{ring_pid = {Aname, Apid}} = Internal,
	?WARNING("Cook died due to ~p (I'm ~p)", [Info, Call#call.id]),
	case agent:query_state(Apid) of
		{ok, ringing} ->
			set_agent_state(Apid, [idle]);
		_ ->
			ok
	end,
	{ok, NewSub} = Callback:handle_ring_stop(inqueue_ringing, Call, Internal, Sub),
	kill_outband_ring({BaseState, Internal}),
	cdr:ringout(Call, {cook_death, Aname}),
	erlang:demonitor(Internal#inqueue_ringing_state.ring_mon),
	NewCall = Call#call{cook = undefined},
	NewBase = BaseState#base_state{substate = NewSub, callrec = NewCall},
	NewInternal = #inqueue_state{
		queue_mon = Internal#inqueue_ringing_state.queue_mon,
		queue_pid = Internal#inqueue_ringing_state.queue_pid
	},
	{next_state, inqueue, {NewBase, NewInternal}};

handle_info({'DOWN', Ref, process, Pid, Info}, inqueue, {BaseState,
		#inqueue_state{cook = Pid, cook_mon = Ref} = Internal}) ->
	% not much to do, a ressurected cook will tell us about itself.
	#base_state{callrec = Call} = BaseState,
	NewCall = Call#call{cook = undefined},
	NewBase = BaseState#base_state{callrec = Call},
	NewInternal = #inqueue_state{
		cook = undefined,
		cook_mon = undefined
	},
	{next_state, inqueue, {NewBase, NewInternal}};
	
handle_info({'DOWN', Ref, process, Pid, Info}, inqueue_ringing, {BaseState,
		#inqueue_ringing_state{ring_pid = {Aname, Pid}, ring_mon = Ref} = Internal}) ->
	#base_state{callrec = Call, callback = Callback, substate = Sub} = BaseState,
	?WARNING("ringing Agent fsm ~p died due to ~p (I'm ~p)", [Aname, Info, Call#call.id]),
	% no need to modify agent state since it's already dead.
	gen_server:cast(Call#call.cook, stop_ringing),
	{ok, Newsub} = Callback:handle_ring_stop(inqueue_ringing, Call, Internal, Sub),
	kill_outband_ring({BaseState, Internal}),
	cdr:ringout(Call, {agent_fsm_death, Aname}),
	NewBase = BaseState#base_state{substate = Sub},
	#inqueue_ringing_state{queue_mon = Qmon, queue_pid = Qpid, cook_mon = Cmon,
		cook = Cook} = Internal,
	NewInternal = #inqueue_state{
		queue_mon = Qmon,
		queue_pid = Qpid,
		cook = Cook,
		cook_mon = Cmon
	},
	{next_state, inqueue, {NewBase, NewInternal}};

handle_info({'DOWN', Ref, process, Pid, Info}, oncall_ringing, {BaseState,
		#oncall_ringing_state{oncall_pid = {Agent, Pid}, oncall_mon = Ref} =
		Internal}) ->
	?WARNING("Oncall agent ~p died while in agent transfer", [Pid]),
	#base_state{callrec = Call, callback = Callback, substate = Sub} = BaseState,
	% it is up to the media to do the messages to ultimately end ringing
	% if that actually matters.
	{default, Qpid} = priv_queue("default_queue", reprioritize_for_requeue(Call), true),
	{ok, NewSub} = Callback:handle_queue_transfer({"default_queue", Qpid}, oncall_ringing, Call, Internal, Sub),
	Qmon = erlang:monitor(process, Qpid),
	NewBase = BaseState#base_state{substate = NewSub},
	#oncall_ringing_state{ring_pid = Rpid, ring_mon = Rmon} = Internal,
	NewInternal = #inqueue_ringing_state{
		ring_pid = Rpid, ring_mon = Rmon, queue_pid = {"default_queue", Qpid},
		queue_mon = Qmon
	},
	{next_state, inqueue_ringing, {NewBase, NewInternal}};
	
handle_info({'DOWN', Ref, process, Pid, Info}, oncall_ringing, {BaseState,
		#oncall_ringing_state{ring_pid = {Agent, Pid}, ring_mon = Ref} = 
		Internal}) ->
	#base_state{callrec = Call, callback = Callback, substate = Sub} = BaseState,
	?WARNING("Ringing agent ~p died while oncall", [Pid]),
	{ok, NewSub} = Callback:handle_ring_stop({Agent, Pid}, oncall_ringing, Call, Internal, Sub),
	NewBase = BaseState#base_state{substate = NewSub},
	#oncall_ringing_state{oncall_pid = Ocpid, oncall_mon = OcMon} = Internal,
	NewInternal = #oncall_state{oncall_pid = Ocpid, oncall_mon = OcMon},
	{next_state, oncall, {NewBase, NewInternal}};

handle_info({'DOWN', Ref, process, Pid, Info}, oncall, {BaseState, 
		#oncall_state{oncall_pid = {Agent, Pid}, oncall_mon = Ref} = 
		Internal}) ->
	?WARNING("Oncall agent ~p died due to ~p", [Pid, Info]),
	#base_state{callrec = Call, callback = Callback, substate = Sub} = BaseState,
	 case priv_queue("default_queue", reprioritize_for_requeue(Call), false) of
		invalid ->
			{stop, {agent_died, Info}, {BaseState, Internal}};
		Qpid ->
			{ok, NewState} = Callback:handle_queue_transfer({"default_queue", Qpid}, oncall, Call, Internal, Sub),
			NewBase = BaseState#base_state{substate = Sub},
			Qmon = erlang:monitor(process, Qpid),
			NewInternal = #inqueue_state{
				queue_pid = {"default_queue", Qpid},
				queue_mon = Qmon
			},
			{next_state, inqueue, {NewBase, NewInternal}}
	end;

handle_info(Msg, StateName, {#base_state{callback = Callback,
		callrec = Call} = BaseState, Extra} = State) ->
	Return = Callback:handle_info(Msg, StateName, Call, Extra, BaseState#base_state.substate),
	handle_custom_return(Return, StateName, noreply, State).

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------

terminate(Reason, StateName, {#base_state{callback = Callback, callrec = 
		Call} = BaseState, Extra} = State) ->
	set_cpx_mon(State, delete),
	Callback:terminate(Reason, StateName, Call, Extra, BaseState#base_state.substate).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------

%% @private
code_change(OldVsn, StateName, State, Extra) ->
	{BaseState, Internal} = State,
	#base_state{callback = Callback, callrec = Call, substate = Sub} = BaseState,
	{ok, Newsub} = Callback:code_change(OldVsn, Call, StateName, Sub, Internal, Extra),
    {ok, StateName, {BaseState#base_state{substate = Newsub}, Internal}}.

-spec(format_status/2 :: (Cause :: any(), [any()]) -> {#base_state{}, any()}).
format_status(Normalcy, [PDict, {#base_state{callback = Mod, substate =
		SubState, callrec = Call} = BaseState, Internal} = State]) ->
	% prevent client data from being dumped
	NewCall = case Call#call.client of
		Client when is_record(Client, client) ->
			Call#call{client = Client#client{options = []}};
		_ ->
			Call
	end,
	% allow media callback to scrub its state
	SubStat = case erlang:function_exported(Mod, format_status, 2) of
		true ->
			case catch Mod:format_status(Normalcy, [PDict, SubState]) of
				{'EXIT', _} -> SubState;
				Else -> Else
			end;
		_ ->
			SubState
	end,
	NewBase = BaseState#base_state{callrec = NewCall, substate = SubStat},
	[{data, [{"State", {NewBase, Internal}}, {"SubState", SubStat}]}];

format_status(terminate, Args) ->
	format_status(normal, Args).

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% handle_custom_return
%%--------------------------------------------------------------------

handle_custom_return({queue, Queue, PCallrec, NewState}, inivr, Reply, {BaseState, CurState}) ->
	Callrec = correct_client(PCallrec),
	QData = case priv_queue(Queue, Callrec, BaseState#base_state.queue_failover) of
		{default, Qpid} ->
			cdr:cdrinit(Callrec),
			cdr:inqueue(Callrec, "default_queue"),
			set_cpx_mon({BaseState#base_state{callrec = Callrec, substate = NewState}, undefined}, [{queue, "default_queue"}]),
			{ok, {"default_queue", Qpid}};
			%{noreply, State#state{callrec = Callrec, substate = NewState, queue_pid = {"default_queue", Qpid}, monitors = Newmons}};
		invalid ->
			?WARNING("Could not queue ~p into ~p (failover ~p)", [Callrec#call.id, Queue, BaseState#base_state.queue_failover]),
			{error, {noqueue, Queue}};
			%{noreply, State#state{callrec = Callrec, substate = NewState}};
		Qpid ->
			cdr:cdrinit(Callrec),
			cdr:inqueue(Callrec, Queue),
			set_cpx_mon({BaseState#base_state{callrec = Callrec, substate = NewState}, undefined}, [{queue, Queue}]),
			{ok, {Queue, Qpid}}
			%{noreply, State#state{callrec = Callrec, substate = NewState, queue_pid = {Queue, Qpid}, monitors = Newmons}}
	end,
	case {Reply, QData} of
		{noreply, {error, _}} ->
			{next_state, inivr, {BaseState#base_state{substate = NewState}, CurState}};
		{reply, {error, _}} ->
			{reply, QData, inivr, {BaseState#base_state{substate = NewState}, CurState}};
		{_, {ok, {QueueName, QPid}}} ->
			Mon = erlang:monitor(process, QPid),
			InternalState = #inqueue_state{
				queue_mon = Mon,
				queue_pid = {QueueName, QPid}
			},
			NewBase = BaseState#base_state{
				callrec = Callrec,
				substate = NewState
			},
			case Reply of
				reply ->
					{reply, ok, inqueue, {NewBase, InternalState}};
				noreply ->
					{next_state, inqueue, {NewBase, InternalState}}
			end
	end;

handle_custom_return({voicemail, NewState}, StateName, Reply, 
		{BaseState, InternalState}) when StateName =:= inqueue; 
		StateName =:= inqueue_ringing ->
	priv_voicemail({BaseState, InternalState}),
	NewBase = BaseState#base_state{
		substate = NewState
	},
	case Reply of
		reply ->
			{reply, ok, wrapup, {NewBase, #wrapup_state{}}};
		noreply ->
			{next_state, wrapup, {NewBase, #wrapup_state{}}}
	end;

handle_custom_return({stop, Reason, Reply, NewState}, StateName, reply, 
		{BaseState, Internal}) ->
	{NewStop, {NewBase, NewInternal}} = handle_stop(Reason, StateName, BaseState, Internal),
	{stop, NewStop, Reply, {NewBase#base_state{substate = NewState}, NewInternal}};

handle_custom_return({stop, Reason, NewState}, StateName, _Reply,
		{BaseState, Internal}) ->
	{NewStop, {NewBase, NewInternal}} = handle_stop(Reason, StateName, BaseState, Internal),
	{stop, NewStop, {NewBase#base_state{substate = NewState}, Internal}};

handle_custom_return({outgoing, AgentChannel, NewState}, StateName, Reply,
		{#base_state{callrec = Call} = BaseState, InternalState}) when 
		is_record(BaseState#base_state.callrec, call) ->
	handle_custom_return({outgoing, AgentChannel, Call, NewState}, StateName, Reply, {BaseState, InternalState});

handle_custom_return({outgoing, {AgentName, AgentChannel}, Call, NewState}, StateName, Reply,
		{BaseState, InternalState}) when is_record(Call, call) ->
	?INFO("Told to set ~s (~p) to outgoing for ~p", [AgentName, AgentChannel, Call#call.id]),
	Response = set_agent_state(AgentChannel, [oncall, Call]),
	State0 = case Response of
		ok ->
			cdr:oncall(Call, AgentName),
			NewBase = BaseState#base_state{
				substate = NewState,
				callrec = Call
			},
			NewInternal = #oncall_state{
				oncall_pid = {AgentName, AgentChannel},
				oncall_mon = erlang:monitor(process, AgentChannel)
			},
			set_cpx_mon({NewBase, NewInternal}, [{agent, AgentName}]),
			{NewBase, NewInternal};
		Else ->
			{BaseState, InternalState}
	end,
	case {Response, Reply} of
		{ok, noreply} ->
			{next_state, oncall, State0};
		{ok, reply} ->
			{reply, ok, oncall, State0};
		{Err, noreply} ->
			?WARNING("Could not set ~p oncall:  ~p", [AgentName, Err]),
			{stop, Err, State0};
		{Err, reply} ->
			?WARNING("Could not set ~p oncall:  ~p", [AgentName, Err]),
			{stop, Err, invalid, State0}
	end;
			
handle_custom_return({reply, Reply, NewState}, State, reply, {BaseState, Internal}) ->
	NewBase = BaseState#base_state{substate = NewState},
	{reply, Reply, State, {NewBase, Internal}};

% Timebernate:  Timeout or hibernate
handle_custom_return({reply, Reply, NewState, Timebernate}, State, reply, {BaseState, Internal}) ->
	NewBase = BaseState#base_state{substate = NewState},
	{reply, Reply, State, {NewBase, Internal}, Timebernate};

handle_custom_return({noreply, NewState}, State, _Reply, {BaseState, Internal}) ->
	NewBase = BaseState#base_state{substate = NewState},
	{next_state, State, {NewBase, Internal}};

handle_custom_return({noreply, NewState, Timebernate}, State, _Reply, {BaseState, Internal}) ->
	NewBase = BaseState#base_state{substate = NewState},
	{next_state, State, {NewBase, Internal}, Timebernate};

handle_custom_return({hangup, NewSub}, inqueue, Reply, State) ->
	handle_custom_return({{hangup, undefined}, NewSub}, inqueue, Reply, State);

handle_custom_return({{hangup, Who}, NewSub}, inqueue, Reply, 
		{#base_state{callrec = Callrec} = BaseState, Internal}) ->
	#inqueue_state{queue_mon = Qmon, queue_pid = {_, Qpid}} = Internal,
	?INFO("hang for ~p up when only queue is a pid", [Callrec#call.id]),
	unqueue(Qpid, self()),
	cdr:hangup(Callrec, Who),
	erlang:demonitor(Qmon),
	case Reply of
		reply ->
			{reply, ok, wrapup, {BaseState#base_state{substate = NewSub}, #wrapup_state{}}};
		noreply ->
			{next_state, wrapup, {BaseState#base_state{substate = NewSub}, #wrapup_state{}}}
	end;

handle_custom_return({{hangup, Who}, NewSub}, inivr, Reply,
		{#base_state{callrec = Callrec} = BaseState, Internal}) ->
	case is_record(Callrec, call) of
		true ->
			?INFO("hangup for ~s while inivr", [Callrec#call.id]),
			cdr:hangup(Callrec, Who);
		_ ->
			?INFO("hangup nor a not yet defined call inivr", [])
	end,
	case Reply of
		reply ->
			{reply, ok, wrapup, {BaseState#base_state{substate = NewSub}, #wrapup_state{}}};
		noreply ->
			{next_state, wrapup, {BaseState#base_state{substate = NewSub}, #wrapup_state{}}}
	end;

handle_custom_return({{hangup, Who}, NewState}, inqueue_ringing, noreply,
		{#base_state{callrec = Callrec} = BaseState, Internal}) ->
	?INFO("hangup for ~s whiile inivr", [Callrec#call.id]),
	cdr:hangup(Callrec, Who),
	% for now just going to trust the agent connection hears this die.
	erlang:demonitor(Internal#inqueue_ringing_state.ring_mon),
	erlang:demonitor(Internal#inqueue_ringing_state.cook_mon),
	erlang:demonitor(Internal#inqueue_ringing_state.queue_mon),
	unqueue(Internal#inqueue_ringing_state.queue_pid,self()),
	{next_state, wrapup, {BaseState#base_state{substate = NewState}, #wrapup_state{}}};

handle_custom_return({{hangup, Who}, NewState}, wrapup, noreply,
		{#base_state{callrec = Callrec} = BaseState, Internal}) ->
	% this can occur when a media goes to voicemail.
	% TODO add a 'leaving voicemail' state?
	?INFO("hangup for ~s while in wrapup", [Callrec#call.id]),
	cdr:hangup(Callrec, Who),
	% leaving it up to the media whether it should stop or not.
	{next_state, wrapup, {BaseState#base_state{substate = NewState}, Internal}};

handle_custom_return({mutate, NewCallback, NewState}, State, noreply,
		{BaseState, Internal}) ->
	?INFO("mutating to ~p from ~p", [NewCallback, BaseState#base_state.callback]),
	NewBase = BaseState#base_state{callback = NewCallback, substate = NewState},
	{next_state, State, {NewBase, Internal}};

handle_custom_return({mutate, Reply, NewCallback, NewState}, State, reply, 
		{BaseState, Internal}) ->
	?INFO("mutating to ~p from ~p", [NewCallback, BaseState#base_state.callback]),
	NewBase = BaseState#base_state{callback = NewCallback,
		substate = NewState},
	{reply, Reply, State, {NewBase, Internal}};

handle_custom_return({AgentInteract, Reply, NewState}, State, reply, 
		StateTuple) when State =:= oncall;
		State =:= oncall_ringing;
		State =:= warm_transfer_hold;
		State =:= warm_transfer_3rd_party;
		State =:= warm_transfer_merged ->
	{NextState, {BaseState, Internal}} = agent_interact(AgentInteract, State, StateTuple),
	NewBase = BaseState#base_state{substate = NewState},
	{reply, Reply, NextState, {NewBase, Internal}};

handle_custom_return({AgentInteract, NewState}, StateName, _Reply, State)
		when StateName =:= oncall;
		StateName =:= oncall_ringing;
		StateName =:= inqueue_ringing;
		StateName =:= warm_transfer_hold;
		StateName =:= warm_transfer_3rd_party;
		StateName =:= warm_transfer_merged ->
	{NextState, {BaseState, Internal}} = agent_interact(AgentInteract, StateName, State),
	NewBase = BaseState#base_state{substate = NewState},
	{next_state, NextState, {NewBase, Internal}}.

set_agent_state(Apid, Args) ->
	try apply(agent_channel, set_state, [Apid | Args]) of
		ok ->
			ok;
		Res ->
			?ERROR("Agent set state wasn't okay:  ~p", [Res]),
			Res
	catch
		exit:{noproc, {gen_fsm, sync_send_event, _TheArgs}} ->
			?WARNING("Agent ~p is a dead pid", [Apid]),
			badagent;
		exit:{max_ringouts, {gen_fsm, sync_send_event, _TheArgs}} ->
			?DEBUG("Max ringouts reached for agent ~p", [Apid]),
			badagent
	end.

handle_stop(hangup, StateName, BaseState, Internal) ->
	handle_stop({hangup, undefined}, StateName, BaseState, Internal);

handle_stop(Reason, inqueue, #base_state{callrec = Call} = BaseState,
		#inqueue_state{queue_pid = {Queuenom, _}} = Internal) ->
	?DEBUG("Once queued in ~p, assuming something else handles hangup.  ~p", [Queuenom, Call#call.id]),
	set_cpx_mon({BaseState, Internal}, delete),
	case Reason of
		{hangup, _} ->
			{normal, {BaseState, Internal}};
		_ ->
			{Reason, {BaseState, Internal}}
	end;

handle_stop(Reason, StateName, #base_state{callrec = Call} = BaseState,
		Internal) when StateName =:= inivr; StateName =:= wrapup ->
	set_cpx_mon({BaseState, Internal}, delete),
	case {is_record(Call, call), Reason} of
		{true, {hangup, Who}} ->
			cdr:hangup(Call, Who);
		_ ->
			ok
	end,
	case Reason of
		{hangup, _} -> {normal, {BaseState, Internal}};
		_ -> {Reason, {BaseState, Internal}}
	end;

handle_stop(Reason, StateName, BaseState, Internal) ->
	set_cpx_mon({BaseState, Internal}, delete),
	{Who, Reason} = case Reason of
		{hangup, W} -> {W, normal};
		_ -> {undefined, Reason}
	end,
	agent_interact({hangup, Who}, StateName, {BaseState, Internal}),
	Reason.
	
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
			agent_channel:url_pop(Agent, Url, "ring")
	end.

%% @doc Set the client record to an actual client record.
-spec(correct_client/1 :: (Callid :: #call{}) -> #call{}).
correct_client(#call{client = Client} = Callrec) ->
	Newclient = case Client of
		#client{id = Id} ->
			correct_client_sub({Id,Client#client.options});
		undefined ->
			correct_client_sub(undefined);
		{Id,Opts} ->
			correct_client_sub({Id,Opts});
		String ->
			% if given the client id; so a media is not burndened with checking
			% mnesia or the client itself.
			% basically, see the next function call:
			correct_client_sub({String,[]})
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
correct_client_sub({Id,Opts}) ->
	#client{options = Defaults} = Client = try call_queue_config:get_client(id, Id) of
		none ->
			correct_client_sub(undefined);
		Else ->
			Else
	catch
		error:{case_clause, {aborted, {node_not_running, _Node}}} ->
			#client{}
	end,
	Opts0 = lists:sort(Opts),
	Defs0 = lists:sort(Defaults),
	Opts1 = merge_defaults(Opts0,Defs0),
	Client#client{options = Opts1}.

merge_defaults(Opts,Defaults) ->
	merge_defaults(Opts,Defaults,[]).

merge_defaults([],Rest,Acc) ->
	lists:append(Rest,Acc);
merge_defaults(Rest,[],Acc) ->
	lists:append(Rest,Acc);
merge_defaults([{Key,_Val} = H | OTail], [{Key,_Val1} | DTail], Acc) ->
	merge_defaults(OTail,DTail,[H|Acc]);
merge_defaults([{OKey,_} = H | OTail], [{DKey,_} | _] = Defs, Acc) when OKey > DKey ->
	merge_defaults(OTail,Defs,[H | Acc]);
merge_defaults(Opts, [H | Tail], Acc) ->
	merge_defaults(Opts, Tail, [H | Acc]).

-spec(set_cpx_mon/2 :: (State :: {#base_state{}, any()}, Action :: proplist() | 'delete') -> 'ok').
set_cpx_mon({#base_state{callrec = Call}, _}, delete) ->
	cpx_monitor:drop({media, Call#call.id});
set_cpx_mon({#base_state{callrec = _Call}, _} = State, Details) ->
	set_cpx_mon(State, Details, ignore).

set_cpx_mon({#base_state{callrec = Call}, _}, Details, Watch) ->
	Client = Call#call.client,
	MidBasedet = [
		{type, Call#call.type},
		{callerid, Call#call.callerid},
		{dnis, Call#call.dnis},
		{client, Client},
		{ring_path, Call#call.ring_path},
		{media_path, Call#call.media_path},
		{direction, Call#call.direction},
		{node, node()}
	],
	{_Hp, Basedet} = case {proplists:get_value(queue, Details), proplists:get_value(agent, Details)} of
		{undefined, undefined} ->
			{[], MidBasedet};
		{undefined, _A} ->
			{[{agent_link, {0, 60 * 5, 60 * 15, {time, util:now()}}}], MidBasedet};
		{_Q, _} ->
			{[{inqueue, {0, 60 * 5, 60 * 10, {time, util:now()}}}], [{queued_at, {timestamp, util:now()}}, {priority, Call#call.priority} | MidBasedet]}
	end,
	Fulldet = lists:append([Basedet, Details]),
	cpx_monitor:set({media, Call#call.id}, Fulldet, Watch).

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

priv_voicemail({BaseState, #inqueue_ringing_state{ring_mon = Rmon, ring_pid = {_, Rpid}} = Internal}) ->
	erlang:demonitor(Rmon),
	set_agent_state(Rpid, [idle]),
	NewInternal = #inqueue_state{
		queue_mon = Internal#inqueue_ringing_state.queue_mon,
		queue_pid = Internal#inqueue_ringing_state.queue_pid,
		cook = Internal#inqueue_ringing_state.cook
	},
	priv_voicemail({BaseState, NewInternal});

priv_voicemail({BaseState, #inqueue_state{queue_mon = Mon, queue_pid = {QNom, QPid}}}) ->
	erlang:demonitor(Mon),
	call_queue:remove(QPid, self()),
	cdr:voicemail(BaseState#base_state.callrec, QNom),
	ok.

% make the call higher priority in preparation for requeueing
reprioritize_for_requeue(Call) ->
	NewPriority = case Call#call.priority of
		P when P < 5 ->
			0;
		P ->
			P - 5
	end,
	Call#call{priority = NewPriority}.

unqueue(undefined, _Callpid) ->
	ok;
unqueue({_Qnom, Qpid}, Callpid) when is_pid(Qpid) ->
	unqueue(Qpid, Callpid);
unqueue(Qpid, Callpid) when is_pid(Qpid) ->
	call_queue:remove(Qpid, Callpid),
	ok.

kill_outband_ring({_, #inqueue_ringing_state{outband_ring_pid = P}}) when is_pid(P) ->
	freeswitch_ring:hangup(P);
kill_outband_ring(_) ->
	ok.

extract_agent(#inqueue_ringing_state{ring_pid = O, ring_mon = M}) ->
	{M, O};
extract_agent(#oncall_state{oncall_pid = O, oncall_mon = M}) ->
	{M, O};
extract_agent(#oncall_ringing_state{oncall_pid = O, oncall_mon = M}) ->
	{M, O};
extract_agent(#warm_transfer_hold_state{oncall_pid = O, oncall_mon = M}) ->
	{M, O};
extract_agent(#warm_transfer_3rd_party_state{oncall_pid = O, oncall_mon = M}) ->
	{M, O};
extract_agent(#warm_transfer_merged_state{oncall_pid = O, oncall_mon = M}) ->
	{M, O}.

agent_interact(Action, StateName, {BaseState, Internal}) ->
	Agent = extract_agent(Internal),
	agent_interact(Action, StateName, BaseState, Internal, Agent).

agent_interact({mediapush, Data}, StateName, #base_state{
		callrec = Call} = BaseState, Internal, {_, {_, Ocpid}}) ->
	?DEBUG("Shoving ~p from ~p", [Data, Call#call.id]),
	agent_channel:media_push(Ocpid, Call, Data),
	{StateName, {BaseState, Internal}};

agent_interact(stop_ring, StateName, BaseState, Internal, Agent) ->
	agent_interact({stop_ring, undefined}, StateName, BaseState, Internal, Agent);

agent_interact({stop_ring, Reason}, oncall_ringing, #base_state{
		callrec = Call} = BaseState, #oncall_ringing_state{ringout = Ringout,
		ring_pid = RingAgent, ring_mon = RMon} = Internal, _Agent) ->
	case Call#call.cook of
		CookPid when is_pid(CookPid) ->
			gen_server:cast(CookPid, stop_ringing);
		_ ->
			ok
	end,
	case Ringout of
		undefined -> ok;
		_ -> gen_fsm:cancel_timer(Ringout)
	end,
	case RingAgent of
		undefined -> ok;
		{Nom, Apid} ->
			set_agent_state(Apid, [idle]),
			cdr:ringout(Call, {Reason, Nom}),
			erlang:demonitor(RMon)
	end,
	NewInternal = #oncall_state{
		oncall_mon = Internal#oncall_ringing_state.oncall_mon,
		oncall_pid = Internal#oncall_ringing_state.oncall_pid
	},
	{oncall, {BaseState, NewInternal}};

agent_interact({stop_ring, Reason}, inqueue_ringing, #base_state{
		callrec = Call} = BaseState, #inqueue_ringing_state{ringout = Ringout,
		ring_pid = RingAgent, ring_mon = RMon} = Internal, _Agent) ->
	case Call#call.cook of
		CookPid when is_pid(CookPid) ->
			gen_server:cast(CookPid, stop_ringing);
		_ ->
			ok
	end,
	case Ringout of
		undefined -> ok;
		_ -> gen_fsm:cancel_timer(Ringout)
	end,
	case RingAgent of
		undefined -> ok;
		{Nom, Apid} ->
			set_agent_state(Apid, [idle]),
			cdr:ringout(Call, {Reason, Nom}),
			erlang:demonitor(RMon)
	end,
	NewInternal = #inqueue_state{
		queue_mon = Internal#inqueue_ringing_state.queue_mon,
		queue_pid = Internal#inqueue_ringing_state.queue_pid,
		cook = Internal#inqueue_ringing_state.cook
	},
	{inqueue, {BaseState, NewInternal}};

agent_interact(wrapup, StateName, #base_state{callrec = Call} = BaseState,
		Internal, {Mon, {Agent, Apid}}) ->
	?INFO("Attempting to set agent at ~p to wrapup for ~p", [Apid, Call#call.id]),
	set_agent_state(Apid, [wrapup, Call]),
	cdr:wrapup(Call, Agent),
	erlang:demonitor(Mon),
	{wrapup, {BaseState, #wrapup_state{}}};

agent_interact(hangup, StateName, BaseState, Internal, Agent) ->
	Hangup = {hangup, undefined},
	agent_interact(Hangup, StateName, BaseState, Internal, Agent);

agent_interact({hangup, Who}, oncall_ringing, #base_state{
		callrec = Callrec} = BaseState, #oncall_ringing_state{
		ring_mon = Rmon, ring_pid = Rpid} = Internal,
		{OcMonref, {OcName, Ocpid}}) ->
	?INFO("hangup for ~s when oncall_ringing", [Callrec#call.id]),
	set_agent_state(element(2, Rpid), [idle]),
	set_agent_state(Ocpid, [wrapup, Callrec]),
	cdr:wrapup(Callrec, OcName),
	cdr:hangup(Callrec, Who),
	kill_outband_ring(Internal),
	erlang:demonitor(Rmon),
	erlang:demonitor(OcMonref),
	{wrapup, {BaseState, #wrapup_state{}}};

agent_interact({hangup, Who}, inqueue_ringing, #base_state{
		callrec = Call} = BaseState, Internal, {Rmon, {Rname, Rpid}}) ->
	?INFO("hangup for ~p when both agent and queue are pid", [Call#call.id]),
	set_agent_state(Rpid, [idle]),
	cdr:hangup(Call, Who),
	kill_outband_ring(Internal),
	erlang:demonitor(Rmon),
	#inqueue_ringing_state{queue_pid = {_, Qpid}, queue_mon = Qmon} = Internal,
	unqueue(Qpid, self()),
	erlang:demonitor(Qmon),
	{wrapup, {BaseState, #wrapup_state{}}};

agent_interact({hangup, Who}, oncall, #base_state{callrec = Callrec} = 
		BaseState, Internal, {Mon, {Agent, Apid}}) ->
	?INFO("hangup for ~p when only oncall is a pid", [Callrec#call.id]),
	set_agent_state(Apid, [wrapup, Callrec]),
	cdr:wrapup(Callrec, Agent),
	cdr:hangup(Callrec, Who),
	erlang:demonitor(Mon),
	{wrapup, {BaseState, #wrapup_state{}}};

agent_interact({hangup, Who}, State, #base_state{callrec = Callrec} = 
		BaseState, Internal, {Mon, {Agent, Apid}}) when
		is_record(Callrec, call) ->
	?INFO("hangup for ~p while in state ~p; not taking action at this time.", [Callrec#call.id, State]),
	{State, {BaseState, Internal}}.

-ifdef(TEST).

dead_spawn() ->
	spawn(fun() -> ok end).

url_pop_test_() ->
	{setup,
	fun() ->
		{ok, Agent} = gen_server_mock:new(),
		Call = #call{id = "testcall", source = dead_spawn()},
		{Call, Agent, undefined}
	end,
	fun({_, Agent, Conn}) ->
		gen_server_mock:stop(Agent)
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
			gen_server_mock:expect_info(Agent, fun({'$gen_all_state_event', {url_pop, "example.com", "ring"}}, _) -> ok end),
			url_pop(Call, Agent, []),
			gen_server_mock:assert_expectations(Agent)
		end},
		{"url is set with some additional options",
		fun() ->
			Call = BaseCall#call{client = #client{label = "client", id = "client", options = [{url_pop, "example.com?a=b"}]}},
			gen_server_mock:expect_info(Agent, fun({'$gen_all_state_event', {url_pop, "example.com?a=b&addkey=addval", "ring"}}, _) -> ok end),
			url_pop(Call, Agent, [{"addkey", "addval"}]),
			gen_server_mock:assert_expectations(Agent)
		end},
		{"url is set with some additional options, some of which are blank",
		fun() ->
			Call = BaseCall#call{client = #client{label = "client", id = "client", options = [{url_pop, "example.com?a=b"}]}},
			gen_server_mock:expect_info(Agent, fun({'$gen_all_state_event', {url_pop, "example.com?a=b&addkey=addval", "ring"}}, _) -> ok end),
			url_pop(Call, Agent, [{"addkey", "addval"}, {"foo", undefined}]),
			gen_server_mock:assert_expectations(Agent)
		end}
	]
	end}.

%% TODO Fix tests.
init_test_d() ->
	util:start_testnode(),
	N = util:start_testnode(gen_media_init_tests),
	{spawn, N, {foreach,
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
	[
%fun({_, _, Assertmocks}) ->
%		{"call rec returned, but no queue",
%		fun() ->
%			Args = [[{id, "dummy"}, {queues, none}], success],
%			Res = init([dummy_media, Args]),
%			?assertMatch({ok, #state{callback = dummy_media, callrec = #call{id = "dummy"}}}, Res),
%			Assertmocks()
%		end}
%	end,
%	fun({QMmock, Qpid, Assertmocks}) ->
%		{"call rec and queue name returned",
%		fun() ->
%			Args = [[{queues, ["testqueue"]}, {id, "dummy"}], success],
%			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
%				{ok, Qpid, State}
%			end),
%			gen_server_mock:expect_call(Qpid, fun({add, 40, _Inpid, _Callrec}, _From, _State) -> ok end),
%			Res = init([dummy_media, Args]),
%			?assertMatch({ok, #state{callback = dummy_media, callrec = #call{id = "dummy"}, queue_pid = {"testqueue", Qpid}}}, Res),
%			#state{monitors = Mons} = element(2, Res),
%			?assertNot(undefined =:= Mons#monitors.queue_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({QMmock, Qpid, Assertmocks}) ->
%		{"call rec and queue name returned, but queue doesn't exist",
%		fun() ->
%			Args = [[{queues, ["testqueue"]}, {id, "dummy"}], success],
%			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
%				{ok, undefined, State}
%			end),
%			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "default_queue"}, _From, State, _Elec) ->
%				{ok, Qpid, State}
%			end),
%			gen_server_mock:expect_call(Qpid, fun({add, 40, _Inpid, _Callrec}, _From, _State) -> ok end),
%			Res = init([dummy_media, Args]),
%			?assertMatch({ok, #state{callback = dummy_media, callrec = #call{id = "dummy"}, queue_pid = {"default_queue", Qpid}}}, Res),
%			Assertmocks()
%		end}
%	end
]}}.

-record(state_changes_mocks, {
	queue_manager,
	queue,
	agent_manager,
	cdr,
	assert,
	make_state
}).

handle_state_changes_test_d() ->
	util:start_testnode(),
	N = util:start_testnode(gen_media_state_changes_tests),
	{spawn, N, {setup,
	fun() ->
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
			{ok, {#base_state{callrec = Callrec} = Out, _}} = init([dummy_media, [[{queues, none}], success]]),
			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}), 
			Out
		end,
		#state_changes_mocks{ queue_manager = QMmock, queue = Qpid,
			agent_manager = Ammock, assert = Assertmocks, make_state = Makestate
		}
	end,
	fun(Mocks) ->
		gen_server_mock:stop(Mocks#state_changes_mocks.queue_manager),
		gen_leader_mock:stop(Mocks#state_changes_mocks.queue),
		gen_leader_mock:stop(Mocks#state_changes_mocks.agent_manager),
		gen_event:stop(cdr)
	end, [
	]}}.




		
%handle_call_test_() ->
%	util:start_testnode(),
%	N = util:start_testnode(gen_media_handle_call_tests),
%	{spawn, N, {foreach,
%	fun() ->
%		{ok, QMmock} = gen_leader_mock:start(queue_manager),
%		{ok, Qpid} = gen_server_mock:new(),
%		{ok, Ammock} = gen_leader_mock:start(agent_manager),
%		gen_event:start({local, cdr}),
%		Assertmocks = fun() ->
%			gen_server_mock:assert_expectations(Qpid),
%			gen_leader_mock:assert_expectations(QMmock),
%			gen_leader_mock:assert_expectations(Ammock),
%			gen_event_mock:assert_expectations(cdr)
%		end,
%		Makestate = fun() ->
%			{ok, #state{callrec = Callrec} = Out} = init([dummy_media, [[{queues, none}], success]]),
%			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}), 
%			Out
%		end,
%		{Makestate, QMmock, Qpid, Ammock, Assertmocks}
%	end,
%	fun({_Makestate, QMmock, Qpid, Ammock, _Assertmocks}) ->
%		gen_server_mock:stop(Qpid),
%		gen_leader_mock:stop(QMmock),
%		gen_leader_mock:stop(Ammock),
%		gen_event:stop(cdr),
%		timer:sleep(10)
%	end,
%	[fun({Makestate, _QMock, _Qpid, _Ammock, Assertmocks}) ->
%		{"spying when there's no agent oncall fails",
%		fun() ->
%			Seedstate = Makestate(),
%			?assertMatch({reply, invalid, Seedstate}, handle_call({'$gen_media', spy, "Pid", "AgentRec"}, "from", Seedstate)),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMock, _Qpid, Ammock, Assertmocks}) ->
%		{"Spy is not the pid making the request",
%		fun() ->
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Spy} = agent:start(#agent{login = "testagent"}),
%			Seedstate = Makestate(),
%			State = Seedstate#state{oncall_pid = {"testagent", Spy}},
%			?assertMatch({reply, invalid, State}, handle_call({'$gen_media', spy, Spy, "AgentRec"}, {self(), "tag"}, State)),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMock, _Qpid, _Ammock, _Assertmocks}) ->
%		{"Can't spy on yourself", 
%		fun() ->
%			Seedstate = Makestate(),
%			Spy = dead_spawn(),
%			State = Seedstate#state{oncall_pid = {"testagent", Spy}},
%			?assertMatch({reply, invalid, State}, handle_call({'$gen_media', spy, Spy, "AgentRec"}, {Spy, "tag"}, State))
%		end}
%	end,
%	fun({_Makestate, _QMock, _Qpid, Ammock, Assertmocks}) ->
%		{"Spy valid, callback says no",
%		fun() ->
%			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
%			Callrec = Seedstate#state.callrec,
%			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
%			Ocpid = dead_spawn(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			SpyRec = #agent{login = "testagent"},
%			{ok, Spy} = agent:start(SpyRec),
%			?assertMatch({reply, invalid, _Newstate}, handle_call({'$gen_media', spy, Spy, SpyRec}, {Spy, "tag"}, Seedstate#state{oncall_pid = {"testagent", Ocpid}})),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMock, _Qpid, Ammock, Assertmocks}) ->
%		{"Spy valid, callback says ok",
%		fun() ->
%			Seedstate = Makestate(),
%			Ocpid = dead_spawn(),
%			State = Seedstate#state{oncall_pid = {"ocagent", Ocpid}},
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			SpyRec = #agent{login = "testagent"},
%			{ok, Spy} = agent:start(SpyRec),
%			?assertMatch({reply, ok, _Newstate}, handle_call({'$gen_media', spy, Spy}, {Spy, "tag"}, State)),
%			Assertmocks()
%		end}
%	end,	
%	fun({Makestate, _, _, Ammock, Assertmocks}) ->
%		{"oncall_pid requests wrapup",
%		fun() ->
%			Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, "testagent"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({hangup, _Callrer, _Time, "agent"}, _State) -> ok end),
%			Monref = make_ref(),
%			Mons = #monitors{oncall_pid = Monref},
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
%			Out = handle_call('$gen_media', wrapup, {Agent, "tag"}, State),
%			?assertMatch({stop, normal, ok, _State}, Out),
%			#state{monitors = Newmon} = element(4, Out),
%			?assertEqual(undefined, Newmon#monitors.oncall_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"oncall_pid can't request wrapup when media_path is outband",
%		fun() ->
%			#state{callrec = Oldcall} = Seedstate = Makestate(),
%			Callrec = Oldcall#call{media_path = outband},
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, callrec = Callrec},
%			?assertMatch({reply, invalid, _State}, handle_call('$gen_media', wrapup, {Agent, "tag"}, State)),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
%		{"sending to queue requested by oncall pid, all works",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
%				{ok, Qpid, State}
%			end),
%			gen_server_mock:expect_call(Qpid, fun({add, 35, Mpid, Rec}, _From, _State) ->
%				Mpid = Callrec#call.source,
%				Rec = Callrec#call{priority = 35},
%				ok
%			end),
%			gen_event_mock:expect_event(cdr, fun({queue_transfer, _Callrec, _Time, "testqueue"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({inqueue, _Callrec, _Time, "testqueue"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, _Agent}, _State) -> ok end),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
%			{reply, ok, Newstate} = handle_call({'$gen_media', queue, "testqueue"}, {Agent, "tag"}, State),
%			?assertEqual(undefined, Newstate#state.oncall_pid),
%			?assertEqual({"testqueue", Qpid}, Newstate#state.queue_pid),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.oncall_pid),
%			?assertNot(undefined =:= Newmons#monitors.queue_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
%		{"oncall pid sends call to queue, but falls back to default queue",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
%			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, MState, _Elec) ->
%				{ok, undefined, MState}
%			end),
%			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "default_queue"}, _From, MState, _Elec) ->
%				{ok, Qpid, MState}
%			end),
%			gen_server_mock:expect_call(Qpid, fun({add, 35, Mpid, Rec}, _From, _State) ->
%				Mpid = Callrec#call.source,
%				Rec = Callrec#call{priority = 35},
%				ok
%			end),
%			gen_event_mock:expect_event(cdr, fun({queue_transfer, _Callrec, _Time, "default_queue"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({inqueue, _Callrec, _Time, "default_queue"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, _Agent}, _State) -> ok end),
%			{reply, ok, Newstate} = handle_call({'$gen_media', queue, "testqueue"}, {Agent, "tag"}, State),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newstate#state.oncall_pid),
%			?assertEqual({"default_queue", Qpid}, Newstate#state.queue_pid),
%			?assertEqual(undefined, Newmons#monitors.oncall_pid),
%			?assertNot(undefined =:= Newmons#monitors.queue_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"oncall pid sends call to queue, but falls back to nowhere w/ fallback set to false",
%		fun() ->
%			Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, queue_failover = false, monitors = Mons},
%			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, MState, _Elec) ->
%				{ok, undefined, MState}
%			end),
%			Out = handle_call({'$gen_media', queue, "testqueue"}, {Agent, "tag"}, State),
%			?assertMatch({reply, invalid, _State}, Out),
%			Newstate = element(3, Out),
%			?assertEqual(Mons, Newstate#state.monitors),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, Qpid, Ammock, Assertmocks}) ->
%		{"sent to queue by something else, and alls well.",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			gen_server_mock:expect_call(Qpid, fun({add, 35, Mpid, Rec}, _From, _State) ->
%				Mpid = Callrec#call.source, 
%				Rec = Callrec#call{priority = 35},
%				ok
%			end),
%			gen_leader_mock:expect_leader_call(queue_manager, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
%				{ok, Qpid, State}
%			end),
%			gen_event_mock:expect_event(cdr, fun({queue_transfer, _Callrec, _Time, "testqueue"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({inqueue, _Callrec, _Time, "testqueue"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, _Agent}, _State) -> ok end),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
%			{reply, ok, Newstate} = handle_call({'$gen_media', queue, "testqueue"}, "from", State),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newstate#state.oncall_pid),
%			?assertEqual({"testqueue", Qpid}, Newstate#state.queue_pid),
%			?assertEqual(undefined, Newmons#monitors.oncall_pid),
%			?assertNot(undefined =:= Newmons#monitors.queue_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
%		{"sent to queue by something else, but falling back",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			gen_server_mock:expect_call(Qpid, fun({add, 35, Mpid, Rec}, _From, _state) ->
%				Mpid = Callrec#call.source,
%				Rec = Callrec#call{priority = 35},
%				ok
%			end),
%			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
%				{ok, undefined, State}
%			end),
%			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "default_queue"}, _From, State, _Elec) ->
%				{ok, Qpid, State}
%			end),
%			gen_event_mock:expect_event(cdr, fun({queue_transfer, _Callrec, _Time, "default_queue"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({inqueue, _Callrec, _Time, "default_queue"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, _Agent}, _State) -> ok end),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
%			{reply, ok, Newstate} = handle_call({'$gen_media', queue, "testqueue"}, "from", State),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newstate#state.oncall_pid),
%			?assertEqual({"default_queue", Qpid}, Newstate#state.queue_pid),
%			?assertEqual(undefined, Newmons#monitors.oncall_pid),
%			?assertNot(undefined =:= Newmons#monitors.queue_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"gen_media_ring setting agent successful, as is the callback module.",
%		fun() ->
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_event_mock:expect_event(cdr, fun({ringing, _Callrec, _Time, "testagent"}, _State) -> ok end),
%			#queued_call{cook = Cook} = Qcall = #queued_call{media = Callrec#call.source, id = "testcall"},
%			gen_leader_mock:expect_cast(Ammock, fun({end_avail, _}, _, _) -> ok end),
%			{reply, ok, Newstate} = handle_call({'$gen_media', ring, {"testagent", Agent}, Qcall, 100}, {Cook, "tag"}, Seedstate),
%			receive
%				{'$gen_media', stop_ring, Cook} ->
%					ok
%			after 150 ->
%				erlang:error(timer_timeout)
%			end,
%			?assertEqual({"testagent", Agent}, Newstate#state.ring_pid),
%			?assertNot(false =:= Newstate#state.ringout),
%			Mons = Newstate#state.monitors,
%			?assertNot(undefined =:= Mons#monitors.ring_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"gen_media_ring setting the agent fails.",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			#queued_call{cook = Cook} = Qcall = #queued_call{media = Callrec#call.source, id = "testcall"},
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = "whatever"}),
%			Out = handle_call({'$gen_media', ring, {"testagent", Agent}, Qcall, 100}, {Cook, "tag"}, Seedstate),
%			?assertMatch({reply, invalid, _State}, Out),
%			receive
%				{'$gen_media', stop_ring, Cook} ->
%					erlang:error(timer_lives)
%			after 150 ->
%				ok
%			end,
%			Newstate = element(3, Out),
%			?assertEqual(Seedstate#state.monitors, Newstate#state.monitors),
%			Assertmocks()
%		end}
%	end,
%	fun({_Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"gen_media_ring callback module fails",
%		fun() ->
%			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
%			Callrec = Seedstate#state.callrec,
%			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
%			#queued_call{cook = Cook} = Qcall = #queued_call{media = Callrec#call.source, id = "testcall"},
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({end_avail, _}, _, _) -> ok end),
%			%gen_leader_mock:expect_cast(Ammock, fun({end_avail, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({now_avail, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent", state = idle, statedata = {}}),
%			{reply, invalid, Newstate} = handle_call({'$gen_media', ring, {"testagent", Agent}, Qcall, 150}, {Cook, "tag"}, Seedstate),
%			receive
%				{'$gen_media', stop_ring, Cook} ->
%					erlang:error(timer_lives)
%			after 150 ->
%				ok
%			end,
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			?assertEqual({ok, idle}, agent:query_state(Agent)),
%			?assertEqual(Seedstate#state.monitors, Newstate#state.monitors),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"can't transfer to yourself, silly!",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
%			?assertEqual({reply, invalid, State}, handle_call({'$gen_media', agent_transfer, {"testagent", Agent}}, "from", State)),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"agent transfer, target agent can't change state",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			{ok, Target} = agent:start(#agent{login = "targetagent"}),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
%			?assertEqual({reply, invalid, State}, handle_call({'$gen_media', agent_transfer, {"targetagent", Target}, 100}, "from", State)),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"agent transfer, all is well",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			%% cdr makes 2 call outs to this, but that will be tested in cdr
%			gen_event_mock:expect_event(cdr, fun({agent_transfer, _Callrec, _Time, {"testagent", "targetagent"}}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({ringing, _Callrec, _Time, "targetagent"}, _State) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({end_avail, "targetagent"}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			{ok, Target} = agent:start(#agent{login = "targetagent"}),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
%			{reply, ok, Newstate} = handle_call({'$gen_media', agent_transfer, {"targetagent", Target}, 100}, "from", State),
%			receive
%				{'$gen_media', stop_ring, _Cook} ->
%					ok
%			after 150 ->
%				erlang:error(timer_nolives)
%			end,
%			?assertEqual({"targetagent", Target}, Newstate#state.ring_pid),
%			?assertEqual({"testagent", Agent}, Newstate#state.oncall_pid),
%			?assertNot(false =:= Newstate#state.ringout),
%			?assertEqual({ok, ringing}, agent:query_state(Target)),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(Mons#monitors.oncall_pid, Newmons#monitors.oncall_pid),
%			?assertNot(undefined =:= Newmons#monitors.ring_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({_Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"agent transfer, callback says no",
%		fun() ->
%			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
%			Callrec = Seedstate#state.callrec,
%			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Target} = agent:start(#agent{login = "testagent"}),
%			Agent = spawn(fun() -> ok end),
%			State = Seedstate#state{oncall_pid = {"testagent", Agent}},
%			{reply, invalid, Newstate} = handle_call({'$gen_media', agent_transfer, Target, 100}, "from", State),
%			receive
%				{'$gen_media', ring_stop, _Cook} ->
%					erlang:error(timer_lives)
%			after 150 ->
%				ok
%			end,
%			?assertNot(Newstate#state.ringout),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			?assertEqual({ok, idle}, agent:query_state(Target)),
%			Assertmocks()
%		end}
%	end,
%	fun({_Makestate, _QMmock, _Qpid, _Ammock, Assertmocks}) ->
%		{"gen_media_announce",
%		fun() ->
%			{ok, #state{callrec = Call} = Seedstate} = init([dummy_media, [[{queues, none}], success]]),
%			gen_event_mock:supplant(cdr, {{cdr, Call#call.id}, []}),
%			{reply, ok, Newstate} = handle_call({'$gen_media', announce, "doesn't matter"}, "from", Seedstate),
%			?CONSOLE("~p", [Seedstate]),
%			?CONSOLE("~p", [Newstate]),
%			?assertEqual({reply, ok, Seedstate}, handle_call({'$gen_media', announce, "doesn't matter"}, "from", Seedstate)),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, Qpid, _Ammock, Assertmocks}) ->
%		{"gen_media_voicemail works",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			Mons = #monitors{queue_pid = make_ref()},
%			State = Seedstate#state{queue_pid = {"default_queue", Qpid}, monitors = Mons},
%			gen_server_mock:expect_call(Qpid, fun({remove, Inpid}, _From, _State) ->
%				Inpid = Callrec#call.source,
%				ok
%			end),
%			gen_event_mock:expect_event(cdr, fun({voicemail, _Callrec, _Time, "default_queue"}, _State) -> ok end),
%			Out = handle_call('$gen_media', voicemail, "from", State),
%			?assertMatch({reply, ok, _State}, Out),
%			#state{monitors = Newmons} = element(3, Out),
%			?assertEqual(undefined, Newmons#monitors.queue_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, Qpid, Ammock, Assertmocks}) ->
%		{"gen_media_voicemail while an agent's ringing",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({now_avail, "testagent"}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			Mons = #monitors{queue_pid = make_ref(), ring_pid = make_ref()},
%			State = Seedstate#state{queue_pid = {"default_queue", Qpid}, ring_pid = {"testagent", Agent}, monitors = Mons},
%			gen_server_mock:expect_call(Qpid, fun({remove, Inpid}, _From, _State) ->
%				Inpid = Callrec#call.source,
%				ok
%			end),
%			gen_event_mock:expect_event(cdr, fun({voicemail, _Callrec, _Time, "default_queue"}, _State) -> ok end),
%			{reply, ok, NewState} = handle_call('$gen_media', voicemail, "from", State),
%			Newmons = NewState#state.monitors,
%			?assertEqual(undefined, NewState#state.ring_pid),
%			?assertEqual({ok, idle}, agent:query_state(Agent)),
%			?assertEqual(undefined, Newmons#monitors.queue_pid),
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({_Makestate, _QMmock, Qpid, _Ammock, Assertmocks}) ->
%		{"gen_media_voicemail callback says no",
%		fun() ->
%			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
%			#state{callrec = Call} = State = Seedstate#state{queue_pid = {"default_queue", Qpid}},
%			gen_event_mock:supplant(cdr, {{cdr, Call#call.id}, []}),
%			?assertMatch({reply, invalid, _State}, handle_call('$gen_media', voicemail, "from", State)),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, _Qpid, _Ammock, Assertmocks}) ->
%		{"Agent can't request oncall if ring_path is outband",
%		fun() ->
%			#state{callrec = Seedcall} = Seedstate = Makestate(),
%			Agent = spawn(fun() -> ok end),
%			Callrec = Seedcall#call{ring_path = outband},
%			State = Seedstate#state{callrec = Callrec, ring_pid = {"testagent", Agent}},
%			?assertEqual({reply, invalid, State}, handle_call('$gen_media', agent_oncall, {Agent, "tag"}, State)),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"agent oncall request when both a ring pid and oncall pid are set and media path is inband",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Oncall} = agent:start(#agent{login = "oncall"}),
%			{ok, Ring} = agent:start(#agent{login = "ringing"}),
%			gen_event_mock:expect_event(cdr, fun({oncall, _Callrec, _Time, "ringing"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, "oncall"}, _State) -> ok end),
%			{ok, Tref} = timer:send_after(100, timer_lives),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"oncall", Oncall}, ring_pid = {"ringing", Ring}, ringout = Tref, monitors = Mons},
%			{reply, ok, Newstate} = handle_call('$gen_media', agent_oncall, {Ring, "tag"}, State),
%			receive
%				timer_lives ->
%					erlang:error(timer_lives)
%			after 150 ->
%				ok
%			end,
%			?assertNot(Newstate#state.ringout),
%			?assertEqual({"ringing", Ring}, Newstate#state.oncall_pid),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			?assertEqual({ok, wrapup}, agent:query_state(Oncall)),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(Mons#monitors.ring_pid, Newmons#monitors.oncall_pid),
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({_Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"agent oncall request when both a ring pid and oncall pid are set and media path is inband, but callback says no",
%		fun() ->
%			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
%			Callrec = Seedstate#state.callrec,
%			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Oncall} = agent:start(#agent{login = "oncall"}),
%			{ok, Ring} = agent:start(#agent{login = "ring"}),
%			{ok, Tref} = timer:send_after(100, timer_lives),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = Seedstate#state{oncall_pid = {"oncall", Oncall}, ring_pid = {"ring", Ring}, ringout = Tref, monitors = Mons},
%			{reply, invalid, Newstate} = handle_call('$gen_media', agent_oncall, {Ring, "tag"}, State),
%			receive
%				timer_lives ->
%					ok
%			after 150 ->
%				erlang:error(timer_nolives)
%			end,
%			?assertEqual(State, Newstate),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"oncall during transfer with outband media",
%		fun() ->
%			#state{callrec = Seedcall} = Seedstate = Makestate(),
%			Callrec = Seedcall#call{ring_path = outband},
%			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({end_avail, "ring"}, _, _) -> ok end),
%			{ok, Oncall} = agent:start(#agent{login = "oncall"}),
%			{ok, Ring} = agent:start(#agent{login = "ring"}),
%			gen_event_mock:expect_event(cdr, fun({oncall, _Callrec, _Time, "ring"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, "oncall"}, _State) -> ok end),
%			{ok, Tref} = timer:send_after(100, timer_lives),
%			Mons = #monitors{
%				oncall_pid = make_ref(),
%				ring_pid = make_ref()
%			},
%			State = Seedstate#state{oncall_pid = {"oncall", Oncall}, ringout = Tref, ring_pid = {"ring", Ring}, callrec = Callrec, monitors = Mons},
%			{reply, ok, Newstate} = handle_call('$gen_media', agent_oncall, "from", State),
%			receive
%				timer_lives ->
%					erlang:error(timer_lives)
%			after 150 ->
%				ok
%			end,
%			?assertEqual({ok, oncall}, agent:query_state(Ring)),
%			?assertEqual({ok, wrapup}, agent:query_state(Oncall)),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			?assertNot(Newstate#state.ringout),
%			?assertEqual({"ring", Ring}, Newstate#state.oncall_pid),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(Mons#monitors.ring_pid, Newmons#monitors.oncall_pid),
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({_Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
%		{"oncall during transfer with outband media, but callback says no",
%		fun() ->
%			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
%			Seedcall = Seedstate#state.callrec,
%			gen_event_mock:supplant(cdr, {{cdr, Seedcall#call.id}, []}),
%			Callrec = Seedcall#call{ring_path = outband},
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({end_avail, "ring"}, _, _) -> ok end),
%			{ok, Oncall} = agent:start(#agent{login = "oncall"}),
%			{ok, Ring} = agent:start(#agent{login = "ring"}),
%			{ok, Tref} = timer:send_after(100, timer_lives),
%			State = Seedstate#state{oncall_pid = {"oncall", Oncall}, ringout = Tref, ring_pid = {"ring", Ring}, callrec = Callrec},
%			{reply, invalid, Newstate} = handle_call('$gen_media', agent_oncall, "from", State),
%			receive
%				timer_lives ->
%					ok
%			after 150 ->
%				erlang:error(timer_nolives)
%			end,
%			% TODO Two agents oncall due to why going on call is handled :/
%			% since there's no way to roll back an oncall, if the handle_answer
%			% callback fails, we have a f*cked state.
%			?assertEqual({ok, oncall}, agent:query_state(Ring)),
%			?assertEqual({ok, oncall}, agent:query_state(Oncall)),
%			?assertEqual({"ring", Ring}, Newstate#state.ring_pid),
%			?assertEqual(Tref, Newstate#state.ringout),
%			?assertEqual({"oncall", Oncall}, Newstate#state.oncall_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, Qpid, Ammock, Assertmocks}) ->
%		{"oncall queue to agent requested by agent with inband media",
%		fun() ->
%			#state{callrec = Callrec} = Seedstate = Makestate(),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			gen_server_mock:expect_call(Qpid, fun({remove, Inpid}, _From, _State) ->
%				Inpid = Callrec#call.source,
%				ok
%			end),
%			gen_event_mock:expect_event(cdr, fun({oncall, _Callrec, _Time, "testagent"}, _State) -> ok end),
%			{ok, Tref} = timer:send_after(100, timer_lives),
%			Mons = #monitors{ring_pid = make_ref(), queue_pid = make_ref()},
%			State = Seedstate#state{queue_pid = {"default_queue", Qpid}, ring_pid = {"testagent", Agent}, ringout = Tref, monitors = Mons},
%			{reply, ok, Newstate} = handle_call('$gen_media', agent_oncall, {Agent, "tag"}, State),
%			receive
%				timer_lives ->
%					erlang:error(timer_lives)
%			after 150 ->
%				ok
%			end,
%			?assertNot(Newstate#state.ringout),
%			?assertEqual({"testagent", Agent}, Newstate#state.oncall_pid),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			?assertEqual("default_queue", Newstate#state.queue_pid),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			?assertEqual(undefined, Newmons#monitors.queue_pid),
%			?assertEqual(Mons#monitors.ring_pid, Newmons#monitors.oncall_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({_Makestate, _QMmock, Qpid, Ammock, Assertmocks}) ->
%		{"oncall quee to agent request by agent, but callback says no",
%		fun() ->
%			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
%			Callrec = Seedstate#state.callrec,
%			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			{ok, Tref} = timer:send_after(100, timer_lives),
%			Mons = #monitors{ring_pid = make_ref()},
%			State = Seedstate#state{ringout = Tref, ring_pid = {"testagent", Agent}, queue_pid = {"default_queue", Qpid}, monitors = Mons},
%			{reply, invalid, Newstate} = handle_call('$gen_media', agent_oncall, {Agent, "tag"}, State),
%			receive
%				timer_lives ->
%					ok
%			after 150 ->
%				erlang:error(timer_nolives)
%			end,
%			?assertEqual({"testagent", Agent}, Newstate#state.ring_pid),
%			?assertEqual({"default_queue", Qpid}, Newstate#state.queue_pid),
%			?assertEqual(Tref, Newstate#state.ringout),
%			?assertEqual(Mons, Newstate#state.monitors),
%			Assertmocks()
%		end}
%	end,
%	fun({Makestate, _QMmock, Qpid, Ammock, Assertmocks}) ->
%		{"oncall queue to agent requst by whoever with outband media",
%		fun() ->
%			#state{callrec = Seedcall} = Seedstate = Makestate(),
%			Callrec = Seedcall#call{ring_path = outband, media_path = outband},
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(Ammock, fun({end_avail, "testagent"}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			gen_server_mock:expect_call(Qpid, fun({remove, Inpid}, _From, _State) ->
%				Inpid = Callrec#call.source,
%				ok
%			end),
%			gen_event_mock:expect_event(cdr, fun({oncall, _Callrec, _Time, "testagent"}, _State) -> ok end),
%			{ok, Tref} = timer:send_after(100, timer_lives),
%			Mons = #monitors{ring_pid = make_ref(), queue_pid = make_ref()},
%			State = Seedstate#state{callrec = Callrec, ring_pid = {"testagent", Agent}, queue_pid = {"default_queue", Qpid}, ringout = Tref, monitors = Mons},
%			{reply, ok, Newstate} = handle_call('$gen_media', agent_oncall, "from", State),
%			receive
%				timer_lives -> 
%					erlang:error(timer_lives)
%			after 150 ->
%				ok
%			end,
%			?assertNot(Newstate#state.ringout),
%			?assertEqual({"testagent", Agent}, Newstate#state.oncall_pid),
%			?assertEqual("default_queue", Newstate#state.queue_pid),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.queue_pid),
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			?assertEqual(Mons#monitors.ring_pid, Newmons#monitors.oncall_pid),
%			Assertmocks()
%		end}
%	end,
%	fun({_Makestate, _QMmock, Qpid, Ammock, Assertmocks}) ->
%		{"oncall queue to agent request by whoever with outband media, but callback says no",
%		fun() ->
%			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
%			Seedcall = Seedstate#state.callrec,
%			gen_event_mock:supplant(cdr, {{cdr, Seedcall#call.id}, []}),
%			Callrec = Seedcall#call{ring_path = outband, media_path = outband},
%			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			{ok, Tref} = timer:send_after(100, timer_lives),
%			Mons = #monitors{
%				oncall_pid = make_ref(),
%				queue_pid = make_ref()
%			},
%			State = Seedstate#state{callrec = Callrec, ring_pid = {"testagent", Agent}, ringout = Tref, queue_pid = {"default_queue", Qpid}, monitors = Mons},
%			{reply, invalid, Newstate} = handle_call('$gen_media', agent_oncall, "from", State),
%			receive
%				timer_lives -> 
%					ok
%			after 150 ->
%				erlang:error(timer_nolives)
%			end,
%			?assertNot(false =:= Newstate#state.ringout),
%			?assertEqual({"testagent", Agent}, Newstate#state.ring_pid),
%			?assertEqual({"default_queue", Qpid}, Newstate#state.queue_pid),
%			?assertEqual(Mons, Newstate#state.monitors),
%			Assertmocks()
%%		end}
%%	end,
%%	fun({Makestate, _QMmock, _Qpid, _Ammock, _Assertmocks}) ->
%%		{"late oncall request (ring_pid is undefined)",
%%		fun() ->
%%			Seedstate = Makestate(),
%%			State = Seedstate#state{ring_pid = undefined},
%%			?assertMatch({reply, invalid, State}, handle_call('$gen_media', agent_oncall, "from", State))
%%		end}
%%	end,
%%	fun({_Makestate, _QMmock, Qpid, _Ammock, Assertmocks}) ->
%%		{"oncall request to agent that is no longer running",
%%		fun() ->
%%			{ok, Seedstate} = init([dummy_media, [[{queues, none}], success]]),
%%			Seedcall = Seedstate#state.callrec,
%%			gen_event_mock:supplant(cdr, {{cdr, Seedcall#call.id}, []}),
%%			Callrec = Seedcall#call{ring_path = outband, media_path = outband},
%%			Agent = spawn(fun() -> ok end),
%%			{ok, Tref} = timer:send_after(100, timer_lives),
%%			Mon = #monitors{ring_pid = make_ref()},
%%			State = Seedstate#state{callrec = Callrec, ring_pid = {"deadagent", Agent}, ringout = Tref, queue_pid = {"default_queue", Qpid}, monitors = Mon},
%%			gen_event_mock:expect_event(cdr, fun({ringout, _Callrec, _Time, _Data}, _) -> ok end),
%%			Out = handle_call('$gen_media', agent_oncall, "from", State),
%%			?assertMatch({reply, invalid, _}, Out),
%%			{reply, invalid, Newstate} = Out,
%%			receive
%%				timer_lives ->
%%					erlang:error(timer_lives)
%%			after 150 ->
%%				ok
%%			end,
%%			?assert(false == Newstate#state.ringout),
%%			?assertEqual(undefined, Newstate#state.ring_pid),
%%			?assertEqual({"default_queue", Qpid}, State#state.queue_pid),
%%			Assertmocks()
%%		end}
%%	end]}}.
%
%handle_cast_test_() ->
%	util:start_testnode(),
%	N = util:start_testnode(gen_media_handle_cast),
%	{spawn, N, {foreach,
%	fun() ->
%		Call = #call{
%			id = "testcall",
%			source = self()
%		},
%		{#state{callrec = Call}}
%	end,
%	fun(_) ->
%		ok
%	end,
%	[fun({Seedstate}) ->
%		{"setting outband ring pid",
%		fun() ->
%			P = dead_spawn(),
%			{noreply, #state{outband_ring_pid = NewP}} = handle_cast({'$gen_media', set_outband_ring_pid, P}, Seedstate),
%			?assertEqual(P, NewP)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"setting the cook with no previous mons",
%		fun() ->
%			P = spawn(fun() ->
%				receive
%					done ->
%						ok
%				end
%			end),
%			{noreply, #state{callrec = Newcall, monitors = Mons}} = handle_cast({'$gen_media', set_cook, P}, Seedstate),
%			?assert(Mons#monitors.cook =/= undefined andalso is_reference(Mons#monitors.cook)),
%			?assertEqual(P, Newcall#call.cook),
%			P ! done
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"setting additional url pop opts",
%		fun() ->
%			{noreply, #state{url_pop_getvars = Urlget}} = handle_cast({'$gen_media', set_url_getvars, [{"key", "val"}]}, Seedstate),
%			?assertEqual([{"key", "val"}], Urlget)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"setting new url pop opts doen't nix old ones",
%		fun() ->
%			State = Seedstate#state{url_pop_getvars = [{"oldkey", "oldval"}]},
%			{noreply, #state{url_pop_getvars = Newget}} = handle_cast({'$gen_media', set_url_getvars, [{"newkey", "newval"}]}, State),
%			?assertEqual("oldval", proplists:get_value("oldkey", Newget)),
%			?assertEqual("newval", proplists:get_value("newkey", Newget))
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"Adding new skills",
%		fun() ->
%			{noreply, #state{callrec = Callrec}} = handle_cast({'$gen_media', add_skills, [cookskill, {'_agent', "anagent"}]}, Seedstate),
%			?assertEqual([cookskill, {'_agent', "anagent"}], Callrec#call.skills)
%		end}
%	end,
%	fun({#state{callrec = Oldcall} = Seedstate}) ->
%		{"Adding existing skills",
%		fun() ->
%			Call = Oldcall#call{skills = [cookskill]},
%			State = Seedstate#state{callrec = Call},
%			{noreply, #state{callrec = Newcall}} = handle_cast({'$gen_media', add_skills, [cookskill]}, State),
%			?assertEqual([cookskill], Newcall#call.skills)
%		end}
%	end]}}.
%
%handle_info_test_() ->
%	util:start_testnode(),
%	N = util:start_testnode(gen_media_handle_info_tests),
%	{spawn, N, {foreach,
%	fun() ->
%		{ok, Seedstate} = init([dummy_media, [[{queues, none}], success]]),
%		{Seedstate}
%	end,
%	fun(_) ->
%		ok
%	end,
%	[fun({#state{callrec = Oldcall} = Seedstate}) ->
%		{"agent requests a ring stop",
%		fun() ->
%			{ok, Apid} = agent:start(#agent{login = "testagent"}),
%			{ok, Cook} = gen_server_mock:new(),
%			gen_server_mock:expect_cast(Cook, fun(stop_ringing, _State) -> ok end),
%			{ok, Am} = gen_leader_mock:start(agent_manager),
%			gen_leader_mock:expect_leader_call(Am, fun(_, _, State, _) -> {ok, "testagent", State} end),
%			Callrec = Oldcall#call{cook = Cook},
%			Mons = #monitors{ring_pid = make_ref()},
%			State = Seedstate#state{ring_pid = {"testagent", Apid}, callrec = Callrec, monitors = Mons},
%			{noreply, Newstate} = handle_info({'$gen_media', stop_ring, Apid}, State),
%			?assertNot(Newstate#state.ringout),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			gen_server_mock:assert_expectations(Cook),
%			gen_server_mock:stop(Cook),
%			gen_leader_mock:stop(Am)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"ring stop request with no ring_pid defined",
%		fun() ->
%			{noreply, Newstate} = handle_info({'$gen_media', stop_ring, "doesn't matter"}, Seedstate),
%			?assertEqual(Seedstate, Newstate)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"ring stop request with no ringout handled",
%		fun() ->
%			Pid = spawn(fun() -> ok end),
%			Mons = #monitors{ring_pid = make_ref()},
%			State = Seedstate#state{ring_pid = Pid, monitors = Mons},
%			{noreply, Newstate} = handle_info({'$gen_media', stop_ring, "doesn't matter"}, State),
%			?assertEqual(State, Newstate)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"ring stop request with a live agent ringing",
%		fun() ->
%			{ok, Cook} = gen_server_mock:new(),
%			gen_server_mock:expect_cast(Cook, fun(stop_ringing, _State) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			{ok, Am} = gen_leader_mock:start(agent_manager),
%			gen_leader_mock:expect_leader_call(Am, fun(_, _, State, _) -> {ok, "testagent", State} end),
%			gen_leader_mock:expect_cast(Am, fun({now_avail, _}, _, _) -> ok end),
%			Mons = #monitors{ring_pid = make_ref()},
%			State = Seedstate#state{ring_pid = {"testagent", Agent}, ringout = true, monitors = Mons},
%			{noreply, Newstate} = handle_info({'$gen_media', stop_ring, Cook}, State),
%			gen_server_mock:assert_expectations(Cook),
%			?assertEqual({ok, idle}, agent:query_state(Agent)),
%			?assertNot(Newstate#state.ringout),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			gen_leader_mock:stop(Am)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"ring stop request with a live agent in wrong state",
%		fun() ->
%			{ok, Cook} = gen_server_mock:new(),
%			gen_server_mock:expect_cast(Cook, fun(stop_ringing, _State) -> ok end),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			{ok, Am} = gen_leader_mock:start(agent_manager),
%			gen_leader_mock:expect_leader_call(Am, fun(_, _, State, _) -> {ok, "testagent", State} end),
%			Mons = #monitors{ring_pid = make_ref()},
%			State = Seedstate#state{ring_pid = {"testagent", Agent}, ringout = true, monitors = Mons},
%			{noreply, Newstate} = handle_info({'$gen_media', stop_ring, Cook}, State),
%			gen_server_mock:assert_expectations(Cook),
%			?assertEqual({ok, oncall}, agent:query_state(Agent)),
%			?assertNot(Newstate#state.ringout),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			gen_leader_mock:stop(Am)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"ring stop request with a dead agent",
%		fun() ->
%			{ok, Cook} = gen_server_mock:new(),
%			gen_server_mock:expect_cast(Cook, fun(stop_ringing, _State) -> ok end),
%			{ok, Am} = gen_leader_mock:start(agent_manager),
%			gen_leader_mock:expect_leader_call(Am, fun(_, _, State, _) -> {ok, "doesn't matter", State} end),
%			Agent = spawn(fun() -> ok end),
%			Mons = #monitors{ring_pid = make_ref()},
%			State = Seedstate#state{ring_pid = {"deadagent", Agent}, ringout = true, monitors = Mons},
%			{noreply, Newstate} = handle_info({'$gen_media', stop_ring, Cook}, State),
%			gen_server_mock:assert_expectations(Cook),
%			?assertNot(Newstate#state.ringout),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			Newmons = Newstate#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			gen_leader_mock:stop(Am)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"queue pid goes down",
%		fun() ->
%			Qpid = dead_spawn(),
%			Qref = make_ref(),
%			Mons = #monitors{queue_pid = Qref},
%			State = Seedstate#state{monitors = Mons, queue_pid = {"testqueue", Qpid}},
%			{noreply, Newstate} = handle_info({'DOWN', Qref, process, Qpid, testdeath}, State),
%			?assertEqual(#monitors{}, Newstate#state.monitors),
%			?assertEqual({"testqueue", undefined}, Newstate#state.queue_pid)
%		end}
%	end,
%	fun({#state{callrec = Oldcall} = Seedstate}) ->
%		{"Cook pid goes down with no agent ringing",
%		fun() ->
%			Qpid = dead_spawn(),
%			Cook = dead_spawn(),
%			Qref = make_ref(),
%			CookRef = make_ref(),
%			Mons = #monitors{queue_pid = Qref, cook = CookRef},
%			State = Seedstate#state{queue_pid = {"testqueue", Qpid}, callrec = Oldcall#call{cook = Cook}, monitors = Mons},
%			{noreply, Newstate} = handle_info({'DOWN', CookRef, process, Cook, testdeath}, State),
%			?assertEqual(#monitors{cook = undefined, queue_pid = Qref}, Newstate#state.monitors),
%			?assertEqual({"testqueue", Qpid}, Newstate#state.queue_pid)
%		end}
%	end,
%	fun({#state{callrec = Oldcall} = Seedstate}) ->
%		{"Cook pid goes down with an agent ringing",
%		fun() ->
%			Qpid = dead_spawn(),
%			Cook = dead_spawn(),
%			{ok, Agent} = agent:start(#agent{login = "testagent"}),
%			Qref = make_ref(),
%			CookRef = make_ref(),
%			Aref = make_ref(),
%			Mons = #monitors{queue_pid = Qref, cook = CookRef, ring_pid = Aref},
%			State = Seedstate#state{queue_pid = {"testqueue", Qpid}, ring_pid = {"testagent", Agent}, callrec = Oldcall#call{cook = Cook}, monitors = Mons},
%			{noreply, Newstate} = handle_info({'DOWN', CookRef, process, Cook, testdeath}, State),
%			?assertEqual(#monitors{queue_pid = Qref}, Newstate#state.monitors),
%			?assertEqual({"testqueue", Qpid}, Newstate#state.queue_pid),
%			?assertEqual(undefined, Newstate#state.ring_pid)
%		end}
%	end,
%	fun({#state{callrec = Oldcall} = Seedstate}) ->
%		{"ringing agent dies",
%		fun() ->
%			{ok, Cook} = gen_server_mock:new(),
%			gen_server_mock:expect_cast(Cook, fun(stop_ringing, _State) -> ok end),
%			Agent = dead_spawn(),
%			AgentRef = make_ref(),
%			Qref = make_ref(),
%			Cookref = make_ref(),
%			Mons = #monitors{queue_pid = Qref, cook = Cookref, ring_pid = AgentRef},
%			Call = Oldcall#call{cook = Cook},
%			State = Seedstate#state{monitors = Mons, queue_pid = {"testqueue", dead_spawn()}, ring_pid = {"testagent", Agent}, callrec = Call},
%			{noreply, Newstate} = handle_info({'DOWN', AgentRef, process, Agent, testdeath}, State),
%			gen_server_mock:assert_expectations(Cook),
%			?assertEqual(#monitors{queue_pid = Qref, cook = Cookref}, Newstate#state.monitors),
%			?assertEqual(undefined, Newstate#state.ring_pid)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"oncall agent dies with no ringing agent",
%		fun() ->
%			OncallRef = make_ref(),
%			Agent = dead_spawn(),
%			Mons = #monitors{
%				oncall_pid = OncallRef
%			},
%			{ok, Newqueue} = gen_server_mock:new(),
%			gen_server_mock:expect_call(Newqueue, fun(_Msg, _From, _State) ->
%				ok
%			end),
%			{ok, Mock} = gen_leader_mock:start(queue_manager),
%			gen_leader_mock:expect_leader_call(Mock, fun(_Msg, _From, State, _Elec) ->
%				{ok, Newqueue, State}
%			end),
%			State = Seedstate#state{monitors = Mons, oncall_pid = {"testagent", Agent}},
%			{noreply, #state{monitors = Newmons} = Newstate} = handle_info({'DOWN', OncallRef, process, Agent, testdeath}, State),
%			?assertMatch({"default_queue", _}, Newstate#state.queue_pid),
%			?assertEqual(undefined, Newmons#monitors.oncall_pid),
%			?assertNot(undefined =:= Newmons#monitors.queue_pid),
%			?assertEqual(undefined, Newstate#state.oncall_pid),
%			gen_leader_mock:stop(Mock)
%		end}
%	end,
%	fun({Seedstate}) ->
%		{"oncall agent dies with a ringing agent",
%		fun() ->
%			OncallRef = make_ref(),
%			Ocagent = dead_spawn(),
%			Ringref = make_ref(),
%			{ok, Ragent} = agent:start(#agent{login = "ringagent"}),
%			Mons = #monitors{
%				oncall_pid = OncallRef,
%				ring_pid = Ringref
%			},
%			{ok, Newqueue} = gen_server_mock:new(),
%			gen_server_mock:expect_call(Newqueue, fun(_Msg, _From, _State) ->
%				ok
%			end),
%			gen_server_mock:expect_call(Newqueue, fun(_Msg, _From, _State) ->
%				ok
%			end),
%			{ok, Mock} = gen_leader_mock:start(queue_manager),
%			gen_leader_mock:expect_leader_call(Mock, fun(_Msg, _From, State, _Elec) ->
%				{ok, Newqueue, State}
%			end),
%			State = Seedstate#state{monitors = Mons, oncall_pid = {"deadagent", Ocagent}, ring_pid = {"ringagent", Ragent}},
%			{noreply, #state{monitors = Newmons} = Newstate} = handle_info({'DOWN', OncallRef, process, Ocagent, testdeath}, State),
%			?assertMatch({"default_queue", _}, Newstate#state.queue_pid),
%			?assertEqual(undefined, Newmons#monitors.oncall_pid),
%			?assertNot(undefined =:= Newmons#monitors.queue_pid),
%			?assertEqual(undefined, Newstate#state.oncall_pid),
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			?assertEqual(undefined, Newstate#state.ring_pid),
%			gen_leader_mock:stop(Mock)
%		end}
%	end]}}.
%	
%agent_interact_test_() ->
%	util:start_testnode(),
%	N = util:start_testnode(gen_media_agent_interact_tests),
%	{spawn, N, {foreach,
%	fun() ->
%		Callrec = #call{id = "testcall", source = self(), client = #client{}},
%		{ok, Mock} = gen_leader_mock:start(agent_manager),
%		gen_leader_mock:expect_leader_call(Mock, fun(_Data, _From, State, _Elec) -> {ok, "testagent", State} end),
%		gen_event:start({local, cdr}),
%		gen_event:add_handler(cdr, gen_event_mock, []),
%		{#agent{login = "testagent"}, Callrec}
%	end,
%	fun({_Arec, _Callrec}) ->
%		Mock = whereis(agent_manager),
%		gen_leader_mock:stop(Mock),
%		gen_event:stop(cdr),
%		timer:sleep(10), % because mocks don't like to die quickly.
%		ok
%	end,
%	[%fun({Arec, Callrecbase}) ->
%%		{"mediapush",
%%		fun() ->
%%			Callrec = Callrecbase#call{media_path = inband},
%%			{ok, Apid} = agent:start(Arec#agent{statedata = Callrec, state = oncall}),
%%			State = #state{oncall_pid = Apid, callrec = Callrec},
%%			Expected = State,
%%			?assertEqual(Expected, agent_interact({mediapush, "data", append}, State)),
%%			agent:stop(Apid),
%%			gen_event_mock:assert_expectations(cdr)
%%		end}
%%	end,
%	%fun({Arec, Callrec}) ->
%		%{"media push when media_path doesn't match",
%		%fun() ->
%			%{ok, Apid} = agent:start(Arec#agent{statedata = Callrec, state = oncall}),
%			%State = #state{oncall_pid = Apid, callrec = Callrec},
%			%Expected = State,
%			%agent:stop(Apid),
%			%?assertEqual(Expected, agent_interact({mediapush, "data", append}, State)),
%			%gen_event_mock:assert_expectations(cdr)
%			%ok
%		%end}
%	%end,
%	fun({Arec, Callrec}) ->
%		{"stop_ring with a ringout timer going",
%		fun() ->
%			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Apid} = agent:start(Arec#agent{}),
%			{ok, Tref} = timer:send_interval(1000, <<"timer">>),
%			Mons = #monitors{ring_pid = make_ref()},
%			State = #state{ring_pid = {"testagent", Apid}, ringout = Tref, callrec = Callrec, monitors = Mons},
%			gen_event_mock:expect_event(cdr, fun({ringout, _Callrec, _Time, {undefined, "testagent"}}, _State) ->
%				ok
%			end),
%			Res = agent_interact(stop_ring, State),
%			agent:stop(Apid),
%			receive
%				<<"timer">> ->
%					 erlang:error(timer_lives)
%			after 1500 ->
%				ok
%			end,
%			?assertEqual(false, Res#state.ringout),
%			?assertEqual(undefined, Res#state.ring_pid),
%			Newmons = Res#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({_Arec, Callrec}) ->
%		{"stop_ring with no ringout or ring_pid defined",
%		fun() ->
%			State = #state{ring_pid = undefined, ringout = false, callrec = Callrec},
%			Res = agent_interact(stop_ring, State),
%			?assertEqual(State, Res),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({Arec, Callrec}) ->
%		{"stop_ring with only ring_pid defined",
%		fun() ->
%			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Apid} = agent:start(Arec#agent{}),
%			Mons = #monitors{ring_pid = make_ref()},
%			State = #state{ring_pid = {"testagent", Apid}, ringout = false, callrec = Callrec, monitors = Mons},
%			gen_event_mock:expect_event(cdr, fun({ringout, _Callrec, _Time, {undefined, "testagent"}}, _State) -> ok end),
%			Res = agent_interact(stop_ring, State),
%			agent:stop(Apid),
%			?assertEqual(undefined, Res#state.ring_pid),
%			Newmons = Res#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({_Arec, Callrec}) ->
%		{"stop_ring with only ringout defined",
%		fun() ->
%			{ok, Tref} = timer:send_interval(1000, <<"timer">>),
%			State = #state{ringout = Tref, callrec = Callrec},
%			Res = agent_interact(stop_ring, State),
%			receive
%				<<"timer">>	->
%					 erlang:error(timer_lives)
%			after 1500 ->
%				ok
%			end,
%			?assertEqual(false, Res#state.ringout),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({Arec, Callrec}) ->
%		{"wrapup",
%		fun() ->
%			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Apid} = agent:start(Arec#agent{}),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = #state{oncall_pid = {"testagent", Apid}, callrec = Callrec, monitors = Mons},
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, "testagent"}, _State) -> ok end),
%			Res = agent_interact(wrapup, State),
%			agent:stop(Apid),
%			?assertEqual(undefined, Res#state.oncall_pid),
%			Newmons = Res#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.oncall_pid),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({Arec, Callrec}) ->
%		{"hangup when both oncall and ring are pids",
%		fun() ->
%			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
%			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Oncall} = agent:start(Arec#agent{}),
%			{ok, Ringing} = agent:start(Arec#agent{login = "ringing"}),
%			Mons = #monitors{ring_pid = make_ref(), oncall_pid = make_ref()},
%			State = #state{oncall_pid = {"testagent", Oncall}, ring_pid = {"ring", Ringing}, callrec = Callrec, monitors = Mons},
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, "testagent"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({hangup, _Callrec, _Time, undefined}, _State) -> ok end),
%			Res = agent_interact(hangup, State),
%			agent:stop(Oncall),
%			agent:stop(Ringing),
%			?assertEqual(undefined, Res#state.oncall_pid),
%			?assertEqual(undefined, Res#state.ring_pid),
%			Newmons = Res#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.oncall_pid),
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({Arec, Callrec}) ->
%		{"hang up when only oncall is a pid",
%		fun() ->
%			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Apid} = agent:start(Arec#agent{}),
%			Mons = #monitors{oncall_pid = make_ref()},
%			State = #state{oncall_pid = {"testagent", Apid}, callrec = Callrec, monitors = Mons},
%			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, "testagent"}, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({hangup, _Callrec, _Time, undefined}, _State) -> ok end),
%			Res = agent_interact(hangup, State),
%			agent:stop(Apid),
%			?assertEqual(undefined, Res#state.oncall_pid),
%			Newmons = Res#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.oncall_pid),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({Arec, Callrec}) ->
%		{"hang up when only ringing is a pid",
%		fun() ->
%			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Apid} = agent:start(Arec#agent{}),
%			Mons = #monitors{ring_pid = make_ref()},
%			gen_event_mock:expect_event(cdr, fun({hangup, _Callrec, _Time, undefined}, _State) -> ok end),
%			State = #state{ring_pid = {"testagent", Apid}, callrec = Callrec, monitors = Mons},
%			Res = agent_interact(hangup, State),
%			agent:stop(Apid),
%			?assertEqual(undefined, Res#state.ring_pid),
%			Newmons = Res#state.monitors,
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({_Arec, Callrec}) ->
%		{"hang up when only queue is a pid",
%		fun() ->
%			{ok, Qpid} = gen_server_mock:new(),
%			gen_server_mock:expect_call(Qpid, fun({remove, _Incpid}, _From, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({hangup, _Callrec, _Time, undefined}, _State) -> ok end),
%			Mons = #monitors{queue_pid = make_ref()},
%			State = #state{queue_pid = {"testqueue", Qpid}, callrec = Callrec, monitors = Mons},
%			#state{monitors = Newmons} = Res = agent_interact(hangup, State),
%			?assertEqual("testqueue", Res#state.queue_pid),
%			?assertEqual(undefined, Newmons#monitors.queue_pid),
%			gen_server_mock:assert_expectations(Qpid),
%			gen_server_mock:stop(Qpid),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({Arec, Callrec}) ->
%		{"hang up when both queue and ring is a pid",
%		fun() ->
%			{ok, Qpid} = gen_server_mock:new(),
%			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
%			{ok, Apid} = agent:start(Arec#agent{}),
%			gen_server_mock:expect_call(Qpid, fun({remove, _Incpid}, _From, _State) -> ok end),
%			gen_event_mock:expect_event(cdr, fun({hangup, _Callrec, _Time, undefined}, _State) -> ok end),
%			Mons = #monitors{queue_pid = make_ref(), ring_pid = make_ref()},
%			State = #state{queue_pid = {"testqueue", Qpid}, ring_pid = {"testagent", Apid}, callrec = Callrec, monitors = Mons},
%			#state{monitors = Newmons} = Res = agent_interact(hangup, State),
%			agent:stop(Apid),
%			?assertEqual("testqueue", Res#state.queue_pid),
%			?assertEqual(undefined, Newmons#monitors.queue_pid),
%			?assertEqual(undefined, Res#state.ring_pid),
%			?assertEqual(undefined, Newmons#monitors.ring_pid),
%			gen_server_mock:assert_expectations(Qpid),
%			gen_server_mock:stop(Qpid),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({_Arec, _Callrec}) ->
%		{"orphaned call, or just not yet queued",
%		fun() ->
%			Res = agent_interact(hangup, #state{}),
%			?assertEqual(#state{}, Res),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({_Arec, Callrec}) ->
%		{"dead agent pid doesn't cause crash",
%		fun() ->
%			Mon = #monitors{ring_pid = make_ref()},
%			State = #state{ring_pid = {"testagent", spawn(fun() -> ok end)}, callrec = Callrec, monitors = Mon},
%			gen_event_mock:expect_event(cdr, fun({ringout, _Callrec, _Time, {undefined, "testagent"}}, _) -> ok end),
%			Res = agent_interact(stop_ring, State),
%			?assertEqual(undefined, Res#state.ring_pid),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end]}}.

%outgoing_test_() ->
%	util:start_testnode(),
%	N = util:start_testnode(gen_media_outgoing_tests),
%	{spawn, N, {foreach,
%	fun() ->
%		{ok, Apid} = agent:start(#agent{login = "testagent"}),
%		{ok, Ammock} = gen_leader_mock:start(agent_manager),
%		gen_event:start({local, cdr}),
%		gen_event:add_handler(cdr, gen_event_mock, []),
%		{Apid, Ammock}
%	end,
%	fun({Apid, Ammock}) ->
%		agent:stop(Apid),
%		gen_leader_mock:stop(Ammock),
%		gen_event:stop(cdr),
%		timer:sleep(10)
%	end,
%	[fun({Apid, Ammock}) ->
%		{"set agent outbound with known call, and agent exists",
%		fun() ->
%			gen_leader_mock:expect_leader_call(Ammock, fun({exists, "testagent"}, _From, State, _Elec) ->
%				{ok, {true, Apid}, State}
%			end),
%			gen_event_mock:expect_event(cdr, fun({oncall, _Callrec, _Time, "testagent"}, _State) -> ok end),
%			State = #state{callrec = #call{id = "testcall", source = self()}},
%			{ok, Res} = outgoing({outbound, "testagent", "newsubstate"}, State),
%			?assertEqual({"testagent", Apid}, Res#state.oncall_pid),
%			?assertEqual("newsubstate", Res#state.substate),
%			gen_leader_mock:assert_expectations(Ammock),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({_Apid, Ammock}) ->
%		{"set agent outbound with known call, but agent doesn't exist",
%		fun() ->
%			gen_leader_mock:expect_leader_call(Ammock, fun({exists, "testagent"}, _From, State, _Elec) ->
%				{ok, false, State}
%			end),
%			State = #state{callrec = #call{id = "testcall", source = self()}},
%			Res = outgoing({outbound, "testagent", "newsubstate"}, State),
%			?assertMatch({{error, {noagent, "testagent"}}, _Newstate}, Res),
%			{_, Newstate} = Res,
%			?assertEqual("newsubstate", Newstate#state.substate),
%			gen_leader_mock:assert_expectations(Ammock),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({Apid, Ammock}) ->
%		{"set agent outbound with a new callrec",
%		fun() ->
%			gen_leader_mock:expect_leader_call(Ammock, fun({exists, "testagent"}, _From, State, _Elec) ->
%				{ok, {true, Apid}, State}
%			end),
%			gen_event_mock:expect_event(cdr, fun({oncall, _Callrec, _Time, "testagent"}, _State) -> ok end),
%			Callrec = #call{id = "testcall", source = self()},
%			State = #state{},
%			{ok, Res} = outgoing({outbound, "testagent", Callrec, "newsubstate"}, State),
%			?assertEqual({"testagent", Apid}, Res#state.oncall_pid),
%			?assertEqual(Callrec, Res#state.callrec),
%			?assertEqual("newsubstate", Res#state.substate),
%			gen_leader_mock:assert_expectations(Ammock),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end,
%	fun({_Apid, Ammock}) ->
%		{"set agent outbound iwth a new call rec, but agent doesn't exist",
%		fun() ->
%			gen_leader_mock:expect_leader_call(Ammock, fun({exists, "testagent"}, _From, State, _Elec) ->
%				{ok, false, State}
%			end),
%			Callrec = #call{id = "testcall", source = self()},
%			State = #state{},
%			Res = outgoing({outbound, "testagent", Callrec, "newsubstate"}, State),
%			?assertMatch({{error, {noagent, "testagent"}}, _State}, Res),
%			{_, Newstate} = Res,
%			?assertEqual("newsubstate", Newstate#state.substate),
%			?assertEqual(Callrec, Newstate#state.callrec),
%			gen_leader_mock:assert_expectations(Ammock),
%			gen_event_mock:assert_expectations(cdr)
%		end}
%	end]}}.

dpid() -> spawn(fun() -> ok end).

priv_queue_test_() ->
	{setup, fun() ->
		meck:new(queue_manager),
		meck:new(call_queue),
		Callrec = #call{id = "testcall", source = self()},
		Validator = fun() ->
			?assert(meck:validate(queue_manager)),
			?assert(meck:validate(call_queue))
		end,
		{Callrec, Validator}
	end,
	fun(_) ->
		meck:unload(queue_manager),
		meck:unload(call_queue)
	end,
	fun({Callrec, Validator}) -> [

		{"All is well", fun() ->
			Qpid = dpid(),
			meck:expect(queue_manager, get_queue, fun(_) -> Qpid end),
			meck:expect(call_queue, add, fun(_, _, _) -> ok end),
			?assertEqual(Qpid, priv_queue("testqueue", Callrec, "doesn't matter")),
			Validator()
		end},

		{"failover is false", fun() ->
			meck:expect(queue_manager, get_queue, fun(_) -> undefined end),
			?assertEqual(invalid, priv_queue("testqueue", Callrec, false)),
			Validator()
		end},

		{"failover is true", fun() ->
			Qpid = dpid(),
			meck:expect(queue_manager, get_queue, fun(QuNom) ->
				case QuNom of
					"testqueue" -> undefined;
					"default_queue" -> Qpid
				end
			end),
			meck:expect(call_queue, add, fun(_, _, _) -> ok end),
			?assertEqual({default, Qpid}, priv_queue("testqueue", Callrec, true)),
			Validator()
		end}

	] end}.

mutate_return_test_() ->
	{setup, fun() ->
		meck:new(first_module),
		meck:new(second_module),
		#base_state{callback = first_module}
	end,
	fun(_) ->
		meck:unload(first_module),
		meck:unload(second_module)
	end,
	fun(Base) -> [

		{"mutate from handle info", fun() ->
			meck:expect(first_module, handle_info, fun(mutate, "gm state", undefined, _GMStateRec, undefined) ->
				{mutate, second_module, "new callback state"}
			end),
			{next_state, "gm state", {NewBase, "gm state rec"}} = handle_info(mutate, "gm state", {Base, "gm state rec"}),
			?assertEqual(second_module, NewBase#base_state.callback),
			?assertEqual("new callback state", NewBase#base_state.substate),
			meck:validate(first_module),
			meck:validate(second_module)
		end},

		{"mutate from call", fun() ->
			meck:expect(first_module, handle_call, fun(mutate, "from", "gm statename", undefined, "gm state data", undefined) ->
				{mutate, {ok, "goober"}, second_module, "new callback state"}
			end),
			{reply, {ok, "goober"}, "gm statename", {NewBase, "gm state data"}} = handle_sync_event(mutate, "from", "gm statename", {Base, "gm state data"}),
			?assertEqual(second_module, NewBase#base_state.callback),
			?assertEqual("new callback state", NewBase#base_state.substate)
		end}

	] end}.

outbound_call_flow_test_() ->
	{setup, fun() ->
		meck:new(media_callback),
		meck:new(agent_channel),
		meck:new(cdr),
		meck:expect(media_callback, init, fun(_) ->
			{ok, {undefined, undefined}}
		end),
		{ok, InitState, GmState} = init([media_callback, undefined]),
		?DEBUG("initstate:  ~p, ~p", [InitState, GmState]),
		Validator = fun() ->
			meck:validate(media_callback),
			meck:validate(agent_channel),
			meck:validate(cdr)
		end,
		{GmState, Validator}
	end,
	fun(_) ->
		meck:unload(media_callback),
		meck:unload(agent_channel),
		meck:unload(cdr)
	end,
	fun({GmState, Validator}) -> [

		{"callback is a success (handle_info)", fun() ->
			Call = #call{source = dpid(), id = "testcall"},
			meck:expect(media_callback, handle_info, fun(doit, _, _, _, _) ->
				{outgoing, {"agent", dpid()}, Call, undefined}
			end),
			meck:expect(agent_channel, set_state, fun(_, _, _) -> ok end),
			meck:expect(cdr, oncall, fun(_, _) -> ok end),
			Out = handle_info(doit, inivr, GmState),
			?assertMatch({next_state, oncall, _Whatever}, Out),
			Validator()
		end},

		{"callback is a success (handle_call)", fun() ->
			Call = #call{source = dpid(), id = "testcall"},
			meck:expect(media_callback, handle_call, fun(doit, _, _, _, _, _) ->
				{outgoing, {"agent", dpid()}, Call, undefined}
			end),
			meck:expect(agent_channel, set_state, fun(_, _, _) -> ok end),
			meck:expect(cdr, oncall, fun(_, _) -> ok end),
			Out = handle_sync_event(doit, {dpid(), "from"}, inivr, GmState),
			?assertMatch({reply, ok, oncall, _Whatever}, Out),
			Validator()
		end}

	] end}.

-endif.
