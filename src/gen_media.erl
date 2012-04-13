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

%% @doc Behaviour module for media types.  Gen_media uses gen_server as 
%% the underlying framework for what it does.  It has a few specific call, 
%% cast, and info callbacks it implements, everything else it passes to 
%% it's callback module to process.  Replies from handle_call, handle_cast,
%% and handle_info are extended to allow for specific events only media 
%% would need.
%%
%%	Callback functions:
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
%%	<b>handle_ring(Agent, Call, State) -> Result</b>
%%		types:	Agent = pid()
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
%%	<b>handle_ring_stop(Call, State) -> Result</b>
%%		types:	Call = #call{}
%%				State = any()
%%				Result = {ok, NewState}
%%
%%		When an agent should no longer be ringing, such as due to ringout, 
%%		this function is called.
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
%%	<b>handle_voicemail(Ringing, Call, State) -> Result</b>
%%		types:	Ringing = undefined | pid()
%%				Call = #call{}
%%				State = any()
%%				Result = {ok, NewState} | {invalid, NewState}
%%
%%		When a media should be removed from queue and moved to voicemail, 
%%		this is called.
%%
%%		State is the internal state of the gen_media callbacks.
%%
%%		Ringing is the pid of the agent ringing with the media, or 
%%		undefined if there is no agent.
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
%%		When a recipe calls for a call in queue to play an announcement, 
%%		this function is called.  Execution then continues with NewState.
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
%%		When a media should be transfered to another agent, this is one of 
%%		the first step.  The target agent is set to ringing, then this 
%%		callback is used to verify that.  If the callback returns 
%%		{ok, NewState}, execution continues with NewState, and gen_media 
%%		handles with oncall or a ringout.
%%
%%	<b>handle_queue_transfer(Call, State) -> {ok, NewState}</b>
%%		types:	Call = #call{}
%%				State = NewState = any()
%%
%%		When a media is placed back into queue from an agent, this is 
%%		called to allow the media to do any required clean up or 
%%		unbridging.  The Call is requeued at the priority it was initially 
%%		queued at.  Execution then continues with NewState.
%%
%%	<b>handle_wrapup(Call, State) -> {Finality, NewState}</b>
%%		types:	Call = #call{}
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
%%		If {ok, NewState} is returned, execution continues with state NewState.
%%
%%	<b>handle_spy(Spy, Call, State) -> {ok, NewState} | {invalid, NewState} | {error, Error, NewState}</b>
%%		types:  Call = #call{}
%%				State = NewState = any()
%%				Spy = pid()
%%
%%		This callback is only valid when the Spy is released and there is 
%%		an agent oncall with the call.  This signals the callback that a 
%%		supervisor is attempting to observe the agent that is oncall.  The 
%%		other callbacks	Should take into account the possibility of a spy 
%%		if 'ok' is returned.
%%		
%%		Be aware that when calling this, gen_media does not have a 
%%		reliable method to determine an agent's security level.  The agent 
%%		connecitons, however, do.
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
	 terminate/2, code_change/3, format_status/2]).

%% gen_media api
-export([
	ring/4,
	get_call/1,
	voicemail/1,
	announce/2,
	%% TODO added for testing only (implemented with focus on real Calls - no other media)
	end_call/1,
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

-record(monitors, {
	queue_pid :: 'undefined' | reference(),
	ring_pid :: 'undefined' | reference(),
	oncall_pid :: 'undefined' | reference(),
	cook :: 'undefined' | reference()
}).

-record(state, {
	callback :: atom(),
	substate :: any(),
	callrec :: #call{},
	ring_pid :: 'undefined' | {string(), pid()},
	oncall_pid :: 'undefined' | {string(), pid()},
	queue_failover = true :: 'true' | 'false',
	queue_pid :: 'undefined' | string() | {string(), pid()},
	ringout = false:: tref() | 'false',
	outband_ring_pid :: 'undefined' | pid(),
	warm_transfer = false :: boolean(),
	monitors = #monitors{} :: #monitors{},
	url_pop_getvars = [] :: [{string(), string()}]
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

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
-spec(ring/4 :: (Genmedia :: pid(), Agent :: pid() | string() | {string(), pid()}, Qcall :: #queued_call{}, Timeout :: pos_integer())  -> 'ok' | 'invalid' | 'deferred').
ring(Genmedia, {_Agent, Apid} = A, Qcall, Timeout) when is_pid(Apid) ->
	gen_server:call(Genmedia, {'$gen_media_ring', A, Qcall, Timeout}, infinity);
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

%% @doc Get the call record associated with `pid() Genmedia'.
-spec(get_call/1 :: (Genmedia :: pid()) -> #call{}).
get_call(Genmedia) ->
	gen_server:call(Genmedia, '$gen_media_get_call', infinity).

%% @doc Send the passed `pid() Genmedia' to voicemail.
-spec(voicemail/1 :: (Genmedia :: pid()) -> 'ok' | 'invalid').
voicemail(Genmedia) ->
	gen_server:call(Genmedia, '$gen_media_voicemail').

%% @doc Pass `any() Annouce' message to `pid() Genmedia'.
-spec(announce/2 :: (Genmedia :: pid(), Annouce :: any()) -> 'ok').
announce(Genmedia, Annouce) ->
	gen_server:call(Genmedia, {'$gen_media_announce', Annouce}).

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
	gen_server:call(Genmedia, '$gen_media_agent_oncall', infinity).

%% @doc Transfer the call from the agent it is associated with to a new agent.
-spec(agent_transfer/3 :: (Genmedia :: pid(), Apid :: pid() | string() | {string(), pid()}, Timeout :: pos_integer()) -> 'ok' | 'invalid').
agent_transfer(Genmedia, {_Login, Apid} = Agent, Timeout) when is_pid(Apid) ->
	gen_server:call(Genmedia, {'$gen_media_agent_transfer', Agent, Timeout});
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
	
%% @doc Attempt to spy on the agent oncall with the given media.  `Spy' is
%% the pid to send media events/load data to, and `AgentRec' is an 
%% `#agent{}' used to hold the end point data.
-spec(spy/3 :: (Genmedia :: pid(), Spy :: pid(), AgentRec :: #agent{}) -> 'ok' | 'invalid' | {'error', any()}).
spy(Genmedia, Spy, AgentRec) ->
	gen_server:call(Genmedia, {'$gen_media_spy', Spy, AgentRec}).

-spec(set_cook/2 :: (Genmedia :: pid(), CookPid :: pid()) -> 'ok').
set_cook(Genmedia, CookPid) ->
	gen_server:cast(Genmedia, {'$gen_media_set_cook', CookPid}).

-spec(set_queue/2 :: (Genmedia :: pid(), Qpid :: pid()) -> 'ok').
set_queue(Genmedia, Qpid) ->
	gen_server:call(Genmedia, {'$gen_media_set_queue', Qpid}).

-spec(set_url_getvars/2 :: (Genmedia :: pid(), Vars :: [{string(), string()}]) -> 'ok').
set_url_getvars(Genmedia, Vars) ->
	gen_server:cast(Genmedia, {'$gen_media_set_url_getvars', Vars}).

-spec(get_url_getvars/1 :: (Genmedia :: pid()) -> {'ok', [{string(), string()}]}).
get_url_getvars(Genmedia) ->
	gen_server:call(Genmedia, '$gen_media_get_url_vars').

-spec(add_skills/2 :: (Genmedia :: pid(), Skills :: [atom() | {atom(), any()}]) -> 'ok').
add_skills(Genmedia, Skills) ->
	gen_server:cast(Genmedia, {'$gen_media_add_skills', Skills}).
	
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
			cpx_monitor:set({media, Callrec#call.id}, [], self()),
			{Qnom, Qpid} = case priv_queue(Queue, Callrec, true) of
				invalid when Queue =/= "default_queue" ->
					% this clause, if hit, will cause a (justifiable) crash
					% TODO wtf?
					priv_queue("default_queue", Callrec, true),
					set_cpx_mon(#state{callrec = Callrec}, [{queue, "default_queue"}]),
					cdr:inqueue(Callrec, "default_queue");
				{default, Pid} ->
					set_cpx_mon(#state{callrec = Callrec}, [{queue, "default_queue"}]),
					cdr:inqueue(Callrec, "default_queue"),
					{"default_queue", Pid};
				Else ->
					cdr:inqueue(Callrec, Queue),
					set_cpx_mon(#state{callrec = Callrec}, [{queue, Queue}]),
					{Queue, Else}
			end,
			Mons = #monitors{queue_pid = erlang:monitor(process, Qpid)},
			{ok, #state{callback = Callback, substate = Substate, callrec = Callrec#call{source = self()}, queue_pid = {Qnom, Qpid}, monitors = Mons}};
		{ok, {Substate, PCallrec, {CDRState, CDRArgs}}} when is_record(PCallrec, call) ->
			Callrec = correct_client(PCallrec),
			cdr:cdrinit(Callrec),
			apply(cdr, CDRState, [Callrec | CDRArgs]),
			set_cpx_mon(#state{callrec = Callrec}, [], self()),
			{ok, #state{callback = Callback, substate = Substate, callrec = Callrec#call{source = self()}}};
		{ok, {Substate, PCallrec}} when is_record(PCallrec, call) ->
			Callrec = correct_client(PCallrec),
			cdr:cdrinit(Callrec),
			set_cpx_mon(#state{callrec = Callrec}, [], self()),
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
handle_call({'$gen_media_spy', Spy, _AgentRec}, _From, #state{oncall_pid = {_Nom, Spy}} = State) ->
	%% Can't spy on yourself.
	?DEBUG("Can't spy on yourself", []),
	{reply, invalid, State};
handle_call({'$gen_media_spy', Spy, AgentRec}, _From, #state{callback = Callback, oncall_pid = {_Agent, Ocpid}, callrec = Call} = State) when is_pid(Ocpid) ->
	case erlang:function_exported(Callback, handle_spy, 3) of
		false ->
			?DEBUG("Callback ~p doesn't support spy for ~p", [Callback, Call#call.id]),
			{reply, invalid, State};
		true ->
			case Callback:handle_spy({Spy, AgentRec}, Call, State#state.substate) of
				{ok, Newstate} ->
					{reply, ok, State#state{substate = Newstate}};
				{invalid, Newstate} ->
					{reply, invalid, State#state{substate = Newstate}};
				{error, Error, Newstate} ->
					?INFO("Callback ~p errored ~p on spy for ~p", [Callback, Error, Call#call.id]),
					{reply, {error, Error}, State#state{substate = Newstate}}
			end
	end;
handle_call({'$gen_media_spy', _Spy, _AgentRec}, _From, State) ->
	{reply, invalid, State};
handle_call('$gen_media_wrapup', {Ocpid, _Tag}, #state{callback = Callback, oncall_pid = {Ocagent, Ocpid}, callrec = Call, monitors = Mons} = State) when Call#call.media_path =:= inband ->
	?INFO("Request to end call ~p from agent", [Call#call.id]),
	cdr:wrapup(State#state.callrec, Ocagent),
	case Callback:handle_wrapup(State#state.callrec, State#state.substate) of
		{ok, NewState} ->
			erlang:demonitor(Mons#monitors.oncall_pid),
			Newmons = Mons#monitors{oncall_pid = undefined},
			{reply, ok, State#state{oncall_pid = undefined, substate = NewState, monitors = Newmons}};
		{hangup, NewState} ->
			cdr:hangup(State#state.callrec, "agent"),
			erlang:demonitor(Mons#monitors.oncall_pid),
			Newmons = Mons#monitors{oncall_pid = undefined},
			{stop, normal, ok, State#state{oncall_pid = undefined, substate = NewState, monitors = Newmons}}
	end;
handle_call('$gen_media_wrapup', {Ocpid, _Tag}, #state{oncall_pid = {_Agent, Ocpid}, callrec = Call} = State) ->
	?ERROR("Cannot do a wrapup directly unless mediapath is inband, and request is from agent oncall. ~p", [Call#call.id]),
	{reply, invalid, State};
handle_call({'$gen_media_queue', Queue}, {Ocpid, _Tag}, #state{callback = Callback, callrec = Call, oncall_pid = {Ocagent, Ocpid}, monitors = Mons} = State) ->
	?INFO("request to queue call ~p from agent", [Call#call.id]),
	% Decrement the call's priority by 5 when requeueing
	case priv_queue(Queue, reprioritize_for_requeue(Call), State#state.queue_failover) of
		invalid ->
			{reply, invalid, State};
		{default, Qpid} ->
			{ok, NewState} = Callback:handle_queue_transfer(State#state.callrec, State#state.substate),
			cdr:queue_transfer(State#state.callrec, "default_queue"),
			cdr:inqueue(State#state.callrec, "default_queue"),
			cdr:wrapup(State#state.callrec, Ocagent),
			set_cpx_mon(State#state{substate = NewState, oncall_pid = undefined}, [{queue, "default_queue"}]),
			erlang:demonitor(Mons#monitors.oncall_pid),
			Newmons = Mons#monitors{queue_pid = erlang:monitor(process, Qpid), oncall_pid = undefined},
			{reply, ok, State#state{substate = NewState, oncall_pid = undefined, queue_pid = {"default_queue", Qpid}, monitors = Newmons}};
		Qpid when is_pid(Qpid) ->
			{ok, NewState} = Callback:handle_queue_transfer(State#state.callrec, State#state.substate),
			cdr:queue_transfer(State#state.callrec, Queue),
			cdr:inqueue(State#state.callrec, Queue),
			cdr:wrapup(State#state.callrec, Ocagent),
			erlang:demonitor(Mons#monitors.oncall_pid),
			Newmons = Mons#monitors{queue_pid = erlang:monitor(process, Qpid), oncall_pid = undefined},
			set_cpx_mon(State#state{substate = NewState, oncall_pid = undefined}, [{queue, Queue}]),
			{reply, ok, State#state{substate = NewState, oncall_pid = undefined, queue_pid = {Queue, Qpid}, monitors = Newmons}}
	end;
handle_call({'$gen_media_queue', Queue}, From, #state{callback = Callback, callrec = Call, oncall_pid = {Ocagent, Apid}, monitors = Mons} = State) when is_pid(Apid) ->
	?INFO("Request to queue ~p from ~p", [Call#call.id, From]),
	% Decrement the call's priority by 5 when requeueing
	case priv_queue(Queue, reprioritize_for_requeue(Call), State#state.queue_failover) of
		invalid ->
			{reply, invalid, State};
		{default, Qpid} ->
			set_agent_state(Apid, [wrapup, State#state.callrec]),
			{ok, NewState} = Callback:handle_queue_transfer(State#state.callrec, State#state.substate),
			cdr:queue_transfer(State#state.callrec, "default_queue"),
			cdr:inqueue(State#state.callrec, "default_queue"),
			cdr:wrapup(State#state.callrec, Ocagent),
			erlang:demonitor(Mons#monitors.oncall_pid),
			Newmons = Mons#monitors{queue_pid = erlang:monitor(process, Qpid), oncall_pid = undefined},
			set_cpx_mon(State#state{substate = NewState, oncall_pid = undefined}, [{queue, "default_queue"}]),
			{reply, ok, State#state{substate = NewState, oncall_pid = undefined, queue_pid = {"default_queue", Qpid}, monitors = Newmons}};
		Qpid when is_pid(Qpid) ->
			set_agent_state(Apid, [wrapup, State#state.callrec]),
			{ok, NewState} = Callback:handle_queue_transfer(State#state.callrec, State#state.substate),
			cdr:queue_transfer(State#state.callrec, Queue),
			cdr:inqueue(State#state.callrec, Queue),
			cdr:wrapup(State#state.callrec, Ocagent),
			erlang:demonitor(Mons#monitors.oncall_pid),
			Newmons = Mons#monitors{queue_pid = erlang:monitor(process, Qpid), oncall_pid = undefined},
			set_cpx_mon(State#state{substate = NewState, oncall_pid = undefined}, [{queue, Queue}]),
			{reply, ok, State#state{substate = NewState, oncall_pid = undefined, queue_pid = {Queue, Qpid}, monitors = Newmons}}
	end;
handle_call('$gen_media_get_call', _From, State) ->
	{reply, State#state.callrec, State};
handle_call({'$gen_media_ring', {Agent, Apid}, #queued_call{cook = Requester} = QCall, Timeout}, {Requester, _Tag}, #state{callrec = CachedCall, callback = Callback, ring_pid = undefined, monitors = Mons, url_pop_getvars = GenPopopts} = State) ->
	?INFO("Trying to ring ~p with ~p with timeout ~p", [Agent, CachedCall#call.id, Timeout]),
	case set_agent_state(Apid, [ringing, CachedCall#call{cook=QCall#queued_call.cook}]) of
		ok ->
			% TODO update callbacks to accept {string(), pid()} structure.
			?INFO("gen_media calling Callback:handle_ring with State#state.substate: ~p", [State#state.substate]),
			case Callback:handle_ring(Apid, State#state.callrec, State#state.substate) of
				Success when element(1, Success) == ok ->
					{Popopts, Call} = case Success of
						{ok, Substate} ->
							{GenPopopts, CachedCall};
						{ok, RetCall, Substate} when is_record(RetCall, call) ->
							{GenPopopts, RetCall};
						{ok, Opts, Substate} ->
							{lists:ukeymerge(1, lists:ukeysort(1, GenPopopts), lists:ukeysort(1, Opts)), CachedCall};
						{ok, Opts, RetCall, Substate} ->
							{lists:ukeymerge(1, lists:ukeysort(1, GenPopopts), lists:ukeysort(1, Opts)), RetCall}
					end,
					cdr:ringing(Call, Agent),
					url_pop(Call, Apid, Popopts),
					Newcall = Call#call{cook = QCall#queued_call.cook},
%					Self = self(),
%					spawn(fun() -> % do this in a subprocess so we don't block
%						Arec = agent:dump_state(Apid),
%						case {Arec#agent.defaultringpath, CachedCall#call.ring_path, whereis(freeswitch_media_manager)} of
%							{outband, inband, Pid} when is_pid(Pid) ->
%								case freeswitch_media_manager:ring_agent(Apid, Arec, Call, Timeout) of
%									{ok, RingChanPid} ->
%										gen_media:cast(Self, {'$gen_media_set_outband_ring_pid', RingChanPid});
%									Else ->
%										?WARNING("Failed to do out of band ring:  ~p for ~p", [Else, Call#call.id]),
%										undefined
%								end;
%							_ ->
%								undefined
%						end
%					end),
					{ok, Tref} = case Newcall#call.ring_path of
						outband ->
							{ok, undefined};
						inband ->
							timer:send_after(Timeout, {'$gen_media_stop_ring', QCall#queued_call.cook})
					end,
					Self = self(),
					spawn(fun() -> % do this in a subprocess so we don't block
						Arec = agent:dump_state(Apid),
						case {Arec#agent.defaultringpath, Call#call.ring_path, whereis(freeswitch_media_manager)} of
							{outband, inband, Pid} when is_pid(Pid) ->
								case freeswitch_media_manager:ring_agent(Apid, Arec, Call, Timeout) of
									{ok, RingChanPid} ->
										gen_media:cast(Self, {'$gen_media_set_outband_ring_pid', RingChanPid}),
										agent:has_successful_ring(Apid);
									Else ->
										?WARNING("Failed to do out of band ring:  ~p for ~p", [Else, Call#call.id]),
										agent:has_failed_ring(Apid),
										undefined
								end;
							_ ->
								agent:has_failed_ring(Apid),
								undefined
						end
					end),
					Newmons = Mons#monitors{ ring_pid = erlang:monitor(process, Apid)},
					{reply, ok, State#state{substate = Substate, ring_pid = {Agent, Apid}, ringout=Tref, callrec = Newcall, monitors = Newmons}};
				{invalid, Substate} ->
					agent:has_failed_ring(Apid),
					%set_agent_state(Apid, [released, {"Ring Fail", "Ring Fail", -1}]),
					{reply, invalid, State#state{substate = Substate}}
			end;
		Else ->
			?INFO("Agent ~p ringing response:  ~p for ~p", [Agent, Else, CachedCall#call.id]),
			{reply, invalid, State}
	end;
handle_call({'$gen_media_ring', {_Agent, Apid}, QCall, _Timeout}, _From, #state{callrec = _Call, callback = _Callback, ring_pid = undefined} = State) ->
	gen_server:cast(QCall#queued_call.cook, {ring_to, Apid, QCall}),
	{reply, deferred, State};
handle_call({'$gen_media_ring', {Agent, Apid}, QCall, Timeout}, From, #state{callrec = Call, callback = _Callback, ring_pid = {RAgent, _Rpid}} = State) ->
	?NOTICE("Changing ring from ~p to ~p for ~p", [RAgent, Agent, Call#call.id]),
	Cook = QCall#queued_call.cook,
	{noreply, Midstate} = handle_info({'$gen_media_stop_ring', Cook}, State),
	handle_call({'$gen_media_ring', {Agent, Apid}, QCall, Timeout}, From, Midstate);

	
handle_call({'$gen_media_agent_transfer', {Agent, Apid}}, _From, #state{oncall_pid = {Agent, Apid}, callrec = Call} = State) ->
	?NOTICE("Can't transfer to yourself, silly ~p! ~p", [Apid, Call#call.id]),
	{reply, invalid, State};
handle_call({'$gen_media_agent_transfer', {Agent, Apid}, Timeout}, _From, #state{callrec = Call, callback = Callback, ring_pid = undefined, oncall_pid = {OcAgent, Ocpid}, monitors = Mons, url_pop_getvars = GenPopopts} = State) when is_pid(Ocpid) ->
	case set_agent_state(Apid, [ringing, State#state.callrec]) of
		ok ->
			case Callback:handle_agent_transfer(Apid, Timeout, State#state.callrec, State#state.substate) of
				Success when element(1, Success) == ok ->
					Popopts = case Success of
						{ok, Substate} ->
							[];
						{ok, Opts, Substate} ->
							lists:ukeymerge(1, lists:ukeysort(1, GenPopopts), lists:ukeysort(1, Opts))
					end,
					% TODO - this is a little ambigious - the pattern match for stop_ring doesn't check for dummy or anything
					{ok, Tref} = timer:send_after(Timeout, {'$gen_media_stop_ring', dummy}),
					cdr:agent_transfer(State#state.callrec, {OcAgent, Agent}),
					cdr:ringing(State#state.callrec, Agent),
					url_pop(Call, Apid, Popopts),
					Newmons = Mons#monitors{ring_pid = erlang:monitor(process, Apid)},
					{reply, ok, State#state{ring_pid = {Agent, Apid}, ringout = Tref, substate = Substate, monitors = Newmons}};
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
handle_call({'$gen_media_warm_transfer_begin', Number}, _From, #state{callback = Callback, oncall_pid = {Agent, Apid}, callrec = Call} = State) when is_pid(Apid) ->
	case erlang:function_exported(Callback, handle_warm_transfer_begin, 3) of
		true ->
			case Callback:handle_warm_transfer_begin(Number, Call, State#state.substate) of
				{ok, UUID, NewState} ->
					Res = set_agent_state(Apid, [warmtransfer, UUID]),
					cdr:warmxfer_begin(State#state.callrec, {Agent, Number}),
					{reply, Res, State#state{substate = NewState, warm_transfer = true}};
				{error, Error, NewState} ->
					?DEBUG("Callback module ~w errored for warm transfer begin:  ~p for ~p", [Callback, Error, Call#call.id]),
					{reply, invalid, State#state{substate = NewState}}
			end;
		false ->
			{reply, invalid, State}
	end;
handle_call('$gen_media_warm_transfer_cancel', _From, #state{callback = Callback, oncall_pid = {Agent, Apid}, callrec = Call} = State) when is_pid(Apid) ->
	case erlang:function_exported(Callback, handle_warm_transfer_cancel, 2) of
		true ->
			case Callback:handle_warm_transfer_cancel(Call, State#state.substate) of
				{ok, NewState} ->
					Res = set_agent_state(Apid, [oncall, Call]),
					cdr:warmxfer_cancel(Call, Agent),
					cdr:oncall(Call, Agent),
					{reply, Res, State#state{substate = NewState, warm_transfer = false}};
				{error, Error, NewState} ->
					?DEBUG("Callback module ~w errored for warm transfer cancel:  ~p for ~p", [Callback, Error, Call#call.id]),
					{reply, invalid, State#state{substate = NewState}}
			end;
		false ->
			{reply, invalid, State}
	end;
handle_call('$gen_media_warm_transfer_complete', _From, #state{callback = Callback, oncall_pid = {Agent, Apid}, callrec = Call, monitors = Mons} = State) when is_pid(Apid) ->
	case erlang:function_exported(Callback, handle_warm_transfer_complete, 2) of
		true ->
			case Callback:handle_warm_transfer_complete(Call, State#state.substate) of
				{ok, NewState} ->
					Res = set_agent_state(Apid, [wrapup, Call]),
					cdr:warmxfer_complete(Call, Agent),
					cdr:wrapup(Call, Agent),
					erlang:demonitor(Mons#monitors.oncall_pid),
					Newmons = Mons#monitors{oncall_pid = undefined},
					{reply, Res, State#state{substate = NewState, oncall_pid = undefined, monitors = Newmons}};
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
handle_call('$gen_media_end_call', _, #state{queue_pid = undefined} = State) ->
	{reply, invalid, State};
handle_call('$gen_media_end_call', {Cook, _}, #state{callback = Callback, substate = InSubstate, callrec = #call{cook = Cook} = Call} = State) ->
	case erlang:function_exported(Callback, handle_end_call, 2) of
		true ->
			case Callback:handle_end_call(Call, InSubstate) of
				{ok, Substate} ->
					% stop agent ringing, kill self
					?INFO("Ending Call for ~p", [Call#call.id]),
					NewState = State#state{substate = Substate},
					Out = handle_stop({hangup, queue}, NewState),
					{stop, Out, ok, NewState};
				{deferred, Substate} ->
					?INFO("Ending Call deferred for ~p", [Call#call.id]),
					% up to the media to kill self.
					{reply, ok, State#state{substate = Substate}};
				{error, Err, Substate} ->
					?INFO("Ending Call for ~p errored:  ~p", [Call#call.id, Err]),
					{reply, invalid, State#state{substate = Substate}}
			end;
		false ->
			{reply, invalid, State}
	end;
handle_call('$gen_media_voicemail', _From, #state{queue_pid = undefined, callrec = Call} = State) ->
	?ERROR("voicemail only valid when the media ~p is queued", [Call#call.id]),
	{reply, invalid, State};
handle_call('$gen_media_voicemail', _From, #state{callback = Callback, callrec = Call, queue_pid = {Queue, Qpid}} = State) when is_pid(Qpid) ->
	?INFO("trying to send media ~p to voicemail", [Call#call.id]),
	case erlang:function_exported(Callback, handle_voicemail, 3) of
		false ->
			{reply, invalid, State};
		true ->
			case  Callback:handle_voicemail(State#state.ring_pid, Call, State#state.substate) of
				{ok, Substate} ->
					priv_voicemail(State),
					{reply, ok, State#state{substate = Substate, queue_pid = Queue, ring_pid = undefined, monitors = #monitors{}}};
				{invalid, Substate} ->
					{reply, invalid, State#state{substate = Substate}}
			end
	end;
handle_call('$gen_media_agent_oncall', {Apid, _Tag}, #state{ring_pid = {_Agent, Apid}, callrec = #call{ring_path = outband} = Call} = State) ->
	?INFO("Cannot accept on call requests from agent (~p) unless ring_path is inband for ~p", [Apid, Call#call.id]),
	{reply, invalid, State};
handle_call('$gen_media_agent_oncall', {Rpid, _Tag}, #state{ring_pid = {Ragent, Rpid}, callback = Callback, oncall_pid = {OcAgent, Ocpid}, callrec = #call{ring_path = inband} = Call, monitors = Mons} = State) when is_pid(Ocpid) ->
	?INFO("oncall request during what looks like an agent transfer (inband) for ~p", [Call#call.id]),
	case Callback:handle_answer(Rpid, Call, State#state.substate) of
		{ok, NewState} ->
			kill_outband_ring(State),
			cdr:oncall(Call, Ragent),
			timer:cancel(State#state.ringout),
			set_agent_state(Ocpid, [wrapup, Call]),
			cdr:wrapup(Call, OcAgent),
			erlang:demonitor(Mons#monitors.oncall_pid),
			Newmons = Mons#monitors{ring_pid = undefined, oncall_pid = Mons#monitors.ring_pid},
			set_cpx_mon(State#state{substate = NewState, ringout = false, oncall_pid = {Ragent, Rpid}, ring_pid = undefined}, [{agent, Ragent}]),
			{reply, ok, State#state{substate = NewState, ringout = false, oncall_pid = {Ragent, Rpid}, ring_pid = undefined, outband_ring_pid = undefined, monitors = Newmons}};
		{error, Reason, NewState} ->
			?ERROR("Cannot set ~p for ~p to oncall due to ~p", [Rpid, Call#call.id, Reason]),
			{reply, invalid, State#state{substate = NewState}}
	end;
handle_call('$gen_media_agent_oncall', _From, #state{warm_transfer = true, callrec = Call, oncall_pid = undefined} = State) ->
	?INFO("stray oncall request during what looks like a warm transfer complete (outofband) for ~p", [Call#call.id]),
	{reply, ok, State};
handle_call('$gen_media_agent_oncall', _From, #state{warm_transfer = true, callrec = Call} = State) ->
	?INFO("oncall request during what looks like a warm transfer (outofband) for ~p", [Call#call.id]),
	agent:media_push(element(2, State#state.oncall_pid), warm_transfer_succeeded),
	{reply, ok, State};
handle_call('$gen_media_agent_oncall', _From, #state{ring_pid = {Ragent, Rpid}, callback = Callback, oncall_pid = {OcAgent, Ocpid}, callrec = Call, monitors = Mons} = State) when is_pid(Ocpid), is_pid(Rpid) ->
	?INFO("oncall request during what looks like an agent transfer (outofband) to ~p for ~p", [Ragent, Call#call.id]),
	case set_agent_state(Rpid, [oncall, State#state.callrec]) of
		invalid ->
			{reply, invalid, State};
		ok ->
			case Callback:handle_answer(Rpid, State#state.callrec, State#state.substate) of
				{ok, NewState} ->
					kill_outband_ring(State),
					cdr:oncall(State#state.callrec, Ragent),
					timer:cancel(State#state.ringout),
					set_agent_state(Ocpid, [wrapup, State#state.callrec]),
					cdr:wrapup(State#state.callrec, OcAgent),
					%Agent = agent_manager:find_by_pid(Rpid),
					set_cpx_mon(State#state{substate = NewState, ringout = false, oncall_pid = {Ragent, Rpid}, ring_pid = undefined}, [{agent, Ragent}]),
					erlang:demonitor(Mons#monitors.oncall_pid),
					Newmons = #monitors{oncall_pid = Mons#monitors.ring_pid},
					{reply, ok, State#state{substate = NewState, ringout = false, oncall_pid = {Ragent, Rpid}, ring_pid = undefined, outband_ring_pid = undefined, monitors = Newmons}};
				{error, Reason, NewState} ->
					?ERROR("Cannot set ~p to oncall due to ~p for ~p", [Rpid, Reason, Call#call.id]),
					{reply, invalid, State#state{substate = NewState}}
			end
	end;
handle_call('$gen_media_agent_oncall', {Apid, _Tag}, #state{callback = Callback, ring_pid = {Agent, Apid}, callrec = #call{ring_path = inband} = Call, monitors = Mons} = State) ->
	?INFO("oncall request from agent ~p for ~p", [Apid, Call#call.id]),
	case Callback:handle_answer(Apid, State#state.callrec, State#state.substate) of
		{ok, NewState} ->
			kill_outband_ring(State),
			timer:cancel(State#state.ringout),
			unqueue(State#state.queue_pid, self()),
			cdr:oncall(State#state.callrec, Agent),
			%Agent = agent_manager:find_by_pid(Apid),
			set_cpx_mon(State#state{substate = NewState, ringout = false, queue_pid = undefined, ring_pid = undefined, oncall_pid = {Agent, Apid}}, [{agent, Agent}]),
			erlang:demonitor(Mons#monitors.queue_pid),
			Newsmons = #monitors{oncall_pid = Mons#monitors.ring_pid},
			{reply, ok, State#state{substate = NewState, ringout = false, queue_pid = element(1, State#state.queue_pid), ring_pid = undefined, oncall_pid = {Agent, Apid}, outband_ring_pid = undefined, monitors = Newsmons}};
		{error, Reason, NewState} ->
			?ERROR("Could not set ~p on call due to ~p for ~p", [Apid, Reason, Call#call.id]),
			{reply, invalid, State#state{substate = NewState}}
	end;

handle_call('$gen_media_agent_oncall', From, #state{ring_pid = {Agent, Apid}, callback = Callback, callrec = Call, monitors = Mons} = State) when is_pid(Apid) ->
	?INFO("oncall request from ~p; agent to set on call is ~p for ~p", [From, Apid, Call#call.id]),
	case set_agent_state(Apid, [oncall, Call]) of
		invalid ->
			{reply, invalid, State};
		ok ->
			case Callback:handle_answer(Apid, Call, State#state.substate) of
				{ok, NewState} ->
					kill_outband_ring(State),
					cdr:oncall(Call, Agent),
					timer:cancel(State#state.ringout),
					call_queue:remove(element(2, State#state.queue_pid), self()),
					%Agent = agent_manager:find_by_pid(Apid),
					set_cpx_mon(State#state{substate = NewState, ringout = false, queue_pid = undefined, oncall_pid = {Agent, Apid}, ring_pid = undefined}, [{agent, Agent}]),
					erlang:demonitor(Mons#monitors.queue_pid),
					Newmons = #monitors{oncall_pid = Mons#monitors.ring_pid},
					{reply, ok, State#state{substate = NewState, ringout = false, queue_pid = element(1, State#state.queue_pid), oncall_pid = {Agent, Apid}, ring_pid = undefined, outband_ring_pid = undefined, monitors = Newmons}};
				{error, Reason, NewState} ->
					?ERROR("Could not set ~p on call with ~p due to ~p", [Apid, Call#call.id, Reason]),
					{reply, invalid, State#state{substate = NewState}}
			end;
		badagent ->
			{ok, NewSubstate} = Callback:handle_ring_stop(State#state.callrec, State#state.substate),
			kill_outband_ring(State),
			cdr:ringout(State#state.callrec, {badagent, Agent}),
			timer:cancel(State#state.ringout),
			erlang:demonitor(Mons#monitors.ring_pid),
			Newmons = Mons#monitors{ring_pid = undefined},
			Newstate = State#state{substate = NewSubstate, ringout = false, ring_pid = undefined, outband_ring_pid = undefined, monitors = Newmons},
			Callrec = State#state.callrec,
			gen_server:cast(Callrec#call.cook, stop_ringing),
			{reply, invalid, Newstate}
	end;
handle_call('$gen_media_agent_oncall', From, #state{oncall_pid = {_Ocagent, OcPid}, callrec = Call} = State) when is_pid(OcPid) ->
	?INFO("oncall request from ~p for ~p when already oncall, ignoring", [From, Call#call.id]),
	{reply, ok, State};
handle_call('$gen_media_agent_oncall', From, #state{ring_pid = undefined, callrec = Call} = State) ->
	?INFO("oncall request from ~p for ~p when no ring_pid (probobly a late request)", [From, Call#call.id]),
	{reply, invalid, State};
handle_call({'$gen_media_set_queue', Qpid}, _From, #state{callrec = Call, queue_pid = {Queue, _}, monitors = Mons} = State) ->
	?NOTICE("Updating queue pid for ~p to ~p", [Call#call.id, Qpid]),
	case Mons#monitors.queue_pid of
		undefined ->
			ok;
		M ->
			erlang:demonitor(M)
	end,
	Newmons = Mons#monitors{queue_pid = erlang:monitor(process, Qpid)},
	{reply, ok, State#state{queue_pid = {Queue, Qpid}, monitors = Newmons}};
handle_call('$gen_media_get_url_vars', _From, #state{url_pop_getvars = GenPopopts, substate = Substate, callback = Callback} = State) ->
	Cbopts = case erlang:function_exported(Callback, urlpop_getvars, 1) of
		true ->
			Callback:urlpop_getvars(Substate);
		false ->
			[]
	end,
	Out = lists:ukeymerge(1, lists:ukeysort(1, GenPopopts), lists:ukeysort(1, Cbopts)),
	{reply, {ok, Out}, State};
handle_call(Request, From, #state{callback = Callback} = State) ->
	?DEBUG("gen_media:handle_call with State#state.substate: ~p~nState: ~p", [State#state.substate, State]),
	Reply = Callback:handle_call(Request, From, State#state.callrec, State#state.substate),
	handle_custom_return(Reply, State, reply).
	
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%--------------------------------------------------------------------

%% @private
handle_cast({'$gen_media_set_outband_ring_pid', Pid}, State) ->
	{noreply, State#state{outband_ring_pid = Pid}};
handle_cast({'$gen_media_set_cook', CookPid}, #state{callrec = Call, monitors = Mons} = State) ->
	?NOTICE("Updating cook pid for ~p to ~p", [Call#call.id, CookPid]),
	case Mons#monitors.cook of
		undefined ->
			ok;
		M ->
			erlang:demonitor(M)
	end,
	Newmons = Mons#monitors{cook = erlang:monitor(process, CookPid)},
	{noreply, State#state{callrec = Call#call{cook = CookPid}, monitors = Newmons}};
handle_cast({'$gen_media_set_url_getvars', Vars}, #state{url_pop_getvars = Oldvars} = State) ->
	Newvars = lists:ukeymerge(1, lists:ukeysort(1, Vars), lists:ukeysort(1, Oldvars)),
	?DEBUG("Input:  ~p;  Old:  ~p;  new:  ~p", [Vars, Oldvars, Newvars]),
	{noreply, State#state{url_pop_getvars = Newvars}};
handle_cast({'$gen_media_add_skills', Skills}, #state{callrec = Call} = State) ->
	Newskills = lists:umerge(Call#call.skills, Skills),
	Newcall = Call#call{skills = Newskills},
	{noreply, State#state{callrec = Newcall}};
handle_cast(Msg, #state{callback = Callback} = State) ->
	Reply = Callback:handle_cast(Msg, State#state.callrec, State#state.substate),
	handle_custom_return(Reply, State, noreply).

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%--------------------------------------------------------------------

%% @private
handle_info({'$gen_media_stop_ring', Apid}, #state{ring_pid = {Agent, Apid}, callback = Callback, callrec = Callrec, monitors = Mons} = State) ->
	?INFO("ring agent ~w requested a stop to ringing for ~p", [Apid, Callrec#call.id]),
	{ok, Newsub} = Callback:handle_ring_stop(State#state.callrec, State#state.substate),
	gen_server:cast(Callrec#call.cook, stop_ringing),
	kill_outband_ring(State),
	cdr:ringout(Callrec, {agent_request, Agent}),
	erlang:demonitor(Mons#monitors.ring_pid),
	Newmons = Mons#monitors{ring_pid = undefined},
	{noreply, State#state{substate = Newsub, ringout = false, ring_pid = undefined, outband_ring_pid = undefined, monitors = Newmons}};
handle_info({'$gen_media_stop_ring', _Cook}, #state{ring_pid = undefined, callrec = Call} = State) ->
	?NOTICE("No agent is ringing for this call ~p", [Call#call.id]),
	{noreply, State};
handle_info({'$gen_media_stop_ring', _Cook}, #state{ringout = false, callrec = Call} = State) ->
	?NOTICE("Ringout is set not to be handled for ~p", [Call#call.id]),
	{noreply, State};
handle_info({'$gen_media_stop_ring', Cook}, #state{ring_pid = {Agent, Apid}, callback = Callback, callrec = Call, monitors = Mons} = State) when is_pid(Apid) ->
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
	cdr:ringout(State#state.callrec, {Reason, Agent}),
	erlang:demonitor(Mons#monitors.ring_pid),
	Newmons = Mons#monitors{ring_pid = undefined},
	{noreply, State#state{substate = Newsub, ringout = false, ring_pid = undefined, monitors = Newmons}};
handle_info({'DOWN', Ref, process, _Pid, Info}, #state{monitors = Mons, callrec = Call, callback = _Callback, queue_pid = {Q, _Qp}} = State) when Ref =:= Mons#monitors.queue_pid ->
	?WARNING("Queue ~p died due to ~p (I'm ~p)", [element(1, State#state.queue_pid), Info, Call#call.id]),
	%% in theory, the cook will tell us when the queue is back up.
	Newmons = Mons#monitors{queue_pid = undefined},
	Newstate = State#state{monitors = Newmons, queue_pid = {Q, undefined}},
	{noreply, Newstate};
handle_info({'DOWN', Ref, process, _Pid, Info}, #state{monitors = Mons, callrec = Call, callback = Callback} = State) when Ref =:= Mons#monitors.cook ->
	?WARNING("Cook died due to ~p (I'm ~p)", [Info, Call#call.id]),
	%% if we're queued (which we should be) and ringing, stop ringing
	%% so the new cook doesn't stomp / route on us
	Midmons = Mons#monitors{cook = undefined},
	case {State#state.ring_pid, State#state.queue_pid} of
		{undefined, _} ->
			%% must be a late down message.
			Newstate = State#state{monitors = Midmons},
			{noreply, Newstate};
		{{_Aname, _Apid}, undefined} ->
			% no queue means there should be no cook.
			% must be a late message.
			Newstate = State#state{monitors = Midmons},
			{noreply, Newstate};
		{{Aname, Apid}, {_Qnom, _Qpid}} ->
			case agent:query_state(Apid) of
				{ok, ringing} ->
					set_agent_state(Apid, [idle]);
				_ ->
					ok
			end,
			{ok, Newsub} = Callback:handle_ring_stop(State#state.callrec, State#state.substate),
			kill_outband_ring(State),
			cdr:ringout(State#state.callrec, {cook_death, Aname}),
			erlang:demonitor(Mons#monitors.ring_pid),
			Newmons = Midmons#monitors{ring_pid = undefined},
			{noreply, State#state{substate = Newsub, ringout = false, ring_pid = undefined, monitors = Newmons}}
	end;
handle_info({'DOWN', Ref, process, _Pid, Info}, #state{monitors = Mons, callrec = Call, callback = Callback} = State) when Ref =:= Mons#monitors.ring_pid ->
	?WARNING("ringing Agent fsm ~p died due to ~p (I'm ~p)", [element(1, State#state.ring_pid), Info, Call#call.id]),
	% no need to modify agent state since it's already dead.
	gen_server:cast(Call#call.cook, stop_ringing),
	{ok, Newsub} = Callback:handle_ring_stop(State#state.callrec, State#state.substate),
	kill_outband_ring(State),
	cdr:ringout(State#state.callrec, {agent_fsm_death, element(1, State#state.ring_pid)}),
	Newmons = Mons#monitors{ring_pid = undefined},
	{noreply, State#state{substate = Newsub, ringout = false, ring_pid = undefined, monitors = Newmons}};
handle_info({'DOWN', Ref, process, _Pid, Info}, #state{monitors = Mons, callrec = Call, callback = Callback} = State) when Ref =:= Mons#monitors.oncall_pid ->
	?WARNING("Oncall agent fsm ~p died due to ~p (I'm ~p)", [element(1, State#state.oncall_pid), Info, Call#call.id]),
	Midstate = case State#state.ring_pid of
		{_Ragent, _Rpid} ->
			 % if this agent doesn't answer, the call is orphaned.  Just going to queue.
			{ok, Newsub} = Callback:handle_ring_stop(State#state.callrec, State#state.substate),
			agent_interact({stop_ring, oncall_fsm_death}, State#state{substate = Newsub});
		_ ->
			% noop
			State
	end,
	Bumpedcall = reprioritize_for_requeue(Call),
	% yes I want this to die horribly if we can't requeue
	Queunom = case State#state.queue_pid of
		undefined ->
			"default_queue";
		_ ->
			State#state.queue_pid
	end,
	{reply, ok, Newstate} = handle_call({'$gen_media_queue', Queunom}, {self(), make_ref()}, Midstate#state{callrec = Bumpedcall}),
	cdr:endwrapup(State#state.callrec, element(1, State#state.oncall_pid)),
	{noreply, Newstate};
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

-spec(format_status/2 :: (Cause :: any(), [any()]) -> #state{}).
format_status(normal, [PDict, #state{callback = Mod, substate = SubState, callrec = Call} = State]) ->
	% prevent client data from being dumped
	NewCall = case Call#call.client of
		Client when is_record(Client, client) ->
			Call#call{client = Client#client{options = []}};
		_ ->
			Call
	end,
	NewState = State#state{callrec = NewCall},

	% allow media callback to scrub its state
	SubStat = case erlang:function_exported(Mod, format_status, 2) of
		true ->
			case catch Mod:format_status(normal, [PDict, SubState]) of
				{'EXIT', _} -> SubState;
				Else -> Else
			end;
		_ ->
			SubState
	end,
	[{data, [{"State", NewState#state{substate = ""}}, {"SubState", SubStat}]}];
format_status(terminate, [PDict, #state{callback = Mod, substate = SubState, callrec = Call} = State]) ->
	% prevent client data from being dumped
	NewCall = case Call#call.client of
		Client when is_record(Client, client) ->
			Call#call{client = Client#client{options = []}};
		_ ->
			Call
	end,
	NewState = State#state{callrec = NewCall},

	% allow media callback to scrub its state
	SubStat = case erlang:function_exported(Mod, format_status, 2) of
		true ->
			case catch Mod:format_status(terminate, [PDict, SubState]) of
				{'EXIT', _} -> SubState;
				Else -> Else
			end;
		_ ->
			SubState
	end,
	NewState#state{substate = SubStat}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

set_agent_state(Apid, Args) ->
	try apply(agent, set_state, [Apid | Args]) of
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

handle_stop(hangup, State) ->
	handle_stop({hangup, undefined}, State);
handle_stop(Reason, #state{queue_pid = Qpid, oncall_pid = Ocpid, ring_pid = Rpid, callrec = Call} = State) ->
	{Who, Return} = case Reason of
		{hangup, W} ->
			{W, normal};
		_ ->
			{undefined, Reason}
	end,
	case {Qpid, Ocpid, Rpid} of
		{undefined, undefined, undefined} ->
			?DEBUG("hanging up orphaned call ~p", [Call#call.id]),
			cdr:hangup(State#state.callrec, Who),
			set_cpx_mon(State, delete);
		{Queuenom, undefined, undefined} when is_list(Queuenom) ->
			%cdr:hangup(State#state.callrec, Who),
			?DEBUG("Once queued in ~p, assuming somethign else handles hangup.  ~p", [Queuenom, Call#call.id]),
			set_cpx_mon(State, delete);
		_ ->
			?DEBUG("Forwarding to agent_interact.  ~p", [Call#call.id]),
			set_cpx_mon(State, delete),
			agent_interact({hangup, Who}, State)
	end,
	Return.

handle_custom_return(Return, #state{monitors = Mons} = State, noreply) ->
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
					Newmons = Mons#monitors{queue_pid = erlang:monitor(process, Qpid)},
					{noreply, State#state{callrec = Callrec, substate = NewState, queue_pid = {"default_queue", Qpid}, monitors = Newmons}};
				invalid ->
					?WARNING("Could not queue ~p into ~p (failover ~p)", [Callrec#call.id, Queue, State#state.queue_failover]),
					{noreply, State#state{callrec = Callrec, substate = NewState}};
				Qpid ->
					cdr:cdrinit(Callrec),
					cdr:inqueue(Callrec, Queue),
					set_cpx_mon(State#state{callrec = Callrec, substate = NewState, queue_pid = {"default_queue", Qpid}}, [{queue, Queue}]),
					Newmons = Mons#monitors{queue_pid = erlang:monitor(process, Qpid)},
					{noreply, State#state{callrec = Callrec, substate = NewState, queue_pid = {Queue, Qpid}, monitors = Newmons}}
			end;
		{voicemail, NewState} when is_tuple(State#state.queue_pid) orelse State#state.oncall_pid == undefined ->
			priv_voicemail(State),
			{noreply, State#state{substate = NewState, queue_pid = element(1, State#state.queue_pid), ring_pid = undefined}};
		Tuple when element(1, Tuple) =:= outbound ->
			{_Reply, NewState} = outgoing(Tuple, State),
			{noreply, NewState};
		{Interact, NewState} ->
			Midstate = agent_interact(Interact, State),
			{noreply, Midstate#state{substate = NewState}}
	end;
handle_custom_return(Return, #state{monitors = Mons} = State, reply) ->
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
					set_cpx_mon(State#state{callrec = Callrec, substate = NewState, queue_pid = {"default_queue", Qpid}}, [{queue, "default_queue"}]),
					Newmons = Mons#monitors{queue_pid = erlang:monitor(process, Qpid)},
					{reply, ok, State#state{callrec = Callrec, substate = NewState, queue_pid = {"default_queue", Qpid}, monitors = Newmons}};
				Qpid ->
					cdr:cdrinit(Callrec),
					cdr:inqueue(Callrec, Queue),
					set_cpx_mon(State#state{callrec = Callrec, substate = NewState, queue_pid = {Queue, Qpid}}, [{queue, Queue}]),
					Newmons = Mons#monitors{queue_pid = erlang:monitor(process, Qpid)},
					{reply, ok, State#state{callrec = Callrec, substate = NewState, queue_pid = {Queue, Qpid}, monitors = Newmons}}
			end;
		{voicemail, NewState} when is_tuple(State#state.queue_pid) orelse State#state.oncall_pid == undefined ->
			priv_voicemail(State),
			Newqnom = case State#state.queue_pid of
				undefined ->
					undefined;
				{Nom, _} ->
					Nom
			end,
			{reply, ok, State#state{substate = NewState, queue_pid = Newqnom, ring_pid = undefined}};
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
set_cpx_mon(#state{callrec = _Call} = State, Details) ->
	set_cpx_mon(State, Details, ignore).

set_cpx_mon(#state{callrec = Call} = _State, Details, Watch) ->
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

priv_voicemail(#state{monitors = Mons} = State) ->
	Qref = case State#state.queue_pid of
		undefined ->
			% meh
			undefined;
		{Qnom, Qpid} ->
			erlang:demonitor(Mons#monitors.queue_pid),
			call_queue:remove(Qpid, self()),
			Qnom
	end,
	cdr:voicemail(State#state.callrec, Qref),
	case State#state.ring_pid of
		undefined ->
			ok;
		{_Agent, Apid} when is_pid(Apid) ->
			erlang:demonitor(Mons#monitors.ring_pid),
			set_agent_state(Apid, [idle])
	end,
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

kill_outband_ring(State) ->
	case State#state.outband_ring_pid of
		undefined ->
			ok;
		Pid when is_pid(Pid) ->
			freeswitch_ring:hangup(Pid)
	end.

agent_interact({mediapush, Data}, #state{oncall_pid = {_OcAgent, Ocpid}, callrec = Call} = State) when is_pid(Ocpid), Call#call.media_path =:= inband ->
	?DEBUG("Shoving ~p from ~p", [Data, Call#call.id]),
	agent:media_push(Ocpid, Data),
	State;
agent_interact({mediapush, _Data}, #state{callrec = Call} = State) ->
	?INFO("Cannot do a media push from ~p in current state:  ~p", [Call#call.id, State]),
	State;
agent_interact(stop_ring, State) ->
	agent_interact({stop_ring, undefined}, State);
agent_interact({stop_ring, Reason}, #state{callrec = Call, ring_pid = Ragent, monitors = Mons} = State)  ->
	case Call#call.cook of
		CookPid when is_pid(CookPid) ->
			gen_server:cast(CookPid, stop_ringing);
		_ ->
			ok
	end,
	Midstate = case {State#state.ringout, Ragent} of
		{false, undefined} ->
			% Nothin' doing.
			?INFO("stop_ring for ~p when there's not much of a ring to handle", [Call#call.id]),
			State;
		{false, {Nom, Apid}} ->
			?INFO("stop_ring for ~p with an agent ringing but no timer", [Call#call.id]),
			set_agent_state(Apid, [idle]),
			cdr:ringout(State#state.callrec, {Reason, Nom}),
			erlang:demonitor(Mons#monitors.ring_pid),
			Newmons = Mons#monitors{ring_pid = undefined},
			State#state{ring_pid = undefined, monitors = Newmons};
		{Tref, undefined} ->
			timer:cancel(Tref),
			?WARNING("stop_ring for ~p with only a timer", [Call#call.id]),
			State#state{ringout = false};
		{Tref, {Nom, Apid}} ->
			?INFO("stop_ring for ~p with a timer and agent", [Call#call.id]),
			timer:cancel(Tref),
			set_agent_state(Apid, [idle]),
			cdr:ringout(State#state.callrec, {Reason, Nom}),
			erlang:demonitor(Mons#monitors.ring_pid),
			Newmons = Mons#monitors{ring_pid = undefined},
			State#state{ring_pid = undefined, ringout = false, monitors = Newmons}
	end,
	kill_outband_ring(Midstate),
	Midstate#state{outband_ring_pid = undefined};
agent_interact(wrapup, #state{oncall_pid = {Agent, Apid}, callrec = Call, monitors = Mons} = State) ->
	?INFO("Attempting to set agent at ~p to wrapup for ~p", [Apid, Call#call.id]),
	set_agent_state(Apid, [wrapup, State#state.callrec]),
	cdr:wrapup(State#state.callrec, Agent),
	erlang:demonitor(Mons#monitors.oncall_pid),
	Newmons = Mons#monitors{oncall_pid = undefined},
	State#state{oncall_pid = undefined, monitors = Newmons};
agent_interact(hangup, State) ->
	agent_interact({hangup, undefined}, State);
agent_interact({hangup, Who}, #state{oncall_pid = {Ocagent, Oncallpid}, ring_pid = {_Ragent, Ringpid}, callrec = Call, monitors = Mons} = State) when is_pid(Oncallpid), is_pid(Ringpid) ->
	?INFO("hangup for ~p when both oncall and ring are pids", [Call#call.id]),
	set_agent_state(Ringpid, [idle]),
	set_agent_state(Oncallpid, [wrapup, State#state.callrec]),
	cdr:wrapup(State#state.callrec, Ocagent),
	Callrec = State#state.callrec,
	cdr:hangup(Callrec, Who),
	kill_outband_ring(State),
	erlang:demonitor(Mons#monitors.ring_pid),
	erlang:demonitor(Mons#monitors.oncall_pid),
	Newmons = Mons#monitors{oncall_pid = undefined, ring_pid = undefined},
	State#state{oncall_pid = undefined, ring_pid = undefined, outband_ring_pid = undefined, monitors = Newmons};
agent_interact({hangup, Who}, #state{oncall_pid = {Agent, Apid}, callrec = Call, monitors = Mons} = State) when is_pid(Apid) ->
	?INFO("hangup for ~p when only oncall is a pid", [Call#call.id]),
	set_agent_state(Apid, [wrapup, State#state.callrec]),
	cdr:wrapup(State#state.callrec, Agent),
	Callrec = State#state.callrec,
	cdr:hangup(Callrec, Who),
	erlang:demonitor(Mons#monitors.oncall_pid),
	State#state{oncall_pid = undefined, monitors = #monitors{}};
agent_interact({hangup, Who}, #state{ring_pid = {_Agent, Apid}, queue_pid = {Queue, Qpid}, callrec = Call, monitors = Mons} = State) when is_pid(Apid), is_pid(Qpid) ->
	?INFO("hangup for ~p when both agent and queue are pid", [Call#call.id]),
	set_agent_state(Apid, [idle]),
	cdr:hangup(Call, Who),
	kill_outband_ring(State),
	erlang:demonitor(Mons#monitors.ring_pid),
	unqueue(Qpid, self()),
	erlang:demonitor(Mons#monitors.queue_pid),
	State#state{queue_pid = Queue, ring_pid = undefined, monitors = #monitors{}};
agent_interact({hangup, Who}, #state{ring_pid = {_Agent, Apid}, callrec = Call, monitors = Mons} = State) when is_pid(Apid) ->
	?INFO("hangup for ~p when only ringing is a pid", [Call#call.id]),
	set_agent_state(Apid, [idle]),
	cdr:hangup(Call, Who),
	kill_outband_ring(State),
	erlang:demonitor(Mons#monitors.ring_pid),
	State#state{ring_pid = undefined, outband_ring_pid = undefined, monitors = #monitors{}};
agent_interact({hangup, Who}, #state{queue_pid = {Queue, Qpid}, callrec = Call, monitors = Mons} = State) when is_pid(Qpid) ->
	?INFO("hang for ~p up when only queue is a pid", [Call#call.id]),
	unqueue(Qpid, self()),
	cdr:hangup(State#state.callrec, Who),
	erlang:demonitor(Mons#monitors.queue_pid),
	State#state{queue_pid = Queue, monitors = #monitors{}};
agent_interact({hangup, Who}, #state{callrec = Call, queue_pid = _Nottuple, oncall_pid = undefined, ring_pid = undefined} = State) when is_record(Call, call) ->
	?INFO("orphaned call ~p, no queue or agents at all", [Call#call.id]),
	cdr:hangup(State#state.callrec, Who),
	State;
agent_interact({hangup, _Who}, #state{queue_pid = undefined, oncall_pid = undefined, ring_pid = undefined} = State) ->
	?INFO("Truely orphaned call, no queue, agents, or even a call record", []),
	State.

%% These should crash the media if the agent doesn't exist.
outgoing({outbound, Agent, NewState}, #state{callrec = Call, monitors = Mons} = State) when is_record(State#state.callrec, call) ->
	?INFO("Told to set ~s to outbound for ~p", [Agent, Call#call.id]),
	case agent_manager:query_agent(Agent) of
		{true, Apid} ->
			agent:set_state(Apid, outgoing, State#state.callrec),
			cdr:oncall(State#state.callrec, Agent),
			NewMons = Mons#monitors{oncall_pid = erlang:monitor(process, Apid)},
			{ok, State#state{oncall_pid = {Agent, Apid}, substate = NewState, monitors = NewMons}};
		false ->
			?ERROR("Agent ~s doesn't exists; can't set outgoing for ~p", [Agent, Call#call.id]),
			{{error, {noagent, Agent}}, State#state{substate = NewState}}
	end;
outgoing({outbound, Agent, Call, NewState}, #state{monitors = Mons} = State) when is_record(Call, call) ->
	?INFO("Told to set ~s to outbound and call ~p", [Agent, Call#call.id]),
	case agent_manager:query_agent(Agent) of
		{true, Apid} ->
			agent:set_state(Apid, outgoing, Call),
			cdr:oncall(Call, Agent),
			NewMons = Mons#monitors{oncall_pid = erlang:monitor(process, Apid)},
			{ok, State#state{callrec = Call, substate = NewState, oncall_pid = {Agent, Apid}, monitors = NewMons}};
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
			gen_server_mock:expect_call(Qpid, fun({add, 40, _Inpid, _Callrec}, _From, _State) -> ok end),
			Res = init([dummy_media, Args]),
			?assertMatch({ok, #state{callback = dummy_media, callrec = #call{id = "dummy"}, queue_pid = {"testqueue", Qpid}}}, Res),
			#state{monitors = Mons} = element(2, Res),
			?assertNot(undefined =:= Mons#monitors.queue_pid),
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
			gen_server_mock:expect_call(Qpid, fun({add, 40, _Inpid, _Callrec}, _From, _State) -> ok end),
			Res = init([dummy_media, Args]),
			?assertMatch({ok, #state{callback = dummy_media, callrec = #call{id = "dummy"}, queue_pid = {"default_queue", Qpid}}}, Res),
			Assertmocks()
		end}
	end]}}.

handle_call_test_() ->
	util:start_testnode(),
	N = util:start_testnode(gen_media_handle_call_tests),
	{spawn, N, {foreach,
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
			?assertMatch({reply, invalid, Seedstate}, handle_call({'$gen_media_spy', "Pid", "AgentRec"}, "from", Seedstate)),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMock, _Qpid, Ammock, Assertmocks}) ->
		{"Spy is not the pid making the request",
		fun() ->
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Spy} = agent:start(#agent{login = "testagent", state = released, statedata = "default"}),
			Seedstate = Makestate(),
			State = Seedstate#state{oncall_pid = {"testagent", Spy}},
			?assertMatch({reply, invalid, State}, handle_call({'$gen_media_spy', Spy, "AgentRec"}, {self(), "tag"}, State)),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMock, _Qpid, _Ammock, _Assertmocks}) ->
		{"Can't spy on yourself", 
		fun() ->
			Seedstate = Makestate(),
			Spy = dead_spawn(),
			State = Seedstate#state{oncall_pid = {"testagent", Spy}},
			?assertMatch({reply, invalid, State}, handle_call({'$gen_media_spy', Spy, "AgentRec"}, {Spy, "tag"}, State))
		end}
	end,
	fun({_Makestate, _QMock, _Qpid, Ammock, Assertmocks}) ->
		{"Spy valid, callback says no",
		fun() ->
			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
			Callrec = Seedstate#state.callrec,
			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
			Ocpid = dead_spawn(),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			SpyRec = #agent{login = "testagent", state = released, statedata = "defaut"},
			{ok, Spy} = agent:start(SpyRec),
			?assertMatch({reply, invalid, _Newstate}, handle_call({'$gen_media_spy', Spy, SpyRec}, {Spy, "tag"}, Seedstate#state{oncall_pid = {"testagent", Ocpid}})),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMock, _Qpid, Ammock, Assertmocks}) ->
		{"Spy valid, callback says ok",
		fun() ->
			Seedstate = Makestate(),
			Ocpid = dead_spawn(),
			State = Seedstate#state{oncall_pid = {"ocagent", Ocpid}},
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			SpyRec = #agent{login = "testagent", state= released, statedata = "default"},
			{ok, Spy} = agent:start(SpyRec),
			?assertMatch({reply, ok, _Newstate}, handle_call({'$gen_media_spy', Spy}, {Spy, "tag"}, State)),
			Assertmocks()
		end}
	end,	
	fun({Makestate, _, _, Ammock, Assertmocks}) ->
		{"oncall_pid requests wrapup",
		fun() ->
			Seedstate = Makestate(),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Seedstate#state.callrec}),
			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, "testagent"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({hangup, _Callrer, _Time, "agent"}, _State) -> ok end),
			Monref = make_ref(),
			Mons = #monitors{oncall_pid = Monref},
			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
			Out = handle_call('$gen_media_wrapup', {Agent, "tag"}, State),
			?assertMatch({stop, normal, ok, _State}, Out),
			#state{monitors = Newmon} = element(4, Out),
			?assertEqual(undefined, Newmon#monitors.oncall_pid),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
		{"oncall_pid can't request wrapup when media_path is outband",
		fun() ->
			#state{callrec = Oldcall} = Seedstate = Makestate(),
			Callrec = Oldcall#call{media_path = outband},
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Callrec}),
			State = Seedstate#state{oncall_pid = {"testagent", Agent}, callrec = Callrec},
			?assertMatch({reply, invalid, _State}, handle_call('$gen_media_wrapup', {Agent, "tag"}, State)),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"sending to queue requested by oncall pid, all works",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Seedstate#state.callrec}),
			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
				{ok, Qpid, State}
			end),
			gen_server_mock:expect_call(Qpid, fun({add, 35, Mpid, Rec}, _From, _State) ->
				Mpid = Callrec#call.source,
				Rec = Callrec#call{priority = 35},
				ok
			end),
			gen_event_mock:expect_event(cdr, fun({queue_transfer, _Callrec, _Time, "testqueue"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({inqueue, _Callrec, _Time, "testqueue"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, _Agent}, _State) -> ok end),
			Mons = #monitors{oncall_pid = make_ref()},
			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
			{reply, ok, Newstate} = handle_call({'$gen_media_queue', "testqueue"}, {Agent, "tag"}, State),
			?assertEqual(undefined, Newstate#state.oncall_pid),
			?assertEqual({"testqueue", Qpid}, Newstate#state.queue_pid),
			Newmons = Newstate#state.monitors,
			?assertEqual(undefined, Newmons#monitors.oncall_pid),
			?assertNot(undefined =:= Newmons#monitors.queue_pid),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"oncall pid sends call to queue, but falls back to default queue",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Seedstate#state.callrec}),
			Mons = #monitors{oncall_pid = make_ref()},
			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, MState, _Elec) ->
				{ok, undefined, MState}
			end),
			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "default_queue"}, _From, MState, _Elec) ->
				{ok, Qpid, MState}
			end),
			gen_server_mock:expect_call(Qpid, fun({add, 35, Mpid, Rec}, _From, _State) ->
				Mpid = Callrec#call.source,
				Rec = Callrec#call{priority = 35},
				ok
			end),
			gen_event_mock:expect_event(cdr, fun({queue_transfer, _Callrec, _Time, "default_queue"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({inqueue, _Callrec, _Time, "default_queue"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, _Agent}, _State) -> ok end),
			{reply, ok, Newstate} = handle_call({'$gen_media_queue', "testqueue"}, {Agent, "tag"}, State),
			Newmons = Newstate#state.monitors,
			?assertEqual(undefined, Newstate#state.oncall_pid),
			?assertEqual({"default_queue", Qpid}, Newstate#state.queue_pid),
			?assertEqual(undefined, Newmons#monitors.oncall_pid),
			?assertNot(undefined =:= Newmons#monitors.queue_pid),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, _Qpid, Ammock, Assertmocks}) ->
		{"oncall pid sends call to queue, but falls back to nowhere w/ fallback set to false",
		fun() ->
			Seedstate = Makestate(),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Seedstate#state.callrec}),
			Mons = #monitors{oncall_pid = make_ref()},
			State = Seedstate#state{oncall_pid = {"testagent", Agent}, queue_failover = false, monitors = Mons},
			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, MState, _Elec) ->
				{ok, undefined, MState}
			end),
			Out = handle_call({'$gen_media_queue', "testqueue"}, {Agent, "tag"}, State),
			?assertMatch({reply, invalid, _State}, Out),
			Newstate = element(3, Out),
			?assertEqual(Mons, Newstate#state.monitors),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMmock, Qpid, Ammock, Assertmocks}) ->
		{"sent to queue by something else, and alls well.",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Callrec}),
			gen_server_mock:expect_call(Qpid, fun({add, 35, Mpid, Rec}, _From, _State) ->
				Mpid = Callrec#call.source, 
				Rec = Callrec#call{priority = 35},
				ok
			end),
			gen_leader_mock:expect_leader_call(queue_manager, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
				{ok, Qpid, State}
			end),
			gen_event_mock:expect_event(cdr, fun({queue_transfer, _Callrec, _Time, "testqueue"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({inqueue, _Callrec, _Time, "testqueue"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, _Agent}, _State) -> ok end),
			Mons = #monitors{oncall_pid = make_ref()},
			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
			{reply, ok, Newstate} = handle_call({'$gen_media_queue', "testqueue"}, "from", State),
			Newmons = Newstate#state.monitors,
			?assertEqual(undefined, Newstate#state.oncall_pid),
			?assertEqual({"testqueue", Qpid}, Newstate#state.queue_pid),
			?assertEqual(undefined, Newmons#monitors.oncall_pid),
			?assertNot(undefined =:= Newmons#monitors.queue_pid),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"sent to queue by something else, but falling back",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Callrec}),
			gen_server_mock:expect_call(Qpid, fun({add, 35, Mpid, Rec}, _From, _state) ->
				Mpid = Callrec#call.source,
				Rec = Callrec#call{priority = 35},
				ok
			end),
			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "testqueue"}, _From, State, _Elec) ->
				{ok, undefined, State}
			end),
			gen_leader_mock:expect_leader_call(QMmock, fun({get_queue, "default_queue"}, _From, State, _Elec) ->
				{ok, Qpid, State}
			end),
			gen_event_mock:expect_event(cdr, fun({queue_transfer, _Callrec, _Time, "default_queue"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({inqueue, _Callrec, _Time, "default_queue"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, _Agent}, _State) -> ok end),
			Mons = #monitors{oncall_pid = make_ref()},
			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
			{reply, ok, Newstate} = handle_call({'$gen_media_queue', "testqueue"}, "from", State),
			Newmons = Newstate#state.monitors,
			?assertEqual(undefined, Newstate#state.oncall_pid),
			?assertEqual({"default_queue", Qpid}, Newstate#state.queue_pid),
			?assertEqual(undefined, Newmons#monitors.oncall_pid),
			?assertNot(undefined =:= Newmons#monitors.queue_pid),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
		{"gen_media_ring setting agent successful, as is the callback module.",
		fun() ->
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = idle, statedata = {}}),
			#state{callrec = Callrec} = Seedstate = Makestate(),
			gen_event_mock:expect_event(cdr, fun({ringing, _Callrec, _Time, "testagent"}, _State) -> ok end),
			#queued_call{cook = Cook} = Qcall = #queued_call{media = Callrec#call.source, id = "testcall"},
			gen_leader_mock:expect_cast(Ammock, fun({end_avail, _}, _, _) -> ok end),
			{reply, ok, Newstate} = handle_call({'$gen_media_ring', {"testagent", Agent}, Qcall, 100}, {Cook, "tag"}, Seedstate),
			receive
				{'$gen_media_stop_ring', Cook} ->
					ok
			after 150 ->
				erlang:error(timer_timeout)
			end,
			?assertEqual({"testagent", Agent}, Newstate#state.ring_pid),
			?assertNot(false =:= Newstate#state.ringout),
			Mons = Newstate#state.monitors,
			?assertNot(undefined =:= Mons#monitors.ring_pid),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
		{"gen_media_ring setting the agent fails.",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			#queued_call{cook = Cook} = Qcall = #queued_call{media = Callrec#call.source, id = "testcall"},
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = "whatever"}),
			Out = handle_call({'$gen_media_ring', {"testagent", Agent}, Qcall, 100}, {Cook, "tag"}, Seedstate),
			?assertMatch({reply, invalid, _State}, Out),
			receive
				{'$gen_media_stop_ring', Cook} ->
					erlang:error(timer_lives)
			after 150 ->
				ok
			end,
			Newstate = element(3, Out),
			?assertEqual(Seedstate#state.monitors, Newstate#state.monitors),
			Assertmocks()
		end}
	end,
	fun({_Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
		{"gen_media_ring callback module fails",
		fun() ->
			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
			Callrec = Seedstate#state.callrec,
			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
			#queued_call{cook = Cook} = Qcall = #queued_call{media = Callrec#call.source, id = "testcall"},
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			gen_leader_mock:expect_cast(Ammock, fun({end_avail, _}, _, _) -> ok end),
			%gen_leader_mock:expect_cast(Ammock, fun({end_avail, _}, _, _) -> ok end),
			gen_leader_mock:expect_cast(Ammock, fun({now_avail, _}, _, _) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = idle, statedata = {}}),
			{reply, invalid, Newstate} = handle_call({'$gen_media_ring', {"testagent", Agent}, Qcall, 150}, {Cook, "tag"}, Seedstate),
			receive
				{'$gen_media_stop_ring', Cook} ->
					erlang:error(timer_lives)
			after 150 ->
				ok
			end,
			?assertEqual(undefined, Newstate#state.ring_pid),
			?assertEqual({ok, idle}, agent:query_state(Agent)),
			?assertEqual(Seedstate#state.monitors, Newstate#state.monitors),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
		{"can't transfer to yourself, silly!",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Callrec}),
			Mons = #monitors{oncall_pid = make_ref()},
			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
			?assertEqual({reply, invalid, State}, handle_call({'$gen_media_agent_transfer', {"testagent", Agent}}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
		{"agent transfer, target agent can't change state",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Callrec}),
			{ok, Target} = agent:start(#agent{login = "targetagent", state = wrapup, statedata = "doesn't matter"}),
			Mons = #monitors{oncall_pid = make_ref()},
			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
			?assertEqual({reply, invalid, State}, handle_call({'$gen_media_agent_transfer', {"targetagent", Target}, 100}, "from", State)),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
		{"agent transfer, all is well",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			%% cdr makes 2 call outs to this, but that will be tested in cdr
			gen_event_mock:expect_event(cdr, fun({agent_transfer, _Callrec, _Time, {"testagent", "targetagent"}}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({ringing, _Callrec, _Time, "targetagent"}, _State) -> ok end),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			gen_leader_mock:expect_cast(Ammock, fun({end_avail, "targetagent"}, _, _) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Callrec}),
			{ok, Target} = agent:start(#agent{login = "targetagent", state = idle, statedata = {}}),
			Mons = #monitors{oncall_pid = make_ref()},
			State = Seedstate#state{oncall_pid = {"testagent", Agent}, monitors = Mons},
			{reply, ok, Newstate} = handle_call({'$gen_media_agent_transfer', {"targetagent", Target}, 100}, "from", State),
			receive
				{'$gen_media_stop_ring', _Cook} ->
					ok
			after 150 ->
				erlang:error(timer_nolives)
			end,
			?assertEqual({"targetagent", Target}, Newstate#state.ring_pid),
			?assertEqual({"testagent", Agent}, Newstate#state.oncall_pid),
			?assertNot(false =:= Newstate#state.ringout),
			?assertEqual({ok, ringing}, agent:query_state(Target)),
			Newmons = Newstate#state.monitors,
			?assertEqual(Mons#monitors.oncall_pid, Newmons#monitors.oncall_pid),
			?assertNot(undefined =:= Newmons#monitors.ring_pid),
			Assertmocks()
		end}
	end,
	fun({_Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
		{"agent transfer, callback says no",
		fun() ->
			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
			Callrec = Seedstate#state.callrec,
			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Target} = agent:start(#agent{login = "testagent", state = idle, statedata = {}}),
			Agent = spawn(fun() -> ok end),
			State = Seedstate#state{oncall_pid = {"testagent", Agent}},
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
	fun({_Makestate, _QMmock, _Qpid, _Ammock, Assertmocks}) ->
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
	fun({Makestate, _QMmock, Qpid, _Ammock, Assertmocks}) ->
		{"gen_media_voicemail works",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			Mons = #monitors{queue_pid = make_ref()},
			State = Seedstate#state{queue_pid = {"default_queue", Qpid}, monitors = Mons},
			gen_server_mock:expect_call(Qpid, fun({remove, Inpid}, _From, _State) ->
				Inpid = Callrec#call.source,
				ok
			end),
			gen_event_mock:expect_event(cdr, fun({voicemail, _Callrec, _Time, "default_queue"}, _State) -> ok end),
			Out = handle_call('$gen_media_voicemail', "from", State),
			?assertMatch({reply, ok, _State}, Out),
			#state{monitors = Newmons} = element(3, Out),
			?assertEqual(undefined, Newmons#monitors.queue_pid),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMmock, Qpid, Ammock, Assertmocks}) ->
		{"gen_media_voicemail while an agent's ringing",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			gen_leader_mock:expect_cast(Ammock, fun({now_avail, "testagent"}, _, _) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = ringing, statedata = Callrec}),
			Mons = #monitors{queue_pid = make_ref(), ring_pid = make_ref()},
			State = Seedstate#state{queue_pid = {"default_queue", Qpid}, ring_pid = {"testagent", Agent}, monitors = Mons},
			gen_server_mock:expect_call(Qpid, fun({remove, Inpid}, _From, _State) ->
				Inpid = Callrec#call.source,
				ok
			end),
			gen_event_mock:expect_event(cdr, fun({voicemail, _Callrec, _Time, "default_queue"}, _State) -> ok end),
			{reply, ok, NewState} = handle_call('$gen_media_voicemail', "from", State),
			Newmons = NewState#state.monitors,
			?assertEqual(undefined, NewState#state.ring_pid),
			?assertEqual({ok, idle}, agent:query_state(Agent)),
			?assertEqual(undefined, Newmons#monitors.queue_pid),
			?assertEqual(undefined, Newmons#monitors.ring_pid),
			Assertmocks()
		end}
	end,
	fun({_Makestate, _QMmock, Qpid, _Ammock, Assertmocks}) ->
		{"gen_media_voicemail callback says no",
		fun() ->
			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
			#state{callrec = Call} = State = Seedstate#state{queue_pid = {"default_queue", Qpid}},
			gen_event_mock:supplant(cdr, {{cdr, Call#call.id}, []}),
			?assertMatch({reply, invalid, _State}, handle_call('$gen_media_voicemail', "from", State)),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMmock, _Qpid, _Ammock, Assertmocks}) ->
		{"Agent can't request oncall if ring_path is outband",
		fun() ->
			#state{callrec = Seedcall} = Seedstate = Makestate(),
			Agent = spawn(fun() -> ok end),
			Callrec = Seedcall#call{ring_path = outband},
			State = Seedstate#state{callrec = Callrec, ring_pid = {"testagent", Agent}},
			?assertEqual({reply, invalid, State}, handle_call('$gen_media_agent_oncall', {Agent, "tag"}, State)),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
		{"agent oncall request when both a ring pid and oncall pid are set and media path is inband",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Oncall} = agent:start(#agent{login = "oncall", state = oncall, statedata = Callrec}),
			{ok, Ring} = agent:start(#agent{login = "ringing", state = oncall, statedata = Callrec}),
			gen_event_mock:expect_event(cdr, fun({oncall, _Callrec, _Time, "ringing"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, "oncall"}, _State) -> ok end),
			{ok, Tref} = timer:send_after(100, timer_lives),
			Mons = #monitors{oncall_pid = make_ref()},
			State = Seedstate#state{oncall_pid = {"oncall", Oncall}, ring_pid = {"ringing", Ring}, ringout = Tref, monitors = Mons},
			{reply, ok, Newstate} = handle_call('$gen_media_agent_oncall', {Ring, "tag"}, State),
			receive
				timer_lives ->
					erlang:error(timer_lives)
			after 150 ->
				ok
			end,
			?assertNot(Newstate#state.ringout),
			?assertEqual({"ringing", Ring}, Newstate#state.oncall_pid),
			?assertEqual(undefined, Newstate#state.ring_pid),
			?assertEqual({ok, wrapup}, agent:query_state(Oncall)),
			Newmons = Newstate#state.monitors,
			?assertEqual(Mons#monitors.ring_pid, Newmons#monitors.oncall_pid),
			?assertEqual(undefined, Newmons#monitors.ring_pid),
			Assertmocks()
		end}
	end,
	fun({_Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
		{"agent oncall request when both a ring pid and oncall pid are set and media path is inband, but callback says no",
		fun() ->
			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
			Callrec = Seedstate#state.callrec,
			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Oncall} = agent:start(#agent{login = "oncall", state = oncall, statedata = Callrec}),
			{ok, Ring} = agent:start(#agent{login = "ring", state = ringing, statedata = Callrec}),
			{ok, Tref} = timer:send_after(100, timer_lives),
			Mons = #monitors{oncall_pid = make_ref()},
			State = Seedstate#state{oncall_pid = {"oncall", Oncall}, ring_pid = {"ring", Ring}, ringout = Tref, monitors = Mons},
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
	fun({Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
		{"oncall during transfer with outband media",
		fun() ->
			#state{callrec = Seedcall} = Seedstate = Makestate(),
			Callrec = Seedcall#call{ring_path = outband},
			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			gen_leader_mock:expect_cast(Ammock, fun({end_avail, "ring"}, _, _) -> ok end),
			{ok, Oncall} = agent:start(#agent{login = "oncall", state = oncall, statedata = Callrec}),
			{ok, Ring} = agent:start(#agent{login = "ring", state = ringing, statedata = Callrec}),
			gen_event_mock:expect_event(cdr, fun({oncall, _Callrec, _Time, "ring"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, "oncall"}, _State) -> ok end),
			{ok, Tref} = timer:send_after(100, timer_lives),
			Mons = #monitors{
				oncall_pid = make_ref(),
				ring_pid = make_ref()
			},
			State = Seedstate#state{oncall_pid = {"oncall", Oncall}, ringout = Tref, ring_pid = {"ring", Ring}, callrec = Callrec, monitors = Mons},
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
			?assertEqual({"ring", Ring}, Newstate#state.oncall_pid),
			Newmons = Newstate#state.monitors,
			?assertEqual(Mons#monitors.ring_pid, Newmons#monitors.oncall_pid),
			?assertEqual(undefined, Newmons#monitors.ring_pid),
			Assertmocks()
		end}
	end,
	fun({_Makestate, _QMmock, _Qpid, Ammock, Assertmocks}) ->
		{"oncall during transfer with outband media, but callback says no",
		fun() ->
			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
			Seedcall = Seedstate#state.callrec,
			gen_event_mock:supplant(cdr, {{cdr, Seedcall#call.id}, []}),
			Callrec = Seedcall#call{ring_path = outband},
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			gen_leader_mock:expect_cast(Ammock, fun({end_avail, "ring"}, _, _) -> ok end),
			{ok, Oncall} = agent:start(#agent{login = "oncall", state = oncall, statedata = Callrec}),
			{ok, Ring} = agent:start(#agent{login = "ring", state = ringing, statedata = Callrec}),
			{ok, Tref} = timer:send_after(100, timer_lives),
			State = Seedstate#state{oncall_pid = {"oncall", Oncall}, ringout = Tref, ring_pid = {"ring", Ring}, callrec = Callrec},
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
			?assertEqual({"ring", Ring}, Newstate#state.ring_pid),
			?assertEqual(Tref, Newstate#state.ringout),
			?assertEqual({"oncall", Oncall}, Newstate#state.oncall_pid),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMmock, Qpid, Ammock, Assertmocks}) ->
		{"oncall queue to agent requested by agent with inband media",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = ringing, statedata = Callrec}),
			gen_server_mock:expect_call(Qpid, fun({remove, Inpid}, _From, _State) ->
				Inpid = Callrec#call.source,
				ok
			end),
			gen_event_mock:expect_event(cdr, fun({oncall, _Callrec, _Time, "testagent"}, _State) -> ok end),
			{ok, Tref} = timer:send_after(100, timer_lives),
			Mons = #monitors{ring_pid = make_ref(), queue_pid = make_ref()},
			State = Seedstate#state{queue_pid = {"default_queue", Qpid}, ring_pid = {"testagent", Agent}, ringout = Tref, monitors = Mons},
			{reply, ok, Newstate} = handle_call('$gen_media_agent_oncall', {Agent, "tag"}, State),
			receive
				timer_lives ->
					erlang:error(timer_lives)
			after 150 ->
				ok
			end,
			?assertNot(Newstate#state.ringout),
			?assertEqual({"testagent", Agent}, Newstate#state.oncall_pid),
			?assertEqual(undefined, Newstate#state.ring_pid),
			?assertEqual("default_queue", Newstate#state.queue_pid),
			Newmons = Newstate#state.monitors,
			?assertEqual(undefined, Newmons#monitors.ring_pid),
			?assertEqual(undefined, Newmons#monitors.queue_pid),
			?assertEqual(Mons#monitors.ring_pid, Newmons#monitors.oncall_pid),
			Assertmocks()
		end}
	end,
	fun({_Makestate, _QMmock, Qpid, Ammock, Assertmocks}) ->
		{"oncall quee to agent request by agent, but callback says no",
		fun() ->
			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
			Callrec = Seedstate#state.callrec,
			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = ringing, statedata = Callrec}),
			{ok, Tref} = timer:send_after(100, timer_lives),
			Mons = #monitors{ring_pid = make_ref()},
			State = Seedstate#state{ringout = Tref, ring_pid = {"testagent", Agent}, queue_pid = {"default_queue", Qpid}, monitors = Mons},
			{reply, invalid, Newstate} = handle_call('$gen_media_agent_oncall', {Agent, "tag"}, State),
			receive
				timer_lives ->
					ok
			after 150 ->
				erlang:error(timer_nolives)
			end,
			?assertEqual({"testagent", Agent}, Newstate#state.ring_pid),
			?assertEqual({"default_queue", Qpid}, Newstate#state.queue_pid),
			?assertEqual(Tref, Newstate#state.ringout),
			?assertEqual(Mons, Newstate#state.monitors),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMmock, Qpid, Ammock, Assertmocks}) ->
		{"oncall queue to agent requst by whoever with outband media",
		fun() ->
			#state{callrec = Seedcall} = Seedstate = Makestate(),
			Callrec = Seedcall#call{ring_path = outband, media_path = outband},
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			gen_leader_mock:expect_cast(Ammock, fun({end_avail, "testagent"}, _, _) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = ringing, statedata = Callrec}),
			gen_server_mock:expect_call(Qpid, fun({remove, Inpid}, _From, _State) ->
				Inpid = Callrec#call.source,
				ok
			end),
			gen_event_mock:expect_event(cdr, fun({oncall, _Callrec, _Time, "testagent"}, _State) -> ok end),
			{ok, Tref} = timer:send_after(100, timer_lives),
			Mons = #monitors{ring_pid = make_ref(), queue_pid = make_ref()},
			State = Seedstate#state{callrec = Callrec, ring_pid = {"testagent", Agent}, queue_pid = {"default_queue", Qpid}, ringout = Tref, monitors = Mons},
			{reply, ok, Newstate} = handle_call('$gen_media_agent_oncall', "from", State),
			receive
				timer_lives -> 
					erlang:error(timer_lives)
			after 150 ->
				ok
			end,
			?assertNot(Newstate#state.ringout),
			?assertEqual({"testagent", Agent}, Newstate#state.oncall_pid),
			?assertEqual("default_queue", Newstate#state.queue_pid),
			?assertEqual(undefined, Newstate#state.ring_pid),
			Newmons = Newstate#state.monitors,
			?assertEqual(undefined, Newmons#monitors.queue_pid),
			?assertEqual(undefined, Newmons#monitors.ring_pid),
			?assertEqual(Mons#monitors.ring_pid, Newmons#monitors.oncall_pid),
			Assertmocks()
		end}
	end,
	fun({_Makestate, _QMmock, Qpid, Ammock, Assertmocks}) ->
		{"oncall queue to agent request by whoever with outband media, but callback says no",
		fun() ->
			{ok, Seedstate} = init([dummy_media, [[{queues, none}], failure]]),
			Seedcall = Seedstate#state.callrec,
			gen_event_mock:supplant(cdr, {{cdr, Seedcall#call.id}, []}),
			Callrec = Seedcall#call{ring_path = outband, media_path = outband},
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = oncall, statedata = Callrec}),
			{ok, Tref} = timer:send_after(100, timer_lives),
			Mons = #monitors{
				oncall_pid = make_ref(),
				queue_pid = make_ref()
			},
			State = Seedstate#state{callrec = Callrec, ring_pid = {"testagent", Agent}, ringout = Tref, queue_pid = {"default_queue", Qpid}, monitors = Mons},
			{reply, invalid, Newstate} = handle_call('$gen_media_agent_oncall', "from", State),
			receive
				timer_lives -> 
					ok
			after 150 ->
				erlang:error(timer_nolives)
			end,
			?assertNot(false =:= Newstate#state.ringout),
			?assertEqual({"testagent", Agent}, Newstate#state.ring_pid),
			?assertEqual({"default_queue", Qpid}, Newstate#state.queue_pid),
			?assertEqual(Mons, Newstate#state.monitors),
			Assertmocks()
		end}
	end,
	fun({Makestate, _QMmock, _Qpid, _Ammock, _Assertmocks}) ->
		{"late oncall request (ring_pid is undefined)",
		fun() ->
			Seedstate = Makestate(),
			State = Seedstate#state{ring_pid = undefined},
			?assertMatch({reply, invalid, State}, handle_call('$gen_media_agent_oncall', "from", State))
		end}
	end,
	fun({_Makestate, _QMmock, Qpid, _Ammock, Assertmocks}) ->
		{"oncall request to agent that is no longer running",
		fun() ->
			{ok, Seedstate} = init([dummy_media, [[{queues, none}], success]]),
			Seedcall = Seedstate#state.callrec,
			gen_event_mock:supplant(cdr, {{cdr, Seedcall#call.id}, []}),
			Callrec = Seedcall#call{ring_path = outband, media_path = outband},
			Agent = spawn(fun() -> ok end),
			{ok, Tref} = timer:send_after(100, timer_lives),
			Mon = #monitors{ring_pid = make_ref()},
			State = Seedstate#state{callrec = Callrec, ring_pid = {"deadagent", Agent}, ringout = Tref, queue_pid = {"default_queue", Qpid}, monitors = Mon},
			gen_event_mock:expect_event(cdr, fun({ringout, _Callrec, _Time, _Data}, _) -> ok end),
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
			?assertEqual({"default_queue", Qpid}, State#state.queue_pid),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, _Ammock, Assertmocks}) ->
		{"endcall request to media that supports it while inqueue",
		fun() ->
			#state{callrec = Callrec} = InSeedstate = Makestate(),
			#queued_call{cook = Cook} = #queued_call{media = Callrec#call.source, id = "testcall"},
			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
			gen_event_mock:expect_event(cdr, fun({hangup, _, _, queue}, _) -> ok end),
			gen_server_mock:expect_call(Qpid, fun({remove, _}, _, _) -> ok end),
			Seedstate = InSeedstate#state{queue_pid = {"testqueue", Qpid}, monitors = #monitors{queue_pid = make_ref()}},
			Out = handle_call('$gen_media_end_call', {Cook, "tag"}, Seedstate),
			?assertMatch({stop, normal, ok, _State}, Out)
		end}
	end,
	fun({Makestate, QMmock, Qpid, Ammock, Assertmocks}) ->
		{"endcall request to media that supports it while inqueue and agent ringing",
		fun() ->
			#state{callrec = Callrec} = InSeedstate = Makestate(),
			#queued_call{cook = Cook} = #queued_call{media = Callrec#call.source, id = "testcall"},
			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
			gen_event_mock:expect_event(cdr, fun({hangup, _, _, queue}, _) -> ok end),
			gen_leader_mock:expect_cast(Ammock, fun({update_skill_list, _, _}, _, _) -> ok end),
			gen_leader_mock:expect_cast(Ammock, fun({now_avail, "testagent"}, _, _) -> ok end),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = ringing, statedata = Callrec}),
			Seedstate = InSeedstate#state{queue_pid = {"testqueue", Qpid}, ring_pid = {"testagent", Agent}, monitors = #monitors{queue_pid = make_ref(), ring_pid = make_ref()}},
			gen_server_mock:expect_call(Qpid, fun({remove, _}, _, _) -> ok end),
			Out = handle_call('$gen_media_end_call', {Cook, "tag"}, Seedstate),
			?assertMatch({stop, normal, ok, _State}, Out),
			receive
				{'$gen_media_stop_ring', Cook} ->
					erlang:error(timer_lives)
			after 150 ->
				ok
			end,
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, _Ammock, Assertmocks}) ->
		{"endcall request to media that supports deffered while inqueue",
		fun() ->
			{ok, InSeedstate} = init([dummy_media, [[{queues, none}], [{call_end, deferred}]]]),
			Seedstate = InSeedstate#state{queue_pid = {"testqueue", Qpid}},
			#state{callrec = Callrec} = Seedstate,
			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
			#queued_call{cook = Cook} = #queued_call{media = Callrec#call.source, id = "testcall"},
			Out = handle_call('$gen_media_end_call', {Cook, "tag"}, Seedstate),
			?assertMatch({reply, ok, _State}, Out),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, _Ammock, Assertmocks}) ->
		{"endcall request to media that fails it",
		fun() ->
			{ok, Seedstate} = init([dummy_media, [[{queues, none}], [{call_end, fail}]]]),
			#state{callrec = Callrec} = Seedstate,
			gen_event_mock:supplant(cdr, {{cdr, Callrec#call.id}, []}),
			#queued_call{cook = Cook} = #queued_call{media = Callrec#call.source, id = "testcall"},
			Out = handle_call('$gen_media_end_call', {Cook, "tag"}, Seedstate),
			?assertMatch({reply, invalid, _State}, Out),
			Assertmocks()
		end}
	end,
	fun({Makestate, QMmock, Qpid, _Ammock, Assertmocks}) ->
		{"endcall request to media that supports it, but not in queue",
		fun() ->
			#state{callrec = Callrec} = Seedstate = Makestate(),
			Out = handle_call('$gen_media_end_call', "from", Seedstate),
			?assertMatch({reply, invalid, _State}, Out),
			Assertmocks()
		end}
	end]}}.

handle_cast_test_() ->
	util:start_testnode(),
	N = util:start_testnode(gen_media_handle_cast),
	{spawn, N, {foreach,
	fun() ->
		Call = #call{
			id = "testcall",
			source = self()
		},
		{#state{callrec = Call}}
	end,
	fun(_) ->
		ok
	end,
	[fun({Seedstate}) ->
		{"setting outband ring pid",
		fun() ->
			P = dead_spawn(),
			{noreply, #state{outband_ring_pid = NewP}} = handle_cast({'$gen_media_set_outband_ring_pid', P}, Seedstate),
			?assertEqual(P, NewP)
		end}
	end,
	fun({Seedstate}) ->
		{"setting the cook with no previous mons",
		fun() ->
			P = spawn(fun() ->
				receive
					done ->
						ok
				end
			end),
			{noreply, #state{callrec = Newcall, monitors = Mons}} = handle_cast({'$gen_media_set_cook', P}, Seedstate),
			?assert(Mons#monitors.cook =/= undefined andalso is_reference(Mons#monitors.cook)),
			?assertEqual(P, Newcall#call.cook),
			P ! done
		end}
	end,
	fun({Seedstate}) ->
		{"setting additional url pop opts",
		fun() ->
			{noreply, #state{url_pop_getvars = Urlget}} = handle_cast({'$gen_media_set_url_getvars', [{"key", "val"}]}, Seedstate),
			?assertEqual([{"key", "val"}], Urlget)
		end}
	end,
	fun({Seedstate}) ->
		{"setting new url pop opts doen't nix old ones",
		fun() ->
			State = Seedstate#state{url_pop_getvars = [{"oldkey", "oldval"}]},
			{noreply, #state{url_pop_getvars = Newget}} = handle_cast({'$gen_media_set_url_getvars', [{"newkey", "newval"}]}, State),
			?assertEqual("oldval", proplists:get_value("oldkey", Newget)),
			?assertEqual("newval", proplists:get_value("newkey", Newget))
		end}
	end,
	fun({Seedstate}) ->
		{"Adding new skills",
		fun() ->
			{noreply, #state{callrec = Callrec}} = handle_cast({'$gen_media_add_skills', [cookskill, {'_agent', "anagent"}]}, Seedstate),
			?assertEqual([cookskill, {'_agent', "anagent"}], Callrec#call.skills)
		end}
	end,
	fun({#state{callrec = Oldcall} = Seedstate}) ->
		{"Adding existing skills",
		fun() ->
			Call = Oldcall#call{skills = [cookskill]},
			State = Seedstate#state{callrec = Call},
			{noreply, #state{callrec = Newcall}} = handle_cast({'$gen_media_add_skills', [cookskill]}, State),
			?assertEqual([cookskill], Newcall#call.skills)
		end}
	end]}}.

handle_info_test_() ->
	util:start_testnode(),
	N = util:start_testnode(gen_media_handle_info_tests),
	{spawn, N, {foreach,
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
			Mons = #monitors{ring_pid = make_ref()},
			State = Seedstate#state{ring_pid = {"testagent", Apid}, callrec = Callrec, monitors = Mons},
			{noreply, Newstate} = handle_info({'$gen_media_stop_ring', Apid}, State),
			?assertNot(Newstate#state.ringout),
			?assertEqual(undefined, Newstate#state.ring_pid),
			Newmons = Newstate#state.monitors,
			?assertEqual(undefined, Newmons#monitors.ring_pid),
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
			Mons = #monitors{ring_pid = make_ref()},
			State = Seedstate#state{ring_pid = Pid, monitors = Mons},
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
			gen_leader_mock:expect_cast(Am, fun({now_avail, _}, _, _) -> ok end),
			Mons = #monitors{ring_pid = make_ref()},
			State = Seedstate#state{ring_pid = {"testagent", Agent}, ringout = true, monitors = Mons},
			{noreply, Newstate} = handle_info({'$gen_media_stop_ring', Cook}, State),
			gen_server_mock:assert_expectations(Cook),
			?assertEqual({ok, idle}, agent:query_state(Agent)),
			?assertNot(Newstate#state.ringout),
			?assertEqual(undefined, Newstate#state.ring_pid),
			Newmons = Newstate#state.monitors,
			?assertEqual(undefined, Newmons#monitors.ring_pid),
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
			Mons = #monitors{ring_pid = make_ref()},
			State = Seedstate#state{ring_pid = {"testagent", Agent}, ringout = true, monitors = Mons},
			{noreply, Newstate} = handle_info({'$gen_media_stop_ring', Cook}, State),
			gen_server_mock:assert_expectations(Cook),
			?assertEqual({ok, oncall}, agent:query_state(Agent)),
			?assertNot(Newstate#state.ringout),
			?assertEqual(undefined, Newstate#state.ring_pid),
			Newmons = Newstate#state.monitors,
			?assertEqual(undefined, Newmons#monitors.ring_pid),
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
			Mons = #monitors{ring_pid = make_ref()},
			State = Seedstate#state{ring_pid = {"deadagent", Agent}, ringout = true, monitors = Mons},
			{noreply, Newstate} = handle_info({'$gen_media_stop_ring', Cook}, State),
			gen_server_mock:assert_expectations(Cook),
			?assertNot(Newstate#state.ringout),
			?assertEqual(undefined, Newstate#state.ring_pid),
			Newmons = Newstate#state.monitors,
			?assertEqual(undefined, Newmons#monitors.ring_pid),
			gen_leader_mock:stop(Am)
		end}
	end,
	fun({Seedstate}) ->
		{"queue pid goes down",
		fun() ->
			Qpid = dead_spawn(),
			Qref = make_ref(),
			Mons = #monitors{queue_pid = Qref},
			State = Seedstate#state{monitors = Mons, queue_pid = {"testqueue", Qpid}},
			{noreply, Newstate} = handle_info({'DOWN', Qref, process, Qpid, testdeath}, State),
			?assertEqual(#monitors{}, Newstate#state.monitors),
			?assertEqual({"testqueue", undefined}, Newstate#state.queue_pid)
		end}
	end,
	fun({#state{callrec = Oldcall} = Seedstate}) ->
		{"Cook pid goes down with no agent ringing",
		fun() ->
			Qpid = dead_spawn(),
			Cook = dead_spawn(),
			Qref = make_ref(),
			CookRef = make_ref(),
			Mons = #monitors{queue_pid = Qref, cook = CookRef},
			State = Seedstate#state{queue_pid = {"testqueue", Qpid}, callrec = Oldcall#call{cook = Cook}, monitors = Mons},
			{noreply, Newstate} = handle_info({'DOWN', CookRef, process, Cook, testdeath}, State),
			?assertEqual(#monitors{cook = undefined, queue_pid = Qref}, Newstate#state.monitors),
			?assertEqual({"testqueue", Qpid}, Newstate#state.queue_pid)
		end}
	end,
	fun({#state{callrec = Oldcall} = Seedstate}) ->
		{"Cook pid goes down with an agent ringing",
		fun() ->
			Qpid = dead_spawn(),
			Cook = dead_spawn(),
			{ok, Agent} = agent:start(#agent{login = "testagent", state = ringing, statedata = Seedstate#state.callrec}),
			Qref = make_ref(),
			CookRef = make_ref(),
			Aref = make_ref(),
			Mons = #monitors{queue_pid = Qref, cook = CookRef, ring_pid = Aref},
			State = Seedstate#state{queue_pid = {"testqueue", Qpid}, ring_pid = {"testagent", Agent}, callrec = Oldcall#call{cook = Cook}, monitors = Mons},
			{noreply, Newstate} = handle_info({'DOWN', CookRef, process, Cook, testdeath}, State),
			?assertEqual(#monitors{queue_pid = Qref}, Newstate#state.monitors),
			?assertEqual({"testqueue", Qpid}, Newstate#state.queue_pid),
			?assertEqual(undefined, Newstate#state.ring_pid)
		end}
	end,
	fun({#state{callrec = Oldcall} = Seedstate}) ->
		{"ringing agent dies",
		fun() ->
			{ok, Cook} = gen_server_mock:new(),
			gen_server_mock:expect_cast(Cook, fun(stop_ringing, _State) -> ok end),
			Agent = dead_spawn(),
			AgentRef = make_ref(),
			Qref = make_ref(),
			Cookref = make_ref(),
			Mons = #monitors{queue_pid = Qref, cook = Cookref, ring_pid = AgentRef},
			Call = Oldcall#call{cook = Cook},
			State = Seedstate#state{monitors = Mons, queue_pid = {"testqueue", dead_spawn()}, ring_pid = {"testagent", Agent}, callrec = Call},
			{noreply, Newstate} = handle_info({'DOWN', AgentRef, process, Agent, testdeath}, State),
			gen_server_mock:assert_expectations(Cook),
			?assertEqual(#monitors{queue_pid = Qref, cook = Cookref}, Newstate#state.monitors),
			?assertEqual(undefined, Newstate#state.ring_pid)
		end}
	end,
	fun({Seedstate}) ->
		{"oncall agent dies with no ringing agent",
		fun() ->
			OncallRef = make_ref(),
			Agent = dead_spawn(),
			Mons = #monitors{
				oncall_pid = OncallRef
			},
			{ok, Newqueue} = gen_server_mock:new(),
			gen_server_mock:expect_call(Newqueue, fun(_Msg, _From, _State) ->
				ok
			end),
			{ok, Mock} = gen_leader_mock:start(queue_manager),
			gen_leader_mock:expect_leader_call(Mock, fun(_Msg, _From, State, _Elec) ->
				{ok, Newqueue, State}
			end),
			State = Seedstate#state{monitors = Mons, oncall_pid = {"testagent", Agent}},
			{noreply, #state{monitors = Newmons} = Newstate} = handle_info({'DOWN', OncallRef, process, Agent, testdeath}, State),
			?assertMatch({"default_queue", _}, Newstate#state.queue_pid),
			?assertEqual(undefined, Newmons#monitors.oncall_pid),
			?assertNot(undefined =:= Newmons#monitors.queue_pid),
			?assertEqual(undefined, Newstate#state.oncall_pid),
			gen_leader_mock:stop(Mock)
		end}
	end,
	fun({Seedstate}) ->
		{"oncall agent dies with a ringing agent",
		fun() ->
			OncallRef = make_ref(),
			Ocagent = dead_spawn(),
			Ringref = make_ref(),
			{ok, Ragent} = agent:start(#agent{login = "ringagent", state = ringing, statedata = Seedstate#state.callrec}),
			Mons = #monitors{
				oncall_pid = OncallRef,
				ring_pid = Ringref
			},
			{ok, Newqueue} = gen_server_mock:new(),
			gen_server_mock:expect_call(Newqueue, fun(_Msg, _From, _State) ->
				ok
			end),
			gen_server_mock:expect_call(Newqueue, fun(_Msg, _From, _State) ->
				ok
			end),
			{ok, Mock} = gen_leader_mock:start(queue_manager),
			gen_leader_mock:expect_leader_call(Mock, fun(_Msg, _From, State, _Elec) ->
				{ok, Newqueue, State}
			end),
			State = Seedstate#state{monitors = Mons, oncall_pid = {"deadagent", Ocagent}, ring_pid = {"ringagent", Ragent}},
			{noreply, #state{monitors = Newmons} = Newstate} = handle_info({'DOWN', OncallRef, process, Ocagent, testdeath}, State),
			?assertMatch({"default_queue", _}, Newstate#state.queue_pid),
			?assertEqual(undefined, Newmons#monitors.oncall_pid),
			?assertNot(undefined =:= Newmons#monitors.queue_pid),
			?assertEqual(undefined, Newstate#state.oncall_pid),
			?assertEqual(undefined, Newmons#monitors.ring_pid),
			?assertEqual(undefined, Newstate#state.ring_pid),
			gen_leader_mock:stop(Mock)
		end}
	end]}}.
	
agent_interact_test_() ->
	util:start_testnode(),
	N = util:start_testnode(gen_media_agent_interact_tests),
	{spawn, N, {foreach,
	fun() ->
		Callrec = #call{id = "testcall", source = self(), client = #client{}},
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
			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Apid} = agent:start(Arec#agent{statedata = Callrec, state = ringing}),
			{ok, Tref} = timer:send_interval(1000, <<"timer">>),
			Mons = #monitors{ring_pid = make_ref()},
			State = #state{ring_pid = {"testagent", Apid}, ringout = Tref, callrec = Callrec, monitors = Mons},
			gen_event_mock:expect_event(cdr, fun({ringout, _Callrec, _Time, {undefined, "testagent"}}, _State) ->
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
			Newmons = Res#state.monitors,
			?assertEqual(undefined, Newmons#monitors.ring_pid),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({_Arec, Callrec}) ->
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
			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Apid} = agent:start(Arec#agent{state = ringing, statedata = Callrec}),
			Mons = #monitors{ring_pid = make_ref()},
			State = #state{ring_pid = {"testagent", Apid}, ringout = false, callrec = Callrec, monitors = Mons},
			gen_event_mock:expect_event(cdr, fun({ringout, _Callrec, _Time, {undefined, "testagent"}}, _State) -> ok end),
			Res = agent_interact(stop_ring, State),
			agent:stop(Apid),
			?assertEqual(undefined, Res#state.ring_pid),
			Newmons = Res#state.monitors,
			?assertEqual(undefined, Newmons#monitors.ring_pid),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({_Arec, Callrec}) ->
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
			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Apid} = agent:start(Arec#agent{state = oncall, statedata = Callrec}),
			Mons = #monitors{oncall_pid = make_ref()},
			State = #state{oncall_pid = {"testagent", Apid}, callrec = Callrec, monitors = Mons},
			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, "testagent"}, _State) -> ok end),
			Res = agent_interact(wrapup, State),
			agent:stop(Apid),
			?assertEqual(undefined, Res#state.oncall_pid),
			Newmons = Res#state.monitors,
			?assertEqual(undefined, Newmons#monitors.oncall_pid),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({Arec, Callrec}) ->
		{"hangup when both oncall and ring are pids",
		fun() ->
			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Oncall} = agent:start(Arec#agent{state = oncall, statedata = Callrec}),
			{ok, Ringing} = agent:start(Arec#agent{state = ringing, statedata = Callrec, login = "ringing"}),
			Mons = #monitors{ring_pid = make_ref(), oncall_pid = make_ref()},
			State = #state{oncall_pid = {"testagent", Oncall}, ring_pid = {"ring", Ringing}, callrec = Callrec, monitors = Mons},
			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, "testagent"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({hangup, _Callrec, _Time, undefined}, _State) -> ok end),
			Res = agent_interact(hangup, State),
			agent:stop(Oncall),
			agent:stop(Ringing),
			?assertEqual(undefined, Res#state.oncall_pid),
			?assertEqual(undefined, Res#state.ring_pid),
			Newmons = Res#state.monitors,
			?assertEqual(undefined, Newmons#monitors.oncall_pid),
			?assertEqual(undefined, Newmons#monitors.ring_pid),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({Arec, Callrec}) ->
		{"hang up when only oncall is a pid",
		fun() ->
			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Apid} = agent:start(Arec#agent{state = oncall, statedata = Callrec}),
			Mons = #monitors{oncall_pid = make_ref()},
			State = #state{oncall_pid = {"testagent", Apid}, callrec = Callrec, monitors = Mons},
			gen_event_mock:expect_event(cdr, fun({wrapup, _Callrec, _Time, "testagent"}, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({hangup, _Callrec, _Time, undefined}, _State) -> ok end),
			Res = agent_interact(hangup, State),
			agent:stop(Apid),
			?assertEqual(undefined, Res#state.oncall_pid),
			Newmons = Res#state.monitors,
			?assertEqual(undefined, Newmons#monitors.oncall_pid),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({Arec, Callrec}) ->
		{"hang up when only ringing is a pid",
		fun() ->
			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Apid} = agent:start(Arec#agent{state = ringing, statedata = Callrec}),
			Mons = #monitors{ring_pid = make_ref()},
			gen_event_mock:expect_event(cdr, fun({hangup, _Callrec, _Time, undefined}, _State) -> ok end),
			State = #state{ring_pid = {"testagent", Apid}, callrec = Callrec, monitors = Mons},
			Res = agent_interact(hangup, State),
			agent:stop(Apid),
			?assertEqual(undefined, Res#state.ring_pid),
			Newmons = Res#state.monitors,
			?assertEqual(undefined, Newmons#monitors.ring_pid),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({_Arec, Callrec}) ->
		{"hang up when only queue is a pid",
		fun() ->
			{ok, Qpid} = gen_server_mock:new(),
			gen_server_mock:expect_call(Qpid, fun({remove, _Incpid}, _From, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({hangup, _Callrec, _Time, undefined}, _State) -> ok end),
			Mons = #monitors{queue_pid = make_ref()},
			State = #state{queue_pid = {"testqueue", Qpid}, callrec = Callrec, monitors = Mons},
			#state{monitors = Newmons} = Res = agent_interact(hangup, State),
			?assertEqual("testqueue", Res#state.queue_pid),
			?assertEqual(undefined, Newmons#monitors.queue_pid),
			gen_server_mock:assert_expectations(Qpid),
			gen_server_mock:stop(Qpid),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({Arec, Callrec}) ->
		{"hang up when both queue and ring is a pid",
		fun() ->
			{ok, Qpid} = gen_server_mock:new(),
			gen_leader_mock:expect_cast(agent_manager, fun({update_skill_list, _, _}, _, _) -> ok end),
			{ok, Apid} = agent:start(Arec#agent{state = ringing, statedata = Callrec}),
			gen_server_mock:expect_call(Qpid, fun({remove, _Incpid}, _From, _State) -> ok end),
			gen_event_mock:expect_event(cdr, fun({hangup, _Callrec, _Time, undefined}, _State) -> ok end),
			Mons = #monitors{queue_pid = make_ref(), ring_pid = make_ref()},
			State = #state{queue_pid = {"testqueue", Qpid}, ring_pid = {"testagent", Apid}, callrec = Callrec, monitors = Mons},
			#state{monitors = Newmons} = Res = agent_interact(hangup, State),
			agent:stop(Apid),
			?assertEqual("testqueue", Res#state.queue_pid),
			?assertEqual(undefined, Newmons#monitors.queue_pid),
			?assertEqual(undefined, Res#state.ring_pid),
			?assertEqual(undefined, Newmons#monitors.ring_pid),
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
			Mon = #monitors{ring_pid = make_ref()},
			State = #state{ring_pid = {"testagent", spawn(fun() -> ok end)}, callrec = Callrec, monitors = Mon},
			gen_event_mock:expect_event(cdr, fun({ringout, _Callrec, _Time, {undefined, "testagent"}}, _) -> ok end),
			Res = agent_interact(stop_ring, State),
			?assertEqual(undefined, Res#state.ring_pid),
			gen_event_mock:assert_expectations(cdr)
		end}
	end]}}.

outgoing_test_() ->
	util:start_testnode(),
	N = util:start_testnode(gen_media_outgoing_tests),
	{spawn, N, {foreach,
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
			gen_event_mock:expect_event(cdr, fun({oncall, _Callrec, _Time, "testagent"}, _State) -> ok end),
			State = #state{callrec = #call{id = "testcall", source = self()}},
			{ok, Res} = outgoing({outbound, "testagent", "newsubstate"}, State),
			?assertEqual({"testagent", Apid}, Res#state.oncall_pid),
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
			gen_event_mock:expect_event(cdr, fun({oncall, _Callrec, _Time, "testagent"}, _State) -> ok end),
			Callrec = #call{id = "testcall", source = self()},
			State = #state{},
			{ok, Res} = outgoing({outbound, "testagent", Callrec, "newsubstate"}, State),
			?assertEqual({"testagent", Apid}, Res#state.oncall_pid),
			?assertEqual(Callrec, Res#state.callrec),
			?assertEqual("newsubstate", Res#state.substate),
			gen_leader_mock:assert_expectations(Ammock),
			gen_event_mock:assert_expectations(cdr)
		end}
	end,
	fun({_Apid, Ammock}) ->
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
	end]}}.

priv_queue_test_() ->
	util:start_testnode(),
	N = util:start_testnode(gen_media_priv_queue_tests),
	{spawn, N, {foreach,
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
			gen_server_mock:expect_call(Qpid, fun({add, 40, _Inpid, _Callrec}, _From, _State) -> ok end),
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
			gen_server_mock:expect_call(Qpid, fun({add, 40, _Inpid, _Callrec}, _From, _State) -> ok end),
			?assertEqual({default, Qpid}, priv_queue("testqueue", Callrec, true)),
			Mocks()
		end}
	end]}}.

-endif.
