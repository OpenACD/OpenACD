%%%-------------------------------------------------------------------
%%% File          : gen_media.erl
%%% Author        : Micah Warren
%%% Organization  : SpiceCSM
%%% Project       : spice-telephony
%%% Description   : 
%%%
%%% Created       :  5/11/09
%%%-------------------------------------------------------------------
-module(gen_media).
-author(micahw).

-behaviour(gen_server).

%% API
-export([
	behaviour_info/1
	start_link/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	callback,
	substate,
	callrec,
	agent_pid
}).

-define(MEDIA_ACTIONS, [ring_agent, get_call, start_cook, voicemail, announce, stop_cook]).

behaviour_info(callbacks) ->
	GS = gen_server:behavior_info(callbacks),
	lists:append([{handle_ring, 3}, {handle_answer, 3}, {handle_voicemail, 1}, {handle_annouce, 2}], GS);
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
	gen_server:cast(Genmedia, '$gen_media_stop_ring').

oncall(Genmedia) ->
	gen_server:cast(Genmedia, '$gen_media_agent_oncall').
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

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Callback, Args]) ->
	{ok, {Substate, Callrec}} = apply(Callback, init, Args),
    {ok, #state{callback = Callback, substate = Substate, callrec = Callrec}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call('$gen_media_get_call', From, State) ->
	{reply, State#state.callrec, State};
handle_call({'$gen_media_ring', Agent, Qcall, Timeout}, From, State) ->
	case agent:set_state(Agent, ringing, Call#call{cook=QCall#queued_call.cook}) of
		ok ->
			case apply(State#state.callback, handle_ring, [Agent]) of
				{ok, Substate} ->
					{reply, ok, State#state{substate = Substate, agent_pid = Agent}};
				{invalid, Substate} ->
					{reply, invalid, State#state{substate = Substate}}
			end;
		Else ->
			?INFO("Agent ringing response:  ~p", [Else]),
			{reply, invalid, State};
handle_call({'$gen_media_annouce', Annouce}, From, State) ->
	{ok, Substate} = apply(State#state.callback, handle_annouce, [Annouce]),
	{reply, ok, State#state{substate = Substate}};
handle_call('$gen_media_voicemail', From, State) ->
	{ok, Substate} = apply(State#state.callback, handle_voicemail, []),
	{reply, ok, State#state{substate = Substate}};
handle_call(Request, From, State) ->
	case apply(State#state.callback, handle_call, [Request, From, State#state.substate]) of
		{reply, Reply, NewState} ->
			{reply, Reply, State#state{substate = NewState}};
		{reply, Reply, Newstate, Timeout} ->
			{reply, Reply, State#state{substate = Newstate}, Timeout};
		{reply, Reply, NewState, hibernate} ->
			{reply, Reply, State#state{substate = NewState}, hibernate} ->
		{noreply, NewState} ->
			{noreply, State#state{substate = NewState}};
		{noreply, NewState, Timeout} ->
			{noreply, State#state{substate = NewState}, Timeout};
		{noreply, NewState, hibernate} ->
			{noreply, State#state{substate = NewState}, hibernate};
		{stop, Reason, Reply, NewState} ->
			{stop, Reason, Reply, State#state{substate = NewState}};
		{stop, Reason, NewState} ->
			{stop, Reason, State#state{substate = NewState}}
	end.
	
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast('$gen_media_agent_oncall', State) ->
	case State#state.agent_pid of
		undefined ->
			?WARNING("No agent to set state to",[]),
			{noreply, State};
		Apid when is_pid(Apid) ->
			case apply(State#state.callback, handle_answer, [State#state.callrec, Apid, State#state.substate]) of
				{ok, Newstate} ->
					agent:set_state(State#state.agent_pid, oncall, State#state.callrec),
					{noreply, State#state{substate = Newstate}};
				{error, Reason, Newstate} ->
					?WARNING("Counld not set agent to on call due to ~p", [Reason]),
					{noreply, State#state{substate = Newstate}}
			end
	end;
handle_cast(Msg, State) ->
	case apply(State#state.callback, handle_cast, [Msg, State#state.substate]) of
		{noreply, NewState} ->
			{noreply, State#state{substate = NewState}};
		{noreply, NewState, Timeout} ->
			{noreply, State#state{substate = NewState}, Timeout};
		{noreply, NewState, hibernate} ->
			{noreply, State#state{substate = NewState}, hibernate};
		{stop, Reason, NewState} ->
			{stop, Reason, State#state{substate = NewState}}
	end.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) ->
	case apply(State#state.callback, handle_info, [Info, State#state.substate]) of
		{noreply, NewState} ->
			{noreply, State#state{substate = NewState}};
		{noreply, NewState,  Timeout} ->
			{noreply, State#state{substate = NewState}, Timeout};
		{noreply, NewState, hibernate} ->
			{noreply, State#state{substate = NewState}, hibernate};
		{stop, Reason, NewState} ->
			{stop, Reason, State#state{substate = NewState}}
	end.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, State) ->
	apply(State#state.callback, terminate, [Reason, State#state.substate]).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
	{ok, Newsub} = apply(State#state.callback, code_change, [OldVsn, State#state.substate, Extra]),
    {ok, State#state{substate = Newsub}}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
