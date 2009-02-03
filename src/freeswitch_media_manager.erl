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

%% @doc The controlling module for connection CPX to a freeswitch installation.  There are 2 primary requirements for this work:  
%% the freeswitch installaction must have mod_erlang installed and active, and the freeswitch dialplan must add the following
%% variables to the call data:
%% <dl>
%% <dt>queue</dt><dd>The name of the queue as entered into the queue_manager</dd>
%% <dt>brand</dt><dd>As the combined brand id</dd>
%% </dl>
%% Primary job of this module is to listen to freeswitch for events, and shove those events to the appriate child process.
%% @see freeswitch_media

-module(freeswitch_media_manager).
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
	start_link/2, 
	start/2, 
	ring_agent/2,
	queued_call/2,
	find_queued/1,
	unqueue_call/1,
	agent_call/2,
	find_agent/1,
	unagent_call/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	nodename :: atom(),
	freeswitch_c_pid :: pid(),
	queued_at = dict:new(),
	agent_at = dict:new(),
	domain
	}).
	
% watched calls structure:
% callid, pid of gen_server handling it

% notes for tomorrow:  set up a process per call id to watch/process that call id

%%====================================================================
%% API
%%====================================================================
%% @doc Start the media manager unlinked to the parent process.  `Nodename' is the name of the C node for mod_erlang in freeswitch; 
%% `Domain' is the domain to ring to sip agents.
%% @clear
% Domain is there to help ring agents.
start(Nodename, Domain) -> 
	gen_server:start({local, ?MODULE}, ?MODULE, [Nodename, Domain], []).
%% @doc Start the media manager linked to the parent process.  `Nodename' is the name of the C node for mod_erlang in freeswitch; 
%% `Domain' is the domain to ring to sip agents.
%% @clear
start_link(Nodename, Domain) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Nodename, Domain], []).

%% @doc Inform the primary manager that call of id `Callid' has been placed in the queue at pid `Qpid'.
queued_call(Callid, Qpid) -> 
	gen_server:cast(?MODULE, {queued_call, Callid, Qpid}).

%% @doc return the pid of the queue the given call id of `Callid' is placed in.
find_queued(Callid) -> 
	gen_server:call(?MODULE, {find_queued, Callid}).

%% @doc Notifies the manager that the call id `Callid' is no longer queued anywhere.
unqueue_call(Callid) ->
	gen_server:cast(?MODULE, {unqueue_call, Callid}).

%% @doc Notifies the manager that `Callid' is now being handled by agent at pid `Apid'.
agent_call(Callid, Apid) -> 
	gen_server:cast(?MODULE, {agent_call, Callid, Apid}).

%% @doc Returns the agent pid that is associated with `Callid' if any.
find_agent(Callid) -> 
	gen_server:call(?MODULE, {find_agent, Callid}).

%% @doc Notifies the manager that the call identified by `Callid' is free from any agents.
unagent_call(Callid) -> 
	gen_server:cast(?MODULE, {unagent_call, Callid}).
	
%%====================================================================
%% gen_server callbacks
%%====================================================================
%% @private
init([Nodename, Domain]) -> 
	?CONSOLE("starting...", []),
	process_flag(trap_exit, true),
	Self = self(),
	% TODO we don't need to register this listener anymore.
	_Lpid = spawn(fun() -> 
		{freeswitchnode, Nodename} ! register_event_handler,
		receive
			ok ->
				Self ! {register_event_handler, {ok, self()}},
				listener(Nodename);
			{error, Reason} -> 
				Self ! {register_event_handler, {error, Reason}}
		after ?TIMEOUT -> 
			Self ! {register_event_handler, timeout}
		end
	end),
	T = freeswitch:event(Nodename, [channel_create, channel_answer, channel_destroy, channel_hangup, custom, 'fifo::info']),
	?CONSOLE("Attempted to start events:  ~p", [T]),
    {ok, #state{nodename=Nodename, domain=Domain}}.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
handle_call({ring_agent, AgentPid, Call}, _From, #state{queued_at = Dict} = State) ->
	% TODO test functionality
	case dict:find(Call#call.id, Dict) of
		{ok, Queue} -> 
			?CONSOLE("As far as we know this call is queued at ~p", [Queue]),
			AgentRec = agent:dump_state(AgentPid),
			Args = "{dstchan=" ++ Call#call.id ++ ",agent="++ AgentRec#agent.login ++"}sofia/default/" ++ AgentRec#agent.login ++ "%" ++ State#state.domain ++ " '&erlang("++atom_to_list(?MODULE)++":! "++atom_to_list(node())++")'",
				X = freeswitch:api(State#state.nodename, originate, Args),
				?CONSOLE("Bgapi call res:  ~p;  With args: ~p", [X, Args]),
				{reply, agent:set_state(AgentPid, ringing, Call), State};
		error -> 
			{reply, {error, not_queued}, State}
	end;
handle_call({find_queued, Callid}, _From, #state{queued_at = Dict} = State) -> 
	case dict:find(Callid, Dict) of
		error -> 
			{reply, {error, not_queued}, State};
		Else -> 
			{reply, Else, State}
	end;
handle_call({find_agent, Callid}, _From, #state{agent_at = Dict} = State) -> 
	case dict:find(Callid, Dict) of 
		error -> 
			?CONSOLE("No agent handling this call", []),
			{reply, {error, no_agent}, State};
		Else -> 
			{reply, Else, State}
	end;
handle_call(Request, _From, State) ->
	?CONSOLE("Sudden call:  ~p", [Request]),
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast({queued_call, Callid, Qpid}, #state{queued_at = Dict} = State) -> 
	?CONSOLE("noting that ~p was put in ~p", [Callid, Qpid]),
	NewDict = dict:store(Callid, Qpid, Dict),
	{noreply, State#state{queued_at = NewDict}};
handle_cast({unqueue_call, Callid}, #state{queued_at = Dict} = State) -> 
	?CONSOLE("Noting that ~p was unqueued.", [Callid]),
	NewDict = dict:erase(Callid, Dict),
	{noreply, State#state{queued_at = NewDict}};
handle_cast({agent_call, Callid, Apid}, #state{agent_at = Dict} = State) -> 
	?CONSOLE("coupling agent at ~p to call ~p", [Apid, Callid]),
	NewDict = dict:store(Callid, Apid, Dict),
	{noreply, State#state{agent_at = NewDict}};
handle_cast({unagent_call, Callid}, #state{agent_at = Dict} = State) ->
	?CONSOLE("Decoupling agents from ~p", [Callid]),
	NewDict = dict:erase(Callid, Dict),
	{noreply, State#state{agent_at = NewDict}};
handle_cast(_Msg, State) ->
	%?CONSOLE("Cast:  ~p", [Msg]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info({new_pid, Ref, From}, State) ->
	{ok, Pid} = freeswitch_media:start(State#state.nodename),
	From ! {Ref, Pid},
	{noreply, State};
handle_info({'EXIT', Pid, normal}, State) -> 
	?CONSOLE("Trapping exit from ~p because of ~p.", [Pid, normal]),
	{noreply, State};
handle_info({'EXIT', Pid, shutdown}, State) ->
	?CONSOLE("Trapping exit from ~p due to ~p.", [Pid, shutdown]),
	{noreply, State};
handle_info({'EXIT', Pid, Reason}, State) -> 
	?CONSOLE("Bad exit ~p, do clean up for ~p", [Reason, Pid]),
	% TODO find out what clean up needs to be done and do it.
	{noreply, State};
handle_info(Info, State) ->
	?CONSOLE("Sudden info:  ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @private
terminate(_Reason, _State) ->
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

% TODO add supporting code so fmm can get the right media to ring it.
%% @doc Ring `AgentPid' with `Call'.
ring_agent(AgentPid, Call) -> 
	gen_server:call(?MODULE, {ring_agent, AgentPid, Call}).

%% @private
% listens for info from the freeswitch c node.
listener(Node) ->
	receive
		{event, _Event} ->
			?CONSOLE("recieved event from c node.", []),
			%gen_server:cast(?MODULE, Event), 
			listener(Node);
		{nodedown, Node} -> 
			gen_server:cast(?MODULE, nodedown);
		 Otherwise -> 
			 ?CONSOLE("Uncertain reply received by the fmm listener:  ~p", [Otherwise]),
			 listener(Node)
	end.
