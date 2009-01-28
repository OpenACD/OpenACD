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

%%%-------------------------------------------------------------------
%%% File          : freeswitch_media_manager.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  11/18/08
%%%-------------------------------------------------------------------

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
	listener/1,
	ring_agent/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	nodename :: atom(),
	freeswitch_c_pid :: pid(),
	watched_calls, % ets table of the call id's already queued.
	domain
	}).
	
% watched calls structure:
% callid, pid of gen_server handling it

% notes for tomorrow:  set up a process per call id to watch/process that call id

%%====================================================================
%% API
%%====================================================================
%% @doc Nodename is the name of the C node for mod_erlang in freeswitch.
start(Nodename, Domain) -> 
	gen_server:start({local, ?MODULE}, ?MODULE, [Nodename, Domain], []).
%% @doc Nodename is the name of the C node for mod_erlang in freeswitch.
start_link(Nodename, Domain) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Nodename, Domain], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Nodename, Domain]) -> 
	io:format("freeswitch media manager starting...~n"),
	process_flag(trap_exit, true),
	Self = self(),
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
	io:format("Attempted to start events in ffm's init:  ~p~n", [T]),
    {ok, #state{nodename=Nodename, watched_calls = ets:new(watched_calls, [named_table]), domain=Domain}}.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({ring_agent, AgentPid, Call}, _From, State) ->
	%% @todo A real media manager would do something substantial here, like try to
	%% send SIP to an agent's phone or whatever. We should do something about
	%% ringout here though...
	AgentRec = agent:dump_state(AgentPid),
	Args = "{dstchan=" ++ Call#call.id ++ "}sofia/default/" ++ AgentRec#agent.login ++ "%" ++ State#state.domain ++ " &park()",
	X = freeswitch:bgapi(State#state.nodename, originate, Args),
	io:format("Bgapi call res:  ~p~nWith args: ~p~n", [X, Args]),
	{reply, agent:set_state(AgentPid, ringing, Call), State};
handle_call(Request, _From, State) ->
	io:format("Sudden call at fmm:  ~p~n", [Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast(Msg, State) ->
	io:format("freeswitch media manager got cast:  ~p~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info({new_pid, Ref, From}, State) ->
%	io:format("Starting new freeswitch media...~n"),
	{ok, Pid} = freeswitch_media:start(),
	From ! {Ref, Pid},
	{noreply, State};
handle_info({register_event_handler, {ok, Pid}}, State) -> 
	{noreply, State#state{freeswitch_c_pid = Pid}};
handle_info({'EXIT', Pid, normal}, State) -> 
	io:format("Trapping exit from ~p because of ~p.~n", [Pid, normal]),
	{noreply, State};
handle_info({'EXIT', Pid, _Reason}, State) -> 
	io:format("Bad exit, do clean up for ~p~n", [Pid]),
	{noreply, State};
handle_info(Info, State) ->
	io:format("Sudden info at the fmm:  ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

ring_agent(AgentPid, Call) -> 
	gen_server:call(?MODULE, {ring_agent, AgentPid, Call}).
	
listener(Node) ->
	receive
		{event, Event} ->
			gen_server:cast(?MODULE, Event), 
			listener(Node);
		{nodedown, Node} -> 
			gen_server:cast(?MODULE, nodedown);
		 Otherwise -> 
			 io:format("Uncertain reply received by the fmm listener:  ~p~n", [Otherwise]),
			 listener(Node)
	end.
