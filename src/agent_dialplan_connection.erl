%% "The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%%
%%	The Original Code is OpenACD.
%%
%%	The Initial Developers of the Original Code is 
%%	Andrew Thompson.
%%
%%	All portions of the code written by the Initial Developers are Copyright
%%	(c) 2008-2009 Andrew Thompson.
%%	All Rights Reserved.
%%
%%	Contributor(s):
%%
%%	Andrew Thompson <andrew at hijacked dot us>

%% @doc Connection module for dialplan agents.

-module(agent_dialplan_connection).

-behaviour(gen_server).

-include("log.hrl").
-include("call.hrl").
%-include("queue.hrl").
-include("agent.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {
	agent_fsm :: pid(),
	listener :: pid()
}).

%% API
-export([start/2, start_link/2, logout/1, go_released/1, go_available/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% API
start(AgentRec, Security) ->
	gen_server:start({local, ?MODULE}, ?MODULE, [AgentRec, Security], []).

start_link(AgentRec, Security) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [AgentRec, Security], []).

logout(Pid) ->
	gen_server:call(Pid, logout).

go_released(Pid) ->
	gen_server:call(Pid, go_released).

go_available(Pid) ->
	gen_server:call(Pid, go_available).

%% gen_server API
init([AgentRec, Security]) ->
	case agent_manager:start_agent(AgentRec) of
		{ok, Apid} ->
			ok;
		{exists, Apid} ->
			ok
	end,
	case agent:set_connection(Apid, self()) of
		error ->
			{stop, "Agent is already logged in"};
		_Else ->
			{ok, #state{agent_fsm = Apid, listener = whereis(agent_dialplan_listener)}}
	end.

handle_call(logout, {From, _Ref}, #state{listener = From} = State) ->
	{stop, normal, ok, State};
handle_call(go_released, {From, _Ref}, #state{listener = From} = State) ->
	Res = agent:set_state(State#state.agent_fsm, {released, default}),
	{reply, Res, State};
handle_call(go_available, {From, _Ref}, #state{listener = From} = State) ->
	Res = agent:set_state(State#state.agent_fsm, idle),
	{reply, Res, State};
handle_call(Request, From, State) ->
	?DEBUG("Call from ~p:  ~p", [From, Request]),
	{reply, {unknown_call, Request}, State}.

handle_cast(Msg, State) ->
	%% TODO - we need to catch change_state events from the agent FSM here
	%% TODO - we need to have some sort of timeout to do an auto-logoff
	?DEBUG("Cast ~p", [Msg]),
	{noreply, State}.

handle_info({'EXIT', Pid, Reason}, #state{listener = Pid} = State) ->
	?WARNING("The listener at ~w died due to ~p", [Pid, Reason]),
	{stop, Reason, State};
handle_info({'EXIT', Agent, Reason}, #state{agent_fsm = Agent} = State) ->
	?INFO("dying along with agent FSM pid", []),
	{stop, Reason, State};
handle_info(Info, State) ->
	?DEBUG("Info:  ~p", [Info]),
	{noreply, State}.

terminate(Reason, _State) ->
	?NOTICE("Terminating dirty:  ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

