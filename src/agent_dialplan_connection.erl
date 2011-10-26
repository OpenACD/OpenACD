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
	listener :: pid(),
	suicide_time :: integer(),
	suicide_timer
}).

%% API
-export([start/2, start/3, start_link/2, start_link/3, logout/1,
	go_released/1, go_available/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

-type(security_atom() :: 'agent' | 'supervisor' | 'admin').

%% API
-spec(start/2 :: (AgentRec :: #agent{}, Security :: security_atom()) -> {'ok', pid()}).
start(AgentRec, Security) ->
	start(AgentRec, Security, undefined).

-spec(start/3 :: (AgentRec :: #agent{}, Security :: security_atom(), SelfKillTime :: 'undefined' | integer()) -> {'ok', pid()}).
start(AgentRec, Security, SelfKillTime) ->
	gen_server:start(?MODULE, [AgentRec, Security, SelfKillTime], []).

-spec(start_link/2 :: (AgentRec :: #agent{}, Security :: 'agent' | 'supervisor' | 'admin') -> {'ok', pid()}).
start_link(AgentRec, Security) ->
	start_link(AgentRec, Security, undefined).

-spec(start_link/3 :: (AgentRec :: #agent{}, Security :: security_atom(), SelfKillTime :: 'undefined' | integer()) -> {'ok', pid()}).
start_link(AgentRec, Security, SelfKillTime) ->
	gen_server:start_link(?MODULE, [AgentRec, Security, SelfKillTime], []).

-spec(logout/1 :: (Pid :: pid()) -> 'ok').
logout(Pid) ->
	gen_server:call(Pid, logout).

-spec(go_released/1 :: (Pid :: pid()) -> 'ok' | 'invalid').
go_released(Pid) ->
	gen_server:call(Pid, go_released).

-spec(go_available/1 :: (Pid :: pid()) -> 'ok' | 'invalid').
go_available(Pid) ->
	gen_server:call(Pid, go_available).

%% gen_server API
init([AgentRec, _Security, SelfKillTime]) -> % TODO if not used, why is it here?
	process_flag(trap_exit, true),
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
			{ok, #state{
				agent_fsm = Apid,
				listener = whereis(agent_dialplan_listener),
				suicide_time = SelfKillTime
			}}
	end.

handle_call(logout, {From, _Ref}, #state{listener = From} = State) ->
	{stop, normal, ok, State};
handle_call(go_released, {From, _Ref}, #state{listener = From} = State) ->
	Res = agent:set_state(State#state.agent_fsm, released, default),
	{reply, Res, State};
handle_call(go_available, {From, _Ref}, #state{listener = From} = State) ->
	Res = agent:set_state(State#state.agent_fsm, idle),
	{reply, Res, State};
handle_call({set_endpoint, MidEndpoint}, _From, #state{agent_fsm = Apid} = State) ->
	% TODO flesh this out so more than just perfect data works.
	{EndpointType, Endpointdata} = case MidEndpoint of
		{_Type, _Data} -> MidEndpoint
	end,
	Reply = agent:set_endpoint(Apid, EndpointType, Endpointdata, transient),
	{reply, Reply, State};
handle_call(Request, From, State) ->
	?DEBUG("Call from ~p:  ~p", [From, Request]),
	{reply, {unknown_call, Request}, State}.

handle_cast({change_state, StateName, _}, State) -> 
	handle_cast({change_state, StateName}, State);

handle_cast({change_state, StateName}, State) when StateName =:= released; StateName =:= wrapup ->
	KillAfter = State#state.suicide_time,
	KillTimer = case {State#state.suicide_timer, KillAfter} of
		{undefined, undefined} ->
			undefined;
		{undefined, _} ->
			Self = self(),
			erlang:send_after(KillAfter * 1000 * 60, Self, suicide_pact);
		{OldTimer, _} ->
			OldTimer
	end,
	{noreply, State#state{suicide_timer = KillTimer}};
handle_cast({change_state, _}, State) ->
	case State#state.suicide_timer of
		undefined -> ok;
		Else -> erlang:cancel_timer(Else)
	end,
	{noreply, State#state{suicide_timer = undefined}};

handle_cast(Msg, State) ->
	?DEBUG("Cast ~p", [Msg]),
	{noreply, State}.

handle_info(suicide_pact, #state{suicide_timer = undefined} = State) ->
	{noreply, State};
handle_info(suicide_pact, State) ->
	{stop, unavailble_timeout, State};
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
	?NOTICE("Terminated: ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

