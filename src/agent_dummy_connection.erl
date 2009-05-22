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

%% @doc The connection handler that communicates with a client UI; in this case it's a dummy.

-module(agent_dummy_connection).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-export([start/0, start/1, start_link/0, start_link/1, start_x/1, start_x/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
		code_change/3]).

-define(Major, 2).
-define(Minor, 0).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").

-record(state, {
	ringing = random :: pos_integer() | 'random',
	ringtimer :: any(),
	oncall = random :: pos_integer() | 'random',
	calltimer :: any(),
	wrapup = random :: pos_integer() | 'random',
	wrapuptimer :: any(),
	maxcalls = unlimited :: pos_integer() | 'unlimited',
	call :: #call{} | 'undefined',
	agent_fsm :: pid()
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

start_x(N) ->
	start_x(N, []).

start_x(N, Args) ->
	F = fun(_I) ->
		start(Args)
	end,
	lists:foreach(F, lists:seq(1, N)).

start() ->
	start([]).

start(Args) ->
	gen_server:start(?MODULE, [Args], []).

start_link() ->
	start_link([]).

start_link(Args) ->
	gen_server:start_link(?MODULE, [Args], []).

init([Args]) ->
	crypto:start(),
	{ok, Pid} = agent_manager:start_agent(#agent{
			login = proplists:get_value(login, Args, lists:flatten(io_lib:format("~p", [make_ref()]))),
			profile = proplists:get_value(profile, Args, "Default"),
			skills = proplists:get_value(skills, Args, [english, '_agent', '_node'])
		}),
	agent:set_state(Pid, idle),
	agent:set_connection(Pid, self()),
	?NOTICE("Created new dummy agent connection", []),
	{ok, #state{
		agent_fsm = Pid,
		ringing = proplists:get_value(ringing, Args, random),
		oncall = proplists:get_value(oncall, Args, random),
		wrapup = proplists:get_value(wrapup, Args, random),
		maxcalls = proplists:get_value(maxcalls, Args, unlimited)}}.

handle_call(Request, _From, State) ->
	{reply, {unknown_call, Request}, State}.

handle_cast({change_state, ringing, #call{} = Call}, State) ->
	Time = get_time(State#state.ringing),
	?INFO("answering call after ~p", [Time]),
	Tref = timer:send_after(Time, answer),
	{noreply, State#state{ringtimer = Tref, call = Call}};
handle_cast({change_state, oncall, #call{} = Call}, State) ->
	timer:cancel(State#state.ringtimer),
	Time = get_time(State#state.oncall),
	?INFO("hanging up call after ~p", [Time]),
	Tref = timer:send_after(Time, hangup),
	{noreply, State#state{ringtimer = undefined, calltimer = Tref, call = Call}};
handle_cast({change_state, wrapup, #call{} = Call}, State) ->
	timer:cancel(State#state.calltimer),
	Time = get_time(State#state.wrapup),
	?INFO("ending wrapup after ~p", [Time]),
	Tref = timer:send_after(Time, endwrapup),
	{noreply, State#state{calltimer = undefined, wrapuptimer = Tref, call = Call}};
handle_cast({change_state, _AgState, _Data}, State) ->
	{noreply, State};
handle_cast({change_state, idle}, State) ->
	?INFO("going idle", []),
	timer:cancel(State#state.ringtimer),
	timer:cancel(State#state.calltimer),
	timer:cancel(State#state.wrapuptimer),
	{noreply, State#state{ringtimer = undefined, calltimer = undefined, wrapuptimer = undefined, call = undefined}};
handle_cast({change_state, _AgState}, State) ->
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(answer, #state{call = Call} = State) ->
	?INFO("time to answer", []),
	%gen_server:call(Call#call.source, unqueue),
	case Call#call.ring_path of
		inband ->
			agent:set_state(State#state.agent_fsm, oncall);
		outband ->
			agent:set_state(State#state.agent_fsm, oncall, State#state.call)
	end,
	gen_server:cast(Call#call.cook, remove_from_queue),
	{noreply, State};
handle_info(hangup, #state{call = Call} = State) ->
	?INFO("time to hangup", []),
	% TODO the media needs to implement a hangup message
	case Call#call.ring_path of
		inband ->
			agent:set_state(State#state.agent_fsm, wrapup);
		outband ->
			agent:set_state(State#state.agent_fsm, wrapup, State#state.call)
	end,
	{noreply, State};
handle_info(endwrapup, State) ->
	?INFO("time to endwrapup", []),
	agent:set_state(State#state.agent_fsm, idle),
	case State#state.maxcalls of
		unlimited -> {noreply, State};
		SomeNumber when (SomeNumber - 1) =< 0 ->
			{stop, shutdown, State};
		SomeNumber ->
			{noreply, State#state{maxcalls = SomeNumber - 1}}
	end;
handle_info(_Info, State) ->
	{noreply, State}.

get_time(T) ->
	case T of
		random -> crypto:rand_uniform(0, 300);% * 1000;
		Else -> Else
	end.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

