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

%% @doc A dummy media process designed to aid testing by mimicking a real call media process.

-module(dummy_media).
-author(micahw).

-behaviour(gen_server).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("queue.hrl").
-include("call.hrl").
-include("agent.hrl").

-define(MEDIA_ACTIONS, [ring_agent, get_call, start_cook, voicemail, announce, stop_cook]).

%% API
-export([
	start_link/1,
	start_link/2,
	start/1,
	start/2,
	ring_agent/2,
	stop/1,
	stop/2,
	set_mode/3,
	set_skills/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	callrec = #call{},
	mode = success :: 'success' | 'failure' | 'fail_once',
	fail = []
	}).

%%====================================================================
%% API
%%====================================================================
start_link(Callid) ->
	start_link(Callid, success).

start_link(Callid, success) ->
    gen_server:start_link(?MODULE, [Callid, success], []);
start_link(Callid, failure) ->
	gen_server:start_link(?MODULE, [Callid, failure], []).

start(Callid) ->
		start(Callid, success).

start(Callid, success) ->
	gen_server:start(?MODULE, [Callid, success], []);
start(Callid, failure) ->
	gen_server:start(?MODULE, [Callid, failure], []).

stop(Pid) -> 
	stop(Pid, normal).

stop(Pid, Reason) ->
	gen_server:call(Pid, {stop, Reason}).

ring_agent(Pid, Agentpid) when is_pid(Pid), is_pid(Agentpid) -> 
	gen_server:call(Pid, {ring_agent, Agentpid}).
	
set_mode(Pid, Action, Mode) ->
	gen_server:call(Pid, {set_action, Action, Mode}).

set_skills(Pid, Skills) ->
	gen_server:call(Pid, {set_skills, Skills}).
	
%set_mode(Pid, success) when is_pid(Pid) -> 
%	gen_server:call(Pid, set_success);
%set_mode(Pid, failure) when is_pid(Pid) -> 
%	gen_server:call(Pid, set_failure).
	
%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Callid, Mode]) ->
	process_flag(trap_exit, true),
	{ok, #state{callrec = #call{id=Callid, source=self()}, mode = Mode}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------

handle_call(set_success, _From, State) -> 
	{reply, ok, State#state{mode = success}};
handle_call(set_failure, _From, State) -> 
	{reply, ok, State#state{mode = failure}};
handle_call(set_fail_once, _From, State) ->
	{reply, ok, State#state{mode = fail_once}};
handle_call({set_action, Action, fail}, _From, #state{fail = Curfail} = State) ->
	case lists:member(Action, Curfail) of
		true ->
			{reply, ok, State};
		false ->
			Newfail = [Action | Curfail],
			{reply, ok, State#state{fail = Newfail}}
	end;
handle_call({set_action, Action, success}, _From, #state{fail = Curfail} = State) ->
	F = fun(E) ->
		E =/= Action
	end,
	Newfail = lists:filter(F, Curfail),
	{reply, ok, State#state{fail = Newfail}};
handle_call({set_skills, Skills}, _From, #state{callrec = Call} = State) ->
	{reply, ok, State#state{callrec = Call#call{skills=Skills}}};
handle_call({ring_agent, AgentPid, _Queuedcall}, _From, #state{fail = Fail} = State) -> 
	case State#state.mode of
		success -> 
			case lists:member(ring_agent, Fail) of
				true ->
					{reply, invalid, State};
				false ->
					{reply, agent:set_state(AgentPid, ringing, State#state.callrec), State}
			end;
		failure -> 
			{reply, invalid, State};
		fail_once ->
			{reply, invalid, State#state{mode = success}}
	end;
handle_call(get_call, _From, State) -> 
	case State#state.mode of
		success -> 
			{reply, State#state.callrec, State};
		failure -> 
			{reply, invalid, State};
		fail_once ->
			{reply, invalid, State#state{mode = success}}
	end;
handle_call({start_cook, Recipe, Queuename}, _From, #state{callrec = Call} = State) -> 
	case State#state.mode of
		failure -> 
			{reply, invalid, State};
		success -> 
			{ok, Pid} = cook:start_link(self(), Recipe, Queuename),
			NewCall = Call#call{cook = Pid},
			{reply, ok, State#state{callrec = NewCall}};
		fail_once ->
			{reply, invalid, State#state{mode = success}}
	end;
handle_call({stop, Reason}, _From, State) ->
	{stop, Reason, ok, State};
handle_call(stop_cook, _From, #state{callrec = Call} = State) -> 
	case State#state.mode of
		success -> 
			case Call#call.cook of
				undefined -> 
					{reply, ok, State};
				Cookpid when is_pid(Cookpid) ->
					Cookres = cook:stop(Cookpid),
					NewCall = Call#call{cook = undefined},
					{reply, Cookres, State#state{callrec = NewCall}}
			end;
		failure -> 
			{reply, invalid, State};
		fail_once ->
			{reply, invalid, State#state{mode = success}}
	end;
handle_call(voicemail, _From, State) ->
	case State#state.mode of
		success ->
			{reply, ok, State};
		failure ->
			{reply, invalid, State};
		fail_once ->
			{reply, invalid, State#state{mode = success}}
	end;
handle_call({announce, _Args}, _From, State) ->
	case State#state.mode of
		success -> 
			{reply, ok, State};
		failure ->
			{reply, invalid, State};
		fail_once ->
			{reply, invalid, State#state{mode = success}}
	end.
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) ->
	?CONSOLE("Info: ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-ifdef(EUNIT).

dummy_test_() -> 
	[
		{
			"Simple start",
			fun() -> 
				?assertMatch({ok, _Pid}, dummy_media:start("testcall"))
			end
		},
		{
			"Set agent ringing when set to success",
			fun() -> 
				{ok, Agentpid} = agent:start(#agent{login="testagent"}),
				agent:set_state(Agentpid, idle),
				{ok, Dummypid} = dummy_media:start("testcall"),
				?assertMatch(ok, gen_server:call(Dummypid, {ring_agent, Agentpid, #queued_call{media=Dummypid, id="testcall"}}))
			end
		},
		{
			"Set agent ringing when set to failure",
			fun() -> 
				{ok, Agentpid} = agent:start(#agent{login="testagent"}),
				agent:set_state(Agentpid, idle),
				{ok, Dummypid} = dummy_media:start("testcall", failure),
				?assertMatch(invalid, gen_server:call(Dummypid, {ring_agent, Agentpid, #queued_call{media=Dummypid, id="testcall"}}))
			end
		},
		{
			"Get call when set to success",
			fun() -> 
				{ok, Dummypid} = dummy_media:start("testcall"),
				Call = gen_server:call(Dummypid, get_call),
				?assertMatch("testcall", Call#call.id)
			end
		},
		{
			"Get call when set to failure",
			fun() -> 
				{ok, Dummypid} = dummy_media:start("testcall", failure),
				?assertMatch(invalid, gen_server:call(Dummypid, get_call))
			end
		},
		{
			"Start cook when set to success",
			fun() -> 
				{ok, Dummypid} = dummy_media:start("testcall"),
				?assertMatch(ok, gen_server:call(Dummypid, {start_cook, ?DEFAULT_RECIPE, "testqueue"}))
			end
		},
		{
			"Start cook when set to fail",
			fun() -> 
				{ok, Dummypid} = dummy_media:start("testcall", failure),
				?assertMatch(invalid, gen_server:call(Dummypid, {start_cook, ?DEFAULT_RECIPE, "testqueue"}))
			end
		},
		{
			"Answer voicemail call when set to success",
			fun() ->
				{ok, Dummypid} = dummy_media:start("testcall"),
				?assertMatch(ok, gen_server:call(Dummypid, voicemail))
			end
		},
		{
			"Answer voicemail call when set to fail",
			fun() ->
				{ok, Dummypid} = dummy_media:start("testcall", failure),
				?assertMatch(invalid, gen_server:call(Dummypid, voicemail))
			end
		},
		{
			"Annouce when set for success",
			fun() ->
				{ok, Dummypid} = dummy_media:start("testcall"),
				?assertMatch(ok, gen_server:call(Dummypid, {announce, "Random data"}))
			end
		},
		{
			"Annouce when set to fail",
			fun() ->
				{ok, Dummypid} = dummy_media:start("testcall", failure),
				?assertMatch(invalid, gen_server:call(Dummypid, {announce, "Random data"}))
			end
		}
	].

-endif.
