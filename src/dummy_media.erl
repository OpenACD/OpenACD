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

%% API
-export([
	start_link/1,
	start_link/2,
	start/1,
	start/2,
	ring_agent/2,
	stop/1,
	stop/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	callrec = #call{},
	mode = success :: 'success' | 'failure'
	}).

%%====================================================================
%% API
%%====================================================================
start_link(Calldata) -> 
	start_link(Calldata, success).

start_link(Calldata, success) ->
    gen_server:start_link(?MODULE, [Calldata, success], []);
start_link(Calldata, failure) ->
	gen_server:start_link(?MODULE, [Calldata, failure], []).

start(Calldata) -> 
		start(Calldata, success).

start(Calldata, success) -> 
	gen_server:start(?MODULE, [Calldata, success], []);
start(Calldata, failure) ->
	gen_server:start(?MODULE, [Calldata, failure], []).

stop(Pid) -> 
	stop(Pid, normal).

stop(Pid, Reason) ->
	gen_server:call(Pid, {stop, Reason}).

ring_agent(Pid, Agentpid) when is_pid(Pid), is_pid(Agentpid) -> 
	gen_server:call(Pid, {ring_agent, Agentpid}).
	
	
	
%set_mode(Pid, success) when is_pid(Pid) -> 
%	gen_server:call(Pid, set_success);
%set_mode(Pid, failure) when is_pid(Pid) -> 
%	gen_server:call(Pid, set_failure).
	
%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Calldata, Mode]) ->
	process_flag(trap_exit, true),
    {ok, #state{callrec = Calldata, mode = Mode}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------

handle_call(set_success, _From, State) -> 
	{reply, ok, State#state{mode = success}};
handle_call(set_failure, _From, State) -> 
	{reply, ok, State#state{mode = failure}};
handle_call({ring_agent, AgentPid}, _From, State) -> 
	case State#state.mode of
		success -> 
			{reply, agent:set_state(AgentPid, ringing, State#state.callrec), State};
		failure -> 
			{reply, invalid, State}
	end;
handle_call(get_call, _From, State) -> 
	case State#state.mode of
		success -> 
			{reply, State#state.callrec, State};
		failure -> 
			{reply, invalid, State}
	end;
handle_call({start_cook, Recipe, Queuename}, _From, #state{callrec = Call} = State) -> 
	case State#state.mode of
		failure -> 
			{reply, invalid, State};
		success -> 
			{ok, Pid} = cook:start_link(self(), Recipe, Queuename),
			NewCall = Call#call{cook = Pid},
			{reply, ok, State#state{callrec = NewCall}}
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
			{reply, invalid, State}
	end;
handle_call(voicemail, _From, State) ->
	case State#state.mode of
		success ->
			{reply, ok, State};
		failure ->
			{reply, invalid, State}
	end;
handle_call({announce, _Args}, _From, State) ->
	case State#state.mode of
		success -> 
			{reply, ok, State};
		failure ->
			{reply, invalid, State}
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
				?assertMatch({ok, _Pid}, dummy_media:start(#call{}))
			end
		},
		{
			"Set agent ringing when set to success",
			fun() -> 
				{ok, Agentpid} = agent:start(#agent{login="testagent"}),
				agent:set_state(Agentpid, idle),
				{ok, Dummypid} = dummy_media:start(#call{}),
				?assertMatch(ok, gen_server:call(Dummypid, {ring_agent, Agentpid}))
			end
		},
		{
			"Set agent ringing when set to failure",
			fun() -> 
				{ok, Agentpid} = agent:start(#agent{login="testagent"}),
				agent:set_state(Agentpid, idle),
				{ok, Dummypid} = dummy_media:start(#call{}, failure),
				?assertMatch(invalid, gen_server:call(Dummypid, {ring_agent, Agentpid}))
			end
		},
		{
			"Get call when set to success",
			fun() -> 
				{ok, Dummypid} = dummy_media:start(#call{id="testcall"}),
				?assertMatch(#call{id="testcall"}, gen_server:call(Dummypid, get_call))
			end
		},
		{
			"Get call when set to failure",
			fun() -> 
				{ok, Dummypid} = dummy_media:start(#call{id="testcall"}, failure),
				?assertMatch(invalid, gen_server:call(Dummypid, get_call))
			end
		},
		{
			"Start cook when set to success",
			fun() -> 
				{ok, Dummypid} = dummy_media:start(#call{id="testcall"}),
				?assertMatch(ok, gen_server:call(Dummypid, {start_cook, ?DEFAULT_RECIPE, "testqueue"}))
			end
		},
		{
			"Start cook when set to fail",
			fun() -> 
				{ok, Dummypid} = dummy_media:start(#call{id="testcall"}, failure),
				?assertMatch(invalid, gen_server:call(Dummypid, {start_cook, ?DEFAULT_RECIPE, "testqueue"}))
			end
		},
		{
			"Answer voicemail call when set to success",
			fun() ->
				{ok, Dummypid} = dummy_media:start(#call{id="testcall"}),
				?assertMatch(ok, gen_server:call(Dummypid, voicemail))
			end
		},
		{
			"Answer voicemail call when set to fail",
			fun() ->
				{ok, Dummypid} = dummy_media:start(#call{id="testcall"}, failure),
				?assertMatch(invalid, gen_server:call(Dummypid, voicemail))
			end
		},
		{
			"Annouce when set for success",
			fun() ->
				{ok, Dummypid} = dummy_media:start(#call{id="testcall"}),
				?assertMatch(ok, gen_server:call(Dummypid, {announce, "Random data"}))
			end
		},
		{
			"Annouce when set to fail",
			fun() ->
				{ok, Dummypid} = dummy_media:start(#call{id="testcall"}, failure),
				?assertMatch(invalid, gen_server:call(Dummypid, {announce, "Random data"}))
			end
		}
	].

-endif.
