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

%% @doc Handles the internal (cpx interaction) part of an agent web connection.
%% (2008 12 09 : marking this for a refactoring.  Micah)
%% @see agent_web_listener
-module(agent_web_connection).
-author("Micah").

-behaviour(gen_server).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-define(TICK_LENGTH, 5000000000).
-else.
-define(TICK_LENGTH, 10000).
-endif.

-include("call.hrl").
-include("agent.hrl").

%% API
-export([
	start_link/2,
	start/2,
	stop/1,
	api/2,
	dump_agent/1,
	encode_statedata/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	salt :: any(),
	ref :: ref() | 'undefined',
	agent_fsm :: pid() | 'undefined',
	ack_queue = dict:new(), % key = counter, value is {when_qed, tries, pollitem} so that a message can be resent
	poll_queue = [] :: [{struct, [{binary(), any()}]}],
		% list of json structs to be sent to the client on poll.
		% struct MUST contain a counter, used to handle acks/errs
	missed_polls = 0 :: non_neg_integer(),
	counter = 1 :: non_neg_integer(),
	table :: atom() | 'undefined',
	ack_timer,
	poll_state :: atom(),
	poll_statedata :: any(),
	securitylevel = agent :: 'agent' | 'supervisor' | 'admin'
}).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Agent, Security) ->
	gen_server:start_link(?MODULE, [Agent, Security], [{timeout, 10000}]).
	
start(Agent, Security) ->
	gen_server:start(?MODULE, [Agent, Security], [{timeout, 10000}]).

stop(Pid) ->
	gen_server:call(Pid, stop).

api(Pid, Apicall) ->
	gen_server:call(Pid, Apicall).

dump_agent(Pid) ->
	gen_server:call(Pid, dump_agent).
	
encode_statedata(Callrec) when is_record(Callrec, call) ->
	case Callrec#call.client of
		undefined ->
			Brand = "unknown client";
		Clientrec when is_record(Clientrec, client) ->
			Brand = Clientrec#client.label
	end,
	{struct, [
		{<<"callerid">>, list_to_binary(Callrec#call.callerid)},
		{<<"brandname">>, list_to_binary(Brand)},
		{<<"ringpath">>, Callrec#call.ring_path},
		{<<"mediapath">>, Callrec#call.media_path},
		{<<"callid">>, list_to_binary(Callrec#call.id)},
		{<<"type">>, Callrec#call.type}]};
encode_statedata(Clientrec) when is_record(Clientrec, client) ->
	{struct, [
		{<<"brandname">>, list_to_binary(Clientrec#client.label)}]};
encode_statedata({onhold, Holdcall, calling, Calling}) ->
	Holdjson = encode_statedata(Holdcall),
	Callingjson = encode_statedata(Calling),
	{struct, [
		{<<"onhold">>, Holdjson},
		{<<"calling">>, Callingjson}]};
encode_statedata({Relcode, _Bias}) ->
	{struct, [{<<"reason">>, list_to_binary(Relcode)}]};
encode_statedata(default) ->
	{struct, [{<<"reason">>, default}]};
encode_statedata(List) when is_list(List) ->
	list_to_binary(List);
encode_statedata({}) ->
	false.


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Agent, Security]) ->
	?CONSOLE("web_connection init ~p", [Agent]),
	case agent_manager:start_agent(Agent) of
		{ok, Apid} ->
			ok;
		{exists, Apid} ->
			ok
	end,
	case agent:set_connection(Apid, self()) of
		error ->
			{stop, "Agent is already logged in"};
		_Else ->
			{ok, Tref} = timer:send_interval(?TICK_LENGTH, check_acks),
			{ok, #state{agent_fsm = Apid, ack_timer = Tref, securitylevel = Security}}
	end.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
	{stop, shutdown, ok, State};
handle_call(poll, _From, #state{poll_queue = Pollq} = State) ->
	State2 = State#state{poll_queue=[], missed_polls = 0, ack_queue = build_acks(State#state.poll_queue, State#state.ack_queue)},
	Json2 = {struct, [{success, true}, {message, <<"Poll successful">>}, {data, lists:reverse(Pollq)}]},
	{reply, {200, [], mochijson2:encode(Json2)}, State2};
handle_call(logout, _From, State) ->
	{stop, normal, {200, [{"Set-Cookie", "cpx_id=dead"}], mochijson2:encode({struct, [{success, true}]})}, State};
handle_call({set_state, Statename}, _From, #state{agent_fsm = Apid} = State) ->
	case agent:set_state(Apid, agent:list_to_state(Statename)) of
		ok ->
			{reply, {200, [], mochijson2:encode({struct, [{success, true}, {<<"status">>, ok}]})}, State};
		invalid ->
			{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"status">>, invalid}]})}, State}
	end;
handle_call({set_state, Statename, Statedata}, _From, #state{agent_fsm = Apid} = State) ->
	case agent:set_state(Apid, agent:list_to_state(Statename), Statedata) of
		invalid ->
			{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"status">>, invalid}]})}, State};
		Status ->
			{reply, {200, [], mochijson2:encode({struct, [{success, true}, {<<"status">>, Status}]})}, State}
	end;
handle_call({set_remote_number, Number}, _From, #state{agent_fsm = Apid} = State) ->
	{reply, agent:set_remote_number(Apid, Number), State};
handle_call({dial, Number}, _From, #state{agent_fsm = AgentPid} = State) ->
	%% don't like it, but hardcoding freeswitch
	case whereis(freeswitch_media_manager) of
		undefined ->
			{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Freeswitch not available">>}]})}, State};
		_Pid ->
			AgentRec = agent:dump_state(AgentPid),
			freeswitch_media_manager:make_outbound_call(Number, AgentPid, AgentRec),
			{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State}
	end;
handle_call(dump_agent, _From, #state{agent_fsm = Apid} = State) ->
	Astate = agent:dump_state(Apid),
	{reply, Astate, State};
handle_call(Allothers, _From, State) ->
	{reply, {unknown_call, Allothers}, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({change_state, AgState, Data}, #state{poll_queue = Pollq, counter = Counter} = State) ->
	?CONSOLE("State:  ~p; Data:  ~p", [AgState, Data]),
	Newqueue =
		[{struct, [
			{<<"counter">>, Counter},
			{<<"command">>, <<"astate">>},
			{<<"state">>, AgState},
			{<<"statedata">>, encode_statedata(Data)}
		]} | Pollq],
	{noreply, State#state{counter = Counter + 1, poll_queue = Newqueue}};
handle_cast({change_state, AgState}, #state{poll_queue = Pollq, counter = Counter} = State) ->
	Newqueue =
		[{struct, [
			{<<"counter">>, Counter},
			{<<"command">>, <<"astate">>},
			{<<"state">>, AgState}
		]} | Pollq],
	{noreply, State#state{counter = Counter + 1, poll_queue = Newqueue}};
handle_cast(Msg, State) ->
	?CONSOLE("Other case ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(check_acks, #state{missed_polls = Missedpolls} = State) when Missedpolls < 4 -> 	
	{noreply, State#state{missed_polls = Missedpolls + 1}};
handle_info(check_acks, State) -> 
	?CONSOLE("too many missed polls.",[]),
	{stop, "Client timeout", ok, State};
handle_info(Info, State) ->
	?CONSOLE("info I can't handle:  ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(Reason, State) ->
	?CONSOLE("terminated ~p", [Reason]),
	%ets:delete(State#state.table, erlang:ref_to_list(State#state.ref)),
	agent:stop(State#state.agent_fsm),
	timer:cancel(State#state.ack_timer),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
	
build_acks([], Acks) -> 
	Acks;
build_acks([{struct, Pollprops} | Pollqueue], Acks) -> 
	%[{Counter, Tried, Type, Data} | Pollqueue]
	Counter = proplists:get_value(<<"counter">>, Pollprops),
	case dict:find(Counter, Acks) of
		error ->
			Acks2 = dict:store(Counter, {now(), 0, {struct, Pollprops}}, Acks);
		{Time, Tried, _Struct} ->
			Acks2 = dict:store(Counter, {Time, Tried+1, {struct, Pollprops}}, Acks)
	end,
	build_acks(Pollqueue, Acks2).
	
-ifdef(TEST).

set_state_test_() ->
	{
		foreach,
		fun() ->
			agent_manager:start([node()]),
			{ok, Connpid} = agent_web_connection:start(#agent{login = "testagent", skills = [english]}, agent),
			{Connpid}
		end,
		fun({Connpid}) ->
			stop(Connpid),
			agent_manager:stop()
		end,
		[
			fun({Connpid}) ->
				{"Set state valid",
				fun() ->
					Reply = gen_server:call(Connpid, {set_state, "idle"}),
					?assertEqual({200, [], [123, [34,"success", 34], 58,<<"true">>, 44, [34,<<"status">>, 34], 58, [34,"ok", 34], 125]}, Reply),
					Reply2 = gen_server:call(Connpid, {set_state, "released", "default"}),
					?assertEqual({200, [], [123, [34,"success", 34], 58,<<"true">>, 44, [34,<<"status">>, 34], 58, [34,"ok", 34], 125]}, Reply2)
				end}
			end,
			fun({Connpid}) ->
				{"Set state invalid",
				fun() ->
					Reply = gen_server:call(Connpid, {set_state, "wrapup"}),
					?CONSOLE("~p", [Reply]),
					?assertEqual({200, [], [123, [34,"success", 34], 58,<<"false">>, 44, [34,<<"status">>, 34], 58, [34,"invalid", 34], 125]}, Reply),
					Reply2 = gen_server:call(Connpid, {set_state, "wrapup", "garbage"}),
					?CONSOLE("~p", [Reply2]),
					?assertEqual({200, [], [123, [34,"success", 34], 58,<<"false">>, 44, [34,<<"status">>, 34], 58, [34,"invalid", 34], 125]}, Reply2)
				end}
			end
		]
	}.

-define(MYSERVERFUNC, 
	fun() ->
		["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		mnesia:create_schema([node()]),
		mnesia:start(),
		Passwd = util:bin_to_hexstr(erlang:md5("Password123")),
		Saltedpasswd = util:bin_to_hexstr(erlang:md5(string:concat("12345", Passwd))),
		Agent = #agent{login = "agent", skills = [english]},
		agent_manager:start([node()]),
		agent_auth:start(),
		{ok, Pid} = start_link(Agent, agent),
		Stopfun = fun() ->
			stop(Pid),
			agent_auth:stop(),
			agent_manager:stop(),
			mnesia:stop(),
			mnesia:delete_schema([node()])
		end,
		{Pid, Stopfun}
	end
).

-include("gen_server_test.hrl").


-endif.
