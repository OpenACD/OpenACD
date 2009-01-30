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
-export([start_link/3, start/3, stop/1, request/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	salt :: any(),
	ref :: ref() | 'undefined',
	agent_fsm :: pid() | 'undefined',
	ack_queue = dict:new(), % key = counter, value is {when_qed, tries, type, data} so that a message can be resent
	poll_queue = [] :: [{non_neg_integer(), non_neg_integer(), atom(), any()}], 
		% {counter to be used as key in the ack_queue, how many tries this message has had, type of message, data to send to client}
	missed_polls = 0 :: non_neg_integer(),
	counter = 1 :: non_neg_integer(),
	table :: atom() | 'undefined',
	ack_timer
}).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Post, Ref, Table) ->
    gen_server:start_link(?MODULE, [Post, Ref, Table], [{timeout, 10000}]).
	
start(Post, Ref, Table) -> 
	io:format("web_connection started~n"),
	gen_server:start(?MODULE, [Post, Ref, Table], [{timeout, 10000}]).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Post, Ref, Table]) ->
	io:format("actual web connection init~n"),
	io:format("Post: ~p~nRef: ~p~nTable: ~p~n", [Post, Ref, Table]),
	case Post of
		[{"username", User},{"password", Passwrd}] -> 
			% io:format("seems like a well formed post~n"),
			Self = self(),
			case agent_auth:auth(User, Passwrd) of
				deny -> 
					{stop, "Login Denied"};
				{allow, Skills} ->
					Agent = #agent{login=User, skills=Skills},
			
					% io:format("if they are already logged in, update the reference~n"),
					Result = ets:match(Table, {'$1', '$2', User}),
					io:format("restults:~p~n", [Result]),
					lists:map(fun([_R, P]) -> ?MODULE:stop(P) end, Result),
					ets:insert(Table, {erlang:ref_to_list(Ref), Self, User}),
					
					% start the agent and associate it with self
					{_Reply, Apid} = agent_manager:start_agent(Agent),
					case agent:set_connection(Apid, Self) of
						error -> 
							{stop, "User could not be started"};
						_Otherwise -> 
							% start the ack timer
							{ok, Tref} = timer:send_interval(?TICK_LENGTH, check_acks),
							{ok, #state{agent_fsm = Apid, ref = Ref, table = Table, ack_timer = Tref}}
					end;
				_Other ->
					{stop, "500 internal server error"}
			end;
		_Other -> 
			% io:format("all other posts~n"),
			{stop, "Invalid Post data"}
	end.


%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call({request, {"/logout", _Post, _Cookie}}, _From, State) -> 
	{stop, normal, {200, [{"Set-Cookie", "cpx_id=0"}], mochijson2:encode({struct, [{success, true}, {message, <<"Logout completed">>}]})}, State};
handle_call({request, {"/poll", _Post, _Cookie}}, _From, State) -> 
	io:format("poll called~n"),
	State2 = State#state{poll_queue=[], missed_polls = 0, ack_queue = build_acks(State#state.poll_queue, State#state.ack_queue)},
	Pollq = State#state.poll_queue,
	Json = [{struct, [{counter, Counter}, {tried, Tried}, {type, Type}, {data, Data}]} || {Counter, Tried, Type, Data} <- Pollq],
	Json2 = {struct, [{success, true}, {message, <<"Poll successful">>}, {data, Json}]},
	io:format("json:  ~p~n", [Json]),
	{reply, cpx_json:encode_trap(Json2), State2};
handle_call({request, {Path, Post, Cookie}}, _From, State) -> 
	io:format("all other requests~n"),
	case util:string_split(Path, "/") of 
		["", "state", Statename] -> 
			io:format("trying to change to ~p~n", [Statename]),
			case agent:set_state(State#state.agent_fsm, list_to_existing_atom(Statename)) of
				ok -> 
					Data = {struct, [{success, true}, {state, list_to_existing_atom(Statename)}]},
					{reply, cpx_json:encode_trap(Data), State};
				_Else -> 
					{reply, {200, [], mochijson2:encode({struct, [{success, false}]})}, State}
			end;
		["", "state", Statename, Statedata] -> 
			io:format("trying to change to ~p with data ~p~n", [Statename, Statedata]),
			case agent:set_state(State#state.agent_fsm, list_to_atom(Statename), Statedata) of 
				ok -> 
					{reply, cpx_json:encode_trap({struct, [{success, true}, {state, list_to_existing_atom(Statename)}, {data, Statedata}]}), State};
				_Else -> 
					{reply, cpx_json:encode_trap({struct, [{success, false}, {message, <<"Invalid state">>}]}), State}
			end;
		["", "ack", Counter] -> 
			io:format("you are acking~p~n", [Counter]),
			Ackq = dict:erase(list_to_integer(Counter), State#state.ack_queue),
			State2 = State#state{ack_queue = Ackq},
			{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State2};
		["", "err", Counter] -> 
			io:format("you are erroring~p", [Counter]),
			Ackq = dict:erase(list_to_integer(Counter), State#state.ack_queue),
			State2 = State#state{ack_queue = Ackq},
			{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State2};
		["", "err", Counter, Message] -> 
			io:format("you are erroring ~p with message ~p~n", [Counter, Message]),
			Ackq = dict:erase(list_to_integer(Counter), State#state.ack_queue),
			State2 = State#state{ack_queue = Ackq},
			{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State2};
		_Allelse -> 
			io:format("I have no idea what you are talking about."),
			{reply, {501, [], io_lib:format("Cannot handle request of Path ~p, Post ~p, with Cookie: ~p\", path:\"~p\", post:\"~p\", cookie:\"~p", [Path, Post, Cookie, Path, Post, Cookie])}, State}
	end.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({change_state, ringing, #call{} = Call}, State) -> 
	Counter = State#state.counter,
	C1 = {Counter+1, 0, 'ASTATE', ringing},
	C2 = {Counter+2, 0, 'CALLINFO', Call},
	Newpoll = lists:append(State#state.poll_queue, [C1, C2]),
	{noreply, State#state{counter = Counter + 2, poll_queue = Newpoll}};









handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(check_acks, State) when State#state.missed_polls < 4 -> 
	io:format("checking...~n"),
	% go through the acks, look for:
	%  ones that have been tried never and been qed for ?TICK_LENGTH * 1 time
	%  ones that have been tried once and been qed for ?TICK_LENGTH * 3 time (indicates time out)
	Ackqueue = State#state.ack_queue,
	Repoll = dict:filter(
		fun(K, V) -> 
			case V of 
				{{_Macro, When, _Micro}, Tried, _Type, _Data} when When >= ?TICK_LENGTH * 2, Tried < 2 -> 
					% put in the poll queue, it's only been tried at most 1 time, and has been in the ack queue longer than a tick.
					true;
				{{_Macro, When, _Micro}, Tried, _Type, _Data} when When >= ?TICK_LENGTH * 4, Tried >= 2 -> 
					% tried 2 or more times and been queued for 4 ticks.  timeout, die
					true;
				_Any -> 
					false
			end
		end, Ackqueue),
	
	Newack = dict:filter(
		fun(K, V) -> 
			case V of 
				{{_Macro, When, _Micro}, Tried, _Type, _Data} when When >= ?TICK_LENGTH * 2, Tried < 2 -> 
					%take out of the ack queue as it's being put in the poll queue
					false;
				{{_Macro, When, _Micro}, Tried, _Type, _Data} when When >= ?TICK_LENGTH * 4, Tried >= 2 -> 
					% same as above.
					false;
				_Any -> 
					 true
			end
		end, Ackqueue),
	
	{Newpoll, Newcounter} = build_poll(dict:to_list(Repoll), State#state.poll_queue, State#state.counter),
	
	Newmissed = State#state.missed_polls + 1,
		
	% got all that?  okay, build the new state
	
	{noreply, State#state{ack_queue = Newack, poll_queue = Newpoll, missed_polls = Newmissed, counter = Newcounter}};
	
handle_info(check_acks, State) -> 
	io:format("too many missed polls.~n"),
	{stop, "Client timeout", ok, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
	% io:format("terminated ~p~n", [?MODULE]),
	ets:delete(State#state.table, erlang:ref_to_list(State#state.ref)),
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

stop(Pid) -> 
	gen_server:call(Pid, stop).

%% @doc This conviently ships the data over to handle_call.
request(Pid, Path, Post, Cookie) -> 
	% io:format("~p:request called~n", [?MODULE]),
	gen_server:call(Pid, {request, {Path, Post, Cookie}}).
	
build_acks([], Acks) -> 
	Acks;
build_acks([{Counter, Tried, Type, Data} | Pollqueue], Acks) -> 
	Acks2 = dict:store(Counter, {now(), Tried, Type, Data}, Acks),
	build_acks(Pollqueue, Acks2).
	
build_poll([], Pollqueue, Runningcount) -> 
	{Pollqueue, Runningcount};
build_poll([{Counter, {Tried, Type, Data}} | Ackqueue], Pollqueue, Runningcount) -> 
	build_poll(Ackqueue, lists:append(Pollqueue, [{Runningcount+1, Tried+1, Type, Data}]), Runningcount+1).
