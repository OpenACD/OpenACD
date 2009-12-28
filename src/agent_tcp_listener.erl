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
%%	The Original Code is OpenACD.
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
%%	Andrew Thompson <andrew at hijacked dot us>
%%	Micah Warren <micahw at fusedsolutions dot com>
%%

%% @doc A non-blocking tcp listener.  based on the tcp_listener module by Serge Aleynikov
%% [http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles]

-module(agent_tcp_listener).

-define(PORT, 1337).

%% depends on agent_tcp_connection, util, agent


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").

-behaviour(gen_server).

%% External API
-export([start_link/1, start/1, start/0, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
		code_change/3]).

-record(state, {
		listener :: port(),       % Listening socket
		acceptor :: reference()       % Asynchronous acceptor's internal reference
		}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

%% @doc Start the listener on port `Port' linked to the calling process.
-spec(start_link/1 :: (Port :: integer()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start_link(Port) when is_integer(Port) ->
	gen_server:start_link(?MODULE, [Port], []).

%% @doc Start the listener on port `Port' linked to no process.
-spec(start/1 :: (Port :: integer()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start(Port) when is_integer(Port) -> 
	gen_server:start(?MODULE, [Port], []).

%% @doc Start the listener on the default port of 1337 linked to no process.
-spec(start/0 :: () -> {'ok', pid()} | 'ignore' | {'error', any()}).
start() -> 
	start(?PORT).

%% @doc Stop the listener pid() `Pid' with reason `normal'.
-spec(stop/1 :: (Pid :: pid()) -> 'ok').
stop(Pid) -> 
	gen_server:call(Pid, stop).

%% @hidden
init([Port]) ->
	?DEBUG("~p starting at ~p", [?MODULE, node()]),
%	process_flag(trap_exit, true),
	Opts = [list, {packet, line}, {reuseaddr, true},
		{keepalive, true}, {backlog, 30}, {active, false}],
	case gen_tcp:listen(Port, Opts) of
		{ok, Listen_socket} ->
			%%Create first accepting process
			{ok, Ref} = prim_inet:async_accept(Listen_socket, -1),
			{ok, #state{listener = Listen_socket, acceptor = Ref}};
		{error, Reason} ->
			?DEBUG("Could not start gen_tcp:  ~p", [Reason]),
			{stop, Reason}
	end.

%% @hidden
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(Request, _From, State) ->
	{reply, {unknown_call, Request}, State}.

%% @hidden
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @hidden
handle_info({inet_async, ListSock, Ref, {ok, CliSocket}}, #state{listener=ListSock, acceptor=Ref} = State) ->
	try
		case set_sockopt(ListSock, CliSocket) of
			ok  ->
				ok;
			{error, Reason} ->
				exit({set_sockopt, Reason})
		end,

		%% New client connected
		?DEBUG("new client connection.~n", []),
		{ok, Pid} = agent_tcp_connection:start(CliSocket),
		gen_tcp:controlling_process(CliSocket, Pid),

		?DEBUG("negiotiating protocol.~n", []),
		agent_tcp_connection:negotiate(Pid),
	
		%% Signal the network driver that we are ready to accept another connection
		case prim_inet:async_accept(ListSock, -1) of
			{ok, NewRef} ->
				ok;
			{error, NewRef} ->
				exit({async_accept, inet:format_error(NewRef)})
		end,

		{noreply, State#state{acceptor=NewRef}}
	catch exit:Why ->
		error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
		{stop, Why, State}
end;

% TODO - why?  trying to close on shutdown w/ reason shutdown, which it does anyway.
% remove the trap exit process flag, see what happens.
handle_info({'EXIT', _From, shutdown}, State) ->
	{stop, shutdown, State};
	
handle_info({inet_async, ListSock, Ref, Error}, #state{listener=ListSock, acceptor=Ref} = State) ->
	error_logger:error_msg("Error in socket acceptor: ~p.\n", [Error]),
	{stop, Error, State};

handle_info(_Info, State) ->
	{noreply, State}.

%% @hidden
terminate(Reason, State) ->
	?NOTICE("Terminating due to ~p", [Reason]),
	gen_tcp:close(State#state.listener),
	ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-spec(set_sockopt/2 :: (ListSock :: port(), CliSocket :: port()) -> 'ok' | any()).
set_sockopt(ListSock, CliSocket) ->
	true = inet_db:register_socket(CliSocket, inet_tcp),
	case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
		{ok, Opts} ->
			case prim_inet:setopts(CliSocket, Opts) of
				ok -> ok;
				Error -> gen_tcp:close(CliSocket),
					Error % return error
			end;
		Error ->
			gen_tcp:close(CliSocket),
			Error % return error
	end.

-ifdef(EUNIT).

start_test() -> 
	{ok, Pid} = start(6666),
	stop(Pid).

double_start_test() -> 
	{ok, Pid} = start(6666),
	?assertMatch({error, eaddrinuse}, start(6666)),
	stop(Pid).
	
async_listsock_test() -> 
	{ok, Pid} = start(6666),
	{ok, Socket} = gen_tcp:connect(net_adm:localhost(), 6666, [list]),
	gen_tcp:send(Socket, "test/r/n"),
	stop(Pid),
	gen_tcp:close(Socket).



-define(MYSERVERFUNC, 
	fun() -> 
		{ok, Pid} = start_link(5050), 
		{Pid, fun() -> stop(Pid) end} 
	end).

-include("gen_server_test.hrl").

-endif.
