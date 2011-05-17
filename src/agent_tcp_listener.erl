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
%%	Micah Warren <micahw at lordnull dot com>
%%

%% @doc A non-blocking tcp listener for agent tcp clients.  Based on the 
%% tcp_listener module by %% Serge Aleynikov [http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles].
%% Similar in concept to {@link agent_web_listener}.  Advantage over web is
%% the ability for the server to push to the client without a messy poll.
%% However, this is still not as feature complete as the web interface, 
%% primarily due to lack of an official client.  {@link agent_tcp_client} is
%% a reference implementation built in erlang.
%% 
%% To create a client cablable of logging in, use the cpx_base.proto and
%% cpx_agent.proto files (found in src/).  Communication is done by 
%% placing a protobuf message in a netstring.  A netstring is a binary:
%% 
%% <pre>	[len]":"[protobuf]","</pre>
%%
%% "len" being the the stringification of the length of the protobuf.  So,
%% a netstring of the binary `<<"hello">>' would look like `<<"5:hello,">>'.
%% For a long discussion, [http://cr.yp.to/proto/netstrings.txt].
%%
%% When a client first connects, they will get a message stating the agent
%% is in the state `PRELOGIN'.  That is the signal to start the login 
%% in ernest.  After that an agent should:
%% <ol>
%% <li>Verify it's version</li>
%% <li>Request a salt</li>
%% <li>Request to login</li>
%% </ol>
%%
%% The version verification will reply an error if the major version doesn't
%% match.  If only the minor version part doesn't match, success is 
%% returned, but error_message is populated with a message stating as much.
%% A perfect match is, well, perfect.
%% 
%% The request for a salt gets a nonce to use for salting the password, as 
%% well as the E and N parts of a public key to rsa encrypt the password.
%% 
%% Using the information from the get salt step, encrypt the password.  
%% Then send a login request.  If successful, the server will reply as such,
%% and an event stating which state the agent is in should also arrive.

-module(agent_tcp_listener).

-ifdef(TEST).
-define(PORT, 51337).
-else.
-define(PORT, 1337).
-endif.

-ifdef(TEST).
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
		listener :: port(), % Listening socket
		acceptor :: any(), % Asynchronous acceptor's internal reference
		radix :: integer(), 
 		% socket type:  tcp, upgrade to ssl, ssl
		socket_type = tcp :: 'tcp' | 'ssl_upgrade' | 'ssl',
		socket_module = gen_tcp :: 'gen_tcp' | 'ssl'
		}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

-type(port_opt() :: {'port', pos_integer()}).
-type(radix_opt() :: {'radix', pos_integer()}).
-type(socket_type_opt() :: {'socket_type', 'tcp' | 'ssl_upgrade' | 'ssl'}).
-type(start_opt() :: port_opt() | radix_opt() | socket_type_opt()).
-type(start_opts() :: [start_opt()]).
%% @doc Start the listener linked to parent process.  If `Options' is an 
%% integer, start the listener on the given port.  Otherwise, it uses start 
%% options.  Defaults are to start on port 337, with a radix of 10, and to 
%% just use straing tcp.  The socket options are `tcp', `ssl_upgrade', and 
%% `ssl'.  `tcp' indicates tcp always, with the password being sent in an
%% rsa form.  `ssl' means the client must connect with ssl initially.  
%% Password does not need special encryption in this case.  `ssl_upgrade' 
%% means the listener/conneciton will request to upgrade to ssl asap.
-spec(start_link/1 :: (Port :: integer() | start_opts()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start_link(Port) when is_integer(Port) ->
	start_link([{port, Port}]);
start_link(Options) when is_list(Options) ->
	ssl:start(),
	gen_server:start_link(?MODULE, Options, []).

%% @doc Start the listener on port `Port' linked to no process; or if a list
%% is given instead, use those settings.
%% @see start_link/1
-spec(start/1 :: (Port :: integer()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start(Port) when is_integer(Port) ->
	start([{port, Port}]);
start(Options) ->
	gen_server:start(?MODULE, [Options], []).

%% @doc Start the listener on the default port of 1337 linked to no process.
-spec(start/0 :: () -> {'ok', pid()} | 'ignore' | {'error', any()}).
start() -> 
	start([]).

%% @doc Stop the listener pid() `Pid' with reason `normal'.
-spec(stop/1 :: (Pid :: pid()) -> 'ok').
stop(Pid) -> 
	gen_server:cast(Pid, stop).

%% @hidden
init(Options) ->
%	process_flag(trap_exit, true),
	Port = proplists:get_value(port, Options, ?PORT),
	Radix = proplists:get_value(radix, Options, 10),
	SocketType = proplists:get_value(socket_type, Options, tcp),
	SimpleOpts = [list, {packet, line}, {reuseaddr, true},
		{keepalive, true}, {backlog, 30}, {active, false}],
	{Mod, Opts} = case SocketType of
		ssl ->
			{ok, CertFile} = cpx:get_env(certfile, "certfile.pem"),
			{ok, Keyfile} = cpx:get_env(keyfile, util:get_keyfile()),
			OutOpts = [{certfile, CertFile}, {keyfile, Keyfile} | SimpleOpts],
			{ssl, OutOpts};
		_ ->
			{gen_tcp, SimpleOpts}
	end,
	case Mod:listen(Port, Opts) of
		{ok, Listen_socket} ->
			%%Create first accepting process
			{ok, Ref} = prim_inet:async_accept(Listen_socket, -1),
			?INFO("Started on port ~p", [Port]),
			{ok, #state{listener = Listen_socket, acceptor = Ref, radix = Radix, socket_type = SocketType, socket_module = Mod}};
		{error, Reason} ->
			?WARNING("Could not start gen_tcp:  ~p", [Reason]),
			{stop, Reason}
	end.

%% @hidden
handle_call(Msg, _From, State) ->
	{reply, {unknown_call, Msg}, State}.

%% @hidden
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @hidden
handle_info({inet_async, ListSock, Ref, {ok, CliSocket}}, #state{listener=ListSock, acceptor=Ref, socket_module = Mod} = State) ->
	try
		case set_sockopt(ListSock, CliSocket, Mod) of
			ok  ->
				ok;
			{error, Reason} ->
				exit({set_sockopt, Reason})
		end,

		%% New client connected
		?DEBUG("new client connection.~n", []),
		{ok, Pid} = agent_tcp_connection:start(CliSocket, State#state.radix, State#state.socket_type),
		Mod:controlling_process(CliSocket, Pid),
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

handle_info({inet_async, ListSock, Ref, Error}, #state{listener=ListSock, acceptor=Ref} = State) ->
	error_logger:error_msg("Error in socket acceptor: ~p.\n", [Error]),
	{stop, Error, State};

handle_info(_Info, State) ->
	{noreply, State}.

%% @hidden
terminate(Reason, #state{socket_module = Mod} = State) ->
	?NOTICE("Terminating due to ~p", [Reason]),
	Mod:close(State#state.listener),
	ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-spec(set_sockopt/3 :: (ListSock :: port(), CliSocket :: port(), SocketMod :: 'gen_tcp' | 'ssl') -> 'ok' | any()).
set_sockopt(ListSock, CliSocket, SocketMod) ->
	true = inet_db:register_socket(CliSocket, inet_tcp),
	case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
		{ok, Opts} ->
			case prim_inet:setopts(CliSocket, Opts) of
				ok -> ok;
				Error -> SocketMod:close(CliSocket),
					Error % return error
			end;
		Error ->
			SocketMod:close(CliSocket),
			Error % return error
	end.

-ifdef(TEST).

start_test() -> 
	{ok, Pid} = start(6666),
	stop(Pid).

double_start_test() -> 
	{ok, Pid} = start(6666),
	?assertMatch({error, eaddrinuse}, start(6666)),
	stop(Pid).
	
async_listsock_test() -> 
	{timeout, 10, fun() -> {ok, Pid} = start(6666),
	{ok, Socket} = gen_tcp:connect(net_adm:localhost(), 6666, [list]),
	gen_tcp:send(Socket, "test/r/n"),
	stop(Pid),
	gen_tcp:close(Socket) end}.


-define(MYSERVERFUNC, 
	fun() -> 
		{ok, Pid} = start_link(?PORT), 
		{Pid, fun() -> stop(Pid),timer:sleep(10) end} 
	end).

-include("gen_server_test.hrl").

-endif.
