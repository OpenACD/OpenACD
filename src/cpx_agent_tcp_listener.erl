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

%% @doc A tcp listener for agent tcp clients using json as the encoding
%% protocol.  This is useful for desktop applications, or if a web poll
%% is not desired.  The Json api is the same as the web api, allowing it
%% to keep pace (aside from obvious protocol mismatches, like path
%% fetching).
%%
%% Json, after being turned into a binary by mochijson2:encode/1, is
%% wrapped in a netstring:
%% 
%% <pre>	[len] ":" [json] ","</pre>
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
%%
%% Encrypting the password is skipped if the connection is over SSL.
%% @see cpx_agent_tcp_connection

-module(cpx_agent_tcp_listener).

-ifdef(TEST).
-define(PORT, 57331).
-else.
-define(PORT, 7331).
-endif.
-define(con_module, cpx_agent_tcp_connection).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").

-behaviour(gen_server).

%% External API
-export([start_link/1, start/1, start/0, stop/1, acceptor/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
		code_change/3]).

-record(state, {
		listener :: port(), % Listening socket
		acceptors :: [pid()], % acceptor pool pids.
		poolsize = 5 :: pos_integer(),
		compression = none :: 'none' | 'zip' | 'gzip',
		radix :: integer(), 
 		% socket type:  tcp, upgrade to ssl, ssl
		socket_type = tcp :: 'tcp' | 'ssl_upgrade' | 'ssl',
		socket_module = gen_tcp :: 'gen_tcp' | 'ssl'
		}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

%% ====================================================================
%% Api
%% ====================================================================

-type(port_opt() :: {'port', pos_integer()}).
-type(radix_opt() :: {'radix', pos_integer()}).
-type(socket_type_opt() :: {'socket_type', 'tcp' | 'ssl_upgrade' | 'ssl'}).
-type(zip_mode_opt() :: 'zip' | 'gzip' | boolean()).
-type(zip_opt() :: 'zip_json' | {'zip_json', zip_mode_opt()}).
-type(poolsize_opt() :: {'poolsize', pos_integer()}).
-type(start_opt() :: port_opt() | radix_opt() | socket_type_opt() | 
	zip_opt() | poolsize_opt()).
-type(start_opts() :: [start_opt()]).
%% @doc Start the listener linked to parent process.  The Options (and
%% defaults) are:
%% <dl>
%% <dt>port :: pos_integer()</dt><dd>`7331'  Port to listen for incoming
%% connections.</dd>
%% <dt>radix :: pos_integer()</dt><dd>`10'  Represent netstring lengths in
%% the given numberical base.  Minimum is 2, maximum is 36.</dd>
%% <dt>socket_type</dt><dd>`tcp'  If `tcp', passwords must be encrypted by
%% rsa using the requested salt.  If `ssl_upgrade', the socket must be
%% upgraded to ssl after the connection is established.  If `ssl', the
%% initial connection must be over ssl.  In both ssl options, the password
%% is not encrypted (other than what ssl does).</dd>
%% <dt>zip_json :: boolean()</dt><dd>`false'  If present, `true', or `zip', 
%% the json binary is put through `zlib:zip/1' before being wrapped in a
%% netstring.  If `gzip', the binary is put through `zlib:gzip/1' instead.
%% If missing or `false', the json is not compressed.  The client is
%% assumed to be using the same compression scheme.</dd>
%% <dt>poolsize :: pos_integer</dt><dd>`5'  How many acceptor processes to
%% start for the listening socket.</dd>
%% </dl>
-spec(start_link/1 :: (Opts :: start_opts()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start_link(Options) when is_list(Options) ->
	ssl:start(),
	gen_server:start_link(?MODULE, Options, []).

%% @doc Start the listener linked to no process.  Generally only useful for
%% debugging or testing.
%% @see start_link/1
-spec(start/1 :: (Opts:: start_opts()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start(Options) ->
	gen_server:start(?MODULE, Options, []).

%% @doc Start the listener unlinked using only default options; generally
%% best only for debugging or testing.
%% @see start_link/1
-spec(start/0 :: () -> {'ok', pid()} | 'ignore' | {'error', any()}).
start() -> 
	start([]).

%% @doc Stop the listener pid() `Pid' with reason `normal'.
-spec(stop/1 :: (Pid :: pid()) -> 'ok').
stop(Pid) -> 
	gen_server:cast(Pid, stop).

%% ====================================================================
%% Init
%% ====================================================================

%% @hidden
init(Options) ->
	process_flag(trap_exit, true),
	ssl:start(),
	Port = proplists:get_value(port, Options, ?PORT),
	Radix = proplists:get_value(radix, Options, 10),
	SocketType = proplists:get_value(socket_type, Options, tcp),
	Compression = case proplists:get_value(zip_json, Options) of
		undefined -> none;
		false -> none;
		true -> zip;
		zip -> zip;
		gzip -> gzip;
		_ -> none
	end,
	Poolsize = proplists:get_value(poolsize, Options, 5),
	SimpleOpts = [binary, {packet, raw}, {reuseaddr, true},
		{keepalive, true}, {backlog, 30}, {active, false}],
	{Mod, Opts} = case SocketType of
		ssl ->
			{ok, CertFile} = cpx:get_env(certfile, util:get_certfile()),
			{ok, Keyfile} = cpx:get_env(keyfile, util:get_keyfile()),
			OutOpts = [{certfile, CertFile}, {keyfile, Keyfile} | SimpleOpts],
			{ssl, OutOpts};
		_ ->
			{gen_tcp, SimpleOpts}
	end,
	case Mod:listen(Port, Opts) of
		{ok, Listen_socket} ->
			State = #state{ listener = Listen_socket, acceptors = [],
				poolsize = Poolsize, compression = Compression, radix = Radix,
				socket_type = SocketType, socket_module = Mod},
			State0 = spawn_acceptors(State),
			?INFO("Started ~s on port ~p", [?MODULE, Port]),
			{ok, State0};
		{error, Reason} ->
			?WARNING("Could not start listen socket:  ~p", [Reason]),
			{stop, Reason}
	end.

%% ====================================================================
%% handle_call
%% ====================================================================

%% @hidden
handle_call(stop, _From, State) ->
	% purposefully not exposed to the public interface.
	% a synchronous stop is only useful to the tests.
	{stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
	{reply, {error, unknown_call}, State}.

%% ====================================================================
%% handle_cast
%% ====================================================================

%% @hidden
handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast({poolsize, NewPool}, State) when is_integer(NewPool), NewPool > 0 ->
	{noreply, State#state{poolsize = NewPool}};

handle_cast(_Msg, State) ->
	{noreply, State}.

%% ====================================================================
%% handle_info
%% ====================================================================

%% @hidden
handle_info({'EXIT', Pid, Reason}, State) ->
	#state{acceptors = OldAcceptors} = State,
	?DEBUG("Acceptor death due to ~p", [Reason]),
	case lists:delete(Pid, OldAcceptors) of
		OldAcceptors ->
			?WARNING("Exit from non acceptor pid ~p, going down with it", [Pid]),
			{stop, Reason, State};
		Acceptors ->
			State0 = spawn_acceptors(State#state{acceptors = Acceptors}),
			{noreply, State0}
	end;

handle_info(_Info, State) ->
	{noreply, State}.

%% ====================================================================
%% terminate
%% ====================================================================

%% @hidden
terminate(Reason, #state{socket_module = Mod} = State) ->
	?NOTICE("Terminating due to ~p", [Reason]),
	Mod:close(State#state.listener),
	ok.

%% ====================================================================
%% code_change
%% ====================================================================

%% @hidden
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ====================================================================
%% internal
%% ====================================================================

spawn_acceptors(#state{acceptors = Accs, poolsize = Poolsize} = State) when
	length(Accs) < Poolsize ->
	Pid = proc_lib:spawn_link(?MODULE, acceptor, [State]),
	Accs0 = [Pid | Accs],
	State0 = State#state{acceptors = Accs0},
	spawn_acceptors(State0);

spawn_acceptors(State) -> State.

acceptor(#state{socket_type = tcp} = State) ->
	?DEBUG("Tcp only acceptor spawned", []),
	#state{listener = ListSocket, compression = Zip, radix = Rad} = State,
	case gen_tcp:accept(ListSocket) of
		{ok, Socket} ->
			{ok, Pid} = ?con_module:start(Socket, tcp, Rad, Zip),
			ok = gen_tcp:controlling_process(Socket, Pid),
			?con_module:negotiate(Pid),
			exit(normal);
		Else ->
			exit(Else)
	end;

acceptor(#state{socket_type = ssl_upgrade} = State) ->
	?DEBUG("SSL upgrade acceptor spawned", []),
	#state{listener = ListSocket, compression = Zip, radix = Rad} = State,
	CertFile = util:get_certfile(),
	Keyfile = util:get_keyfile(),
	SSLOpts = [{certfile, CertFile}, {keyfile, Keyfile}],
	case gen_tcp:accept(ListSocket) of
		{ok, TcpSocket} ->
%			UpgradeCmd = mochijson2:encode({struct, [{<<"command">>, <<"ssl_upgrade">>}]}),
%			UpgradeCmdBin = iolist_to_binary(UpgradeCmd),
%			Sendbin = netstring:encode(UpgradeCmdBin),
%			gen_tcp:send(TcpSocket, Sendbin),
			case ssl:ssl_accept(TcpSocket, SSLOpts) of
				{ok, SslSocket} ->
					{ok, Pid} = ?con_module:start(SslSocket, ssl, Rad, Zip),
					ssl:controlling_process(SslSocket, Pid),
					?con_module:negotiate(Pid),
					exit(normal);
				SslAcceptErr ->
					exit(SslAcceptErr)
			end;
		Else ->
			exit(Else)
	end;

acceptor(#state{socket_type = ssl} = State) ->
	?DEBUG("SSL only acceptor spawned", []),
	#state{listener = ListSocket, compression = Zip, radix = Rad} = State,
	case ssl:transport_accept(ListSocket) of
		{ok, Socket} ->
			case ssl:ssl_accept(Socket) of
				ok ->
					{ok, Pid} = ?con_module:start(Socket, ssl, Rad, Zip),
					ssl:controlling_process(Socket, Pid),
					?con_module:negotiate(Pid),
					exit(normal);
				Else ->
					exit(Else)
			end;
		TransElse ->
			exit(TransElse)
	end.

%% ====================================================================
%% tests
%% ====================================================================

-ifdef(TEST).

client_connect_test_() ->
	{setup, fun() ->
		{ok, Host} = inet:gethostname(),
		Zombie = util:zombie(),
		Ets = ets:new(cpx_agent_tcp_listener_ets, [public]),
		{Host, Zombie, Ets}
	end,
	fun({Host, Zombie, Ets}) -> 
		{foreach, fun() ->
			meck:new(?con_module),
			ets:delete_all_objects(Ets)
		end,
		fun(_) ->
			meck:unload(?con_module)
		end, [
		
		fun(_) ->
			{"tcp only", fun() ->
				meck:expect(?con_module, start, fun(Sock, tcp, 10, none) ->
					ets:insert(Ets, {sock, Sock}),
					{ok, self()}
				end),
				meck:expect(?con_module, negotiate, fun(InZombie) ->
					?assertEqual(self(), InZombie),
					[{sock, Sock}] = ets:lookup(Ets, sock),
					gen_tcp:send(Sock, <<"yo">>)
				end),
				{ok, Server} = start([{socket_type, tcp}, {poolsize, 1}]),
				{ok, CliSocket} = gen_tcp:connect(Host, ?PORT, [binary,
					{active, false}]),
				Bin = gen_tcp:recv(CliSocket, 0, 1000),
				?assertEqual({ok, <<"yo">>}, Bin),
				?assert(meck:validate(?con_module)),
				gen_server:call(Server, stop)
			end}
		end,

		fun(_) ->
			{"ssl only", fun() ->
				meck:expect(?con_module, start, fun(Sock, ssl, 10, none) ->
					ets:insert(Ets, {sock, Sock}),
					{ok, self()}
				end),
				meck:expect(?con_module, negotiate, fun(InZombie) ->
					?assertEqual(self(), InZombie),
					[{sock, Sock}] = ets:lookup(Ets, sock),
					?assertEqual(ok, ssl:send(Sock, <<"yo">>))
				end),
				{ok, Server} = start([{socket_type, ssl}, {poolsize, 1}]),
				{ok, CertFile} = cpx:get_env(certfile, util:get_certfile()),
				{ok, Keyfile} = cpx:get_env(keyfile, util:get_keyfile()),
				{ok, CliSocket} = ssl:connect(Host, ?PORT, [binary, {active, false},
					{certfile, CertFile}, {keyfile, Keyfile}]),
				%Bin = ssl:recv(CliSocket, 0, 1000),
				Bin = ssl_nommer(CliSocket),
				?assertEqual(<<"yo">>, Bin),
				?assert(meck:validate(?con_module)),
				gen_server:call(Server, stop)
			end}
		end,

		fun(_) ->
			{"ssl upgrade", fun() ->
				meck:expect(?con_module, start, fun(Sock, ssl, 10, none) ->
					ets:insert(Ets, {sock, Sock}),
					{ok, self()}
				end),
				meck:expect(?con_module, negotiate, fun(InZombie) ->
					?assertEqual(self(), InZombie),
					[{sock, Sock}] = ets:lookup(Ets, sock),
					?assertEqual(ok, ssl:send(Sock, <<"yo">>))
				end),
				{ok, Server} = start([{socket_type, ssl_upgrade}, {poolsize, 1}]),
				{ok, CertFile} = cpx:get_env(certfile, util:get_certfile()),
				{ok, Keyfile} = cpx:get_env(keyfile, util:get_keyfile()),
				{ok, CliTcpSocket} = gen_tcp:connect(Host, ?PORT, [binary,
					{active, false}]),
				Bin = tcp_nommer(CliTcpSocket),
				Expected = mochijson2:encode({struct, [{<<"command">>, <<"ssl_upgrade">>}]}),
				ExpectedBin = netstring:encode(iolist_to_binary(Expected)),
				?assertEqual(ExpectedBin, Bin),
				{ok, CliSSLSocket} = ssl:connect(CliTcpSocket, [{certfile, CertFile}, {keyfile, Keyfile}]),
				SslBin = ssl_nommer(CliSSLSocket),
				?assertEqual(<<"yo">>, SslBin),
				?assert(meck:validate(?con_module)),
				gen_server:call(Server, stop)
			end}
		end ]}
	end}.

ssl_nommer(Socket) ->
	nommer(Socket, ssl, []).

tcp_nommer(Socket) ->
	nommer(Socket, gen_tcp, []).

nommer(Socket, Mod, Acc) ->
	case Mod:recv(Socket, 0, 1000) of
		{ok, Bin} ->
			Acc0 = [Bin | Acc],
			nommer(Socket, Mod, Acc0);
		{error, timeout} ->
			list_to_binary(lists:reverse(Acc));
		{error, closed} ->
			list_to_binary(lists:reverse(Acc));
		E ->
			E
	end.
-endif.
