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

%% @doc The connection handler that communicates with a client UI over tcp
%% using json as the encoding mechanism.  Works as a go between the wire
%% and {@link cpx_agent_connection}.
%% @see cpx_agent_tcp_listener

-module(cpx_agent_tcp_connection).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

% api
-export([start/4, start_link/4, negotiate/1]).
% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
		code_change/3]).

-define(Major, 2).
-define(Minor, 0).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("queue.hrl").

-record(state, {
	agent_conn_state :: any(),
	socket,
	socket_mod,
	compression = none,
	netstring = 10,
	client_errs = 0,
	err_threshold = 3 % 3 errors, and the connection ends.
}).

% ================================================================
% API
% ================================================================

-type(compression() :: 'zip' | 'gzip' | 'none').
-type(socket_type() :: 'tcp' | 'ssl').

%% @doc start the conection unlinked.
-spec(start/4 :: (Socket :: port(), SocketMod :: socket_type(),
	Radix :: pos_integer(), Compress :: compression()) -> {'ok', pid()}).
start(Socket, SocketMod, Radix, Compress) ->
	gen_server:start(?MODULE, [Socket, SocketMod, Radix, Compress], []).

%% @doc start the conection linked to calling process.
-spec(start_link/4 :: (Socket :: port(), SocketMod :: socket_type(),
	Radix :: pos_integer(), Compress :: compression()) -> {'ok', pid()}).
start_link(Socket, SocketMod, Radix, Compress) ->
	gen_server:start_link(?MODULE, [Socket, SocketMod, Radix, Compress], []).

%% @doc Notify the connection that it should set the socket to recieve
%% requets, and sent events to the client.
-spec(negotiate/1 :: (Pid :: pid()) -> 'ok').
negotiate(Pid) ->
	gen_server:cast(Pid, negotiate).

% ================================================================
% Init
% ================================================================

%% @hidden
init([Socket, SocketType, Radix, Compress]) ->
	SocketMod = case SocketType of
		ssl -> ssl;
		tcp -> gen_tcp
	end,
	State = #state{socket = Socket, socket_mod = SocketMod,
		compression = Compress, netstring = Radix},
	{ok, State}.

% ================================================================
% handle_call
% ================================================================

%% @hidden
handle_call(Request, _From, State) ->
	{reply, {unknown_call, Request}, State}.

% ================================================================
% handle_cast
% ================================================================

%% @hidden
handle_cast(negotiate, State) ->
	ok = inet:setopts(State#state.socket, [{packet, raw}, binary, {active, once}]),
	{noreply, State};

handle_cast(Msg, #state{agent_conn_state = undefined} = State) ->
	?WARNING("passed over cast due to now agent conn state:  ~p", [Msg]),
	{noreply, State};

handle_cast(Msg, State) ->
	#state{agent_conn_state = Conn} = State,
	case cpx_agent_connection:encode_cast(Conn, Msg) of
		{ok, undefined, Conn0} ->
			{noreply, State#state{agent_conn_state = Conn0}};
		{ok, Json, Conn0} ->
			send_json(Json, State),
			{noreply, State#state{agent_conn_state = Conn0}};
		{exit, undefined, Conn0} ->
			{stop, normal, State#state{agent_conn_state = Conn0}};
		{exit, Json, Conn0} ->
			send_json(Json, State),
			{stop, normal, State#state{agent_conn_state = Conn0}}
	end.

% ================================================================
% handle_info
% ================================================================

%% @hidden
handle_info({_Type, Socket, Packet}, #state{socket = Socket} = State) ->
	#state{netstring = Nscont} = State,
	{Bins, Nscont0} = netstring:decode(Packet, Nscont),
	{Jsons, Errs} = decode_binaries(Bins, State#state.compression),
	ErrCount = State#state.client_errs + Errs,
	if
		ErrCount >= State#state.err_threshold ->
			{stop, client_errors, State#state{netstring = Nscont0, client_errs = ErrCount}};
		true ->
			{Exit, State0} = service_jsons(Jsons, State#state{netstring = Nscont0}),
			case Exit of
				exit -> {stop, normal, State0};
				_ -> {noreply, State0}
			end
	end;

handle_info(_, State) ->
	{noreply, State}.

% ================================================================
% terminate
% ================================================================

%% @hidden
terminate(_Reason, _State) ->
	ok.

% ================================================================
% code_change
% =================================================================

%% @hidden
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% ================================================================
% Internal functions
% ================================================================

send_json(Json, State) ->
	#state{socket_mod = Mod, socket = Sock, compression = Zip} = State,
	BigBin = iolist_to_binary(mochijson2:encode(Json)),
	Bin = case Zip of
		none -> BigBin;
		zip -> zlib:zip(BigBin);
		gzip -> zlib:gzip(BigBin)
	end,
	NetBin = netstring:encode(Bin),
	Mod:send(Sock, NetBin).

decode_binaries(Bins, Comp) ->
	decode_binaries(Bins, Comp, [], 0).

decode_binaries([], _Comp, Acc, Errs) ->
	{lists:reverse(Acc), Errs};

decode_binaries([Bin | Tail], Comp, Acc, Errs) ->
	try begin
		BigBin = case Comp of
			none -> Bin;
			zip -> zlib:unzip(Bin);
			gzip -> zlib:gunzip(Bin)
		end,
		Json = mochijson2:decode(BigBin),
		decode_binaries(Tail, Comp, [Json | Acc], Errs)
	end catch
		What:Why ->
			?INFO("Client sent garbage data:  ~p; error:  ~p:~p", [Bin,What,Why]),
			decode_binaries(Tail, Comp, Acc, Errs+1)
	end.

service_jsons([], State) ->
	{ok, State};

service_jsons([Json | Tail], State) ->
	#state{agent_conn_state = AConn} = State,
	{Exit, OutJson, Conn0} = cpx_agent_connection:handle_json(AConn, Json),
	State0 = State#state{agent_conn_state = Conn0},
	case OutJson of
		undefined -> ok;
		_ -> send_json(OutJson, State0)
	end,
	case Exit of
		ok ->
			service_jsons(Tail, State0);
		exit ->
			{exit, State0}
	end.

% ================================================================
% Test
% ================================================================

-ifdef(TEST).

input_output_test_() -> [

		{"handle_cast", setup, fun() ->
			meck:new(cpx_agent_connection),
			meck:new(socket_mod),
			InState = #state{agent_conn_state = 1, socket_mod = socket_mod, socket = sock},
			ExpectState = InState#state{agent_conn_state = 2},
			{InState, ExpectState, 2}
		end,
		fun(_) ->
			meck:unload(cpx_agent_connection),
			meck:unload(socket_mod)
		end,
		fun({State, Expect, NewConnState}) -> [
			{"no conn state, skipped", fun() ->
				State0 = State#state{agent_conn_state = undefined},
				?assertEqual({noreply, State0}, handle_cast(random_cast, State0))
			end},

			{"no json to return, carry on", fun() ->
				meck:expect(cpx_agent_connection, encode_cast, fun(1, random_cast) ->
					{ok, undefined, NewConnState}
				end),
				Out = handle_cast(random_cast, State),
				?assertEqual({noreply, Expect}, Out)
			end},

			{"no json to return, exit", fun() ->
				meck:expect(cpx_agent_connection, encode_cast, fun(1, random_cast) ->
					{exit, undefined, NewConnState}
				end),
				Out = handle_cast(random_cast, State),
				?assertEqual({stop, normal, Expect}, Out)
			end},

			{"json to return, carry on", fun() ->
				meck:expect(cpx_agent_connection, encode_cast, fun(1, random_cast) ->
					{ok, <<"a json string">>, NewConnState}
				end),
				meck:expect(socket_mod, send, fun(sock, <<"15:\"a json string\",">>) ->
					ok
				end),
				Out = handle_cast(random_cast, State),
				?assertEqual({noreply, Expect}, Out)
			end},

			{"json to return, exit", fun() ->
				meck:expect(cpx_agent_connection, encode_cast, fun(1, random_cast) ->
					{exit, <<"a json string">>, NewConnState}
				end),
				meck:expect(socket_mod, send, fun(sock, <<"15:\"a json string\",">>) ->
					ok
				end),
				Out = handle_cast(random_cast, State),
				?assertEqual({stop, normal, Expect}, Out)
			end}

		] end},

		{"handle_info", setup, fun() ->
			meck:new(cpx_agent_connection),
			meck:new(socket_mod),
			#state{agent_conn_state = 1, socket_mod = socket_mod, socket = sock}
		end,
		fun(_) ->
			meck:unload(cpx_agent_connection),
			meck:unload(socket_mod)
		end,
		fun(State) -> [

			{"too many client errors", fun() ->
				Binaries = [<<"not a json string">>, <<"also fail">>, <<"yeah, big fail">>],
				Netstring = netstring:encode(Binaries),
				Out = handle_info({tcp, sock, Netstring}, State),
				?assertMatch({stop, client_errors, _NewState}, Out)
			end},

			{"one of the jsons demand exit", fun() ->
				Binaries = [<<"\"good\"">>, <<"\"evil\"">>],
				Netstring = netstring:encode(Binaries),
				meck:expect(cpx_agent_connection, handle_json, fun(1, <<"good">>) ->
					{exit, undefined, 2}
				end),
				Out = handle_info({tcp, sock, Netstring}, State),
				?assertMatch({stop, normal, _State}, Out)
			end},

			{"normal flow", fun() ->
				Binaries = [<<"\"first\"">>, <<"\"second\"">>],
				Netstring = netstring:encode(Binaries),
				meck:expect(cpx_agent_connection, handle_json, fun
					(1, <<"first">>) ->
						{ok, <<"first good">>, 2};
					(2, <<"second">>) ->
						{ok, <<"second good">>, 3}
				end),
				meck:expect(socket_mod, send, fun
					(sock, <<"12:\"first good\",">>) -> ok;
					(sock, <<"13:\"second good\",">>) -> ok
				end),
				Out = handle_info({tcp, sock, Netstring}, State),
				?assertMatch({noreply, _State}, Out)
			end}

		] end}
	].

% ----------------------------------------------------------------

decode_bins_test_() ->
	{setup, fun() ->
		Jsons = [<<"string the first">>, {struct, [{<<"success">>, true}]}, 42],
		Binaries = [iolist_to_binary(mochijson2:encode(X)) || X <- Jsons],
		{Jsons, Binaries}
	end,
	fun({Jsons, Binaries}) -> [

		{"no compressioned", fun() ->
			?assertEqual({Jsons, 0}, decode_binaries(Binaries, none))
		end},

		{"zip", fun() ->
			Compressed = [zlib:zip(B) || B <- Binaries],
			?assertEqual({Jsons, 0}, decode_binaries(Compressed, zip))
		end},

		{"gzip", fun() ->
			Compressed = [zlib:gzip(B) || B <- Binaries],
			?assertEqual({Jsons, 0}, decode_binaries(Compressed, gzip))
		end},

		{"compression mismatch", fun() ->
			?assertEqual({[], 3}, decode_binaries(Binaries, zip))
		end},

		{"json decode error", fun() ->
			Binaries0 = [<<"not valid json">> | Binaries],
			?assertEqual({Jsons, 1}, decode_binaries(Binaries0, none))
		end}

	] end}.

% ----------------------------------------------------------------

send_json_test_() ->
	{setup, fun() ->
		meck:new(socket_mod),
		Json = {struct, [{<<"success">>, true}]},
		State = #state{socket_mod = socket_mod, socket = sock},
		{Json, State}
	end,
	fun(_) ->
		meck:unload(socket_mod)
	end,
	fun({Json, State0}) -> [

		{"no compression", fun() ->
			State = State0#state{compression = none},
			Expect = netstring:encode(iolist_to_binary(mochijson2:encode(Json))),
			Self = self(),
			meck:expect(socket_mod, send, fun(sock, Bin) ->
				Self ! {ok, Bin},
				ok
			end),
			send_json(Json, State),
			AssertThis = receive
				R -> R
			after
				100 -> timeout
			end,
			?assertEqual({ok, Expect}, AssertThis)
		end},

		{"ziping it up", fun() ->
			State = State0#state{compression = zip},
			Expect = netstring:encode(zlib:zip(iolist_to_binary(mochijson2:encode(Json)))),
			Self = self(),
			meck:expect(socket_mod, send, fun(sock, Bin) ->
				Self ! {ok, Bin},
				ok
			end),
			send_json(Json, State),
			AssertThis = receive
				R -> R
			after
				100 -> timeout
			end,
			?assertEqual({ok, Expect}, AssertThis)
		end},

		{"gzip", fun() ->
			State = State0#state{compression = gzip},
			Expect = netstring:encode(zlib:gzip(iolist_to_binary(mochijson2:encode(Json)))),
			Self = self(),
			meck:expect(socket_mod, send, fun(sock, Bin) ->
				Self ! {ok, Bin}
			end),
			send_json(Json, State),
			AssertThis = receive
				R -> R
			after
				100 -> timeout
			end,
			?assertEqual({ok, Expect}, AssertThis)
		end}

	] end}.

%send_json(Json, State) ->
%	#state{socket_mod = Mod, socket = Sock, compression = Zip} = State,
%	BigBin = iolist_to_binary(mochijson2:encode(Json)),
%	Bin = case Zip of
%		none -> BigBin;
%		zip -> zlib:zip(BigBin);
%		gzip -> zlib:gzip(BigBin)
%	end,
%	NetBin = netstring:encode(Bin),
%	Mod:send(Sock, NetBin).

%-define(MYSERVERFUNC,
%	fun() ->
%		{ok, Pid} = start_link("garbage data"),
%		unlink(Pid),
%		{Pid, fun() -> exit(Pid, kill), ok end}
%	end).
%
%-include("gen_server_test.hrl").

-endif.
