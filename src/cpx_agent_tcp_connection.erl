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
	compression,
	netstring,
	client_errs = 0,
	err_threshold = 3 % 3 errors, and the connection ends.
}).

% =====
% API
% =====

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

% =====
% Init
% =====

%% @hidden
init([Socket, SocketType, Radix, Compress]) ->
	SocketMod = case SocketType of
		ssl -> ssl;
		tcp -> gen_tcp
	end,
	State = #state{socket = Socket, socket_mod = SocketMod,
		compression = Compress, netstring = Radix},
	{ok, State}.

% =====
% handle_call
% =====

%% @hidden
handle_call(Request, _From, State) ->
	{reply, {unknown_call, Request}, State}.

% =====
% handle_cast
% =====

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

% =====
% handle_info
% =====

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

% =====
% terminate
% =====

%% @hidden
terminate(_Reason, _State) ->
	ok.

% =====
% code_change
% ======

%% @hidden
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% =====
% Internal functions
% =====

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
	{Exit, Json, Conn0} = cpx_agent_connection:handle_json(AConn, Json),
	State0 = State#state{agent_conn_state = Conn0},
	send_json(Json, State0),
	case Exit of
		ok ->
			service_jsons(Tail, State0);
		exit ->
			{exit, State0}
	end.

% =====
% Test
% =====

-ifdef(TEST).

%-define(MYSERVERFUNC,
%	fun() ->
%		{ok, Pid} = start_link("garbage data"),
%		unlink(Pid),
%		{Pid, fun() -> exit(Pid, kill), ok end}
%	end).
%
%-include("gen_server_test.hrl").

-endif.
