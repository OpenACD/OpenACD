-module(agent_connection).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-export([start/1, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
		code_change/3]).

start(Socket) ->
	gen_server:start(?MODULE, [Socket], []).

start_link(Socket) ->
	gen_server:start_link(?MODULE, [Socket], []).

init([_Socket]) ->
	{ok, {}}.

handle_call(Request, _From, State) ->
	{stop, {unknown_call, Request}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({tcp, Socket, Bin}, State) ->
	% Flow control: enable forwarding of next TCP message
	ok = inet:setopts(Socket, [{active, once}]),
	io:format("got ~p from socket~n", [Bin]),
	ok = gen_tcp:send(Socket, "ok\r\n"),
	{noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
	io:format("Client disconnected~n", []),
	{stop, normal, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

