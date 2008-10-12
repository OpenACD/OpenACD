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
	%io:format("got ~p from socket~n", [Bin]),
	ok = gen_tcp:send(Socket, handle_event(parse_event(binary_to_list(Bin))) ++ "\r\n"),
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


handle_event(["PING", Counter]) when is_integer(Counter) ->
	{MegaSecs, Secs, _MicroSecs} = now(),
	"ACK " ++ integer_to_list(Counter) ++ " " ++ integer_to_list(MegaSecs) ++ integer_to_list(Secs);

handle_event(["STATE", Counter, _State]) when is_integer(Counter) ->
	"ACK " ++ integer_to_list(Counter);

handle_event([Event, Counter]) when is_integer(Counter) ->
	"ACK " ++ integer_to_list(Counter) ++ " Unknown event " ++ Event;

handle_event(_Stuff) ->
	"ACK Invalid Event, missing or invalid counter".

parse_event(Args) ->
	String = string:strip(string:strip(string:strip(Args,right, $\n), right, $\r)),
	case util:string_split(String, " ", 3) of
		[Event] ->
			[Event];
		[Event, Counter] ->
			[Event, parse_counter(Counter)];
		[Event, Counter, NArgs] ->
			lists:append([Event, parse_counter(Counter)], util:string_split(NArgs, " "))
	end.

parse_counter(Counter) ->
	try list_to_integer(Counter) of
		IntCounter -> IntCounter
	catch
		_:_ -> Counter
	end.
