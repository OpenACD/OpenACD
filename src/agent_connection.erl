-module(agent_connection).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-export([start/1, start_link/1, negotiate/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
		code_change/3]).

-define(Major, 2).
-define(Minor, 0).

-include("call.hrl").
-include("agent.hrl").

-record(state, {
	salt,
	socket,
	agent_fsm
	}).

start(Socket) ->
	gen_server:start(?MODULE, [Socket], []).

start_link(Socket) ->
	gen_server:start_link(?MODULE, [Socket], []).

negotiate(Pid) ->
	gen_server:cast(Pid, negotiate).

init([Socket]) ->
	random:seed(), % seed the random number generator with some process info
	{ok, #state{socket=Socket}}.

handle_call(Request, _From, State) ->
	{stop, {unknown_call, Request}, State}.

% negotiate the client's protocol version and such
handle_cast(negotiate, State) ->
	inet:setopts(State#state.socket, [{active, false}, {packet, 0}, binary]),
	gen_tcp:send(State#state.socket, "Agent Server: -1\r\n"),
	{ok, Packet} = gen_tcp:recv(State#state.socket, 0),
	io:format("packet: ~p.~n", [Packet]),
		case binary_to_list(Packet) of
			"Protocol: " ++ Args ->
				io:format("Got protcol version ~p.~n", [Args]),
				try lists:map(fun(X) -> list_to_integer(X) end, util:string_split(util:string_chomp(Args), ".", 2)) of
					[?Major, ?Minor] ->
						gen_tcp:send(State#state.socket, "0 OK\r\n"),
						inet:setopts(State#state.socket, [{active, once}]),
						{noreply, State};
					[?Major, _Minor] ->
						gen_tcp:send(State#state.socket, "1 Protocol version mismatch. Please consider upgrading your client\r\n"),
						inet:setopts(State#state.socket, [{active, once}]),
						{noreply, State};
					[_Major, _Minor] ->
						gen_tcp:send(State#state.socket, "2 Protocol major version mismatch. Login denied\r\n"),
						{stop, normal}
				catch
					_:_ ->
					gen_tcp:send(State#state.socket, "2 Invalid Response. Login denied\r\n"),
					{stop, normal}
				end;
			_ ->
				gen_tcp:send(State#state.socket, "2 Invalid Response. Login denied\r\n"),
				{stop, normal}
		end;

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({tcp, Socket, Bin}, State) ->
	% Flow control: enable forwarding of next TCP message
	ok = inet:setopts(Socket, [{active, once}]),
	case parse_event(binary_to_list(Bin)) of
		[] ->
			% for some reason, erlang is sometimes getting the /n after the rest of the event
			% so lets ignore those until we find out why
			{noreply, State};
		Ev ->
			io:format("got ~p from socket~n", [Bin]),
			{Reply, State2} = handle_event(Ev, State),
			io:format("sent ~p to socket~n", [Reply]),
			ok = gen_tcp:send(Socket, Reply ++ "\r\n"),
			{noreply, State2}
	end;

handle_info({tcp_closed, _Socket}, State) ->
	io:format("Client disconnected~n", []),
	{stop, normal, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_event(["GETSALT", Counter], State) when is_integer(Counter) ->
	State2 = State#state{salt=random:uniform(4294967295)},
	{"ACK " ++ integer_to_list(Counter) ++ " " ++ integer_to_list(State2#state.salt), State2};

handle_event(["LOGIN", Counter, _Credentials], State) when is_integer(Counter), is_atom(State#state.salt) ->
	{"ERR " ++ integer_to_list(Counter) ++ " Please request a salt with GETSALT first", State};

handle_event(["LOGIN", Counter, Credentials], State) when is_integer(Counter) ->
	[Username, Password] = util:string_split(Credentials, ":", 2),
	{ok, Pid} = agent:start_link(#agent{login=Username}),
	State2 = State#state{agent_fsm=Pid},
	io:format("User ~p is trying to authenticate using ~p.~n", [Username, Password]),
	{"ACK " ++ integer_to_list(Counter) ++ " 1 1 1", State2};

handle_event([_Event, Counter], State) when is_integer(Counter), is_atom(State#state.agent_fsm) ->
	{"ERR " ++ integer_to_list(Counter) ++ " This is an unauthenticated connection, the only permitted actions are GETSALT and LOGIN", State};
	
handle_event(["PING", Counter], State) when is_integer(Counter) ->
	{MegaSecs, Secs, _MicroSecs} = now(),
	{"ACK " ++ integer_to_list(Counter) ++ " " ++ integer_to_list(MegaSecs) ++ integer_to_list(Secs), State};

handle_event(["STATE", Counter, AgState], State) when is_integer(Counter) ->
	try agent:list_to_state(AgState) of
		NewState ->
			case agent:set_state(State#state.agent_fsm, NewState) of
				ok ->
					{"ACK " ++ integer_to_list(Counter), State};
				invalid ->
					{ok, OldState} = agent:query_state(State#state.agent_fsm),
					{"ERR " ++ integer_to_list(Counter) ++ " Invalid state change from " ++ atom_to_list(OldState) ++ " to " ++ atom_to_list(NewState), State}
			end
	catch
		_:_ ->
			{"ERR " ++ integer_to_list(Counter) ++ " Invalid state " ++ AgState, State}
	end;

handle_event([Event, Counter], State) when is_integer(Counter) ->
	{"ERR " ++ integer_to_list(Counter) ++ " Unknown event " ++ Event, State};

handle_event([Event | [Counter | Args]], State) when is_integer(Counter) ->
	{"ERR " ++ integer_to_list(Counter) ++ " invalid arguments for event " ++ Event, State};
	
handle_event(_Stuff, State) ->
	{"ERR Invalid Event, missing or invalid counter", State}.

parse_event(String) ->
	case util:string_split(string:strip(util:string_chomp(String)), " ", 3) of
		[Event] ->
			[Event];
		[Event, Counter] ->
			[Event, parse_counter(Counter)];
		[Event, Counter, Args] ->
			lists:append([Event, parse_counter(Counter)], util:string_split(Args, " "));
		[] ->
			[]
	end.

parse_counter(Counter) ->
	try list_to_integer(Counter) of
		IntCounter -> IntCounter
	catch
		_:_ -> Counter
	end.

-ifdef(EUNIT).

-endif.
