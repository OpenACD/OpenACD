-module(agent_connection).

%% depends on util, agent

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
	agent_fsm,
	send_queue = [],
	counter = 1
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
	inet:setopts(State#state.socket, [{active, false}, {packet, line}, list]),
	gen_tcp:send(State#state.socket, "Agent Server: -1\r\n"),
	{ok, Packet} = gen_tcp:recv(State#state.socket, 0),
	io:format("packet: ~p.~n", [Packet]),
		case Packet of
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

handle_cast({change_state, ringing, #call{} = Call}, State) ->
	Counter = State#state.counter,
	gen_tcp:send(State#state.socket, "ASTATE " ++ integer_to_list(Counter) ++ " " ++ integer_to_list(agent:state_to_integer(ringing)) ++ "\r\n"),
	gen_tcp:send(State#state.socket, "CALLINFO " ++ integer_to_list(Counter+1) ++ " 00310003 " ++ atom_to_list(Call#call.type) ++ " " ++ Call#call.callerid  ++ "\r\n"),
	{noreply, State#state{counter = Counter + 2}};

handle_cast({change_state, AgState, _Data}, State) ->
	Counter = State#state.counter,
	gen_tcp:send(State#state.socket, "ASTATE " ++ integer_to_list(Counter) ++ " " ++ integer_to_list(agent:state_to_integer(AgState)) ++ "\r\n"),
	{noreply, State#state{counter = Counter + 1}};

handle_cast({change_state, AgState}, State) ->
	Counter = State#state.counter,
	gen_tcp:send(State#state.socket, "ASTATE " ++ integer_to_list(Counter) ++ " " ++ integer_to_list(agent:state_to_integer(AgState)) ++ "\r\n"),
	{noreply, State#state{counter = Counter + 1}};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({tcp, Socket, Bin}, State) ->
	% Flow control: enable forwarding of next TCP message
	case parse_event(Bin) of
		[] ->
			% for some reason, erlang is sometimes getting the /n after the rest of the event
			% so lets ignore those until we find out why
			ok = inet:setopts(Socket, [{active, once}]),
			{noreply, State};
		Ev ->
			%io:format("got ~p from socket~n", [Bin]),
			case handle_event(Ev, State) of
				{Reply, State2} ->
					%io:format("sent ~p to socket~n", [Reply]),
					ok = gen_tcp:send(Socket, Reply ++ "\r\n"),
					State3 = State2#state{send_queue = flush_send_queue(lists:reverse(State2#state.send_queue), Socket)},
					ok = inet:setopts(Socket, [{active, once}]),
					{noreply, State3};
				State2 ->
					%io:format("Event requires no response~n"),
					ok = inet:setopts(Socket, [{active, once}]),
					{noreply, State2}
			end
	end;

handle_info({tcp_closed, _Socket}, State) ->
	io:format("Client disconnected~n", []),
	gen_fsm:send_all_state_event(State#state.agent_fsm, stop),
	{stop, normal, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_event(["GETSALT", Counter], State) when is_integer(Counter) ->
	State2 = State#state{salt=random:uniform(4294967295)},
	{ack(Counter, integer_to_list(State2#state.salt)), State2};

handle_event(["LOGIN", Counter, _Credentials], State) when is_integer(Counter), is_atom(State#state.salt) ->
	{err(Counter, "Please request a salt with GETSALT first"), State};

handle_event(["LOGIN", Counter, Credentials], State) when is_integer(Counter) ->
	[Username, Password] = util:string_split(Credentials, ":", 2),
	{_Reply, Pid} = agent_manager:start_agent(#agent{login=Username}),
	case agent:set_connection(Pid, self()) of
		ok ->
			State2 = State#state{agent_fsm=Pid},
			io:format("User ~p is trying to authenticate using ~p.~n", [Username, Password]),
			{ack(Counter, "1 1 1"), State2};
		error ->
			{err(Counter, Username ++ " is already logged in"), State}
	end;

handle_event([_Event, Counter], State) when is_integer(Counter), is_atom(State#state.agent_fsm) ->
	{err(Counter, "This is an unauthenticated connection, the only permitted actions are GETSALT and LOGIN"), State};

handle_event(["PING", Counter], State) when is_integer(Counter) ->
	{MegaSecs, Secs, _MicroSecs} = now(),
	{ack(Counter, integer_to_list(MegaSecs) ++ integer_to_list(Secs)), State};

handle_event(["STATE", Counter, AgState, AgStateData], State) when is_integer(Counter) ->
	try agent:list_to_state(AgState) of
		released ->
			try list_to_integer(AgStateData) of
				ReleaseState ->
					case agent:set_state(State#state.agent_fsm, released, {ReleaseState, 0}) of
						ok ->
							{ack(Counter), State};
						queued ->
							{ack(Counter), State};
						invalid ->
							{ok, OldState} = agent:query_state(State#state.agent_fsm),
							{err(Counter, "Invalid state change from " ++ atom_to_list(OldState) ++ " to released"), State}
					end
				catch
					_:_ ->
						{err(Counter, "Invalid release option"), State}
				end;
			%precall ->
				%agent:set_state(State#state.agent_fsm, precall, AgStateData)
		NewState ->
			case agent:set_state(State#state.agent_fsm, NewState, AgStateData) of
				ok ->
					{ack(Counter), State};
				invalid ->
					{ok, OldState} = agent:query_state(State#state.agent_fsm),
					{err(Counter, "Invalid state change from " ++ atom_to_list(OldState) ++ " to " ++ atom_to_list(NewState)), State}
			end
	catch
		_:_ ->
			{err(Counter, "Invalid state " ++ AgState), State}
	end;

handle_event(["STATE", Counter, AgState], State) when is_integer(Counter) ->
	try agent:list_to_state(AgState) of
		NewState ->
			case agent:set_state(State#state.agent_fsm, NewState) of
				ok ->
					{ack(Counter), State};
				%queued ->
					%{ack(Counter), State};
				invalid ->
					{ok, OldState} = agent:query_state(State#state.agent_fsm),
					{err(Counter, "Invalid state change from " ++ atom_to_list(OldState) ++ " to " ++ atom_to_list(NewState)), State}
			end
	catch
		_:_ ->
			{err(Counter, "Invalid state " ++ AgState), State}
	end;

handle_event(["BRANDLIST", Counter], State) when is_integer(Counter) ->
	{ack(Counter, "(0031003|Slic.com),(00420001|WTF)"), State};

handle_event(["PROFILES", Counter], State) when is_integer(Counter) ->
	{ack(Counter, "1:Level1 2:Level2 3:Level3 4:Supervisor"), State};

handle_event(["QUEUENAMES", Counter], State) when is_integer(Counter) ->
	{ack(Counter, "(L1-0031003|Slic.com),(L1-00420001|WTF)"), State};

handle_event(["RELEASEOPTIONS", Counter], State) when is_integer(Counter) ->
	{ack(Counter, "1:bathroom:0,2:smoke:-1"), State};

handle_event(["ACK" | [Counter | _Args]], State) when is_integer(Counter) ->
	State;

handle_event(["ACK", Counter], State) when is_integer(Counter) ->
	State;

% beware for here be errors 
handle_event(["ERR" | [Counter | _Args]], State) when is_integer(Counter) ->
	State;

handle_event(["ERR", Counter], State) when is_integer(Counter) ->
	State;

handle_event([Event, Counter], State) when is_integer(Counter) ->
	{err(Counter, "Unknown event " ++ Event), State};

handle_event([Event | [Counter | _Args]], State) when is_integer(Counter) ->
	{err(Counter, "invalid arguments for event " ++ Event), State};
	
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

ack(Counter, Data) ->
	"ACK " ++ integer_to_list(Counter) ++ " " ++ Data.

ack(Counter) ->
	"ACK " ++ integer_to_list(Counter).

err(Counter, Error) ->
	"ERR " ++ integer_to_list(Counter) ++ " " ++ Error.

-spec(send/3 :: (Event :: string(), Message :: string(), State :: #state{}) -> #state{}).
send(Event, Message, State) ->
	Counter = State#state.counter,
	State#state{counter=Counter + 1, send_queue=[Event++" "++integer_to_list(Counter)++" "++Message | State#state.send_queue]}.

flush_send_queue([], _Socket) ->
	[];
flush_send_queue([H|T], Socket) ->
	io:format("sent ~p to socket~n", [H]),
	gen_tcp:send(Socket, H ++ "\r\n"),
	flush_send_queue(T, Socket).

-ifdef(EUNIT).

-endif.
