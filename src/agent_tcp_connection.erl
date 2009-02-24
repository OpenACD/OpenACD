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

%% @doc The connection handler that communicates with a client UI; in this case the desktop client.
%% @clear
-module(agent_tcp_connection).

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

%% @doc start the conection unlinked on the given Socket.  This is usually done by agent_tcp_listener
start(Socket) ->
	gen_server:start(?MODULE, [Socket], []).

%% @doc start the conection linked on the given Socket.  This is usually done by agent_tcp_listener
start_link(Socket) ->
	gen_server:start_link(?MODULE, [Socket], []).

%% @doc negotiate the client's protocol, and version before login.
negotiate(Pid) ->
	gen_server:cast(Pid, negotiate).

% TODO seed w/ something so it's actually random
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
		_Else ->
			gen_tcp:send(State#state.socket, "2 Invalid Response. Login denied\r\n"),
			{stop, normal}
	end;

% TODO brandid is hard coded, not good (it's the 00310003)
handle_cast({change_state, ringing, #call{} = Call}, State) ->
	?CONSOLE("change_state to ringint with call ~p", [Call]),
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
	
handle_info({tcp, Socket, Packet}, State) ->
	% Flow control: enable forwarding of next TCP message
	case parse_event(Packet) of
		[] ->
			% TODO this may have been solved by setting the socket in line mode.
			% for some reason, erlang is sometimes getting the /n after the rest of the event
			% so lets ignore those until we find out why
			ok = inet:setopts(Socket, [{active, once}]),
			{noreply, State};
		Ev ->
			case handle_event(Ev, State) of
				{Reply, State2} ->
					ok = gen_tcp:send(Socket, Reply ++ "\r\n"),
					State3 = State2#state{send_queue = flush_send_queue(lists:reverse(State2#state.send_queue), Socket)},
					ok = inet:setopts(Socket, [{active, once}]),
					{noreply, State3};
				State2 ->
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
	State2 = State#state{salt=random:uniform(4294967295)}, %bounds of number
	{ack(Counter, integer_to_list(State2#state.salt)), State2};

handle_event(["LOGIN", Counter, _Credentials], State) when is_integer(Counter), is_atom(State#state.salt) ->
	{err(Counter, "Please request a salt with GETSALT first"), State};

handle_event(["LOGIN", Counter, Credentials], State) when is_integer(Counter) ->
	[Username, Password] = util:string_split(Credentials, ":", 2),
	% currently the password is coming in as a lowercase hex string (2008 / 12 / 04, Micah)
	BinPass = util:hexstr_to_bin(Password),
	io:format("username: ~p~npassword: ~p~nbinpass: ~p~nsalt: ~p~n", [Username, Password, BinPass, State#state.salt]),
	case agent_auth:auth(Username, Password, integer_to_list(State#state.salt)) of
		deny -> 
			io:format("Authentication failure~n"),
			{err(Counter, "Authentication Failure"), State};
		{allow, Skills} -> 
			io:format("Authenciation success, next steps..."),
			{_Reply, Pid} = agent_manager:start_agent(#agent{login=Username, skills=Skills}),
			case agent:set_connection(Pid, self()) of
				ok ->
					State2 = State#state{agent_fsm=Pid},
					io:format("User ~p has authenticated using ~p.~n", [Username, Password]),
					% TODO 1 1 1 should be updated to correct info (something, something security level, not in that order maybe.  RESEARCH!).
					{ack(Counter, "1 1 1"), State2};
				error ->
					{err(Counter, Username ++ " is already logged in"), State}
			end
	end;

handle_event([_Event, Counter], State) when is_integer(Counter), is_atom(State#state.agent_fsm) ->
	{err(Counter, "This is an unauthenticated connection, the only permitted actions are GETSALT and LOGIN"), State};

handle_event(["PING", Counter], State) when is_integer(Counter) ->
	{MegaSecs, Secs, _MicroSecs} = now(),
	{ack(Counter, integer_to_list(MegaSecs) ++ integer_to_list(Secs)), State};

handle_event(["STATE", Counter, AgState, AgStateData], State) when is_integer(Counter) ->
	?CONSOLE("Trying to set state to ~p with data ~p.", [AgState, AgStateData]),
	try agent:list_to_state(AgState) of
		released ->
			try list_to_integer(AgStateData) of
				ReleaseState ->
					case agent:set_state(State#state.agent_fsm, released, {ReleaseState, 0}) of % {humanReadable, release_reason_id}
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
	?CONSOLE("Trying to set state ~p.", [AgState]),
	try agent:list_to_state(AgState) of
		NewState ->
			case agent:set_state(State#state.agent_fsm, NewState) of
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

% TODO Hardcoding...
handle_event(["BRANDLIST", Counter], State) when is_integer(Counter) ->
	{ack(Counter, "(0031003|Slic.com),(00420001|WTF)"), State};

handle_event(["PROFILES", Counter], State) when is_integer(Counter) ->
	{ack(Counter, "1:Level1 2:Level2 3:Level3 4:Supervisor"), State};

handle_event(["QUEUENAMES", Counter], State) when is_integer(Counter) ->
	{ack(Counter, "(L1-0031003|Slic.com),(L1-00420001|WTF)"), State};

handle_event(["RELEASEOPTIONS", Counter], State) when is_integer(Counter) ->
	{ack(Counter, "1:bathroom:0,2:smoke:-1"), State};

handle_event(["ENDWRAPUP", Counter], State) when is_integer(Counter) -> 
	case agent:set_state(State#state.agent_fsm, idle) of
		ok -> 
			{ok, Curstate} = agent:query_state(State#state.agent_fsm),
			send("ASTATE", integer_to_list(agent:state_to_integer(Curstate)), State),
			{ack(Counter), State#state{counter = State#state.counter + 1}};
		invalid -> 
			{err(Counter, "invalid state"), State}
	end;

% TODO track unacked events
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
