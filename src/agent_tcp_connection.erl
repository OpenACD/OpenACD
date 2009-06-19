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
%%	The Original Code is Spice Telephony.
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
%%	Andrew Thompson <athompson at spicecsm dot com>
%%	Micah Warren <mwarren at spicecsm dot com>
%%

%% @doc The connection handler that communicates with a client UI; in this case the desktop client.

-module(agent_tcp_connection).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-export([start/1, start_link/1, negotiate/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
		code_change/3]).

-define(Major, 2).
-define(Minor, 0).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").

% {Counter, Event, Data, now()}
-type(unacked_event() :: {pos_integer(), string(), string(), {pos_integer(), pos_integer(), pos_integer()}}).

-record(state, {
		salt :: pos_integer(),
		socket :: port(),
		agent_fsm :: pid(),
		send_queue = [] :: [string()],
		counter = 1 :: pos_integer(),
		unacked = [] :: [unacked_event()],
		resent = [] :: [unacked_event()],
		resend_counter = 0 :: non_neg_integer(),
		securitylevel = agent :: 'agent' | 'supervisor' | 'admin'
	}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

%% @doc start the conection unlinked on the given Socket.  This is usually done by agent_tcp_listener.
-spec(start/1 :: (Socket :: port()) -> {'ok', pid()}).
start(Socket) ->
	gen_server:start(?MODULE, [Socket], []).

%% @doc start the conection linked on the given Socket.  This is usually done by agent_tcp_listener.
-spec(start_link/1 :: (Socket :: port()) -> {'ok', pid()}).
start_link(Socket) ->
	gen_server:start_link(?MODULE, [Socket], []).

%% @doc negotiate the client's protocol and version before login.
-spec(negotiate/1 :: (Pid :: pid()) -> 'ok').
negotiate(Pid) ->
	gen_server:cast(Pid, negotiate).

%% @hidden
init([Socket]) ->
	timer:send_interval(10000, do_tick),
	{ok, #state{socket=Socket}}.

%% @hidden
handle_call(Request, _From, State) ->
	{reply, {unknown_call, Request}, State}.

%% @hidden
% negotiate the client's protocol version and such
handle_cast(negotiate, State) ->
	?DEBUG("starting negotiation...", []),
	inet:setopts(State#state.socket, [{active, false}, {packet, line}, list]),
	gen_tcp:send(State#state.socket, "Agent Server: -1\r\n"),
	{ok, Packet} = gen_tcp:recv(State#state.socket, 0), % TODO timeout
	%?CONSOLE("packet: ~p.~n", [Packet]),
	case Packet of
		"Protocol: " ++ Args ->
			?DEBUG("Got protcol version ~p.~n", [Args]),
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
					{stop, normal, State}
			catch
				_:_ ->
					gen_tcp:send(State#state.socket, "2 Invalid Response. Login denied\r\n"),
					{stop, normal, State}
			end;
		_Else ->
			gen_tcp:send(State#state.socket, "2 Invalid Response. Login denied\r\n"),
			{stop, normal, State}
	end;

%% @hidden
handle_cast({change_state, ringing, #call{} = Call}, State) ->
	?DEBUG("change_state to ringing with call ~p", [Call]),
	Counter = State#state.counter,
	gen_tcp:send(State#state.socket, "ASTATE " ++ integer_to_list(Counter) ++ " " ++ integer_to_list(agent:state_to_integer(ringing)) ++ "\r\n"),
	gen_tcp:send(State#state.socket, "CALLINFO " ++ integer_to_list(Counter+1) ++ " " ++ clientrec_to_id(Call#call.client) ++ " " ++ atom_to_list(Call#call.type) ++ " " ++ Call#call.callerid  ++ "\r\n"),
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

%% @hidden
handle_info({tcp, Socket, Packet}, State) ->
	%?CONSOLE("handle_info {~p, ~p, ~p} ~p", [tcp, Socket, Packet, State]),
	Ev = parse_event(Packet),
	case handle_event(Ev, State) of
		{Reply, State2} ->
			%?CONSOLE("response: ~p", [Reply]),
			ok = gen_tcp:send(Socket, Reply ++ "\r\n"),
			State3 = State2#state{send_queue = flush_send_queue(lists:reverse(State2#state.send_queue), Socket)},
			% Flow control: enable forwarding of next TCP message
			ok = inet:setopts(Socket, [{active, once}]),
			%?CONSOLE("leaving info", []),
			{noreply, State3};
		State2 ->
			% Flow control: enable forwarding of next TCP message
			ok = inet:setopts(Socket, [{active, once}]),
			{noreply, State2}
	end;

handle_info({tcp_closed, _Socket}, State) ->
	?NOTICE("Client disconnected", []),
	case is_pid(State#state.agent_fsm) of
		true ->
			gen_fsm:send_all_state_event(State#state.agent_fsm, stop);
		false ->
			ok
	end,
	{stop, normal, State};

handle_info(do_tick, #state{resend_counter = Resends} = State) when Resends > 2 ->
	?NOTICE("Resend threshold exceeded, disconnecting: ~p", [Resends]),
	gen_fsm:send_all_state_event(State#state.agent_fsm, stop),
	{stop, normal, State};
handle_info(do_tick, State) ->
	ExpiredEvents = lists:filter(fun(X) -> timer:now_diff(now(), element(4,X)) >= 60000000 end, State#state.resent),
	ResendEvents = lists:filter(fun(X) -> timer:now_diff(now(), element(4,X)) >= 10000000 end, State#state.unacked),
	lists:foreach(fun({Counter, Event, Data, _Time}) ->
		?NOTICE("Expired event ~s ~p ~s", [Event, Counter, Data])
	end, ExpiredEvents),
	State2 = State#state{unacked = lists:filter(fun(X) -> timer:now_diff(now(), element(4,X)) < 10000000 end, State#state.unacked)},
	State3 = State2#state{resent = lists:append(lists:filter(fun(X) -> timer:now_diff(now(), element(4,X)) < 60000000 end, State#state.resent), ResendEvents)},
	case length(ResendEvents) of
		0 ->
			{noreply, State3};
		_Else ->
			{noreply, resend_events(ResendEvents, State3)}
	end;

handle_info(_Info, State) ->
	{noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
	ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_event(["GETSALT", Counter], State) when is_integer(Counter) ->
	State2 = State#state{salt=crypto:rand_uniform(0, 4294967295)}, %bounds of number
	{ack(Counter, integer_to_list(State2#state.salt)), State2};

handle_event(["LOGIN", Counter, _Credentials], State) when is_integer(Counter), is_atom(State#state.salt) ->
	{err(Counter, "Please request a salt with GETSALT first"), State};

handle_event(["LOGIN", Counter, Credentials, RemoteNumber], State) when is_integer(Counter), is_atom(State#state.agent_fsm) ->
	case util:string_split(Credentials, ":", 2) of
		[Username, Password] ->
			case agent_auth:auth(Username, Password, integer_to_list(State#state.salt)) of
				deny -> 
					?INFO("Authentication failure for ~s",[Username]),
					{err(Counter, "Authentication Failure"), State};
				{allow, Skills, Security, Profile} -> 
					%?CONSOLE("Authenciation success, next steps...",[]),
					{_Reply, Pid} = agent_manager:start_agent(#agent{login=Username, skills=Skills, profile=Profile}),
					case agent:set_connection(Pid, self()) of
						ok ->
							% TODO validate this?
							?NOTICE("remote number is ~p~n", [RemoteNumber]),
							case RemoteNumber of
								[] ->
									agent:set_endpoint(Pid, {sip_registration, Username});
								undefined ->
									agent:set_endpoint(Pid, {sip_registration, Username});
								_ ->
									agent:set_endpoint(Pid, {pstn, RemoteNumber})
							end,
							State2 = State#state{agent_fsm=Pid, securitylevel=Security},
							?DEBUG("User ~p has authenticated using ~p.~n", [Username, Password]),
							{MegaSecs, Secs, _MicroSecs} = now(),
							% TODO 1 1 1 should be updated to correct info (security level, profile id, current timestamp).
							{ack(Counter, io_lib:format("~p 1 ~p~p", [securitylevel_to_id(Security), MegaSecs, Secs])), State2};
						error ->
							{err(Counter, Username ++ " is already logged in"), State}
					end
			end;
			_Else ->
				{err(Counter, "Authentication Failure"), State}
	end;

handle_event(["LOGIN", Counter, Credentials], State) when is_integer(Counter), is_atom(State#state.agent_fsm) ->
	handle_event(["LOGIN", Counter, Credentials, undefined], State);

handle_event([_Event, Counter], State) when is_integer(Counter), is_atom(State#state.agent_fsm) ->
	{err(Counter, "This is an unauthenticated connection, the only permitted actions are GETSALT and LOGIN"), State};

handle_event(["PING", Counter], State) when is_integer(Counter) ->
	{MegaSecs, Secs, _MicroSecs} = now(),
	{ack(Counter, integer_to_list(MegaSecs) ++ integer_to_list(Secs)), State};

handle_event(["STATE", Counter, AgState, AgStateData], State) when is_integer(Counter) ->
	?DEBUG("Trying to set state to ~p with data ~p.", [AgState, AgStateData]),
	try agent:list_to_state(AgState) of
		released ->
			try list_to_integer(AgStateData) of
				ReleaseState ->
					case agent:set_state(State#state.agent_fsm, released, {ReleaseState, 0}) of % {humanReadable, release_reason_id}
						ok ->
							{ack(Counter), State};
						queued ->
							{ack(Counter), State}
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
	?DEBUG("Trying to set state ~p.", [AgState]),
	try agent:list_to_state(AgState) of
		released ->
			case agent:set_state(State#state.agent_fsm, released, default) of
				ok ->
					{ack(Counter), State};
				invalid ->
					{ok, OldState} = agent:query_state(State#state.agent_fsm),
					{err(Counter, "Invalid state change from " ++ atom_to_list(OldState) ++ " to released"), State}
			end;
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

handle_event(["BRANDLIST", Counter], State) when is_integer(Counter) ->
	case call_queue_config:get_clients() of
		[] ->
			{err(Counter, "No brands configured"), State};
		Brands ->
			F = fun(Elem) ->
					%Idbase = integer_to_list(Elem#client.tenant * 10000 + Elem#client.brand),
					%Padding = lists:duplicate(8 - length(Idbase), "0"),
					%Idstring = lists:append([Padding, Idbase]),
					Idstring = clientrec_to_id(Elem),
					lists:append(["(", Idstring, "|", Elem#client.label, ")"])
			end,
			Brandstringed = lists:map(F, Brands),
			{ack(Counter, string:join(Brandstringed, ",")), State}
	end;
handle_event(["PROFILES", Counter], State) when is_integer(Counter) ->
	{ack(Counter, "1:Level1 2:Level2 3:Level3 4:Supervisor"), State};

handle_event(["QUEUENAMES", Counter], State) when is_integer(Counter) ->
	% queues only have one name right now...
	Queues = string:join(lists:map(fun({Name, _Pid}) -> io_lib:format("~s|~s", [Name, Name]) end,queue_manager:queues()), "),("),
	{ack(Counter, io_lib:format("(~s)", [Queues])), State};

handle_event(["RELEASEOPTIONS", Counter], State) when is_integer(Counter) ->
	Releaseopts = agent_auth:get_releases(),
	F = fun(Elem) ->
		string:join([integer_to_list(Elem#release_opt.id), Elem#release_opt.label, integer_to_list(Elem#release_opt.bias)], ":")
	end,
	Releasestringed = string:join(lists:map(F, Releaseopts), ","),
	{ack(Counter, Releasestringed), State};

handle_event(["ENDWRAPUP", Counter], State) when is_integer(Counter) ->
	case agent:query_state(State#state.agent_fsm) of
		{ok, wrapup} ->
			case agent:set_state(State#state.agent_fsm, idle) of
				ok ->
					{ok, Curstate} = agent:query_state(State#state.agent_fsm),
					State2 = send("ASTATE", integer_to_list(agent:state_to_integer(Curstate)), State),
					{ack(Counter), State2};
				invalid ->
					{err(Counter, "invalid state"), State}
			end;
		_Else ->
			{err(Counter, "Agent must be in wrapup to send an ENDWRAPUP"), State}
	end;

handle_event(["QUEUES", Counter, "ALL"], #state{securitylevel = Security} = State) when Security =:= supervisor; Security =:= admin ->
	State2 = lists:foldl(fun({Name, _Pid}, St) -> send("QUEUE", io_lib:format("~s 0 0 0 0 0 0 0", [Name]), St) end, State, queue_manager:queues()),
	{ack(Counter), State2};
handle_event(["QUEUES", Counter, QueueNames], #state{securitylevel = Security} = State) when Security =:= supervisor; Security =:= admin ->
	Queues = util:string_split(QueueNames, ":"),
	State2 = lists:foldl(fun({Name, _Pid}, St) ->
		case lists:member(Name, Queues) of
			true ->
				send("QUEUE", io_lib:format("~s 0 0 0 0 0 0 0", [Name]), St);
			false ->
				St
		end
	end, State, queue_manager:queues()),
	{ack(Counter), State2};

% TODO use the brand on the outbound call
handle_event(["DIAL", Counter, _Brand, "outbound", Number, "1"], State) when is_integer(Counter) ->
	freeswitch_media_manager:make_outbound_call(Number, State#state.agent_fsm, agent:dump_state(State#state.agent_fsm)),
	{ack(Counter), State};

handle_event(["TRANSFER", Counter, "agent", Agent], State) when is_integer(Counter) ->
	case agent_manager:query_agent(Agent) of
		{true, AgentPid} ->
			AState = agent:dump_state(State#state.agent_fsm),
			case AState#agent.state of
				oncall ->
					Call = AState#agent.statedata,
					% TODO Move this to a more logical place.  Maybe agent?
					gen_media:agent_transfer(Call#call.source, AgentPid, 60000),
					%gen_server:call(Call#call.source, {transfer_agent, AgentPid, 100}),
					{ack(Counter), State};
				_ ->
					{err(Counter, "Agent must be oncall to make a transfer"), State}
			end;
		false ->
			{err(Counter, "No such agent logged in"), State}
	end;

handle_event(["ACK" | [Counter | _Args]], State) when is_integer(Counter) ->
	State#state{unacked = lists:filter(fun(X) -> element(1, X) =/= Counter end, State#state.unacked), resend_counter=0};

handle_event(["ACK", Counter], State) when is_integer(Counter) ->
	State#state{unacked = lists:filter(fun(X) -> element(1, X) =/= Counter end, State#state.unacked), resend_counter=0};

% beware for here be errors 
handle_event(["ERR" | [Counter | _Args]], State) when is_integer(Counter) ->
	State#state{unacked = lists:filter(fun(X) -> element(1, X) =/= Counter end, State#state.unacked), resend_counter=0};

handle_event(["ERR", Counter], State) when is_integer(Counter) ->
	State#state{unacked = lists:filter(fun(X) -> element(1, X) =/= Counter end, State#state.unacked), resend_counter=0};

handle_event([Event, Counter], State) when is_integer(Counter) ->
	?INFO("Unhandled: ~p", [Event]),
	{err(Counter, "Unknown event " ++ Event), State};

handle_event([Event | [Counter | Args]], State) when is_integer(Counter) ->
	?INFO("Unhandled: ~p with Args: ~p", [Event, Args]),
	{err(Counter, "Unknown event " ++ Event), State};

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
	SendQueue = [Event++" "++integer_to_list(Counter)++" "++Message | State#state.send_queue],
	UnackedEvents = [{Counter, Event, Message, now()} | State#state.unacked],
	State#state{counter=Counter + 1, send_queue=SendQueue, unacked=UnackedEvents}.

-spec(flush_send_queue/2 :: (Queue :: [string()], Socket :: port()) -> []).
flush_send_queue([], _Socket) ->
	[];
flush_send_queue([H|T], Socket) ->
	?DEBUG("sent ~s to socket~n", [lists:flatten(H)]),
	gen_tcp:send(Socket, H ++ "\r\n"),
	flush_send_queue(T, Socket).

-spec(resend_events/2 :: (Events :: [unacked_event()], State :: #state{}) -> #state{}).
resend_events([], State) ->
	State#state{resend_counter = State#state.resend_counter + 1};
resend_events([{Counter, Event, Data, _Time}|T], State) ->
	?NOTICE("Resending event ~s ~p ~s", [Event, Counter, Data]),
	resend_events(T, send(Event, Data, State)).

clientrec_to_id(Rec) ->
	Idbase = integer_to_list(Rec#client.tenant * 10000 + Rec#client.brand),
	Padding = lists:duplicate(8 - length(Idbase), "0"),
	lists:flatten(lists:append([Padding, Idbase])).

securitylevel_to_id(agent) ->
	1;
securitylevel_to_id(supervisor) ->
	3;
securitylevel_to_id(admin) ->
	4.

-ifdef(EUNIT).

unauthenticated_agent_test_() ->
	{
		foreach,
		fun() ->
			crypto:start(),
			mnesia:delete_schema([node()]),
			mnesia:create_schema([node()]),
			mnesia:start(),
			agent_auth:start(),
			agent_auth:new_profile("Testprofile", [skill1, skill2]),
			agent_auth:cache("Username", erlang:md5("Password"), "Testprofile", agent),
			agent_manager:start([node()]),
			#state{}
		end,
		fun(_State) ->
			agent_auth:destroy_profile("Testprofile"),
			agent_auth:stop(),
			agent_manager:stop(),
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			ok
		end,
		[
			fun(State) ->
				{"Agent can only send GETSALT and LOGIN until they're authenticated",
				fun() ->
						{Reply, _State2} = handle_event(["PING",  1], State),
						?assertEqual("ERR 1 This is an unauthenticated connection, the only permitted actions are GETSALT and LOGIN", Reply)
				end}
			end,

			fun(State) ->
				{"Agent must get a salt with GETSALT before sending LOGIN",
				fun() ->
					{Reply, _State2} = handle_event(["LOGIN",  2, "username:password"], State),
					?assertEqual("ERR 2 Please request a salt with GETSALT first", Reply)
				end}
			end,

			fun(State) ->
				{"GETSALT returns a random number",
				fun() ->
					{Reply, _State2} = handle_event(["GETSALT",  3], State),

					[_Ack, Counter, Args] = util:string_split(string:strip(util:string_chomp(Reply)), " ", 3),
					?assertEqual(Counter, "3"),

					{Reply2, _State3} = handle_event(["GETSALT",  4], State),
					[_Ack2, Counter2, Args2] = util:string_split(string:strip(util:string_chomp(Reply2)), " ", 3),
					?assertEqual(Counter2, "4"),
					?assertNot(Args =:= Args2)
				end}
			end,

			fun(State) ->
				{"LOGIN with no password",
				fun() ->
					{_Reply, State2} = handle_event(["GETSALT",  3], State),
					{Reply, _State3} = handle_event(["LOGIN",  2, "username"], State2),
					?assertMatch(["ERR", "2", "Authentication Failure"], util:string_split(string:strip(util:string_chomp(Reply)), " ", 3))
				end}
			end,

			fun(State) ->
				{"LOGIN with blank password",
				fun() ->
					{_Reply, State2} = handle_event(["GETSALT",  3], State),
					{Reply, _State3} = handle_event(["LOGIN",  2, "username:"], State2),
					?assertMatch(["ERR", "2", "Authentication Failure"], util:string_split(string:strip(util:string_chomp(Reply)), " ", 3))
				end}
			end,

			fun(State) ->
				{"LOGIN with bad credentials",
				fun() ->
					{_Reply, State2} = handle_event(["GETSALT",  3], State),
					{Reply, _State3} = handle_event(["LOGIN",  2, "username:password"], State2),
					?assertMatch(["ERR", "2", "Authentication Failure"], util:string_split(string:strip(util:string_chomp(Reply)), " ", 3))
				end}
			end,
			fun(State) ->
				{"LOGIN with good credentials",
				fun() ->
					{Reply, State2} = handle_event(["GETSALT",  3], State),
					[_Ack, "3", Args] = util:string_split(string:strip(util:string_chomp(Reply)), " ", 3),
					Password = string:to_lower(util:bin_to_hexstr(erlang:md5(Args ++ string:to_lower(util:bin_to_hexstr(erlang:md5("Password")))))),
					{Reply2, _State3} = handle_event(["LOGIN",  4, "Username:" ++ Password], State2),
					?assertMatch(["ACK", "4", _Args], util:string_split(string:strip(util:string_chomp(Reply2)), " ", 3))
				end}
			end,
			fun(State) ->
				{"LOGIN twice fails",
				fun() ->
					{Reply, State2} = handle_event(["GETSALT",  3], State),
					[_Ack, "3", Args] = util:string_split(string:strip(util:string_chomp(Reply)), " ", 3),
					Password = string:to_lower(util:bin_to_hexstr(erlang:md5(Args ++ string:to_lower(util:bin_to_hexstr(erlang:md5("Password")))))),
					{Reply2, _State3} = handle_event(["LOGIN",  4, "Username:" ++ Password], State2),
					?assertMatch(["ACK", "4", _Args], util:string_split(string:strip(util:string_chomp(Reply2)), " ", 3)),
					{Reply3, _State4} = handle_event(["LOGIN",  5, "Username:" ++ Password], State2),
					?assertMatch(["ERR", "5", _Args], util:string_split(string:strip(util:string_chomp(Reply3)), " ", 3))
				end}
			end
		]
	}.


authenticated_agent_test_() ->
	{
		foreach,
		fun() ->
			crypto:start(),
			mnesia:delete_schema([node()]),
			mnesia:create_schema([node()]),
			mnesia:start(),
			agent_auth:start(),
			agent_auth:new_profile("Testprofile", [skill1, skill2]),
			agent_auth:cache("Username", erlang:md5("Password"), "Testprofile", agent),
			agent_manager:start([node()]),
			State = #state{},
			{Reply, State2} = handle_event(["GETSALT",  3], State),
			[_Ack, "3", Args] = util:string_split(string:strip(util:string_chomp(Reply)), " ", 3),
			Password = string:to_lower(util:bin_to_hexstr(erlang:md5(Args ++ string:to_lower(util:bin_to_hexstr(erlang:md5("Password")))))),
			{_Reply2, State3} = handle_event(["LOGIN",  4, "Username:" ++ Password], State2),
			State3
		end,
		fun(_State) ->
			agent_auth:destroy_profile("Testprofile"),
			agent_auth:stop(),
			agent_manager:stop(),
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			ok
		end,
		[
			fun(State) ->
				{"LOGIN should be an unknown event",
				fun() ->
					{Reply, _State2} = handle_event(["LOGIN",  3, "username:password"], State),
					?assertEqual("ERR 3 Unknown event LOGIN", Reply)
				end}
			end,

			fun(State) ->
				{"PING should return the current timestamp",
				fun() ->
					{MegaSecs, Secs, _MicroSecs} = now(),
					Now = list_to_integer(integer_to_list(MegaSecs) ++ integer_to_list(Secs)),
					timer:sleep(1000),
					{Reply, _State2} = handle_event(["PING", 3], State),
					["ACK", "3", Args] = util:string_split(string:strip(util:string_chomp(Reply)), " ", 3),
					Pingtime = list_to_integer(Args),
					?assert(Now < Pingtime),
					timer:sleep(1000),
					{MegaSecs2, Secs2, _MicroSecs2} = now(),
					Now2 = list_to_integer(integer_to_list(MegaSecs2) ++ integer_to_list(Secs2)),
					?assert(Now2 > Pingtime)
				end}
			end,

			fun(State) ->
				{"ENDWRAPUP should not work while in not in wrapup",
				fun() ->
					{Reply, _State2} = handle_event(["ENDWRAPUP", 3], State),
					?assertEqual("ERR 3 Agent must be in wrapup to send an ENDWRAPUP", Reply)
				end}
			end,

			fun(State) ->
				{"ENDWRAPUP should work while in wrapup",
				fun() ->
					Call = #call{id="testcall", source=self()},
					?assertEqual(ok, agent:set_state(State#state.agent_fsm, idle)),
					?assertEqual(ok, agent:set_state(State#state.agent_fsm, ringing, Call)),
					?assertEqual(ok, agent:set_state(State#state.agent_fsm, oncall, Call)),
					?assertEqual(ok, agent:set_state(State#state.agent_fsm, wrapup, Call)),
					{Reply, _State2} = handle_event(["ENDWRAPUP", 3], State),
					?assertEqual("ACK 3", Reply)
				end
				}
			end
		]
	}.

socket_enabled_test_() ->
	{
		foreach,
		fun() ->
			crypto:start(),
			mnesia:delete_schema([node()]),
			mnesia:create_schema([node()]),
			mnesia:start(),
			agent_auth:start(),
			agent_auth:new_profile("Testprofile", [skill1, skill2]),
			agent_auth:cache("Username", erlang:md5("Password"), "Testprofile", agent),
			agent_manager:start([node()]),
			{ok, Pid} = agent_tcp_listener:start(),
			{ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, 1337, [inet, list, {active, false}, {packet, line}]),
			%timer:sleep(1000),
			%State = #state{},
			%{Reply, State2} = handle_event(["GETSALT",  3], State),
			%[_Ack, "3", Args] = util:string_split(string:strip(util:string_chomp(Reply)), " ", 3),
			%Password = string:to_lower(util:bin_to_hexstr(erlang:md5(Args ++ string:to_lower(util:bin_to_hexstr(erlang:md5("Password")))))),
			%{_Reply2, State3} = handle_event(["LOGIN",  4, "Username:" ++ Password], State2),
			%State3
			{Socket, Pid}
		end,
		fun({Socket, Pid}) ->
			agent_auth:destroy_profile("Testprofile"),
			agent_auth:stop(),
			agent_manager:stop(),
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			agent_tcp_listener:stop(Pid),
			gen_tcp:close(Socket),
			ok
		end,
		[
			fun({Socket, _Pid}) ->
				{"Negiotiate protocol with correct version",
				fun() ->
					?debugFmt("getting initial banner~n", []),
					{ok, Packet} = gen_tcp:recv(Socket, 0),
					?assertEqual("Agent Server: -1\r\n", Packet),
					gen_tcp:send(Socket, io_lib:format("Protocol: ~p.~p\r\n", [?Major, ?Minor])),
					{ok, Packet2} = gen_tcp:recv(Socket, 0),
					?assertEqual("0 OK\r\n", Packet2)
				end}
			end,
			fun({Socket, _Pid}) ->
				{"Negiotiate protocol with minor version mismatch",
				fun() ->
					?debugFmt("getting initial banner~n", []),
					{ok, Packet} = gen_tcp:recv(Socket, 0),
					?assertEqual("Agent Server: -1\r\n", Packet),
					gen_tcp:send(Socket, io_lib:format("Protocol: ~p.~p\r\n", [?Major, ?Minor -1])),
					{ok, Packet2} = gen_tcp:recv(Socket, 0),
					?assertEqual("1 Protocol version mismatch. Please consider upgrading your client\r\n", Packet2)
				end}
			end,
			fun({Socket, _Pid}) ->
				{"Negiotiate protocol with major version mismatch",
				fun() ->
					?debugFmt("getting initial banner~n", []),
					{ok, Packet} = gen_tcp:recv(Socket, 0),
					?assertEqual("Agent Server: -1\r\n", Packet),
					gen_tcp:send(Socket, io_lib:format("Protocol: ~p.~p\r\n", [?Major -1, ?Minor])),
					{ok, Packet2} = gen_tcp:recv(Socket, 0),
					?assertEqual("2 Protocol major version mismatch. Login denied\r\n", Packet2)
				end}
			end,
			fun({Socket, _Pid}) ->
				{"Negiotiate protocol with non-integer version",
				fun() ->
					?debugFmt("getting initial banner~n", []),
					{ok, Packet} = gen_tcp:recv(Socket, 0),
					?assertEqual("Agent Server: -1\r\n", Packet),
					gen_tcp:send(Socket, "Protocol: a.b\r\n"),
					{ok, Packet2} = gen_tcp:recv(Socket, 0),
					?assertEqual("2 Invalid Response. Login denied\r\n", Packet2)
				end}
			end,
			fun({Socket, _Pid}) ->
				{"Negiotiate protocol with gibberish",
				fun() ->
					?debugFmt("getting initial banner~n", []),
					{ok, Packet} = gen_tcp:recv(Socket, 0),
					?assertEqual("Agent Server: -1\r\n", Packet),
					gen_tcp:send(Socket, "asdfasdf\r\n"),
					{ok, Packet2} = gen_tcp:recv(Socket, 0),
					?assertEqual("2 Invalid Response. Login denied\r\n", Packet2)
				end}
			end
		]
	}.
	
post_login_test_() ->
	{
		foreach,
		local,
		fun() ->
			?CONSOLE("setup", []),
			crypto:start(),
			mnesia:delete_schema([node()]),
			mnesia:create_schema([node()]),
			mnesia:start(),
			agent_auth:start(),
			agent_auth:new_profile("Testprofile", [skill1, skill2]),
			agent_auth:cache("Username", erlang:md5("Password"), "Testprofile", agent),
			agent_manager:start([node()]),
			{ok, Pid} = agent_tcp_listener:start(),
			{ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, 1337, [inet, list, {active, once}, {packet, line}]),
			receive {tcp, Socket, _Packet} -> inet:setopts(Socket, [{active, once}]) end,
			ok = gen_tcp:send(Socket, io_lib:format("Protocol: ~p.~p\r\n", [?Major, ?Minor])),
			receive {tcp, Socket, _Packet2} -> inet:setopts(Socket, [{active, once}]) end,
			ok = gen_tcp:send(Socket, "GETSALT 3\r\n"),
			receive {tcp, Socket, Reply} -> inet:setopts(Socket, [{active, once}]) end,
			[_Ack, "3", Args] = util:string_split(string:strip(util:string_chomp(Reply)), " ", 3),
			Password = string:to_lower(util:bin_to_hexstr(erlang:md5(Args ++ string:to_lower(util:bin_to_hexstr(erlang:md5("Password")))))),
			?CONSOLE("agents:  ~p", [agent_manager:list()]),
			ok = gen_tcp:send(Socket, "LOGIN 4 Username:" ++ Password ++ "\r\n"),
			?CONSOLE("login sent...", []),
			receive {tcp, Socket, Whatever} -> inet:setopts(Socket, [{active, once}]) end,
			?CONSOLE("recv:  ~p", [Whatever]),
			receive {tcp, Socket, Whatever2} -> inet:setopts(Socket, [{active, once}]) end,
			?CONSOLE("recv:  ~p", [Whatever2]),
			{true, APid} = agent_manager:query_agent("Username"),
			{Pid, Socket, APid}
		end,
		fun({Tcplistener, Clientsock, APid}) ->
			?CONSOLE("Cleanup", []),
			agent:stop(APid),
			agent_auth:destroy("Testprofile"),
			agent_auth:stop(),
			agent_manager:stop(),
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			agent_tcp_listener:stop(Tcplistener),
			gen_tcp:close(Clientsock),
			ok
		end,
		[
			fun({_Tcplistener, Clientsock, APid}) ->
				{"Set agent to idle",
				fun() ->
					?CONSOLE("Set agent to idle", []),
					agent:set_state(APid, idle),
					receive {tcp, Clientsock, Packet} -> ok end,
					?assertEqual("ASTATE 2 " ++ integer_to_list(agent:state_to_integer(idle)) ++ "\r\n", Packet),
					?CONSOLE("post assert", [])
				end}
			end,
			fun({_Tcplistener, Clientsock, APid}) ->
				{"Set agent to new released",
				fun() ->
					?CONSOLE("Set agent to new released", []),
					agent:set_state(APid, released, "test reason"),
					receive {tcp, Clientsock, Packet} -> ok end,
					?assertEqual("ASTATE 2 " ++ integer_to_list(agent:state_to_integer(released)) ++ "\r\n", Packet)
				end}
			end,
			fun({_Tcplistener, Clientsock, APid}) ->
				{"set ringing",
				fun() ->
					Call = #call{id="testcall", source = self(), client=#client{label="testclient", brand = 1, tenant = 57, timestamp=1}},
					agent:set_state(APid, ringing, Call),
					receive {tcp, Clientsock, Packet} -> inet:setopts(Clientsock, [{active, once}]) end,
					?assertEqual("ASTATE 2 " ++ integer_to_list(agent:state_to_integer(ringing)) ++ "\r\n", Packet),
					receive {tcp, Clientsock, Packet2} -> ok end,
					?assertEqual("CALLINFO 3 00570001 voice Unknown Unknown\r\n", Packet2)
				end}
			end,
			fun({_Tcplistener, Clientsock, _APid}) ->
				{"Client request to go idle",
				fun() ->
					gen_tcp:send(Clientsock, "STATE 7 " ++ integer_to_list(agent:state_to_integer(idle)) ++ "\r\n"),
					receive {tcp, Clientsock, Packet} -> ok end,
					?assertEqual("ACK 7\r\n", Packet)
				end}
			end,
			fun({_Tcplistener, Clientsock, _APid}) ->
				{"Client requests invalid state change",
				fun() ->
					gen_tcp:send(Clientsock, "STATE 7 " ++ integer_to_list(agent:state_to_integer(wrapup)) ++ "\r\n"),
					receive {tcp, Clientsock, Packet} -> ok end,
					?assertEqual("ERR 7 Invalid state change from released to wrapup\r\n", Packet)
				end}
			end,
			fun({_Tcplistener, Clientsock, _APid}) ->
				{"Client state change request with data",
				fun() ->
					gen_tcp:send(Clientsock, "STATE 7 " ++ integer_to_list(agent:state_to_integer(released)) ++ " 6\r\n"),
					receive {tcp, Clientsock, Packet} -> ok end,
					?assertEqual("ACK 7\r\n", Packet)
				end}
			end,
			fun({_Tcplistener, Clientsock, APid}) ->
				{"Client requests release, but it's queued",
				fun() ->
					Call = #call{id="testcall", source = self(), client=#client{label="testclient", brand = 1, tenant = 57, timestamp=1}},
					agent:set_state(APid, ringing, Call),
					receive {tcp, Clientsock, _Packet} -> inet:setopts(Clientsock, [{active, once}]) end,
					receive {tcp, Clientsock, _Packet2} -> inet:setopts(Clientsock, [{active, once}]) end,
					agent:set_state(APid, oncall, Call),
					receive {tcp, Clientsock, _Packet3} -> inet:setopts(Clientsock, [{active, once}]) end,
					gen_tcp:send(Clientsock, "STATE 7 " ++ integer_to_list(agent:state_to_integer(released)) ++ " 6\r\n"),
					receive {tcp, Clientsock, Packet4} -> inet:setopts(Clientsock, [{active, once}]) end,
					?assertEqual("ACK 7\r\n", Packet4)
				end}
			end,
			fun({_Tcplistener, Clientsock, _APid}) ->
				{"Client requests invalid release option",
				fun() ->
					gen_tcp:send(Clientsock, "STATE 7 " ++ integer_to_list(agent:state_to_integer(released)) ++ " notvalid\r\n"),
					receive {tcp, Clientsock, Packet} -> ok end,
					?assertEqual("ERR 7 Invalid release option\r\n", Packet)
				end}
			end,
			fun({_Tcplistener, Clientsock, _APid}) ->
				{"Brandlist requested",
				fun() ->
					call_queue_config:build_tables(),
					call_queue_config:new_client(#client{label = "Aclient", tenant = 10, brand = 1, timestamp=1}),
					call_queue_config:new_client(#client{label = "Bclient", tenant = 5, brand = 2, timestamp=1}),
					call_queue_config:new_client(#client{label = "Cclient", tenant = 20, brand = 3, timestamp=1}),
					gen_tcp:send(Clientsock, "BRANDLIST 7\r\n"),
					receive {tcp, Clientsock, Packet} -> ok end,
					?assertEqual("ACK 7 (00100001|Aclient),(00050002|Bclient),(00200003|Cclient),(00990099|Demo Client)\r\n", Packet)
				end}
			end,
			fun({_Tcplistener, Clientsock, _APid}) ->
				{"Release options requested",
				fun() ->
					agent_auth:new_release(#release_opt{label = "Bathroom", id=1, bias=0, timestamp=1}),
					agent_auth:new_release(#release_opt{label = "Default", id = 2, bias = -1, timestamp=1}),
					gen_tcp:send(Clientsock, "RELEASEOPTIONS 7\r\n"),
					receive {tcp, Clientsock, Packet} -> ok end,
					?assertEqual("ACK 7 1:Bathroom:0,2:Default:-1\r\n", Packet)
				end}
			end%,
			%fun({_Tcplistener, _Client, APid}) ->
%				{"Unresponsive client",
%				timeout,
%				60,
%				fun() ->
%					Call = #call{id="testcall", source = self()},
%					agent:set_state(APid, ringing, Call),
%					#agent{connection = Tcp} = agent:dump_state(APid),
%					%timer:sleep(31),
%					Tcp ! do_tick,
%					Tcp ! do_tick,
%					Tcp ! do_tick,
%					?assertNot(is_process_alive(Tcp))
%				%	?assertExit(noproc, gen_server:call(Tcplistener, garbage))
%				end}
%			end
		]
	}.

clientrec_to_id_test() ->
	Client = #client{label = "testclient", tenant = 50, brand = 7, timestamp=1},
	?assertEqual("00500007", clientrec_to_id(Client)).


-define(MYSERVERFUNC,
	fun() ->
		{ok, Pid} = start_link("garbage data"),
		unlink(Pid),
		{Pid, fun() -> exit(Pid, kill), ok end}
	end).

-include("gen_server_test.hrl").

-endif.
