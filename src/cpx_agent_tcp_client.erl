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

%% @doc a reference implementation of an agent_tcp client, in erlang for
%% testing goodness.

-module(cpx_agent_tcp_client).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

% API
% call flow
-export([
	start/1,
	start_link/1,
	check_version/1,
	get_nonce/1,
	login/3,
	idle/1,
	released/1,
	released/2,
	answer/2,
	hangup/2,
	to_agent/3,
	to_queue/3,
	to_queue/4,
	to_queue/5
]).
% support (not all are implemented, just the important ones)
-export([
	brands/1,
	queues/1,
	release_opts/1,
	queueing_opts/1,
	profiles/1,
	agents/1,
	ring_test/1,
	logout/1,
	do_request/2
]).

-export([
	media_command/4
]).
	

% gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
		code_change/3]).

-define(Major, 2).
-define(Minor, 0).
-ifdef(TEST).
-define(port, 57331).
-else.
-define(port, 7331).
-endif.

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").

-record(options, {
	username,
	password,
	port,
	server,
	protocol,
	compression,
	endpoints,
	radix
}).
-record(state, {
		salt :: pos_integer(),
		socket :: {'gen_tcp' | 'ssl', port()},
		requests = dict:new(),
		channels = dict:new(),
		last_req_id = 0,
		options :: #options{},
		netstring = 10,
		subscribers = []
	}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

% =====
% API
% =====

-type(server_option() :: {server, string()}).
-type(port_option() :: {port, pos_integer}).
-type(username_option() :: {username, string()}).
-type(password_option() :: {password, string()}).
-type(silent_option() :: silent).
-type(voipendpoint() :: 
	sip |
	sip_registration |
	h323 |
	iax |
	pstn
).
-type(voipendpoint_option() :: {voipendpoint, voipendpoint()}).
-type(voipendpoint_data_option() :: {voipendpoint_data, string()}).
-type(persistent_ring_option() :: 'persistent_ring').
-type(start_option() :: 
	server_option() |
	port_option() | 
	username_option() | 
	password_option() | 
	voipendpoint_option() | 
	voipendpoint_data_option() |
	silent_option() |
	persistent_ring_option()
).
-type(start_options() :: [start_option()]).

%% @doc Start the agent with the given options unlinked to a process.
-spec(start/1 :: (Options :: start_options()) -> {'ok', pid()}).
start(Options) ->
	ssl:start(),
	gen_server:start(?MODULE, Options, []).

%% @doc start the conection linked to the calling process.
-spec(start_link/1 :: (Options :: start_options()) -> {'ok', pid()}).
start_link(Options) ->
	ssl:start(),
	gen_server:start_link(?MODULE, Options, []).

%% @doc Do a version check.
check_version(Pid) ->
	do_request(Pid, {undefined, check_version, undefined}).

%% @doc Get a nonce.
get_nonce(Pid) ->
	do_request(Pid, {undefined, get_nonce, undefined}).

%% @doc Login like a boss.  Password should already be encoded if needed.
login(Pid, Username, Password) ->
	do_request(Pid, {undefined, login, [Username, Password]}).
	
%% @doc Attempt to go idle.
-spec(idle/1 :: (Pid :: pid()) -> 'ok').
idle(Pid) ->
	do_request(Pid, {undefined, set_release, false}).

%% @doc Attempt to go released for default reason.
-spec(released/1 :: (Pid :: pid()) -> 'ok').
released(Pid) ->
	released(Pid, default).

%% @doc Attempt to go released for the specified reason.
-spec(released/2 :: (Pid :: pid(), Why :: 'default' | string()) -> 'ok').
released(Pid, Why) ->
	do_request(Pid, {undefined, set_release, Why}).

%% @doc Attempt to answer a call.
-spec(answer/2 :: (Pid :: pid(), ChannelId :: any()) -> 'ok').
answer(Pid, ChannelId) ->
	do_request(Pid, {undefined, set_state, [ChannelId, oncall]}).

%% @doc Attempt to hangup a call.
-spec(hangup/2 :: (Pid :: pid(), ChannelId :: any()) -> 'ok').
hangup(Pid, ChannelId) ->
	do_request(Pid, {undefined, set_state, [ChannelId, wrapup]}).

%% @doc Attempt to transfer a call to another agent.
-spec(to_agent/3 :: (Pid :: pid(), ChannelId :: any(),
	Agent :: string()) -> 'ok').
to_agent(Pid, ChannelId, Agent) ->
	do_request(Pid, {undefined, agent_transfer, [ChannelId, Agent]}).

%% @doc Attempt to transfer a call to a queue.
-spec(to_queue/3 :: (Pid :: pid(), ChannelId :: any(),
	Queue :: string()) -> 'ok').
to_queue(Pid, ChannelId, Queue) ->
	to_queue(Pid, ChannelId, Queue, [], []).

to_queue(Pid, ChannelId, Queue, Skills) ->
	to_queue(Pid, ChannelId, Queue, Skills, []).

to_queue(Pid, ChannelId, Queue, Skills, Options) ->
	do_request(Pid, {undefined, queue_transfer, [ChannelId, Queue, Skills, Options]}).

%% @doc Get the brands.
-spec(brands/1 :: (Pid :: pid()) -> 'ok').
brands(Pid) ->
	do_request(Pid, 'GET_BRAND_LIST').

%% @doc Get the queues.
-spec(queues/1 :: (Pid :: pid()) -> 'ok').
queues(Pid) ->
	do_request(Pid, 'GET_QUEUE_LIST').

%% @doc Get the release options.
-spec(release_opts/1 :: (Pid :: pid()) -> 'ok').
release_opts(Pid) ->
	do_request(Pid, 'GET_RELEASE_OPTS').

%% @doc Get the queue transfer options.
-spec(queueing_opts/1 :: (Pid :: pid()) -> 'ok').
queueing_opts(Pid) ->
	do_request(Pid, 'GET_QUEUE_TRANSFER_OPTS').

%% @doc Get the profiles in the system.
-spec(profiles/1 :: (Pid :: pid()) -> 'ok').
profiles(Pid) ->
	do_request(Pid, 'GET_PROFILES').

%% @doc Get the available agents; avialable in this case means released or
%% idle.
-spec(agents/1 :: (Pid :: pid()) -> 'ok').
agents(Pid) ->
	do_request(Pid, 'GET_AVAIL_AGENTS').

%% @doc Test the ring a ling.
-spec(ring_test/1 :: (Pid :: pid()) -> 'ok').
ring_test(Pid) ->
	do_request(Pid, 'RING_TEST').

%% @doc Logout.  Try to leave gracefully.
-spec(logout/1 :: (Pid :: pid()) -> 'ok').
logout(Pid) ->
	do_request(Pid, 'LOGOUT').

do_request(Pid, {Module, Function, Args}) ->
	Props = [{<<"function">>, Function}],
	Props0 = case Module of
		undefined -> Props;
		_ -> [{<<"module">>, Module} | Props]
	end,
	Props1 = case Args of
		[] -> Props0;
		undefined -> Props0;
		Else -> [{<<"args">>, Else} | Props0]
	end,
	gen_server:call(Pid, {do_request, Props1});

%% @doc Make an agent request.
do_request(Pid, Request) when is_atom(Request) ->
	do_request(Pid, {undefined, Request, []}).

%% @doc Make a request to connection media.
media_command(Pid, RequestHint, CallCast, Args) ->
	gen_server:cast(Pid, {media_command, RequestHint, CallCast, Args}).

% =====
% Init
% =====

%% @hidden
init(Options) ->
	case whereis(cpxlog) of
		undefined ->
			cpxlog:start();
		_ ->
			ok
	end,
	crypto:start(),
	?INFO("The options:  ~p", [Options]),
	Username = proplists:get_value(username, Options, "agent"),
	% TODO error if no password or username is sent
	% currently like this becuase I'm lazy.
	Password = proplists:get_value(password, Options, "Password123"),
	Port = proplists:get_value(port, Options, ?port),
	Server = proplists:get_value(host, Options, "localhost"),
	Protocol = proplists:get_value(protocol, Options, tcp),
	Compression = proplists:get_value(compression, Options, none),
	Radix = proplists:get_value(radix, Options, 10),
	{ok, Socket} = case Protocol of
		ssl -> ssl:connect(Server, Port, [binary, {packet, raw}, {active, once}]);
		_ -> gen_tcp:connect(Server, Port, [binary, {packet, raw}, {active, once}])
	end,
	{Mod, Socket0} = case Protocol of
		ssl_upgrade ->
			{ok, S} = ssl:connect(Socket, [binary, {packet, raw}, {active, once}]),
			{ssl, S};
		ssl ->
			{ssl, Socket};
		tcp ->
			{gen_tcp, Socket}
	end,
	%timer:send_interval(10000, do_tick),
	?INFO("~s started.", [?MODULE]),
	OptionRec = #options{
		username = Username,
		password = Password,
		port = Port,
		server = Server,
		protocol = Protocol,
		compression = Compression,
		radix = Radix
	},
	{ok, #state{socket={Mod, Socket0}, netstring = Radix, options = OptionRec}}.

% =====
% handle_call
% =====

%% @hidden
handle_call({do_request, BaseStruct}, From, State) ->
	#state{last_req_id = LastId, requests = Reqs, socket = {Mod, Socket},
		options = Options} = State,
	#options{compression = Compression, radix = Radix} = Options,
	ReqId = case LastId of
		X when X > 999 -> 1;
		X -> X + 1
	end,
	Request = {struct, [{<<"request_id">>, ReqId} | BaseStruct]},
	?INFO("Sending request:  ~p", [Request]),
	Requests0 = dict:store(ReqId, From, Reqs),
	BigBin = iolist_to_binary(mochijson2:encode(Request)),
	Bin = case Compression of
		none -> BigBin;
		zip -> zlib:zip(BigBin);
		gzip -> zlib:gzip(BigBin)
	end,
	Mod:send(Socket, netstring:encode(Bin, Radix)),
	State0 = State#state{last_req_id = ReqId, requests = Requests0},
	{noreply, State0};
	
handle_call(Request, _From, State) ->
	{reply, {unknown_call, Request}, State}.

% =====
% handle_cast
% =====

%% @hidden
handle_cast(_, State) ->
	{noreply, State}.

% =====
% handle_info
% =====

%% @hidden
handle_info({SocketType, Socket, Bin}, #state{
	socket = {SocketMod, Socket}} = State)
	when SocketType =:= ssl, SocketMod =:= ssl;
	SocketType =:= tcp, SocketMod =:= gen_tcp ->
	NewState = handle_socket_msg(Bin, State),
	case SocketMod of
		gen_tcp ->
			inet:setopts(Socket, [{active, once}]);
		ssl ->
			ssl:setopts(Socket, [{active, once}])
	end,
	{noreply, NewState};

handle_info({tcp_closed, Socket}, #state{socket = {_Mod, Socket}} = State) ->
	?NOTICE("Server disconnected", []),
	{stop, normal, State};

handle_info(Info, State) ->
	?NOTICE("Unhandled info ~p", [Info]),
	{noreply, State}.

% =====
% terminate
% =====

%% @hidden
terminate(_Reason, #state{socket = {Mod, Socket}} = _State) ->
	Mod:close(Socket),
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

handle_socket_msg(Bin, State) ->
	#state{options = Options, netstring = NetCont} = State,
	{Bins, NetCont0} = netstring:decode(Bin, NetCont),
	{Replys, Events} = get_jsons(Bins, Options#options.compression),
	NewRequests = consume_replys(Replys, State#state.requests),
	fire_events(Events, State#state.subscribers),
	State#state{requests = NewRequests, netstring = NetCont0}.

fire_events([], _Subs) ->
	ok;
fire_events([Ev | Tail], Subs) ->
	fire_event(Ev, Subs),
	fire_events(Tail, Subs).

fire_event(_, []) ->
	ok;
fire_event(Ev, [Sub | Tail]) ->
	Sub ! {agent_event, Ev},
	fire_event(Ev, Tail).

get_jsons(Bins, Compression) ->
	InflateFunc = case Compression of
		zip -> unzip;
		gzip -> gunzip;
		none -> none
	end,
	get_jsons(Bins, InflateFunc, [], []).

get_jsons([], _Bigger, Replys, Events) ->
	{lists:reverse(Replys), lists:reverse(Events)};

get_jsons([Bin | Tail], Bigger, RepAcc, EvAcc) ->
	BigBin = case Bigger of
		none -> Bin;
		F -> zlib:F(Bin)
	end,
	?INFO("BigBin:  ~p", [BigBin]),
	{struct, Props} = Json = mochijson2:decode(BigBin),
	case proplists:get_value(<<"request_id">>, Props) of
		undefined ->
			get_jsons(Tail, Bigger, RepAcc, [Json | EvAcc]);
		_ ->
			get_jsons(Tail, Bigger, [Json | RepAcc], EvAcc)
	end.

consume_replys([], Requests) ->
	Requests;

consume_replys([{struct, Props} | Tail], Requests) ->
	ReqId = proplists:get_value(<<"request_id">>, Props),
	case dict:find(ReqId, Requests) of
		error ->
			consume_replys(Tail, Requests);
		From ->
			case proplists:get_value(<<"success">>, Props) of
				true ->
					Result = proplists:get_value(<<"result">>, Props),
					Return = {ok, Result},
					gen_server:reply(From, Return);
				_ ->
					Errcode = proplists:get_value(<<"errcode">>, Props),
					Msg = proplists:get_value(<<"message">>, Props),
					Return = {error, {Errcode, Msg}},
					gen_server:reply(From, Return)
			end,
			consume_replys(Tail, Requests)
	end.
