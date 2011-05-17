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

-module(agent_tcp_client).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

% API
% call flow
-export([
	start/1,
	start_link/1,
	idle/1,
	released/1,
	released/2,
	answer/1,
	hangup/1,
	to_agent/2,
	to_queue/2,
	to_queue/3,
	to_queue/4,
	to_other/2
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
	

% gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
		code_change/3]).

-define(Major, 0).
-define(Minor, 1).
-ifdef(TEST).
-define(port, 51337).
-else.
-define(port, 1337).
-endif.

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("cpx_agent_pb.hrl").

-record(options, {
	username,
	password,
	voipendpoint,
	voipendpoint_data,
	port,
	server,
	silent
}).
-record(state, {
		salt :: pos_integer(),
		socket :: {'gen_tcp' | 'ssl', port()},
		requests = [] :: [{non_neg_integer(), tuple()}],
		last_req_id = 0,
		options :: #options{}
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
-type(start_option() :: 
	server_option() |
	port_option() | 
	username_option() | 
	password_option() | 
	voipendpoint_option() | 
	voipendpoint_data_option() |
	silent_option()
).
-type(start_options() :: [start_option()]).

%% @doc Start the agent with the given options unlinked to a process.
-spec(start/1 :: (Options :: start_options()) -> {'ok', pid()}).
start(Options) ->
	gen_server:start(?MODULE, Options, []).

%% @doc start the conection linked to the calling process.
-spec(start_link/1 :: (Options :: start_options()) -> {'ok', pid()}).
start_link(Options) ->
	gen_server:start_link(?MODULE, Options, []).

%% @doc Attempt to go idle.
-spec(idle/1 :: (Pid :: pid()) -> 'ok').
idle(Pid) ->
	gen_server:cast(Pid, idle).

%% @doc Attempt to go released for default reason.
-spec(released/1 :: (Pid :: pid()) -> 'ok').
released(Pid) ->
	released(Pid, default).

%% @doc Attempt to go released for the specified reason.
-spec(released/2 :: (Pid :: pid(), Why :: 'default' | string()) -> 'ok').
released(Pid, Why) ->
	gen_server:cast(Pid, {released, Why}).

%% @doc Attempt to answer a call.
-spec(answer/1 :: (Pid :: pid()) -> 'ok').
answer(Pid) ->
	gen_server:cast(Pid, answer).

%% @doc Attempt to hangup a call.
-spec(hangup/1 :: (Pid :: pid()) -> 'ok').
hangup(Pid) ->
	gen_server:cast(Pid, hangup).

%% @doc Attempt to transfer a call to another agent.
-spec(to_agent/2 :: (Pid :: pid(), Agent :: string()) -> 'ok').
to_agent(Pid, Agent) ->
	gen_server:cast(Pid, {to_agent, Agent}).

%% @doc Attempt to transfer a call to a queue.
-spec(to_queue/2 :: (Pid :: pid(), Queue :: string()) -> 'ok').
to_queue(Pid, Queue) ->
	to_queue(Pid, Queue, [], []).

to_queue(Pid, Queue, Skills) ->
	to_queue(Pid, Queue, Skills, []).

to_queue(Pid, Queue, Skills, Options) ->
	gen_server:cast(Pid, {to_queue, Queue, Skills, Options}).

%% @doc Attempt to transfer a call to a 3rd party; ie warm transfer.
-spec(to_other/2 :: (Pid :: pid(), Other :: string()) -> 'ok').
to_other(Pid, Other) ->
	gen_server:cast(Pid, {to_other, Other}).

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

%% @doc Make an agent request.
do_request(Pid, Request) when is_atom(Request) ->
	gen_server:cast(Pid, {do_request, Request}).

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
	Username = proplists:get_value(username, Options, "agent"),
	% TODO error if no password or username is sent
	% currently like this becuase I'm lazy.
	Password = proplists:get_value(password, Options, "Password123"),
	Voip = case proplists:get_value(voipendpoint, Options, sip) of
		sip ->
			'SIP';
		sip_registration ->
			'SIP_REGISTRATION';
		iax ->
			'IAX';
		h323 ->
			'H323';
		pstn ->
			'PSTN'
	end,
	VoipData = proplists:get_value(voipendpoint_data, Options),
	Port = proplists:get_value(port, Options, ?port),
	Server = proplists:get_value(server, Options, "localhost"),
	Silent = proplists:get_value(silent, Options, false),
	{ok, Socket} = gen_tcp:connect(Server, Port, [binary, {packet, raw}]),
	%timer:send_interval(10000, do_tick),
	?INFO("~s started.", [?MODULE]),
	OptionRec = #options{
		username = Username,
		password = Password,
		voipendpoint = Voip,
		voipendpoint_data = VoipData,
		port = Port,
		server = Server,
		silent = Silent
	},
	{ok, #state{socket={gen_tcp, Socket}, options = OptionRec}}.

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
handle_cast(idle, #state{socket = {Mod, Sock}} = State) ->
	{NewId, Bin} = make_bin('GO_IDLE', State#state.last_req_id),
	ok = Mod:send(Sock, Bin),
	NewRequests = [{NewId, 'GO_IDLE'} | State#state.requests],
	{noreply, State#state{last_req_id = NewId, requests = NewRequests}};
handle_cast({released, Why}, #state{socket = {Mod, Socket}} = State) ->
	Req = case Why of
		default -> #goreleasedrequest{use_default = true};
		{Id, Name, Bias} ->
			#goreleasedrequest{
				use_default = false,
				release_opt = #release{
					id = Id,
					name = Name,
					bias = Bias
				}
			}
	end,
	{NewId, Bin} = make_bin(Req, State#state.last_req_id),
	ok = Mod:send(Socket, Bin),
	NewRequests = [{NewId, 'GO_RELEASED'} | State#state.requests],
	{noreply, State#state{last_req_id = NewId, requests = NewRequests}};
handle_cast(answer, #state{socket = {Mod, Socket}} = State) ->
	{NewId, Bin} = make_bin('MEDIA_ANSWER', State#state.last_req_id),
	ok = Mod:send(Socket, Bin),
	NewRequests = [{NewId, 'MEDIA_ANSWER'} | State#state.requests],
	{noreply, State#state{last_req_id = NewId, requests = NewRequests}};
handle_cast(hangup, #state{socket = {Mod, Socket}} = State) ->
	{NewId, Bin} = make_bin('MEDIA_HANGUP', State#state.last_req_id),
	ok = Mod:send(Socket, Bin),
	NewRequests = [{NewId, 'MEDIA_HANGUP'} | State#state.requests],
	{noreply, State#state{last_req_id = NewId, requests = NewRequests}};
handle_cast({to_agent, Agent}, #state{socket = {Mod, Socket}} = State) ->
	Request = #agenttransferrequest{agent_id = Agent},
	{NewId, Bin} = make_bin(Request, State#state.last_req_id),
	ok = Mod:send(Socket, Bin),
	NewRequests = [{NewId, 'AGENT_TRANSFER'} | State#state.requests],
	{noreply, State#state{last_req_id = NewId, requests = NewRequests}};
handle_cast({to_queue, Queue, Skills, Options}, #state{socket = {Mod, Socket}} = State) ->
	SkillsList = [case X of
		{Key, Val} -> #skill{atom = Key, expanded = Val};
		Val -> #skill{atom = Val}
	end || X <- Skills],
	OptionsList = [#simplekeyvalue{
		key = K,
		value = V
	} || {K, V} <- Options],
	TransferOpts = #queuetransferoptions{
		options = OptionsList,
		skills = SkillsList
	},
	Request = #queuetransferrequest{
		queue_name = Queue,
		transfer_options = TransferOpts
	},
	{NewId, Bin} = make_bin(Request, State#state.last_req_id),
	ok = Mod:send(Socket, Bin),
	NewRequests = [{NewId, 'QUEUE_TRANSFER'} | State#state.requests],
	{noreply, State#state{last_req_id = NewId, requests = NewRequests}};
handle_cast({to_other, Other}, #state{socket = {Mod, Socket}} = State) ->
	Request = #warmtransferrequest{number = Other},
	{NewId, Bin} = make_bin(Request, State#state.last_req_id),
	ok = Mod:send(Socket, Bin),
	NewRequests = [{NewId, 'WARM_TRANSFER_BEGIN'} | State#state.requests],
	{noreply, State#state{last_req_id = NewId, requests = NewRequests}};
handle_cast({do_request, Request}, #state{last_req_id = OldId, socket = {Mod, Socket}} = State) ->
	{NewId, Bin} = make_bin(Request, OldId),
	ok = Mod:send(Socket, Bin),
	NewRequests = [{NewId, Request} | State#state.requests],
	{noreply, State#state{last_req_id = NewId, requests = NewRequests}};
handle_cast(_, State) ->
	{noreply, State}.

% =====
% handle_info
% =====

%% @hidden
handle_info({tcp, Socket, Bin}, #state{socket = {_Mod, Socket}} = State) ->
	?DEBUG("In bin:  ~p", [Bin]),
	{Replys, _} = decode_bin(Bin),
	{NewRequests, TruReplys} = consume_replys(Replys, State#state.requests),
	Midstate = State#state{requests = NewRequests},
	NewState = handle_server_messages(TruReplys, Midstate),
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

make_req_id(99999) ->
	1;
make_req_id(X) ->
	X + 1.

make_bin(RequestAtom, LastReqId) when is_atom(RequestAtom) ->
	ReqId = make_req_id(LastReqId),
	Request = #agentrequest{
		request_id = ReqId,
		request_hint = RequestAtom
	},
	NetBin = make_netstring(Request),
	{ReqId, NetBin};
make_bin(Record, LastReqId) when is_record(Record, loginrequest) ->
	ReqId = make_req_id(LastReqId),
	Request = #agentrequest{
		request_id = ReqId,
		request_hint = 'LOGIN',
		login_request = Record
	},
	NetBin = make_netstring(Request),
	{ReqId, NetBin};
make_bin(Record, LastReqId) when is_record(Record, agentclientversion) ->
	ReqId = make_req_id(LastReqId),
	Request = #agentrequest{
		request_id = ReqId,
		request_hint = 'CHECK_VERSION',
		agent_client_version = Record
	},
	NetBin = make_netstring(Request),
	{ReqId, NetBin};
make_bin(Record, LastReqId) when is_record(Record, goreleasedrequest) ->
	ReqId = make_req_id(LastReqId),
	Request = #agentrequest{
		request_id = ReqId,
		request_hint = 'GO_RELEASED',
		go_released_request = Record
	},
	NetBin = make_netstring(Request),
	{ReqId, NetBin};
make_bin(Record, LastReqId) when is_record(Record, warmtransferrequest) ->
	ReqId = make_req_id(LastReqId),
	Request = #agentrequest{
		request_id = ReqId,
		request_hint = 'WARM_TRANSFER_BEGIN',
		warm_transfer_request = Record
	},
	NetBin = make_netstring(Request),
	{ReqId, NetBin};
make_bin(Record, LastReqId) when is_record(Record, agenttransferrequest) ->
	ReqId = make_req_id(LastReqId),
	Request = #agentrequest{
		request_id = ReqId,
		request_hint = 'AGENT_TRANSFER',
		agent_transfer = Record
	},
	NetBin = make_netstring(Request),
	{ReqId, NetBin};
make_bin(Record, LastReqId) when is_record(Record, queuetransferrequest) ->
	ReqId = make_req_id(LastReqId),
	Request = #agentrequest{
		request_id = ReqId,
		request_hint = 'QUEUE_TRANSFER',
		queue_transfer_request = Record
	},
	NetBin = make_netstring(Request),
	{ReqId, NetBin};
make_bin(Record, LastReqId) when is_record(Record, agentrequest) ->
	ReqId = make_req_id(LastReqId),
	Request = Record#agentrequest{
		request_id = ReqId
	},
	NetBin = make_netstring(Request),
	{ReqId, NetBin}.

make_netstring(Request) ->
	Protobuf = cpx_agent_pb:encode(Request),
	Size = list_to_binary(integer_to_list(size(Protobuf))),
	<<Size/binary, $:, Protobuf/binary, $,>>.

decode_bin(Bin) ->
	{ProtoBins, Rest} = read_tcp_strings(Bin),
	{decode_bins(ProtoBins), Rest}.

decode_bins(Bins) ->
	decode_bins(Bins, []).

decode_bins([], Acc) ->
	lists:reverse(Acc);
decode_bins([Head | Tail], Acc) ->
	decode_bins(Tail, [cpx_agent_pb:decode_servermessage(Head) | Acc]).
	
read_tcp_strings(Bin) ->
	read_tcp_strings(Bin, []).

read_tcp_strings(Bin, Acc) ->
	case read_tcp_string(Bin) of
		nostring ->
			{lists:reverse(Acc), Bin};
		{String, Rest} ->
			read_tcp_strings(Rest, [String | Acc])
	end.

read_tcp_string(Bin) ->
	read_tcp_string(Bin, []).

read_tcp_string(<<$:, Bin/binary>>, RevSize) ->
	Size = list_to_integer(lists:reverse(RevSize)),
	case Bin of
		<<String:Size/binary, $,, Rest/binary>> ->
			{String, Rest};
		_ ->
			nostring
	end;
read_tcp_string(<<>>, _Acc) ->
	nostring;
read_tcp_string(<<Char/integer, Rest/binary>>, Acc) ->
	read_tcp_string(Rest, [Char | Acc]).

consume_replys(Replys, Requests) ->
	consume_replys(Replys, [], Requests).

consume_replys([], AccReplys, Requests) ->
	{Requests, lists:reverse(AccReplys)};
consume_replys([Head | Tail], AccReplys, Requests) ->
	NewRequests = consume_reply(Head, Requests),
	consume_replys(Tail, [Head | AccReplys], NewRequests).

consume_reply(#servermessage{type_hint = 'REPLY', reply = ReplyRec}, Requests) ->
	consume_reply(ReplyRec, Requests);
consume_reply(#servermessage{type_hint = 'EVENT', event = Event}, Requests) ->
	?INFO("Got server event ~p", [Event]),
	Requests;
consume_reply(#serverreply{request_id = ReqId, request_hinted = Hint} = Request, RequestList) ->
	NewReqs = case proplists:get_value(ReqId, RequestList) of
		undefined ->
			?WARNING("The request id ~s was not expected.", [ReqId]),
			RequestList;
		Hint ->
			proplists:delete(ReqId, RequestList);
		Other ->
			?WARNING("Reply hint ~s is not expected hint ~s", [Hint, Other]),
			RequestList
	end,
	case Request#serverreply.success of
		false ->
			?ERROR("(~s) (~s) ~s", [Hint, Request#serverreply.error_code, Request#serverreply.error_message]);
		true ->
			?NOTICE("(~s) ~p", [Hint, Request])
	end,
	NewReqs.

handle_server_messages([], State) ->
	State;
handle_server_messages([Head | Tail], State) ->
	NewState = handle_server_message(Head, State),
	handle_server_messages(Tail, NewState).

handle_server_message(#servermessage{type_hint = 'REPLY', reply = Reply}, State) ->
	handle_server_message(Reply, State);
handle_server_message(#servermessage{type_hint = 'EVENT', event = Event}, State) ->
	handle_server_message(Event, State);
handle_server_message(#serverreply{success = false} = Reply, State) ->
	?WARNING("Request ~B of type ~s failed due to ~p", [Reply#serverreply.request_id, Reply#serverreply.request_hinted, Reply#serverreply.error_message]),
	State;
handle_server_message(#serverreply{request_hinted = Hint} = Reply, #state{socket = {Mod, Socket}} = State) ->
	case Hint of
		'CHECK_VERSION' ->
			?INFO("Check version passed, getting salt next", []),
			{NewId, Bin} = make_bin('GET_SALT', State#state.last_req_id),
			ok = Mod:send(Socket, Bin),
			NewRequests = [{NewId, 'GET_SALT'} | State#state.requests],
			State#state{requests = NewRequests};
		'GET_SALT' ->
			?INFO("Got a salt, now to login.", []),
			Options = State#state.options,
			#saltreply{salt = Salt, pubkey_e = E, pubkey_n = N} = Reply#serverreply.salt_and_key,
			Password = crypto:rsa_public_encrypt(list_to_binary(Salt ++ Options#options.password), [crypto:mpint(list_to_integer(E)), crypto:mpint(list_to_integer(N))], rsa_pkcs1_padding),
			Username = Options#options.username,
			VoipEndPointData = Options#options.voipendpoint_data,
			Voipendpoint = Options#options.voipendpoint,
			LoginRequest = #loginrequest{
				username = Username,
				password = Password,
				voipendpoint = Voipendpoint,
				voipendpointdata = VoipEndPointData
			},
			{NewId, Bin} = make_bin(LoginRequest, State#state.last_req_id),
			ok = gen_tcp:send(State#state.socket, Bin),
			NewRequests = [{NewId, 'LOGIN'} | State#state.requests],
			State#state{requests = NewRequests};
		'LOGIN' ->
			?INFO("Login successful!  Yays!  ~p", [Reply]),
			State;
		'GO_IDLE' ->
			?INFO("idle-icious!", []),
			State;
		'GO_RELEASED' ->
			?INFO("Released (the hounds?)", []),
			State;
		_ ->
			?DEBUG("nothing much to be done for other events", []),
			State
	end;
handle_server_message(Event, #state{socket = {_Mod, Socket}} = State) ->
	case Event#serverevent.command of
		'ASTATE' ->
			?INFO("State change to ~p", [Event#serverevent.state_change]),
			case Event#serverevent.state_change of
				#statechange{ agent_state = 'PRELOGIN', ssl_upgrade = SSLUpgrade} ->
					Ver = #agentclientversion{
						major = ?Major,
						minor = ?Minor
					},
					{NewId, Bin} = make_bin(Ver, State#state.last_req_id),
					{NewMod,NewSock} = case SSLUpgrade of
						true ->
							{ok, SSLSock} = ssl:connect(Socket, [
								%{cacertfile, "cacerts.pem"},
								{certfile, "cert.pem"},
								{keyfile, "key.pem"}
							]),
							{ssl, SSLSock};
						_ ->
						{gen_tcp, Socket}
					end,
					ok = NewMod:send(NewSock, Bin),
					?DEBUG("post send", []),
					State#state{socket = {NewMod, NewSock}, last_req_id = NewId, requests = [{NewId, 'CHECK_VERSION'} | State#state.requests]};
			_ ->
				State
			end;
		'APROFILE' ->
			?INFO("Profile change to ~s", [Event#serverevent.profile]),
			State;
		'ABLAB' ->
			?INFO("Blab:  ~s", [Event#serverevent.text_message]),
			State;
		'AURLPOP' ->
			?INFO("Url pop ~s in window ~s", [Event#serverevent.url, Event#serverevent.url_window]),
			State;
		'MEDIA_EVENT' ->
			?INFO("Media event:  ~p", [Event#serverevent.media_event]),
			State
	end.

-ifdef(TEST).

read_tcp_strings_test_() ->
	[?_assertEqual({[<<"hi">>], <<>>}, read_tcp_strings(<<"2:hi,">>)),
	?_assertEqual({[<<"hi">>,<<"bye">>], <<>>}, read_tcp_strings(<<"2:hi,3:bye,">>)),
	?_assertEqual({[<<"hi">>], <<"10:rest">>}, read_tcp_strings(<<"2:hi,10:rest">>)),
	?_assertEqual({[<<"hi">>], <<$1>>}, read_tcp_strings(<<"2:hi,1">>)),
	?_assertEqual({[<<"hi">>,<<"bye">>], <<"garbage">>}, read_tcp_strings(<<"2:hi,3:bye,garbage">>))].

-endif.
