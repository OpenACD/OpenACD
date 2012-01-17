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

%% @doc The connection handler that communicates with a client UI; in this 
%% case a tcp client using the protobufs defined in cpx_agent.proto.
%% @see agent_tcp_listener

-module(agent_tcp_connection).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-export([start/3, start_link/3, negotiate/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
		code_change/3]).

-define(Major, 0).
-define(Minor, 1).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("queue.hrl").
-include("cpx_agent_pb.hrl").

% {Counter, Event, Data, now()}
% TODO if unacked events become and issue, worry.  For now there are many
% other systems that catch deadified agents, killing the fsm, which will
% kill this.
%-type(unacked_event() :: {pos_integer(), string(), string(), {pos_integer(), pos_integer(), pos_integer()}}).
-type(socket_module() :: 'gen_tcp' | 'ssl').
-type(socket_type() :: 'gen_tcp' | 'ssl_upgrade').
-record(state, {
		salt :: pos_integer(),
		socket :: {socket_module(), port()},
		radix :: integer(),
		agent_login :: string(),
		agent_fsm :: pid(),
		%send_queue = [] :: [string()],
		%counter = 1 :: pos_integer(),
		%unacked = [] :: [unacked_event()],
		%resent = [] :: [unacked_event()],
		%resend_counter = 0 :: non_neg_integer(),
		securitylevel = agent :: 'agent' | 'supervisor' | 'admin',
		state,
		statedata,
		mediaload,
		socket_upgrade = 'never' :: 'never' | 'ssl_upgrade'
	}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

% =====
% API
% =====

%% @doc start the conection unlinked on the given Socket to be changd or 
%% not depending on SocketType.  This is usually done by agent_tcp_listener.
-spec(start/3 :: (Socket :: port(), Radix :: integer(), SocketType :: socket_type()) -> {'ok', pid()}).
start(Socket, Radix, SocketType) ->
	gen_server:start(?MODULE, [Socket, Radix, SocketType], []).

%% @doc start the conection linked on the given Socket to be changed or not
%% depending on the SocketType.  This is usually done by agent_tcp_listener.
-spec(start_link/3 :: (Socket :: port(), Radix :: integer(), SocketType :: socket_type()) -> {'ok', pid()}).
start_link(Socket, Radix, SocketType) ->
	gen_server:start_link(?MODULE, [Socket, Radix, SocketType], []).

%% @doc Notify the client that it should begin the login proceedure.  If 
%% the socket is supposed to be upgraded to ssl, this is the time it is 
%% done.
-spec(negotiate/1 :: (Pid :: pid()) -> 'ok').
negotiate(Pid) ->
	gen_server:cast(Pid, negotiate).

% =====
% Init
% =====

%% @hidden
init([Socket, Radix, SocketType]) ->
	{SocketModule, Upgrade} = case SocketType of
		gen_tcp -> {gen_tcp, never};
		ssl_upgrade -> {gen_tcp, ssl_upgrade}
	end,
	%timer:send_interval(10000, do_tick),
	{ok, #state{socket={SocketModule, Socket}, radix = Radix, socket_upgrade = Upgrade}}.

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
% negotiate the client's protocol version and such
handle_cast(negotiate, #state{socket_upgrade = SockUpgrade} = State) ->
	?DEBUG("starting negotiation...", []),
	ok = inet:setopts(element(2, State#state.socket), [{packet, raw}, binary, {active, once}]),
	Statechange = case SockUpgrade of
		ssl_upgrade -> #statechange{ agent_state = 'PRELOGIN', ssl_upgrade = true};
		_ -> #statechange{ agent_state = 'PRELOGIN'}
	end,
	Command = #serverevent{
		command = 'ASTATE',
		state_change = Statechange
	},
	server_event(State#state.socket, Command, State#state.radix),
	{O, NewSocket} = case SockUpgrade of
		ssl_upgrade ->
			inet:setopts(element(2, State#state.socket), [{active, false}]),
			%{ok, CaCertFile} = cpx:get_env(cacertfile, "cacertfile.pem"),
			CertFile = util:get_certfile(),
			Keyfile = util:get_keyfile(),
			{ok, SSLSocket} = ssl:ssl_accept(element(2, State#state.socket), [
			%	{cacertfile, CaCertFile},
				{certfile, CertFile},
				{keyfile, Keyfile}
			]),
			Data = ssl:recv(SSLSocket, 0),
			ssl:setopts(SSLSocket, [{active, once}]),
			{Data, {ssl, SSLSocket}};
		_ ->
			Data = gen_tcp:recv(element(2, State#state.socket), 0), % TODO timeout
			inet:setopts(element(2, State#state.socket), [{active, once}]),
			{Data, State#state.socket}
	end,
	case O of
		{ok, Packet} ->
			?DEBUG("packet: ~p.~n", [Packet]),
			{_Rest, Bins} = protobuf_util:netstring_to_bins(Packet),
			NewState = service_requests(Bins, State#state{socket = NewSocket, socket_upgrade = never}),
			{noreply, NewState};
		Else ->
			?DEBUG("O was ~p", [Else]),
			{noreply, State#state{socket = NewSocket}}
	end;
handle_cast({change_state, Statename, Statedata}, State) ->
	Statechange = case Statename of
		idle -> #statechange{ agent_state = 'IDLE'};
		ringing -> #statechange{
			agent_state = 'RINGING',
			call_record = protobuf_util:call_to_protobuf(Statedata)
		};
		oncall -> #statechange{
			agent_state = 'ONCALL',
			call_record = protobuf_util:call_to_protobuf(Statedata)
		};
		wrapup -> #statechange{
			agent_state = 'WRAPUP',
			call_record = protobuf_util:call_to_protobuf(Statedata)
		};
		warmtransfer -> #statechange{
			agent_state = 'WARMTRANSFER',
			call_record = protobuf_util:call_to_protobuf(element(2, Statedata)),
			warm_transfer_number = element(4, Statedata)
		};
		precall -> #statechange{
			agent_state = 'PRECALL',
			client = protobuf_util:call_to_protobuf(Statedata)
		};
		outgoing -> #statechange{
			agent_state = 'OUTGOING',
			call_record = protobuf_util:call_to_protobuf(Statedata)
		};
		released -> #statechange{
			agent_state = 'RELEASED',
			release = protobuf_util:release_to_protobuf(Statedata)
		}
	end,
	Command = #serverevent{
		command = 'ASTATE',
		state_change = Statechange
	},
	server_event(State#state.socket, Command, State#state.radix),
	{noreply, State#state{state = Statename, statedata = Statedata}};
handle_cast({change_state, Statename}, State) ->
	Statechange = #statechange{
		agent_state = protobuf_util:statename_to_enum(Statename)
	},
	Command = #serverevent{
		command = 'ASTATE',
		state_change = Statechange
	},
	server_event(State#state.socket, Command, State#state.radix),
	{noreply, State#state{state = Statename}};
handle_cast({mediaload, Callrec}, State) ->
	% TODO implement
	?INFO("mediaload nyi (~p)", [Callrec]),
	{noreply, State#state{mediaload = []}};
handle_cast({mediaload, _Callrec, Options}, State) ->
	{noreply, State#state{mediaload = Options}};
handle_cast({mediapush, Callrec, Data}, State) ->
	Newpush = translate_media_push(Callrec, Data, State),
	case Newpush of
		false -> 
			?INFO("Not forwarding non-protobuf-able tuple ~p", [Data]),
			ok;
		_ ->
			Command = #serverevent{
				command = 'MEDIA_EVENT',
				media_event = Newpush
			},
			%server_event(State#state.socket, Command, State#state.radix);
			media_event(State#state.socket, Command, Callrec, State#state.radix)
	end,
	{noreply, State};
handle_cast({set_salt, Salt}, State) ->
	{noreply, State#state{salt = Salt}};
handle_cast({change_profile, Profile}, State) ->
	Command = #serverevent{
		command = 'APROFILE',
		profile = Profile
	},
	server_event(State#state.socket, Command, State#state.radix),
	{noreply, State};
handle_cast({url_pop, Url, Name}, State) ->
	Command = #serverevent{
		command = 'AURLPOP',
		url = Url,
		url_window = Name
	},
	server_event(State#state.socket, Command, State#state.radix),
	{noreply, State};
handle_cast({blab, Text}, State) ->
	Command = #serverevent{
		command = 'ABLAB',
		text_message = Text
	},
	server_event(State#state.socket, Command, State#state.radix),
	{noreply, State};
handle_cast({plugin_event, Binary}, State) ->
	send(State#state.socket, Binary, State#state.radix),
	{noreply, State};

handle_cast(Msg, State) ->
	?DEBUG("Unhandled msg:  ~p", [Msg]),
	{noreply, State}.

% =====
% handle_info
% =====

%% @hidden
handle_info({tcp, Socket, Packet}, #state{socket = {_Mod, Socket}} = State) ->
	?DEBUG("got packet ~p", [Packet]),
	{_Rest, Bins} = protobuf_util:netstring_to_bins(Packet, State#state.radix),
	NewState = service_requests(Bins, State),
	ok = inet:setopts(Socket, [{active, once}]),
	{noreply, NewState};
handle_info({tcp_closed, Socket}, #state{socket = {_Mod, Socket}} = State) ->
	{stop, tcp_closed, State};
handle_info({ssl, Socket, Packet}, #state{socket = {_Mod, Socket}} = State) ->
	?DEBUG("got packet ~p", [Packet]),
	{_Rest, Bins} = protobuf_util:netstring_to_bins(Packet, State#state.radix),
	NewState = service_requests(Bins, State),
	ssl:setopts(Socket, [{active, once}]),
	{noreply, NewState};
handle_info({ssl_closed, Socket}, #state{socket = {_Mod, Socket}} = State) ->
	{stop, ssl_closed, State};
handle_info(Info, State) ->
	?DEBUG("Unhandled info ~p", [Info]),
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

translate_media_push(#call{type = voice} = Callrec, Data, State) when is_atom(Data) ->
	NewData = case Data of
		'3rd_party' -> 'THIRD_PARTY';
		caller_hold -> 'CALLER_ONHOLD';
		_ -> list_to_atom(string:to_upper(atom_to_list(Data)))
	end,
	{ok, Out} = cpx_freeswitch_pb:set_extension(#mediaevent{}, freeswitch_event, NewData),
	Out;
translate_media_push(_,_,_) ->
	false.

media_event({Mod,Socket}, Command, Callrec, Radix) ->
	BaseOutrec = #servermessage{
		type_hint = 'EVENT',
		event = Command
	},
	% TODO ugh, hard coding
	Bin = case Callrec#call.type of
		voice -> cpx_freeswitch_pb:encode(BaseOutrec);
		_ -> cpx_agent_pb:encode(BaseOutrec)
	end,
	Outbin = protobuf_util:bin_to_netstring(Bin,Radix),
	ok = Mod:send(Socket, Outbin).

server_event(Socket, Record, Radix) ->
	Outrec = #servermessage{
		type_hint = 'EVENT',
		event = Record
	},
	send(Socket, Outrec, Radix).

send(Target, Record, Radix) when is_record(Record, servermessage) ->
	Bin = cpx_agent_pb:encode(Record),
	send(Target, Bin, Radix);
send({Mod, Socket}, Bin, Radix) when is_binary(Bin) ->
	Outbin = protobuf_util:bin_to_netstring(Bin, Radix),
	ok = Mod:send(Socket, Outbin).

service_requests([], State) ->
	State;
service_requests([Head | Tail], State) ->
	NewState = service_request(Head, State),
	service_requests(Tail, NewState).

service_request(Bin, State) when is_binary(Bin) ->
	#agentrequest{request_id = ReqId, request_hint = Hint} = Request = cpx_agent_pb:decode_agentrequest(Bin),
	BaseReply = #serverreply{
		request_id = ReqId,
		request_hinted = Hint,
		success = false
	},
	{NewReply, NewState} = service_request(Request, BaseReply, State),
	Message = if
		is_binary(NewReply) -> NewReply;
		true -> #servermessage{ type_hint = 'REPLY', reply = NewReply }
	end,
	send(State#state.socket, Message, State#state.radix),
	NewState.

service_request(#agentrequest{request_hint = 'CHECK_VERSION', agent_client_version = Ver} = _Request, BaseReply, State) ->
	case Ver of
		#agentclientversion{major = ?Major, minor = ?Minor} ->
			{BaseReply#serverreply{success = true}, State};
		#agentclientversion{major = ?Major} ->
			?WARNING("A client is connecting with a minor version mismatch.", []),
			BaseReply#serverreply{
				success = true,
				error_message = "minor version mismatch"
			};
		_ ->
			?ERROR("A client is connection with a major version mismtach", []),
			{BaseReply#serverreply{
				error_message = "major version mismatch",
				error_code = "VERSION_MISMATCH"
			}, State}
	end;
service_request(#agentrequest{request_hint = 'GET_SALT'}, BaseReply, State) ->
	Salt = integer_to_list(crypto:rand_uniform(0, 4294967295)),
	[E, N] = util:get_pubkey(),
	SaltReply = #saltreply{
		salt = Salt,
		pubkey_e = integer_to_list(E),
		pubkey_n = integer_to_list(N)
	},
	Reply = BaseReply#serverreply{
		success = true,
		salt_and_key = SaltReply
	},
	{Reply, State#state{salt = Salt}};
service_request(#agentrequest{request_hint = 'LOGIN', login_request = LoginRequest}, BaseReply, #state{socket = {ssl, _}} = State) ->
	DecryptedPass = LoginRequest#loginrequest.password,
	case agent_auth:auth(LoginRequest#loginrequest.username, DecryptedPass) of
		deny ->
			Reply = BaseReply#serverreply{
				error_message = "invalid username or password",
				error_code = "INVALID_LOGIN"
			},
			{Reply, State};
		{allow, Id, Skills, Security, Profile} ->
			EndpointType = case LoginRequest#loginrequest.voipendpoint of
				'SIP' -> sip;
				'SIP_REGISTRATION' -> sip_registration;
				'IAX' -> iax;
				'H323' -> h323;
				'PSTN' -> pstn
			end,
			ProtoEndpointdata = LoginRequest#loginrequest.voipendpointdata,
			Persistance = LoginRequest#loginrequest.use_persistent_ring,
			Persisty = case LoginRequest#loginrequest.use_persistent_ring of
				true -> persistent;
				_ -> transient
			end,
			Username = LoginRequest#loginrequest.username,
			{Endpoint, Endpointdata} = case {EndpointType, ProtoEndpointdata, Persistance} of
				{undefined, _, true} ->
					%{{persistent, sip_registration}, Username};
					{{undefined, persistent, sip_registration}, Username};
				{undefined, _, _} ->
					%{sip_registration, Username};
					{{undefined, transient, sip_registration}, Username};
				{sip_registration, undefined, true} ->
					%{{persistent, sip_registration}, Username};
					{{undefined, persistent, sip_registration}, Username};
				{sip_registration, undefined, false} ->
					%{sip_registration, Username};
					{{undefined, transient, sip_registration}, Username};
				{EndpointType, _, true} ->
					{{undefined, persistent, EndpointType}, ProtoEndpointdata};
				{EndpointType, _, _} ->
					{{undefined, transient, EndpointType}, ProtoEndpointdata}
			end,
			Agent = #agent{
				id = Id, 
				defaultringpath = outband, 
				login = LoginRequest#loginrequest.username, 
				skills = Skills, 
				profile=Profile, 
				password=DecryptedPass,
				endpointtype = Endpoint,
				endpointdata = Endpointdata,
				security_level = Security
			},
			case agent_manager:start_agent(Agent) of
				{ok, Pid} ->
					agent:set_endpoint(Pid, EndpointType, Endpointdata, Persisty),
					ok = agent:set_connection(Pid, self()),
					RawQueues = call_queue_config:get_queues(),
					RawBrands = call_queue_config:get_clients(),
					RawReleases = agent_auth:get_releases(),
					Releases = [protobuf_util:release_to_protobuf({X#release_opt.id, X#release_opt.label, X#release_opt.bias}) || X <- RawReleases],
					Queues = protobuf_util:proplist_to_protobuf([{X#call_queue.name, X#call_queue.name} || X <- RawQueues]),
					Brands = protobuf_util:proplist_to_protobuf([{X#client.id, X#client.label} || X <- RawBrands, X#client.id =/= undefined]),
					Reply = BaseReply#serverreply{
						success = true,
						release_opts = Releases,
						queues = Queues,
						brands = Brands
					},
					{Reply, State#state{agent_login = LoginRequest#loginrequest.username, agent_fsm = Pid, securitylevel = Security}};
				{exists, _Pid} ->
					Reply = BaseReply#serverreply{
						error_message = "Multiple login detected",
						error_code = "MULTIPLE_LOGINS"
					},
					{Reply, State}
			end
	end;
service_request(#agentrequest{request_hint = 'LOGIN', login_request = LoginRequest}, BaseReply, State) ->
	Salt = State#state.salt,
	CryptedPass = list_to_binary(LoginRequest#loginrequest.password),
	case util:decrypt_password(CryptedPass) of
		{ok, Decrypted} ->
			case string:substr(Decrypted, 1, length(Salt)) of
				Salt ->
					DecryptedPass = string:substr(Decrypted, length(Salt) + 1),
					case agent_auth:auth(LoginRequest#loginrequest.username, DecryptedPass) of
						deny ->
							Reply = BaseReply#serverreply{
								error_message = "invalid username or password",
								error_code = "INVALID_LOGIN"
							},
							{Reply, State};
						{allow, Id, Skills, Security, Profile} ->
							Agent = #agent{
								id = Id, 
								defaultringpath = outband, 
								login = LoginRequest#loginrequest.username, 
								skills = Skills, 
								profile=Profile, 
								password=DecryptedPass,
								security_level = Security
							},
							case agent_manager:start_agent(Agent) of
								{ok, Pid} ->
									ok = agent:set_connection(Pid, self()),
									RawQueues = call_queue_config:get_queues(),
									RawBrands = call_queue_config:get_clients(),
									RawReleases = agent_auth:get_releases(),
									Releases = [protobuf_util:release_to_protobuf({X#release_opt.id, X#release_opt.label, X#release_opt.bias}) || X <- RawReleases],
									Queues = protobuf_util:proplist_to_protobuf([{X#call_queue.name, X#call_queue.name} || X <- RawQueues]),
									Brands = protobuf_util:proplist_to_protobuf([{X#client.id, X#client.label} || X <- RawBrands, X#client.id =/= undefined]),
									Reply = BaseReply#serverreply{
										success = true,
										release_opts = Releases,
										queues = Queues,
										brands = Brands
									},
									{Reply, State#state{agent_login = LoginRequest#loginrequest.username, agent_fsm = Pid, securitylevel = Security}};
								{exists, _Pid} ->
									Reply = BaseReply#serverreply{
										error_message = "Multiple login detected",
										error_code = "MULTIPLE_LOGINS"
									},
									{Reply, State}
							end
					end;
				NotSalt ->
					?WARNING("Salt matching failed (~s)", [NotSalt]),
					Reply = BaseReply#serverreply{
						error_message = "Invalid username or password",
						error_code = "INVALID_LOGIN"
					},
					{Reply, State}
			end;
		{error, decrypt_failed} ->
			?WARNING("decrypt of ~p failed", [LoginRequest#loginrequest.password]),
			Reply = BaseReply#serverreply{
				error_message = "decrypt_failed",
				error_code = "DECRYPT_FAILED"
			},
			{Reply, State}
	end;
service_request(#agentrequest{request_hint = 'GO_IDLE'}, BaseReply, State) ->
	case agent:set_state(State#state.agent_fsm, idle) of
		ok ->
			Reply = BaseReply#serverreply{success = true},
			{Reply, State};
		invalid ->
			Reply = BaseReply#serverreply{
				error_message = "Invalid state change",
				error_code = "INVALID_STATE_CHANGE"
			},
			{Reply, State}
	end;
service_request(#agentrequest{request_hint = 'GO_RELEASED', go_released_request = ReleaseReq}, BaseReply, State) ->
	ReleaseReason = case ReleaseReq#goreleasedrequest.use_default of
		false ->
			Release = ReleaseReq#goreleasedrequest.release_opt,
			{Release#release.id, Release#release.name, Release#release.bias};
		true ->
			default
	end,
	case agent:set_state(State#state.agent_fsm, released, ReleaseReason) of
		queued ->
			{BaseReply#serverreply{success = true}, State};
		ok ->
			{BaseReply#serverreply{success = true}, State};
		invalid ->
			Reply = BaseReply#serverreply{
				error_message = "invalid state change",
				error_code = "INVALID_STATE_CHANGE"
			},
			{Reply, State}
	end;
service_request(#agentrequest{request_hint = 'GET_BRAND_LIST'}, BaseReply, State) ->
	RawBrands = call_queue_config:get_clients(),
	Brands = [#simplekeyvalue{key = X#client.id, value = X#client.label} || X <- RawBrands, X#client.label =/= undefined],
	Reply = BaseReply#serverreply{
		brands = Brands,
		success = true
	},
	{Reply, State};
service_request(#agentrequest{request_hint = 'GET_QUEUE_LIST'}, BaseReply, State) ->
	RawQueues = call_queue_config:get_queues(),
	Queues = [#simplekeyvalue{key = Name, value = Name} || #call_queue{name = Name} <- RawQueues],
	Reply = BaseReply#serverreply{
		queues = Queues,
		success = true
	},
	{Reply, State};
service_request(#agentrequest{request_hint = 'GET_QUEUE_TRANSFER_OPTS'}, BaseReply, State) ->
	{ok, {Prompts, Skills}} = cpx:get_env(transferprompt, {[], []}),
	Opts = [{simplekeyvalue, Name, Label} || {Name, Label, _} <- Prompts],
	SkillOpts = [protobuf_util:skill_to_protobuf(X) || X <- Skills],
	TransferOpts = #queuetransferoptions{
		options = Opts,
		skills = SkillOpts
	},
	Reply = BaseReply#serverreply{
		success = true,
		queue_transfer_opts = TransferOpts
	},
	{Reply, State};
service_request(#agentrequest{request_hint = 'GET_PROFILES'}, BaseReply, State) ->
	RawProfiles = agent_auth:get_profiles(),
	Profiles = [X#agent_profile.name || X <- RawProfiles],
	Reply = BaseReply#serverreply{
		success = true,
		profiles = Profiles
	},
	{Reply, State};
service_request(#agentrequest{request_hint = 'GET_AVAIL_AGENTS'}, BaseReply, State) ->
	RawAgents = agent_manager:list(),
	AgentStates = [agent:dump_state(Pid) || {_K, {Pid, _Id, _Time, _Skills}} <- RawAgents],
	Agents = [{availagent, X#agent.login, X#agent.profile, state_to_pb(X#agent.state)} || X <- AgentStates],
	Reply = BaseReply#serverreply{
		success = true,
		agents = Agents
	},
	{Reply, State};
service_request(#agentrequest{request_hint = 'GET_RELEASE_OPTS'}, BaseReply, State) ->
	RawOpts = agent_auth:get_releases(),
	Opts = [protobuf_util:release_to_protobuf(X) || X <- RawOpts],
	Reply = BaseReply#serverreply{
		success = true,
		release_opts = [protobuf_util:release_to_protobuf(default) | Opts]
	},
	{Reply, State};
service_request(#agentrequest{request_hint = 'MEDIA_HANGUP'}, BaseReply, #state{statedata = C} = State) when is_record(C, call) ->
	Reply = case agent:set_state(State#state.agent_fsm, {wrapup, C}) of
		invalid ->
			BaseReply#serverreply{
				error_message = "invalid state change",
				error_code = "INVALID_STATE_CHANGE"
			};
		ok ->
			BaseReply#serverreply{
				success = true
			}
	end,
	{Reply, State};
service_request(#agentrequest{request_hint = 'RING_TEST'}, BaseReply, State) ->
	Reply = case whereis(freeswitch_media_manager) of
		undefined ->
			BaseReply#serverreply{
				error_message = "freeswtich isn't available",
				error_code = "MEDIA_NOEXISTS"
			};
		_Pid ->
			case agent:dump_state(State#state.agent_fsm) of
				#agent{state = released} = AgentRec ->
					Callrec = #call{id = "unused", source = self(), callerid= {"Echo test", "0000000"}},
					case freeswitch_media_manager:ring_agent_echo(State#state.agent_fsm, AgentRec, Callrec, 600) of
						{ok, _} ->
							BaseReply#serverreply{success = true};
						{error, Error} ->
							BaseReply#serverreply{
								error_message = io_lib:format("ring test failed:  ~p", [Error]),
								error_code = "UNKNOWN_ERROR"
							}
					end;
				_ ->
					BaseReply#serverreply{
						error_message = "must be released to do a ring test",
						error_code = "INVALID_STATE"
					}
			end
	end,
	{Reply, State};
%service_request(#agentrequest{request_hint = 'WARM_TRANSFER_BEGIN', warm_transfer_request = WarmTransRequest}, BaseReply, #state{state = oncall, statedata = C} = State) when is_record(C, call) ->
%	Number = WarmTransRequest#warmtransferrequest.number,
%	Reply = case gen_media:warm_transfer_begin(C#call.source, Number) of
%		ok ->
%			BaseReply#serverreply{success = true};
%		invalid ->
%			BaseReply#serverreply{
%				error_message = "media denied transfer",
%				error_code = "INVALID_MEDIA_CALL"
%			}
%	end,
%	{Reply, State};
%service_request(#agentrequest{request_hint = 'WARM_TRANSFER_COMPLETE'}, BaseReply, #state{state = warmtransfer} = State) ->
%	Call = State#state.statedata,
%	Reply = case gen_media:warm_transfer_complete(Call#call.source) of
%		ok ->
%			BaseReply#serverreply{success = true};
%		invalid ->
%			BaseReply#serverreply{
%				error_message = "media denied transfer",
%				error_code = "INVALID_MEDIA_CALL"
%			}
%	end,
%	{Reply, State};
%service_request(#agentrequest{request_hint = 'WARM_TRANSFER_CANCEL'}, BaseReply, #state{state = warmtransfer} = State) ->
%	Call = State#state.statedata,
%	Reply = case gen_media:warm_transfer_cancel(Call#call.source) of
%		ok ->
%			BaseReply#serverreply{success = true};
%		invalid ->
%			BaseReply#serverreply{
%				error_message = "media denied transfer",
%				error_code = "INVALID_MEDIA_CALL"
%			}
%	end,
%	{Reply, State};
service_request(#agentrequest{request_hint = 'LOGOUT'}, BaseReply, State) ->
	gen_tcp:close(State#state.socket),
	%% TODO Not a very elegant shutdown from this point onward...
	Reply = BaseReply#serverreply{success = true},
	{Reply, State};
service_request(#agentrequest{request_hint = 'DIAL', dial_request = Number}, BaseReply, #state{state = precall} = State) ->
	Call = State#state.statedata,
	Reply = case Call#call.direction of
		outbound ->
			case gen_media:call(Call#call.source, {dial, Number}) of
				ok ->
					BaseReply#serverreply{success = true};
				{error, Error} ->
					?NOTICE("Outbound call error ~p", [Error]),
					BaseReply#serverreply{
						error_message = io_lib:format("Error:  ~p", [Error]),
						error_code = "UNKNOWN_ERROR"
					}
			end;
		_ ->
			BaseReply#serverreply{
				error_message = "invalid call direction",
				error_code = "INVALID_MEDIA_CALL"
			}
	end,
	{Reply, State};
service_request(#agentrequest{request_hint = 'AGENT_TRANSFER', agent_transfer = Transfer}, BaseReply, #state{state = oncall, statedata = Call} = State) ->
	case Transfer#agenttransferrequest.other_data of
		undefined ->
			ok;
		Caseid ->
			gen_media:cast(Call#call.source, {set_caseid, Caseid})
	end,
	Reply = case agent_manager:query_agent(Transfer#agenttransferrequest.agent_id) of
		{true, Target} ->
			case agent:agent_transfer(State#state.agent_fsm, Target) of
				ok ->
					BaseReply#serverreply{success = true};
				invalid ->
					BaseReply#serverreply{
						error_message = "invalid state change",
						error_code = "INVALID_STATE_CHANGE"
					}
			end;
		false ->
			BaseReply#serverreply{
				error_message = "no such agent",
				error_code = "AGENT_NOEXISTS"
			}
	end,
	{Reply, State};
service_request(
	#agentrequest{
		request_hint = 'MEDIA_COMMAND', 
		media_command_request = Command}, 
	BaseReply, 
	#state{statedata = C} = State) when is_record(C, call) ->
	Reply = case Command#mediacommandrequest.need_reply of
		true ->
			try gen_media:call(C#call.source, Command) of
				invalid ->
					BaseReply#serverreply{
						error_message = "invalid media call",
						error_code = "INVALID_MEDIA_CALL"
					};
				Response when is_tuple(Response) andalso element(1, Response) == mediacommandreply ->
					BaseReply#serverreply{
						success = true,
						media_command_reply = Response
					}
			catch
				exit:{noproc, _} ->
					?DEBUG("Media no longer exists", []),
					BaseReply#serverreply{
						error_message = "media no longer exists",
						error_code = "MEDIA_NOEXISTS"
					}
			end;
		false ->
			gen_media:cast(C#call.source, Command),
			BaseReply#serverreply{success = true}
	end,
	{Reply, State};
service_request(#agentrequest{request_hint = 'QUEUE_TRANSFER', queue_transfer_request = Trans}, BaseReply, #state{statedata = C} = State) when is_record(C, call) ->
	TransOpts = Trans#queuetransferrequest.transfer_options,
	gen_media:set_url_getvars(C#call.source, TransOpts#queuetransferoptions.options),
	Skills = convert_skills(TransOpts#queuetransferoptions.skills),
	gen_media:add_skills(C#call.source, Skills),
	Reply = case agent:queue_transfer(State#state.agent_fsm, Trans#queuetransferrequest.queue_name) of
		ok ->
			BaseReply#serverreply{success = true};
		invalid ->
			BaseReply#serverreply{
				error_message = "invalid state change",
				error_code = "INVALID_STATE_CHANGE"
			}
	end,
	{Reply, State};
service_request(
	#agentrequest{request_hint = 'INIT_OUTBOUND', 
		init_outbound_request = Request}, 
	BaseReply, 
	#state{state = S} = State) when S =:= released; S =:= idle ->
	Reply = case Request#initoutboundrequest.media_type of
		"freeswitch" ->
			case whereis(freeswitch_media_manager) of
				undefined ->
					BaseReply#serverreply{
						error_message = "freeswitch not available",
						error_code = "MEDIA_TYPE_NOT_AVAILABLE"
					};
				Pid ->
					case freeswitch_media_manager:make_outbound_call(Request#initoutboundrequest.client_id, State#state.agent_fsm, State#state.agent_login) of
						{ok, Pid} ->
							Call = gen_media:get_call(Pid),
							% yes, This should die horribly if it fails.
							ok = agent:set_state(State#state.agent_fsm, precal, Call),
							BaseReply#serverreply{success = true};
						{error, Reason} ->
							BaseReply#serverreply{
								error_message = io_lib:format("initializing outbound call failed (~p)", [Reason]),
								error_code = "UNKNOWN_ERROR"
							}
					end
			end;
		OtherMedia ->
			?INFO("Media ~s not yet available for outboundiness.", [OtherMedia]),
			BaseReply#serverreply{
				error_message = "Media not available for outbound",
				error_code = "MEDIA_TYPE_NOT_AVAILABLE"
			}
	end,
	{Reply, State};
service_request(#agentrequest{request_hint = 'MEDIA_ANSWER'}, BaseReply, #state{state = ringing} = State) ->
	Reply = case agent:set_state(State#state.agent_fsm, oncall) of
		ok ->
			BaseReply#serverreply{success = true};
		invalid ->
			BaseReply#serverreply{
				error_message = "invalid state change",
				error_code = "INVALID_STATE_CHANGE"
			}
	end,
	{Reply, State};
service_request(#agentrequest{request_hint = 'PLUGIN_CALL', plugin_app = Plugin} = Request, BaseReply, State) when Plugin =/= undefined ->
	RunningApps = application:which_applications(),
	Apps = [App || {App, _, _} <- RunningApps, atom_to_list(App) =:= Plugin],
	case Apps of
		[] ->
			BaseReply#serverreply{
				error_message = "No such plugin to call to",
				error_code = "PLUGIN_NOEXISTS"
			};
		[App | _] ->
			case application:get_env(App, agent_tcp_handler) of
				undefined ->
					BaseReply#serverreply{
						error_message = "Plugin doesn't handle tcp",
						error_code = "PLUGIN_NON_TCP"
					};
				{Mod, Func} ->
					erlang:apply(Mod, Func, [State#state.agent_fsm, BaseReply, Request])
			end
	end;
service_request(_, BaseReply, State) ->
	Reply = BaseReply#serverreply{
		error_message = "request not implemented",
		error_code = "INVALID_REQUEST"
	},
	{Reply, State}.

state_to_pb(idle) ->
	'IDLE';
state_to_pb(ringing) ->
	'RINGING';
state_to_pb(precall) ->
	'PRECALL';
state_to_pb(oncall) ->
	'ONCALL';
state_to_pb(outgoing) ->
	'OUTGOING';
state_to_pb(released) ->
	'RELEASED';
state_to_pb(warmtransfer) ->
	'WARMTRANSFER';
state_to_pb(wrapup) ->
	'WRAPUP';
state_to_pb(_) ->
	'PRELOGIN'.


convert_skills(Skills) ->
	convert_skills(Skills, []).

convert_skills([], Acc) ->
	lists:reverse(Acc);
convert_skills([Head | Tail], Acc) ->
	try list_to_existing_atom(Head#skill.atom) of
		A ->
			case Head#skill.expanded of
				undefined ->
					convert_skills(Tail, [A | Acc]);
				X ->
					convert_skills(Tail, [{A, X} | Acc])
			end
	catch
		error:badarg ->
			convert_skills(Tail, Acc)
	end.

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
