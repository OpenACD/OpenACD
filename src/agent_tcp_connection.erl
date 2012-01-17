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
-type(chan_state() :: atom()).
-type(chan_state_data() :: any()).
-type(chan_mediaload() :: any()).
-record(chan_dict_entry, {
	state :: chan_state(),
	statedata :: chan_state_data(),
	mediaload :: chan_mediaload()
}).
-record(state, {
		salt :: pos_integer(),
		socket :: {socket_module(), port()},
		radix :: integer(),
		agent_login :: string(),
		agent_fsm :: pid(),
		securitylevel = agent :: 'agent' | 'supervisor' | 'admin',
		agent_state,
		agent_statedata,
		%mediaload,
		socket_upgrade = 'never' :: 'never' | 'ssl_upgrade',
		channels = dict:new()
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
	{noreply, State#state{agent_state = Statename, agent_statedata = Statedata}};
handle_cast({change_state, Statename}, State) ->
	Statechange = #statechange{
		agent_state = protobuf_util:statename_to_enum(Statename)
	},
	Command = #serverevent{
		command = 'ASTATE',
		state_change = Statechange
	},
	server_event(State#state.socket, Command, State#state.radix),
	{noreply, State#state{agent_state = Statename}};

handle_cast({set_channel, Pid, StateName, Statedata}, #state{channels = AChannels} = State) ->
	NewDict = case dict:find(Pid, AChannels) of
		{ok, Data} ->
			NewData = Data#chan_dict_entry{state = StateName, statedata = Statedata},
			dict:store(Pid, NewData);
		error ->
			NewData = #chan_dict_entry{state = StateName, statedata = Statedata},
			dict:store(Pid, NewData)
	end,
	ChanStateChange = #agentchannelstatechange{
		statename = biggify_atom(StateName),
		channel_id = pid_to_list(Pid),
		call_record = case is_record(call, Statedata) of true -> Statedata; _ -> undefined end
	},
	Command = #serverevent{
		command = 'ACHANNEL_STATE',
		agent_channel_state_change = ChanStateChange
	},
	server_event(State#state.socket, Command, State#state.radix),
	{noreply, State#state{channels = NewDict}};

handle_cast({channel_died, Pid, NewAvail}, #state{channels = AChannels} = State) ->
	NewDict = dict:erase(Pid, AChannels),
	Command = #serverevent{
		command = 'ACHANNEL_DOWN',
		agent_channel_down = pid_to_list(Pid)
	},
	server_event(State#state.socket, Command, State#state.radix),
	{noreply, State#state{channels = NewDict}};

handle_cast({mediapush, ChanPid, _Callrec, {mediaload, Callrec}}, State) ->
	#state{channels = ChanDict} = State,
	NewDict = case dict:find(ChanPid, ChanDict) of
		{ok, ChanData} ->
			NewChanData = ChanData#chan_dict_entry{mediaload = Callrec},
			dict:store(ChanPid, NewChanData);
		error ->
			NewChanData = #chan_dict_entry{mediaload = Callrec},
			dict:store(ChanPid, NewChanData)
	end,
	{noreply, State#state{channels = NewDict}};

handle_cast({mediapush, ChanPid, Callrec, Data}, State) ->
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
	PopRec = #urlpopevent{ url = Url, window_id = Name },
	Command = #serverevent{
		command = 'AURLPOP',
    url_pop_event = PopRec
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

biggify_atom(Atom) ->
	biggify_atom(Atom, list_to_existing_atom).

biggify_atom(Atom, unsafe) ->
	biggify_atom(Atom, list_to_atom);
biggify_atom(Atom, safe) ->
	biggify_atom(Atom, list_to_existing_atom);
biggify_atom(Atom, ToAtom) ->
	ToAtom(string:to_upper(atom_to_list(Atom))).

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

send({Mod, Socket}, Record, Radix) ->
	Bin = cpx_agent_pb:encode(Record),
	%Size = list_to_binary(integer_to_list(size(Bin))),
	%?DEBUG("Das size:  ~p", [Size]),
	%Outbin = <<Size/binary, $:, Bin/binary, $,>>,
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
	Message = #servermessage{
		type_hint = 'REPLY',
		reply = NewReply
	},
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
			Username = LoginRequest#loginrequest.username,
			Agent = #agent{
				id = Id, 
				login = LoginRequest#loginrequest.username, 
				skills = Skills, 
				profile=Profile
			},
			case agent_manager:start_agent(Agent) of
				{ok, Pid} ->
					%agent:set_endpoint(Pid, EndpointType, Endpointdata, Persisty),
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
	case decrypt_password(CryptedPass) of
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
								login = LoginRequest#loginrequest.username, 
								skills = Skills, 
								profile=Profile
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
	Agents = [{availagent, X#agent.login, X#agent.profile} || X <- AgentStates],
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
%service_request(#agentrequest{request_hint = 'MEDIA_HANGUP'}, BaseReply, #state{statedata = C} = State) when is_record(C, call) ->
%	Reply = case agent:set_state(State#state.agent_fsm, {wrapup, C}) of
%		invalid ->
%			BaseReply#serverreply{
%				error_message = "invalid state change",
%				error_code = "INVALID_STATE_CHANGE"
%			};
%		ok ->
%			BaseReply#serverreply{
%				success = true
%			}
%	end,
%	{Reply, State};
%service_request(#agentrequest{request_hint = 'RING_TEST'}, BaseReply, State) ->
%	Reply = case whereis(freeswitch_media_manager) of
%		undefined ->
%			BaseReply#serverreply{
%				error_message = "freeswtich isn't available",
%				error_code = "MEDIA_NOEXISTS"
%			};
%		_Pid ->
%			case agent:dump_state(State#state.agent_fsm) of
%				#agent{release_data = undefined} ->
%					BaseReply#serverreply{
%						error_message = "must be released to do a ring test",
%						error_code = "INVALID_STATE"
%					};
%				AgentRec ->
%					Callrec = #call{id = "unused", source = self(), callerid= {"Echo test", "0000000"}},
%					case freeswitch_media_manager:ring_agent_echo(State#state.agent_fsm, AgentRec, Callrec, 600) of
%						{ok, _} ->
%							BaseReply#serverreply{success = true};
%						{error, Error} ->
%							BaseReply#serverreply{
%								error_message = io_lib:format("ring test failed:  ~p", [Error]),
%								error_code = "UNKNOWN_ERROR"
%							}
%					end
%			end
%	end,
%	{Reply, State};
service_request(#agentrequest{request_hint = 'LOGOUT'}, BaseReply, State) ->
	gen_tcp:close(State#state.socket),
	%% TODO Not a very elegant shutdown from this point onward...
	Reply = BaseReply#serverreply{success = true},
	{Reply, State};
%service_request(#agentrequest{request_hint = 'DIAL', dial_request = Number}, BaseReply, #state{state = precall} = State) ->
%	Call = State#state.statedata,
%	Reply = case Call#call.direction of
%		outbound ->
%			case gen_media:call(Call#call.source, {dial, Number}) of
%				ok ->
%					BaseReply#serverreply{success = true};
%				{error, Error} ->
%					?NOTICE("Outbound call error ~p", [Error]),
%					BaseReply#serverreply{
%						error_message = io_lib:format("Error:  ~p", [Error]),
%						error_code = "UNKNOWN_ERROR"
%					}
%			end;
%		_ ->
%			BaseReply#serverreply{
%				error_message = "invalid call direction",
%				error_code = "INVALID_MEDIA_CALL"
%			}
%	end,
%	{Reply, State};
service_request(#agentrequest{request_hint = 'AGENT_CHANNEL_REQUEST',
	agent_channel_request = ChanReq}, BaseReply, State) when
	is_record(ChanReq, agentchannelrequest) ->
		service_channel_request(ChanReq, BaseReply, State);
%service_request(
%	#agentrequest{request_hint = 'INIT_OUTBOUND', 
%		init_outbound_request = Request}, 
%	BaseReply, 
%	#state{state = S} = State) when S =:= released; S =:= idle ->
%	Reply = case Request#initoutboundrequest.media_type of
%		"freeswitch" ->
%			case whereis(freeswitch_media_manager) of
%				undefined ->
%					BaseReply#serverreply{
%						error_message = "freeswitch not available",
%						error_code = "MEDIA_TYPE_NOT_AVAILABLE"
%					};
%				Pid ->
%					case freeswitch_media_manager:make_outbound_call(Request#initoutboundrequest.client_id, State#state.agent_fsm, State#state.agent_login) of
%						{ok, Pid} ->
%							Call = gen_media:get_call(Pid),
%							% yes, This should die horribly if it fails.
%							ok = agent:set_state(State#state.agent_fsm, precal, Call),
%							BaseReply#serverreply{success = true};
%						{error, Reason} ->
%							BaseReply#serverreply{
%								error_message = io_lib:format("initializing outbound call failed (~p)", [Reason]),
%								error_code = "UNKNOWN_ERROR"
%							}
%					end
%			end;
%		OtherMedia ->
%			?INFO("Media ~s not yet available for outboundiness.", [OtherMedia]),
%			BaseReply#serverreply{
%				error_message = "Media not available for outbound",
%				error_code = "MEDIA_TYPE_NOT_AVAILABLE"
%			}
%	end,
%	{Reply, State};
%service_request(#agentrequest{request_hint = 'MEDIA_ANSWER'}, BaseReply, #state{state = ringing} = State) ->
%	Reply = case agent:set_state(State#state.agent_fsm, oncall) of
%		ok ->
%			BaseReply#serverreply{success = true};
%		invalid ->
%			BaseReply#serverreply{
%				error_message = "invalid state change",
%				error_code = "INVALID_STATE_CHANGE"
%			}
%	end,
%	{Reply, State};
service_request(_, BaseReply, State) ->
	Reply = BaseReply#serverreply{
		error_message = "request not implemented",
		error_code = "INVALID_REQUEST"
	},
	{Reply, State}.

service_channel_request(#agentchannelrequest{request_hint = 'AGENT_TRANSFER', agent_transfer_request = Transfer} = ChanReq, BaseReply, State)
		when is_record(Transfer, agenttransferrequest) ->
	%% TODO use the channel id to do this.
	case Transfer#agenttransferrequest.other_data of
		undefined ->
			ok;
		Caseid ->
			%gen_media:cast(Call#call.source, {set_caseid, Caseid})
			ok
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

service_channel_request(#agentchannelrequest{request_hint =
		'MEDIA_COMMAND', media_command_request = Command} = ChanReq,
		BaseReply, State) when is_record(Command, mediacommandrequest) ->
	% TODO kinda useless w/o pulling out the channel id.
	Reply = case Command#mediacommandrequest.need_reply of
		true ->
			%try gen_media:call(C#call.source, Command) of
			try invalid of
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
			%gen_media:cast(C#call.source, Command),
			BaseReply#serverreply{success = true}
	end,
	{Reply, State};

service_channel_request(#agentchannelrequest{request_hint =
		'QUEUE_TRANSFER', queue_transfer_request = Trans}, BaseReply, State)
		when is_record(Trans, queuetransferrequest) ->
	% TODO use the channel id.
	TransOpts = Trans#queuetransferrequest.transfer_options,
	%gen_media:set_url_getvars(C#call.source, TransOpts#queuetransferoptions.options),
	Skills = convert_skills(TransOpts#queuetransferoptions.skills),
	%gen_media:add_skills(C#call.source, Skills),
	Reply = case agent:queue_transfer(State#state.agent_fsm, Trans#queuetransferrequest.queue_name) of
		ok ->
			BaseReply#serverreply{success = true};
		invalid ->
			BaseReply#serverreply{
				error_message = "invalid state change",
				error_code = "INVALID_STATE_CHANGE"
			}
	end,
	{Reply, State}.
% TODO add media_load and media_hangup handlings.

decrypt_password(Password) ->
	Key = util:get_keyfile(),
	% TODO - this is going to break again for R15A, fix before then
	Entry = case public_key:pem_to_der(Key) of
		{ok, [Ent]} ->
			Ent;
		[Ent] ->
			Ent
	end,
	{ok,{'RSAPrivateKey', 'two-prime', N , E, D, _P, _Q, _E1, _E2, _C, _Other}} =  public_key:decode_private_key(Entry),
	PrivKey = [crypto:mpint(E), crypto:mpint(N), crypto:mpint(D)],
	try crypto:rsa_private_decrypt(Password, PrivKey, rsa_pkcs1_padding) of
		Bar ->
			{ok, binary_to_list(Bar)}
	catch
		error:decrypt_failed ->
			{error, decrypt_failed}
	end.

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

-ifdef(TEST_DEPRICATED).
% these will likely fail, and fail hard.
unauthenticated_agent_test_() ->
	{
		foreach,
		fun() ->
			crypto:start(),
			mnesia:delete_schema([node()]),
			mnesia:create_schema([node()]),
			mnesia:start(),
			agent_auth:new_profile("Testprofile", [skill1, skill2]),
			agent_auth:cache("Username", erlang:md5("Password"), "Testprofile", agent),
			agent_manager:start([node()]),
			#state{}
		end,
		fun(_State) ->
			agent_auth:destroy_profile("Testprofile"),
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
					Call = #call{id="testcall", source = self(), client=#client{label="testclient", brand = 1, tenant = 57}},
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
					Call = #call{id="testcall", source = self(), client=#client{label="testclient", brand = 1, tenant = 57}},
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
					call_queue_config:new_client(#client{label = "Aclient", tenant = 10, brand = 1}),
					call_queue_config:new_client(#client{label = "Bclient", tenant = 5, brand = 2}),
					call_queue_config:new_client(#client{label = "Cclient", tenant = 20, brand = 3}),
					gen_tcp:send(Clientsock, "BRANDLIST 7\r\n"),
					receive {tcp, Clientsock, Packet} -> ok end,
					?assertEqual("ACK 7 (00100001|Aclient),(00050002|Bclient),(00200003|Cclient),(00990099|Demo Client)\r\n", Packet)
				end}
			end,
			fun({_Tcplistener, Clientsock, _APid}) ->
				{"Release options requested",
				fun() ->
					agent_auth:new_release(#release_opt{label = "Bathroom", id=1, bias=0}),
					agent_auth:new_release(#release_opt{label = "Default", id = 2, bias = -1}),
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
	Client = #client{label = "testclient", tenant = 50, brand = 7},
	?assertEqual("00500007", clientrec_to_id(Client)).


-define(MYSERVERFUNC,
	fun() ->
		{ok, Pid} = start_link("garbage data"),
		unlink(Pid),
		{Pid, fun() -> exit(Pid, kill), ok end}
	end).

-include("gen_server_test.hrl").

-endif.
