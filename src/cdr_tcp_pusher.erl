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

%% @doc A gen_server that sends cdr's to a tcp port.  The cdr's are 
%% converted to protobufs and sent in a netstring format.  It expects an
%% ack back for each cdr sent so that it can act as a true dumper (and not
%% just a dispatcher).

-module(cdr_tcp_pusher).
-author(micahw).
-behavior(gen_server).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("cpx_cdr_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% API
-export([
	start/1,
	start_link/1,
	resend/0
]).

% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {
	port,
	server = "localhost",
	socket,
	ack_queue = dict:new(),
	last_id = 0
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

% =====
% API
% =====

-type(port_opt() :: {port, pos_integer()}).
-type(server_opt() :: {server, string()}).
-type(start_opt() :: port_opt() | server_opt()).
-type(start_opts() :: [start_opt()]).

%% @doc Start unlinked.
-spec(start/1 :: (Opts :: start_opts()) -> {'ok', pid()}).
start(Opts) ->
	gen_server:start({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Start linked.
-spec(start_link/1 :: (Opts :: start_opts()) -> {'ok', pid()}).
start_link(Opts) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Resend any cdr/agent state protobufs that have not yet gotten an
%% ack.
-spec(resend/0 :: () -> ok).
resend() ->
	gen_server:cast(?MODULE, resend).

% =====
% Callbacks
% =====

%% @hidden
init(Opts) ->
	Port = proplists:get_value(port, Opts),
	Server = proplists:get_value(server, Opts, "localhost"),
	{ok, Socket} = gen_tcp:connect(Server, Port, [binary, {packet, raw}, {active, once}]),
	cpx_monitor:subscribe(fun cpx_msg_filter/1),
	{ok, #state{
		port = Port,
		server = Server,
		socket = Socket
	}}.

% =====
% handle_call
% =====

%% @hidden
handle_call(Msg, _From, State) ->
	?DEBUG("Unhandled call ~p", [Msg]),
	{reply, unknown_call, State}.

% =====
% handle cast
% =====

%% @hidden
handle_cast(Msg, State) ->
	?DEBUG("Unhandled cast ~p", [Msg]),
	{noreply, State}.

% =====
% handle info
% =====

%% @hidden
handle_info({tcp, Socket, Packet}, #state{socket = Socket} = State) ->
	{_Rest, Acks} = protobuf_util:netstring_to_bins(Packet),
	NewAckq = remove_acked(Acks, State#state.ack_queue),
	{noreply, State#state{ack_queue = NewAckq}};
handle_info({tcp_closted, Socket}, #state{socket = Socket} = State) ->
	{noreply, State#state{socket = undefined}};
handle_info({cpx_monitor_event, {info, _Time, {agent_state, Astate}}}, State) ->
	NewState = send(Astate, State),
	{noreply, NewState};
handle_info({cpx_monitor_event, {info, _Time, {cdr_raw, CdrRaw}}}, State) ->
	NewState = send(CdrRaw, State),
	{noreply, NewState};
handle_info(resend, #state{socket = undefined} = State) ->
	case gen_tcp:connect(State#state.server, State#state.port, [binary, {packet, raw}, {atcive, once}]) of
		{ok, Socket} ->
			NewState = resend(State#state.ack_queue, State#state{socket = Socket}),
			{noreply, NewState};
		Else ->
			?WARNING("Could not reconnect for resend:  ~p", [Else]),
			{noreply, State}
	end;
handle_info(resend, State) ->
	?WARNING("Resending even though I think my socket is up.", []),
	NewState = resend(State#state.ack_queue, State),
	{noreply, NewState};
handle_info({cpx_monitor_event, {info, _Time, {cdr_rec, CdrRec}}}, State) ->
	NewState = send(CdrRec, State),
	{noreply, NewState};


handle_info(Msg, State) ->
	?DEBUG("Unhandled Info ~p", [Msg]),
	{noreply, State}.

% =====
% Terminate
% =====

%% @hidden
terminate(_Reason, _State) ->
	ok.

% =====
% code_change
% =====

%% @hidden
code_change(_Oldvsn, State, _Extra) ->
	{ok, State}.

% =====
% Internal functions
% =====

cpx_msg_filter({cpx_monitor_event, {info, _, {agent_state, _}}}) ->
	true;
cpx_msg_filter({cpx_monitor_event, {info, _, {cdr_rec, _}}}) ->
	true;
cpx_msg_filter({cpx_monitor_event, {info, _, {cdr_raw, _}}}) ->
	true;
cpx_msg_filter(_) ->
	false.

send(Astate, State) when is_record(Astate, agent_state) ->
	NewId = next_id(State#state.last_id),
	Send = #cdrdumpmessage{
		message_id = NewId,
		message_hint = 'AGENT_STATE',
		agent_state_change = agent_state_to_protobuf(Astate)
	},
	NewDict = dict:store(NewId, Send, State#state.ack_queue),
	try_send(Send, State#state{last_id = NewId, ack_queue = NewDict});
send(CdrRaw, State) when is_record(CdrRaw, cdr_raw) ->
	NewId = next_id(State#state.last_id),
	Send = #cdrdumpmessage{
		message_id = NewId,
		message_hint = 'CDR_RAW',
		cdr_raw = cdr_raw_to_protobuf(CdrRaw)
	},
	NewDict = dict:store(NewId, Send, State#state.ack_queue),
	try_send(Send, State#state{last_id = NewId, ack_queue = NewDict});
send(CdrRec, State) when is_record(CdrRec, cdr_rec) ->
	NewId = next_id(State#state.last_id),
	Send = #cdrdumpmessage{
		message_id = NewId,
		message_hint = 'CDR_REC',
		cdr_rec = cdr_rec_to_protobuf(CdrRec)
	},
	NewDict = dict:store(NewId, Send, State#state.ack_queue),
	try_send(Send, State#state{last_id = NewId, ack_queue = NewDict}).

resend([], State) ->
	State;
resend([Send | Tail], State) ->
	case try_send(Send, State) of
		#state{socket = undefined} = Out ->
			Out;
		NewState ->
			resend(Tail, NewState)
	end.

try_send(_Send, #state{socket = undefined} = State) ->
	State;
try_send(Send, #state{socket = Socket} = State) ->
	Bin = protobuf_util:bin_to_netstring(cpx_cdr_pb:encode(Send)),
	case gen_tcp:send(Socket, Bin) of
		ok ->
			State;
		{error, Else} ->
			?WARNING("Could not send data (queued only) due to ~p", [Else]),
			State#state{socket = undefined}
	end.

next_id(LastId) when LastId > 999998 ->
	1;
next_id(LastId) ->
	LastId + 1.

remove_acked([], Dict) ->
	Dict;
remove_acked([Bin | Tail], Dict) ->
	Decoded = cpx_cdr_pb:decode_cdrdumpack(Bin),
	NewDict = dict:rease(Decoded#cdrdumpack.message_id, Dict),
	remove_acked(Tail, NewDict).

agent_state_to_protobuf(AgentState) ->
	Base = #agentstatechange{
		agent_id = AgentState#agent_state.id,
		agent_login = AgentState#agent_state.agent,
		is_login = case AgentState#agent_state.state of
			login -> true;
			_ -> false
		end,
		is_logout = case AgentState#agent_state.state of
			logout -> true;
			_ -> false
		end,
		new_state = protobuf_util:statename_to_enum(AgentState#agent_state.state),
		old_state = protobuf_util:statename_to_enum(AgentState#agent_state.oldstate),
		start_time = AgentState#agent_state.start,
		stop_time = AgentState#agent_state.ended,
		profile = AgentState#agent_state.profile
	},
	case AgentState#agent_state.state of
		idle ->
			Base;
		precall ->
			Base#agentstatechange{
				client_record = protobuf_util:client_to_protobuf(AgentState#agent_state.statedata)
			};
		released ->
			Base#agentstatechange{
				released = protobuf_util:release_to_protobuf(AgentState#agent_state.statedata)
			};
		warm_transfer ->
			Base#agentstatechange{
				call_record = protobuf_util:call_to_protobuf(element(2, AgentState#agent_state.statedata)),
				dialed_number = protobuf_util:call_to_protobuf(AgentState#agent_state.statedata)
			};
		_ when is_record(AgentState#agent_state.statedata, call) ->
			Base#agentstatechange{
				call_record = protobuf_util:call_to_protobuf(AgentState#agent_state.statedata)
			};
		_ ->
			Base
	end.
			

cdr_rec_to_protobuf(Cdr) when is_record(Cdr, cdr_rec) ->
	Summary = summary_to_protobuf(Cdr#cdr_rec.summary),
	Raws = [cdr_raw_to_protobuf(X) || X <- Cdr#cdr_rec.transactions],
	Call = protobuf_util:call_to_protobuf(Cdr#cdr_rec.media),
	#cpxcdrrecord{
		call_record = Call,
		details = Summary,
		raw_transactions = Raws
	}.
cdr_raw_to_protobuf(Cdr) when is_record(Cdr, cdr_raw) ->
	Base = #cpxcdrraw{
		call_id = Cdr#cdr_raw.id,
		transaction = cdr_transaction_to_enum(Cdr#cdr_raw.transaction),
		start_time = Cdr#cdr_raw.start,
		stop_time = Cdr#cdr_raw.start,
		terminates = case Cdr#cdr_raw.terminates of
			infoevent ->
				'INFOEVENT';
			_ ->
				[cdr_transaction_to_enum(X) || X <- Cdr#cdr_raw.terminates]
		end
	},
	case Cdr#cdr_raw.transaction of
		cdrinit -> Base;
		inivr -> Base#cpxcdrraw{ dnis = Cdr#cdr_raw.eventdata};
		dialoutgoing -> Base#cpxcdrraw{number_dialed = Cdr#cdr_raw.eventdata};
		inqueue -> Base#cpxcdrraw{queue = Cdr#cdr_raw.eventdata};
		ringing -> Base#cpxcdrraw{agent = Cdr#cdr_raw.eventdata};
		ringout -> Base#cpxcdrraw{agent = Cdr#cdr_raw.eventdata};
		precall -> Base#cpxcdrraw{client = Cdr#cdr_raw.eventdata};
		oncall -> Base#cpxcdrraw{agent = Cdr#cdr_raw.eventdata};
		agent_transfer -> Base#cpxcdrraw{
			agent = element(1, Cdr#cdr_raw.eventdata),
			agent_transfer_recipient = element(2, Cdr#cdr_raw.eventdata)
		};
		queue_transfer -> Base#cpxcdrraw{queue = Cdr#cdr_raw.eventdata};
		transfer -> Base#cpxcdrraw{
			transfer_to = Cdr#cdr_raw.eventdata
		};
		warmxfer_begin -> Base#cpxcdrraw{
			transfer_to = element(2, Cdr#cdr_raw.eventdata),
			agent = element(1, Cdr#cdr_raw.eventdata)
		};
		warmxfer_cancel -> Base#cpxcdrraw{agent = element(1, Cdr#cdr_raw.eventdata)};
		warmxfer_fail -> Base#cpxcdrraw{agent = Cdr#cdr_raw.eventdata};
		warmxfer_complete -> Base#cpxcdrraw{agent = Cdr#cdr_raw.eventdata};
		wrapup -> Base#cpxcdrraw{agent = Cdr#cdr_raw.eventdata};
		endwrapup -> Base#cpxcdrraw{agent = Cdr#cdr_raw.eventdata};
		abandonqueue -> Base#cpxcdrraw{queue = Cdr#cdr_raw.eventdata};
		abandonivr -> Base;
		voicemail -> Base#cpxcdrraw{queue = Cdr#cdr_raw.eventdata};
		hangup -> Base#cpxcdrraw{hangup_by = case Cdr#cdr_raw.eventdata of
			agent -> 
				"agent";
			_ ->
				Cdr#cdr_raw.eventdata
		end};
		undefined -> Base;
		cdrend -> Base;
		_ -> Base
	end.

summary_to_protobuf(Summary) ->
	summary_to_protobuf(Summary, #cpxcdrsummary{}).

summary_to_protobuf([], Acc) ->
	Acc;
summary_to_protobuf([{wrapup, {Total, Specifics}} | Tail], Acc) ->
	NewAcc = Acc#cpxcdrsummary{
		wrapup = Total,
		wrapup_breakdown = make_cpxcdrkeytime(Specifics)
	},
	summary_to_protobuf(Tail, NewAcc);
summary_to_protobuf([{warmxfer_fail, {Total, Specifics}} | Tail], Acc) ->
	NewAcc = Acc#cpxcdrsummary{
		warmxfer_fail = Total,
		warmxfer_fail_breakdown = make_cpxcdrkeytime(Specifics)
	},
	summary_to_protobuf(Tail, NewAcc);
summary_to_protobuf([{warmxfer_begin, {Total, Specifics}} | Tail], Acc) ->
	NewAcc = Acc#cpxcdrsummary{
		warmxfer_begin = Total,
		warmxfer_begin_breakdown = make_cpxcdrkeytime(Specifics)
	},
	summary_to_protobuf(Tail, NewAcc);
summary_to_protobuf([{oncall, {Total, Specifics}} | Tail], Acc) ->
	NewAcc = Acc#cpxcdrsummary{
		oncall = Total,
		oncall_breakdown = make_cpxcdrkeytime(Specifics)
	},
	summary_to_protobuf(Tail, NewAcc);
summary_to_protobuf([{ringing, {Total, Specifics}} | Tail], Acc) ->
	NewAcc = Acc#cpxcdrsummary{
		ringing = Total,
		ringing_breakdown = make_cpxcdrkeytime(Specifics)
	},
	summary_to_protobuf(Tail, NewAcc);
summary_to_protobuf([{inqueue, {Total, Specifics}} | Tail], Acc) ->
	NewAcc = Acc#cpxcdrsummary{
		inqueue = Total,
		inqueue_breakdown = make_cpxcdrkeytime(Specifics)
	},
	summary_to_protobuf(Tail, NewAcc);
summary_to_protobuf([Head | Tail], Acc) ->
	?WARNING("Unable to protobuf ~p", [Head]),
	summary_to_protobuf(Tail, Acc).

make_cpxcdrkeytime(Proplist) ->
	[#cpxcdrkeytime{ key = Key, value = Value } 
		|| {Key, Value} <- Proplist].
	

cdr_transaction_to_enum(T) ->
	list_to_existing_atom(string:to_upper(atom_to_list(T))).

%enum_to_cdr_transactions(T) ->
%	list_to_existing_atom(string:to_lower(atom_to_list(T))).

