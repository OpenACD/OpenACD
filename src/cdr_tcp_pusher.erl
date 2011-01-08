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

%% @doc A CDR dumper that sends cdr's to a tcp port.  The cdr's are 
%% converted to protobufs and sent in a netstring format.  It expects an
%% ack back for each cdr sent so that it can act as a true dumper (and not
%% just a dispatcher.

-module(cdr_tcp_pusher).
-author(micahw).
-behavior(gen_cdr_dumper).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("cpx_cdr_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
	init/1,
	terminate/2,
	code_change/3,
	dump/2,
	commit/1,
	rollback/1
]).

-record(state, {
	tcp_minion :: pid()
}).

-record(loop_state, {
	socket,
	ack_queue = dict:new(),
	last_id = 0
}).

-type(state() :: #state{}).
-define(GEN_CDR_DUMPER, true).
-include("gen_spec.hrl").

% =====
% Callbacks
% =====

init(Opts) ->
	Port = proplists:get_value(port, Opts),
	Server = proplists:get_value(server, Opts, "localhost"),
	Minion = start_tcp_loop(Server, Port),
	{ok, #state{tcp_minion = Minion}}.

terminate(_Reason, _State) ->
	ok.

code_change(_Oldvsn, State, _Extra) ->
	{ok, State}.

dump(Rec, #state{tcp_minion = Pid} = State) when is_record(Rec, agent_state); is_record(Rec, cdr_rec) ->
	Pid ! {send, Rec},
	{ok, State}.

commit(State) ->
	{ok, State}.

rollback(State) ->
	{ok, State}.

% =====
% Internal functions
% =====

next_id(LastId) when LastId > 999998 ->
	1;
next_id(LastId) ->
	LastId + 1.

start_tcp_loop(Server, Port) ->
	Fun = fun() ->
		case gen_tcp:connect(Server, Port, [binary, {packet, raw}, {active, once}]) of
			{ok, Socket} ->
				tcp_loop(#loop_state{socket = Socket});
			Else ->
				exit(Else)
		end
	end,
	spawn_link(Fun).

tcp_loop(#loop_state{socket = Socket} = State) ->
	receive
		{tcp, Socket, Packet} ->
			{_Rest, Acks} = protobuf_util:netstring_to_bins(Packet),
			NewAckq = remove_acked(Acks, State#loop_state.ack_queue),
			tcp_loop(State#loop_state{ack_queue = NewAckq});
		{send, Astate} when is_record(Astate, agent_state) ->
			NewId = next_id(State#loop_state.last_id),
			Send = #cdrdumpmessage{
				message_id = NewId,
				message_hint = 'AGENT_STATE',
				agent_state_change = agent_state_to_protobuf(Astate)
			},
			Bin = protobuf_util:bin_to_netstring(cpx_cdr_pb:encode(Send)),
			ok = gen_tcp:send(Socket, Bin),
			NewDict = dict:store(NewId, Send, State#loop_state.ack_queue),
			tcp_loop(State#loop_state{last_id = NewId, ack_queue = NewDict});
		{send, Cdr} when is_record(Cdr, cdr_rec) ->
			NewId = next_id(State#loop_state.last_id),
			Send = #cdrdumpmessage{
				message_id = NewId,
				message_hint = 'CDR',
				cdr = cdr_rec_to_protobuf(Cdr)
			},
			Bin = protobuf_util:bin_to_netstring(cpx_cdr_pb:encode(Send)),
			ok = gen_tcp:send(Socket, Bin),
			NewDict = dict:store(NewId, Send, State#loop_state.ack_queue),
			tcp_loop(State#loop_state{last_id = NewId, ack_queue = NewDict});
		resend ->
			Proplist = dict:to_list(State#loop_state.ack_queue),
			Bins = [cpx_cdr_pb:encode(Send) || {_Key, Send} <- Proplist],
			Bin = protobu_util:bins_to_netstring(Bins),
			ok = gen_tcp:send(Socket, Bin),
			tcp_loop(State);
		Else ->
			?WARNING("unhandlable message ~p", [Else]),
			tcp_loop(State)
	end.
	
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

