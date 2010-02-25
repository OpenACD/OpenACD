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
%%	Micah Warren <micahw at fusedsolutions dot com>
%%

%% @doc Dump CDRs to ODBC

-module(cdr_odbc).
-author(micahw).
-behavior(gen_cdr_dumper).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").

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
		dsn :: string(),
		ref :: any()
}).

-type(state() :: #state{}).
-define(GEN_CDR_DUMPER, true).
-include("gen_spec.hrl").

%% =====
%% callbacks
%% =====

init([DSN, Options]) ->
	try odbc:start() of
		_ -> % ok or {error, {already_started, odbc}}
			Realopts = case proplists:get_value(trace_driver, Options) of
				true ->
					[{trace_driver, on}, {auto_commit, off}, {scrollable_cursors, off}];
				undefined ->
					[{auto_commit, off}, {scrollable_cursors, off}]
			end,
			case odbc:connect(DSN, Realopts) of
				{ok, Ref} ->
					{ok, #state{dsn = DSN, ref = Ref}};
				Else ->
					Else
			end
	catch
		_:_ ->
			{error, odbc_failed}
	end.

terminate(_Reason, _State) ->
	ok.

code_change(_Oldvsn, State, _Extra) ->
	{ok, State}.

dump(Agentstate, State) when is_record(Agentstate, agent_state) ->
	CallID = case Agentstate#agent_state.statedata of
		Call when is_record(Call, call) ->
			Call#call.id;
		_ ->
			""
	end,
	Query = io_lib:format("INSERT INTO agent_states set agent=~B, newstate=~B,
		oldstate=~B, start=~B, end=~B, data='~s', profile=~B;", [
		(try list_to_integer(Agentstate#agent_state.id) catch error:badarg -> 0 end) + 1000,
		agent:state_to_integer(Agentstate#agent_state.state),
		agent:state_to_integer(Agentstate#agent_state.oldstate),
		Agentstate#agent_state.start, 
		Agentstate#agent_state.ended, CallID, profile_id(Agentstate#agent_state.profile)]),
	case odbc:sql_query(State#state.ref, lists:flatten(Query)) of
		{error, Reason} ->
			{error, Reason};
		_Else ->
			%?NOTICE ("SQL query result: ~p", [Else]),
			{ok, State}
	end;
%dump(_, State) -> % trap_all, CDRs aren't ready yet
	%{ok, State};
dump(CDR, State) when is_record(CDR, cdr_rec) ->
	Media = CDR#cdr_rec.media,
	Client = Media#call.client,
	{InQueue, Oncall, Wrapup, Agent, Queue} = calculate_times(CDR#cdr_rec.summary),

	T = lists:sort(fun(#cdr_raw{start = Start1, ended = End1}, #cdr_raw{start =
					Start2, ended = End2}) ->
				Start1 =< Start2 andalso End1 =< End2
		end, CDR#cdr_rec.transactions),

	Type = calculate_type(Media),

	LastState = calculate_last_transaction(T),

	DNIS = lists:foldl(
		fun(#cdr_raw{transaction = T2, eventdata = E}, _Acc) when T2 == inivr -> E;
		(_, Acc) -> Acc
	end, "", T),

	[First | _] = T,
	[Last | _] = lists:reverse(T),

	Start = First#cdr_raw.start,
	End = Last#cdr_raw.ended,

	lists:foreach(
		fun(#cdr_raw{transaction = Tfun} = Transaction) when Tfun == cdrinit ->
				% work around micah's "fanciness" and make cdrinit the 0 length transaction it should be
				Q = io_lib:format("INSERT INTO billing_transactions set UniqueID='~s', Transaction=~B, Start=~B, End=~B, Data='~s'",
					[Media#call.id,
					cdr_transaction_to_integer(Tfun),
					Transaction#cdr_raw.start,
					Transaction#cdr_raw.start,
					add_slashes(get_transaction_data(Transaction, CDR), "'")]),
			odbc:sql_query(State#state.ref, lists:flatten(Q));
			(#cdr_raw{transaction = Tfun} = Transaction) ->
				case cdr_transaction_to_integer(Tfun) of
					undefined ->
						ok;
					TID ->
						Q = io_lib:format("INSERT INTO billing_transactions set UniqueID='~s', Transaction=~B, Start=~B, End=~B, Data='~s'",
							[Media#call.id,
								TID,
								Transaction#cdr_raw.start,
								Transaction#cdr_raw.ended,
								add_slashes(get_transaction_data(Transaction, CDR), "'")]),
						odbc:sql_query(State#state.ref, lists:flatten(Q))
				end
	end,
	T),
	case Client#client.id of
		ClientID when is_list(ClientID), length(ClientID) =:= 8->
			Tenantid = list_to_integer(string:substr(ClientID, 1, 4)),
			Brandid = list_to_integer(string:substr(ClientID, 5, 4));
		_ ->
			Tenantid = 0,
			Brandid = 0
	end,

	AgentID = case agent_auth:get_agent(Agent) of
		{atomic, [Rec]} when is_tuple(Rec) ->
			list_to_integer(element(2, Rec)) + 1000;
		_ ->
			0
	end,

	Query = io_lib:format("INSERT INTO billing_summaries set UniqueID='~s',
		TenantID=~B, BrandID=~B, Start=~B, End=~B, InQueue=~B, InCall=~B, Wrapup=~B, CallType='~s', AgentID='~B', LastQueue='~s', LastState=~B, DNIS='~s';", [
		Media#call.id,
		Tenantid,
		Brandid,
		Start,
		End,
		InQueue,
		Oncall,
		Wrapup,
		Type,
		AgentID,
		add_slashes(Queue, "'"),
		cdr_transaction_to_integer(LastState),
		DNIS
	]),
	%?NOTICE("query is ~s", [Query]),
	case odbc:sql_query(State#state.ref, lists:flatten(Query)) of
		{error, Reason} ->
			{error, Reason};
		_ ->
			Dialednum = lists:foldl(
				fun(#cdr_raw{transaction = T2, eventdata = E}, _Acc) when T2 == dialoutgoing -> E;
					(_, Acc) -> Acc
				end, "", T),
			VoicemailID = case Media#call.type of
				voicemail ->
					% store ID of original call
					string:substr(Media#call.id, 1, length(Media#call.id) - 3);
				_ ->
					""
			end,
			InfoQuery = io_lib:format("INSERT INTO call_info SET UniqueID='~s', TenantID=~B, BrandID=~B, DNIS=~s, CallType='~s', CallerIDNum='~s', CallerIDName='~s', DialedNumber=~s, VoicemailID=~s;", [
				Media#call.id,
				Tenantid,
				Brandid,
				string_or_null(DNIS),
				Type,
				add_slashes(element(2, Media#call.callerid), "'"),
				add_slashes(element(1, Media#call.callerid), "'"),
				string_or_null(Dialednum),
				string_or_null(VoicemailID)
			]),
			%?NOTICE("query is ~s", [InfoQuery]),
			case odbc:sql_query(State#state.ref, lists:flatten(InfoQuery)) of
				{error, Reason} ->
					{error, Reason};
				_ ->
				{ok, State}
			end
	end.

commit(State) ->
	%?NOTICE("committing pending operations", []),
	odbc:commit(State#state.ref, commit),
	{ok, State}.

rollback(State) ->
	%?NOTICE("committing pending operations", []),
	odbc:commit(State#state.ref, rollback),
	{ok, State}.

cdr_transaction_to_integer(T) ->
	case T of
		cdrinit -> 0;
		inivr -> 1;
		dialoutgoing -> 2;
		inqueue -> 3;
		ringing -> 4;
		precall -> 5;
		oncall -> 6; % was ONCALL
		inoutgoing -> 7;
		failedoutgoing -> 8;
		transfer -> 9;
		agent_transfer -> 9;
		queue_transfer -> 9;
		warmxfer_begin -> 10;
		warmxfer_complete -> 11;
		warmxfer_cancel -> 12;
		warmxfer_fail -> 12;
		warmxferleg -> 13;
		wrapup -> 14; % was INWRAPUP
		endwrapup -> 15;
		abandonqueue -> 16;
		abandonivr -> 17;
		voicemail -> 18; % was LEFTVOICEMAIL
		hangup -> 19; % was ENDCALL
		unknowntermination -> 20;
		cdrend -> 21;
		_ -> ?WARNING("unhandled CDR transaction ~p", [T]), undefined
	end.

get_transaction_data(#cdr_raw{transaction = T} = Transaction, _CDR) when T =:= oncall; T =:= wrapup; T =:= endwrapup; T =:= ringing; T == warmxfer_cancel; T == warmxfer_fail; T == warmxfer_complete ->
	case agent_auth:get_agent(Transaction#cdr_raw.eventdata) of
		{atomic, [Rec]} when is_tuple(Rec) ->
			integer_to_list(list_to_integer(element(2, Rec)) + 1000);
		_ ->
			"0"
	end;
get_transaction_data(#cdr_raw{transaction = T} = Transaction, _CDR) when T =:= inqueue; T == precall; T == dialoutgoing; T == voicemail, T == abandonqueue ->
	Transaction#cdr_raw.eventdata;
get_transaction_data(#cdr_raw{transaction = T} = Transaction, _CDR) when T =:= queue_transfer  ->
	"queue " ++ Transaction#cdr_raw.eventdata;
get_transaction_data(#cdr_raw{transaction = T} = Transaction, _CDR) when T =:= warmxfer_begin  ->
	element(2, Transaction#cdr_raw.eventdata);
get_transaction_data(#cdr_raw{transaction = T} = Transaction, _CDR) when T =:= agent_transfer  ->
	{_From, To} = Transaction#cdr_raw.eventdata,
	Agent = case agent_auth:get_agent(To) of
		{atomic, [Rec]} when is_tuple(Rec) ->
			integer_to_list(list_to_integer(element(2, Rec)) + 1000);
		_ ->
			"0"
	end,
	"agent " ++ Agent;
get_transaction_data(#cdr_raw{transaction = T}, #cdr_rec{media = Media}) when T =:= cdrinit  ->
	case {Media#call.type, Media#call.direction} of
		{voice, inbound} -> "call";
		{voice, outbound} -> "outgoing";
		{Type, _ } -> atom_to_list(Type)
	end;
get_transaction_data(#cdr_raw{transaction = T, eventdata = D}, _Rec) when T =:= hangup  ->
	case D of
		undefined ->
			"";
		_ ->
			D
	end;
get_transaction_data(#cdr_raw{transaction = T} = Transaction, _CDR) ->
	?NOTICE("eventdata for ~p is ~p", [T, Transaction#cdr_raw.eventdata]),
	"".

%% @doc STring is the string to check, Slashlist is what characters to
%% slashify.  Use:  add_slashes("Barney's ale!", "'!") -> "Barney\'s ale\!"
-spec(add_slashes/2 :: (String :: string(), Slashlist :: string()) -> string()).
add_slashes(String, Slashlist) ->
	add_slashes(String, Slashlist, []).
		
add_slashes([], _, Acc) ->
	lists:reverse(Acc);
add_slashes([H | Tail], Slashlist, Acc) ->
	Newacc = case lists:member(H, Slashlist) of
		true ->
			[H, 92 | Acc]; % 92 is a backslash
		false ->
			[H | Acc]
	end,
	add_slashes(Tail, Slashlist, Newacc).

string_or_null([]) ->
	"NULL";
string_or_null(undefined) ->
	"NULL";
string_or_null(String) ->
	lists:flatten([$', String, $']).

calculate_times(Summary) ->
	lists:foldl(
		fun({oncall, {Time, [{Agent,_} | _]}}, {Q, C, W, _A, Qu}) ->
				{Q, C + Time, W, Agent, Qu};
			({inqueue, {Time, [{Queue, _} | _]}}, {Q, C, W, A, _Qu}) ->
				{Q + Time, C, W, A, Queue};
			({wrapup, {Time, _}}, {Q, C, W, A, Qu}) ->
				{Q, C, W + Time, A, Qu};
			(_, {Q, C, W, A, Qu}) ->
				{Q, C, W, A, Qu}
		end, {0, 0, 0, undefined, ""}, Summary).

calculate_type(Media) ->
	case {Media#call.type, Media#call.direction} of
		{voice, inbound} -> "call";
		{voice, outbound} -> "outgoing";
		{IType, _ } -> atom_to_list(IType)
	end.

calculate_last_transaction(Transactions) ->
	Last = lists:foldl(
		fun(#cdr_raw{transaction = T2}, _Acc) when T2 == abandonqueue; T2 == abandonivr; T2 == voicemail -> T2;
			(_, Acc) -> Acc
		end, hangup, Transactions),
	case Last of
		abandonqueue ->
			case lists:any(fun(#cdr_raw{transaction = T3}) when T3 == oncall; T3 == inoutgoing -> true; (_) -> false end, Transactions) of
				true ->
					hangup;
				false ->
					Last
			end;
		_ ->
			Last
	end.

profile_id(Profile) ->
	case Profile of
		"Probationary" -> 1;
		"Level 1" -> 2;
		"Level 2" -> 3;
		"Level 3" -> 4;
		"Supervisor" -> 5;
		"CustomerService" -> 6;
		"Graveyard" -> 7;
		"Magic" -> 9;
		"NRTC Only" -> 13;
		"Master Tech" -> 10;
		Else ->
			?NOTICE("Unknown profile ~p", [Else]),
			1
	end.
% TODO - write some tests for the functions not directly using ODBC

-ifdef(TEST).
last_transaction_test_() ->
	[
		{"abandon in IVR",
			fun() ->
					?assertEqual(abandonivr, calculate_last_transaction([#cdr_raw{transaction = cdrinit}, #cdr_raw{transaction=inivr}, #cdr_raw{transaction=abandonivr}]))
			end
		},
		{"abandon in Queue",
			fun() ->
					?assertEqual(abandonqueue, calculate_last_transaction([#cdr_raw{transaction = cdrinit}, #cdr_raw{transaction=inivr}, #cdr_raw{transaction=inqueue}, #cdr_raw{transaction=abandonqueue}]))
			end
		},
		{"abandon in Queue after ringing",
			fun() ->
					?assertEqual(abandonqueue, calculate_last_transaction([#cdr_raw{transaction = cdrinit}, #cdr_raw{transaction=inivr}, #cdr_raw{transaction=inqueue}, #cdr_raw{transaction=ringing}, #cdr_raw{transaction=abandonqueue}]))
			end
		},
		{"abandon in Queue after oncall",
			fun() ->
					?assertEqual(hangup, calculate_last_transaction([#cdr_raw{transaction = cdrinit}, #cdr_raw{transaction=inivr}, #cdr_raw{transaction=inqueue}, #cdr_raw{transaction=ringing}, #cdr_raw{transaction=oncall}, #cdr_raw{transaction=queue_transfer}, #cdr_raw{transaction=abandonqueue}]))
			end
		}
	].

-endif.
