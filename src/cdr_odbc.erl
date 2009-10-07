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

%% @doc Dump CDRs to ODBC

-module(cdr_odbc).
-author(micahw).
-behavior(gen_cdr_dumper).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").

-ifdef(EUNIT).
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
		dsn,
		ref,
		summary_table,
		transaction_table
}).

%% =====
%% callbacks
%% =====

init([DSN, Options]) ->
	try odbc:start() of
		_ -> % ok or {error, {already_started, odbc}}
			case odbc:connect(DSN, [{trace_driver, on}, {auto_commit, off}, {scrollable_cursors, off}]) of
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
	Query = io_lib:format("INSERT INTO agent_states set agent='~s', newstate=~B,
		oldstate=~B, start=~B, end=~B, data='~s';", [
		Agentstate#agent_state.id, 
		agent:state_to_integer(Agentstate#agent_state.state), 
		agent:state_to_integer(Agentstate#agent_state.oldstate), 
		Agentstate#agent_state.start, 
		Agentstate#agent_state.ended, CallID]),
	case odbc:sql_query(State#state.ref, lists:flatten(Query)) of
		{error, Reason} ->
			{error, Reason};
		Else ->
			%?NOTICE ("SQL query result: ~p", [Else]),
			{ok, State}
	end;
%dump(_, State) -> % trap_all, CDRs aren't ready yet
	%{ok, State};
dump(CDR, State) when is_record(CDR, cdr_rec) ->
	Media = CDR#cdr_rec.media,
	Client = Media#call.client,
	{InQueue, Oncall, Wrapup, Agent, Queue} = lists:foldl(
		fun({oncall, {Time, [{Agent,_}]}}, {Q, C, W, A, Qu}) ->
				{Q, C + Time, W, Agent, Qu};
			({inqueue, {Time, [{Queue, _}]}}, {Q, C, W, A, Qu}) ->
				{Q + Time, C, W, A, Queue};
			({wrapup, {Time, _}}, {Q, C, W, A, Qu}) ->
				{Q, C, W + Time, A, Qu};
			(_, {Q, C, W, A, Qu}) ->
				{Q, C, W, A, Qu}
		end, {0, 0, 0, undefined, ""}, CDR#cdr_rec.summary),

	T = lists:sort(fun(#cdr_raw{start = Start1, ended = End1}, #cdr_raw{start =
					Start2, ended = End2}) ->
				Start1 =< Start2 andalso End1 =< End2
		end, CDR#cdr_rec.transactions),

	Type = case {Media#call.type, Media#call.direction} of
		{voice, inbound} -> "call";
		{voice, outbound} -> "outgoing";
		{IType, _ } -> atom_to_list(IType)
	end,

	[First | _] = T,
	[Last | _] = lists:reverse(T),

	Start = First#cdr_raw.start,
	End = Last#cdr_raw.ended,

	lists:foreach(
		fun(Transaction) ->
				Q = io_lib:format("INSERT INTO billing_transactions set UniqueID='~s', Transaction=~B, Start=~B, End=~B, Data='~s'",
					[Media#call.id,
					cdr_transaction_to_integer(Transaction#cdr_raw.transaction),
					Transaction#cdr_raw.start,
					Transaction#cdr_raw.ended,
					get_transaction_data(Transaction, CDR)]),
			odbc:sql_query(State#state.ref, lists:flatten(Q))
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

	%?WARNING("agent is ~p", [Agent]),

	AgentID = case agent_auth:get_agent(Agent) of
		{atomic, [Rec]} when is_tuple(Rec) ->
			list_to_integer(element(2, Rec));
		_ ->
			0
	end,


	?WARNING("callid is ~p", [Media#call.id]),

	Query = io_lib:format("INSERT INTO billing_summaries set UniqueID='~s',
		TenantID=~B, BrandID=~B, Start=~B, End=~b, InQueue=~B, InCall=~B, Wrapup=~B, CallType='~s', AgentID='~B', LastQueue='~s';", [
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
		Queue]),
	case odbc:sql_query(State#state.ref, lists:flatten(Query)) of
		{error, Reason} ->
			{error, Reason};
		Else ->
			%?NOTICE ("SQL query result: ~p", [Else]),
			{ok, State}
	end.

commit(State) ->
	?NOTICE("committing pending operations", []),
	odbc:commit(State#state.ref, commit).

rollback(State) ->
	odbc:commit(State#state.ref, rollback).

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
		warmtransfer -> 10;
		warmtransfercomplete -> 11;
		warmtransferfailed -> 12;
		warmxferleg -> 13;
		wrapup -> 14; % was INWRAPUP
		endwrapup -> 15;
		abandonqueue -> 16;
		abandonivr -> 17;
		voicemail -> 18; % was LEFTVOICEMAIL
		hangup -> 19; % was ENDCALL
		unknowntermination -> 20;
		cdrend -> 21
	end.

get_transaction_data(#cdr_raw{transaction = T} = Transaction, CDR) when T =:= oncall; T =:= wrapup; T =:= ringing  ->
	case agent_auth:get_agent(Transaction#cdr_raw.eventdata) of
		{atomic, [Rec]} when is_tuple(Rec) ->
			element(2, Rec);
		_ ->
			"0"
	end;
get_transaction_data(#cdr_raw{transaction = T} = Transaction, CDR) when T =:= inqueue  ->
	Transaction#cdr_raw.eventdata;
get_transaction_data(#cdr_raw{transaction = T} = Transaction, #cdr_rec{media = Media} = CDR) when T =:= cdrinit  ->
	case {Media#call.type, Media#call.direction} of
		{voice, inbound} -> "call";
		{voice, outbound} -> "outgoing";
		{Type, _ } -> atom_to_list(Type)
	end;
get_transaction_data(#cdr_raw{transaction = T} = Transaction, CDR) ->
	?NOTICE("eventdata for ~p is ~p", [T, Transaction#cdr_raw.eventdata]),
	"".

