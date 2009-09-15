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

-export([
	init/1,
	terminate/2,
	code_change/3,
	dump/2
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
			case odbc:connect(DSN, [{trace_driver, on}]) of
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
	Query = io_lib:format("INSERT INTO agent_states set agent='~s', newstate=~B,
		oldstate=~B, start=~B, end=~B;", [
		Agentstate#agent_state.agent, 
		agent:state_to_integer(Agentstate#agent_state.state), 
		agent:state_to_integer(Agentstate#agent_state.oldstate), 
		Agentstate#agent_state.start, 
		Agentstate#agent_state.ended]),
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
		end, {0, 0, 0, undefined, undefined}, CDR#cdr_rec.summary),

	T = lists:sort(fun(#cdr_raw{start = Start1, ended = End1}, #cdr_raw{start =
					Start2, ended = End2}) ->
				Start1 =< Start2 andalso End1 =< End2
		end, CDR#cdr_rec.transactions),

	[First | _] = T,
	[Last | _] = lists:reverse(T),

	Start = First#cdr_raw.start,
	End = Last#cdr_raw.ended,

	Query = io_lib:format("INSERT INTO billing_summaries set UniqueID='~s',
		TenantID=~B, BrandID=~B, Start=~B, End=~b, InQueue=~B, InCall=~B, Wrapup=~B, CallType='~w', AgentID='~s', LastQueue='~s';", [
		Media#call.id,
		Client#client.tenant,
		Client#client.brand,
		Start,
		End,
		InQueue,
		Oncall,
		Wrapup,
		Media#call.type,
		Agent,
		Queue]),
	case odbc:sql_query(State#state.ref, lists:flatten(Query)) of
		{error, Reason} ->
			{error, Reason};
		Else ->
			%?NOTICE ("SQL query result: ~p", [Else]),
			{ok, State}
	end.
