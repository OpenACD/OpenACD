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

%% @doc A very simple cdr dumper module.

-module(cdr_csv).
-author(micahw).
-behavior(gen_cdr_dumper).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").

-export([
	init/1,
	terminate/2,
	code_change/3,
	dump/2,
	commit/1,
	rollback/1
]).

-record(state, {
	agent_states_file,
	agent_states_buffer = [],
	cdr_file,
	cdr_buffer = []
}).

%% =====
%% callbacks
%% =====

init(Opts) ->
	case proplists:get_value(file, Opts) of
		undefined ->
			{ok, File1} = file:open("./cpx_agent_states.csv", [append]),
			{ok, File2} = file:open("./cpx_cdr.csv", [append]),
			{ok, #state{agent_states_file = File1, cdr_file = File2}};
		Filename ->
			{ok, File} = file:open(Filename, [append]),
			{ok, #state{agent_states_file = File}}
	end.

terminate(_Reason, _State) ->
	ok.

code_change(_Oldvsn, State, _Extra) ->
	{ok, State}.

dump(Agentstate, #state{agent_states_buffer = Buf} = State) when is_record(Agentstate, agent_state) ->
	L = io_lib:format("~s, ~w, ~B, ~B~n", [
		Agentstate#agent_state.agent, 
		Agentstate#agent_state.state, 
		Agentstate#agent_state.start, 
		Agentstate#agent_state.ended]),
	{ok, State#state{agent_states_buffer = [L | Buf]}};
dump(CDR, #state{cdr_buffer = Buf} = State) when is_record(CDR, cdr_rec) ->
	Media = CDR#cdr_rec.media,
	case lists:keysearch(oncall, 1, CDR#cdr_rec.summary) of
		{value, {oncall,{Oncall,_}}} ->
			ok;
		false ->
			Oncall = 0
	end,

	case lists:keysearch(ringing, 1, CDR#cdr_rec.summary) of
		{value, {ringing,{Ringing,_}}} ->
			ok;
		false ->
			Ringing = 0
	end,

	case lists:keysearch(inqueue, 1, CDR#cdr_rec.summary) of
		{value, {inqueue,{Inqueue,_}}} ->
			ok;
		false ->
			Inqueue = 0
	end,

	case lists:keysearch(wrapup, 1, CDR#cdr_rec.summary) of
		{value, {wrapup,{Wrapup,_}}} ->
			ok;
		false ->
			Wrapup = 0
	end,

	L = io_lib:format("~s, ~B, ~B, ~B, ~B~n", [
		Media#call.id,
		Ringing, Inqueue, Oncall, Wrapup]),
	{ok, State#state{cdr_buffer = [L | Buf]}}.

commit(#state{cdr_buffer = CdrBuf, agent_states_buffer = AgentBuf, cdr_file = CdrFile, agent_states_file = AgentFile} = State) ->
	case CdrBuf of
		[] -> ok;
		Else -> file:write(CdrFile, lists:reverse(Else))
	end,
	case AgentBuf of
		[] -> ok;
		Else2 -> file:write(AgentFile, lists:reverse(Else2))
	end,
	{ok, State#state{cdr_buffer = [], agent_states_buffer = []}}.

rollback(State) ->
	{ok, State#state{cdr_buffer = [], agent_states_buffer = []}}.

