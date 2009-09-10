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
	dump/2
]).

-record(state, {
	agent_states_file,
	cdr_file
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

dump(Agentstate, #state{agent_states_file = File} = State) when is_record(Agentstate, agent_state) ->
	io:fwrite(File, "~s, ~w, ~B, ~B~n", [
		Agentstate#agent_state.agent, 
		Agentstate#agent_state.state, 
		Agentstate#agent_state.start, 
		Agentstate#agent_state.ended]),
	{ok, State};
dump(CDR, #state{cdr_file = File} = State) when is_record(CDR, cdr_rec) ->
	Media = CDR#cdr_rec.media,
	io:fwrite(File, "~s, ~w, ~B, ~B~n", [
		Media#call.id,
		CDR#cdr_rec.summary]),
		%Agentstate#agent_state.state, 
		%Agentstate#agent_state.start, 
		%Agentstate#agent_state.ended]),
	{ok, State}.
