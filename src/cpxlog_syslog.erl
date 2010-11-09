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

%% @doc A syslog output backend for cpxlog.

-module(cpxlog_syslog).
-behaviour(gen_event).

-include("log.hrl").

-export([
	init/1,
	handle_event/2,
	handle_call/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {
		placeholder :: any()
}).

-type(state() :: #state{}).
-define(GEN_EVENT, true).
-include("gen_spec.hrl").

init([Name, Facility]) when is_list(Name), Facility >=0, Facility =< 23 ->
	{ok, _Pid} = syslog:start(),
	ok = syslog:open(Name, 4, Facility bsl 3),
	{ok, #state{}}.

handle_event({Level, _Time, Module, Line, Pid, Message, Args}, State) ->
	syslog:log(level_to_number(Level),
		io_lib:format("[~s] ~w@~s:~w ~s~n", [
				string:to_upper(atom_to_list(Level)),
				Pid, Module, Line,
				io_lib:format(Message, Args)])),
	{ok, State};
handle_event({Level, _Time, Pid, Message, Args}, State) ->
	syslog:log(level_to_number(Level),
		io_lib:format("[~s] ~w ~s~n", [
				string:to_upper(atom_to_list(Level)),
				Pid,
				io_lib:format(Message, Args)])),
	{ok, State};
handle_event(_Event, State) ->
	{ok, State}.

handle_call(_Request, State) ->
	{ok, ok, State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(_Args, _State) ->
	syslog:stop(),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

level_to_number(Level) ->
	abs(util:list_index(Level, ?LOGLEVELS) - 7).
