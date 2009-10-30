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

%% @doc A redirector from error_logger into cpxlog.

-module(cpxlog_error_logger_redirect).
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

-type(state() :: tuple()).
-define(GEN_EVENT, true).
-include("gen_spec.hrl").

init([]) ->
	{ok, {}}.

%% error_handler messages
handle_event({error, Gleader, {Pid, Format, Data}}, State) ->
	cpxlog:log(error, erlang:localtime(), Pid, Format, [Data]),
	{ok, State};
handle_event({error_report, Gleader, {Pid, std_error, Report}}, State) ->
	cpxlog:log(error, erlang:localtime(), Pid, Report, []),
	{ok, State};
handle_event({error_report, Gleader, {Pid, supervisor_report, Report}}, State) ->
	cpxlog:log(error, erlang:localtime(), Pid, "Supervisor Report: ~p", [Report]),
	{ok, State};
handle_event({error_report, Gleader, {Pid, crash_report, Report}}, State) ->
	cpxlog:log(error, erlang:localtime(), Pid, "Crash Report: ~p", [Report]),
	{ok, State};
handle_event({warning_msg, Gleader, {Pid, Format, Data}}, State) ->
	cpxlog:log(warning, erlang:localtime(), Pid, Format, [Data]),
	{ok, State};
handle_event({warning_report, Gleader, {Pid, std_warning, Report}}, State) ->
	cpxlog:log(warning, erlang:localtime(), Pid, Report, []),
	{ok, State};
handle_event({info_msg, Gleader, {Pid, Format, Data}}, State) ->
	cpxlog:log(info, erlang:localtime(), Pid, Format, [Data]),
	{ok, State};
handle_event({info_report, Gleader, {Pid, std_info, Report}}, State) ->
	cpxlog:log(info, erlang:localtime(), Pid, Report, []),
	{ok, State};
handle_event({info_report, Gleader, {Pid, progress, Report}}, State) ->
	cpxlog:log(info, erlang:localtime(), Pid, "Progress Report: ~p", [Report]),
	{ok, State};
handle_event(Event, State) ->
	io:format("Event: ~p~n", [Event]),
	{ok, State}.

handle_call(_Request, State) ->
	{ok, ok, State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(_Args, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
