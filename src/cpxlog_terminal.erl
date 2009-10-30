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

%% @doc A terminal output backend for cpxlog.

-module(cpxlog_terminal).
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
	level = info :: loglevels(),
	debugmodules = [] :: [atom()],
	lasttime = erlang:localtime() :: {{non_neg_integer(), non_neg_integer(), non_neg_integer()}, {non_neg_integer(), non_neg_integer(), non_neg_integer()}}
}).

-type(state() :: #state{}).
-define(GEN_EVENT, true).
-include("gen_spec.hrl").

init([]) ->
	io:format("Loglevel is info~n"),
	{ok, #state{}};
init([LogLevel]) ->
	io:format("Loglevel is ~p~n", [LogLevel]),
	case lists:member(LogLevel, ?LOGLEVELS) of
		true ->
			{ok, #state{level = LogLevel}};
		false ->
			{'EXIT', "bad loglevel"}
	end.

handle_event({Level, Time, Module, Line, Pid, Message, Args}, State) ->
	case (element(3, element(1, Time)) =/= element(3, element(1, State#state.lasttime))) of
		true ->
			io:format("Day changed from ~p to ~p~n", [element(1, State#state.lasttime), element(1, Time)]);
		false ->
			ok
	end,
	case ((lists:member(Level, ?LOGLEVELS) andalso (util:list_index(Level, ?LOGLEVELS) >= util:list_index(State#state.level, ?LOGLEVELS))) orelse lists:member(Module, State#state.debugmodules)) of
		true ->
			io:format("~w:~s:~s [~s] ~w@~s:~w ~s~n", [
					element(1, element(2, Time)),
					string:right(integer_to_list(element(2, element(2, Time))), 2, $0),
					string:right(integer_to_list(element(3, element(2, Time))), 2, $0),
					string:to_upper(atom_to_list(Level)),
					Pid, Module, Line,
					io_lib:format(Message, Args)]);
		false ->
			ok
	end,
	{ok, State#state{lasttime = Time}};
handle_event({Level, Time, Pid, Message, Args}, State) ->
	case (element(3, element(1, Time)) =/= element(3, element(1, State#state.lasttime))) of
		true ->
			io:format("Day changed from ~p to ~p~n", [element(1, State#state.lasttime), element(1, Time)]);
		false ->
			ok
	end,
	case (lists:member(Level, ?LOGLEVELS) andalso (util:list_index(Level, ?LOGLEVELS) >= util:list_index(State#state.level, ?LOGLEVELS))) of
		true ->
			io:format("~w:~s:~s [~s] ~w ~s~n", [
					element(1, element(2, Time)),
					string:right(integer_to_list(element(2, element(2, Time))), 2, $0),
					string:right(integer_to_list(element(3, element(2, Time))), 2, $0),
					string:to_upper(atom_to_list(Level)),
					Pid,
					io_lib:format(Message, Args)]);
		false ->
			ok
	end,
	{ok, State#state{lasttime = Time}};
handle_event({set_log_level, Level}, State) ->
	case lists:member(Level, ?LOGLEVELS) of
		true ->
			io:format("Loglevel changed to: ~s~n", [string:to_upper(atom_to_list(Level))]),
			{ok, State#state{level = Level}};
		false ->
			io:format("Invalid loglevel: ~s~n", [string:to_upper(atom_to_list(Level))]),
			{ok, State}
	end;
handle_event({debug_module, Module}, State) ->
	io:format("Now showing all messages for module ~s~n", [Module]),
	{ok, State#state{debugmodules = lists:umerge(State#state.debugmodules, [Module])}};
handle_event({nodebug_module, Module}, State) ->
	io:format("No longer showing all messages for module ~s~n", [Module]),
	{ok, State#state{debugmodules = lists:subtract(State#state.debugmodules, [Module])}};
handle_event(_Event, State) ->
	{ok, State}.

handle_call(_Request, State) ->
	{ok, ok, State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(Args, _State) ->
	io:format("~p terminated with reason ~p~n", [?MODULE, Args]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
