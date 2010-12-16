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

%% @doc The process that does the writing to the database via ODBC.  Note
%% this was build with postgres as the target, so other sql servers might choke.
-module(cpx_monitor_kgb_odbc).

-behaviour(gen_server).

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("call.hrl").
-include("cpx.hrl").
-include("odbc_kgb.hrl").

-export([
	start/2,
	start_link/2,
	stop/1
]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).


-record(state, {
	connection :: reference(),
	dsn :: dsn()
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

% =====
% API
% =====

%% @doc Start unlinked.
-spec(start/2 :: (Dsn :: string(), Trace :: 'undefined' | 'true') -> {'ok', pid()}).
start(Dsn, Trace) ->
	gen_server:start(?MODULE, [Dsn, Trace], []).

%% @doc start linked.
-spec(start_link/2 :: (Dsn :: string(), Trace :: 'undefined' | 'true') -> {'ok', pid()}).
start_link(Dsn, Trace) ->
	gen_server:start(?MODULE, [Dsn, Trace], []).

%% @doc Stops the process.
-spec(stop/1 :: (Pid :: pid()) -> 'ok').
stop(Pid) ->
	gen_server:cast(Pid, stop).

% =====
% Init
% =====
init([Dsn, undefined]) ->
	init([Dsn, []]);
init([Dsn, true]) ->
	init([Dsn, [{trace_driver, on}]]);
init([Dsn, Trace]) ->
	ok = case odbc:start() of
		ok ->
			ok;
		{error, {already_started, odbc}} ->
			ok
	end,
	Opts = lists:append(Trace, [{auto_commit, off}, {scrollable_cursors, off}]),
	{ok, Conn} = odbc:connect(Dsn, Opts),
	?INFO("Started with dsn ~s", [Dsn]),
	{ok, #state{dsn = Dsn, connection = Conn}}.

% =====
% handle_call
% =====

handle_call(Msg, _From, State) ->
	{reply, invalid, State}.

% =====
% handle_cast
% =====

handle_cast(stop, State) ->
	case odbc:disconnect(State#state.connection) of
		ok ->
			ok,
			{stop, normal, State};
		{error, Reason} ->
			?WARNING("Disconnect was not clean:  ~p", [Reason]),
			{stop, {dirty, Reason}, State#state{connection = undefined}}
	end.

% =====
% handle_info
% =====

handle_info(EventLog, State) when is_record(EventLog, event_log_row) ->
	Sql = build_sql(EventLog),
	case odbc:sql_query(State#state.connection, Sql, 2000) of
		{error, Reason} ->
			?DEBUG("Sql:  ~p", [Sql]),
			{stop, {failed_query, Reason}, State};
		{updated, _N} ->
			ok = odbc:commit(State#state.connection, commit),
			cpx_monitor_odbc_supervisor ! {ack, EventLog#event_log_row.id},
			{noreply, State}
	end;
handle_info(_Msg, State) ->
	{noreply, State}.

% =====
% terminate
% =====

terminate(Why, #state{connection = undefined}) ->
	?INFO("Ending:  ~p", [Why]);
terminate(Why, #state{connection = Conn} = State) ->
	case odbc:disconnect(Conn) of
		ok ->
			ok;
		{error, Reason} ->
			?WARNING("Disconnect was not clean:  ~p", [Reason])
	end,
	terminate(Why, State#state{connection = undefined}).

% =====
% code_change
% =====

code_change(_Oldvsn, State, _Extra) ->
	{ok, State}.

% =====
% Internal Functions
% =====

build_sql(EventRow) ->
	Fields = record_info(fields, event_log_row),
	{Cols, Vals} = get_values(EventRow, Fields),
	StringFields = [atom_to_list(X) || X <- Cols],
	FormatVals = [format(X) || X <- Vals],
	JoinedFields = string:join(StringFields, ", "),
	JoinedVals = string:join(FormatVals, ", "),
	lists:flatten(io_lib:format("INSERT INTO event_logs (~s) VALUES (~s);", [JoinedFields, JoinedVals])).

format(V) when is_atom(V); is_list(V); is_binary(V) ->
	format("'~s'", V);
format(V) ->
	format("~p", V).

format(Str, Val) ->
	io_lib:format(Str, [Val]).

get_values(EventRow, Fields) ->
	get_values(EventRow, Fields, 2, [], []).

get_values(_EventRow, [], _Nth, Cols, Vals) ->
	{lists:reverse(Cols), lists:reverse(Vals)};
get_values(EventRow, [Col | Tail], Nth, Cols, Vals) ->
	case element(Nth, EventRow) of
		undefined ->
			get_values(EventRow, Tail, Nth + 1, Cols, Vals);
		Val ->
			get_values(EventRow, Tail, Nth + 1, [Col | Cols], [Val | Vals])
	end.

-ifdef(TEST).
% =====
% Tests
% =====

build_sql_test_() ->
	[{"A few strings", fun() ->
		Expected = "INSERT INTO event_logs (event_type, acd_type) VALUES ('acd_start', 'openacd');",
		Got = build_sql(#event_log_row{event_type = acd_start, acd_type = "openacd", source_ip = undefined}),
		?assertEqual(Expected, Got)
	end},
	{"Strings and numbers", fun() ->
		Expected = "INSERT INTO event_logs (id, event_type, acd_type) VALUES (5, 'acd_start', 'openacd');",
		Got = build_sql(#event_log_row{id = 5, event_type = acd_start, acd_type = "openacd", source_ip = undefined}),
		?assertEqual(Expected, Got)
	end}].
	
-endif.