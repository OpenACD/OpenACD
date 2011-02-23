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

%% @doc A gen_event based logging framework.

-module(cpxlog).
-export([
	start/0,
	start_link/0,
	stop/0,
	log/7,
	log/5,
	debug/6,
	info/6,
	notice/6,
	warning/6,
	error/6,
	critical/6,
	alert/6,
	emergency/6,
	set_loglevel/1,
	set_loglevel/2,
	debug_module/1,
	debug_modules/1,
	nodebug_module/1,
	nodebug_modules/1
]).

-type(level() :: 'debug' | 'info' | 'notice' | 'warning' | 'error' | 'critical' | 'alert' | 'emergency').

-spec(start/0 :: () -> 'ok' | {'error', any()}).
start() ->
	Out = gen_event:start({local, cpxlog}),
	case lists:member(cpxlog_error_logger_redirect, gen_event:which_handlers(error_logger)) of
		false ->
			case application:get_env('OpenACD', logmsg_maxsize) of
				{ok, LogSize} ->
					ok = gen_event:add_sup_handler(error_logger, cpxlog_error_logger_redirect, [LogSize]);
				_ ->
					ok = gen_event:add_sup_handler(error_logger, cpxlog_error_logger_redirect, [])
			end,
			error_logger:delete_report_handler(sasl_report_tty_h),
			error_logger:delete_report_handler(error_logger_tty_h),
			ok;
		true -> ok
	end,

	case application:get_env('OpenACD', console_loglevel) of
		{ok, LogLevel} ->
			gen_event:add_handler(cpxlog, cpxlog_terminal, [LogLevel]);
		_ ->
			gen_event:add_handler(cpxlog, cpxlog_terminal, [])
	end,
	case application:get_env('OpenACD', logfiles) of
		{ok, Files} ->
			gen_event:add_handler(cpxlog, cpxlog_file, [Files]);
		_ ->
			io:format("Logging to disk is not configured~n"),
			ok
	end,
	Out.

	
-spec(start_link/0 :: () -> 'ok' | {'error', any()}).
start_link() ->
	Out = gen_event:start_link({local, cpxlog}),
	case lists:member(cpxlog_error_logger_redirect, gen_event:which_handlers(error_logger)) of
		false ->
			case application:get_env('OpenACD', logmsg_maxsize) of
				{ok, LogSize} ->
					ok = gen_event:add_sup_handler(error_logger, cpxlog_error_logger_redirect, [LogSize]);
				_ ->
					ok = gen_event:add_sup_handler(error_logger, cpxlog_error_logger_redirect, [])
			end,
			error_logger:delete_report_handler(sasl_report_tty_h),
			error_logger:delete_report_handler(error_logger_tty_h),
			ok;
		true -> ok
	end,

	case application:get_env('OpenACD', console_loglevel) of
		{ok, LogLevel} ->
			gen_event:add_sup_handler(cpxlog, cpxlog_terminal, [LogLevel]);
		_ ->
			gen_event:add_sup_handler(cpxlog, cpxlog_terminal, [])
	end,
	case application:get_env('OpenACD', logfiles) of
		{ok, Files} ->
			gen_event:add_sup_handler(cpxlog, cpxlog_file, [Files]);
		_ ->
			io:format("Logging to disk is not configured~n"),
			ok
	end,
	Out.

-spec(stop/0 :: () -> 'ok').
stop() ->
	% try to clean everything up
	error_logger:delete_report_handler(cpxlog_error_logger_redirect),
	case lists:member(error_logger_tty_h, gen_event:which_handlers(error_logger)) of
		false ->
			error_logger:add_report_handler(error_logger_tty_h, []);
		_ ->
			ok
	end,
	case lists:member(sasl_report_tty_h, gen_event:which_handlers(error_logger)) of
		false ->
			error_logger:add_report_handler(sasl_report_tty_h, all);
		_ ->
			ok
	end,
	gen_event:stop(cpxlog),
	ok.

-spec(log/7 :: (Level :: level(), Time :: any(), Module :: atom(), Line :: non_neg_integer(), Pid :: pid(), Message :: any(), Args :: [any()]) -> 'ok').
log(Level, Time, Module, Line, Pid, Message, Args) ->
	catch gen_event:notify(cpxlog, {Level, Time, Module, Line, Pid, Message, Args}),
	ok.

-spec(log/5 :: (Level :: level(), Time :: any(), Pid :: pid(), Message :: any(), Ars :: [any()]) -> 'ok').
log(Level, Time, Pid, Message, Args) ->
	%io:format("log/5: ~p ~p ~p ~p ~p~n", [Level, Time, Pid, Message, Args]),
	catch gen_event:notify(cpxlog, {Level, Time, Pid, Message, Args}),
	ok.

-spec(debug/6 :: (Time :: any(), Module :: atom(), Line :: non_neg_integer(), Pid :: pid(), Message :: any(), Args :: [any()]) -> 'ok').
debug(Time, Module, Line, Pid, Message, Args) ->
	log(debug, Time, Module, Line, Pid, Message, Args).

-spec(info/6 :: (Time :: any(), Module :: atom(), Line :: non_neg_integer(), Pid :: pid(), Message :: any(), Args :: [any()]) -> 'ok').
info(Time, Module, Line, Pid, Message, Args) ->
	log(info, Time, Module, Line, Pid, Message, Args).

-spec(notice/6 :: (Time :: any(), Module :: atom(), Line :: non_neg_integer(), Pid :: pid(), Message :: any(), Args :: [any()]) -> 'ok').
notice(Time, Module, Line, Pid, Message, Args) ->
	log(notice, Time, Module, Line, Pid, Message, Args).

-spec(warning/6 :: (Time :: any(), Module :: atom(), Line :: non_neg_integer(), Pid :: pid(), Message :: any(), Args :: [any()]) -> 'ok').
warning(Time, Module, Line, Pid, Message, Args) ->
	log(warning, Time, Module, Line, Pid, Message, Args).

-spec(error/6 :: (Time :: any(), Module :: atom(), Line :: non_neg_integer(), Pid :: pid(), Message :: any(), Args :: [any()]) -> 'ok').
error(Time, Module, Line, Pid, Message, Args) ->
	log(error, Time, Module, Line, Pid, Message, Args).

-spec(critical/6 :: (Time :: any(), Module :: atom(), Line :: non_neg_integer(), Pid :: pid(), Message :: any(), Args :: [any()]) -> 'ok').
critical(Time, Module, Line, Pid, Message, Args) ->
	log(critical, Time, Module, Line, Pid, Message, Args).

-spec(alert/6 :: (Time :: any(), Module :: atom(), Line :: non_neg_integer(), Pid :: pid(), Message :: any(), Args :: [any()]) -> 'ok').
alert(Time, Module, Line, Pid, Message, Args) ->
	log(alert, Time, Module, Line, Pid, Message, Args).

-spec(emergency/6 :: (Time :: any(), Module :: atom(), Line :: non_neg_integer(), Pid :: pid(), Message :: any(), Args :: [any()]) -> 'ok').
emergency(Time, Module, Line, Pid, Message, Args) ->
	log(emergency, Time, Module, Line, Pid, Message, Args).

-spec(set_loglevel/1 :: (Level :: pos_integer()) -> 'ok').
set_loglevel(Level) ->
	catch gen_event:notify(cpxlog, {set_log_level, Level}),
	catch gen_event:notify(error_logger, {set_log_level, Level}).

-spec(set_loglevel/2 :: (File :: list(), Level :: pos_integer()) -> 'ok').
set_loglevel(File, Level) ->
	catch gen_event:notify(cpxlog, {set_log_level, File, Level}).

-spec(debug_module/1 :: (Module :: atom()) -> {'ok', atom()}).
debug_module(Module) ->
	catch gen_event:notify(cpxlog, {debug_module, Module}).

-spec(debug_modules/1 :: (Modules :: [atom()]) -> 'ok').
debug_modules(Modules) ->
	Fun = fun(Mod) ->
		debug_module(Mod)
	end,
	lists:foreach(Fun, Modules),
	ok.

-spec(nodebug_module/1 :: (Module :: atom()) -> {'ok', atom()}).
nodebug_module(Module) ->
	catch gen_event:notify(cpxlog, {nodebug_module, Module}).

-spec(nodebug_modules/1 :: (Modules :: [atom()]) -> 'ok').
nodebug_modules(Modules) ->
	Fun = fun(Mod) ->
		nodebug_module(Mod)
	end,
	lists:foreach(Fun, Modules),
	ok.
