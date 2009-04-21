-module(cpxlog).
-export([
	start/0,
	start_link/0,
	log/7,
	info/6,
	set_loglevel/1
]).

start() ->
	gen_event:start({local, cpxlog}),
	gen_event:add_handler(cpxlog, cpxlog_terminal, []),
	ok.

start_link() ->
	gen_event:start_link({local, cpxlog}),
	gen_event:add_handler(cpxlog, cpxlog_terminal, []),
	ok.

log(Level, Time, Module, Line, Pid, Message, Args) ->
	catch gen_event:notify(cpxlog, {Level, Time, Module, Line, Pid, Message, Args}),
	ok.

info(Time, Module, Line, Pid, Message, Args) ->
	log(info, Time, Module, Line, Pid, Message, Args).

set_loglevel(Level) ->
	catch gen_event:notify(cpxlog, {set_log_level, Level}).
