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

%% @doc A gen_event based logging framework.

-module(cpxlog).
-export([
	start/0,
	start_link/0,
	log/7,
	debug/6,
	info/6,
	notice/6,
	warning/6,
	error/6,
	critical/6,
	alert/6,
	emergency/6,
	set_loglevel/1,
	debug_module/1,
	nodebug_module/1
]).

-type(level() :: 'debug' | 'info' | 'notice' | 'warning' | 'error' | 'critical' | 'alert' | 'emergency').

-spec(start/0 :: () -> 'ok' | {'error', any()}).
start() ->
	Out = gen_event:start({local, cpxlog}),
	gen_event:add_handler(cpxlog, cpxlog_terminal, []),
	Out.
	
-spec(start_link/0 :: () -> 'ok' | {'error', any()}).
start_link() ->
	Out = gen_event:start_link({local, cpxlog}),
	gen_event:add_handler(cpxlog, cpxlog_terminal, []),
	Out.

-spec(log/7 :: (Level :: level(), Time :: any(), Module :: atom(), Line :: non_neg_integer(), Pid :: pid(), Message :: any(), Args :: [any()]) -> 'ok').
log(Level, Time, Module, Line, Pid, Message, Args) ->
	catch gen_event:notify(cpxlog, {Level, Time, Module, Line, Pid, Message, Args}),
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
	catch gen_event:notify(cpxlog, {set_log_level, Level}).

-spec(debug_module/1 :: (Module :: atom()) -> {'ok', atom()}).
debug_module(Module) ->
	catch gen_event:notify(cpxlog, {debug_module, Module}).

-spec(nodebug_module/1 :: (Module :: atom()) -> {'ok', atom()}).
nodebug_module(Module) ->
	catch gen_event:notify(cpxlog, {nodebug_module, Module}).
