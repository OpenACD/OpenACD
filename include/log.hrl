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
%%	Micah Warren <micahw at fusedsolutions dot com>
%%

-define(LOGLEVELS, [
	debug,
	info,
	notice,
	warning,
	error,
	critical,
	alert,
	emergency
]).

-type(loglevels() :: 'debug' | 'info' | 'notice' | 'warning' | 'error' | 'critical' | 'alert' | 'emergency').

-ifdef(EUNIT).
-define(DEBUG(Message, Args), ?debugFmt("[~p][~p][~p]~n	DEBUG: ~s~n", [erlang:localtime(), node(), self(), lists:flatten(io_lib:format(Message, Args))])).
-define(INFO(Message, Args), ?debugFmt("[~p][~p][~p]~n	INFO: ~s~n", [erlang:localtime(), node(), self(), lists:flatten(io_lib:format(Message, Args))])).
-define(NOTICE(Message, Args), ?debugFmt("[~p][~p][~p]~n	NOTICE: ~s~n", [erlang:localtime(), node(), self(), lists:flatten(io_lib:format(Message, Args))])).
-define(WARNING(Message, Args), ?debugFmt("[~p][~p][~p]~n	WARNING: ~s~n", [erlang:localtime(), node(), self(), lists:flatten(io_lib:format(Message, Args))])).
-define(ERROR(Message, Args), ?debugFmt("[~p][~p][~p]~n	ERROR: ~s~n", [erlang:localtime(), node(), self(), lists:flatten(io_lib:format(Message, Args))])).
-define(CRITICAL(Message, Args), ?debugFmt("[~p][~p][~p]~n	CRITICAL: ~s~n", [erlang:localtime(), node(), self(), lists:flatten(io_lib:format(Message, Args))])).
-define(ALERT(Message, Args), ?debugFmt("[~p][~p][~p]~n	ALERT: ~s~n", [erlang:localtime(), node(), self(), lists:flatten(io_lib:format(Message, Args))])).
-define(EMERGENCY(Message, Args), ?debugFmt("[~p][~p][~p]~n	EMERGENCY: ~s~n", [erlang:localtime(), node(), self(), lists:flatten(io_lib:format(Message, Args))])).
-else.
-define(DEBUG(Message, Args), cpxlog:debug(erlang:localtime(), ?MODULE, ?LINE, self(), Message, Args)).
-define(INFO(Message, Args), cpxlog:info(erlang:localtime(), ?MODULE, ?LINE, self(), Message, Args)).
-define(NOTICE(Message, Args), cpxlog:notice(erlang:localtime(), ?MODULE, ?LINE, self(), Message, Args)).
-define(WARNING(Message, Args), cpxlog:warning(erlang:localtime(), ?MODULE, ?LINE, self(), Message, Args)).
-define(ERROR(Message, Args), cpxlog:error(erlang:localtime(), ?MODULE, ?LINE, self(), Message, Args)).
-define(CRITICAL(Message, Args), cpxlog:critical(erlang:localtime(), ?MODULE, ?LINE, self(), Message, Args)).
-define(ALERT(Message, Args), cpxlog:alert(erlang:localtime(), ?MODULE, ?LINE, self(), Message, Args)).
-define(EMERGENCY(Message, Args), cpxlog:emergency(erlang:localtime(), ?MODULE, ?LINE, self(), Message, Args)).
-endif.

-ifdef(EUNIT).
-define(CONSOLE(Message, Args), ?debugFmt("[~p][~p][~p]~n	~s~n", [erlang:localtime(), node(), self(), lists:flatten(io_lib:format(Message, Args))])).
-endif.
