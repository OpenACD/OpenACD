%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%%
%% The Original Code is Spice Telphony.
%%
%% The Initial Developer of the Original Code is
%% Andrew Thompson and Micah Warren.
%% Portions created by the Initial Developers are Copyright (C)
%% SpiceCSM. All Rights Reserved.

%% Contributor(s):

%% Andrew Thompson <athompson at spicecsm dot com>
%% Micah Warren <mwarren at spicecsm dot com>
%%

-record(client, {
		tenant :: pos_integer(),
		brand :: pos_integer(),
		label :: string()
}).

-record(call, {
		id :: string(),
		type = voice :: 'voice' | 'email' | 'chat',
		callerid = "Unknown Unknown" :: string(),
		% source is the Pid of the media manager this is from
		source :: pid(),
		bound = [] :: [pid()],
		% client record
		client :: #client{},
		skills = [english] :: [atom(), ...],
		cook :: pid(),
		ring_path = outband :: 'inband' | 'outband' | 'any',
		media_path = outband :: 'inband' | 'outband'
}).

-record(queued_call, {
	media :: pid(),
	id :: string(),
	skills = [] :: [atom()],
	dispatchers = [] :: [pid()],
	cook :: pid()
	}).


-ifdef(EUNIT).
-define(CONSOLE(Message, Args), ?debugFmt("[~p][~p]~n            " ++ Message ++ "~n", lists:append([[erlang:localtime(), self()], Args]))).
-else.
-define(CONSOLE(Message, Args), io:format("[~p][~p][~p][~p]~n            " ++ Message ++ "~n", lists:append([[erlang:localtime(), ?MODULE, ?LINE, self()], Args]))).
-endif.
