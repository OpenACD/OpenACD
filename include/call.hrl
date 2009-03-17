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

%% @hidden
-record(client, {
		label :: string(),
		tenant :: pos_integer(),
		brand :: pos_integer()
}).

-record(call, {
		id = erlang:error({undefined, id}) :: string(),
		type = voice :: 'voice' | 'email' | 'chat',
		callerid = "Unknown Unknown" :: string(),
		% source is the Pid of the media manager this is from
		source = erlang:error({undefined, source}) :: pid(),
		bound = [] :: [pid()],
		% client record
		client :: #client{},
		skills = [english] :: [atom(), ...],
		cook :: pid(), % TODO is this ever set?
		ring_path = outband :: 'inband' | 'outband' | 'any',
		media_path = outband :: 'inband' | 'outband'
}).

-record(queued_call, {
	media = erlang:error({undefined, media}) :: pid(),
	id = erlang:error({undefined, id}) :: string(),
	skills = [] :: [atom() | string()],
	dispatchers = [] :: [pid()],
	cook :: pid()
	}).


-ifdef(EUNIT).
-define(CONSOLE(Message, Args), ?debugFmt("[~p][~p][~p]~n	~s~n", [erlang:localtime(), node(), self(), lists:flatten(io_lib:format(Message, Args))])).
-else.
-define(CONSOLE(Message, Args), io:format("[~p][~p][~p][~p][~p]~n	~s~n", [erlang:localtime(), ?MODULE, ?LINE, node(), self(), lists:flatten(io_lib:format(Message, Args))])).
-endif.
