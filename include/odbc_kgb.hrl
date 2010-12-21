%% "The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%%
%%	The Original Code is OpenACD.
%%
%%	The Initial Developers of the Original Code is 
%%	Andrew Thompson.
%%
%%	All portions of the code written by the Initial Developers are Copyright
%%	(c) 2008-2010 Andrew Thompson.
%%	All Rights Reserved.
%%
%%	Contributor(s):
%%
%%	Andrew Thompson <andrew at hijacked dot us>
%% 	Micah Warre <micahw at lordnull dot com>

-type(event_type() ::
	'acd_start' |
	'acd_stop' |
	'agent_login' |
	'agent_logout' |
	'agent_start' |
	'agent_stop' |
	'agent_available' |
	'agent_unavailable' |
	'call_enqueue' |
	%'call_pickup' |
	'call_answer' |
	'call_terminate' |
	'transfer'
).

-type(dsn() :: string()).

-record(event_log_row, {
	id :: string(),
	hostname :: string(),
	event_type :: 'undefined' | event_type(),
	agent_id :: string(),
	acd_type = "openacd" :: string(),
	acd_name :: string(), % usually hostname
	acd_agent_id :: string(), % usually agent login
	acd_agent_ip :: string(), 
	ani :: string(), % field one of from_header
	uci :: string(), % field 2 of from_header
	did :: string(), % field 4 of from_header
	origin_code :: string(), % field 3 of from_header
	queue :: string(),
	queue_name :: string(),
	source_ip = "Source IP" :: string(),
	from_header :: string(), % '*' deliminated string value
	information :: string(),
	created_at :: string(), % formated datetime string
	% updated at is left to the db.
	line :: string() % remnant of using events.log; remainder of line.
}).