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

-define(getRingout, element(2, cpx:get_env(default_ringout, 60)) * 1000).

-type(url_format() :: string()).
-type(client_opt() :: {any(), any()}).
-type(client_opts() :: [client_opt()]).
-record(client, {
		id :: string() | 'undefined',
		label :: string() | 'undefined',
		options = [] :: client_opts(),
		last_integrated :: 'undefined' | pos_integer(),
		timestamp = util:now() :: pos_integer()
}).

-record(call, {
		id = erlang:error({undefined, id}) :: string(),
		type = voice :: 'voice' | 'voicemail' | 'email' | 'chat',
		callerid = {"Unknown", "Unknown"} :: {string(), string()},
		dnis = "" :: string(),
		source = erlang:error({undefined, source}) :: pid(),
		bound = [] :: [pid()],
		% client record
		client = undefined :: 'undefined' | #client{}, %#client{label="Unknown", tenant=0, brand=0, timestamp = 1} :: ,
		skills = [] :: [atom()],
		cook :: pid() | 'undefined', % gen_media uses this
		% ring_path really means if the agent fsm can send an answer
		ring_path = outband :: 'inband' | 'outband' | 'any',
		% media_path really means if the agent fsm can send a hangup
		media_path = outband :: 'inband' | 'outband',
		direction = inbound :: 'inbound' | 'outbound',
		priority = 40 :: non_neg_integer()
}).

-record(queued_call, {
	media = erlang:error({undefined, media}) :: pid(),
	id = erlang:error({undefined, id}) :: string(),
	skills = [] :: [atom() | string()],
	dispatchers = [] :: [pid()],
	cook :: pid()
	}).

-type(transaction_type() :: 
	'cdrinit' | 
	'inivr' | 
	'dialoutgoing' |
	'inqueue' |
	'ringing' |
	'precall' |
	'oncall' |
	'outgoing' |
	'failedoutgoing' |
	'agent_transfer' |
	'queue_transfer' |
	'warmxfer' |
	'warmxfercomplete' |
	'warmxferfailed' |
	'warmxferleg' |
	'wrapup' |
	'endwrapup' |
	'abandonqueue' |
	'abandonivr' |
	'voicemail' |
	'hangup' |
	'undefined' |
	'cdrend'
).

-type(cpx_time() :: pos_integer()).
-type(callid() :: string()).
-type(cdr_proplist() :: [{any(), any()}]).

-record(cdr_raw, {
	id :: callid(),
	transaction :: transaction_type(),
	eventdata :: any(),
	start = util:now() :: cpx_time(),
	ended :: 'undefined' | cpx_time(),
	terminates = [] :: [transaction_type()] | 'infoevent',
	timestamp = util:now() :: cpx_time(),
	nodes = [] :: [atom()]
}).

-record(cdr_rec, {
	media :: #call{},
	summary = inprogress :: 'inprogress' | cdr_proplist(),
	transactions = inprogress :: 'inprogress' | [#cdr_raw{}],
	timestamp = util:now() :: cpx_time(),
	nodes = [] :: [atom()]
}).
