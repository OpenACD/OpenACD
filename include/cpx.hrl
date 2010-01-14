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

%% @hidden

% child_spec() = {Id,StartFunc,Restart,Shutdown,Type,Modules}

-record(cpx_conf, {
	id = erlang:error({undefined, id}) :: any(),
	module_name = erlang:error({undefined, module_name}) :: atom(),
	start_function = erlang:error({undefined, start_function}) :: atom(),
	start_args = [] :: [any()],
	supervisor = management_sup :: 'routing_sup' | 'agent_sup' | 'agent_connection_sup' | 'management_sup' | 'mediamanager_sup',
	timestamp = util:now() :: pos_integer()
}).

-record(cpx_value, {
	key :: any(),
	value :: any(),
	timestamp = util:now() :: pos_integer()
}).

%cpx ->
%	cpx_web_mangement
%	{routing, temp, 1for1} ->
%		{dispatch_manager, permanent}
%		{queue_manager, permanent}
%	agent ->
%		agent_manager
%		agent_connections ->
%			agent_tcp_connection
%			agent_web_connection
%	media ->
%		freeswitch
