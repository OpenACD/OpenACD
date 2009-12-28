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

%% @doc The application module.
-module(cpx).
-author("Micah").

-behaviour(application).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").

-ifdef(EUNIT).
	-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start/2, prep_stop/1, stop/1]).

-spec(start/2 :: (Type :: 'normal' | {'takeover', atom()} | {'failover', atom()}, StartArgs :: [any()]) -> {'ok', pid(), any()} | {'ok', pid()} | {'error', any()}).
start(_Type, StartArgs) ->
	io:format("Start args ~p~n", [StartArgs]),
	io:format("All env: ~p~n", [application:get_all_env(cpx)]),
	crypto:start(),
	%Nodes = lists:append([nodes(), [node()]]),
	%mnesia:create_schema(Nodes),
	%mnesia:start(),
	case application:get_env(cpx, nodes) of
		{ok, Nodes} ->
			lists:foreach(fun(Node) -> net_adm:ping(Node) end, Nodes),
			case nodes() of
				[] ->
					ok;
				AliveNodes ->
					io:format("Alive nodes: ~p~n", [AliveNodes]),
					mnesia:change_config(extra_db_nodes, AliveNodes)
			end,
			ok;
		_Else ->
			Nodes = [node()]
	end,
	mnesia:change_table_copy_type(schema, node(), disc_copies),
	mnesia:set_master_nodes(lists:umerge(Nodes, [node()])),
	try cpx_supervisor:start_link(Nodes) of
		{ok, Pid} ->
			?NOTICE("Application cpx started sucessfully!", []),
			{ok, Pid}
	catch
		What:Why ->
			?ERROR("Application cpx failed to start successfully! ~p:~p", [What, Why]),
			{What, Why}
	end.

-spec(prep_stop/1 :: (State :: any()) -> any()).
prep_stop(State) ->
	?NOTICE("Application cpx stopping...", []),
	State.

-spec(stop/1 :: (State :: any()) -> 'ok').
stop(_State) ->
	ok.
	
% to be added soon TODO
%
%get_queue
%get_agent
%get_agents/0 /1
%get_queues/0 /1
%get_media/1 %% string representation of pid, or a real pid.
%kick_agent/1
%kick_call/1
%can_answer/2 (Media, Agent) -> true | missing skills
%migrate_queue/1 -> for the future.
%cdr:orphan_search
%cdr:pending_states (agent/cdr), global/node
%start_spec pretty print.
%help command
%cpx:start_spec
%is_running (cpx_supervisor specs)
%uptime

