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

-export([start/2, stop/1]).

-spec(start/2 :: (Type :: 'normal' | {'takeover', atom()} | {'failover', atom()}, StartArgs :: [any()]) -> {'ok', pid(), any()} | {'ok', pid()} | {'error', any()}).
start(_Type, StartArgs) ->
	?DEBUG("Start args ~p", [StartArgs]),
	?DEBUG("All env: ~p", [application:get_all_env(cpx)]),
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
					?NOTICE("Alive nodes: ~p", [AliveNodes]),
					mnesia:change_config(extra_db_nodes, AliveNodes)
			end,
			ok;
		_Else ->
			Nodes = [node()]
	end,
	mnesia:change_table_copy_type(schema, node(), disc_copies),
	mnesia:set_master_nodes(lists:umerge(Nodes, [node()])),
	cpx_supervisor:start_link(Nodes).

-spec(stop/1 :: (State :: any()) -> 'ok').
stop(_State) -> 
	ok.
