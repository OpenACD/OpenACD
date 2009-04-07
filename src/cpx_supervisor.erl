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

%% @doc The top-most supervisor of the cpx system.  This is responsible for starting and monitoring the primary supervisors
%% as well as any additional modules that are configured.  
%% Primary (hard coded) modules started:
%% <ul>
%% <li>{@link dispatch_manager}</li>
%% <li>{@link agent_manager}</li>
%% <li>{@link queue_manager}</li>
%% </ul>
%% Additional modules are loaded from the mnesia table 'cpx_conf'.  These modules would include 
%% those for agent authentication and media managers.  If it cannot build or access the 'cpx_conf' table
%% the supervisor does not start, thus halting all of cpx from starting.
%%
%% If the system starts without a cpx_conf table, it will build one, placing some default information there.
%% #cpx_conf{agent_auth, start, []} is added and started, as well as #cpx_conf{agent_tcp_listener}.
%% 
%% The 3 most important function are {@link add_conf/3}, {@link destroy/1}, {@link update_conf/4}.

-module(cpx_supervisor).
-author("Micah").

-include("call.hrl").
-include("agent.hrl").
-include("cpx.hrl").
-include_lib("stdlib/include/qlc.hrl").

-ifdef(EUNIT).
	-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(supervisor).

%% API
-export([start_link/1, start/1]).
-export([
	add_conf/3,
	build_spec/1,
	build_tables/0,
	destroy/1,
	update_conf/4,
	get_conf/1,
	stop/0
	]).
	
%% Supervisor callbacks
-export([init/1]).

%% API functions
%% @doc Start the cpx_supervisor linked to the parent process.
start_link(Nodes) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Nodes]).
%% @doc Start the cpx_supervisor unlinked.
start(Nodes) -> 
	{ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, [Nodes]),
	unlink(Pid),
	{ok, Pid}.

%% @doc Exit with reason `shutdown'
stop() ->
	?CONSOLE("stopping ~p...", [?MODULE]),
	exit(whereis(?MODULE), shutdown).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%% @private
init([Nodes]) ->
	% TODO Create warnings for missing/requires specs (at least one media manager, the agent_auth).
	?CONSOLE("starting cpx_supervisor on ~p", [node()]),
	case build_tables() of
		ok -> 
			DispatchSpec = {dispatch_manager, {dispatch_manager, start_link, []}, permanent, 2000, worker, [?MODULE]},
			AgentManagerSpec = {agent_manager, {agent_manager, start_link, [Nodes]}, permanent, 2000, worker, [?MODULE]},
			QueueManagerSpec = {queue_manager, {queue_manager, start_link, [Nodes]}, permanent, 20000, worker, [?MODULE]},
			
			Specs = lists:append([DispatchSpec, AgentManagerSpec, QueueManagerSpec], load_specs()),
			
			?CONSOLE("specs:  ~p", [supervisor:check_childspecs(Specs)]),
			{ok,{{one_for_one,3,5}, Specs}}
	end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Adds a configuration to get started and stores it in the database.  
%% Mod is the module name, Start is the function to start it, and 
%% Args is a list of terms passed to the start function.
add_conf(Mod, Start, Args) -> 
	Rec = #cpx_conf{module_name = Mod, start_function = Start, start_args = Args},
	F = fun() -> 
		mnesia:write(Rec)
	end,
	mnesia:transaction(F),
	start_spec(build_spec(Rec)).

%% @doc Attempts to build a valid childspec suitable for a supervisor module from the `#cpx_conf{}'.
build_spec(#cpx_conf{module_name = Mod, start_function = Start, start_args = Args}) -> 
	Spec = {Mod, {Mod, Start, Args}, permanent, 20000, worker, [?MODULE]},
	?CONSOLE("Building spec:  ~p", [Spec]),
	case supervisor:check_childspecs([Spec]) of
		ok -> 
			Spec;
		Else -> 
			?CONSOLE("Spec failed check:  ~p", [Spec]),
			Else
	end.

%% @doc Attempts to build the `cpx_conf' table.
build_tables() ->
	?CONSOLE("cpx building tables...",[]),
	A = util:build_table(cpx_conf, [
		{attributes, record_info(fields, cpx_conf)},
		{disc_copies, lists:append([[node()], nodes()])},
		{local_content, true}
	]),
	case A of
		{atomic, ok} -> 
			% create some default info so the system is at least a bit usable.
			F = fun() -> 
				mnesia:write(#cpx_conf{module_name = agent_auth, start_function = start, start_args = []}),
				mnesia:write(#cpx_conf{module_name = agent_tcp_listener, start_function = start, start_args = [1337]}),
				mnesia:write(#cpx_conf{module_name = cpx_web_management, start_function = start, start_args = []})
			end,
			case mnesia:transaction(F) of
				{atomic, ok} -> 
					ok;
				Else -> 
					Else
			end;
		Else ->
			?CONSOLE("unusual response building tables: ~p", [Else]),
			Else
	end.

%% @doc Removes the passed `childspec()' or `#cpx_conf{}' from the database.
destroy({Id, _Params, _Transience, _Time, _Type, _Module}) -> 
	destroy(Id);
destroy(Spec) when is_atom(Spec) -> 
	F = fun() -> 
		mnesia:delete({cpx_conf, Spec})
	end,
	mnesia:transaction(F),
	stop_spec(Spec).

%% @doc updates the conf with key `Name' with new `Mod', `Start', and `Args'.
%% @see add_conf/3
update_conf(Name, Mod, Start, Args) -> 
	Rec = #cpx_conf{module_name = Mod, start_function = Start, start_args = Args},
	F = fun() -> 
		destroy(Name),
		mnesia:write(Rec)
	end,
	mnesia:transaction(F),
	start_spec(build_spec(Rec)).

get_conf(Name) ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(cpx_conf), X#cpx_conf.module_name =:= Name]),
		qlc:e(QH)
	end,
	case mnesia:transaction(F) of
		{atomic, []} ->
			undefined;
		{atomic, [Rec]} ->
			Rec
	end.

%% @private
start_spec(Spec) ->
	supervisor:delete_child(?MODULE, element(1, Spec)),
	supervisor:start_child(?MODULE, Spec).

stop_spec(SpecID) ->
	supervisor:terminate_child(?MODULE, SpecID).

%% @private
load_specs() -> 
	?CONSOLE("loading specs...",[]),
	F = fun() -> 
		QH = qlc:q([X || X <- mnesia:table(cpx_conf)]),
		qlc:e(QH)
	end,
	case mnesia:transaction(F) of
		{atomic, Records} -> 
			lists:map(fun(I) -> build_spec(I) end, Records);
		Else -> 
			?CONSOLE("unable to retrieve specs:  ~p", [Else]),
			Else
	end.

	
-ifdef(EUNIT).

config_test_() -> 
	["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
	{
		setup,
		fun() -> 
			?CONSOLE("f1 ~p", [mnesia:stop()]),
			?CONSOLE("f2 ~p", [mnesia:delete_schema([node()])]),
			?CONSOLE("f3 ~p", [mnesia:create_schema([node()])]),
			?CONSOLE("F4 ~p", [mnesia:start()]),
			?CONSOLE("findme ~p", [cpx_supervisor:start([node()])])
		end,
		fun(_Whatever) -> 
			cpx_supervisor:stop(),
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			ok
		end,
		[
			{
				"Adding a Valid Config",
				fun() -> 
					Valid = #cpx_conf{module_name = dummy_mod, start_function = start, start_args = []},
					try add_conf(dummy_mod, start, [])
					catch
						_:_ -> ok
					end,
					QH = qlc:q([X || X <- mnesia:table(cpx_conf), X#cpx_conf.module_name =:= dummy_mod]),
					F = fun() -> 
						qlc:e(QH)
					end,
					?assertMatch({atomic, [Valid]}, mnesia:transaction(F)),
					destroy(dummy_mod)
				end
			},
			{
				"Destroy a Config by full spec",
				fun() -> 
					Spec = {dummy_mod, {dummy_mod, start, []}, permanent, 100, worker, [?MODULE]},
					try add_conf(dummy_mod, start, [])
					catch
						_:_ -> ok
					end,
					destroy(Spec),
					QH = qlc:q([X || X <- mnesia:table(cpx_conf), X#cpx_conf.module_name =:= dummy_mod]),
					F = fun() -> 
						qlc:e(QH)
					end,
					?assertMatch({atomic, []}, mnesia:transaction(F))
				end
			},
			{
				"Destroy a Config by id only",
				fun() -> 
					try add_conf(dummy_mod, start, [])
					catch
						_:_ -> ok
					end,
					destroy(dummy_mod),
					QH = qlc:q([X || X <- mnesia:table(cpx_conf), X#cpx_conf.module_name =:= dummy_mod]),
					F = fun() -> 
						qlc:e(QH)
					end,
					?assertMatch({atomic, []}, mnesia:transaction(F))
				end
			},
			{
				"Update a Config",
				fun() -> 
					%Spec = {dummy_mod, {dummy_mod, start, []}, permanent, 100, worker, [?MODULE]},
					try add_conf(dummy_mod, start, [])
					catch
						_:_ -> ok
					end,
					update_conf(dummy_mod, new_mod, new_start, [new_arg]),
					QH = qlc:q([X || X <- mnesia:table(cpx_conf), X#cpx_conf.module_name =:= dummy_mod]),
					F = fun() ->
						qlc:e(QH)
					end,
					?assertMatch({atomic, []}, mnesia:transaction(F)),
					Valid = #cpx_conf{module_name = new_mod, start_function = new_start, start_args = [new_arg]},
					QH2 = qlc:q([X || X <- mnesia:table(cpx_conf), X#cpx_conf.module_name =:= new_mod]),
					F2 = fun() -> 
						qlc:e(QH2)
					end,
					?assertMatch({atomic, [Valid]}, mnesia:transaction(F2)),
					destroy(new_mod),
					destroy(dummy_mod)
				end
			},
			{
				"Build a Spec from Record",
				fun() -> 
					Record = #cpx_conf{module_name = dummy_mod, start_function = start, start_args = []},
					?assertMatch({dummy_mod, {dummy_mod, start, []}, permanent, 20000, worker, [?MODULE]}, build_spec(Record))
				end
			}
		]
	}.

mutlinode_test_() ->
	["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
	{
		foreach,
		fun() ->
			[_Name, Host] = string:tokens(atom_to_list(node()), "@"),
			Master = list_to_atom(lists:append("master@", Host)),
			Slave = list_to_atom(lists:append("slave@", Host)),
			
			slave:start(net_adm:localhost(), master, " -pa debug_ebin"),
			slave:start(net_adm:localhost(), slave, " -pa debug_ebin"),
			mnesia:stop(),

			mnesia:change_config(extra_db_nodes, [Master, Slave]),
			?CONSOLE("~p", [mnesia:delete_schema([node(), Master, Slave])]),
			?CONSOLE("~p", [mnesia:create_schema([node(), Master, Slave])]),
			
			cover:start([Master, Slave]),
			
			rpc:call(Master, mnesia, start, []),
			rpc:call(Slave, mnesia, start, []),
			mnesia:start(),

			?CONSOLE("~p", [mnesia:change_table_copy_type(schema, Master, disc_copies)]),
			?CONSOLE("~p", [mnesia:change_table_copy_type(schema, Slave, disc_copies)]),
			
			% nix the agent_tcp_default and web_managmeent to keep addresses from binding
			rpc:call(Master, cpx_supervisor, build_tables, []),
			rpc:call(Slave, cpx_supervisor, build_tables, []),

			rpc:call(Master, cpx_supervisor, destroy, [agent_tcp_listener]),
			rpc:call(Slave, cpx_supervisor, destroy, [agent_tcp_listener]),
			rpc:call(Master, cpx_supervisor, destroy, [cpx_web_management]),
			rpc:call(Slave, cpx_supervisor, destroy, [cpx_web_management]),

			{Master, Slave}
		end,
		fun({Master, Slave}) ->
			cover:stop([Master, Slave]),
			
			slave:stop(Master),
			slave:stop(Slave),
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			
			ok
		end,
		[
			fun({Master, Slave}) ->
				{"Start on two different nodes",
				fun() ->
					Masterres = rpc:call(Master, ?MODULE, start, [[Master, Slave]]),
					%timer:sleep(3000),
					Slaveres = rpc:call(Slave, ?MODULE, start, [[Master, Slave]]),
					?CONSOLE("M:  ~p; S:  ~p", [Masterres, Slaveres]),
					?assertMatch({ok, _Pid}, Masterres),
					?assertMatch({ok, _Pid2}, Slaveres)
				end}
			end
		]
	}.
	
-endif.
