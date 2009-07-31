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
%% The 3 most important function are {@link add_conf/5}, {@link destroy/1}, {@link update_conf/4}.

-module(cpx_supervisor).
-author("Micah").

-include("log.hrl").
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
	add_conf/5,
	add_conf/1,
	build_spec/1,
	build_tables/0,
	destroy/1,
	update_conf/2,
	get_conf/1,
	stop/0,
	load_specs/1,
	restart/2
	]).
	
%% Supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, true).
-include("gen_spec.hrl").

-type(supervisor_name() :: 'routing_sup' | 'agent_sup' | 'agent_connection_sup' | 'management_sup').

%% API functions
%% @doc Start the cpx_supervisor linked to the parent process.
-spec(start_link/1 :: (Nodes :: [atom()]) -> {'ok', pid()}).
start_link(Nodes) ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, [Nodes]),
	
	Routingspec = {routing_sup, {cpx_middle_supervisor, start_named, [3, 5, routing_sup]}, temporary, 2000, supervisor, [?MODULE]},
	Managementspec = {management_sup, {cpx_middle_supervisor, start_named, [3, 5, management_sup]}, permanent, 2000, supervisor, [?MODULE]},
	Agentspec = {agent_sup, {cpx_middle_supervisor, start_named, [3, 5, agent_sup]}, temporary, 2000, supervisor, [?MODULE]},
	Specs = [Routingspec, Agentspec, Managementspec],
	?DEBUG("specs:  ~p", [supervisor:check_childspecs(Specs)]),
	
	supervisor:start_child(Pid, Managementspec),
	%load_specs(),
	Cpxmonitorspec = {cpx_monitor, {cpx_monitor, start_link, [[{nodes, Nodes}, auto_restart_mnesia]]}, permanent, 2000, worker, [?MODULE]},
	supervisor:start_child(management_sup, Cpxmonitorspec),

	supervisor:start_child(Pid, Routingspec),
	
	Cpxlogspec = {cpxlog, {cpxlog, start_link, []}, permanent, brutal_kill, worker, [?MODULE]},
	DispatchSpec = {dispatch_manager, {dispatch_manager, start_link, []}, permanent, 2000, worker, [?MODULE]},
	QueueManagerSpec = {queue_manager, {queue_manager, start_link, [Nodes]}, permanent, 20000, worker, [?MODULE]},
	Cdrspec = {cdr, {cdr, start_link, []}, permanent, brutal_kill, worker, [?MODULE]},
	
	supervisor:start_child(routing_sup, DispatchSpec),
	supervisor:start_child(routing_sup, QueueManagerSpec),
	supervisor:start_child(routing_sup, Cpxlogspec),
	supervisor:start_child(routing_sup, Cdrspec),
	
	supervisor:start_child(Pid, Agentspec),
	
	Agentconnspec = {agent_connection_sup, {cpx_middle_supervisor, start_named, [3, 5, agent_connection_sup]}, temporary, 2000, supervisor, [?MODULE]},
	AgentManagerSpec = {agent_manager, {agent_manager, start_link, [Nodes]}, permanent, 2000, worker, [?MODULE]},
	supervisor:start_child(agent_sup, AgentManagerSpec),
	supervisor:start_child(agent_sup, Agentconnspec),
	
	
	{ok, Pid}.
	
%% @doc Start the cpx_supervisor unlinked.
-spec(start/1 :: (Nodes :: [atom()]) -> {'ok', pid()}).
start(Nodes) -> 
	{ok, Pid} = start_link(Nodes),
	unlink(Pid),
	{ok, Pid}.

%% @doc Exit with reason `shutdown'
-spec(stop/0 :: () -> 'true').
stop() ->
	?NOTICE("stopping ~p...", [?MODULE]),
	exit(whereis(?MODULE), shutdown).

%% @doc In case of a branch going down, this funciton will restart the specified branch.
-spec(restart/2 :: (Branch :: supervisor_name(), Args :: [any()]) -> 'ok').
restart(agent_connection_sup, _Args) ->
	?INFO("Restaring agent_connection_sup.", []),
	supervisor:restart_child(agent_sup, agent_connection_sup);
restart(routing_sup, [Nodes]) ->
	Out = supervisor:restart_child(cpx_supervisor, routing_sup),
	DispatchSpec = {dispatch_manager, {dispatch_manager, start_link, []}, permanent, 2000, worker, [?MODULE]},
	QueueManagerSpec = {queue_manager, {queue_manager, start_link, [Nodes]}, permanent, 20000, worker, [?MODULE]},
	Cpxlogspec = {cpxlog, {cpxlog, start_link, []}, permanent, brutal_kill, worker, [?MODULE]},
	Cdrspec = {cdr, {cdr, start_link, []}, permanent, brutal_kill, worker, [?MODULE]},
	supervisor:start_child(routing_sup, DispatchSpec),
	supervisor:start_child(routing_sup, QueueManagerSpec),
	supervisor:start_child(routing_sup, Cpxlogspec),
	supervisor:start_child(routing_sup, Cdrspec),
	Out;
restart(agent_sup, [Nodes]) ->
	?INFO("Restarting agent_sup.", []),
	Out = supervisor:restart_child(cpx_supervisor, agent_sup),
	Agentconnspec = {agent_connection_sup, {cpx_middle_supervisor, start_named, [3, 5, agent_connection_sup]}, temporary, 2000, supervisor, [?MODULE]},
	AgentManagerSpec = {agent_manager, {agent_manager, start_link, [Nodes]}, permanent, 2000, worker, [?MODULE]},
	supervisor:start_child(agent_sup, Agentconnspec),
	supervisor:start_child(agent_sup, AgentManagerSpec),
	Out;
restart(Branch, _Args) when is_atom(Branch) ->
	supervisor:restart_child(cpx_supervisor, Branch),
	load_specs(Branch).
	
%%====================================================================
%% Supervisor callbacks
%%====================================================================
%% @private
init([_Nodes]) ->
	% TODO Nodes is no longer used here.
	% TODO Create warnings for missing/requires specs (at least one media manager, the agent_auth).
	?DEBUG("starting cpx_supervisor on ~p", [node()]),
	case build_tables() of
		ok ->
			{ok,{{one_for_one,3,5}, []}}
	end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Adds a configuration to get started and stores it in the database.  
%% Mod is the module name, Start is the function to start it, and 
%% Args is a list of terms passed to the start function.
%% Erlang/OTP documentation recommends keeping the module name and 
%% id the same; the ability to keep them separate is here for completeness
%% and flexibility.  It is recommended to treat this function as an API to be used
%% by other modules rather than call it directly from a shell.
-spec(add_conf/5 :: (Id :: atom(), Mod :: atom(), Start :: atom(), Args :: [any()], Super :: supervisor_name()) -> {'atomic', 'ok'}).
add_conf(Id, Mod, Start, Args, Super) -> 
	Rec = #cpx_conf{id = Id, module_name = Mod, start_function = Start, start_args = Args, supervisor = Super, timestamp = 1},
	add_conf(Rec).

-spec(add_conf/1 :: (Rec :: #cpx_conf{}) -> {'atomic', 'ok'}).
add_conf(Rec) ->
	F = fun() -> 
		mnesia:write(Rec#cpx_conf{timestamp = util:now()}),
		start_spec(Rec)
	end,
	mnesia:transaction(F).

%% @doc Attempts to build a valid childspec suitable for a supervisor module from the `#cpx_conf{}'.
-spec(build_spec/1 :: (Spec :: #cpx_conf{}) -> child_spec() | {'error', any()}).
build_spec(#cpx_conf{module_name = Mod, start_function = Start, start_args = Args, id = Id}) -> 
	Spec = {Id, {Mod, Start, Args}, permanent, 20000, worker, [?MODULE]},
	?DEBUG("Building spec:  ~p", [Spec]),
	case supervisor:check_childspecs([Spec]) of
		ok -> 
			Spec;
		Else -> 
			?ERROR("Spec failed check:  ~p", [Spec]),
			Else
	end.

%% @doc Attempts to build the `cpx_conf' table.
-spec(build_tables/0 :: () -> 'ok' | {'error', any()}).
build_tables() ->
	?DEBUG("cpx building tables...",[]),
	A = util:build_table(cpx_conf, [
		{attributes, record_info(fields, cpx_conf)},
		{disc_copies, [node()]},
		{local_content, true}
	]),
	case A of
		Result when Result =:= {atomic, ok}; Result =:= copied -> 
			% create some default info so the system is at least a bit usable.
			F = fun() -> 
				mnesia:write(#cpx_conf{id = agent_auth, module_name = agent_auth, start_function = start_link, start_args = [], supervisor=agent_connection_sup, timestamp = util:now()}),
				mnesia:write(#cpx_conf{id = agent_web_listener, module_name = agent_web_listener, start_function = start_link, start_args = [5050], supervisor=agent_connection_sup, timestamp = util:now()}),
				mnesia:write(#cpx_conf{id = cpx_web_management, module_name = cpx_web_management, start_function = start_link, start_args = [], supervisor = management_sup, timestamp = util:now()})
			end,
			case mnesia:transaction(F) of
				{atomic, ok} -> 
					ok;
				Else -> 
					Else
			end;
		exists ->
			ok;
		Else ->
			?NOTICE("unusual response building tables: ~p", [Else]),
			Else
	end.

%% @doc Removes the passed `childspec()' or `#cpx_conf{}' from the database.
-spec(destroy/1 :: (Spec :: child_spec() | atom()) -> {'atomic', 'ok'}).
destroy(Spec) when is_record(Spec, cpx_conf) ->
	F = fun() ->
		stop_spec(Spec),
		mnesia:delete({cpx_conf, Spec#cpx_conf.id})
	end,
	mnesia:transaction(F);
destroy(Spec) ->
	F = fun() ->
		[Rec] = mnesia:read({cpx_conf, Spec}),
		stop_spec(Rec),
		mnesia:delete({cpx_conf, Spec})
	end,
	mnesia:transaction(F).

%% @doc updates the conf with key `Name' with new `Mod', `Start', and `Args'.
%% @see add_conf/5
-spec(update_conf/2 :: (Id :: atom(), Conf :: #cpx_conf{}) -> {'atomic', 'ok'}).
update_conf(Id, Conf) when is_record(Conf, cpx_conf) ->
	F = fun() ->
		destroy(Id),
		start_spec(Conf),
		mnesia:write(Conf#cpx_conf{timestamp = util:now()})
	end,
	mnesia:transaction(F).

%% @doc Pull the `#cpx_conf{}' from the database for the given module name.
-spec(get_conf/1 :: (Name :: atom()) -> 'undefined' | #cpx_conf{}).
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
start_spec(Spec) when is_record(Spec, cpx_conf) ->
	?DEBUG("Starting ~p with supervisor ~p", [Spec#cpx_conf.id, Spec#cpx_conf.supervisor]),
	cpx_middle_supervisor:add_with_middleman(Spec#cpx_conf.supervisor, 3, 5, Spec).

stop_spec(Spec) when is_record(Spec, cpx_conf) ->
	Out = cpx_middle_supervisor:drop_child(Spec#cpx_conf.supervisor, Spec),
%	Out = supervisor:terminate_child(Spec#cpx_conf.supervisor, Spec#cpx_conf.id),
	?DEBUG("Out:  ~p.  Spec:  ~p.", [Out, Spec]),
	Out.

%% @private
%-spec(load_specs/0 :: () -> {'error', any()} | none()).
%load_specs() -> 
%	?DEBUG("loading specs...",[]),
%	F = fun() -> 
%		QH = qlc:q([X || X <- mnesia:table(cpx_conf)]),
%		qlc:e(QH)
%	end,
%	case mnesia:transaction(F) of
%		{atomic, Records} -> 
%			lists:map(fun(I) -> start_spec(I) end, Records);
%		Else -> 
%			?ERROR("unable to retrieve specs:  ~p", [Else]),
%			Else
%	end.

-spec(load_specs/1 :: (Super :: supervisor_name()) -> {'aborted', any()} | 'ok').
load_specs(Super) ->
	?DEBUG("loading specs for supervisor ~s", [Super]),
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(cpx_conf), X#cpx_conf.supervisor =:= Super]),
		qlc:e(QH)
	end,
	case mnesia:transaction(F) of
		{atomic, Records} ->
			lists:foreach(fun(I) -> start_spec(I) end, Records);
		Else ->
			?ERROR("unable to retrieve specs for ~s:  ~p", [Super, Else]),
			Else
	end.

-ifdef(EUNIT).

config_test_() -> 
	["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
	{
		foreach,
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
				"Adding a Valid Config gets it to start",
				fun() -> 
					Valid = #cpx_conf{id = dummy_media_manager, module_name = dummy_media_manager, start_function = start_link, start_args = ["dummy_arg"], supervisor = management_sup, timestamp=1},
					add_conf(dummy_media_manager, dummy_media_manager, start_link, ["dummy_arg"], management_sup),
					QH = qlc:q([X || X <- mnesia:table(cpx_conf), X#cpx_conf.module_name =:= dummy_media_manager]),
					F = fun() -> 
						qlc:e(QH)
					end,
					?CONSOLE("~p", [mnesia:transaction(F)]),
					?assertMatch({atomic, [#cpx_conf{}]}, mnesia:transaction(F)),
					?assert(is_pid(whereis(dummy_media_manager)))
				end
			},
			{
				"Destroy a Config by full spec, ensure it also kills what was running.",
				fun() -> 
					Spec = #cpx_conf{id = dummy_id, module_name = dummy_media_manager, start_function = start_link, start_args = ["dummy_arg"], supervisor = management_sup, timestamp = util:now()},
					try add_conf(dummy_id, dummy_media_manager, start_link, ["dummy_arg"], management_sup)
					catch
						_:_ -> ok
					end,
					?assert(is_pid(whereis(dummy_media_manager))),
					destroy(Spec),
					?assertEqual(undefined, whereis(dummy_media_manager)),
					QH = qlc:q([X || X <- mnesia:table(cpx_conf), X#cpx_conf.module_name =:= dummy_media_manager]),
					F = fun() -> 
						qlc:e(QH)
					end,
					?assertMatch({atomic, []}, mnesia:transaction(F))
				end
			},
			{
				"Destroy a Config by id only",
				fun() -> 
					add_conf(dummy_media_manager, dummy_media_manager, start_link, ["dummy_arg"], management_sup),
					?assert(is_pid(whereis(dummy_media_manager))),
					destroy(dummy_media_manager),
					?CONSOLE("~p", [whereis(dummy_media_manager)]),
					?assertEqual(undefined, whereis(dummy_media_manager)),
					QH = qlc:q([X || X <- mnesia:table(cpx_conf), X#cpx_conf.module_name =:= dummy_media_manager]),
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
					add_conf(dummy_media_manager, dummy_media_manager, start_link, ["dummy_arg"], management_sup),
%					try add_conf(dummy_media_manager, dummy_media_manager, start_link, ["dummy_arg"], management_sup)
%					catch
%						_:_ -> ok
%					end,
					Oldpid = whereis(dummy_media_manager),
					Newrec = #cpx_conf{
						id=dummy_media_manager,
						module_name = dummy_media_manager,
						start_function = start_link,
						start_args = ["new_arg"],
						supervisor = management_sup,
						timestamp = util:now()
					},
					update_conf(dummy_media_manager, Newrec),
					Newpid = whereis(dummy_media_manager),
					?assertNot(Oldpid =:= Newpid),
					QH = qlc:q([X || X <- mnesia:table(cpx_conf), X#cpx_conf.module_name =:= dummy_media_manager]),
					F = fun() ->
						qlc:e(QH)
					end,
					{atomic, [Rec]} = mnesia:transaction(F),
					?assertEqual(["new_arg"], Rec#cpx_conf.start_args),
					destroy(dummy_media_manager)
				end
			},
			{
				"Build a Spec from Record",
				fun() -> 
					Record = #cpx_conf{id = dummy_id, module_name = dummy_mod, start_function = start, start_args = [], timestamp = util:now()},
					?assertMatch({dummy_id, {dummy_mod, start, []}, permanent, 20000, worker, [?MODULE]}, build_spec(Record))
				end
			}
		]
	}.

murder_test_() ->
	{foreach,
	fun() ->
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		mnesia:create_schema([node()]),
		mnesia:start(),
		cpx_supervisor:start([node()]),
		ok
	end,
	fun(ok) -> 
		cpx_supervisor:stop(),
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		ok
	end,
	[{"Killing the management branch",
	fun() ->
		Where = whereis(management_sup),
		?assertNot(Where =:= undefined),
		exit(whereis(management_sup), kill),
		timer:sleep(5),
		restart(management_sup, []),
		Newwhere = whereis(management_sup),
		?assertNot(Where =:= Newwhere),
		?assertNot(whereis(cpx_web_management) =:= undefined)
	end},
	{"Killing the agent connection branch (and bringing it back)",
	fun() ->
		Where = whereis(agent_connection_sup),
		?assertNot(Where =:= undefined),
		exit(Where, kill),
		timer:sleep(5),
		restart(agent_connection_sup, []),
		Newwhere = whereis(agent_connection_sup),
		?assertNot(Where =:= Newwhere),
		?assertNot(whereis(agent_auth) =:= undefined)
	end},
	{"Killing the agent branch (and bringing it back)",
	fun() ->
		Where = whereis(agent_sup),
		?assertNot(Where =:= undefined),
		exit(Where, kill),
		timer:sleep(5),
		restart(agent_sup, [[node()]]),
		Newwhere = whereis(agent_sup),
		?assertNot(Where =:= Newwhere),
		?assertNot(whereis(agent_connection_sup) =:= undefined),
		?assertNot(whereis(agent_manager) =:= undefined)
	end},
	{"Killing the routing branch (and bringing it back)",
	fun() ->
		Where = whereis(routing_sup),
		?assertNot(Where =:= undefined),
		exit(Where, kill),
		timer:sleep(5),
		restart(routing_sup, [[node()]]),
		Newwhere = whereis(routing_sup),
		?assertNot(Where =:= Newwhere),
		?assertNot(whereis(dispatch_manager) =:= undefined),
		?assertNot(whereis(queue_manager) =:= undefined)
	end}]}.

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
