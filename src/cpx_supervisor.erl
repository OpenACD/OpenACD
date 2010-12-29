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

%% @doc The top-most supervisor of the cpx system.  This is responsible for starting and monitoring the primary supervisors
%% as well as any additional modules that are configured.
%% There 4 branches off of cpx_supervisor:  routing_sup, 
%% mediamanager_sup, agent_sup, and management_sup.  They are started in
%% that order.
%% 
%% routing_sup starts the following process in the listed order (they
%% cannot be configured aside from applicatoin options):
%% cpxlog, cpx_monitor, dispatch_manager, queue_manager, and cdr.  Any
%% additions to this branch MUST NOT be dependant on other branches 
%% running and MUST be hard-coded in a specific order.
%% 
%% mediamanager_sup loads it's children from the cpx_conf table in mnesia.
%% Start order is undefined.  Any of those children MAY be dependant on
%% children in routing sup, and MUST NOT be dependant on children from
%% the other branches.
%%
%% agent_sup loads the agent_manager and starts the agent_connection_sup
%% supervisor.  This follows the same dependancies as mediamanager_sup.
%% Integration tools should go here.
%%
%% management_sup contains most other optional modules.  It MAY be
%% dependant on children in routing_sup, MUST NOT de dependant on
%% specific children from mediamanager_sup (ie, it should handle the
%% possibility those process are no longer configured to run).
%% 
%% Additional modules are loaded from the mnesia table 'cpx_conf'.  These modules would include 
%% those for agent authentication and media managers.  If it cannot build or access the 'cpx_conf' table
%% the supervisor does not start, thus halting all of cpx from starting.
%%
%% If the system starts without a cpx_conf table, it will build one, placing some default information there.
%% #cpx_conf{agent_tcp_listener} is the only one.
%% 
%% The 2 most important function are {@link destroy/1}, {@link update_conf/4}.

-module(cpx_supervisor).
-author("Micah").

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("cpx.hrl").
-include_lib("stdlib/include/qlc.hrl").

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
	-define(AGENTWEBPORT, 55050).
-else.
	-define(AGENTWEBPORT, 5050).
-endif.

-behaviour(supervisor).

-define(DEFAULT_CONF, [
	#cpx_conf{id = agent_web_listener, module_name = agent_web_listener, start_function = start_link, start_args = [?AGENTWEBPORT], supervisor = agent_connection_sup},
	#cpx_conf{id = cpx_web_management, module_name = cpx_web_management, start_function = start_link, start_args = [], supervisor = management_sup},
	#cpx_conf{id = gen_cdr_dumper, module_name = gen_cdr_dumper, start_function = start_link, start_args = [], supervisor = management_sup}
]).
	
%% API
-export([start_link/1, start/1]).
%% Conf handling
-export([
	build_spec/1,
	build_tables/0,
	default_conf/0,
	destroy/1,
	update_conf/2,
	get_conf/1,
	get_conf/0,
	stop/0,
	load_specs/1,
	start_spec/1,
	stop_spec/1,
	restart/2,
	restart_spec/1,
	get_archive_path/1,
	submit_bug_report/4,
	submit_bug_report/1
]).
%% General system settings
-export([
	set_value/2,
	get_value/1,
	drop_value/1
]).

%% Supervisor callbacks
-export([init/1]).

-ifndef(NOWEB).
-include("web.hrl").
-webconf([
	{label, "CPXCONF"},
	{file, "cpx_supervisor.html"},
	{callback, web_api}
]).
-endif.

-define(SUPERVISOR, true).
-include("gen_spec.hrl").

-type(supervisor_name() :: 'routing_sup' | 'agent_sup' | 'agent_connection_sup' | 'management_sup').

%% API functions
%% @doc Start the cpx_supervisor linked to the parent process.
-spec(start_link/1 :: (Nodes :: [atom()]) -> {'ok', pid()}).
start_link(Nodes) ->
	{ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
	
	Routingspec = {routing_sup, {cpx_middle_supervisor, start_named, [3, 5, routing_sup]}, temporary, 2000, supervisor, [?MODULE]},
	Managementspec = {management_sup, {cpx_middle_supervisor, start_named, [3, 5, management_sup]}, permanent, 2000, supervisor, [?MODULE]},
	Agentspec = {agent_sup, {cpx_middle_supervisor, start_named, [3, 5, agent_sup]}, temporary, 2000, supervisor, [?MODULE]},
	Mediamanagerspec = {mediamanager_sup, {cpx_middle_supervisor, start_named, [3, 5, mediamanager_sup]}, permanent, 2000, supervisor, [?MODULE]},
	Specs = [Routingspec, Agentspec, Managementspec, Mediamanagerspec],
	?DEBUG("specs:  ~p", [supervisor:check_childspecs(Specs)]),
	
	%load_specs(),
	%supervisor:start_child(management_sup, Cpxmonitorspec),

	supervisor:start_child(Pid, Routingspec),
	
	Cpxlogspec = {cpxlog, {cpxlog, start_link, []}, permanent, brutal_kill, worker, [?MODULE]},
	Cpxmonitorspec = {cpx_monitor, {cpx_monitor, start_link, [[{nodes, Nodes}, auto_restart_mnesia]]}, permanent, 2000, worker, [?MODULE]},
	DispatchSpec = {dispatch_manager, {dispatch_manager, start_link, []}, permanent, 2000, worker, [?MODULE]},
	QueueManagerSpec = {queue_manager, {queue_manager, start_link, [Nodes]}, permanent, 20000, worker, [?MODULE]},
	Cdrspec = {cdr, {cdr, start_link, []}, permanent, brutal_kill, worker, [?MODULE]},
	
	supervisor:start_child(routing_sup, Cpxlogspec),
	supervisor:start_child(routing_sup, Cpxmonitorspec),
	supervisor:start_child(routing_sup, DispatchSpec),
	supervisor:start_child(routing_sup, QueueManagerSpec),
	supervisor:start_child(routing_sup, Cdrspec),
	
	supervisor:start_child(Pid, Mediamanagerspec),
		
	supervisor:start_child(Pid, Agentspec),
	
	Agentconnspec = {agent_connection_sup, {cpx_middle_supervisor, start_named, [3, 5, agent_connection_sup]}, temporary, 2000, supervisor, [?MODULE]},
	AgentManagerSpec = {agent_manager, {agent_manager, start_link, [Nodes]}, permanent, 2000, worker, [?MODULE]},
	supervisor:start_child(agent_sup, AgentManagerSpec),
	supervisor:start_child(agent_sup, Agentconnspec),
	
	supervisor:start_child(Pid, Managementspec),
	
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
restart(routing_sup, Nodes) ->
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
restart(agent_sup, Nodes) ->
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

%% @doc Set a node data `Key' to `Value'.
-spec(set_value/2 :: (Key :: any(), Value :: any()) -> {'atomic', 'ok'}).
set_value(Key, Value) ->
	F = fun() ->
		mnesia:write(#cpx_value{key = Key, value = Value})
	end,
	{ok, Locked} = application:get_env('OpenACD', locked_env),
	case lists:member(Key, Locked) of
		true ->
			?WARNING("Env ~s only changed in database due to being in static config file.", [Key]),
			ok;
		false ->
			application:set_env('OpenACD', Key, Value)
	end,
	mnesia:transaction(F).

%% @doc Get the value for node data `Key'.  Note that this is specifically
%% for getting it from the database.  Usual code should just go got for
%% cpx:get_env/1, /2.
-spec(get_value/1 :: (Key :: any()) -> 'none' | {'ok', any()}).
get_value(Key) ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(cpx_value), X#cpx_value.key =:= Key]),
		qlc:e(QH)
	end,
	case mnesia:transaction(F) of
		{atomic, []} ->
			none;
		{atomic, [Rec]} ->
			{ok, Rec#cpx_value.value};
		Else ->
			?NOTICE("Error getting value for ~p:  ~p", [Key, Else]),
			none
	end.

%% @doc Drop the node data `Key'.
-spec(drop_value/1 :: (Key :: any()) -> {'atomic', 'ok'}).
drop_value(Key) ->
	F = fun() ->
		mnesia:delete({cpx_value, Key})
	end,
	{ok, Locked} = application:get_env('OpenACD', locked_env),
	case lists:member(Key, Locked) of
		true ->
			?WARNING("Setting ~s only removed from database since it also exists in static config.", [Key]),
			ok;
		false ->
			application:unset_env('OpenACD', Key)
	end,
	mnesia:transaction(F).
	
%%====================================================================
%% Supervisor callbacks
%%====================================================================
%% @private
init([]) ->
	?DEBUG("starting cpx_supervisor on ~p", [node()]),
	case build_tables() of
		ok ->
			{ok,{{one_for_one,3,5}, []}}
	end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec(add_conf/1 :: (Rec :: #cpx_conf{}) -> {'atomic', {'ok', pid()}} | {'aborted', any()}).
add_conf(Rec) ->
	F = fun() -> 
		mnesia:write(Rec),
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
			io:format("cpx_supervisor:build tables result:  ~p~n", [Result]),
			% create some default info so the system is at least a bit usable.
			F = fun() ->
				lists:foreach(fun(Rec) -> mnesia:write(Rec) end, ?DEFAULT_CONF)
			end,
			case mnesia:transaction(F) of
				{atomic, ok} -> 
					ok;
				Else -> 
					Else
			end;
		exists ->
			io:format("cpx_supervisor:build_tables, table exists~n"),
			ok;
		Else ->
			?NOTICE("unusual response building tables: ~p", [Else]),
			Else
	end,
	B = util:build_table(cpx_value, [
		{attributes, record_info(fields, cpx_value)},
		{disc_copies, [node()]},
		{local_content, true}
	]),
	case B of
		Result2 when Result2 =:= {atomic, ok}; Result2 =:= copied; Result2 =:= exists ->
			ok;
		Else2 ->
			?NOTICE("unusual response building cpx_value table:  ~p", [Else2])
	end.

%% @doc Adds the default configuration to the database.
-spec(default_conf/0 :: () -> {'atomic', 'ok'}).
default_conf() ->
	F = fun() ->
		lists:foreach(fun(Rec) -> add_conf(Rec) end, ?DEFAULT_CONF),
		ok
	end,
	mnesia:transaction(F).

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
-spec(update_conf/2 :: (Id :: atom(), Conf :: #cpx_conf{}) -> {'atomic', 'ok'} | {'aborted', any()}).
update_conf(Id, Conf) when is_record(Conf, cpx_conf) ->
	F = fun() ->
		case mnesia:read({cpx_conf, Id}) of
			[Rec] ->
				stop_spec(Rec),
				case start_spec(Conf) of
					{ok, Pid} = Out when is_pid(Pid) ->
						mnesia:delete({cpx_conf, Id}),
						mnesia:write(Conf#cpx_conf{timestamp = util:now()}),
						Out;
					Else ->
						?WARNING("Starting new spec got ~p, restarting previous configuration", [Else]),
						start_spec(Rec),
						erlang:error({start_fail, Else})
				end;
			[] ->
				case add_conf(Conf) of
					{atomic, {ok, Pid} = Out} when is_pid(Pid) ->
						Out;
					Else ->
						?WARNING("Adding new spec got ~p", [Else]),
						erlang:error({start_fail, Else})
				end
		end
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

%% @doc Pull all `#cpx_conf{}'s from the database
-spec(get_conf/0 :: () -> 'undefined' | [#cpx_conf{}]).
get_conf() ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(cpx_conf)]),
		qlc:e(QH)
	end,
	case mnesia:transaction(F) of
		{atomic, []} ->
			undefined;
		{atomic, Rec} ->
			Rec
	end.

%% @doc restart the conf
-spec(restart_spec/1 :: (Spec :: #cpx_conf{} | atom()) -> {'ok', pid()} | {'ok', pid(), any()} | {'error', any()}).
restart_spec(Spec) when is_atom(Spec) ->
	case get_conf(Spec) of
		undefined ->
			{error, noconf};
		Conf ->
			restart_spec(Conf)
	end;
restart_spec(Conf) when is_record(Conf, cpx_conf) ->
	cpx_middle_supervisor:drop_child(Conf#cpx_conf.supervisor, Conf#cpx_conf.id),
	cpx_middle_supervisor:add_with_middleman(Conf#cpx_conf.supervisor, 3, 5, Conf).

%% @private
-spec(start_spec/1 :: (Spec :: #cpx_conf{}) -> {'ok', pid()} | {'ok', pid(), any()} | {'error', any()}).
start_spec(Spec) when is_record(Spec, cpx_conf) ->
	?DEBUG("Starting ~p with supervisor ~p", [Spec#cpx_conf.id, Spec#cpx_conf.supervisor]),
	cpx_middle_supervisor:add_with_middleman(Spec#cpx_conf.supervisor, 3, 5, Spec).

-spec(stop_spec/1 :: (Spec :: #cpx_conf{}) -> 'ok').
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
			case length(Records) of
				0 ->
					?WARNING("No specs to load for ~w", [Super]),
					ok;
				_Else ->
					ok
			end,
%			Startthese = case Super of
%				management_sup ->
%					{ok, Nodes} = case application:get_env(cpx, nodes) of
%						undefined ->
%							{ok, [node()]};
%						Else ->
%							Else
%					end,
%					Monrec = #cpx_conf{
%						id = cpx_monitor,
%						module_name = cpx_monitor,
%						start_function = start_link,
%						start_args = [[{nodes, Nodes}, auto_restart_mnesia]],
%						supervisor = management_sup
%					},
%					[Monrec | Records];
%				_ ->
%					Records
%			end,
			lists:foreach(fun(I) -> start_spec(I) end, Records);
		Else ->
			?ERROR("unable to retrieve specs for ~s:  ~p", [Super, Else]),
			Else
	end.

%% @doc interpolate the system's archivepath with date and call info
-spec(get_archive_path/1 :: (Call :: #call{}) -> 'none' | {'error', any(), string()} | string()).
get_archive_path(Call) ->
	case get_value(archivepath) of
		{ok, Path} ->
			{Year, Month, Day} = date(),
			ExpandedPath = util:string_interpolate(Path,  [
					{"year", integer_to_list(Year)},
					{"month", string:right(integer_to_list(Month), 2, $0)},
					{"day", string:right(integer_to_list(Day), 2, $0)},
					{"callid", Call#call.id},
					{"calltype", atom_to_list(Call#call.type)},
					{"calldirection", atom_to_list(Call#call.direction)}
			]),
			
			case filelib:ensure_dir(ExpandedPath) of
				ok ->
					ExpandedPath;
				{error, Reason} ->
					{error, Reason, ExpandedPath}
			end;
		X ->
			?NOTICE("got ~p", [X]),
			none
	end.

%% @doc Try to submit a bug report to a (possibly) configred mantis.
-spec(submit_bug_report/4 :: (Summary :: binary(), Description :: binary(), Reproduce :: binary(), Other :: binary()) -> 'ok' | {'error', any()}).
submit_bug_report(Summary, Description, Reproduce, Other) when is_binary(Summary), is_binary(Description), is_binary(Reproduce), is_binary(Other) ->
	Options = [
		{<<"summary">>, Summary},
		{<<"description">>, Description},
		{<<"steps_to_reproduce">>, Reproduce},
		{<<"additional_information">>, Other}
	],
	submit_bug_report(Options).

-spec(submit_bug_report/1 :: (Options :: [{binary(), binary()}]) -> 'ok' | {'error', any()}).
submit_bug_report(Options) when is_list(Options) ->
	case lists:any(fun({Key, _Val}) when is_binary(Key) -> false; (_) -> true end, Options) of
		true ->
			{error, badarg};
		false ->
			case get_value(mantispath) of
				none ->
					{error, noconf};
				{ok, Path} ->
					Json = {struct, Options},
					case http:request(post, {Path, [], "application/x-www-form-urlencoded", lists:append(["report=", binary_to_list(list_to_binary(mochijson2:encode(Json)))])}, [{timeout, 4000}], []) of
						{ok, {{_Ver, 200, _Msg}, _Head, Body}} ->
							{struct, Props} = mochijson2:decode(Body),
							case proplists:get_value(<<"success">>, Props) of
								true ->
									Bugid = proplists:get_value(<<"issueid">>, Props),
									?NOTICE("bug report submitted:  ~p", [Bugid]),
									ok;
								_ ->
									Message = proplists:get_value(<<"message">>, Props),
									?NOTICE("Bug report attempt errored ~p", [Message]),
									{error, Message}
							end;
						Else ->
							?NOTICE("bug report attempt errored ~p", [Else]),
							Else
					end
			end
	end.

-ifdef(TEST).

config_test_() -> 
	%["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
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
					Valid = #cpx_conf{id = gen_server_mock, module_name = gen_server_mock, start_function = named, start_args = [{local, dummy_media_manager}], supervisor = management_sup},
					Out = update_conf(gen_server_mock, Valid),
					?CONSOLE("Out:  ~p", [Out]),
					QH = qlc:q([X || X <- mnesia:table(cpx_conf), X#cpx_conf.module_name =:= gen_server_mock]),
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
					Spec = #cpx_conf{id = gen_server_mock, module_name = gen_server_mock, start_function = named, start_args = [{local, dummy_media_manager}], supervisor = management_sup},
					update_conf(gen_server_mock, Spec),
					?assert(is_pid(whereis(dummy_media_manager))),
					destroy(Spec),
					?assertEqual(undefined, whereis(dummy_media_manager)),
					QH = qlc:q([X || X <- mnesia:table(cpx_conf), X#cpx_conf.module_name =:= gen_server_mock]),
					F = fun() -> 
						qlc:e(QH)
					end,
					?assertMatch({atomic, []}, mnesia:transaction(F))
				end
			},
			{
				"Destroy a Config by id only",
				fun() -> 
					Spec = #cpx_conf{id = gen_server_mock, module_name = gen_server_mock, start_function = named, start_args = [{local, dummy_media_manager}], supervisor = management_sup},
					update_conf(gen_server_mock, Spec),
					?assert(is_pid(whereis(dummy_media_manager))),
					destroy(gen_server_mock),
					?CONSOLE("~p", [whereis(dummy_media_manager)]),
					?assertEqual(undefined, whereis(dummy_media_manager)),
					QH = qlc:q([X || X <- mnesia:table(cpx_conf), X#cpx_conf.module_name =:= gen_server_mock]),
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
					Oldrec = #cpx_conf{
						id = gen_server_mock,
						module_name = gen_server_mock,
						start_function = named,
						start_args = [{local, dummy_media_manager}],
						supervisor = management_sup
					},
					update_conf(gen_server_mock, Oldrec),
					Oldpid = whereis(dummy_media_manager),
					Newrec = #cpx_conf{
						id=gen_server_mock,
						module_name = gen_server_mock,
						start_function = named,
						start_args = [{local, gen_server_mock}],
						supervisor = management_sup
					},
					Out = update_conf(gen_server_mock, Newrec),
					?assertMatch({atomic, {ok, _}}, Out),
					Newpid = whereis(gen_server_mock),
					?assertNot(Oldpid =:= Newpid),
					?assert(is_pid(Newpid)),
					QH = qlc:q([X || X <- mnesia:table(cpx_conf), X#cpx_conf.module_name =:= gen_server_mock]),
					F = fun() ->
						qlc:e(QH)
					end,
					{atomic, [Rec]} = mnesia:transaction(F),
					?assertEqual([{local, gen_server_mock}], Rec#cpx_conf.start_args),
					destroy(gen_server_mock)
				end
			},
			{
				"Updating a config fails, so the old config is started",
				fun() ->
					Oldrec = #cpx_conf{
						id = gen_server_mock,
						module_name = gen_server_mock,
						start_function = named,
						start_args = [{local, dummy_media_manager}],
						supervisor = management_sup
					},
					update_conf(gen_server_mock, Oldrec),
					Oldpid = whereis(dummy_media_manager),
					Newrec = #cpx_conf{
						id=gen_server_mock,
						module_name = fake_module,
						start_function = bad_start,
						start_args = [],
						supervisor = management_sup
					},
					Out = update_conf(gen_server_mock, Newrec),
					?assertMatch({aborted, {{start_fail, _}, _}}, Out),
					Newpid = whereis(dummy_media_manager),
					?assertNot(Oldpid =:= Newpid),
					?DEBUG("new pid:  ~p", [Newpid]),
					?assert(is_pid(Newpid)),
					QH = qlc:q([X || X <- mnesia:table(cpx_conf), X#cpx_conf.module_name =:= gen_server_mock]),
					F = fun() ->
						qlc:e(QH)
					end,
					{atomic, [Rec]} = mnesia:transaction(F),
					?assertEqual([{local, dummy_media_manager}], Rec#cpx_conf.start_args),
					destroy(gen_server_mock)
				end
			},
			{
				"Updating a config adds when there is no old config",
				fun() ->
					undefined = whereis(dummy_media_manager),
					Rec = #cpx_conf{
						id = gen_server_mock,
						module_name = gen_server_mock,
						start_function = named,
						start_args = [{local, dummy_media_manager}],
						supervisor = management_sup
					},
					Out = update_conf(gen_server_mock, Rec),
					?assertMatch({atomic, {ok, _}}, Out),
					QH = qlc:q([X || X <- mnesia:table(cpx_conf), X#cpx_conf.module_name =:= gen_server_mock]),
					F = fun() ->
						qlc:e(QH)
					end,
					{atomic, [Rec]} = mnesia:transaction(F),
					?assertEqual([{local, dummy_media_manager}], Rec#cpx_conf.start_args),
					?assert(is_pid(whereis(dummy_media_manager))),
					destroy(gen_server_mock)
				end
			},
			{
				"Build a Spec from Record",
				fun() -> 
					Record = #cpx_conf{id = dummy_id, module_name = dummy_mod, start_function = start, start_args = []},
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
		?assertNot(Where =:= Newwhere)
	end},
	{"Killing the agent branch (and bringing it back)",
	fun() ->
		Where = whereis(agent_sup),
		?assertNot(Where =:= undefined),
		exit(Where, kill),
		timer:sleep(5),
		restart(agent_sup, [node()]),
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
		restart(routing_sup, [node()]),
		Newwhere = whereis(routing_sup),
		?assertNot(Where =:= Newwhere),
		?assertNot(whereis(dispatch_manager) =:= undefined),
		?assertNot(whereis(queue_manager) =:= undefined)
	end},
	{"Killing the media manager's branch (and bringin it back)",
	fun() ->
		Where = whereis(mediamanager_sup),
		?assertNot(Where == undefined),
		exit(Where, kill),
		timer:sleep(5),
		restart(mediamanager_sup, []),
		Newwhere = whereis(mediamanager_sup),
		?assertNot(Where == Newwhere)
	end}]}.

% TODO to be re-enabled with either rebar can have eunit run on a node or
% can be re-written to not require a live node.
mutlinode_test_d() ->
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
