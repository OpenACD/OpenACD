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

%% @doc The application module, as well as primary shell interface module.
-module(cpx).
-author("Micah").

-behaviour(application).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("queue.hrl").
-include("cpx.hrl").
-include("commit_ver.hrl").

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("stdlib/include/qlc.hrl").

-ifdef(CPXHELP).
	-cpxhelp(["head data",
		{chapter1, "chapter1"},
		{chapter2, "chapter2"}
	]).
-endif.

-ifndef(OPENACD_COMMIT).
	-define(OPENACD_COMMIT, undefined).
-endif.

% behavior cbs.
-export([start/2, prep_stop/1, stop/1]).
% some get_env and get_key nicities
-export([
	get_env/1,
	get_env/2,
	get_key/1,
	get_key/2
]).

% helper funcs
-export([
	agent_state/1,
	agent_states/0,
	agent_states/1,
	backup_config/0,
	backup_config/1,
	restore_config/1,
	restore_config/2,
	reload_plugins/0,
	reload_plugin/1,
	unload_plugin/1,
	load_plugin/1,
	plugins_running/0,
	plugin_status/1,
	set_plugin_env/2,
	load_plugin_env/1,
	load_plugin_envs/1,
	save_plugin_env/2,
	call_state/1,	
	get_queue/1,
	get_agent/1,
	get_agents/0,
	get_agents/1,
	get_queues/0,
	get_queues/1,
	get_queue_status/0,
	get_agent_status/0,
	status/0,
	get_media/1,
	get_commit/0,
	kick_agent/1,
	kick_call/1,
	kick_media/1,
	is_running/0,
	is_running/1,
	help/0,
	help/1,
	help/2,
	media_state/1,
	uptime/0,
	uptime/1,
	reload_recipe/1,
	in_progress/0,
	print_raws/1,
	find_cdr/1,
	print_cdr/1
]).

-spec(start/2 :: (Type :: 'normal' | {'takeover', atom()} | {'failover', atom()}, StartArgs :: [any()]) -> {'ok', pid(), any()} | {'ok', pid()} | {'error', any()}).
start(_Type, StartArgs) ->
	io:format("Start args ~p~n", [StartArgs]),
	io:format("All env: ~p~n", [application:get_all_env('OpenACD')]),
	crypto:start(),
	%Nodes = lists:append([nodes(), [node()]]),
	%mnesia:create_schema(Nodes),
	%mnesia:start(),
	case application:get_env('OpenACD', nodes) of
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
			Nodes = [node()],
			application:set_env('OpenACD', nodes, Nodes)
	end,
	mnesia:change_table_copy_type(schema, node(), disc_copies),
	mnesia:set_master_nodes(lists:umerge(Nodes, [node()])),
	merge_env(),

	add_plugin_paths(),
	try cpx_supervisor:start_link(Nodes) of
		{ok, Pid} ->
			application:set_env('OpenACD', uptime, util:now()),
			?NOTICE("Application OpenACD started sucessfully!", []),
			% to not block the shell.
			spawn(fun() ->
				case cpx:get_env(plugin_dir) of
					undefined ->
						?INFO("No plugins to load, no plugin dir", []);
					{ok, PluginDir} ->
						case filelib:ensure_dir(filename:join(PluginDir, "touch")) of
							ok ->
								start_plugins(PluginDir);
							{error, Error} ->
								?ERROR("Could not ensure plugin directory ~s exists:  ~p", [PluginDir, Error])
						end
				end
			end),
			{ok, Pid}
	catch
		What:Why ->
			?ERROR("Application OpenACD failed to start successfully! ~p:~p", [What, Why]),
			{What, Why}
	end.

-spec(prep_stop/1 :: (State :: any()) -> any()).
prep_stop(State) ->
	?NOTICE("Application OpenACD stopping...", []),
	State.

-spec(stop/1 :: (State :: any()) -> 'ok').
stop(_State) ->
	application:unset_env('OpenACD', uptime),
	ok.

%% ====
%% better getter/setters
%% ====

%% @doc The erlang application:get_env and get_key functions don't allow for
%% a default value.  These do.
-spec(get_env/1 :: (Key :: any()) -> {'ok', any()} | 'undefined').
get_env(Key) ->
	application:get_env('OpenACD', Key).

%% @doc @see gen_env/1
-spec(get_env/2 :: (Key :: any(), Default :: any()) -> {'ok', any()}).
get_env(Key, Default) ->
	case application:get_env('OpenACD', Key) of
		undefined ->
			{ok, Default};
		Out ->
			Out
	end.

-spec(get_key/1 :: (Key :: any()) -> {'ok', any()} | 'undefined').
get_key(Key) ->
	application:get_key('OpenACD', Key).

-spec(get_key/2 :: (Key :: any(), Default :: any()) -> {'ok', any()}).
get_key(Key, Default) ->
	case application:get_key('OpenACD', Key) of
		undefined ->
			{ok, Default};
		Out ->
			Out
	end.

%% =====
%% helper funcs
%% =====

%% @doc Takes the env gotten from the config file and merges it with
%% the vars found int he database. The env file wins in case of both being
%% defined.
-spec(merge_env/0 :: () -> 'ok').
merge_env() ->
	Fun = fun() ->
		qlc:e(qlc:q([{Key, Value} || #cpx_value{key = Key, value = Value} = _X <- mnesia:table(cpx_value)]))
	end,
	case mnesia:transaction(Fun) of
		{aborted, {no_exists, {cpx_value, index}}} ->
			application:set_env('OpenACD', locked_env, [uptime]),
			ok;
		{aborted, {no_exists, cpx_value}} ->
			application:set_env('OpenACD', locked_env, [uptime]),
			ok;
		{atomic, Cpxdb} ->
			Locked = [uptime | [Key || {Key, _} <- application:get_all_env('OpenACD')]],
			application:set_env('OpenACD', locked_env, Locked),
			merge_env(Cpxdb, Locked)
	end.

merge_env([], _LockedKeys) ->
	ok;
merge_env([{Key, Val} | T], Locked) ->
	case application:get_env('OpenACD', Key) of
		undefined ->
			application:set_env('OpenACD', Key, Val),
			merge_env(T, Locked);
		_ ->
			merge_env(T, Locked)
	end.

%% @doc Backups the mnesia database for use with the restore functions.  Does a
%% full backup, so the mnesia restore funtions could be used directly to restore
%% even the cdr and agent state tables.
-spec(backup_config/0 :: () -> 'ok').
backup_config() ->
	backup_config(lists:flatten(io_lib:format("cpx-~B", [util:now()]))).

%% @doc Same as above, but you give the file to store the backup in.
-spec(backup_config/1 :: (Filename :: string()) -> {'ok', string()} | {'error', any()}).
backup_config(Filename) ->
	case mnesia:backup(Filename) of
		ok ->
			{ok, Filename};
		Else ->
			{error, Else}
	end.

%% @doc Restore all the config files from the given back up file.
-spec(restore_config/1 :: (Filename :: string()) -> {'atomic', [atom()]} | {'aborted', any()}).
restore_config(Filename) ->
	mnesia:restore(Filename, [{skip_tables, [agent_state, cdr_raw, cdr_rec]}]).

%% @doc Restore only the passed in table names from the given back up file.
-spec(restore_config/2 :: (Filename :: string(), Tables :: [atom()] | atom()) -> {'atomic', [atom()]} | {'aborted', any()}).
restore_config(Filename, Table) when is_atom(Table) ->
	restore_config(Filename, [Table]);
restore_config(Filename, Tables) ->
	mnesia:restore(Filename, [
		{clear_tables, Tables},
		{default_op, skip_tables}
	]).

%% @doc Return which plugins are running and which are stopped.
plugins_running() ->
	{ok, ConfigedPlugins} = cpx:get_env(plugins, []),
	Running = [N ||
		{N, _, _} <- application:which_applications(),
		lists:member(N, ConfigedPlugins)
	],
	[case lists:member(P, Running) of
		true -> {P, running};
		false -> {P, stopped}
	end || P <- ConfigedPlugins].

%% @doc Get the running status of a specific plugin.
plugin_status(Plugin) ->
	Status = plugins_running(),
	proplists:get_value(Plugin, Status).

%% @doc Reload the code for all plugins using {@link util:reload/0}.
reload_plugins() ->
	{ok, Plugins} = cpx:get_env(plugins, []),
	[reload_plugin(P) || P <- Plugins].

%% @doc Reload the code the plugin and applications plugin depends on using
%% {@link util:reload/0}.
reload_plugin(Plugin) ->
	{ok, Keys} = application:get_all_key(Plugin),
	Apps = proplists:get_value(applications, Keys),
	[reload_plugin(P) || P <- Apps],
	Modules = proplists:get_value(modules, Keys),
	[util:reload(M) || M <- Modules].

%% @doc Stop the plugin (but not always what the plugin depends on).
unload_plugin(Plugin) ->
	{ok, Plugins} = cpx:get_env(plugins, []),
	{ok, PluginDir} = cpx:get_env(plugin_dir, "plugins.d"),
	case lists:member(Plugin, Plugins) of
		false -> ok;
		true ->
			application:stop(Plugin),
			NewPlugins = lists:delete(Plugin, Plugins),
			Appfile = atom_to_list(Plugin) ++ ".app",
			file:delete(filename:join(PluginDir, Appfile)),
			application:set_env('OpenACD', plugins, NewPlugins),
			ok
	end.

%% @doc Add the plugin to the path and start it.
load_plugin(Plugin) ->
	{ok, PluginDir} = cpx:get_env(plugin_dir, "plugins.d"),
	case add_plugin_paths(PluginDir) of
		{error, badarg} ->
			{error, badarg};
		ok ->
			case code:where_is_file(atom_to_list(Plugin) ++ ".app") of
				non_existing ->
					{error, appfile_noexist};
				Appfile ->
					case file:make_symlink(Appfile, filename:join(PluginDir, atom_to_list(Plugin) ++ ".app")) of
						ok ->
							{ok, Plugins} = cpx:get_env(plugins, []),
							application:set_env('OpenACD', plugins, lists:usort([Plugin | Plugins])),
							start_plugin_app(Plugin);
						Else ->
							?INFO("Could not make link:  ~p", [Else]),
							{error, Else}
					end
			end
	end.

%% @doc Sets the application variables for a plugin.
-spec set_plugin_env(PluginName :: atom(), Env :: [{atom(),any()}]) -> 'ok'.
set_plugin_env(_PluginName, []) ->
	ok;
set_plugin_env(PluginName, [{Key,Val}|Tail]) ->
	application:set_env(PluginName, Key, Val),
	set_plugin_env(PluginName,Tail);
set_plugin_env(PluginName, [Key | Tail]) when is_atom(Key) ->
	application:set_env(PluginName, Key, true),
	set_plugin_env(PluginName,Tail).

-spec(get_queue/1 :: (Queue :: string()) -> pid() | 'none').
get_queue(Queue) ->
	queue_manager:get_queue(Queue).

-spec(get_agent/1 :: (Agent :: string()) -> pid() | 'none').
get_agent(Agent) when is_list(Agent) ->
	case agent_manager:query_agent(Agent) of
		{true, Pid} ->
			Pid;
		false ->
			none
	end;
get_agent(_) ->
	io:format("Give a name~n"),
	none.

-spec(get_agents/0 :: () -> [{string(), pid()}]).
get_agents() ->
	agent_manager:list().

-spec(get_agents/1 :: (Profile :: string()) -> [{string(), pid()}]).
get_agents(Profile) ->
	Agents = agent_manager:list(),
	Fun = fun({_Login, Pid}) ->
		case agent:dump_state(Pid) of
			#agent{profile = Profile} ->
				true;
			_ ->
				false
		end
	end,
	lists:filter(Fun, Agents).

-spec(get_queues/0 :: () -> [{string(), pid()}]).
get_queues() ->
	queue_manager:queues().

-spec(get_queue_status/0 :: () -> 'ok').
get_queue_status() ->
	[io:format("Queue: ~s; ~B calls~n", [Name, call_queue:call_count(Pid)]) || {Name, Pid} <- queue_manager:queues()],
	ok.

-spec(get_agent_status/0 :: () -> 'ok').
get_agent_status() ->
	Agents = qlc:e(qlc:q([X || {{agent, _}, _, _, _, _, _} = X <- ets:table(cpx_monitor)])),
	{I, O, W, R} = lists:foldl(fun({_, Info, _, _, _, _}, {Idle, Oncall, Wrapup, Released}) ->
				case proplists:get_value(state, Info) of
					X when X == ringing; X == idle ->
					{Idle + 1, Oncall, Wrapup, Released};
				X when X == oncall; X == precall; X == warmtransfer; X == outgoing ->
					{Idle, Oncall + 1, Wrapup, Released};
				wrapup ->
					{Idle, Oncall, Wrapup + 1, Released};
				released ->
					{Idle, Oncall, Wrapup, Released + 1};
				_ ->
					{Idle, Oncall, Wrapup, Released}
			end
	end,
	{0, 0, 0, 0}, Agents),
	io:format("~B Agents; ~B Idle, ~B Oncall, ~B Wrapup, ~B Released~n", [length(Agents), I, O, W, R]).

-spec(status/0 :: () -> 'ok').
status() ->
	io:format("uptime:  "),
	uptime(),
	io:format("\nCore Modules running:\n"),
	is_running(),
	io:format("\nPlugins Enabled and running:\n"),
	Plugins = plugins_running(),
	pretty_print(Plugins),
	io:format("\nAgents:  "),
	get_agent_status(),
	io:format("\nQueues:\n"),
	get_queue_status().

-spec(get_queues/1 :: (Group :: string()) -> [{string(), pid()}]).
get_queues(Group) ->
	Queues = get_queues(),
	StashedQueues = call_queue_config:get_queues(Group),
	Compare = lists:map(fun(#call_queue{name = Nom}) -> Nom end, StashedQueues),
	Fun = fun({Name, _Pid}) ->
		lists:member(Name, Compare)
	end,
	lists:filter(Fun, Queues).

-spec(get_media/1 :: (IsPid :: string() | pid()) -> pid() | 'none').
get_media(Pid) when is_pid(Pid) ->
	%% There's prolly something else I was supposed to do...
	Pid;
get_media(LPid) ->
	try list_to_pid(LPid) of
		Pid ->
			get_media(Pid)
	catch
		error:badarg ->
			%% okay, let's ask the media managers.
			Confs = cpx_supervisor:get_conf(),
			get_medias_managers(Confs, LPid)
	end.

get_medias_managers([], _Needle) ->
	none;
get_medias_managers([#cpx_conf{supervisor = mediamanager_sup, module_name = Mod} | Tail], Needle) ->
	case Mod:get_media(Needle) of
		none ->
			get_medias_managers(Tail, Needle);
		{Needle, Pid} ->
			Pid
	end;
get_medias_managers([_Head | Tail], Needle) ->
	get_medias_managers(Tail, Needle).

%get_media_queues([], _Callref) ->
%	none;
%get_media_queues([{Qnom, Queue} | Tail], Callref) ->
%	Calls = call_queue:to_list(Queue),
%	case get_media_queues_medias(Calls, Callref) of
%		none ->
%			get_media_queues(Tail, Callref);
%		Pid ->
%			Pid
%	end.
%
%get_media_queues_medias([], _Callref) ->
%	none;
%get_media_queues_medias([QueuedCall | Tail],  Callref) ->
%	case QueuedCall#queued_call.id of
%		Callref ->
%			QueuedCall#queued_call.media;
%		_ ->
%			get_media_queues_medias(Tail, Callref)
%	end.
%
%get_media_agents([], _Callref) ->
%	none;
%get_media_agents([{_Login, Pid} | Tail], Callref) ->
%	#agent{statedata = State} = agent:dump_state(Pid),
%	case State of
%		#call{id = Callref} ->
%			State#call.source;
%		_ ->
%			get_media_agents(Tail, Callref)
%	end.

-spec(get_commit/0 :: () -> 'undefined' | string()).
get_commit() ->
	?OPENACD_COMMIT.

-spec(kick_agent/1 :: (AgentRef :: string() | pid()) -> 'ok' | 'none').
kick_agent(Pid) when is_pid(Pid) ->
	exit(Pid, kill),
	ok;
kick_agent(List) ->
	try list_to_pid(List) of
		Pid ->
			kick_agent(Pid)
	catch
		error:badarg ->
			case get_agent(List) of
				none ->
					none;
				Pid ->
					kick_agent(Pid)
			end
	end.

-spec(kick_call/1 :: (Callref :: string() | pid()) -> 'ok' | 'none').
kick_call(Pid) when is_pid(Pid) ->
	exit(Pid, kill),
	ok;
kick_call(Callref) ->
	try list_to_pid(Callref) of
		Pid ->
			kick_call(Pid)
	catch
		error:badarg ->
			case get_media(Callref) of
				none ->
					none;
				Pid ->
					kick_call(Pid)
			end
	end.

-spec(kick_media/1 :: (Mediaref :: string() | pid()) -> 'ok' | 'none').
kick_media(Mediaref) ->
	kick_call(Mediaref).

-spec(is_running/0 :: () -> 'ok').
is_running() ->
	case cpx_supervisor:get_conf() of
		undefined ->
			io:format("No modules configured.~n");
		List ->
			Outmap = lists:map(fun(#cpx_conf{id = Modid}) -> Status = is_running(Modid), {Modid, Status} end, List),
			pretty_print(Outmap)
	end,
	ok.

-spec(is_running/1 :: (Specid :: atom) -> pid() | 'stopped' | 'noexists').
is_running(Specid) ->
	case cpx_supervisor:get_conf(Specid) of
		undefined ->
			noexists;
		#cpx_conf{supervisor = Super} ->
			case supervisor:which_children(Super) of
				[] ->
					stopped;
				Kids ->
					is_running(Kids, Specid)
			end
	end.

is_running([], _Specid) ->
	stopped;
is_running([Head | _Tail], Specid) when element(1, Head) == Specid ->
	case element(2, Head) of
		undefined ->
			stopped;
		Pid ->
			Pid
	end;
is_running([_ | Tail], Specid) ->
	is_running(Tail, Specid).

-spec(help/0 :: () -> 'ok').
help() ->
	{ok, Bin} = file:read_file("include/cpxhelp.txt"),
	io:format("~s", [Bin]),
	ok.

-spec(help/1 :: (Module :: atom()) -> 'ok').
help(Module) ->
	case check_module(Module) of
		false ->
			ok;
		true ->
			Attribs = Module:module_info(attributes),
			case proplists:get_value(cpxhelp, Attribs) of
				undefined ->
					io:format("There is no help information available for ~p~n~n", [Module]),
					ok;
				[Head | _Tail] ->
					io:format("~p", [Head]),
					ok
			end,
			Exports = Module:module_info(exports),
			pretty_print(Exports),
			ok
	end.

-spec(help/2 :: (Module :: atom(), Chapter :: any()) -> 'ok').
help(Module, exports) ->
	case check_module(Module) of
		false ->
			ok;
		true ->
			pretty_print(lists:sort(Module:module_info(exports))),
			ok
	end;
help(Module, Chapter) ->
	case check_module(Module) of
		false ->
			ok;
		true ->
			Attribs = Module:module_info(attributes),
			case proplists:get_value(cpxhelp, Attribs) of
				undefined ->
					io:format("There is no help information available for ~p", [Module]),
					ok;
				[_Head | Chapters] ->
					case proplists:get_value(Chapter, Chapters) of
						undefined ->
							io:format("There is no help information available for ~p:~p", [Module, Chapter]);
						Data ->
							io:format("~p", [Data]),
							ok
					end
			end
	end.

check_module(Module) ->
	case erlang:function_exported(Module, module_info, 1) of
		false ->
			io:format("There's no such module ~p", [Module]),
			false;
		true ->
			true
	end.

-spec(agent_states/0 :: () -> [{string(), string(), atom()}]).
agent_states() ->
	agent_states(any).

-spec(agent_states/1 :: (Profiles :: 'any' | string() | [string()]) -> [{string(), string(), atom()}]).
agent_states(RawProfiles) ->
	% TODO misnomered until it accounts for channels.
	Agentlist = agent_manager:list(),
	Profiles = case RawProfiles of
		any ->
			any;
		[H | _] when is_list(H) ->
			RawProfiles;
		_ ->
			[RawProfiles]
	end,
	Fold = fun({Login, {Pid, Id}}, Acc) ->
		Astate = agent:dump_state(Pid),
		case in_list(Astate#agent.profile, Profiles) of
			true ->
				[{Login, Id} | Acc];
			false ->
				Acc
		end
	end,
	lists:foldl(Fold, [], Agentlist).
	
in_list(_Value, any) ->
	true;
in_list(Value, List) when is_list(List) ->
	lists:member(Value, List).

-spec(agent_state/1 :: (Agent :: any()) -> 'ok').
agent_state(Agent) ->
	case get_agent(Agent) of
		none ->
			io:format("No such agent"),
			ok;
		Pid ->
			State = agent:dump_state(Pid),
			Print = [
				{"Id", State#agent.id}, 
				{"Pid", Pid},
				{"Profile", State#agent.profile},
				{"Release", case State#agent.release_data of
					default ->
						"default";
					{Name, _, _} ->
						Name;
					_ ->
						false
				end}
			],
			pretty_print(Print),
			ok
	end.

pretty_print(List) ->
	FindLongest = fun({K, _}, Length) ->
		ListK = if 
			is_atom(K) ->
				atom_to_list(K);
			is_binary(K) ->
				binary_to_list(K);
			true -> 
				K 
		end,
		case length(ListK) > Length of
			true ->
				length(ListK);
			false ->
				Length
		end
	end,
	Longest = lists:foldl(FindLongest, 0, List) + 1,
	{Io, Params} = build_io_and_params(List, Longest, "", []),
	io:format(Io, Params).

build_io_and_params([], _, Io, Params) ->
	{Io, lists:reverse(Params)};
build_io_and_params([{K, Value} | Tail], Length, Io, Params) ->
	ListK = if 
		is_atom(K) ->
			atom_to_list(K);
		is_binary(K) ->
			binary_to_list(K);
		true -> 
			K 
	end,
	Newio = lists:append([Io, string:right(ListK, Length), ":  ~p~n"]),
	Newparams = [Value | Params],
	build_io_and_params(Tail, Length, Newio, Newparams).

-spec(call_state/1 :: (Media :: any()) -> 'ok').
call_state(Media) ->
	media_state(Media).

-spec(media_state/1 :: (Media :: any()) -> 'ok').
media_state(Media) ->
	case get_media(Media) of
		none ->
			io:format("No such media"),
			ok;
		Pid ->
			Call = gen_media:get_call(Pid),
			io:format("State of call ~p~n", [Media]),
			Print = [
				{"Id", Call#call.id},
				{"Source", Call#call.source},
				{"Type", Call#call.type},
				{"Callerid", Call#call.callerid},
				{"Client", case Call#call.client of
					undefined ->
						"Not yet set";
					Client ->
						Client#client.label
				end},
				{"Cook", Call#call.cook},
				{"Bound Dispatchers", Call#call.bound},
				{"Direction", Call#call.direction},
				{"Priority", Call#call.priority},
				{"Ring Path", Call#call.ring_path},
				{"Media Path", Call#call.media_path}
			],
			pretty_print(Print),
			ok
	end.

-spec(uptime/0 :: () -> non_neg_integer() | 'stopped').
uptime() ->
	uptime(false).

-spec(uptime/1 :: (Fallback :: boolean()) -> non_neg_integer() | 'stopped').
uptime(Fallback) ->
	Apps = application:which_applications(),
	Fun = fun({'OpenACD', _, _}) -> true; (_) -> false end,
	Running = lists:any(Fun, Apps),
	case Running of
		false -> 
			stopped;
		true ->
			case {application:get_env('OpenACD', uptime), Fallback} of
				{undefined, false} ->
					io:format("The uptime is not available for this node.~nYou can call cpx:uptime(true) to set the uptime to now~n"),
					0;
				{undefined, true} ->
					io:format("The uptime was not available, so resetting it as requested~n"),
					Now = util:now(),
					application:set_env('OpenACD', uptime, Now),
					0;
				{{ok, Time}, _} ->
					Out = util:now() - Time,
					io:format("~s~n", [pretty_print_time(Out)]),
					Out
			end
	end.

pretty_print_time(Time) ->
	Times = [
		{"Seconds", 60},
		{"Minutes", 60},
		{"Hours", 24},
		{"Days", 7},
		{"Weeks", 52},
		"Years"
	],
	pretty_print_time(Times, Time, "").

pretty_print_time(_, 0, Acc) ->
	Acc;
pretty_print_time([Label], Time, Acc) ->
	lists:append([integer_to_list(Time), " ", Label, " ", Acc]);
pretty_print_time([{Label, Interval} | Tail], Time, Acc) ->
	Rem = Time rem Interval,
	Newtime = util:floor(Time / Interval),
	Newacc = lists:append([integer_to_list(Rem), " ", Label, " ", Acc]),
	pretty_print_time(Tail, Newtime, Newacc).

-spec(reload_recipe/1 :: (Queue :: string()) -> 'ok').
reload_recipe(Queue) ->
	#call_queue{group = Group, recipe = Recipe} = call_queue_config:get_queue(Queue),
	{atomic, [#queue_group{recipe = Grep}]} = call_queue_config:get_queue_group(Group),
	Newrec = lists:append(Grep, Recipe),
	Q = queue_manager:get_queue(Queue),
	call_queue:set_recipe(Q, Newrec).

-spec(in_progress/0 :: () -> [#cdr_rec{}]).
in_progress() ->
	{atomic, List} = mnesia:transaction(fun() -> qlc:e(qlc:q([begin M = R#cdr_rec.media, M#call.id end || #cdr_rec{summary = inprogress} = R <- mnesia:table(cdr_rec)])) end),
	List.

-spec(print_raws/1 :: (Cdr :: string() | #cdr_rec{}) -> 'ok').
print_raws(#cdr_rec{transactions = inprogress, media = Media}) ->
	Id = Media#call.id,
	print_raws(Id);
print_raws(#cdr_rec{media = Call, transactions = Trans}) ->
	io:format("Id:  ~s~n", [Call#call.id]),
	pretty_print_raws(Trans);
print_raws(Id) ->
	Recres = mnesia:transaction(fun() -> 
		qlc:e(qlc:q([X || 
			#cdr_rec{media = Call, transactions = Trans} = X <- mnesia:table(cdr_rec),
			Trans =/= inprogress,
			Call#call.id =:= Id
		]))
	end),
	case Recres of
		{atomic, []} ->
			{atomic, List} = mnesia:transaction(fun() -> qlc:e(qlc:q([X || #cdr_raw{id = Testid} = X <- mnesia:table(cdr_raw), Testid =:= Id])) end),
			io:format("Id:  ~s~n", [Id]),
			pretty_print_raws(List);
		{atomic, [Rec]} ->
			print_raws(Rec)
	end.

pretty_print_raws(UnsortedList) ->
	Sort = fun
		(#cdr_raw{transaction = cdrinit}, #cdr_raw{transaction = _Btran}) ->
			true;
		(#cdr_raw{transaction = _Tran}, #cdr_raw{transaction = cdrinit}) ->
			false;
		(#cdr_raw{start = Astart}, #cdr_raw{start = Bstart}) ->
			Astart =< Bstart
	end,
	List = lists:sort(Sort, UnsortedList),
	F = fun(#cdr_raw{transaction = Trans, eventdata = Edata, start = Start, ended = End}) ->
		Fixedend = case Edata of
			#call{client = Client} ->
				Client#client.label;
			_ ->
				Edata
		end,
		Duration = case End of
			undefined ->
				undefined;
			_ ->
				End - Start
		end,
		io:format("~s\t~p - ~p\t~p\t~p~n", [Trans, Start, End, Duration, Fixedend])
	end,
	[F(X) || X <- List],
	ok.

pretty_print_summary(Summary) ->
	pretty_print_summary_sections(Summary).

pretty_print_summary_sections([]) ->
	ok;
pretty_print_summary_sections([{Name, {Total, Details}} | Tail]) ->
	io:format("\t~s:  ~p~n", [Name, Total]),
	pretty_print_summary_details(Details),
	pretty_print_summary_sections(Tail).

pretty_print_summary_details([]) ->
	ok;
pretty_print_summary_details([{Key, Total} | Tail]) ->
	io:format("\t\t~s:  ~p~n", [Key, Total]),
	pretty_print_summary_details(Tail).

-spec(print_cdr/1 :: (Cdr :: #cdr_rec{}) -> 'ok').
print_cdr(#cdr_rec{media = M, summary = inprogress, transactions = inprogress}) ->
	Id = M#call.id,
	io:format("Id:  ~s~n", [Id]),
	io:format("No Summary~n", []),
	{atomic, List} = mnesia:transaction(fun() -> qlc:e(qlc:q([X || #cdr_raw{id = Testid} = X <- mnesia:table(cdr_raw), Testid =:= Id])) end),
	pretty_print_raws(List),
	io:format("~n"),
	ok;
print_cdr(#cdr_rec{media = M, summary = S, transactions = T}) when is_list(S), is_list(T) ->
	Id = M#call.id,
	io:format("Id:  ~s~n", [Id]),
	io:format("Summary:~n"),
	pretty_print_summary(S),
	io:format("~nRaws:~n"),
	pretty_print_raws(T),
	io:format("~n"),
	ok;
print_cdr(Id) ->
	Res = mnesia:transaction(fun() -> qlc:e(qlc:q([X || #cdr_rec{media = M} = X <- mnesia:table(cdr_rec), M#call.id =:= Id])) end),
	case Res of
		{atomic, []} ->
			io:format("~s has no cdr_rec~n", [Id]),
			ok;
		{atomic, [Rec]} ->
			print_cdr(Rec)
	end.

-type(find_cdr_test() :: any()). % TODO flesh this out.  there's a lot of em
-type(find_cdr_tests() :: [find_cdr_test()]).
-spec(find_cdr/1 :: (Tests :: find_cdr_tests()) -> [#cdr_rec{}]).
find_cdr(Tests) when is_list(Tests) ->
	QH = qlc:q([Cdr || Cdr <- mnesia:table(cdr_rec), lists:all(fun(Test) -> find_cdr_test(Test, Cdr) end, Tests)]),
	find_cdr(QH);
find_cdr(QH) ->
	{atomic, List} = mnesia:transaction(fun() -> qlc:e(QH) end),
	List.

find_cdr_test({type, Type}, Cdr) ->
	find_cdr_test({mediatype, Type}, Cdr);
find_cdr_test({mediatype, Type}, #cdr_rec{media = Call}) ->
	Call#call.type =:= Type;
find_cdr_test({older, Time}, #cdr_rec{timestamp = Tstamp}) when Time < Tstamp ->
	true;
find_cdr_test({younger, Time}, #cdr_rec{timestamp = Tstamp}) when Time > Tstamp ->
	true;
find_cdr_test({duration, Time}, Cdr) ->
	find_cdr_test({length, Time}, Cdr);
find_cdr_test({length, Time}, #cdr_rec{timestamp = Tstamp, transactions = inprogress}) ->
	Now = util:now(),
	Now - Tstamp >= Time;
find_cdr_test({length, Time}, #cdr_rec{transactions = Trans}) ->
	[#cdr_raw{start = S, ended = E}] = [X || #cdr_raw{transaction = cdrinit} = X <- Trans],
	E - S >= Time;
find_cdr_test({brand, Data}, Cdr) ->
	find_cdr_test({client, Data}, Cdr);
find_cdr_test({client, Data}, #cdr_rec{media = Media}) ->
	Client = Media#call.client,
	case Data of
		_ when Client#client.label == Data; Client#client.id == Data ->
			true;
		_ ->
			false
	end;
find_cdr_test({agent, Agent}, #cdr_rec{media = Media, transactions = inprogress}) ->
	Id = Media#call.id,
	case mnesia:transaction(fun() -> qlc:e(qlc:q([X || #cdr_raw{id = Testid, eventdata = Edata} = X <- mnesia:table(cdr_raw), Edata =:= Agent, Testid =:= Id])) end) of
		{atomic, []} ->
			false;
		{atomic, _} ->
			true
	end;
find_cdr_test({agent, Agent}, #cdr_rec{media = _Media, transactions = Trans}) ->
	case [X || #cdr_raw{eventdata = Edata} = X <- Trans, Edata =:= Agent] of
		[] ->
			false;
		_ ->
			true
	end;
find_cdr_test({callerid, Idbit}, #cdr_rec{media = Media}) ->
	Callerid = Media#call.callerid,
	case string:str(Idbit, Callerid) of
		0 ->
			false;
		_ ->
			true
	end;
find_cdr_test({callid, Id}, Cdr) ->
	find_cdr_test({mediaid, Id}, Cdr);
find_cdr_test({mediaid, Id}, #cdr_rec{media = Media}) ->
	Media#call.id =:= Id;
find_cdr_test(_, _) ->
	false.

% to be added soon TODO
%
%can_answer/2 (Media, Agent) -> true | missing skills
% queue_state -> pretty print the queue state
% media_states/0, /1 -> print a summary of medias (limited by type)
% queue_states/0, /1 -> print a summary of queue states (limited by type).
% migrate_queue/1 -> for the future.
% cdr:orphan_search
% cdr:pending_states (agent/cdr), global/node
% start_spec pretty print.
% cpx:start_spec

verify_apps(Appfiles, Dir) ->
	verify_apps(Appfiles, Dir, []).

verify_apps([], _Dir, Acc) ->
	Acc;
verify_apps([Appfile | Tail], Dir, Acc) ->
	case file:consult(filename:join(Dir, Appfile)) of
		{ok, AppList} when is_list(AppList) ->
			FilteredApplist = [Appname || {application, Appname, _} <- AppList, atom_to_list(Appname) ++ ".app" =:= Appfile],
			case FilteredApplist of
				[] ->
					?INFO("consulting ~s did not find app config", [Appfile]),
					verify_apps(Tail, Dir, Acc);
				[Appname] ->
					verify_apps(Tail, Dir, [Appname | Acc])
			end;
		_ ->
			?INFO("consulting ~s reveals it is not an app file.", [Appfile]),
			verify_apps(Tail, Dir, Acc)
	end.

start_plugins(undefined) ->
	?NOTICE("No plugin directory configured", []),
	ok;
start_plugins(Dir) ->
	case add_plugin_paths(Dir) of
		{error, badarg} ->
			?WARNING("Plugin directory ~p is not a directory!", [Dir]),
			ok;
		ok ->
			Appfiles = filelib:wildcard("*.app", Dir),
			Plugins = verify_apps(Appfiles, Dir),
			load_plugin_envs(Plugins),
			application:set_env('OpenACD', plugins, Plugins),
			start_plugin_apps(Plugins)
	end.

load_plugin_envs(Plugins) ->
	Transfun = fun() ->
		Qh = qlc:q([{Appname, Env} ||
			#cpx_conf{id = Appname, start_args = Env, supervisor = plugin} <- mnesia:table(cpx_conf),
			lists:member(Appname, Plugins)
		]),
		qlc:e(Qh)
	end,
	case mnesia:transaction(Transfun) of
		{aborted, Err} ->
			?WARNING("Could not load stored plugin envs:  ~p", [Err]);
		{atomic, Res} ->
			[set_plugin_env(Appname0,Envs0) || {Appname0, Envs0} <- Res]
	end.

load_plugin_env(Plugin) ->
	Transfun = fun() ->
		Qh = qlc:q([Env || #cpx_conf{id = P, start_args = Env, supervisor = plugin} <- mnesia:table(cpx_conf),
			P =:= Plugin
		]),
		qlc:e(Qh)
	end,
	case mnesia:transaction(Transfun) of
		{atomic, []} ->
			ok;
		{atomic, [Env]} ->
			set_plugin_env(Plugin, Env);
		Else ->
			?WARNING("Could not load plugin ~p env:  ~p", [Plugin, Else])
	end.

save_plugin_env(Plugin, Envs) ->
	Rec = #cpx_conf{id = Plugin, module_name = application,
		start_function = start, start_args = Envs, supervisor = plugin},
	Transfun = fun() ->
		mnesia:write(Rec)
	end,
	Res = mnesia:transaction(Transfun),
	?INFO("Result of saving plugin ~p env:  ~p", [Plugin, Res]).

start_plugin_apps(undefined) ->
	?INFO("No plugins to start", []),
	ok;
start_plugin_apps({ok, Plugins}) ->
	start_plugin_apps(Plugins);
start_plugin_apps([]) ->
	?INFO("Plugins started", []),
	ok;
start_plugin_apps([Plugin | Tail]) when is_atom(Plugin) ->
	start_plugin_app(Plugin),
	start_plugin_apps(Tail).

start_plugin_app(Plugin) ->
	StartFun = fun() ->
		case application:start(Plugin) of
			ok ->
				?INFO("Started plugin ~p", [Plugin]),
				ok;
			{error, {already_started, Plugin}} ->
				?INFO("Plugin ~p already started; perhaps something else depends on it?", [Plugin]),
				ok;
			{error, Else} ->
				?INFO("Plugin ~p not started due to ~p", [Plugin, Else]),
				ok
		end
	end,
	case application:load(Plugin) of
		{error, {already_loaded, Plugin}} ->
			StartFun();
		{error, Reason} ->
			?INFO("Plugin ~p app load failed:  ~p", [Plugin, Reason]),
			ok;
		ok ->
			{ok, Keys} = application:get_all_key(Plugin),
			Deps = proplists:get_value(applications, Keys),
			start_plugin_apps(Deps),
			StartFun()
	end.

add_plugin_paths() ->
	case cpx:get_env(plugin_dir) of
		{ok, Dir} ->
			add_plugin_paths(Dir);
		_ ->
			ok
	end.

add_plugin_paths(PluginDir) ->
	case filelib:is_dir(PluginDir) of
		false ->
			?ERROR("plugin_dir ~p not a directory", [PluginDir]),
			{error, badarg};
		true ->
			{ok, AppDirs} = file:list_dir(PluginDir),
			Paths = [filename:join([PluginDir, AppDir, "ebin"]) ||
				AppDir <- AppDirs],
			code:add_pathsz(Paths),
			ok
	end.

%add_plugin_deps({ok, Deps}, Dir) ->
%	add_plugin_deps(Deps, Dir);
%add_plugin_deps([], _) ->
%	ok;
%add_plugin_deps([Dep | Tail], Dir) ->
%	case filelib:is_dir(filename:join([Dir, "deps", Dep, "ebin"])) of
%		true ->
%			?INFO("Adding plugin dependancy  ~p to code path", [Dep]),
%			true = code:add_pathz(filename:join([Dir, "deps", Dep, "ebin"]));
%		false ->
%			ok
%	end,
%	add_plugin_deps(Tail, Dir).

%start_plugin_apps({ok, Plugins}, Dir) ->
%	start_plugin_apps(Plugins, Dir, []).
%
%start_plugin_apps([], _, Apps) ->
%	application:set_env('OpenACD', plugins_started, Apps);
%start_plugin_apps(["deps" | Tail], Dir, Apps) ->
%	start_plugin_apps(Tail, Dir, Apps);
%start_plugin_apps([Plugin | Tail], Dir, AccApps) ->
%	NewAcc = case filelib:is_dir(filename:join([Dir, Plugin, "ebin"])) of
%		true ->
%			?INFO("Adding plugin ~p to code path", [Plugin]),
%			true = code:add_pathz(filename:join([Dir, Plugin, "ebin"])),
%			case filelib:is_dir(filename:join([Dir, Plugin, "deps"])) of
%				true ->
%					DepDepsDir = filename:join(Dir, Plugin),
%					add_plugin_deps(file:list_dir(filename:join([DepDepsDir, "deps"])), DepDepsDir);
%				false ->
%					ok
%			end,
%			case file:consult(filename:join([Dir, Plugin, "ebin", [Plugin, ".app"]])) of
%				{ok, [{application, _, Properties}|_]} ->
%					Apps = proplists:get_value(applications, Properties, []),
%					[application:start(App) || App <- Apps],
%					PluginApp = list_to_atom(Plugin),
%					StartRes = application:start(PluginApp),
%					?INFO("starting plugin ~p:  ~p", [Plugin, StartRes]),
%					[PluginApp | AccApps];
%				{error, Err} ->
%					?WARNING("Plugin ~s failed to start due to app file read error ~p.", [Plugin, Err]),
%					AccApps
%			end;
%		false ->
%			AccApps
%	end,
%	start_plugin_apps(Tail, Dir, NewAcc).
	
-ifdef(TEST).

pretty_print_time_test_() ->
	[{"A few seconds",
	?_assertEqual("5 Seconds ", pretty_print_time(5))},
	{"A couple of minutes",
	?_assertEqual("2 Minutes 23 Seconds ", pretty_print_time(120 + 23))},
	{"A few hours",
	?_assertEqual("12 Hours 32 Minutes 54 Seconds ", pretty_print_time((12 * 60 * 60) + (32 * 60) + 54))},
	{"Several days",
	?_assertEqual("3 Days 0 Hours 27 Minutes 10 Seconds ", pretty_print_time((3 * 24 * 60 * 60) + (60 * 27) + 10))},
	{"Many weeks",
	?_assertEqual("32 Weeks 4 Days 10 Hours 3 Minutes 7 Seconds ", pretty_print_time((32 * 7 * 24 * 60 * 60) + (4 * 24 * 60 * 60) + (10 * 60 * 60) + (3 * 60) + 7))},
	{"Years",
	?_assertEqual("3 Years 5 Weeks 6 Days 21 Hours 53 Minutes 3 Seconds ", pretty_print_time((3 * 52 * 7 * 24 * 60 * 60) + (5 * 7 * 24 * 60 * 60) + (6 * 24 * 60 * 60) + (60 * 60 * 21) + (53 * 60) + 3))}].

-endif.
