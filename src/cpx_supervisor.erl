%%%-------------------------------------------------------------------
%%% File          : cpx.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  10/17/08
%%%-------------------------------------------------------------------

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

-module(cpx_supervisor).
-author("Micah").

-include("call.hrl").
-include("agent.hrl").
-include_lib("stdlib/include/qlc.hrl").

-ifdef(EUNIT).
	-include_lib("eunit/include/eunit.hrl").
-endif.

-record(cpx_conf, {
	module_name :: atom(),
	start_function :: atom(),
	start_args :: [any()]
}).

-behaviour(supervisor).

%% API
-export([start_link/0, start/0]).
-export([
	add_conf/3,
	build_spec/1,
	build_tables/0,
	destroy/1,
	update_conf/4
	]).
	
%% Supervisor callbacks
-export([init/1]).

%-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
start() -> 
	{ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
	unlink(Pid),
	{ok, Pid}.
%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
	case build_tables() of
		ok -> 
			DispatchSpec = {dispatch_manager, {dispatch_manager, start_link, []}, permanent, 2000, worker, [?MODULE]},
			AgentManagerSpec = {agent_manager, {agent_manager, start_link, []}, permanent, 2000, worker, [?MODULE]},
			QueueManagerSpec = {queue_manager, {queue_manager, start, []}, permanent, 20000, worker, [?MODULE]},

			Specs = lists:append([DispatchSpec, AgentManagerSpec, QueueManagerSpec], load_specs()),
			
			io:format("specs:  ~p~n", [supervisor:check_childspecs(Specs)]),
			{ok,{{one_for_one,3,5}, Specs}};
		Else -> 
			ignore
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

%% @doc Attempts to build a valid childspec suitbable for a supervisor module from the Record#cpx_conf.
build_spec(#cpx_conf{module_name = Mod, start_function = Start, start_args = Args} = Record) -> 
	Spec = {Mod, {Mod, Start, Args}, permanent, 20000, worker, [?MODULE]},
	case supervisor:check_childspecs([Spec]) of
		ok -> 
			Spec;
		Else -> 
			Else
	end.

%% @doc attempts to build the cpx_conf table.  Ignores errors which occur if it already exists.
build_tables() -> 
	io:format("cpx building tables...~n"),
	Nodes = [node()],
	mnesia:create_schema(Nodes),
	mnesia:start(),
	A = mnesia:create_table(cpx_conf, [
		{attributes, record_info(fields, cpx_conf)},
		{disc_copies, Nodes},
		{local_content, true}
	]),
	case A of
		{atomic, ok} -> 
			ok;
		{aborted, {already_exists, _Table}} ->
			ok;
		Else -> 
			Else
	end.

%% @doc Removes the passed childspec() or #cpx_conf from the database.
destroy({Id, _Params, _Transience, _Time, _Type, _Module}) -> 
	destroy(Id);
destroy(Spec) when is_atom(Spec) -> 
	F = fun() -> 
		mnesia:delete({cpx_conf, Spec})
	end,
	mnesia:transaction(F).

%% @doc updates the conf with key Name with new Mod, Start, and Args.
%% @see add_conf/3
update_conf(Name, Mod, Start, Args) -> 
	Rec = #cpx_conf{module_name = Mod, start_function = Start, start_args = Args},
	F = fun() -> 
		destroy(Name),
		mnesia:write(Rec)
	end,
	mnesia:transaction(F).
	
-ifndef(EUNIT).
start_spec(Spec) -> 
	supervisor:start_child(cpx_supervisor, Spec).
	
load_specs() -> 
	F = fun() -> 
		QH = qlc:q([X || X <- mnesia:table(cpx_conf)]),
		qlc:e(QH)
	end,
	case mnesia:transaction(F) of
		{atomic, Records} -> 
			lists:map(fun(I) -> build_spec(I) end, Records);
		Else -> 
			Else
	end.
-else.
start_spec(_Sepc) -> 
	supervisor:start_child(cpx_supervisor, Spec).
	
load_specs() -> 
	[
		{agent_tcp_listener, {agent_tcp_listener, start, []}, permanent, 20000, worker, [?MODULE]},
		{freeswitch_media_manager, {freeswitch_media_manager, start, [freeswitch@freecpx.dev, "freecpx.dev"]}, permanent, 20000, worker, [?MODULE]},
		{cpx_web_management, {cpx_web_management, start, []}, permanent, 100, worker, [?MODULE]},
		{agent_web_listener, {agent_web_listener, start, []}, permanent, 20000, worker, [?MODULE]},
		{agent_auth, {agent_auth, start, []}, permanent, 20000, worker, [?MODULE]}
	].
			 
-endif.

-ifdef(EUNIT).

config_test_() -> 
	mnesia:start(),
	[
		{
			"Adding a Valid Config",
			fun() -> 
				Valid = #cpx_conf{module_name = dummy_mod, start_function = start, start_args = []},
				add_conf(dummy_mod, start, []),
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
				add_conf(dummy_mod, start, []),
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
				add_conf(dummy_mod, start, []),
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
				Spec = {dummy_mod, {dummy_mod, start, []}, permanent, 100, worker, [?MODULE]},
				add_conf(dummy_mod, start, []),
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
	].
				
%start_test() -> 
%	{ok, Pid} = start_link(),
%	?debugFmt("This pid is:  ~p.~n", [Pid]).
-endif.
