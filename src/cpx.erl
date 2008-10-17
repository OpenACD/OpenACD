%%%-------------------------------------------------------------------
%%% File          : cpx.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  10/17/08
%%%-------------------------------------------------------------------
-module(cpx).
-author("Micah").

-include("call.hrl").
-include("agent.hrl").

-ifdef(EUNIT).
	-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
	DispatchSpec = {dispatch_manager, {dispatch_manager, start_link, []}, permanent, 2000, worker, [?MODULE]},
	AgentManagerSpec = {agent_manager, {agent_manager, start_link, []}, permanent, 2000, worker, [?MODULE]},
	AgentListenerSpec = {agent_connection_listener, {agent_connection_listener, start, []}, permanent, 20000, worker, [?MODULE]},
	Specs = [DispatchSpec, AgentManagerSpec, AgentListenerSpec],
	io:format("specs:  ~p~n", [supervisor:check_childspecs(Specs)]),
    {ok,{{one_for_one,3,5}, Specs}}.

%%====================================================================
%% Internal functions
%%====================================================================

-ifdef(EUNIT).

%start_test() -> 
%	{ok, Pid} = start_link(),
%	?debugFmt("This pid is:  ~p.~n", [Pid]).
-endif.