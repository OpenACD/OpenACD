%%%-------------------------------------------------------------------
%%% File          : cook_supervisor.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  10/21/08
%%%-------------------------------------------------------------------

%% @doc The supervisor for the cooks.  When a new call is put into a queue, this starts
%% a new {@link cook} to watch/modify that call based on the queues recipe.
%% @see cook
%% @see call_queue
-module(cook_supervisor).
-author("Micah").

-behaviour(supervisor).

-include("call.hrl").
-include("agent.hrl").
-include("queue.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(Call) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
	
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
	Cook = {cook, {cook, start_link, []}, transient, brutal_kill, worker, [?MODULE]},
	{ok, {{simple_one_for_one, 3, 5}, [Cook]}}.

%%====================================================================
%% Internal functions
%%====================================================================
