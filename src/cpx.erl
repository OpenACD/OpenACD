%%%-------------------------------------------------------------------
%%% File          : cpx.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  10/17/08
%%%-------------------------------------------------------------------

%% @doc The applcation module for the cpx.
-module(cpx).
-author("Micah").

-behaviour(application).

-include("call.hrl").
-include("agent.hrl").

-ifdef(EUNIT).
	-include_lib("eunit/include/eunit.hrl").
-endif.

-export[start/2, stop/1].

start(_Type, _StartArgs) -> 
	cpx_supervisor:start_link().
	
stop(_State) -> 
	ok.