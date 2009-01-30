%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Original Code is Spice Telphony.
%% 
%% The Initial Developer of the Original Code is 
%% Andrew Thompson and Micah Warren.
%% Portions created by the Initial Developers are Copyright (C) 
%% SpiceCSM. All Rights Reserved.

%% Contributor(s): 

%% Andrew Thompson <athompson at spicecsm dot com>
%% Micah Warren <mwarren at spicecsm dot com>
%% 

%% @doc The supervisor for the cooks.  When a new call is put into a queue, this starts
%% a new {@link cook} to watch/modify that call based on the queue's recipe.
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
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% API functions
%% @doc Starts the cook supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
	
%% Supervisor callbacks
%% @private
init([]) ->
	Cook = {cook, {cook, start_link, []}, transient, brutal_kill, worker, [?MODULE]},
	{ok, {{simple_one_for_one, 3, 5}, [Cook]}}.
