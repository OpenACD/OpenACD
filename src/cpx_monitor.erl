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

%% @doc Stat tracker callback module.  Similar to the cdr handler, the monitor
%% tracks changes to the system, and recalculates the aggregate values about
%% every 5 seconds based on the tracked changes.
%%

-module(cpx_monitor).
-author(micahw).

-behaviour(gen_event).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type(call_health() :: {Aggregate :: integer()}).
-type(agent_health() :: {Aggregate :: integer(), [{string(), call_health()}]}).
-type(queue_health() :: {Aggregate :: integer(), [{string(), call_health()}]}).
-type(agent_profile_health() :: {Aggregate :: integer(), [{string(), agent_health()}]}).
-type(queue_group_health() :: {Aggregate :: integer(), [{string(), queue_health()}]}).
-type(node_health() :: {Aggregate :: integer(), Queuegroups :: [{string(), queue_group_health()}], Agentprofiles :: [{string(), agent_profile_health()}]}).
-type(system_health() :: {Aggregate :: integer(), Nodes :: [{atom(), node_health()}]}).

-include("log.hrl").

%% API
-export([
	start/0,
	start_link/0, 
	add_handler/1
]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-record(state, {
	debug = false,
	regen_timer,
	data_cache = {0, []} :: system_health()
}).

-define(REGEN_INTERVAL, 5000).
-define(DEBUG_OUTPUT, {10, [
	{'unhealthy@testplace', {0, [
		{"unhealthyqueuegroup", {0, 
			[{"healthyqueue", {100, [
				{"healthycall1", {100}},
				{"healthycall2", {90}}
			]}},
			{"unhealthyqueue", {0, [
				{"unhealthycall1", {0}},
				{"unhealthycall2", {20}}
			]}}
		]}}
	]}},
	{'healthy@testplace', {100, [
		{"healthyqueuegroup", {80, 
			[{"healthyqueue", {90, [
				{"healthycall1", {80}},
				{"healythcall2", {70}}
			]}}
		]}}
	]}}
]}).
	

%%====================================================================
%% gen_event callbacks
%%====================================================================
start() ->
	gen_event:start({local, ?MODULE}).

start_link() ->
    gen_event:start_link({local, ?MODULE}). 

debug() ->
	start(),
	add_handler([{debug, true}]).

%%--------------------------------------------------------------------
%% Function: add_handler() -> ok | {'EXIT',Reason} | term()
%%--------------------------------------------------------------------
add_handler(Opts) ->
    gen_event:add_handler(?MODULE, ?MODULE, [Opts]).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%%--------------------------------------------------------------------
init(Opts) ->
	Debugs = proplists:get_value(debug, Opts, false),
	{ok, Tref} = timer:send_after(?REGEN_INTERVAL, regen_data),
    {ok, #state{debug = Debugs, regen_timer = Tref}}.

%%--------------------------------------------------------------------
%% handle_event(Event, State) -> {ok, State} |
%%--------------------------------------------------------------------
handle_event(Event, State) ->
	?INFO("Handilning event ~p", [Event]),
	%% TODO actually implement the stats tracking
    {ok, State}.

%%--------------------------------------------------------------------
%% handle_call(Request, State) -> {ok, Reply, State} |
%%--------------------------------------------------------------------

handle_call(Request, State) ->
	?DEBUG("Handling request ~p", [Request]),
	{ok, ok, State}.

%%--------------------------------------------------------------------
%% handle_info(Info, State) -> {ok, State} |
%%--------------------------------------------------------------------
handle_info(regen_data, State) ->
	?DEBUG("regenerating data cache", []),
	% Again, this should acutally do stuff.  For now, just recreate the timer.
	{ok, Tref} = timer:send_after(?REGEN_INTERVAL, regen_data),
	{ok, State#state{regen_timer = Tref}};
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(_Reason, #state{regen_timer = Tref} = State) ->
	timer:cancel(Tref),
    ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState} 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
