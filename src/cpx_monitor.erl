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

%% @doc Stat tracker and monitoring module.  A combination of the cdr handler
%% and cpx_supervisor.  This handles health stats, and non-process monitoring.

-module(cpx_monitor).
-author(micahw).

-behaviour(gen_server).

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
	start_link/1,
	start/1,
	stop/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	nodes = [] :: [atom()],
	monitoring = [] :: [{atom(), any()}],
	down = [] :: [atom()]
}).

%%====================================================================
%% API
%%====================================================================
	
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

start(Args) ->
	gen_server:start({local, ?MODULE}, ?MODULE, Args, []).
	
stop() ->
	gen_server:call(?MODULE, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init(Args) when is_list(Args) ->
	process_flag(trap_exit, true),
	Nodes = proplists:get_all_values(nodes, Args),
	Mons = Nodes,
	lists:foreach(fun(N) -> monitor_node(N, true) end, Mons),
    {ok, #state{nodes = Nodes, monitoring = Mons}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_cast({recover, Node}, State) ->
	case lists:member(Node, State#state.down) of
		false ->
			% just chill
			{noreply, State};
		true ->
			?INFO("~w appears to have recovered.", [Node]),
			Newdown = lists:delete(Node, State#state.down),
			Newmon = [Node | State#state.monitoring],
			{noreply, State#state{down = Newdown, monitoring = Newmon}}
	end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_info({nodedown, Node}, State) ->
	case lists:member(Node, State#state.monitoring of
		true ->
			%monitor_node(Node, false),
			?ALERT("Node ~w is detected as being down!", [Node]),
			Newdown = [Node | State#state.down],
			Newmon = lists:delete(Node, State#state.monitoring),
			Sfun = fun() ->
				check_loop(Node)
			end,
			Pid = spawn(Sfun),
			timer:send_after(10000, Pid, check),
			{noreply, State#state{down = Newdown, monitoring = Newmon}};
		false ->
			% just chill
			{noreply, State}
	end;
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

recover(Node) ->
	gen_server:cast(?MODULE, {recover, Node}).

recovery_watch(Node) ->
	monitor_node(Node, false),
	receive
		{nodedown, Node} ->
			timer:send_after(1000, check)
	after 100 ->
		recover(Node),
		ok
	end,
	check_loop(Node).

check_loop(Node) ->
	receive
		check ->
			recovery_watch(Node); 
		_Otherwise -> 
			check_loop(Node)
	end.



-ifdef(EUNIT).


-endif.

