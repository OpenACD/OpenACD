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

-behaviour(gen_leader).

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
-export([init/1, 
	elected/3,
	surrendered/3,
	handle_DOWN/3,
	handle_leader_call/4,
	handle_leader_cast/3,
	from_leader/3,
	handle_call/4, 
	handle_cast/3, 
	handle_info/2,
	terminate/2, 
	code_change/4]).

-record(state, {
	nodes = [] :: [atom()],
	monitoring = [] :: [{atom(), any()}],
	down = [] :: [atom()],
	auto_restart_mnesia = true :: 'false' | 'false',
	merge_status = []
}).

%%====================================================================
%% API
%%====================================================================
	
start_link(Args) ->
	Nodes = proplists:get_all_values(nodes, Args),
    gen_leader:start_link(?MODULE, Nodes, [], ?MODULE, Args, []).

start(Args) ->
	Nodes = proplists:get_all_values(nodes, Args),
	gen_leader:start(?MODULE, Nodes, [], ?MODULE, Args, []).
	
stop() ->
	gen_leader:call(?MODULE, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init(Args) when is_list(Args) ->
	process_flag(trap_exit, true),
	Nodes = proplists:get_all_values(nodes, Args),
	Mons = Nodes,
	lists:foreach(fun(N) -> monitor_node(N, true) end, Mons),
    {ok, #state{
		nodes = Nodes, 
		monitoring = Mons,
		auto_restart_mnesia = proplists:get_value(auto_restart_mnesia, Args, true)
	}}.


%% @hidden
elected(State, Election, Node) -> 
	?INFO("elected by ~w", [Node]),
	{ok, ok, State}.
	
%% @hidden
%% TODO what about an agent started at both places?
surrendered(State, _LeaderState, _Election) -> 
	{ok, State}.
	
%% @hidden
handle_DOWN(Node, State, _Election) -> 
	{ok, State}.

%% @hidden
handle_leader_call(Message, From, State, _Election) ->
	?WARNING("received unexpected leader_call ~p from ~p", [Message, From]),
	{reply, ok, State}.

%% @hidden
handle_leader_cast(Message, State, Election) ->
	?WARNING("received unexpected leader_cast ~p", [Message]),
	{noreply, State}.

%% @hidden
from_leader(_Msg, State, _Election) -> 
	?DEBUG("Stub from leader.", []),
	{ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------
handle_call(stop, _From, State, Election) ->
	{stop, normal, ok, State};
handle_call(_Request, _From, State, Election) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_cast({recover, Node}, State, _Election) ->
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
handle_cast(_Request, State, _Election) -> 
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_info({nodedown, Node}, State) ->
	case lists:member(Node, State#state.monitoring) of
		true ->
			%monitor_node(Node, false),
			?ALERT("Node ~w is detected as being down!", [Node]),
			Newdown = [Node | State#state.down],
			Newmon = lists:delete(Node, State#state.monitoring),
			Sfun = fun() ->
				check_loop(Node)
			end,
			Pid = spawn_link(Sfun),
			timer:send_after(10000, Pid, check),
			{noreply, State#state{down = Newdown, monitoring = Newmon}};
		false ->
			% just chill
			{noreply, State}
	end;
handle_info({'EXIT', From, Reason}, State) ->
	?INFO("~p said it died due to ~p.", [From, Reason]),
	{noreply, State};
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
code_change(_OldVsn, State, _Election, _Extra) ->
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

state_test_() ->
	[{"node down message recieved",
	fun() ->
		State = #state{
			monitoring = [node]
		},
		{noreply, Newstate} = handle_info({nodedown, node}, State),
		Expect = #state{down = [node], monitoring = []},
		?assertEqual(Expect, Newstate)
	end},
	{"node down message about a node we aren't monitoring",
	fun() ->
		State = #state{down = [node]},
		{noreply, Newstate} = handle_info({nodedown, node}, State),
		?assertEqual(State, Newstate)
	end},
	{"recovery message",
	fun() ->
		State = #state{down = [node]},
		{noreply, Newstate} = handle_cast({recover, node}, State, {}),
		Expected = #state{monitoring = [node], down = []},
		?assertEqual(Expected, Newstate)
	end},
	{"recovery about a node that isn't down",
	fun() ->
		State = #state{monitoring = [node]},
		{noreply, Newstate} = handle_cast({recover, node}, State, {}),
		?assertEqual(State, Newstate)
	end}].
	

-endif.

