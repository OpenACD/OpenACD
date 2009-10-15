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

-module(cpx_monitor_grapher).

-behaviour(gen_server).

-include("contrib/errd/include/errd.hrl").

%% API
-export([
	start_link/0,
	start/0
]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {
		rrd :: pid(),
		lastrun,
		agents
}).

% API

start() ->
	gen_server:start(?MODULE, [], []).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

% gen_server callbacks

init([]) ->
	{ok, Pid} = errd_server:start_link(),
	Now = now(),
	GroupedAgents = get_agents(Now),
	timer:send_after(30000, update),
	cpx_monitor:subscribe(),
	{ok, #state{rrd = Pid, lastrun = Now, agents = GroupedAgents}, hibernate}.

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State, hibernate}.

handle_cast(_Msg, State) ->
	{noreply, State, hibernate}.

handle_info(update, State) ->
	io:format("update time ~n"),
	Now = now(),
	GroupedAgents = get_agents(Now),
	timer:send_after(30000, update),
	Util = calculate_utilization(State#state.agents),
	update_utilization(Util, State#state.rrd),
	io:format("Utilization is ~p~n", [Util]),
	{noreply, State#state{lastrun = Now, agents = GroupedAgents}, hibernate};
handle_info({cpx_monitor_event, {set, {{agent, _}, _, Agent, _}}}, #state{agents = Agents} = State) ->
	NewAgents = update_agent(Agent, Agents),
	%io:format("Agents: ~p~n", [NewAgents]),
	{noreply, State#state{agents = NewAgents}, hibernate};
handle_info(Info, State) ->
	io:format("info: ~p~n", [Info]),
	{noreply, State, hibernate}.

terminate(_Reason, _State) ->
	ok.

code_change(_Oldvsn, State, _Extra) ->
	{ok, State}.

% internal functions

get_agents(Now) ->
	Agents = lists:map(fun({_, _, Agent}) -> [{lastchangetimestamp, Now} | proplists:delete(lastchangetimestamp, Agent)] end, element(2, cpx_monitor:get_health(agent))),
	util:group_by_with_key(fun(Agent) -> proplists:get_value(profile, Agent) end, Agents).

update_agent(Agent, Agents) ->
	Profile = proplists:get_value(profile, Agent),
	case proplists:get_value(Profile, Agents) of
		undefined ->
			[{Profile, [Agent]} | Agents];
		PAgents ->
			NAgents = proplists:delete(Profile, Agents),
			[{Profile, [Agent | PAgents]} | NAgents]
	end.

calculate_utilization(Agents) ->
	calculate_utilization_by_profile(Agents, []).

calculate_utilization_by_profile([], Acc) ->
	Acc;
calculate_utilization_by_profile([{Profile, Agents} | Tail], Acc) ->
	GroupedAgents = util:group_by_with_key(fun(Agent) -> proplists:get_value(login, Agent) end, Agents),
	Util = calculate_utilization_by_agent(GroupedAgents, 0),
	%io:format("groupedagents for profile ~p: ~p~n", [Profile, GroupedAgents]),
	calculate_utilization_by_profile(Tail, [{Profile, Util} | Acc]).

calculate_utilization_by_agent([], Acc) ->
	Acc;
calculate_utilization_by_agent([{Agent, States} | Tail], Acc) ->
	Util = calc(lists:reverse(States), 0, 0),
	%io:format("Agent ~p's utilization is ~p~n", [Agent, Util]),
	calculate_utilization_by_agent(Tail, Acc + Util).


calc([State], Util, Total) ->
	Diff = round(timer:now_diff(now(), proplists:get_value(lastchangetimestamp, State)) /1000000),
	AgentState = proplists:get_value(state, State),
	io:format("Agent was ~p for ~p~n", [AgentState, Diff]),
	NUtil = get_util(AgentState, Diff, Util),
	round((NUtil / (Total + Diff)) * 100);
calc([State1, State2 | Tail], Util, Total) ->
	Diff = round(timer:now_diff(proplists:get_value(lastchangetimestamp, State2), proplists:get_value(lastchangetimestamp, State1)) /1000000),
	AgentState = proplists:get_value(state, State1),
	io:format("Agent was ~p for ~p~n", [AgentState, Diff]),
	NUtil = get_util(AgentState, Diff, Util),
	calc([State2 | Tail], NUtil, Total + Diff).

get_util(AgentState, Diff, Util) when AgentState =:= oncall; AgentState =:= wrapup; AgentState =:= precall; AgentState =:= outgoing ->
	Diff + Util;
get_util(_, _Diff, Util) ->
	Util.


update_utilization([], _RRD) ->
	ok;
update_utilization([{Profile, Util} | Tail], RRD) ->
	Filename = re:replace(Profile, "[^a-zA-Z0-9_]", "", [{return, list}, global]),
	case errd_server:info(RRD, [Filename, ".rrd"]) of
		{error, _} ->
			% try to create on the assumption it doesn't exist
			{ok, _} = errd_server:command(RRD,
				#rrd_create{file=Filename++".rrd",
					step=30,
					ds_defs = [#rrd_ds{name=Filename, args="60:0:100", type = gauge}],
					rra_defs = [
						#rrd_rra{cf=average, args="0.5:120:24"}, % 1 day of 1 hour averages
						#rrd_rra{cf=average, args="0.5:960:42"} % 2 weeks of 8 hour averages
					]
				});
		_ ->
			ok
	end,
	errd_server:command(RRD, #rrd_update{file=Filename++".rrd", updates=[#rrd_ds_update{name=Filename, value=Util}]}),
	update_utilization(Tail, RRD).






