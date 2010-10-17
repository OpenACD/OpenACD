%% "The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%%
%%	The Original Code is OpenACD.
%%
%%	The Initial Developers of the Original Code is 
%%	Andrew Thompson.
%%
%%	All portions of the code written by the Initial Developers are Copyright
%%	(c) 2008-2010 Andrew Thompson.
%%	All Rights Reserved.
%%
%%	Contributor(s):
%%
%%	Andrew Thompson <andrew at hijacked dot us>

%% @doc A simple outbound dialer, this is NOT a predictive dialer.

-module(freeswitch_monitor).

-behaviour(gen_server).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

%% API
-export([
		monitor_agent/2,
		monitor_client/2
	]).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").

-record(state, {
		type :: 'agent' | 'client',
		oncall = false :: boolean()
	}).

monitor_agent(Agent, Dialstring) ->
	case agent_auth:get_agent(Agent) of
		{'atomic', [AgentAuth]} ->
			ID = AgentAuth#agent_auth.id,
			Filter = fun({set, {{agent, AID}, _Health, _Details, _Timestamp}}) when AID == ID ->
					true;
				(_Msg) ->
					false
			end,
			gen_server:start(?MODULE, [agent, Filter, Dialstring], []);
		_ ->
			{error, no_agent}
	end.

monitor_client(Client, Dialstring) ->
	case call_queue_config:get_client(Client) of
		none ->
			{error, no_client};
		ClientRec ->
			Filter = fun({set, {_, _Health, Details, Timestamp}}) ->
					case proplists:get_value(statedata, Details) of
						Res when is_record(Res, call) ->
							case Res#call.client of
								Res2 when Res2#client.id == ClientRec#client.id ->
									true;
								_ ->
									false
							end;
						_ ->
							false
					end;
				(_Msg) ->
					false
			end,
			gen_server:start(?MODULE, [client, Filter, Dialstring], [])
	end.

init([Type, Filter, Dialstring]) ->
	cpx_monitor:subscribe(Filter),
	{ok, #state{type = Type}}.

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({cpx_monitor_event, {set, {{agent, Key}, _Health, Details, Timestamp}}}, State) ->
	case proplists:get_value(state, Details) of
		S when S == oncall; S == outgoing ->
			?NOTICE("Watched ~p just went oncall", [State#state.type]),
			{noreply, State#state{oncall = true}};
		_ when State#state.oncall == true ->
			?NOTICE("Watched ~p just went offcall", [State#state.type]),
			{noreply, State#state{oncall = false}};
		_ ->
			{noreply, State}
	end;
handle_info(Info, State) ->
	%?NOTICE("Got message: ~p", [Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_Oldvsn, State, _Extra) ->
	{ok, State}.

