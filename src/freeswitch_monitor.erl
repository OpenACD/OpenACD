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
		monitor_agent/3,
		monitor_client/3
	]).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").

-record(state, {
		type :: 'agent' | 'client',
		oncall = false :: boolean(),
		uuid :: string(),
		filter :: fun(),
		node :: node()
	}).

monitor_agent(Agent, Dialstring, Node) ->
	case agent_auth:get_agent(Agent) of
		{'atomic', [AgentAuth]} ->
			ID = AgentAuth#agent_auth.id,
			Filter = fun({set, {{agent, AID}, _Health, _Details, _Timestamp}}) when AID == ID ->
					true;
				(_Msg) ->
					false
			end,
			gen_server:start(?MODULE, [agent, Filter, Dialstring, Node], []);
		_ ->
			{error, no_agent}
	end.

monitor_client(Client, Dialstring, Node) ->
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
			gen_server:start(?MODULE, [client, Filter, Dialstring, Node], [])
	end.

init([Type, Filter, Dialstring, Node]) ->
	Self = self(),
	freeswitch:bgapi(Node, originate, Dialstring ++ " &park()", fun(Status, Reply) -> Self ! {originate, Status, Reply} end),
	{ok, #state{type = Type, filter = Filter, node = Node}}.

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({cpx_monitor_event, {set, {{agent, Key}, _Health, Details, Timestamp}}}, State) ->
	case proplists:get_value(state, Details) of
		S when State#state.oncall == false, (S == oncall orelse S == outgoing) ->
			?NOTICE("Watched ~p just went oncall ~p", [State#state.type, Details]),
			% check the state data to make sure its a voice call
			case proplists:get_value(statedata, Details) of
				Call when is_record(Call, call), element(3, Call) == voice ->
					?NOTICE("we can eavesdrop on ~p", [Call#call.id]),
					Res = freeswitch:sendmsg(State#state.node, State#state.uuid,
						[{"call-command", "execute"},
							{"execute-app-name", "eavesdrop"},
							{"execute-app-arg", Call#call.id}]),
					?NOTICE("eavesdrop result: ~p", [Res]),
					{noreply, State#state{oncall = true}};
				_ ->
					{noreply, State}
			end;
		_ when State#state.oncall == true ->
			?NOTICE("Watched ~p just went offcall", [State#state.type]),
			{noreply, State#state{oncall = false}};
		_ ->
			{noreply, State}
	end;
handle_info({originate, ok, "+OK "++RawUUID}, State) ->
	UUID = util:string_chomp(RawUUID),
	?NOTICE("Call originated OK: ~p", [UUID]),
	case freeswitch:handlecall(State#state.node, UUID) of
		ok ->
			?NOTICE("bound to call", []),
			cpx_monitor:subscribe(State#state.filter),
			% TODO - find initial call to monitor, if any
			{noreply, State#state{uuid = UUID}};
		Reply ->
			{stop, {no_channel, Reply}, State}
	end;
handle_info({originate, error, Reply}, State) ->
	?NOTICE("Call originate FAILED", []),
	{stop, {bad_originate, Reply}, State};
handle_info(call_hangup, State) ->
	{stop, normal, State};
handle_info(Info, State) ->
	%?NOTICE("Got message: ~p", [Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_Oldvsn, State, _Extra) ->
	{ok, State}.

