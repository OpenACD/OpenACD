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

%% @doc Listener for agent events from the dialplan -- dialplan agents can be started from
%% here and then subsequently control them.

-module(agent_dialplan_listener).

-behaviour(gen_server).

-include("log.hrl").
-include("call.hrl").
%-include("queue.hrl").
-include("agent.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {
	registry = dict:new()
}).

%% API
-export([start/0, start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	process_flag(trap_exit, true),
	{ok, #state{}}.

handle_call(Request, From, State) ->
	?DEBUG("Call from ~p:  ~p", [From, Request]),
	{reply, {unknown_call, Request}, State}.

handle_cast(Msg, State) ->
	?DEBUG("Cast ~p", [Msg]),
	{noreply, State}.

handle_info({freeswitch_sendmsg, "agent_login " ++ Parameters}, State) ->
	case util:string_split(Parameters, " ") of
		[Username] ->
			Endpoint = {sip_registration, Username};
		[Username, EndpointType] ->
			Endpoint = construct_endpoint(EndpointType, Username);
		[Username, EndpointType, EndpointData] ->
			Endpoint = construct_endpoint(EndpointType, EndpointData)
	end,

	case dict:find(Username, State#state.registry) of
		error ->
			case agent_auth:get_agent(Username) of
				{atomic, []} ->
					?INFO("no such agent ~p", [Username]),
					{noreply, State};
				{atomic, [_A, _B | _]} ->
					?WARNING("more than one agent found for username ~p, login failed", [Username]),
					{noreply, State};
				{atomic, [AgentAuth]} when Endpoint == error ->
					?WARNING("~p tried to login with invalid endpoint parameters ~p", [Parameters]);
				{atomic, [AgentAuth]} ->
					Agent = #agent{id = AgentAuth#agent_auth.id, login = AgentAuth#agent_auth.login, skills = AgentAuth#agent_auth.skills, profile = AgentAuth#agent_auth.profile},
					case agent_dialplan_connection:start(Agent, AgentAuth#agent_auth.securitylevel) of
						{ok, Pid} ->
							?INFO("~s logged in with endpoint ~p", [Username, Endpoint]),
							gen_server:call(Pid, {set_endpoint, Endpoint}),
							link(Pid),
							{noreply, State#state{registry = dict:store(Username, Pid, State#state.registry)}};
						ignore ->
							?WARNING("Ignore message trying to start connection for ~p", [Username]),
							{noreply, State};
						{error, Error} ->
							?ERROR("Error ~p trying to start connection for ~p", [Error, Username]),
							{noreply, State}
					end
			end;
		{ok, Pid} ->
			?NOTICE("~p is already logged in at ~p", [Username, Pid]),
			{noreply, State}
	end;
handle_info({freeswitch_sendmsg, "agent_logoff " ++ Username}, State) ->
	case dict:find(Username, State#state.registry) of
		{ok, Pid} ->
			?DEBUG("requesting ~p logoff", [Username]),
			catch agent_dialplan_connection:logout(Pid),
			% wait for the EXIT message to cleanup
			{noreply, State};
		error ->
			?NOTICE("~p is not logged in", [Username]),
			{noreply, State}
	end;
handle_info({freeswitch_sendmsg, "agent_release " ++ Username}, State) ->
	case dict:find(Username, State#state.registry) of
		{ok, Pid} ->
			?DEBUG("requesting ~p go released", [Username]),
			catch agent_dialplan_connection:go_released(Pid),
			{noreply, State};
		error ->
			?NOTICE("~p is not logged in", [Username]),
			{noreply, State}
	end;
handle_info({freeswitch_sendmsg, "agent_available " ++ Username}, State) ->
	case dict:find(Username, State#state.registry) of
		{ok, Pid} ->
			?DEBUG("requesting ~p go available", [Username]),
			catch agent_dialplan_connection:go_available(Pid),
			{noreply, State};
		error ->
			?NOTICE("~p is not logged in", [Username]),
			{noreply, State}
	end;
handle_info({'EXIT', Pid, Reason}, State) ->
	?DEBUG("Doing a cleanup for pid ~w which died due to ~p", [Pid, Reason]),
	List = [ {B, A} || {A, B} <- dict:to_list(State#state.registry) ],
	case proplists:get_value(Pid, List) of
		undefined ->
			?INFO("unable to find entry for ~p", [Pid]),
			{noreply, State};
		Value ->
			?INFO("removing connection for ~p at ~p", [Value, Pid]),
			{noreply, State#state{registry = dict:erase(Value, State#state.registry)}}
		end;
handle_info(Info, State) ->
	?DEBUG("Info:  ~p", [Info]),
	{noreply, State}.

terminate(Reason, _State) when Reason == normal; Reason == shutdown ->
	?NOTICE("Graceful termination:  ~p", [Reason]),
	ok;
terminate(Reason, _State) ->
	?NOTICE("Terminating dirty:  ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% TODO - this should be a common function
construct_endpoint(Type, Data) ->
	case list_to_existing_atom(Type) of
			sip_registration ->
				{sip_registration, Data};
			sip ->
				{sip, Data};
			iax2 ->
				{iax2, Data};
			h323 ->
				{h323, Data};
			pstn ->
				{pstn, Data};
			_ ->
				invalid
		end.

