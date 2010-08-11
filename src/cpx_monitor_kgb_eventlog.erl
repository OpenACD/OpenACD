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

%% @doc Generates an eventlog in a format approaching a legacy format used by KGB

-module(cpx_monitor_kgb_eventlog).

-behaviour(gen_server).

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
-endif.
-include("log.hrl").
-include("call.hrl").

%% API
-export([
	start/0,
	start/1,
	start_link/0,
	start_link/1
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
		file,
		agents,
		calls
	}).


start() ->
	start([]).

start(Props) ->
	gen_server:start(?MODULE, Props, []).

start_link() ->
	start_link([]).

start_link(Props) ->
	gen_server:start_link(?MODULE, Props, []).

init(Props) ->
	Filename = proplists:get_value(filename, Props, "events.log"),
	case file:open(Filename, [append]) of
			{ok, File} ->
				{ok, Agents} = cpx_monitor:get_health(agent),
				{ok, Calls} = cpx_monitor:get_health(media),
				AgentDict = dict:from_list([{Key, Value} || {{agent, Key}, _Health, Value} <- Agents]),
				CallDict = dict:from_list([{Key, Value} || {{media, Key}, _Health, Value} <- Calls]),
				cpx_monitor:subscribe(),
				{ok, #state{file = File, agents = AgentDict, calls = CallDict}};
			{error, Reason} ->
				{stop, {error, Reason}}
	end.

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State, hibernate}.

handle_cast(_Msg, State) ->
	{noreply, State, hibernate}.

handle_info({cpx_monitor_event, {set, {{agent, Key}, _Health, Details, Timestamp}}}, State) ->
	case dict:find(Key, State#state.agents) of
		error ->
			%?NOTICE("Agent ~p just logged in", [Key]),
			io:format(State#state.file, "~s : ~p : agent_start : ~p : ~s~n", [inet_db:gethostname(), Timestamp, proplists:get_value(node, Details), Key]),
			[io:format(State#state.file, "~s : ~p : agent_login : ~p : ~s : ~s~n", [inet_db:gethostname(), Timestamp, proplists:get_value(node, Details), Queue, Key]) || {'_queue', Queue} <- proplists:get_value(skills, Details)],
			?INFO("skills: ~p", [proplists:get_value(skills, Details)]),
			ok;
		{ok, Current} ->
			%?NOTICE("Udating agent ~p from  ~p to ~p", [Key, Current, Details]),
			agent_diff(Key, Details, Current, State#state.file)
	end,
	{noreply, State#state{agents = dict:store(Key, Details, State#state.agents)}};
handle_info({cpx_monitor_event, {drop, {agent, Key}}}, State) ->
	case dict:find(Key, State#state.agents) of
		error ->
			{noreply, State};
		{ok, Current} ->
			io:format(State#state.file, "~s : ~p : agent_stop : ~p : ~s~n", [inet_db:gethostname(), util:now(), proplists:get_value(node, Current), Key]),
			[io:format(State#state.file, "~s : ~p : agent_logout : ~p : ~s : ~s~n", [inet_db:gethostname(), util:now(), proplists:get_value(node, Current), Queue, Key]) || {'_queue', Queue} <- proplists:get_value(skills, Current)],
			{noreply, State#state{agents = dict:erase(Key, State#state.agents)}}
	end;
handle_info({cpx_monitor_event, {set, {{media, Key}, _Health, Details, Timestamp}}}, State) ->
	case proplists:get_value(queue, Details) of
		undefined ->
			{noreply, State};
		Queue ->
			io:format(State#state.file, "~s : ~p : call_enqueue : ~p : ~s : ~s : ~s : ~s : ~s : ~s : ~s~n", [
					inet_db:gethostname(),
					Timestamp,
					proplists:get_value(node, Details),
					Queue,
					element(2, proplists:get_value(callerid, Details)),
					Key,
					"Origin Code",
					"CLS",
					"Source IP",
					proplists:get_value(dnis, Details, "")]),
			{noreply, State}
	end;
handle_info(Info, State) ->
	%?NOTICE("Got message: ~p", [Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_Oldvsn, State, _Extra) ->
	{ok, State}.

agent_diff(Agent, New, Old, File) ->
	% has the agent's state changed?
	case proplists:get_value(state, New) == proplists:get_value(state, Old) of
		true ->
			% ok, so not a state change, is it a profile change?
			case proplists:get_value(profile, New) == proplists:get_value(profile, Old) of
					true ->
						% hell if I know what changed
						ok;
					false ->
						% ok, now diff the skill lists to see if we've changed queue membership
						Lost = proplists:get_value(skills, Old) -- proplists:get_value(skills, New),
						Gained = proplists:get_value(skills, New) -- proplists:get_value(skills, Old),
						[io:format(File, "~s : ~p : agent_logout : ~p : ~s : ~s~n", [inet_db:gethostname(), util:now(), proplists:get_value(node, New), Queue, Agent]) || {'_queue', Queue} <- Lost],
						[io:format(File, "~s : ~p : agent_login : ~p : ~s : ~s~n", [inet_db:gethostname(), util:now(), proplists:get_value(node, New), Queue, Agent]) || {'_queue', Queue} <- Gained],
						ok
			end;
		false ->
			% state change!
			% what's the new state; if its oncall we just grabbed a call, if its released/idle we just finished one
			case {proplists:get_value(state, Old), proplists:get_value(state, New)} of
					{wrapup, _} ->
						Call = proplists:get_value(statedata, Old),

						io:format(File, "~s : ~p : call_terminate : ~p : ~s : ~s : ~s : ~s : ~s : ~s : ~s : ~s~n", [
								inet_db:gethostname(),
								util:now(),
								proplists:get_value(node, New),
								"Queue Name",
								Agent,
								element(2, Call#call.callerid),
								Call#call.id,
								"Origin",
								"CLS",
								"Source IP",
								Call#call.dnis
							]);
					{_, oncall} ->
						Call = proplists:get_value(statedata, New),

						io:format(File, "~s : ~p : call_pickup : ~p : ~s : ~s : ~s : ~s : ~s : ~s : ~s : ~s~n", [
								inet_db:gethostname(),
								util:now(),
								proplists:get_value(node, New),
								"Queue Name",
								Agent,
								element(2, Call#call.callerid),
								Call#call.id,
								"Origin",
								"CLS",
								"Source IP",
								Call#call.dnis
							]);
					{_, _} ->
						ok
				end
		end.

