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
		calls,
		callqueuemap
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
				% hopefully the below won't take too long if this module is started on a busy system.
				CallQMap = init_call_queue_map(),
				cpx_monitor:subscribe(),
				{ok, #state{file = File, agents = AgentDict, calls = CallDict, callqueuemap = CallQMap}};
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
			%?NOTICE("Agent ~p just logged in ~p", [Key, Details]),
			io:format(State#state.file, "~s : ~s : agent_start : ~p : ~s~n", [inet_db:gethostname(), iso8601_timestamp(Timestamp), proplists:get_value(node, Details), proplists:get_value(login, Details)]),
			[io:format(State#state.file, "~s : ~s : agent_login : ~p : ~s : ~s~n", [inet_db:gethostname(), iso8601_timestamp(Timestamp), proplists:get_value(node, Details), proplists:get_value(login, Details), Queue]) || {'_queue', Queue} <- proplists:get_value(skills, Details)],
			?INFO("skills: ~p", [proplists:get_value(skills, Details)]),
			ok;
		{ok, Current} ->
			%?NOTICE("Udating agent ~p from  ~p to ~p", [Key, Current, Details]),
			agent_diff(Key, Details, Current, Timestamp, State)
	end,
	{noreply, State#state{agents = dict:store(Key, Details, State#state.agents)}};
handle_info({cpx_monitor_event, {drop, {agent, Key}, Timestamp}}, State) ->
	case dict:find(Key, State#state.agents) of
		error ->
			{noreply, State};
		{ok, Current} ->
			io:format(State#state.file, "~s : ~s : agent_stop : ~p : ~s~n", [inet_db:gethostname(), iso8601_timestamp(Timestamp), proplists:get_value(node, Current), proplists:get_value(login, Current)]),
			[io:format(State#state.file, "~s : ~s : agent_logout : ~p : ~s : ~s~n", [inet_db:gethostname(), iso8601_timestamp(Timestamp), proplists:get_value(node, Current), proplists:get_value(login, Current), Queue]) || {'_queue', Queue} <- proplists:get_value(skills, Current)],
			{noreply, State#state{agents = dict:erase(Key, State#state.agents)}}
	end;
handle_info({cpx_monitor_event, {set, {{media, Key}, _Health, Details, Timestamp}}}, State) ->
	case proplists:get_value(queue, Details) of
		undefined ->
			{noreply, State};
		Queue ->
			io:format(State#state.file, "~s : ~s : call_enqueue : ~p : ~s : ~s : ~s : ~s : ~s : ~s : ~s~n", [
					inet_db:gethostname(),
					%Timestamp,
					iso8601_timestamp(Timestamp),
					proplists:get_value(node, Details),
					Queue,
					element(2, proplists:get_value(callerid, Details)),
					Key,
					"Origin Code",
					"CLS",
					"Source IP",
					proplists:get_value(dnis, Details, "")]),
			{noreply, State#state{callqueuemap = dict:store(Key, Queue, State#state.callqueuemap)}}
	end;
handle_info({cpx_monitor_event, {drop, {media, Key}, _Timestamp}}, State) ->
	% setting up a delay as there may be messages about agents that need the 
	% info.
	Self = self(),
	erlang:send_after(5000, Self, {redrop, {media, Key}}),
	{noreply, State};
handle_info({redrop, {media, Key}}, #state{callqueuemap = Callqmap, calls = Calls} = State) ->
	Newcalls = dict:erase(Key, Calls),
	Newmap = dict:erase(Key, Callqmap),
	{noreply, State#state{callqueuemap = Newmap, calls = Newcalls}};
handle_info(Info, State) ->
	%?NOTICE("Got message: ~p", [Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_Oldvsn, State, _Extra) ->
	{ok, State}.

agent_diff(Agent, New, Old, Timestamp, #state{file = File} = State) ->
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
						[io:format(File, "~s : ~s : agent_logout : ~p : ~s : ~s~n", [inet_db:gethostname(), iso8601_timestamp(Timestamp), proplists:get_value(node, New), proplists:get_value(login, New), Queue]) || {'_queue', Queue} <- Lost],
						[io:format(File, "~s : ~s : agent_login : ~p : ~s : ~s~n", [inet_db:gethostname(), iso8601_timestamp(Timestamp), proplists:get_value(node, New), proplists:get_value(login, New), Queue]) || {'_queue', Queue} <- Gained],
						ok
			end;
		false ->
			% state change!
			% what's the new state; if its oncall we just grabbed a call, if its released/idle we just finished one
			case {proplists:get_value(state, Old), proplists:get_value(state, New)} of
					{wrapup, _} ->
						Call = proplists:get_value(statedata, Old),
						Queue = case dict:find(Call#call.id, State#state.callqueuemap) of
							error -> "Unknown Queue";
							{ok, Value} -> Value
						end,

						io:format(File, "~s : ~s : call_terminate : ~p : ~s : ~s : ~s : ~s : ~s : ~s : ~s : ~s~n", [
								inet_db:gethostname(),
								%util:now(),
								iso8601_timestamp(Timestamp),
								proplists:get_value(node, New),
								Queue,
								proplists:get_value(login, New),
								element(2, Call#call.callerid),
								Call#call.id,
								"Origin",
								"CLS",
								"Source IP",
								Call#call.dnis
							]);
					{_, oncall} ->
						Call = proplists:get_value(statedata, New),
						Queue = case dict:find(Call#call.id, State#state.callqueuemap) of
							error -> "Unknown Queue";
							{ok, Value} -> Value
						end,

						io:format(File, "~s : ~s : call_pickup : ~p : ~s : ~s : ~s : ~s : ~s : ~s : ~s : ~s~n", [
								inet_db:gethostname(),
								%util:now(),
								iso8601_timestamp(Timestamp),
								proplists:get_value(node, New),
								Queue,
								proplists:get_value(login, New),
								element(2, Call#call.callerid),
								Call#call.id,
								"Origin",
								"CLS",
								"Source IP",
								Call#call.dnis
							]);
					{idle, released} ->
						[io:format(State#state.file, "~s : ~s : agent_unavailable : ~p : ~s : ~s~n", [inet_db:gethostname(), iso8601_timestamp(Timestamp), proplists:get_value(node, New), proplists:get_value(login, New), Queue]) || {'_queue', Queue} <- proplists:get_value(skills, New)];
					{released, idle} ->
						[io:format(State#state.file, "~s : ~s : agent_available : ~p : ~s : ~s~n", [inet_db:gethostname(), iso8601_timestamp(Timestamp), proplists:get_value(node, New), proplists:get_value(login, New), Queue]) || {'_queue', Queue} <- proplists:get_value(skills, New)];
					{_, _} ->
						ok
				end
		end.

% generates time stamps like 2010-07-29T12:31:02.776357Z according to ISO8601
iso8601_timestamp() ->
	iso8601_timestamp(os:timestamp()).

iso8601_timestamp(Now) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_universal_time(Now),
	{_, _, Microseconds} = Now,
	%Milliseconds = Microseconds div 1000,
	lists:flatten(io_lib:format("~B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~BZ", [Year, Month, Day, Hour, Minute, Second, Microseconds])).

init_call_queue_map() ->
	Queuelist = queue_manager:queues(),
	map_call_to_queue(Queuelist, dict:new()).
	
map_call_to_queue([], Dict) ->
	Dict;
map_call_to_queue([{Name, Pid} | Tail], Dict) ->
	Calls = call_queue:get_calls(Pid),
	Newdict = map_call_to_queue(Calls, Name, Dict),
	map_call_to_queue(Tail, Newdict).
	
map_call_to_queue([], _Name, Dict) ->
	Dict;
map_call_to_queue([{_Key, #queued_call{id = Id} = _Media} | Tail], Name, Dict) ->
	map_call_to_queue(Tail, Name, dict:store(Id, Name, Dict)).


