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
-include_lib("stdlib/include/qlc.hrl").
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
		callqueuemap,
		callagentmap
	}).


start() ->
	start([]).

start(Props) ->
	gen_server:start({local, ?MODULE}, ?MODULE, Props, []).

start_link() ->
	start_link([]).

start_link(Props) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Props, []).

init(Props) ->
	Filename = proplists:get_value(filename, Props, "events.log"),
	case file:open(Filename, [append]) of
			{ok, File} ->
				inet_config:do_load_resolv(os:type(), longnames),
				Agents = qlc:e(qlc:q([{Key, Details} || {{agent, Key}, Details, _, _, _, _} = X <- ets:table(cpx_monitor)])),
				Calls = qlc:e(qlc:q([{Key, Details} || {{media, Key}, Details, _, _, _, _} = X <- ets:table(cpx_monitor)])),
				AgentDict = dict:from_list(Agents),
				CallDict = dict:from_list(Calls),
				% hopefully the below won't take too long if this module is started on a busy system.
				CallQMap = init_call_queue_map(),
				CallAgentMap = dict:new(), % TODO bootstrap this to avoid false positives on abandon
				cpx_monitor:subscribe(),
				{ok, #state{file = File, agents = AgentDict, calls = CallDict, callqueuemap = CallQMap, callagentmap = CallAgentMap}};
			{error, Reason} ->
				{stop, {error, Reason}}
	end.

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State, hibernate}.

handle_cast(_Msg, State) ->
	{noreply, State, hibernate}.

handle_info({cpx_monitor_event, {set, Timestamp, {{agent, Key}, Details, _Node}}}, State) ->
	case dict:find(Key, State#state.agents) of
		error ->
			%?NOTICE("Agent ~p just logged in ~p", [Key, Details]),
			log_event(State#state.file, "agent_start", Timestamp, proplists:get_value(node, Details), [proplists:get_value(login, Details)]),
			[log_event(State#state.file, "agent_login", Timestamp, proplists:get_value(node, Details), [proplists:get_value(login, Details), Queue]) || {'_queue', Queue} <- proplists:get_value(skills, Details)],
			?INFO("skills: ~p", [proplists:get_value(skills, Details)]),
			ok;
		{ok, Current} ->
			%?NOTICE("Udating agent ~p from  ~p to ~p", [Key, Current, Details]),
			agent_diff(Key, Details, Current, Timestamp, State)
	end,
	case {proplists:get_value(statedata, Details), proplists:get_value(state, Details)} of
		{X, Y} when is_record(X, call), Y =/= ringing ->
			% ok, an agent is definitely interacting with this call
			{noreply, State#state{agents = dict:store(Key, Details, State#state.agents), callagentmap = dict:store(X#call.id, Key, State#state.callagentmap)}};
		_ ->
			{noreply, State#state{agents = dict:store(Key, Details, State#state.agents)}}
	end;
handle_info({cpx_monitor_event, {drop, Timestamp, {agent, Key}}}, State) ->
	case dict:find(Key, State#state.agents) of
		error ->
			{noreply, State};
		{ok, Current} ->
			log_event(State#state.file, "agent_stop", Timestamp, proplists:get_value(node, Current), [proplists:get_value(login, Current)]),
			[log_event(State#state.file, "agent_logout", Timestamp, proplists:get_value(node, Current), [proplists:get_value(login, Current), Queue]) || {'_queue', Queue} <- proplists:get_value(skills, Current)],
			{noreply, State#state{agents = dict:erase(Key, State#state.agents)}}
	end;
handle_info({cpx_monitor_event, {set, Timestamp, {{media, Key}, Details, _Node}}}, State) ->
	case proplists:get_value(queue, Details) of
		undefined ->
			{noreply, State#state{calls = dict:store(Key, Details, State#state.calls)}};
		Queue ->
			log_event(State#state.file, "call_enqueue", Timestamp, 
					proplists:get_value(node, Details), [
					Queue,
					element(2, proplists:get_value(callerid, Details)),
					Key,
					"Origin Code",
					"CLS",
					"Source IP",
					proplists:get_value(dnis, Details, "")]),
			{noreply, State#state{callqueuemap = dict:store(Key, Queue, State#state.callqueuemap), calls = dict:store(Key, Details, State#state.calls)}}
	end;
handle_info({cpx_monitor_event, {drop, Timestamp, {media, Key}}}, #state{file = File} = State) ->
	% setting up a delay as there may be messages about agents that need the 
	% info.
	case dict:find(Key, State#state.callagentmap) of
		error -> % An abandonment!
			Queue = case dict:find(Key, State#state.callqueuemap) of
				error -> "Unknown Queue";
				{ok, Value} -> Value
			end,

			case dict:find(Key, State#state.calls) of
				{ok, New} ->
					?INFO("~p abandoned", [Key]),
					log_event(File, "call_terminate", Timestamp, 
							proplists:get_value(node, New), [
							Queue,
							"", %proplists:get_value(login, New),
							element(2, proplists:get_value(callerid, New)),
							Key,
							"Origin",
							"CLS",
							"Source IP",
							proplists:get_value(dnis, New, "")
						]);
				error ->
					?ERROR("unknown call ~p abandoned", [Key])
			end;
		{ok, _Agent} ->
			ok
	end,
	Self = self(),
	erlang:send_after(5000, Self, {redrop, {media, Key}}),
	{noreply, State#state{calls = dict:erase(Key, State#state.calls)}};
handle_info({redrop, {media, Key}}, #state{callqueuemap = Callqmap, callagentmap = CallAgentMap, calls = Calls} = State) ->
	Newcalls = dict:erase(Key, Calls),
	Newcmap = dict:erase(Key, Callqmap),
	Newamap = dict:erase(Key, CallAgentMap),
	{noreply, State#state{callqueuemap = Newcmap, callagentmap = Newamap, calls = Newcalls}};
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
						[log_event(File, "agent_logout", Timestamp, proplists:get_value(node, New), [proplists:get_value(login, New), Queue]) || {'_queue', Queue} <- Lost],
						[log_event(File, "agent_login", Timestamp, proplists:get_value(node, New), [proplists:get_value(login, New), Queue]) || {'_queue', Queue} <- Gained],
						ok
			end;
		false ->
			% state change!
			% what's the new state; if its oncall we just grabbed a call, if its released/idle we just finished one
			case {proplists:get_value(state, Old), proplists:get_value(state, New)} of
					{_, wrapup} ->
						Call = proplists:get_value(statedata, Old),
						Queue = case dict:find(Call#call.id, State#state.callqueuemap) of
							error -> "Unknown Queue";
							{ok, Value} -> Value
						end,

						log_event(File, "call_terminate", Timestamp, 
								proplists:get_value(node, New), [
								Queue,
								proplists:get_value(login, New),
								element(2, Call#call.callerid),
								Call#call.id,
								"Origin",
								"CLS",
								"Source IP",
								Call#call.dnis
							]);
					{wrapup, _} ->
						Call = proplists:get_value(statedata, Old),
						Queue = case dict:find(Call#call.id, State#state.callqueuemap) of
							error -> "Unknown Queue";
							{ok, Value} -> Value
						end,

						log_event(File, "call_complete", Timestamp, 
								proplists:get_value(node, New), [
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

						log_event(File, "call_pickup", Timestamp, 
								proplists:get_value(node, New), [
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
						[log_event(State#state.file, "agent_unavailable", Timestamp, proplists:get_value(node, New), [proplists:get_value(login, New), Queue]) || {'_queue', Queue} <- proplists:get_value(skills, New)];
					{released, idle} ->
						[log_event(State#state.file, "agent_available", Timestamp, proplists:get_value(node, New), [proplists:get_value(login, New), Queue]) || {'_queue', Queue} <- proplists:get_value(skills, New)];
					{_, _} ->
						ok
				end
		end.

-spec monotonic_counter() -> float().
monotonic_counter() ->
	{MegaSeconds, Seconds, MicroSeconds} = erlang:now(),
	MegaSeconds * 1000000 + Seconds + MicroSeconds / 1000000.

log_event(File, Event, Timestamp, Node, Args) ->
	FormatString = "~f : ~s : ~s : ~s : ~p : " ++ string:join([ "~s" || _ <- lists:seq(1, length(Args)) ], " : ") ++ "~n",
	AllArgs = [monotonic_counter(), get_FQDN(), iso8601_timestamp(Timestamp), Event, Node] ++ Args,
	io:format(File, FormatString, AllArgs).

% generates time stamps like 2010-07-29T12:31:02.776357Z according to ISO8601
-spec iso8601_timestamp() -> string().
iso8601_timestamp() ->
	iso8601_timestamp(os:timestamp()).

-spec iso8601_timestamp(Now :: {integer(), integer(), integer()}) -> string().
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

get_FQDN() ->
	lists:flatten([inet_db:gethostname(),".",inet_db:res_option(domain)]).

