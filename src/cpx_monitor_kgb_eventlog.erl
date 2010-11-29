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

%% @doc Generates an eventlog in a format approaching a legacy format used 
%% by KGB. Most of the records here are based on documentation provided by
%% kgb.

-module(cpx_monitor_kgb_eventlog).

-behaviour(gen_server).

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/file.hrl").
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

-type(agent_event() ::
	'agent_login' |
	'agent_logout' |
	'agent_start' |
	'agent_stop' |
	'agent_available' |
	'agent_unavailable'
).

-record(agent_event, {
	uuid = monotonic_counter() :: float(),
	hostname,
	timestamp,
	event_type :: agent_event(),
	agent_id :: string(),
	queue_name :: 'undefined' | string()
}).

-record(call_enqueue_event, {
	uuid = monotonic_counter() :: float(),
	hostname,
	timestamp,
	event_type = call_enqueue,
	node,
	queue,
	from_header,
	call_id,
	origin_code = "Origin",
	cls = "CLS",
	source_ip = "Source IP",
	did_number
}).

% =====
% API
% =====

start() ->
	start([]).

start(Props) ->
	gen_server:start({local, ?MODULE}, ?MODULE, Props, []).

start_link() ->
	start_link([]).

start_link(Props) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Props, []).

% =====
% init 
% =====
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
				cpx_monitor:subscribe(fun subscription_filter/1),
				{ok, FileInfo} = file:read_file_info(Filename),
				{ok, #state{file = {File, Filename, FileInfo#file_info.inode}, agents = AgentDict, calls = CallDict, callqueuemap = CallQMap, callagentmap = CallAgentMap}};
			{error, Reason} ->
				?WARNING("~p failed to start due to error:~p", [?MODULE, Reason]),
				{stop, {error, Reason}}
	end.

% =====
% Call
% =====
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State, hibernate}.

% =====
% Cast
% =====
handle_cast(_Msg, State) ->
	{noreply, State, hibernate}.

% =====
% Info
% =====
handle_info({cpx_monitor_event, {set, Timestamp, {{agent, Key}, Details, _Node}}}, State) ->
	NewFile = check_file(State#state.file),
	case dict:find(Key, State#state.agents) of
		error ->
			%?NOTICE("Agent ~p just logged in ~p", [Key, Details]),
			log_event(NewFile, "agent_start", Timestamp, proplists:get_value(node, Details), [proplists:get_value(login, Details)]),
			[log_event(NewFile, "agent_login", Timestamp, proplists:get_value(node, Details), [proplists:get_value(login, Details), Queue]) || {'_queue', Queue} <- proplists:get_value(skills, Details)],
			?INFO("skills: ~p", [proplists:get_value(skills, Details)]),
			ok;
		{ok, Current} ->
			%?NOTICE("Udating agent ~p from  ~p to ~p", [Key, Current, Details]),
			agent_diff(Key, Details, Current, Timestamp, State#state{file = NewFile})
	end,
	case {proplists:get_value(statedata, Details), proplists:get_value(state, Details)} of
		{X, Y} when is_record(X, call), Y =/= ringing ->
			% ok, an agent is definitely interacting with this call
			{noreply, State#state{file = NewFile, agents = dict:store(Key, Details, State#state.agents), callagentmap = dict:store(X#call.id, Key, State#state.callagentmap)}};
		_ ->
			{noreply, State#state{file = NewFile, agents = dict:store(Key, Details, State#state.agents)}}
	end;
handle_info({cpx_monitor_event, {drop, Timestamp, {agent, Key}}}, State) ->
	NewFile = check_file(State#state.file),
	case dict:find(Key, State#state.agents) of
		error ->
			{noreply, State#state{file = NewFile}};
		{ok, Current} ->
			log_event(NewFile, "agent_stop", Timestamp, proplists:get_value(node, Current), [proplists:get_value(login, Current)]),
			[log_event(NewFile, "agent_logout", Timestamp, proplists:get_value(node, Current), [proplists:get_value(login, Current), Queue]) || {'_queue', Queue} <- proplists:get_value(skills, Current)],
			{noreply, State#state{agents = dict:erase(Key, State#state.agents)}}
	end;
handle_info({cpx_monitor_event, {set, Timestamp, {{media, Key}, Details, _Node}}}, State) ->
	NewFile = check_file(State#state.file),
	case proplists:get_value(queue, Details) of
		undefined ->
			{noreply, State#state{file = NewFile, calls = dict:store(Key, Details, State#state.calls)}};
		Queue ->
			log_event(NewFile, "call_enqueue", Timestamp, 
					proplists:get_value(node, Details), [
					Queue,
					element(2, proplists:get_value(callerid, Details)),
					Key,
					"Origin Code",
					"CLS",
					"Source IP",
					proplists:get_value(dnis, Details, "")]),
			{noreply, State#state{file = NewFile, callqueuemap = dict:store(Key, Queue, State#state.callqueuemap), calls = dict:store(Key, Details, State#state.calls)}}
	end;
handle_info({cpx_monitor_event, {drop, Timestamp, {media, Key}}}, #state{file = File} = State) ->
	% setting up a delay as there may be messages about agents that need the 
	% info.
	NewFile = check_file(File),
	case dict:find(Key, State#state.callagentmap) of
		error -> % An abandonment!
			Queue = case dict:find(Key, State#state.callqueuemap) of
				error -> "Unknown Queue";
				{ok, Value} -> Value
			end,

			case dict:find(Key, State#state.calls) of
				{ok, New} ->
					?INFO("~p abandoned", [Key]),
					log_event(NewFile, "call_terminate", Timestamp, 
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
	{noreply, State#state{file = NewFile, calls = dict:erase(Key, State#state.calls)}};
handle_info({redrop, {media, Key}}, #state{callqueuemap = Callqmap, callagentmap = CallAgentMap, calls = Calls} = State) ->
	Newcalls = dict:erase(Key, Calls),
	Newcmap = dict:erase(Key, Callqmap),
	Newamap = dict:erase(Key, CallAgentMap),
	{noreply, State#state{callqueuemap = Newcmap, callagentmap = Newamap, calls = Newcalls}};
handle_info(Info, State) ->
	%?NOTICE("Got message: ~p", [Info]),
	{noreply, State}.

% =====
% Terminate
% =====
terminate(_Reason, _State) ->
	ok.

code_change(_Oldvsn, State, _Extra) ->
	{ok, State}.

% =====
% Internal Functions
% =====

subscription_filter({set, _, {{agent, _}, Details, _}}) ->
	case proplists:get_value(skills, Details) of
		undefined ->
			false;
		[] ->
			false;
		Skills ->
			case length([X || {'_queue', X} <- Skills]) of
				0 ->
					false;
				_ ->
					true
			end
	end;
subscription_filter({set, _, {{media, _}, _, _}}) ->
	true;
subscription_filter({drop, _, _}) ->
	true;
subscription_filter(_) ->
	false.

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
					{wrapup, NextState} ->
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
							]),
						case NextState of
							idle ->
								[log_event(State#state.file, "agent_available", Timestamp, proplists:get_value(node, New), [proplists:get_value(login, New), Queue]) || {'_queue', Queue} <- proplists:get_value(skills, New)];
							_ ->
								ok
						end;	
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

log_event({File, _Name, Inode}, Event, Timestamp, Node, Args) ->
	FormatString = "~f : ~s : ~s : ~s : ~p : " ++ string:join([ "~s" || _ <- lists:seq(1, length(Args)) ], " : ") ++ "~n",
	AllArgs = [monotonic_counter(), get_FQDN(), iso8601_timestamp(Timestamp), Event, Node] ++ Args,
	io:format(File, FormatString, AllArgs).

check_file({FileHandle, FileName, FileInode}) ->
	?DEBUG("Checking file ~s", [FileName]),
	{Fhandle, Finode} = case file:read_file_info(FileName) of
		{ok, #file_info{inode = FileInode} = _Fileinfo} ->
	 		?DEBUG("file ~s say's it's cool", [FileName]),
			{FileHandle, FileInode};
		{ok, #file_info{inode = NewInode} = Fileinfo} ->
			case file:open(FileName, [append]) of
				{ok, NewHandle} ->
					?DEBUG("Gots myself a new file inode.", []),
					{NewHandle, NewInode};
				_ ->
					?ERROR("Could not re-open disappeared file ~s", [FileName]),
					exit(nofile)
			end;
		{error, enoent} ->
			?DEBUG("Looks like the file ~s was removed", [FileName]),
			{ok, NewHandle} = file:open(FileName, [append]),
			{ok, #file_info{inode = NewInode}} = file:read_file_info(FileName),
			{NewHandle, NewInode};
		Else ->
			?WARNING("Could not ensure events file ~s exists (~p).", [FileName, Else])
	end,
	{Fhandle, FileName, Finode}.

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

-ifdef(TEST).

-define(test_file, "event.test.log").

file_handling_test_() ->
	{foreach,
	fun() ->
		file:delete(?test_file),
		Ets = ets:new(cpx_monitor, [named_table]),
		{ok, Qm} = gen_leader_mock:start(queue_manager),
		gen_leader_mock:expect_leader_call(Qm, fun(_, _, State, _) -> 
			{ok, [], State}
		end),
		{ok, EventLog} = start([{filename, ?test_file}]),
		{EventLog, Ets}
	end,
	fun({EventLog, Ets}) ->
		exit(EventLog, kill),
		gen_leader_mock:stop(whereis(queue_manager)),
		file:delete(?test_file),
		ets:delete(Ets),
		timer:sleep(10) % letting everything die...
	end,
	[fun({EventLog, _}) -> {"removal and touch", fun() ->
		EventLog ! {cpx_monitor_event, {set, erlang:now(), {{agent, "agent"}, [{login, "agent"}, {skills, []}], node()}}},
		file:delete(?test_file),
		{ok, File} = file:open(?test_file, [append]),
		file:close(File),
		EventLog ! {cpx_monitor_event, {drop, erlang:now(), {agent, "agent"}}},
		timer:sleep(5),
		{ok, Bin} = file:read_file(?test_file),
		?assertNot(<<>> == Bin)
	end} end,
	fun({EventLog, _}) -> {"straight removal", fun() ->
		EventLog ! {cpx_monitor_event, {set, erlang:now(), {{agent, "agent"}, [{login, "agent"}, {skills, []}], node()}}},
		file:delete(?test_file),
		timer:sleep(5),
		EventLog ! {cpx_monitor_event, {drop, erlang:now(), {agent, "agent"}}},
		timer:sleep(5),
		?assertMatch({ok, Bin}, file:read_file(?test_file))
	end} end]}.

-record(write_test_rec, {
	logpid,
	ets,
	localhost,
	node
}).

send_agent_set(Pid, drop) ->
	Pid ! {cpx_monitor_event, {drop, erlang:now(), {agent, "agent"}}};
send_agent_set(Pid, State) when is_atom(State) ->
	send_agent_set(Pid, [{state, State}]);
send_agent_set(Pid, Props) ->
	FullProps = [{login, "agentName"}, {skills, [{'_queue', "Queue"}]}, {node, node()} | Props],
	Pid ! {cpx_monitor_event, {set, erlang:now(), {{agent, "agent"}, FullProps, node()}}}.

send_media_set(Pid) ->
	send_media_set(Pid, []).

send_media_set(Pid, drop) ->
	Pid ! {cpx_monitor_event, {drop, erlang:now(), {media, "media"}}};
send_media_set(Pid, InProps) ->
	Props = [
		{callerid, "2318232979*12358497*112*9080"},
		{dnis, "9080"},
		{node, node()}
	],
	Pid ! {cpx_monitor_event, {set, erlang:now(), {{media, "media"}, Props, node()}}}.

%% Yes, these use ?assert(true).  The real test are the pattern matching.
%% because I want to use Rest, I'm doing the match directly rather than 2x.
%% someone more ambitious than I can change it.
formatting_test_() ->
	{foreach,
	fun() ->
		file:delete(?test_file),
		Ets = ets:new(cpx_monitor, [named_table]),
		{ok, Qm} = gen_leader_mock:start(queue_manager),
		gen_leader_mock:expect_leader_call(Qm, fun(_, _, State, _) -> 
			{ok, [], State}
		end),
		{ok, EventLog} = start([{filename, ?test_file}]),
		Localhost = list_to_binary(net_adm:localhost()),
		LocalhostSize = size(Localhost),
		Node = list_to_binary(atom_to_list(node())),
		NodeSize = size(Node),
		#write_test_rec{
			logpid = EventLog,
			ets = Ets,
			localhost = {Localhost, LocalhostSize},
			node = {Node, NodeSize}
		}
	end,
	fun(#write_test_rec{logpid = EventLog, ets = Ets} = _) ->
		exit(EventLog, kill),
		gen_leader_mock:stop(whereis(queue_manager)),
		%file:delete(?test_file),
		ets:delete(Ets),
		timer:sleep(10) % letting everything die...
	end,
	[fun(#write_test_rec{logpid = EventLog, localhost = {Lhost, LhSize}, node = {Node, NodeSize}} = _) -> {"An agent logs in", fun() ->
		send_agent_set(EventLog, released),
		timer:sleep(5),
		{ok, Bin} = file:read_file(?test_file),
		?DEBUG("Got bin:  ~p", [Bin]),
		<<_:17/binary, " : ", Lhost:LhSize/binary, " : ", 
		_:27/binary, " : agent_start : ", Node:NodeSize/binary, 
		" : agentName", _:1/binary, Rest/binary>> = Bin,
		?DEBUG("Rest:  ~p", [Rest]),
		<<_:17/binary, " : ", Lhost:LhSize/binary, " : ",
		_:27/binary, " : agent_login : ", Node:NodeSize/binary,
		" : agentName : Queue", _:1/binary>> = Rest,
		?assert(true)
	end} end,
	fun(#write_test_rec{logpid = EventLog, localhost = {Lhost, LhSize}, node = {Node, NodeSize}} = _) -> {"An agent goes available", fun() ->
		send_agent_set(EventLog, released),
		timer:sleep(5),
		file:delete(?test_file),
		send_agent_set(EventLog, idle),
		timer:sleep(5),
		{ok, Bin} = file:read_file(?test_file),
		?DEBUG("Got bin:  ~p", [Bin]),
		<<_:17/binary, " : ", Lhost:LhSize/binary, " : ",
		_:27/binary, " : agent_available : ", Node:NodeSize/binary,
		" : agentName : Queue", _:1/binary, Rest/binary>> = Bin,
		<<>> = Rest,
		?assert(true)
	end} end,
	fun(#write_test_rec{logpid = EventLog, localhost = {Lhost, LhSize}, node = {Node, NodeSize}} = _) -> {"An agent goes unavailable", fun() ->
		send_agent_set(EventLog, released),
		send_agent_set(EventLog, idle),
		timer:sleep(5),
		file:delete(?test_file),
		send_agent_set(EventLog, released),
		timer:sleep(5),
		{ok, Bin} = file:read_file(?test_file),
		?DEBUG("Got bin:  ~p", [Bin]),
		<<_:17/binary, " : ", Lhost:LhSize/binary, " : ",
		_:27/binary, " : agent_unavailable : ", Node:NodeSize/binary,
		" : agentName : Queue", _:1/binary, Rest/binary>> = Bin,
		<<>> = Rest,
		?assert(true)
	end} end,
	fun(#write_test_rec{logpid = EventLog, localhost = {Lhost, LhSize}, node = {Node, NodeSize}} = _) -> {"An agent logs out", fun() ->
		send_agent_set(EventLog, released),
		send_agent_set(EventLog, idle),
		timer:sleep(5),
		file:delete(?test_file),
		send_agent_set(EventLog, drop),
		timer:sleep(5),
		{ok, Bin} = file:read_file(?test_file),
		?DEBUG("Got bin:  ~p", [Bin]),
		<<_:17/binary, " : ", Lhost:LhSize/binary, " : ",
		_:27/binary, " : agent_stop : ", Node:NodeSize/binary,
		" : agentName", _:1/binary, Rest/binary>> = Bin,
		?DEBUG("Got rest:  ~p", [Rest]),
		<<_:17/binary, " : ", Lhost:LhSize/binary, " : ",
		_:27/binary, " : agent_logout : ", Node:NodeSize/binary,
		" : agentName : Queue", _:1/binary>> = Rest,
		?assert(true)
	end} end
	%fun(#write_test_rec{logpid = EventLog, localhost = {Lhost, LhSize}, node = {Node, NodeSize}} = _) -> {"a call enters queue", fun() ->
		]}.
		
-endif.
