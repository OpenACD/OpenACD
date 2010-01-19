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
%%	The Original Code is OpenACD.
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
%%	Andrew Thompson <andrew at hijacked dot us>
%%	Micah Warren <micahw at fusedsolutions dot com>
%%

%% @doc The application module.
-module(cpx).
-author("Micah").

-behaviour(application).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("queue.hrl").
-include("cpx.hrl").

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
-endif.

% behavior cbs.
-export([start/2, prep_stop/1, stop/1]).

% helper funcs
-export([
	agent_state/1,
	agent_states/1,
	call_state/1,	
	get_queue/1,
	get_agent/1,
	get_agents/0,
	get_agents/1,
	get_queues/0,
	get_queues/1,
	get_media/1,
	kick_agent/1,
	kick_call/1,
	kick_media/1,
	is_running/1,
	help/0,
	media_state/1,
	uptime/0,
	uptime/1
]).

-spec(start/2 :: (Type :: 'normal' | {'takeover', atom()} | {'failover', atom()}, StartArgs :: [any()]) -> {'ok', pid(), any()} | {'ok', pid()} | {'error', any()}).
start(_Type, StartArgs) ->
	io:format("Start args ~p~n", [StartArgs]),
	io:format("All env: ~p~n", [application:get_all_env(cpx)]),
	crypto:start(),
	%Nodes = lists:append([nodes(), [node()]]),
	%mnesia:create_schema(Nodes),
	%mnesia:start(),
	case application:get_env(cpx, nodes) of
		{ok, Nodes} ->
			lists:foreach(fun(Node) -> net_adm:ping(Node) end, Nodes),
			case nodes() of
				[] ->
					ok;
				AliveNodes ->
					io:format("Alive nodes: ~p~n", [AliveNodes]),
					mnesia:change_config(extra_db_nodes, AliveNodes)
			end,
			ok;
		_Else ->
			Nodes = [node()]
	end,
	mnesia:change_table_copy_type(schema, node(), disc_copies),
	mnesia:set_master_nodes(lists:umerge(Nodes, [node()])),
	try cpx_supervisor:start_link(Nodes) of
		{ok, Pid} ->
			application:set_env(cpx, uptime, util:now()),
			?NOTICE("Application cpx started sucessfully!", []),
			{ok, Pid}
	catch
		What:Why ->
			?ERROR("Application cpx failed to start successfully! ~p:~p", [What, Why]),
			{What, Why}
	end.

-spec(prep_stop/1 :: (State :: any()) -> any()).
prep_stop(State) ->
	?NOTICE("Application cpx stopping...", []),
	State.

-spec(stop/1 :: (State :: any()) -> 'ok').
stop(_State) ->
	application:unset_env(cpx, uptime),
	ok.

%% =====
%% helper funcs
%% =====

-spec(get_queue/1 :: (Queue :: string()) -> pid() | 'none').
get_queue(Queue) ->
	queue_manager:get_queue(Queue).

-spec(get_agent/1 :: (Agent :: string()) -> pid() | 'none').
get_agent(Agent) ->
	case agent_manager:query_agent(Agent) of
		{true, Pid} ->
			Pid;
		false ->
			none
	end.

-spec(get_agents/0 :: () -> [{string(), pid()}]).
get_agents() ->
	agent_manager:list().

-spec(get_agents/1 :: (Profile :: string()) -> [{string(), pid()}]).
get_agents(Profile) ->
	Agents = agent_manager:list(),
	Fun = fun({_Login, Pid}) ->
		case agent:dump_state(Pid) of
			#agent{profile = Profile} ->
				true;
			_ ->
				false
		end
	end,
	lists:filter(Fun, Agents).

-spec(get_queues/0 :: () -> [{string(), pid()}]).
get_queues() ->
	queue_manager:queues().

-spec(get_queues/1 :: (Group :: string()) -> [{string(), pid()}]).
get_queues(Group) ->
	Queues = get_queues(),
	StashedQueues = call_queue_config:get_queues(Group),
	Compare = lists:map(fun(#call_queue{name = Nom}) -> Nom end, StashedQueues),
	Fun = fun({Name, _Pid}) ->
		lists:member(Name, Compare)
	end,
	lists:filter(Fun, Queues).

-spec(get_media/1 :: (IsPid :: string() | pid()) -> pid() | 'none').
get_media(Pid) when is_pid(Pid) ->
	%% There's prolly something else I was supposed to do...
	Pid;
get_media(LPid) ->
	try list_to_pid(LPid) of
		Pid ->
			get_media(Pid)
	catch
		error:badarg ->
			%% time to ask the queues and the agents.
			Queues = get_queues(),
			case get_media_queues(Queues, LPid) of
				none ->
					Agents = get_agents(),
					case get_media_agents(Agents, LPid) of
						none ->
							none;
						Pid ->
							get_media(Pid)
					end;
				Pid ->
					get_media(Pid)
			end
	end.
	

get_media_queues([], _Callref) ->
	none;
get_media_queues([{Qnom, Queue} | Tail], Callref) ->
	Calls = call_queue:to_list(Queue),
	case get_media_queues_medias(Calls, Callref) of
		none ->
			get_media_queues(Tail, Callref);
		Pid ->
			Pid
	end.

get_media_queues_medias([], _Callref) ->
	none;
get_media_queues_medias([QueuedCall | Tail],  Callref) ->
	case QueuedCall#queued_call.id of
		Callref ->
			QueuedCall#queued_call.media;
		_ ->
			get_media_queues_medias(Tail, Callref)
	end.

get_media_agents([], _Callref) ->
	none;
get_media_agents([{_Login, Pid} | Tail], Callref) ->
	#agent{statedata = State} = agent:dump_state(Pid),
	case State of
		#call{id = Callref} ->
			State#call.source;
		_ ->
			get_media_agents(Tail, Callref)
	end.

-spec(kick_agent/1 :: (AgentRef :: string() | pid()) -> 'ok' | 'none').
kick_agent(Pid) when is_pid(Pid) ->
	exit(Pid, kill),
	ok;
kick_agent(List) ->
	try list_to_pid(List) of
		Pid ->
			kick_agent(Pid)
	catch
		error:badarg ->
			case get_agent(List) of
				none ->
					none;
				Pid ->
					kick_agent(Pid)
			end
	end.

-spec(kick_call/1 :: (Callref :: string() | pid()) -> 'ok' | 'none').
kick_call(Pid) when is_pid(Pid) ->
	exit(Pid, kill),
	ok;
kick_call(Callref) ->
	try list_to_pid(Callref) of
		Pid ->
			kick_call(Pid)
	catch
		error:badarg ->
			case get_media(Callref) of
				none ->
					none;
				Pid ->
					kick_call(Pid)
			end
	end.

-spec(kick_media/1 :: (Mediaref :: string() | pid()) -> 'ok' | 'none').
kick_media(Mediaref) ->
	kick_call(Mediaref).

-spec(is_running/1 :: (Specid :: atom) -> pid() | 'stopped' | 'noexists').
is_running(Specid) ->
	case cpx_supervisor:get_conf(Specid) of
		undefined ->
			noexists;
		#cpx_conf{supervisor = Super} ->
			case supervisor:which_children(Super) of
				[] ->
					stopped;
				Kids ->
					is_running(Kids, Specid)
			end
	end.

is_running([], _Specid) ->
	stopped;
is_running([Head | _Tail], Specid) when element(1, Head) == Specid ->
	element(2, Head);
is_running([_ | Tail], Specid) ->
	is_running(Tail, Specid).

-spec(help/0 :: () -> 'ok').
help() ->
	{ok, Bin} = file:read_file("include/cpxhelp.txt"),
	io:format("~s", [Bin]),
	ok.

-spec(agent_states/0 :: () -> [{string(), string(), atom()}]).
agent_states() ->
	agent_states(any).

-spec(agent_states/1 :: (Profiles :: 'any' | string() | [string()]) -> [{string(), string(), atom()}]).
agent_states(RawProfiles) ->
	Agentlist = agent_manager:list(),
	Profiles = case RawProfiles of
		any ->
			any;
		[H | _] when is_list(H) ->
			RawProfiles;
		_ ->
			[RawProfiles]
	end,
	Fold = fun({Login, {Pid, Id}}, Acc) ->
		Astate = agent:dump_state(Pid),
		case in_list(Astate#agent.profile, Profiles) of
			true ->
				[{Login, Id, Astate#agent.state} | Acc];
			false ->
				Acc
		end
	end,
	lists:fold(Fold, [], Agentlist).
	
in_list(_Value, any) ->
	true;
in_list(Value, List) when is_list(List) ->
	lists:member(Value, List).

-spec(agent_state/1 :: (Agent :: any()) -> 'ok').
agent_state(Agent) ->
	case get_agent(Agent) of
		none ->
			io:format("No such agent"),
			ok;
		Pid ->
			State = agent:dump_state(Pid),
			Print = [
				{"Id", State#agent.id}, 
				{"Pid", Pid},
				{"Profile", State#agent.profile},
				{"State", State#agent.state},
				{"Previous", State#agent.oldstate},
				{"Changed", State#agent.lastchange},
				{"Queued Release", case State#agent.queuedrelease of
					default ->
						"default";
					{Name, _, _} ->
						Name;
					_ ->
						false
				end}
			],
			pretty_print(Print),
			ok
	end.

pretty_print(List) ->
	FindLongest = fun({K, _}, Length) ->
		case length(K) > Length of
			true ->
				length(K);
			false ->
				Length
		end
	end,
	Longest = lists:foldl(FindLongest, 0, List) + 1,
	{Io, Params} = build_io_and_params(List, Longest, "", []),
	io:format(Io, Params).

build_io_and_params([], _, Io, Params) ->
	{Io, lists:reverse(Params)};
build_io_and_params([{Key, Value} | Tail], Length, Io, Params) ->
	Newio = lists:append([Io, string:right(Key, Length), ":  ~p~n"]),
	Newparams = [Value | Params],
	build_io_and_params(Tail, Length, Newio, Newparams).

-spec(call_state/1 :: (Media :: any()) -> 'ok').
call_state(Media) ->
	media_state(Media).

-spec(media_state/1 :: (Media :: any()) -> 'ok').
media_state(Media) ->
	case get_media(Media) of
		none ->
			io:format("No such media"),
			ok;
		Pid ->
			Call = gen_media:get_call(Pid),
			io:format("State of call ~p~n", [Media]),
			Print = [
				{"Id", Call#call.id},
				{"Source", Call#call.source},
				{"Type", Call#call.type},
				{"Callerid", Call#call.callerid},
				{"Client", case Call#call.client of
					undefined ->
						"Not yet set";
					Client ->
						Client#client.label
				end},
				{"Cook", Call#call.cook},
				{"Bound Dispatchers", Call#call.bound},
				{"Direction", Call#call.direction},
				{"Priority", Call#call.priority},
				{"Ring Path", Call#call.ring_path},
				{"Media Path", Call#call.media_path}
			],
			pretty_print(Print),
			ok
	end.

-spec(uptime/0 :: () -> non_neg_integer() | 'stopped').
uptime() ->
	uptime(false).

-spec(uptime/1 :: (Fallback :: boolean()) -> non_neg_integer() | 'stopped').
uptime(Fallback) ->
	Apps = application:which_applications(),
	Fun = fun({cpx, _, _}) -> true; (_) -> false end,
	Running = lists:any(Fun, Apps),
	case Running of
		false -> 
			stopped;
		true ->
			case {application:get_env(cpx, uptime), Fallback} of
				{undefined, false} ->
					io:format("The uptime is not available for this node.~nYou can call cpx:uptime(true) to set the uptime to now~n"),
					0;
				{undefined, true} ->
					io:format("The uptime was not available, so resetting it as requested~n"),
					Now = util:now(),
					application:set_env(cpx, uptime, Now),
					0;
				{{ok, Time}, _} ->
					Out = util:now() - Time,
					io:format("~s~n", [pretty_print_time(Out)]),
					Out
			end
	end.

pretty_print_time(Time) ->
	Times = [
		{"Seconds", 60},
		{"Minutes", 60},
		{"Hours", 24},
		{"Days", 7},
		{"Weeks", 52},
		"Years"
	],
	pretty_print_time(Times, Time, "").

pretty_print_time(_, 0, Acc) ->
	Acc;
pretty_print_time([Label], Time, Acc) ->
	lists:append([integer_to_list(Time), " ", Label, " ", Acc]);
pretty_print_time([{Label, Interval} | Tail], Time, Acc) ->
	Rem = Time rem Interval,
	Newtime = util:floor(Time / Interval),
	Newacc = lists:append([integer_to_list(Rem), " ", Label, " ", Acc]),
	pretty_print_time(Tail, Newtime, Newacc).

% to be added soon TODO
%
%can_answer/2 (Media, Agent) -> true | missing skills
% queue_state -> pretty print the queue state
% media_states/0, /1 -> print a summary of medias (limited by type)
% queue_states/0, /1 -> print a summary of queue states (limited by type).
% migrate_queue/1 -> for the future.
% cdr:orphan_search
% cdr:pending_states (agent/cdr), global/node
% start_spec pretty print.
% cpx:start_spec
% uptime

-ifdef(TEST).

pretty_print_time_test_() ->
	[{"A few seconds",
	?_assertEqual("5 Seconds ", pretty_print_time(5))},
	{"A couple of minutes",
	?_assertEqual("2 Minutes 23 Seconds ", pretty_print_time(120 + 23))},
	{"A few hours",
	?_assertEqual("12 Hours 32 Minutes 54 Seconds ", pretty_print_time((12 * 60 * 60) + (32 * 60) + 54))},
	{"Several days",
	?_assertEqual("3 Days 0 Hours 27 Minutes 10 Seconds ", pretty_print_time((3 * 24 * 60 * 60) + (60 * 27) + 10))},
	{"Many weeks",
	?_assertEqual("32 Weeks 4 Days 10 Hours 3 Minutes 7 Seconds ", pretty_print_time((32 * 7 * 24 * 60 * 60) + (4 * 24 * 60 * 60) + (10 * 60 * 60) + (3 * 60) + 7))},
	{"Years",
	?_assertEqual("3 Years 5 Weeks 6 Days 21 Hours 53 Minutes 3 Seconds ", pretty_print_time((3 * 52 * 7 * 24 * 60 * 60) + (5 * 7 * 24 * 60 * 60) + (6 * 24 * 60 * 60) + (60 * 60 * 21) + (53 * 60) + 3))}].

-endif.
