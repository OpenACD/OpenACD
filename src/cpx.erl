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
	help/0
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
	agent_manager:to_list().

-spec(get_agents/1 :: (Profile :: string()) -> [{string(), pid()}]).
get_agents(Profile) ->
	Agents = agent_manager:to_list(),
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
					end
			end
	end.
	

get_media_queues([], _Callref) ->
	none;
get_media_queues([Queue | Tail], Callref) ->
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

% to be added soon TODO
%
%can_answer/2 (Media, Agent) -> true | missing skills
% agent_states/0, /1 -> print a summary of agent states (profile limitation)
% agent_state/1 -> pretty print the agent dump_state
% media_state -> pretty print the callrec
% queue_state -> pretty print the queue state
% media_states/0, /1 -> print a summary of medias (limited by type)
% queue_states/0, /1 -> print a summary of queue states (limited by type).
% migrate_queue/1 -> for the future.
% cdr:orphan_search
% cdr:pending_states (agent/cdr), global/node
% start_spec pretty print.
% cpx:start_spec
% uptime

