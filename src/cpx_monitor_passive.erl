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

%% @doc Module to spit a subset of the cpx_monitor data to an xml file 
%% periodically.
-module(cpx_monitor_passive).
-author(micahw).

-behaviour(gen_server).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("call.hrl").
-include("queue.hrl").
-include("agent.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(WRITE_INTERVAL, 60). % in seconds.
-define(DETS, passive_cache).

-type(xml_output() :: {xmlfile, string()}).
-type(queues() :: {queues, [string()]}).
-type(queue_groups() :: {queue_groups, [string()]}).
-type(agents() :: {agents, [string()]}).
-type(agent_profiles() :: {agent_profiles, [string()]}).
-type(output_filter() :: 
	xml_output() | 
	queues() | 
	queue_groups() | 
	agents() | 
	agent_profiles()).
-type(output_filters() :: [output_filter()]).
-type(output_name() :: string()).
-type(outputs() :: [{output_name(), output_filters()}]).
-type(outputs_option() :: {outputs, outputs()}).

-type(write_interval() :: {write_interval, pos_integer()}). % in seconds
-type(start_option() :: 
	outputs_option() |
	write_interval()).
	
-type(start_options() :: [start_option()]).

%% Dets data types
-type(dets_key() :: {'media' | 'agent', string()}).
-type(timestamp() :: integer()).
-type(health_data() :: [{atom(), any()}]).
-type(details() :: [{any(), any()}]).
-type(historical_key() :: {'inbound', 'queued'} | {'inbound', 'ivr'} | {'inbound', 'qabandoned'} | {'inbound', 'ivrabandoned'} | {'inbound', 'handled'} | 'outbound' | 'undefined').
-type(historical_tuple() :: {dets_key(), time(), health_data(), details(), historical_key()}).

%% API
-export([
	start_link/1,
	start/1,
	stop/0,
	write_output/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(filter_state, {
	state = [] :: [{dets_key(), {timestamp(), historical_key()}}],
	agent_profiles = [],
	queue_groups = [],
	clients = []
}).

-record(filter, {
	file_output :: string(),
	max_age = max :: 'max' | pos_integer(),
	queues :: [string()] | 'all',
	queue_groups :: [string()] | 'all',
	agents :: [string()] | 'all',
	agent_profiles :: [string()] | 'all',
	clients :: [string()] | 'all',
	nodes :: [atom()] | 'all',
	output_as = json :: 'json' | 'xml',
	state = #filter_state{}
}).

-record(state, {
	filters = [] :: [{string(), #filter{}}],
	interval = ?WRITE_INTERVAL :: pos_integer(),
	%agent_cache = [],
	%media_cache = [],
	timer :: any(),
	write_pids = [] :: [{pid(), string()}]
}).

%%====================================================================
%% API
%%====================================================================

-spec(start_link/1 :: (Options :: start_options()) -> {'ok', pid()}).
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

-spec(start/1 :: (Options :: start_options()) -> {'ok', pid()}).
start(Options) ->
	gen_server:start({local, ?MODULE}, ?MODULE, Options, []).

-spec(stop/0 :: () -> 'ok').
stop() ->
	gen_server:cast(?MODULE, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%--------------------------------------------------------------------
init(Options) ->
	process_flag(trap_exit, true),
	Subtest = fun(Message) ->
		%?DEBUG("passive sub testing message: ~p", [Message]),
		Type = case Message of
			{set, {{T, _}, _, _, _}} ->
				T;
			{drop, {T, _}} ->
				T
		end,
		case Type of
			media ->
				true;
			agent ->
				true;
			_ ->
				false
		end
	end,
	Interval = proplists:get_value(write_interval, Options, ?WRITE_INTERVAL) * 1000,
	Outputs = proplists:get_value(outputs, Options),
	Torec = fun({Name, Props}) ->
		Fileout = lists:append([proplists:get_value(file_output, Props, "."), "/", Name]),
		Filter = #filter{
			file_output = lists:append([Fileout, ".", atom_to_list(json)]),
			max_age = proplists:get_value(max_age, Props, max),
			queues = proplists:get_value(queues, Props, all),
			queue_groups = proplists:get_value(queue_groups, Props, all),
			agents = proplists:get_value(agents, Props, all),
			agent_profiles = proplists:get_value(agent_profiles, Props, all),
			clients = proplists:get_value(clients, Props, all),
			nodes = proplists:get_value(nodes, Props, all),
			output_as = json
		},
		{Name, Filter}
	end,
	Filters = lists:map(Torec, proplists:get_value(outputs, Options, [{"default", []}])),
	dets:open_file(?DETS, []),
	{ok, Agents} = cpx_monitor:get_health(agent),
	{ok, Medias} = cpx_monitor:get_health(media),
	cpx_monitor:subscribe(Subtest),
	lists:foreach(fun({K, H, D}) -> cache_event({set, {K, H, D, util:now()}}) end, lists:append(Agents, Medias)),
	%Agentcache = sort_agents(Agents),
	%Mediacache = create_queued_clients(Medias),
	{ok, Timer} = timer:send_after(Interval, write_output),
	?DEBUG("started", []),
	{ok, #state{
		filters = Filters,
		interval = Interval,
		timer = Timer
		%agent_cache = Agentcache,
		%media_cache = Mediacache
	}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_info(write_output, #state{filters = Filters} = State) ->
	?DEBUG("Writing output.", []),
	Qh = qlc:q([Key || {Key, Time, _Hp, _Details, _History} <- dets:table(?DETS), util:now() - Time > 86400]),
	Keys = qlc:e(Qh),
	lists:foreach(fun(K) -> dets:delete(?DETS, K) end, Keys),
	WritePids = lists:map(fun({Nom, _F} = Filter) ->
		Pid = spawn_link(?MODULE, write_output, [Filter]),
		{Pid, Nom}
	end, Filters),
	?DEBUG("das pids:  ~p", [WritePids]),
	Timer = erlang:send_after(State#state.interval, self(), write_output),
	{noreply, State#state{timer = Timer, write_pids = WritePids}};
handle_info({cpx_monitor_event, Event}, #state{filters = Filters} = State) ->
	Row = cache_event(Event),
	Newfilters = update_filter_states(Row, Filters),
	{noreply, State#state{filters = Newfilters}};
handle_info({'EXIT', Pid, Reason}, #state{write_pids = Pids} = State) ->
	case proplists:get_value(Pid, Pids) of
		undefined ->
			{noreply, State};
		Name ->
			case Reason of
				normal ->
					?DEBUG("output written for filter ~p", [Name]),
					ok;
				Else ->
					?WARNING("output write for ~p exited abnormally:  ~p", [Name, Reason]),
					ok
			end,
			Newlist = proplists:delete(Pid, Pids),
			{noreply, State#state{write_pids = Newlist}}
	end;
handle_info(Info, State) ->
	?DEBUG("Someother info:  ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(Reason, #state{timer = Timer} = State) ->
	?INFO("terminating due to ~p", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

cache_event({drop, {media, _Id} = Key}) ->
	Row = case dets:lookup(?DETS, Key) of
		[{Key, Time, Hp, Details, {inbound, queued}}] ->
			dets:insert(?DETS, {Key, Time, Hp, Details, {inbound, qabandoned}}),
			{Key, Time, Hp, Details, {inbound, qabandoned}};
		[{Key, Time, Hp, Details, {inbound, ivr}}] ->
			dets:insert(?DETS, {Key, Time, Hp, Details, {inbound, ivrabandoned}}),
			{Key, Time, Hp, Details, {inbound, ivrabandoned}};
		_Else ->
			{Key, none}
	end,
	Row;
cache_event({drop, {agent, _Id} = Key}) ->
	dets:delete(?DETS, Key),
	{Key, none};
cache_event({set, {{media, _Id} = Key, EventHp, EventDetails, EventTime}}) ->
	case dets:lookup(?DETS, Key) of
		[{Key, Time, Hp, Details, {inbound, queued}}] ->
			case {proplists:get_value(queue, EventDetails), proplists:get_value(agent, EventDetails)} of
				{undefined, undefined} ->
					dets:insert(?DETS, {Key, Time, EventHp, EventDetails, {inbound, qabandoned}}),
					{Key, Time, EventHp, EventDetails, {inbound, qabandoned}};
				{undefined, _Agent} ->
					dets:insert(?DETS, {Key, Time, EventHp, EventDetails, {inbound, handled}}),
					{Key, Time, EventHp, EventDetails, {inbound, handled}};
				{_Queue, undefined} ->
					% updating n case HP or Details changed.
					dets:insert(?DETS, {Key, Time, EventHp, EventDetails, {inbound, queued}}),
					{Key, Time, EventHp, EventDetails, {inbound, queued}};
				BothQnAgent ->
					?WARNING("both and agent and queue defined, ignoring", []),
					none
			end;
		[{Key, Time, Hp, Details, History}] ->
			% blind update
			dets:insert(?DETS, {Key, Time, EventHp, EventDetails, History}),
			{Key, Time, EventHp, EventDetails, History};
		[] ->
			case {proplists:get_value(queue, EventDetails), proplists:get_value(direction, EventDetails)} of
				{undefined, outbound} ->
					dets:insert(?DETS, {Key, EventTime, EventHp, EventDetails, outbound}),
					{Key, EventTime, EventHp, EventDetails, outbound};
				{undefined, inbound} ->
					?INFO("Didn't find queue, but still inbound.  Assuming it's in ivr call:  ~p", [Key]),
					dets:insert(?DETS, {Key, EventTime, EventHp, EventDetails, {inbound, ivr}}),
					{Key, EventTime, EventHp, EventDetails, {inbound, ivr}};
				{_Queue, inbound} ->
					dets:insert(?DETS, {Key, EventTime, EventHp, EventDetails, {inbound, queued}}),
					{Key, EventTime, EventHp, EventDetails, {inbound, queued}}
			end
	end;
cache_event({set, {{agent, _Id} = Key, EventHp, EventDetails, EventTime}}) ->
	case dets:lookup(?DETS, Key) of
		[] ->
			dets:insert(?DETS, {Key, EventTime, EventHp, EventDetails, undefined}),
			{Key, EventTime, EventHp, EventDetails, undefined};
		[{Key, Time, Hp, Details, undefined}] ->
			dets:insert(?DETS, {Key, Time, Hp, Details, undefined}),
			{Key, Time, Hp, Details, undefined}
	end.

get_clients(Filter) ->
	QH = qlc:q([proplists:get_value(client, Details) || 
		{{Type, _Id}, Time, _Hp, Details, _History} = Row <- dets:table(?DETS), 
		Type == media, 
		filter_row(Filter, Row)
	]),
	Out = qlc:e(QH),
	lists:foldl(fun(I, Acc) -> 
		case lists:member(I, Acc) of
			false ->
				[I | Acc];
			true ->
				Acc
		end
	end, [], qlc:e(QH)).

get_queue_groups(Filter) ->
	QH = qlc:q([proplists:get_value(queue, Details) || 
		{{Type, _Id}, _Time, _Hp, Details, _History} = Row <- dets:table(?DETS),
		Type == media,
		filter_row(Filter, Row),
		proplists:get_value(queue, Details) =/= undefined
	]),
	Dupped = qlc:e(QH),
	{Groups, _} = lists:foldl(fun(I, {Acc, Queues}) ->
		case lists:member(I, Queues) of
			true ->
				{Acc, Queues};
			false ->
				Group = case call_queue_config:get_queue(I) of
					Rec when is_record(Rec, call_queue) ->
						Rec#call_queue.group;
					_ ->
						"Default"
				end,
				case lists:member(Group, Acc) of
					true ->
						{Acc, Queues};
					false ->
						{[Group | Acc], [I | Queues]}
				end
		end
	end,
	{[], []},
	Dupped),
	Groups.

get_agent_profiles(Filter) ->
	QH = qlc:q([proplists:get_value(profile, Details) ||
		{{Type, _Id}, _Time, _Hp, Details, _Historyf} = Row <- dets:table(?DETS),
		Type == agent,
		filter_row(Filter, Row),
		proplists:get_value(profile, Details) =/= undefined
	]),
	Dupped = qlc:e(QH),
	lists:foldl(fun(I, Acc) ->
		case lists:member(I, Acc) of
			true ->
				Acc;
			false ->
				[I | Acc]
		end
	end,
	[], Dupped).

get_agents(Filter, Profile) ->
	QH = qlc:q([Row ||
		{{Type, _Id}, _Time, _Hp, Details, _History} = Row <- dets:table(?DETS),
		Type == agent,
		filter_row(Filter, Row),
		proplists:get_value(profile, Details) == Profile
	]),
	qlc:e(QH).

get_queues(Filter, Group) ->
	QH = qlc:q([proplists:get_value(queue, Details) || 
		{{Type, _Id}, _Time, _Hp, Details, _History} = Row <- dets:table(?DETS),
		Type == media,
		filter_row(Filter, Row),
		begin 
			Q = proplists:get_value(queue, Details),
			case call_queue_config:get_queue(Q) of
				#call_queue{group = Group} ->
					true;
				_ ->
					false
			end
		end
	]),
	Dupped = qlc:e(QH),
	lists:foldl(fun(I, Acc) ->
		case lists:member(I, Acc) of
			true ->
				Acc;
			false ->
				[I | Acc]
		end
	end,
	[], Dupped).

get_queued_medias(Filter, Queue) ->
	QH = qlc:q([Row || 
		{{Type, _Id}, _Hp, Details, _History} = Row <- dets:table(?DETS),
		Type == media,
		filter_row(Filter, Row),
		proplists:get_value(queue, Details) == Queue
	]),
	qlc:e(QH).

get_agent_medias(Filter, Agent) ->
	QH = qlc:q([Row || 
		{{Type, _Id}, _Hp, Details, _History} = Row <- dets:table(?DETS),
		Type == media,
		filter_row(Filter, Row),
		proplists:get_value(agent, Details) == Agent
	]),
	qlc:e(QH).

get_client_medias(Filter, Client) ->
	QH = qlc:q([Row ||
		{{Type, _Id}, _Hp, Details, _History} = Row <- dets:table(?DETS),
		Type == media,
		filter_row(Filter, Row),
		proplists:get_value(client, Details) == Client
	]),
	qlc:e(QH).

update_filter_states(none, Filters) ->
	Filters;
update_filter_states({{media, _}, none}, Filters) ->
	Filters;
update_filter_states({{agent, _}, none}, Filters) ->
	Filters;
update_filter_states({{agent, _}, _, _, _, _}, Filters) ->
	Filters;
update_filter_states(Row, Filters) ->
	update_filter_states(Row, Filters, []).

update_filter_states(_Row, [], Acc) ->
	lists:reverse(Acc);
update_filter_states({{media, Id}, Time, _Hp, _Details, Histroy} = Row, [{Nom, #filter{state = State} = Filter} | Tail], Acc) ->
	Midstatistics = proplists_replace(Id, {Time, Histroy}, State#filter_state.state),
	Newstatistics = lists:filter(fun({_, {T, _}}) -> T > util:now() - 86400 end, Midstatistics),
	Newstate = State#filter_state{state = Newstatistics},
	update_filter_states(Row, Tail, [{Nom, Filter#filter{state = Newstate}} | Acc]).				

%% @doc If the row passes through the filter, return true.
filter_row(#filter{queues = all, queue_groups = all, agents = all, agent_profiles = all, clients = all, nodes = all}, Row) ->
	true;
filter_row(Filter, {{media, _Id}, _Time, _Hp, Details, {_Direction, handled}}) ->
	#client{label = Client} = proplists:get_value(client, Details),
	case list_member(Client, Filter#filter.clients) of
		false ->
			false;
		true ->
			Agent = proplists:get_value(agent, Details),
			case list_member(Agent, Filter#filter.agents) of
				false ->
					false;
				true ->
					case agent_auth:get_agent(Agent) of
						{atomic, [#agent_auth{profile = Prof}]} ->
							list_member(Prof, Filter#filter.agent_profiles);
						_ ->
							false
					end
			end
	end;
filter_row(Filter, {{media, _Id}, _Time, _Hp, Details, {_Direction, Queued}}) when Queued =:= queued; Queued =:= qabandoned ->
	Node = proplists:get_value(node, Details),
	case list_member(Node, Filter#filter.nodes) of
		false ->
			false;
		true ->
			#client{label = Client} = proplists:get_value(client, Details),
			case list_member(Client, Filter#filter.clients) of
				false ->
					false;
				true ->
					Queue = proplists:get_value(queue, Details),
					case {list_member(Queue, Filter#filter.queues), Filter#filter.queue_groups} of
						{false, _} ->
							false;
						{true, all} ->
							true;
						{true, List} ->
							case call_queue_config:get_queue(Queue) of
								noexists ->
									false;
								#call_queue{group = Group} ->
									list_member(Group, List)
							end
					end
			end
	end;
filter_row(Filter, {{media, _Id}, _Time, _Hp, Details, _History}) ->
	false;
filter_row(Filter, {{agent, Agent}, _Time, _Hp, Details, _History}) ->
	Node = proplists:get_value(node, Details),
	case list_member(Node, Filter#filter.nodes) of
		false ->
			false;
		true ->
			case list_member(Agent, Filter#filter.agents) of
				false ->
					false;
				true ->
					case agent_auth:get_agent(Agent) of
						{atomic, [#agent_auth{profile = Prof}]} ->
							list_member(Prof, Filter#filter.agent_profiles);
						_ ->
							false
					end
			end
	end.

write_output({_Nom, #filter{state = FilterState, file_output = Fileout} = Filter}) ->
	Hourago = util:now() - 3600,
	Inbound = [X || {_Key, {_Time, Hkey}} = X <- FilterState#filter_state.state, element(1, Hkey) == inbound],
	Outbound = [X || {_Key, {_Time, Hkey}} = X <- FilterState#filter_state.state, element(1, Hkey) == outbound],
	Abandoned = [X || {_Key, {_Time, Hkey}} = X <- FilterState#filter_state.state, element(1, Hkey) == inbound, element(2, Hkey) == qabandoned orelse element(2, Hkey) == ivrabandoned],
	HourInbound = [X || {_Key, {Time, Hkey}} = X <- FilterState#filter_state.state, element(1, Hkey) == inbound, Time > Hourago],
	HourOutbound = [X || {_Key, {Time, Hkey}} = X <- FilterState#filter_state.state, element(1, Hkey) == outbound, Time > Hourago],
	HourAbn = [X || {_Key, {Time, Hkey}} = X <- FilterState#filter_state.state, element(1, Hkey) == inbound, element(2, Hkey) == qabandoned orelse element(2, Hkey) == ivrabandoned, Time > Hourago],
	Clients = get_clients(Filter),
	ClientsJson = clients_to_json(Clients, Filter),
	Queugroups = get_queue_groups(Filter),
	QueuegroupJson = queuegroups_to_json(Queugroups, Filter),
	AgentProfiles = get_agent_profiles(Filter),
	AgentProfsJson = agentprofiles_to_json(AgentProfiles, Filter),
	Json = {struct, [
		{<<"writeTime">>, util:now()},
		{<<"totalInbound">>, length(Inbound)},
		{<<"totalOutbound">>, length(Outbound)},
		{<<"totalAbandoned">>, length(Abandoned)},
		{<<"hourInbound">>, length(HourInbound)},
		{<<"hourOutbound">>, length(HourOutbound)},
		{<<"hourAbandoned">>, length(HourAbn)},
		{<<"clients_in_queues">>, ClientsJson},
		{<<"queueGroups">>, QueuegroupJson},
		{<<"agentProfiles">>, AgentProfsJson}
	]},
	Out = mochijson2:encode(Json),
	{ok, File} = file:open(Fileout, [write, binary]),
	file:write(File, Out).	

clients_to_json(Clients, Filter) ->
	clients_to_json(Clients, Filter, []).

clients_to_json([], _Filter, Acc) ->
	lists:reverse(Acc);
clients_to_json([Client | Tail], Filter, Acc) ->
	Medias = get_client_medias(Filter, Client),
	{Oldest, MediaJson} = medias_to_json(Medias),
	Label = case Client#client.label of
		undefined ->
			undefined;
		Else ->
			list_to_binary(Else)
	end,
	Json = {struct, [
		{<<"label">>, Label},
		{<<"oldestAge">>, Oldest},
		{<<"medias">>, MediaJson}
	]},
	clients_to_json(Tail, Filter, [Json | Acc]).

medias_to_json(Rows) ->
	Time = util:now(),
	medias_to_json(Rows, {Time, []}).

medias_to_json([], {Time, Acc}) ->
	{Time, lists:reverse(Acc)};
medias_to_json([{{media, Id}, Time, _Hp, Details, HistoricalKey} | Tail], {CurTime, Acc}) ->
	Newtime = case Time < CurTime of
		true ->
			Time;
		false ->
			CurTime
	end,
	NewHead = {struct, [
		{<<"id">>, list_to_binary(Id)},
		{<<"time">>, Time},
		{<<"node">>, proplists:get_value(node, Details)},
		{<<"type">>, proplists:get_value(type, Details)}
	]},
	medias_to_json(Tail, {Newtime, [NewHead | Acc]}).

queuegroups_to_json(Groups, Filter) ->
	queuegroups_to_json(Groups, Filter, []).

queuegroups_to_json([], _Filter, Acc) ->
	lists:reverse(Acc);
queuegroups_to_json([Group | Tail], Filter, Acc) ->
	Queues = get_queues(Filter, Group),
	QueuesJson = queues_to_json(Queues, Filter),
	Json = {struct, [
		{<<"name">>, list_to_binary(Group)},
		{<<"queues">>, QueuesJson}
	]},
	queuegroups_to_json(Tail, Filter, [Json | Acc]).

queues_to_json(Queues, Filter) ->
	queues_to_json(Queues, Filter, []).

queues_to_json([], _Filter, Acc) ->
	lists:reverse(Acc);
queues_to_json([Queue | Tail], Filter, Acc) ->
	Medias = get_queued_medias(Filter, Queue),
	{Oldest, MediaJson} = medias_to_json(Medias),
	Json = {struct, [
		{<<"name">>, list_to_binary(Queue)},
		{<<"oldestAge">>, Oldest},
		{<<"medias">>, MediaJson}
	]},
	queues_to_json(Tail, Filter, [Json | Acc]).

agentprofiles_to_json(Profiles, Filter) ->
	agentprofiles_to_json(Profiles, Filter, []).

agentprofiles_to_json([], _Filter, Acc) ->
	lists:reverse(Acc);
agentprofiles_to_json([Profile | Tail], Filter, Acc) ->
	Agents = get_agents(Filter, Profile),
	{Avail, Rel, Busy, AgentJson} = agents_to_json(Agents),
	Json = {struct, [
		{<<"name">>, list_to_binary(Profile)},
		{<<"available">>, Avail},
		{<<"released">>, Rel},
		{<<"incall">>, Busy},
		{<<"agents">>, AgentJson}
	]},
	agentprofiles_to_json(Tail, Filter, [Json | Acc]).

agents_to_json(Rows) ->
	agents_to_json(Rows, {0, 0, 0}, []).

agents_to_json([], {Avail, Rel, Busy}, Acc) ->
	{Avail, Rel, Busy, lists:reverse(Acc)};
agents_to_json([{{agent, Id}, Time, _Hp, Details, _HistoryKey} | Tail], {Avail, Rel, Busy}, Acc) ->
	{Newcounts, State, Statedata} = case {proplists:get_value(state, Details), proplists:get_value(statedata, Details)} of
		{idle, {}} ->
			{{Avail + 1, Rel, Busy}, idle, false};
		{released, {Id, default, Bias}} ->
			Data = {struct, [
				{<<"id">>, list_to_binary(Id)},
				{<<"label">>, default},
				{<<"bias">>, Bias}
			]},
			{{Avail, Rel + 1, Busy}, released, Data};
		{released, {Id, Label, Bias}} ->
			Data = {struct, [
				{<<"id">>, list_to_binary(Id)},
				{<<"label">>, list_to_binary(Label)},
				{<<"bias">>, Bias}
			]},
			{{Avail, Rel + 1, Busy}, released, Data};
		{Statename, #call{client = Client} = Media} ->
			Data = {struct, [
				{<<"client">>, case Client#client.label of undefined -> undefined; _ -> list_to_binary(Client#client.label) end},
				{<<"mediaType">>, Media#call.type},
				{<<"mediaId">>, Media#call.id}
			]},
			{{Avail, Rel, Busy + 1}, Statename, Data};
		{Statename, _Otherdata} ->
			{{Avail, Rel, Busy + 1}, Statename, false}
	end,
	NewHead = {struct, [
		{<<"id">>, list_to_binary(Id)},
		{<<"login">>, list_to_binary(proplists:get_value(login, Details))},
		{<<"node">>, proplists:get_value(node, Details)},
		{<<"lastchangetimestamp">>, begin {Mega, Sec, _} = proplists:get_value(lastchangetimestamp, Details), Mega * 1000000 + Sec end},
		{<<"state">>, State},
		{<<"stateData">>, Statedata}
	]},
	agents_to_json(Tail, Newcounts, [NewHead | Acc]).

deep_seek(Id, []) ->
	undefined;
deep_seek(Id, [{Topkey, Toprops} | Tail]) ->
	case proplists:get_value(Id, Toprops) of
		undefined ->
			deep_seek(Id, Tail);
		Value ->
			{Topkey, Value}
	end.

deep_delete(Id, Toprops) ->
	case deep_seek(Id, Toprops) of
		undefined ->
			Toprops;
		{Key, Value} ->
			Subprop = proplists:get_value(Key, Toprops),
			Newsubprop = proplists:delete(Id, Subprop),
			Midtop = proplists:delete(Key, Toprops),
			[{Key, Newsubprop} | Midtop]
	end.

proplists_replace(Key, Value, List) ->
	Mid = proplists:delete(Key, List),
	[{Key, Value} | Mid].

list_member(_Member, all) ->
	true;
list_member(Member, List) ->
	lists:member(Member, List).

-ifdef(EUNIT).

filter_row_test_() ->
	{setup,
	fun() ->
		mnesia:create_schema([node()]),
		mnesia:start(),
		call_queue_config:build_tables(),
		agent_auth:build_tables(),
		agent_auth:add_agent("agent1", "test", [], agent, "profile1"),
		agent_auth:add_agent("agent2", "test", [], agent, "profile2"),
		call_queue_config:new_queue("queue1", 1, [], [], "queue_group1"),
		call_queue_config:new_queue("queue2", 1, [], [], "queue_group1"),
		call_queue_config:new_queue("queue3", 1, [], [], "queue_group2"),
		call_queue_config:new_queue("queue4", 1, [], [], "queue_group2"),
		Client1 = #client{
			id = "1",
			label = "client1"
		},
		Client2 = #client{
			id = "2",
			label = "client2"
		},
		Rows = [
			{{media, "media-c1-q1"}, 5, [], [{queue, "queue1"}, {client, Client1}], {inbound, queued}},
			{{media, "media-c1-q2"}, 5, [], [{queue, "queue2"}, {client, Client1}], {inbound, queued}},
			{{media, "media-c1-q3"}, 5, [], [{queue, "queue3"}, {client, Client1}], {inbound, queued}},
			{{media, "media-c1-q4"}, 5, [], [{queue, "queue4"}, {client, Client1}], {inbound, queued}},
			{{media, "media-c2-q1"}, 5, [], [{queue, "queue1"}, {client, Client2}], {inbound, queued}},
			{{media, "media-c2-q2"}, 5, [], [{queue, "queue2"}, {client, Client2}], {inbound, queued}},
			{{media, "media-c2-q3"}, 5, [], [{queue, "queue3"}, {client, Client2}], {inbound, queued}},
			{{media, "media-c2-q4"}, 5, [], [{queue, "queue4"}, {client, Client2}], {inbound, queued}},
			{{media, "media-c1-a1"}, 5, [], [{agent, "agent1"}, {client, Client1}], {inbound, handled}},
			{{media, "media-c2-a2"}, 5, [], [{agent, "agent2"}, {client, Client2}], {inbound, handled}},
			{{agent, "agent1"}, 5, [], [{profile, "profile1"}], undefined},
			{{agent, "agent2"}, 5, [], [{profile, "profile2"}], undefined}
		],
		DoFilter = fun(Filter) ->
			Test = fun(R, Acc) ->
				case filter_row(Filter, R) of
					true ->
						[element(1, R) | Acc];
					false ->
						Acc
				end
			end,
			lists:sort(lists:foldl(Test, [], Rows))
		end,
		{Rows, DoFilter}
	end,
	fun(_) ->
		Queues = ["queue1", "queue2", "queue3", "queue4"],
		lists:foreach(fun(Q) -> call_queue_config:destroy_queue(Q) end, Queues),
		mnesia:stop(),
		mnesia:delete_schema([node()])
	end,
	fun({Rows, DoFilter}) ->
		[{"filter with all set to, well, all",
		fun() ->
			Filter = #filter{
				clients = all,
				queues = all,
				queue_groups = all,
				agents = all,
				agent_profiles = all,
				nodes = all
			},
			Out = lists:map(fun(R) -> filter_row(Filter, R) end, Rows),
			?assert(lists:all(fun(In) -> In end, Out))
		end},
		{"filter by client only",
		fun() ->
			Filter = #filter{
				nodes = all,
				clients = ["client1"],
				queues = all,
				queue_groups = all,
				agents = all,
				agent_profiles = all
			},
			Expected = lists:sort([
				{media, "media-c1-a1"},
				{media, "media-c1-q1"},
				{media, "media-c1-q2"},
				{media, "media-c1-q3"},
				{media, "media-c1-q4"},
				{agent, "agent1"},
				{agent, "agent2"}
			]),
			Filtered = DoFilter(Filter),
			?assertEqual(Expected, Filtered)
		end},
		{"filter by queue only",
		fun() ->
			Filter = #filter{
				nodes = all,
				clients = all,
				queues = ["queue1"],
				queue_groups = all,
				agents = all,
				agent_profiles = all
			},
			Expected = lists:sort([
				{media, "media-c1-q1"},
				{media, "media-c2-q1"},
				{agent, "agent1"},
				{agent, "agent2"},
				{media, "media-c1-a1"},
				{media, "media-c2-a2"}
			]),
			Filtered = DoFilter(Filter),
			?assertEqual(Expected, Filtered)
		end},
		{"filter by queue group only",
		fun() ->
			Filter = #filter{
				nodes = all,
				clients = all,
				queues = all,
				queue_groups = ["queue_group1"],
				agents = all,
				agent_profiles = all
			},
			Expected = lists:sort([
				{media, "media-c1-q1"},
				{media, "media-c1-q2"},
				{media, "media-c2-q1"},
				{media, "media-c2-q2"},
				{media, "media-c1-a1"},
				{media, "media-c2-a2"},
				{agent, "agent1"},
				{agent, "agent2"}
			]),
			Filtered = DoFilter(Filter),
			?assertEqual(Expected, Filtered)
		end},
		{"filter by agent only",
		fun() ->
			Filter = #filter{
				nodes = all,
				clients = all,
				queue_groups = all,
				queues = all,
				agents = ["agent1"],
				agent_profiles = all
			},
			Expected = lists:sort([
				{media, "media-c1-q1"}, 
				{media, "media-c1-q2"}, 
				{media, "media-c1-q3"}, 
				{media, "media-c1-q4"}, 
				{media, "media-c2-q1"}, 
				{media, "media-c2-q2"}, 
				{media, "media-c2-q3"}, 
				{media, "media-c2-q4"}, 
				{media, "media-c1-a1"}, 
				{agent, "agent1"}
			]),
			Filtered = DoFilter(Filter),
			?assertEqual(Expected, Filtered)
		end},
		{"filter by agent_profile only",
		fun() ->
			Filter = #filter{
				nodes = all,
				clients = all,
				queue_groups = all,
				queues = all,
				agents = all,
				agent_profiles = ["profile2"]
			},
			Expected = lists:sort([
				{media, "media-c1-q1"}, 
				{media, "media-c1-q2"}, 
				{media, "media-c1-q3"}, 
				{media, "media-c1-q4"}, 
				{media, "media-c2-q1"}, 
				{media, "media-c2-q2"}, 
				{media, "media-c2-q3"}, 
				{media, "media-c2-q4"}, 
				{media, "media-c2-a2"}, 
				{agent, "agent2"}
			]),
			Filtered = DoFilter(Filter),
			?assertEqual(Expected, Filtered)
		end},
		{"filter by queues and client",
		fun() ->
			Filter = #filter{
				nodes = all,
				clients = ["client1"],
				queue_groups = all,
				queues = ["queue1"],
				agents = all,
				agent_profiles = all
			},
			Expected = lists:sort([
				{media, "media-c1-q1"}, 
				{media, "media-c1-a1"}, 
				{agent, "agent1"},
				{agent, "agent2"}
			]),
			Filtered = DoFilter(Filter),
			?assertEqual(Expected, Filtered)
		end},
		{"filter by queues, client, and agent",
		fun() ->
			Filter = #filter{
				nodes = all,
				clients = ["client1"],
				queue_groups = all,
				queues = ["queue1"],
				agents = ["agent1"],
				agent_profiles = all
			},
			Expected = lists:sort([
				{media, "media-c1-q1"}, 
				{media, "media-c1-a1"}, 
				{agent, "agent1"}
			]),
			Filtered = DoFilter(Filter),
			?assertEqual(Expected, Filtered)
		end}]
	end}.

-endif.