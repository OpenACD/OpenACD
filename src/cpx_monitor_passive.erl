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
	stop/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(filter, {
	file_output :: string(),
	queues :: [string()] | 'all',
	queue_groups :: [string()] | 'all',
	agents :: [string()] | 'all',
	agent_profiles :: [string()] | 'all',
	clients :: [string()] | 'all',
	output_as = json :: 'json' | 'xml'
}).

-record(state, {
	filters = [] :: [{string(), #filter{}}],
	interval = ?WRITE_INTERVAL :: pos_integer(),
	%agent_cache = [],
	%media_cache = [],
	timer :: any(),
	write_pid :: 'undefined' | pid()
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
			queues = proplists:get_value(queues, Props, all),
			queue_groups = proplists:get_value(queue_groups, Props, all),
			agents = proplists:get_value(agents, Props, all),
			agent_profiles = proplists:get_value(agent_profiles, Props, all),
			clients = proplists:get_value(clients, Props, all),
			output_as = json
		},
		{Name, Filter}
	end,
	Filters = lists:map(Torec, proplists:get_value(outputs, Options, [{"default", []}])),
	dets:open_file(?DETS, []),
	{ok, Agents} = cpx_monitor:get_health(agent),
	{ok, Medias} = cpx_monitor:get_health(media),
	cpx_monitor:subscribe(Subtest),
	lists:foreach(fun(E) -> cache_event(E) end, lists:append(Agents, Medias)),
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
	?DEBUG("Writing output with state:  ~p", [State]),
%	Fun = fun({Name, FilterRec} = Filter) ->
%		Fmedias = filter_medias(Medias, FilterRec),
%		Fagents = filter_agents(Agents, FilterRec),
%		JsonQued = clients_queued_to_json(Fmedias),
%		JsonAgents = agent_profs_to_json(Fagents),
%		Json = {struct, [
%			{<<"clients_in_queues">>, JsonQued},
%			{<<"agent_profiles">>, JsonAgents}
%		]},
%		Encoded = mochijson2:encode(Json),
%		{ok, File} = file:open(FilterRec#filter.file_output, [write, binary]),
%		ok = file:write(File, Encoded),
%		?DEBUG("wrote ~p to ~p", [Name, FilterRec#filter.file_output]),
%		ok
%	end,
%	
%	lists:foreach(fun(Elem) -> spawn(fun() -> Fun(Elem) end) end, Filters),
	
	{ok, Timer} = timer:send_after(State#state.interval, write_output),
	{noreply, State#state{timer = Timer}};
handle_info({cpx_monitor_event, Event}, State) ->
	cache_event(Event),
	{noreply, Event};
%handle_info({cpx_monitor_event, {set, {{media, Id}, Hp, Det, _Time}}}, #state{media_cache = Medias} = State) ->
%	?DEBUG("setting a media", []),
%	Entry = {{media, Id}, Hp, Det},
%	case proplists:get_value(queue, Det) of
%		undefined ->
%			Newmedias = deep_delete(Id, Medias),
%			{noreply, State#state{media_cache = Newmedias}};
%		Queue ->
%			#client{label = Client} = proplists:get_value(client, Det),
%			Midsub = proplists:get_value(Client, Medias, []),
%			Newsub = [{Id, Entry} | Midsub],
%			Newmedias = proplists_replace(Client, Newsub, Medias),
%			{noreply, State#state{media_cache = Newmedias}}
%	end;
%handle_info({cpx_monitor_event, {drop, {media, Id}}}, #state{media_cache = Medias} = State) ->
%	?DEBUG("Dropping media", []),
%	Newmedias = deep_delete(Id, Medias),
%	{noreply, State#state{media_cache = Newmedias}};
%handle_info({cpx_monitor_event, {set, {{agent, Id}, Hp, Det, _Time}}}, #state{agent_cache = Agent} = State) ->
%	?DEBUG("Setting agent", []),
%	Entry = {{media, Id}, Hp, Det},
%	Profile = proplists:get_value(profile, Det),
%	Midsub = proplists:get_value(Profile, Agent, []),
%	Newsub = [{Id, Entry} | Midsub],
%	Newagents = proplists_replace(Profile, Newsub, Agent),
%	{noreply, State#state{agent_cache = Agent}};
%handle_info({cpx_monitor_event, {drop, {agent, Id}}}, #state{agent_cache = Agent} = State) ->
%	?DEBUG("dropping agent", []),
%	Newagents = deep_delete(Id, Agent),
%	{noreply, State#state{agent_cache = Newagents}};
handle_info(Info, State) ->
	?DEBUG("Someother info:  ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(Reason, #state{timer = Timer} = State) ->
	?INFO("terminating due to ~p", [Reason]),
	timer:cancel(Timer),
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
	case dets:lookup(?DETS, Key) of
		[{Key, Time, Hp, Details, {inbound, queued}}] ->
			dets:insert(?DETS, {Key, Time, Hp, Details, {inbound, qabandoned}});
		[{Key, Time, Hp, Details, {inbound, ivr}}] ->
			dets:insert(?DETS, {Key, Time, Hp, Details, {inbound, ivrabandoned}});
		_Else ->
			ok
	end;
cache_event({drop, {agent, _Id} = Key}) ->
	dets:delete(?DETS, Key);
cache_event({set, {{media, _Id} = Key, EventHp, EventDetails, EventTime}}) ->
	case dets:lookup(?DETS, Key) of
		[{Key, Time, Hp, Details, {inbound, queued}}] ->
			case {proplists:get_value(queue, EventDetails), proplists:get_value(agent, EventDetails)} of
				{undefined, undefined} ->
					dets:insert(?DETS, {Key, Time, EventHp, EventDetails, {inbound, qabandoned}});
				{undefined, _Agent} ->
					dets:insert(?DETS, {Key, Time, EventHp, EventDetails, {inbound, handled}});
				{_Queue, undefined} ->
					% updating n case HP or Details changed.
					dets:insert(?DETS, {Key, Time, EventHp, EventDetails, {inbound, queued}});
				BothQnAgent ->
					?WARNING("both and agent and queue defined, ignoring", []),
					ok
			end;
		[{Key, Time, Hp, Details, History}] ->
			% blind update
			dets:insert(?DETS, {Key, Time, EventHp, EventDetails, History});
		[] ->
			case {proplists:get_value(queue, EventDetails), proplists:get_value(direction, EventDetails)} of
				{undefined, outbound} ->
					dets:insert(?DETS, {Key, EventTime, EventHp, EventDetails, outbound});
				{undefined, inbound} ->
					?INFO("Didn't find queue, but still inbound.  Assuming it's in ivr call:  ~p", [Key]),
					dets:insert(?DETS, {Key, EventTime, EventHp, EventDetails, {inbound, ivr}});
				{_Queue, inbound} ->
					dets:insert(?DETS, {Key, EventTime, EventHp, EventDetails, {inbound, queued}})
			end
	end;
cache_event({set, {{agent, _Id} = Key, EventHp, EventDetails, EventTime}}) ->
	case dets:lookup(?DETS, Key) of
		[] ->
			dets:insert(?DETS, {Key, EventTime, EventHp, EventDetails, undefined});
		[{Key, Time, Hp, Details, undefined}] ->
			dets:insert(?DETS, {Key, Time, Hp, Details, undefined})
	end.
	
%% @doc If the row passes through the filter, return true.
filter_row(#filter{queues = all, queue_groups = all, agents = all, agent_profiles = all, clients = all}, Row) ->
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
	end;
filter_row(Filter, {{media, _Id}, _Time, _Hp, Details, _History}) ->
	false;
filter_row(Filter, {{agent, Agent}, _Time, _Hp, Details, _History}) ->
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
	end.










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

create_queued_clients(Medias) ->
	?DEBUG("Medias:  ~p", [Medias]),
	create_queued_clients(Medias, []).

create_queued_clients([], Acc) ->
	?DEBUG("clients faking queues:  ~p", [Acc]),
	Acc;
create_queued_clients([{{media, Id}, _Hp, Det} = Head | Tail], Acc) ->
	case proplists:get_value(queue, Det) of
		undefined ->
			create_queued_clients(Tail, Acc);
		_Else ->
			#client{label = Client} = proplists:get_value(client, Det),
			{Othermeds, Midacc} = case proplists:get_value(Client, Acc) of
				undefined ->
					{[], Acc};
				List ->
					{List, proplists:delete(Client, Acc)}
			end,
			NewAcc = [{Client, [{Id, Head} | Othermeds]} | Midacc],
			create_queued_clients(Tail, NewAcc)
	end.

filter_medias(Medias, Filter) ->
	filter_medias(Medias, Filter, []).

filter_medias([], _, Acc) ->
	Acc;
filter_medias([{Id, {{media, Id}, _Hp, Det} = Entry} | Tail], Filter, Acc) ->
	Queue = proplists:get_value(queue, Det),
	#call_queue{group = Group} = call_queue_config:get_queue(Queue),
	#client{label = Client} = proplists:get_value(client, Det),
	case {list_member(Queue, Filter#filter.queues), list_member(Group, Filter#filter.queue_groups), list_member(Client, Filter#filter.clients)} of
		{true, true, true} ->
			Midsub = proplists:get_value(Client, Acc, []),
			Newsubs = [{Id, Entry} | Midsub],
			Newacc = proplists_replace(Client, Newsubs, Acc),
			filter_medias(Tail, Filter, Newacc);
		Else ->
			?DEBUG("media filtered out:  ~p", [Else]),
			filter_medias(Tail, Filter, Acc)
	end;
filter_medias([{Client, Medias} | Tail], Filter, Acc) ->
	case list_member(Client, Filter#filter.clients) of
		true ->
			Newsub = filter_medias(Medias, Filter, Acc),
			Newacc = [{Client, Newsub} | Acc],
			filter_medias(Tail, Filter, Newacc);
		false ->
			?DEBUG("media filtered out at client level", []),
			filter_medias(Tail, Filter, Acc)
	end.

filter_agents(List, Filter) ->
	filter_agents(List, Filter, []).

filter_agents([], _, Acc) ->
	Acc;
filter_agents([{Id, {{agent, Id}, _Hp, Det} = Entry} | Tail], Filter, Acc) ->
	Login = proplists:get_value(login, Det),
	case list_member(Login, Filter#filter.agents) of
		true ->
			filter_agents(Tail, Filter, [{Id, Entry} | Acc]);
		false ->
			filter_agents(Tail, Filter, Acc)
	end;
filter_agents([{Profile, Agents} | Tail], Filter, Acc) ->
	case list_member(Profile, Filter#filter.agent_profiles) of
		true ->
			Newsub = filter_agents(Agents, Filter, []),
			Newacc = [{Profile, Newsub} | Acc],
			filter_agents(Tail, Filter, Newacc);
		false ->
			filter_agents(Tail, Filter, Acc)
	end.

clients_queued_to_json(List) ->
	clients_queued_to_json(List, []).

clients_queued_to_json([], Acc) ->
	Acc;
clients_queued_to_json([{Id, {{media, Id}, _, Det}} | Tail], Acc) ->
	Age = proplists:get_value(queued_at, Det),
	Json = {struct, [
		{<<"id">>, list_to_binary(Id)},
		{<<"queued_at">>, Age}
	]},
	clients_queued_to_json(Tail, [Json | Acc]);
clients_queued_to_json([{Client, Medias} | Tail], Acc) ->
	Cbin = case Client of
		undefined ->
			undefined;
		_Else ->
			list_to_binary(Client)
	end,
	Jmedia = clients_queued_to_json(Medias, []),
	Json = {struct, [
		{<<"client">>, Cbin},
		{<<"medias">>, Jmedia}
	]},
	clients_queued_to_json(Tail, [Json | Acc]).

sort_agents(Agents) ->
	sort_agents(Agents, []).

sort_agents([], Acc) ->
	?DEBUG("Sorted agents:  ~p", [Acc]),
	Acc;
sort_agents([{{agent, Id}, _Hp, Det} = Head | Tail], Acc) ->
	Login = proplists:get_value(login, Det),
	Prof = proplists:get_value(profile, Det),
	{Proflist, Midacc} = case proplists:get_value(Prof, Acc) of
		undefined ->
			{[], Acc};
		List ->
			{List, proplists:delete(Prof, Acc)}
	end,
	NewAcc = [{Prof, [{Id, Head} | Proflist]} | Midacc],
	sort_agents(Tail, NewAcc).

agent_profs_to_json(List) ->
	agent_profs_to_json(List, []).
	
agent_profs_to_json([], Acc) ->
	Acc;
agent_profs_to_json([{Profname, Agents} | Tail], Acc) ->
	JsonAgents = agents_to_json(Agents),
	Json = {struct, [
		{<<"name">>, list_to_binary(Profname)},
		{<<"agents">>, JsonAgents}
	]},
	NewAcc = [Json | Acc],
	agent_profs_to_json(Tail, NewAcc).
	
agents_to_json(Agents) ->
	agents_to_json(Agents, []).

agents_to_json([], Acc) ->
	Acc;
agents_to_json([{_, {_, _, Det}} | Tail], Acc) ->
	Login = proplists:get_value(login, Det),
	Lastchange = begin
		{Mega, Sec, _} = proplists:get_value(lastchangetimestamp, Det),
		Mega * 1000000 + Sec
	end,
	{State, Statedata} = case {proplists:get_value(state, Det), proplists:get_value(statedata, Det)} of
		{released, {"default", default, -1}} ->
			{released, {struct, [
				{<<"id">>, default},
				{<<"label">>, default},
				{<<"bias">>, -1}
			]}};
		{released, {Id, Reason, Bias}} ->
			{released, {struct, [
				{<<"id">>, list_to_binary(Id)},
				{<<"label">>, list_to_binary(Reason)},
				{<<"bias">>, Bias}
			]}};
		{idle, {}} ->
			{idle, false};
		{Statename, #call{client = Client}} ->
			Cbin = case Client#client.label of
				undefined ->
					undefined;
				_Else ->
					list_to_binary(Client#client.label)
			end,
			{Statename, Cbin}
	end,
	Json = {struct, [
		{<<"login">>, list_to_binary(Login)},
		{<<"lastchangetimestamp">>, Lastchange},
		{<<"state">>, State},
		{<<"statedata">>, Statedata}
	]},
	agents_to_json(Tail, [Json | Acc]).

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
				agent_profiles = all
			},
			Out = lists:map(fun(R) -> filter_row(Filter, R) end, Rows),
			?assert(lists:all(fun(In) -> In end, Out))
		end},
		{"filter by client only",
		fun() ->
			Filter = #filter{
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