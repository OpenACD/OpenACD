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

-define(WRITE_INTERVAL, 60).

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

-type(write_interval() :: {write_interval, pos_integer()}).
-type(start_option() :: 
	outputs_option() |
	write_interval()).
	
-type(start_options() :: [start_option()]).

%% API
-export([
	start_link/1,
	start/1,
	stop/1
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
	agent_cache = [],
	media_cache = [],
	timer :: any()
}).

%%====================================================================
%% API
%%====================================================================

-spec(start_link/1 :: (Options :: start_options()) -> {'ok', pid()}).
start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

-spec(start/1 :: (Options :: start_options()) -> {'ok', pid()}).
start(Options) ->
	gen_server:start(?MODULE, Options, []).

-spec(stop/1 :: (Pid :: pid()) -> 'ok').
stop(Pid) ->
	gen_server:cast(Pid, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%--------------------------------------------------------------------
init(Options) ->
	Subtest = fun(Message) ->
		?DEBUG("passive sub testing message: ~p", [Message]),
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
	cpx_monitor:subscribe(Subtest),
	Interval = proplists:get_value(write_interval, Options, ?WRITE_INTERVAL) * 1000,
	Outputs = proplists:get_value(outputs, Options),
	Torec = fun({Name, Props}) ->
		Fileout = proplists:get_value(xml_output, Props, lists:append(["./", Name])),
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
	{ok, Timer} = timer:send_after(Interval, write_output),
	{ok, Agents} = cpx_monitor:get_health(agent),
	{ok, Medias} = cpx_monitor:get_health(media),

	Agentcache = sort_agents(Agents),
	Mediacache = create_queued_clients(Medias),
	?DEBUG("started", []),
	{ok, #state{
		filters = Filters,
		interval = Interval,
		timer = Timer,
		agent_cache = Agentcache,
		media_cache = Mediacache
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
handle_info(write_output, #state{filters = Filters, agent_cache = Agents, media_cache = Medias} = State) ->
	?DEBUG("Writing output with state:  ~p", [State]),
	Fun = fun({Name, FilterRec} = Filter) ->
		Fmedias = filter_medias(Medias, FilterRec),
		Fagents = filter_agents(Agents, FilterRec),
		JsonQued = clients_queued_to_json(Fmedias),
		JsonAgents = agent_profs_to_json(Fagents),
		Json = {struct, [
			{<<"clients_in_queues">>, JsonQued},
			{<<"agent_profiles">>, JsonAgents}
		]},
		Encoded = mochijson2:encode(Json),
		{ok, File} = file:open(FilterRec#filter.file_output, [write, binary]),
		ok = file:write(File, Encoded),
		?DEBUG("wrote ~p to ~p", [Name, FilterRec#filter.file_output]),
		ok
	end,
	
	lists:foreach(fun(Elem) -> spawn(fun() -> Fun(Elem) end) end, Filters),
	
	{ok, Timer} = timer:send_after(State#state.interval, write_output),
	{noreply, State#state{timer = Timer}};
handle_info({cpx_monitor_event, {set, {{media, Id}, Hp, Det, _Time}}}, #state{media_cache = Medias} = State) ->
	?DEBUG("setting a media", []),
	Entry = {{media, Id}, Hp, Det},
	case proplists:get_value(queue, Det) of
		undefined ->
			Newmedias = deep_delete(Id, Medias),
			{noreply, State#state{media_cache = Newmedias}};
		Queue ->
			#client{label = Client} = proplists:get_value(client, Det),
			Midsub = proplists:get_value(Client, Medias, []),
			Newsub = [{Id, Entry} | Midsub],
			Newmedias = proplists_replace(Client, Newsub, Medias),
			{noreply, State#state{media_cache = Newmedias}}
	end;
handle_info({cpx_monitor_event, {drop, {media, Id}}}, #state{media_cache = Medias} = State) ->
	?DEBUG("Dropping media", []),
	Newmedias = deep_delete(Id, Medias),
	{noreply, State#state{media_cache = Newmedias}};
handle_info({cpx_monitor_event, {set, {{agent, Id}, Hp, Det, _Time}}}, #state{agent_cache = Agent} = State) ->
	?DEBUG("Setting agent", []),
	Entry = {{media, Id}, Hp, Det},
	Profile = proplists:get_value(profile, Det),
	Midsub = proplists:get_value(Profile, Agent, []),
	Newsub = [{Id, Entry} | Midsub],
	Newagents = proplists_replace(Profile, Newsub, Agent),
	{noreply, State#state{agent_cache = Agent}};
handle_info({cpx_monitor_event, {drop, {agent, Id}}}, #state{agent_cache = Agent} = State) ->
	?DEBUG("dropping agent", []),
	Newagents = deep_delete(Id, Agent),
	{noreply, State#state{agent_cache = Newagents}};
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
		{released, default} ->
			{released, default};
		{released, Reason} ->
			{released, list_to_binary(Reason)};
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
