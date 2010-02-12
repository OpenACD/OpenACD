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

%% @doc Module to spit a subset of the cpx_monitor data to an xml file 
%% periodically.
-module(cpx_monitor_passive).
-author(micahw).

-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("call.hrl").
-include("queue.hrl").
-include("agent.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(WRITE_INTERVAL, 60). % in seconds.
-ifdef(TEST).
-define(DETS, passive_cache_test).
-else.
-define(DETS, passive_cache).
-endif.

-define(slug(), ?DEBUG("slug", [])).

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
-type(prune_dets() :: 'prune_dets' | {'prune_dets', boolean()}).

-type(start_option() :: 
	outputs_option() |
	write_interval() | 
	prune_dets()).
	
-type(start_options() :: [start_option()]).

%% Dets data types
-type(timestamp() :: integer()).
%-type(health_data() :: [{atom(), any()}]).
%-type(details() :: [{any(), any()}]).
-type(historical_event() :: 'ivr' | 'queued' | 'handled' | 'ended').
%-type(historical_tuple() :: {dets_key(), time(), health_data(), details(), historical_key()}).

%% API
-export([
	start_link/1,
	start/1,
	stop/0,
	write_output/3,
	prune_dets/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(filter_state, {
	inbound = 0 :: non_neg_integer(),
	outbound = 0 :: non_neg_integer(),
	abandoned = 0 :: non_neg_integer(),
	agents = 0 :: non_neg_integer(),
	available = 0 :: non_neg_integer(),
	incall = 0 :: non_neg_integer(),
	released = 0 :: non_neg_integer(),
	wrapup = 0 :: non_neg_integer()
}).

-record(filter, {
	file_output :: string(),
	max_age = max :: 'max' | pos_integer() | {'since', pos_integer()},
	queues :: [string()] | 'all',
	queue_groups :: [string()] | 'all',
	agents :: [string()] | 'all',
	agent_profiles :: [string()] | 'all',
	clients :: [string()] | 'all',
	nodes :: [atom()] | 'all',
	output_as = json :: 'json' | 'xml',
	state = #filter_state{} :: #filter_state{}
}).

-type(agent_login() :: string()).
-type(agent_profile() :: string()).
-type(queue_name() :: string()).
-type(queue_group() :: string()).

-record(cached_media, {
	id :: string(),
	client_id :: string(),
	client_label :: string(),
	direction :: 'inbound' | 'outbound',
	state :: 'ivr' | 'queue' | 'agent' | 'ended',
	statedata :: {queue_group(), queue_name()} | {agent_profile(), agent_login()} | 'null',
	endstate = inprogress :: 'ivrabandoned' | 'queueabandoned' | 'handled' | 'inprogress',
	time = 0 :: non_neg_integer(),
	history = [] :: [{timestamp(), historical_event(), any()}],
	json :: any()
}).

-record(cached_agent, {
	id :: string(),
	login :: string(),
	profile :: string(),
	state :: atom(),
	statedata :: any(),
	time :: non_neg_integer(),
	json :: any()
}).

-record(media_stats, {
	inbound = 0,
	outbound = 0,
	inivr = 0,
	inqueue = 0,
	abandoned = 0,
	oldest = null :: 'null' | {string(), non_neg_integer()}
}).

-record(agent_stats, {
	total = 0,
	available = 0,
	oncall = 0,
	released = 0,
	wrapup = 0
}).

-record(state, {
	agent_cache :: any(), % ets tid
	media_cache :: any(), % ets tid
	filters = [] :: [{string(), #filter{}}],
	interval = ?WRITE_INTERVAL :: pos_integer(),
	timer :: any(),
	write_pids = undefined :: [{pid(), string()}],
	pruning_pid :: 'undefined' | pid(),
	queue_group_cache = [] :: [{string(), string()}], %% [{queue_name, group_queue_is_in}]
	agent_profile_cache = [] :: [{string(), string()}] %% [{agent_name, agent_profile}]
}).	

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

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

%% @doc Attempts to remove from the dets table any medias that no longer
%% exist.
-spec(prune_dets/0 :: () -> 'ok').
prune_dets() ->
	?MODULE ! prune_dets.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%--------------------------------------------------------------------
init(Options) ->
	process_flag(trap_exit, true),
	Subtest = fun(Message) ->
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

	%% caching the queue groups
	QueueRecs = call_queue_config:get_queues(),
	QueueRecsCache = [{Queue, Group} || #call_queue{name = Queue, group = Group} <- QueueRecs],
	%% and cache the agent proflies
	AgentRecs = agent_auth:get_agents(),
	AgentProfsCache = [{Agent, Profile} || #agent_auth{login = Agent, profile = Profile} <- AgentRecs],
	dets:open_file(?DETS, []),
	ets:new(cached_media, [named_table, public, {keypos, 2}]),
	ets:new(cached_agent, [named_table, public, {keypos, 2}]),
	ets:new(stats_cache, [named_table, public]),
	qlc:e(qlc:q([
		begin
			update_queue_stats(null, Row),
			update_client_stats(null, Row),
			ets:insert(cached_media, Row)
		end || Row <- dets:table(?DETS)])),
	cpx_monitor:subscribe(Subtest),
	{ok, Timer} = timer:send_after(Interval, write_output),
	?DEBUG("started", []),
	{ok, #state{
		filters = undefined,
		interval = Interval,
		timer = Timer,
		queue_group_cache = QueueRecsCache,
		agent_profile_cache = AgentProfsCache
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
handle_info(write_output, #state{filters = Filters, write_pids = undefined, queue_group_cache = QueueCache, agent_profile_cache = AgentCache} = State) ->
	qlc:e(qlc:q([
		begin
			ets:delete(cached_media, Id),
			update_queue_stats(M, null),
			dets:delete(?DETS, Id)
		end || 
		#cached_media{id = Id} = M <- ets:table(cached_media),
		util:now() - M#cached_media.time > 86400,
		M#cached_media.state == ended
	])),
	%?DEBUG("das pids:  ~p", [WritePids]),
	Pid = spawn_link(fun() -> write_output(State#state.interval, QueueCache, AgentCache) end),
	Timer = erlang:send_after(State#state.interval, self(), write_output),
	{noreply, State#state{timer = Timer, write_pids = Pid}};
handle_info(write_output, State) ->
	?WARNING("Write output request with an outstanding write:  ~p", [State#state.write_pids]),
	Timer = erlang:send_after(State#state.interval, self(), write_output),
	{noreply, State#state{timer = Timer}};
handle_info(prune_dets, #state{pruning_pid = undefined} = State) ->
	{noreply, State#state{pruning_pid = undefined}};
handle_info(prune_dets, State) ->
	?WARNING("A prune is already running.", []),
	{noreply, State};
handle_info({cpx_monitor_event, Event}, #state{filters = Filters} = State) ->
	case cache_event(Event, State#state.queue_group_cache, State#state.agent_profile_cache) of
		nochange ->
			{noreply, State};
		{Old, New} ->
			Update = case Event of
				{drop, {T, _}} ->
					T;
				{set, {{T, _}, _, _, _}} ->
					T
			end,
			case Update of
				media ->
					update_queue_stats(Old, New),
					update_client_stats(Old, New);
				agent ->
					update_agent_profiles(Old, New)
			end,
			{noreply, State}
	end;
handle_info({'EXIT', Pid, Reason}, #state{pruning_pid = Pid} = State) ->
	case Reason of
		normal ->
			?INFO("Pruning pid exited normally", []);
		_ ->
			?WARNING("Pruning pid exited due to ~p", [Reason])
	end,
	{noreply, State#state{pruning_pid = undefined}};
handle_info({'EXIT', Pid, Reason}, #state{write_pids = Pid} = State) ->
	case Reason of
		normal ->
			%?DEBUG("output written for filter ~p", [Name]),
			ok;
		_Else ->
			?ERROR("output write abnormally:  ~p", [Reason]),
			ok
	end,
	{noreply, State#state{write_pids = undefined}};
handle_info(_Info, State) ->
	%?DEBUG("Someother info:  ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
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

cache_event({set, {{media, Id}, _, _, _}} = Event, QueueCache, AgentCache) ->
	?slug(),
	Old = ets:lookup(cached_media, Id),
	New = transform_event(Event, Old, QueueCache, AgentCache),
	dets:insert(?DETS, New),
	ets:insert(cached_media, New),
	Fixedold = case Old of [] -> null; [O] -> O end,
	{Fixedold, New};
cache_event({drop, {media, Id}} = Event, QueueCache, AgentCache) ->
	?slug(),
	case ets:lookup(cached_media, Id) of
		[] ->
			nochange;
		[Realold] = Old ->
			New = transform_event(Event, Old, QueueCache, AgentCache),
			dets:insert(?DETS, New),
			ets:insert(cached_media, New),
			{Realold, New}
	end;
cache_event({drop, {agent, Id}}, _QueueCache, _AgentCache) ->
	?slug(),
	case ets:lookup(cached_agent, Id) of
		[] ->
			nochange;
		[Rold] = Old ->
			ets:delete(cached_agent, Id),
			{Rold, null}
	end;
cache_event({set, {{agent, Id}, _, _, _}} = Event, QueueCache, AgentCache) ->
	?slug(),
	New = transform_event(Event, [], QueueCache, AgentCache),
	Old = case ets:lookup(cached_agent, Id) of
		[] ->
			null;
		[Row] ->
			Row
	end,
	ets:insert(cached_agent, New),
	{Old, New}.

update_client_stats(#cached_media{client_id = Id, client_label = Label} = Old, #cached_media{client_id = Id} = New) ->
	?slug(),
	Oldstats = case ets:lookup(stats_cache, {client, Id}) of
		[] ->
			#media_stats{};
		[{{client, Id}, S}] ->
			S
	end,
	Newstats = update_media_stats(Old, New, Oldstats),
	ets:insert(stats_cache, {{client, Id, Label}, Newstats});
update_client_stats(null, #cached_media{client_id = Id, client_label = Label} = New) ->
	?slug(),
	Oldstats = case ets:lookup(stats_cache, {client, Id, Label}) of
		[] ->
			#media_stats{};
		[{{client, Id, Label}, S}] ->
			S
	end,
	Newstats = update_media_stats(null, New, Oldstats),
	ets:insert(stats_cache, {{client, Id, Label}, Newstats}).

update_queue_stats(#cached_media{state = queue} = Old, #cached_media{state = queue} = New) ->
	?slug(),
	% TODO if/when we do queue transfer, this'll become important.
	ok;
update_queue_stats(#cached_media{state = ivr} = Old, #cached_media{state = queue} = New) ->
	?slug(),
	{_G, Newqueue} = New#cached_media.statedata,
	Stats = case ets:lookup(stats_cache, {queue, Newqueue}) of
		[] ->
			#media_stats{inqueue = 1};
		[{{queue, Newqueue}, S}] ->
			S#media_stats{inqueue = S#media_stats.inqueue + 1}
	end,
	case lists:any(fun({_, queue, {_, Newqueue}}) -> true; (_) -> false end, Old#cached_media.history) of
		true ->
			ets:insert(stats_cache, {{queue, Newqueue}, Stats});
		false ->
			Newstats = case Old#cached_media.direction of
				inbound ->
					Stats#media_stats{inbound = Stats#media_stats.inbound + 1};
				outbound ->
					Stats#media_stats{outbound = Stats#media_stats.outbound + 1}
			end,
			ets:insert(stats_cache, {{queue, Newqueue}, Stats})
	end;
update_queue_stats(#cached_media{state = queue} = Old, #cached_media{state = agent} = New) ->
	?slug(),
	{_G, Newqueue} = Old#cached_media.statedata,
	[{{queue, Newqueue}, Stats}] = ets:lookup(stats_cache, {queue, Newqueue}),
	Newstats = Stats#media_stats{inqueue = Stats#media_stats.inqueue + 1},
	ets:insert(stats_cache, {{queue, Newqueue}, Newstats});
update_queue_stats(#cached_media{state = queue} = Old, #cached_media{state = ended} = New) ->
	?slug(),
	{_G, Newqueue} = Old#cached_media.statedata,
	[{{queue, Newqueue}, Stats}] = ets:lookup(stats_cache, {queue, Newqueue}),
	Midstats = Stats#media_stats{inqueue = Stats#media_stats.inqueue + 1},
	Newstats = case New#cached_media.endstate of
		handled ->
			Midstats;
		_ ->
			Midstats#media_stats{abandoned = Stats#media_stats.abandoned + 1}
	end,
	ets:insert(stats_cache, {{queue, Newqueue}, Newstats});
update_queue_stats(#cached_media{state = queue} = Old, null) ->
	?slug(),
	{_G, Newqueue} = Old#cached_media.statedata,
	[{{queue, Newqueue}, Stats}] = ets:lookup(stats_cache, {queue, Newqueue}),
	case lists:any(fun({_, queue, {_, Newqueue}}) -> true; (_) -> false end, Old#cached_media.history) of
		true ->
			Midstats = case Old#cached_media.direction of
				inbound ->
					Stats#media_stats{inbound = Stats#media_stats.inbound - 1};
				outbound ->
					Stats#media_stats{outbound = Stats#media_stats.outbound - 1}
			end,
			Newstats = case Old#cached_media.endstate of
				handled ->
					Midstats;
				_ ->
					Midstats#media_stats{abandoned = Midstats#media_stats.abandoned - 1}
			end,
			ets:insert(stats_cache, {{queue, Newqueue}, Newstats});
		false ->
			ok
	end;
update_queue_stats(null, #cached_media{state = queue} = New) ->
	?slug(),
	{_G, Newqueue} = New#cached_media.statedata,
	Stats = case ets:lookup(stats_cache, {queue, Newqueue}) of
		[] ->
			#media_stats{};
		[{{queue, Newqueue}, S}] ->
			S
	end,
	Newstats = update_media_stats(null, New, Stats),
	ets:insert(stats_cache, {{queue, Newqueue}, Newstats});
update_queue_stats(null, _) ->
	?slug(),
	ok;
update_queue_stats(_, _) ->
	?slug(),
	% anything else we ignore.
	ok.

update_media_stats(null, #cached_media{direction = inbound, state = ivr} = New, Oldstats) when is_record(New, cached_media) ->
	?slug(),
	Oldstats#media_stats{
		inbound = Oldstats#media_stats.inbound + 1,
		inivr = Oldstats#media_stats.inivr + 1
	};
update_media_stats(null, #cached_media{direction = inbound, state = queue} = New, Oldstats) when is_record(New, cached_media) ->
	?slug(),
	Oldstats#media_stats{
		inbound = Oldstats#media_stats.inbound + 1,
		inqueue = Oldstats#media_stats.inqueue + 1
	};
update_media_stats(Old, New, Oldstats) when is_record(New, cached_media) ->
	?slug(),
	case {Old#cached_media.state, New#cached_media.state} of
		{X, X} ->
			Oldstats;
		{ivr, queue} ->
			Oldstats#media_stats{
				inivr = Oldstats#media_stats.inivr - 1,
				inqueue = Oldstats#media_stats.inqueue + 1
			};
		{ivr, ended} ->
			Inivr = Oldstats#media_stats.inivr - 1,
			case New#cached_media.endstate of
				handled ->
					Oldstats#media_stats{inivr = Inivr};
				_ ->
					Oldstats#media_stats{inivr = Inivr, abandoned = Oldstats#media_stats.abandoned + 1}
			end;
		{queue, agent} ->
			Oldstats#media_stats{
				inqueue = Oldstats#media_stats.inqueue - 1
			};
		{queue, ended} ->
			Inqueue = Oldstats#media_stats.inqueue - 1,
			case New#cached_media.endstate of
				handled ->
					Oldstats#media_stats{inqueue = Inqueue};
				_ ->
					Oldstats#media_stats{inqueue = Inqueue, abandoned = Oldstats#media_stats.abandoned + 1}
			end;
		{_, ivr} ->
			Oldstats#media_stats{
				inivr = Oldstats#media_stats.inivr + 1
			};
		{_, queue} ->
			Oldstats#media_stats{
				inqueue = Oldstats#media_stats.inqueue + 1
			};
		{_, _} ->
			Oldstats
	end;
update_media_stats(Old, null, Oldstats) when is_record(Old, cached_media) ->
	?slug(),
	Abn = case Old#cached_media.endstate of
		handled ->
			Oldstats#media_stats.abandoned;
		_ ->
			Oldstats#media_stats.abandoned - 1
	end,
	Oldstats#media_stats{abandoned = Abn}.	

update_agent_profiles(#cached_agent{profile = P} = Old, #cached_agent{profile = P} = New) ->
	?slug(),
	Stats = case ets:lookup(stats_cache, {profile, P}) of
		[] ->
			#agent_stats{};
		[{{profile, P}, S}] ->
			S
	end,
	Mid = update_agent_profiles_down(Old, Stats),
	Newstasts = update_agent_profiles_up(New, Mid),
	ets:insert(stats_cache, {{profile, P}, Newstasts});
update_agent_profiles(#cached_agent{profile = P} = Old, #cached_agent{profile = Q} = New) ->
	?slug(),
	PStats = case ets:lookup(stats_cache, {profile, P}) of
		[] ->
			#agent_stats{};
		[{{profile, P}, PS}] ->
			PS
	end,
	Qstast = case ets:lookup(stats_cache, {profile, Q}) of
		[] ->
			#agent_stats{};
		[{{profile, Q}, Qs}] ->
			Qs
	end,
	NewP = update_agent_profiles_down(Old, PStats),
	NewQ = update_agent_profiles_up(New, Qstast),
	ets:insert(stats_cache, {{profile, P}, NewP}),
	ets:insert(stats_cache, {{profile, Q}, NewQ});
update_agent_profiles(#cached_agent{profile = P} = Old, null) ->
	?slug(),
	Stats = case ets:lookup(stats_cache, {profile, P}) of
		[] ->
			#agent_stats{};
		[{{profile, P}, S}] ->
			S
	end,
	Newstasts = update_agent_profiles_down(Old, Stats),
	ets:insert(stats_cache, {{profile, P}, Newstasts});
update_agent_profiles(null, #cached_agent{profile = P} = New) ->
	?slug(),
	Stats = case ets:lookup(stats_cache, {profile, P}) of
		[] ->
			#agent_stats{};
		[{{profile, P}, S}] ->
			S
	end,
	Newstats = update_agent_profiles_up(New, Stats),
	ets:insert(stats_cache, {{profile, P}, Newstats});
update_agent_profiles(null, null) ->
	?slug(),
	?ERROR("THE IMPOSSIBLE HAS HAPPENED!", []).

update_agent_profiles_down(Old, Stats) ->
	?slug(),
	case Old#cached_agent.state of
		released ->
			Stats#agent_stats{released = Stats#agent_stats.released - 1};
		wrapup ->
			Stats#agent_stats{wrapup = Stats#agent_stats.wrapup - 1};
		idle ->
			Stats#agent_stats{available = Stats#agent_stats.available - 1};
		_ ->
			Stats#agent_stats{oncall = Stats#agent_stats.oncall - 1}
	end.

update_agent_profiles_up(New, Mid) ->
	?slug(),
	case New#cached_agent.state of
		released ->
			Mid#agent_stats{released = Mid#agent_stats.released + 1};
		wrapup ->
			Mid#agent_stats{wrapup = Mid#agent_stats.wrapup + 1};
		idle ->
			Mid#agent_stats{available = Mid#agent_stats.available + 1};
		_ ->
			Mid#agent_stats{oncall = Mid#agent_stats.oncall + 1}
	end.

transform_event({set, {{media, Id}, Hp, Details, Time}}, [], QueueCache, AgentCache) ->
	?slug(),
	{State, Statedata} = case proplists:get_value(queue, Details) of
		undefined ->
			{ivr, null};
		Nom ->
			Group = proplists:get_value(Nom, QueueCache),
			{queue, {Group, Nom}}
	end,
	History = case State of
		ivr ->
			[{Time, ivr, null}];
		queue ->
			[{Time, queue, Statedata}]
	end,
	#client{id = ClientId, label = ClientLabel} = proplists:get_value(client, Details),
	RecInit = #cached_media{
		id = Id,
		time = Time,
		client_id = ClientId,
		client_label = ClientLabel,
		direction = proplists:get_value(direction, Details, inbound),
		state = State,
		statedata = Statedata,
		endstate = inprogress,
		history = History
	},
	RecInit#cached_media{json = media_to_json(RecInit)};
transform_event({set, {{media, Id}, Hp, Details, Time}}, [#cached_media{state = OldState, statedata = OldStateData} = OldRec], QueueCache, AgentCache) ->
	?slug(),
	% movements:
	% ivr -> queue
	% ivr -> drop (though with a set, that's impossible)
	% queue -> queue
	% queue -> agent
	% queue -> drop
	% agent -> agent
	% agent -> queue
	% agent -> drop
	{NewState, NewStateData, Change} = determine_event(OldState, OldStateData, Details, QueueCache, AgentCache),
	case Change of
		nochange ->
			OldRec#cached_media{time = Time};
		changed ->
			{Endstate, History} = case {OldState, NewState} of
				{ivr, ended} -> 
					{ivrabandoned, OldRec#cached_media.history ++ [{Time, ended, ivrabandoned}]};
				{ivr, queue} ->
					{OldRec#cached_media.endstate, OldRec#cached_media.history ++ [{Time, queue, NewStateData}]};
				{queue, ended} ->
					{queueabandoned, OldRec#cached_media.history ++ [{Time, ended, queueabandoned}]};
				{queue, queue} ->
					{OldRec#cached_media.endstate, OldRec#cached_media.history ++ [{Time, queue, NewStateData}]};
				{queue, agent} ->
					{handled, OldRec#cached_media.history ++ [{Time, agent, NewStateData}]};
				{agent, agent} ->
					{handled, OldRec#cached_media.history ++ [{Time, agent, NewStateData}]};
				{agent, queue} ->
					{handled, OldRec#cached_media.history ++ [{Time, queue, NewStateData}]};
				{agent, ended} ->
					{handled, OldRec#cached_media.history ++ [{Time, ended, handled}]}
			end,
			Midrec = OldRec#cached_media{state = NewState, statedata = NewStateData, endstate = Endstate, history = History},
			Midrec#cached_media{time = Time, json = media_to_json(Midrec)}
	end;
transform_event({drop, {media, Id}}, [#cached_media{state = OldState, statedata = OldStateData} = OldRec], _QueueCache, _AgentCache) ->
	?slug(),
	% if dropping we were:
	% queue
	% ivr
	% agent
	{EndState, History} = case {OldState, OldRec#cached_media.endstate} of
		{ivr, inprogress} ->
			{ivrabandoned, OldRec#cached_media.history ++ [{util:now(), ended, ivrabandoned}]};
		{queue, inprogress} ->
			{queueabandoned, OldRec#cached_media.history ++ [{util:now(), ended, queueabandoned}]};
		{ended, _} ->
			{OldRec#cached_media.statedata, OldRec#cached_media.history};
		{_, handled} ->
			{handled, OldRec#cached_media.history ++ [{util:now(), ended, handled}]}
	end,
	Midrec = OldRec#cached_media{state = ended, statedata = EndState, endstate = EndState, history = History},
	Midrec#cached_media{time = util:now(), json = media_to_json(Midrec)};
transform_event({set, {{agent, Id}, _Hp, Details, Time}}, _, _QueueCache, _AgentCache) ->
	?slug(),
	State = proplists:get_value(state, Details),
	StateData = proplists:get_value(statedata, Details),
	Midrec = #cached_agent{
		id = Id, 
		login = proplists:get_value(login, Details, "Unknown"),
		profile = proplists:get_value(profile, Details, "default"),
		state = State,
		statedata = simplify_agent_state(State, StateData),
		time = Time
	},
	Midrec#cached_agent{time = Time, json = agent_to_json(Midrec)}.

simplify_agent_state(released, {Nom, _, _}) ->
	Nom;
simplify_agent_state(warmtransfer, {onhold, #call{client = Client} = Call, calling, Calling}) ->
	{Call#call.id, Client#client.id, Client#client.label, Call#call.type, Calling};
simplify_agent_state(_, #call{client = Client, id = Callid} = Call) ->
	{Callid, Client#client.id, Client#client.label, Call#call.type};
simplify_agent_state(_, _) ->
	null.

media_to_json(Rec) ->
	[State, Statedata] = media_statedata_to_json(Rec#cached_media.state, Rec#cached_media.statedata),
	{struct, [
		{<<"id">>, list_to_binary(Rec#cached_media.id)},
		{<<"clientId">>, case Rec#cached_media.client_id of undefined -> <<"undefined">>; _ -> list_to_binary(Rec#cached_media.client_id) end},
		{<<"clientLabel">>, case Rec#cached_media.client_label of undefined -> undefined; _ -> list_to_binary(Rec#cached_media.client_label) end},
		{<<"direction">>, Rec#cached_media.direction},
		State,
		Statedata,
		{<<"endState">>, Rec#cached_media.endstate},
		{<<"history">>, media_history_to_json(Rec#cached_media.history)}
	]}.

media_statedata_to_json(State, Data) ->
	case {State, Data} of
		{queue, {Group, Queue}} ->
			[{<<"state">>, queue}, {<<"statedata">>, {struct, [{<<"group">>, list_to_binary(Group)}, {<<"queue">>, list_to_binary(Queue)}]}}];
		{agent, {Prof, Nom}} ->
			[{<<"state">>, agent}, {<<"statedata">>, {struct, [{<<"profile">>, list_to_binary(Prof)}, {<<"agent">>, list_to_binary(Nom)}]}}];
		{S, _} ->
			[{<<"state">>, S}, {<<"statedata">>, null}]
	end.

media_history_to_json(L) ->
	media_history_to_json(L, []).

media_history_to_json([], Acc) ->
	lists:reverse(Acc);
media_history_to_json([{T, S, D} | Tail], Acc) ->
	[State, Data] = media_statedata_to_json(S, D),
	Newhead = {struct, [
		{timestamp, T},
		State,
		Data
	]},
	media_history_to_json(Tail, [Newhead | Acc]).

agent_to_json(Rec) ->
	?DEBUG("cached rec:  ~p", [Rec]),
	Statedata = case Rec#cached_agent.statedata of
		null ->
			null;
		{Cid, Clientid, Clientlabel, Type, Calling} ->
			?slug(),
			{struct, [
				{onhold, Cid},
				{<<"clientId">>, case Clientid of undefined -> undefined; _ -> list_to_binary(Clientid) end},
				{<<"clientLabel">>, case Clientlabel of undefined -> undefined; _ -> list_to_binary(Clientlabel) end},
				{calling, list_to_binary(Calling)}
			]};
		{Cid, undefined, undefined, Type} ->
			?slug(),
			{struct, [
				{<<"callId">>, list_to_binary(Cid)},
				{<<"clientId">>, undefined},
				{<<"clientLabel">>, undefined},
				{type, Type}
			]};
		{Cid, Cliid, Clilab, Type} ->
			?slug(),
			{struct, [
				{<<"callId">>, list_to_binary(Cid)},
				{<<"clientId">>, list_to_binary(Cliid)},
				{<<"clientLabel">>, list_to_binary(Clilab)},
				{type, Type}
			]};
		Nom ->
			list_to_binary(Nom)
	end,
	{struct, [
		{id, list_to_binary(Rec#cached_agent.id)},
		{login, list_to_binary(Rec#cached_agent.login)},
		{profile, list_to_binary(Rec#cached_agent.profile)},
		{state, Rec#cached_agent.state},
		{statedata, Statedata},
		{timestamp, Rec#cached_agent.time}
	]}.

determine_event(ivr, null, Details, QueueCache, _AgentCache) ->
	case proplists:get_value(queue, Details) of
		undefined ->
			{ivr, null, nochange};
		Queue ->
			Group = proplists:get_value(Queue, QueueCache, "default"),
			{queue, {Group, Queue}, changed}
	end;
determine_event(queue, {Group, Queue}, Details, QueueCache, AgentCache) ->
	case {proplists:get_value(queue, Details), proplists:get_value(agent, Details)} of
		{undefined, undefined} ->
			{ended, null, changed};
		{Queue, undefined} ->
			{queue, {Group, Queue}};
		{NewQueue, undefined, nochange} ->
			NewGroup = proplists:get_value(Queue, QueueCache, "default"),
			{queue, {NewGroup, NewQueue}, changed};
		{undefined, Agent} ->
			Profile = proplists:get_value(Agent, AgentCache, "default"),
			{agent, {Profile, Agent}, changed}
	end;
determine_event(agent, {Profile, Agent}, Details, QueueCache, AgentCache) ->
	case {proplists:get_value(queue, Details), proplists:get_value(agent, Details)} of
		{undefined, undefined} ->
			{ended, null, changed};
		{Queue, undefined} ->
			Group = proplists:get_value(Queue, QueueCache, "default"),
			{queue, {Group, Queue}, changed};
		{undefined, Agent} ->
			{agent, {Profile, Agent}, nochange};
		{undefined, NewAgent} ->
			NewProfile = proplists:get_value(Agent, AgentCache, "default"),
			{agent, {NewProfile, NewAgent}, changed}
	end;
determine_event(ended, D, _, _, _) ->
	% I am not going to dignify this with a response.
	{ended, D, nochange}.

write_output(Interval, QueueCache, AgentCache) ->
	% clients
	% queues
	% agents
	Clients = get_clients_json(),
	Queues = get_queues_json(),
	Agents = get_agents_json(),
	Raws = get_medias_json(),
	Json = {struct, [
		{<<"writeTime">>, util:now()},
		{<<"writeInterval">>, Interval},
		{clients, Clients},
		{queues, Queues},
		{<<"agentProfiles">>, Agents},
		{<<"rawData">>, Raws}
	]},
	{ok, File} = file:open("www/dynamic/all.json", [write, binary]),
	file:write(File, mochijson2:encode(Json)).
	
get_clients_json() ->
	qlc:e(qlc:q([get_client_json(Id, Label, Stats) || {{client, Id, Label}, Stats} <- ets:table(stats_cache)])).
	
get_client_json(Id, Label, Stats) ->
	JMedias = qlc:e(qlc:q([list_to_binary(Callid) || #cached_media{id = Callid, client_id = ClientId} <- ets:table(cached_media), ClientId =:= Id])),
	JStats = stats_to_proplist(Stats),
	Jdata = [{id, case Id of undefined -> undefined; _ -> list_to_binary(Id) end},
		{label, case Label of undefined -> undefined; _ -> list_to_binary(Label) end}],
	Props = lists:append([JStats, Jdata, [{medias, JMedias}]]),
	{struct, Props}.

stats_to_proplist(Stats) when is_record(Stats, media_stats) ->
	[{inbound, Stats#media_stats.inbound},
	{outbound, Stats#media_stats.outbound},
	{inivr, Stats#media_stats.inivr},
	{inqueue, Stats#media_stats.inqueue},
	{abandoned, Stats#media_stats.abandoned}];
stats_to_proplist(Stats) when is_record(Stats, agent_stats) ->
	[{total, Stats#agent_stats.total},
	{available, Stats#agent_stats.available},
	{oncall, Stats#agent_stats.oncall},
	{released, Stats#agent_stats.released},
	{wrapup, Stats#agent_stats.wrapup}].

get_queues_json() ->
	qlc:e(qlc:q([get_queue_json(Queue, Stats) || {{queue, Queue}, Stats} <- ets:table(stats_cache)])).

get_queue_json(Qnom, Stats) ->
	JStats = stats_to_proplist(Stats),
	JMedias = qlc:e(qlc:q([list_to_binary(Callid) || #cached_media{id = Callid, state = queue, statedata = {_, Qname}} <- ets:table(cached_media), Qname =:= Qnom])),
	Props = lists:append([[{<<"name">>, list_to_binary(Qnom)}, {medias, JMedias}], JStats]),
	{struct, Props}.

get_agents_json() ->
	qlc:e(qlc:q([get_agents_json(Pname, Stats) || {{profile, Pname}, Stats} <- ets:table(stats_cache)])).

get_agents_json(Pname, Stats) ->
	JStats = stats_to_proplist(Stats),
	JAgents = qlc:e(qlc:q([Json || #cached_agent{profile = Profile, json = Json} <- ets:table(cached_agent), Pname =:= Profile])),
	Props = lists:append([[{<<"name">>, list_to_binary(Pname)}, {agents, JAgents}], JStats]),
	{struct, Props}.

get_medias_json() ->
	Props = qlc:e(qlc:q([{list_to_binary(Id), Json} || #cached_media{id = Id, json = Json} <- ets:table(cached_media)])),
	{struct, Props}.

-ifdef(TEST).

cache_event_test_() ->
	{setup,
	fun() ->
		dets:open_file(?DETS, [])
	end,
	fun(_) ->
		{foreach,
		fun() ->
			dets:delete_all_objects(?DETS)
		end,
		fun(_) ->
			dets:delete_all_objects(?DETS)
		end,
		[{"droping a media properly ends it in dets",
		fun() ->
			Media = {{media, "media"}, util:now() - 120, [], [], {inbound, [{queued, util:now() - 60}]}},
			dets:insert(?DETS, Media),
			?assertMatch([{queued, _}, {ended, _}], element(2, element(5, cache_event({drop, {media, "media"}})))),
			?assertMatch([{queued, _}, {ended, _}], element(2, element(5, lists:nth(1, dets:lookup(?DETS, {media, "media"})))))
		end},
		{"dropping a non existant media",
		fun() ->
			?assertEqual({{media, "media"}, none}, cache_event({drop, {media, "media"}}))
		end},
		{"dropping an agent",
		fun() ->
			dets:insert(?DETS, {{agent, "agent"}, 10, [], [], undefined}),
			?assertEqual({{agent, "agent"}, none}, cache_event({drop, {agent, "agent"}})),
			?assertEqual([], dets:lookup(?DETS, {agent, "agent"}))
		end},
		{"setting a brand new inbound media in ivr",
		fun() ->
			cache_event({set, {{media, "media"}, [], [{direction, inbound}], util:now()}}),
			[Obj] = dets:lookup(?DETS, {media, "media"}),
			?assertMatch({inbound, [{ivr, _}]}, element(5, Obj))
		end},
		{"setting a new inbound media in queue",
		fun() ->
			cache_event({set, {{media, "media"}, [], [{direction, inbound}, {queue, "queue"}], util:now()}}),
			[Obj] = dets:lookup(?DETS, {media, "media"}),
			?assertMatch({inbound, [{queued, _}]}, element(5, Obj))
		end},
		{"setting a new inbound media in queue respects queued_at",
		fun() ->
			cache_event({set, {{media, "media"}, [], [{direction, inbound}, {queue, "queue"}, {queued_at, {timestamp, 1337}}], util:now()}}),
			[Obj] = dets:lookup(?DETS, {media, "media"}),
			?assertMatch({inbound, [{queued, 1337}]}, element(5, Obj))
		end},
		{"setting a new outbound media", 
		fun() ->
			cache_event({set, {{media, "media"}, [], [{direction, outbound}], 120}}),
			[Obj] = dets:lookup(?DETS, {media, "media"}),
			?assertMatch({outbound, [{handled, 120}]}, element(5, Obj))
		end},
		{"setting existing media with no agent, no queue, no history",
		fun() ->
			dets:insert(?DETS, {{media, "media"}, 120, [], [], {inbound, []}}),
			cache_event({set, {{media, "media"}, [{hp, 50}], [{detail, "data"}], util:now()}}),
			[Obj] = dets:lookup(?DETS, {media, "media"}),
			?assertMatch({{media, "media"}, 120, [{hp, 50}], [{detail, "data"}], {inbound, [{ivr, _}]}}, Obj)
		end},
		{"setting existing media with no agent, no queue, ivr history",
		fun() ->
			dets:insert(?DETS, {{media, "media"}, 120, [], [], {inbound, [{ivr, 130}]}}),
			cache_event({set, {{media, "media"}, [{hp, 50}], [{detail, "data"}], 140}}),
			[Obj] = dets:lookup(?DETS, {media, "media"}),
			?assertEqual({{media, "media"}, 120, [{hp, 50}], [{detail, "data"}], {inbound, [{ivr, 130}]}}, Obj)
		end},
		{"setting existing media with no agent, queue, ivr history",
		fun() ->
			dets:insert(?DETS, {{media, "media"}, 120, [], [], {inbound, [{ivr, 130}]}}),
			cache_event({set, {{media, "media"}, [{hp, 50}], [{queue, "queue"}], 140}}),
			[Obj] = dets:lookup(?DETS, {media, "media"}),
			?assertEqual({{media, "media"}, 120, [{hp, 50}], [{queue, "queue"}], {inbound, [{ivr, 130}, {queued, 140}]}}, Obj)
		end},
		{"Setting existing media with no agent, same queue data, queue history",
		fun() ->
			dets:insert(?DETS, {{media, "media"}, 120, [], [{queue, "queue"}, {queued_at, 130}], {inbound, [{queued, 130}]}}),
			cache_event({set, {{media, "media"}, [{hp, 50}], [{queue, "queue"}, {queued_at, {timestamp, 130}}], 140}}),
			[Obj] = dets:lookup(?DETS, {media, "media"}),
			?assertEqual({{media, "media"}, 120, [{hp, 50}], [{queue, "queue"}, {queued_at, {timestamp, 130}}], {inbound, [{queued, 130}]}}, Obj)
		end},
%		{"Setting existing media with no agent, same queue data, queue history, no queued_at",
%		fun() ->
%			dets:insert(?DETS, {{media, "media"}, 120, [], [{queue, "queue"}, {queued_at, 130}], {inbound, [{queued, 130}]}}),
%			cache_event({set, {{media, "media"}, [{hp, 50}], [{queue, "queue"}], 140}}),
%			[Obj] = dets:lookup(?DETS, {media, "media"}),
%			?assertEqual({{media, "media"}, 120, [{hp, 50}], [{queue, "queue"}], {inbound, [{queued, 130}]}}, Obj)
%		end},
		{"setting existing media with no agent, new queue data, queue history",
		fun() ->
			dets:insert(?DETS, {{media, "media"}, 120, [], [{queue, "oldqueue"}], {inbound, [{queue, 130}]}}),
			cache_event({set, {{media, "media"}, [{hp, 50}], [{queue, "newqueue"}], 140}}),
			[Obj] = dets:lookup(?DETS, {media, "media"}),
			?assertEqual({{media, "media"}, 120, [{hp, 50}], [{queue, "newqueue"}], {inbound, [{queue, 130}, {queued, 140}]}}, Obj)
		end},
		{"setting existing media, no agent, new queue, queue history and timestamp",
		fun() ->
			dets:insert(?DETS, {{media, "media"}, 120, [], [{queue, "oldqueue"}], {inbound, [{queue, 130}]}}),
			cache_event({set, {{media, "media"}, [{hp, 50}], [{queue, "newqueue"}, {queued_at, {timestamp, 140}}], 140}}),
			[Obj] = dets:lookup(?DETS, {media, "media"}),
			?assertEqual({{media, "media"}, 120, [{hp, 50}], [{queue, "newqueue"}, {queued_at, {timestamp, 140}}], {inbound, [{queue, 130}, {queued, 140}]}}, Obj)
		end},
		{"setting existing media with no agent, same queue but new queued_at, queue history",
		fun() ->
			dets:insert(?DETS, {{media, "media"}, 120, [], [{queue, "queue"}], {inbound, [{queued, 130}]}}),
			cache_event({set, {{media, "media"}, [{hp, 50}], [{queue, "queue"}, {queued_at, {timestamp, 120}}], 140}}),
			[Obj] = dets:lookup(?DETS, {media, "media"}),
			?assertEqual({{media, "media"}, 120, [{hp, 50}], [{queue, "queue"}, {queued_at, {timestamp, 120}}], {inbound, [{queued, 120}]}}, Obj)
		end},
		{"setting existing media, no agent, same queue, new queued_at, ivr history",
		fun() ->
			dets:insert(?DETS, {{media, "media"}, 110, [], [{queue, "queue"}], {inbound, [{ivr, 110}, {queued, 130}]}}),
			cache_event({set, {{media, "media"}, [{hp, 50}], [{queue, "queue"}, {queued_at, {timestamp, 120}}], 140}}),
			[Obj] = dets:lookup(?DETS, {media, "media"}),
			?assertEqual({{media, "media"}, 110, [{hp, 50}], [{queue, "queue"}, {queued_at, {timestamp, 120}}], {inbound, [{ivr, 110}, {queued, 120}]}}, Obj)
		end},
		{"Setting existing media with agent, no queue, queue history",
		fun() ->
			dets:insert(?DETS, {{media, "media"}, 120, [], [], {inbound, [{queued, 130}]}}),
			cache_event({set, {{media, "media"}, [{hp, 50}], [{agent, "agent"}], 140}}),
			[Obj] = dets:lookup(?DETS, {media, "media"}),
			?assertEqual({{media, "media"}, 120, [{hp, 50}], [{agent, "agent"}], {inbound, [{queued, 130}, {handled, 140}]}}, Obj)
		end},
		{"Setting existing media with agent and queue",
		fun() ->
			dets:insert(?DETS, {{media, "media"}, 120, [], [], {inbound, [{ivr, 130}]}}),
			cache_event({set, {{media, "media"}, [{hp, 50}], [{agent, "agent"}, {queue, "queue"}], 140}}),
			[Obj] = dets:lookup(?DETS, {media, "media"}),
			?assertEqual({{media, "media"}, 120, [], [], {inbound, [{ivr, 130}]}}, Obj)
		end}]}
	end}.




%
%
%
%cache_event({set, {{media, _Id} = Key, EventHp, EventDetails, EventTime}}) ->
%	case dets:lookup(?DETS, Key) of
%		[{Key, Time, _Hp, _Details, {inbound, History}}] ->
%			case {proplists:get_value(queue, EventDetails), proplists:get_value(agent, EventDetails), History} of
%				{undefined, undefined, []} ->
%					% just update the hp and details.
%					Newrow = {Key, Time, EventHp, EventDetails, {inbound, [{ivr, EventTime}]}},
%					dets:insert(?DETS, Newrow),
%					Newrow;
%				{undefined, undefined, _List} ->
%					% either death in ivr or queue, can be figured out later.
%					% let the drop handle the ended time.
%					Newrow = {Key, Time, EventHp, EventDetails, {inbound, History}},
%					dets:insert(?DETS, Newrow),
%					Newrow;
%				{undefined, _Agent, List} when length(List) > 0, length(List) < 3 ->
%					Newrow = {Key, Time, EventHp, EventDetails, {inbound, History ++ [{handled, EventTime}]}},
%					dets:insert(?DETS, Newrow),
%					Newrow;
%				{_Queue, undefined, Hlist} ->
%					Newhist = case lists:reverse(Hlist) of
%						[{ivr, _} | _] ->
%							Hlist ++ [{queued, EventTime}];
%						[{queued, _} | _] ->
%							?WARNING("Possible corrupted history for ~p", [Key]),
%							Hlist;
%						[] ->
%							[{queued, EventTime}];
%						_ ->
%							?WARNING("Possible corrupted history for ~p (~p)", [Key, Hlist]),
%							Hlist
%					end,
%					Newrow = {Key, Time, EventHp, EventDetails, {inbound, Newhist}},
%					dets:insert(?DETS, Newrow),
%					Newrow;
%				{Queue, Agent, _} when Queue =/= undefined, Agent =/= undefined ->
%					?WARNING("both agent and queue defined, ignoring (~p)", [Key]),
%					none
%			end;
%		[{Key, Time, _Hp, _Details, History}] ->
%			% either undefined or outbound history, blind update.
%			Newrow = {Key, Time, EventHp, EventDetails, History},
%			dets:insert(?DETS, Newrow),
%			Newrow;
%		[] ->
%			case {proplists:get_value(queue, EventDetails), proplists:get_value(direction, EventDetails)} of
%				{undefined, outbound} ->
%					Newrow = {Key, EventTime, EventHp, EventDetails, outbound},
%					dets:insert(?DETS, Newrow),
%					Newrow;
%				{undefined, inbound} ->
%					?INFO("Didn't find queue, but still inbound.  Assuming it's in ivr call:  ~p", [Key]),
%					Newrow = {Key, EventTime, EventHp, EventDetails, {inbound, [{ivr, EventTime}]}},
%					dets:insert(?DETS, Newrow),
%					Newrow;
%				{_Queue, inbound} ->
%					% guess it went right to queue.  /shrug.
%					Newrow = {Key, EventTime, EventHp, EventDetails, {inbound, [{queued, EventTime}]}},
%					dets:insert(?DETS, Newrow),
%					Newrow
%			end
%	end;
%cache_event({set, {{agent, _Id} = Key, EventHp, EventDetails, EventTime}}) ->
%	case dets:lookup(?DETS, Key) of
%		[] ->
%			dets:insert(?DETS, {Key, EventTime, EventHp, EventDetails, undefined}),
%			{Key, EventTime, EventHp, EventDetails, undefined};
%		[{Key, Time, Hp, Details, undefined}] ->
%			dets:insert(?DETS, {Key, Time, EventHp, EventDetails, undefined}),
%			{Key, Time, Hp, Details, undefined}
%	end.
%
%
%
%
%



sort_medias_test_() ->
	{setup,
	fun() ->
		Map = fun(List) ->
			lists:map(fun(E) ->
				element(1, E)
			end, List)
		end,
		Map
	end,
	fun(Map) ->
		[{"time",
		fun() ->
			Medias = [
				{2, data, data, [{queued, 200}], data},
				{3, data, data, [{queued, 300}], data},
				{1, data, data, [{queued, 100}], data}
			],
			Expected = [1, 2, 3],
			?assertEqual(Expected, Map(sort_medias(Medias)))
		end},
		{"priority",
		fun() ->
			Medias = [
				{3, data, data, [{priority, 30}], data},
				{1, data, data, [{priority, 10}], data},
				{2, data, data, [{priority, 20}], data}
			],
			Expected = [1, 2, 3],
			?assertEqual(Expected, Map(sort_medias(Medias)))
		end},
		{"time and priority",
		fun() ->
			Medias = [
				{2, data, data, [{priority, 10}, {queued, 200}], data},
				{4, data, data, [{priority, 20}, {queued, 200}], data},
				{1, data, data, [{priority, 10}, {queued, 100}], data},
				{3, data, data, [{priority, 20}, {queued, 100}], data}
			],
			Expected = [1, 2, 3, 4],
			?assertEqual(Expected, Map(sort_medias(Medias)))
		end}]
	end}.
				

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
			{{media, "media-c1-q1"}, 5, [], [{queue, "queue1"}, {client, Client1}], {inbound, [{queued, 5}]}},
			{{media, "media-c1-q2"}, 5, [], [{queue, "queue2"}, {client, Client1}], {inbound, [{queued, 5}]}},
			{{media, "media-c1-q3"}, 5, [], [{queue, "queue3"}, {client, Client1}], {inbound, [{queued, 5}]}},
			{{media, "media-c1-q4"}, 5, [], [{queue, "queue4"}, {client, Client1}], {inbound, [{queued, 5}]}},
			{{media, "media-c2-q1"}, 5, [], [{queue, "queue1"}, {client, Client2}], {inbound, [{queued, 5}]}},
			{{media, "media-c2-q2"}, 5, [], [{queue, "queue2"}, {client, Client2}], {inbound, [{queued, 5}]}},
			{{media, "media-c2-q3"}, 5, [], [{queue, "queue3"}, {client, Client2}], {inbound, [{queued, 5}]}},
			{{media, "media-c2-q4"}, 5, [], [{queue, "queue4"}, {client, Client2}], {inbound, [{queued, 5}]}},
			{{media, "media-c1-a1"}, 5, [], [{agent, "agent1"}, {client, Client1}], {inbound, [{queued, 5}, {handled, 6}]}},
			{{media, "media-c2-a2"}, 5, [], [{agent, "agent2"}, {client, Client2}], {inbound, [{queued, 5}, {handled, 6}]}},
			{{agent, "agent1"}, 5, [], [{profile, "profile1"}], undefined},
			{{agent, "agent2"}, 5, [], [{profile, "profile2"}], undefined}
		],
		DoFilter = fun(Filter) ->
			Test = fun(R, Acc) ->
				% TODO - fix this properly
				case filter_row(Filter, R, [], []) of
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
			Out = lists:map(fun(R) -> filter_row(Filter, R, [], []) end, Rows),
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
		end},
		{"filter by age",
		fun() ->
			Filter = #filter{
				max_age = 1,
				clients = all,
				queues = all,
				queue_groups = all,
				agents = all,
				agent_profiles = all,
				nodes = all
			},
			Expected = [],
			Filtered = DoFilter(Filter),
			?assertEqual(Expected, Filtered)
		end},
		{"filter since time (like midnight)",
		fun() ->
			TheFilter = #filter{
				max_age = {since, 0},
				clients = all,
				queues = all,
				queue_groups = all,
				agents = all,
				agent_profiles = all,
				nodes = all
			},
			Now = util:now(),
			{_Date, {Hour, Min, Sec}} = erlang:localtime(),
			Diff = Sec + (Min * 60) + (Hour * 60 * 60),
			Midnight = Now - Diff,
			TheRows = [
				{{media, "pre-midnight"}, Midnight - 100, [], [], {inbound, queued}},
				{{media, "post-midnight"}, Midnight + 100, [], [], {inbound, queued}}
			],
			Expected = [{{media, "post-midnight"}, Midnight + 100, [], [], {inbound, queued}}],
			Fun = fun(R, Acc) -> 
				case filter_row(TheFilter, R, [], []) of
					true ->
						[R | Acc];
					false ->
						Acc
				end
			end,
			Got = lists:foldl(Fun, [], TheRows),
			?assertEqual(Expected, Got)
		end},
		{"filter since time (like 2 minutes after midnight)",
		fun() ->
			TheFilter = #filter{
				max_age = {since, 120},
				clients = all,
				queues = all,
				queue_groups = all,
				agents = all,
				agent_profiles = all,
				nodes = all
			},
			Now = util:now(),
			{_Date, {Hour, Min, Sec}} = erlang:localtime(),
			Diff = Sec + (Min * 60) + (Hour * 60 * 60),
			Midnight = Now - Diff,
			TheRows = [
				{{media, "pre-midnight"}, Midnight - 60, [], [], {inbound, queued}},
				{{media, "midnight"}, Midnight, [], [], {inbound, queued}},
				{{media, "post-midnight-1"}, Midnight + 60, [], [], {inbound, queued}},
				{{media, "post-midnight-3"}, Midnight + 180, [], [], {inbound, queued}}
			],
			Expected = [{{media, "post-midnight-3"}, Midnight + 180, [], [], {inbound, queued}}],
			Fun = fun(R, Acc) -> 
				case filter_row(TheFilter, R, [], []) of
					true ->
						[R | Acc];
					false ->
						Acc
				end
			end,
			Got = lists:foldl(Fun, [], TheRows),
			?assertEqual(Expected, Got)
		end}]
	end}.

qlc_test_() ->
	{setup,
	fun() ->
		dets:open_file(?DETS, []),
		dets:delete_all_objects(?DETS),
		Client1 = #client{
			id = "1",
			label = "client1"
		},
		Client2 = #client{
			id = "2",
			label = "client2"
		},
		Rows = [
			{{media, "media-c1-q1"}, 5, [], [{queue, "queue1"}, {client, Client1}], {inbound, [{queued, 10}]}},
			{{media, "media-c1-q2"}, 5, [], [{queue, "queue2"}, {client, Client1}], {inbound, [{queued, 10}]}},
			{{media, "media-c1-q3"}, 5, [], [{queue, "queue3"}, {client, Client1}], {inbound, [{queued, 10}]}},
			{{media, "media-c1-q4"}, 5, [], [{queue, "queue4"}, {client, Client1}], {inbound, [{queued, 10}]}},
			{{media, "media-c2-q1"}, 5, [], [{queue, "queue1"}, {client, Client2}], {inbound, [{queued, 10}]}},
			{{media, "media-c2-q2"}, 5, [], [{queue, "queue2"}, {client, Client2}], {inbound, [{queued, 10}]}},
			{{media, "media-c2-q3"}, 5, [], [{queue, "queue3"}, {client, Client2}], {inbound, [{queued, 10}]}},
			{{media, "media-c2-q4"}, 5, [], [{queue, "queue4"}, {client, Client2}], {inbound, [{queued, 10}]}},
			{{media, "media-c1-a1"}, 5, [], [{agent, "agent1"}, {client, Client1}], {inbound, [{queued, 10}, {handled, 20}]}},
			{{media, "media-c2-a2"}, 5, [], [{agent, "agent2"}, {client, Client2}], {inbound, [{queued, 10}, {handled, 20}]}},
			{{media, "media-undef-qq"}, 5, [], [{queue, "qq"}, {client, #client{id = undefined, label = undefined}}], {inbound, [{queued, 10}]}},
			{{agent, "agent1"}, 5, [], [{profile, "profile1"}], undefined},
			{{agent, "agent2"}, 5, [], [{profile, "profile2"}], undefined}
		],
		lists:foreach(fun(R) -> dets:insert(?DETS, R) end, Rows),
		AllFilter = #filter{
			clients = all,
			queues = all,
			queue_groups = all,
			agents = all,
			agent_profiles = all,
			nodes = all
		},
		Getids = fun({{_Type, Id}, _, _, _, _}) ->
			Id
		end,
		{AllFilter, Client1, Client2, Getids}
	end,
	fun(_) ->
		file:delete(?DETS),
		ok
	end,
	fun({AllFilter, Client1, _Client2, Getids}) ->
		[{"get medias with a given client",
		fun() ->
			Out = get_client_medias(AllFilter, Client1#client.label, [], []),
			?assertEqual(4, length(Out)),
			Expected = ["media-c1-q1", "media-c1-q2", "media-c1-q3", "media-c1-q4"],
			?assert(lists:all(fun(I) -> lists:member(I, Expected) end, lists:map(Getids, Out)))
		end},
		{"get medias with a given queue",
		fun() ->
			Out = get_queued_medias(AllFilter, "queue1", [], []),
			?assertEqual(2, length(Out)),
			Expected = ["media-c1-q1", "media-c2-q1"],
			?assert(lists:all(fun(I) -> lists:member(I, Expected) end, lists:map(Getids, Out)))
		end},
		{"get media with an 'undefined' client",
		fun() ->
			[H | _] = Out = get_client_medias(AllFilter, undefined, [], []),
			?assertEqual(1, length(Out)),
			?assertEqual({media, "media-undef-qq"}, element(1, H))
		end}]
	end}.

prune_dets_medias_test_() ->
	{setup,
	fun() ->
		dets:open_file(?DETS, [])
	end,
	fun(_) ->
		file:delete(?DETS),
		Managers = [dummy_media_manager, email_media_manager, freeswitch_media_manager],
		lists:foreach(fun(Manager) ->
			case whereis(Manager) of
				undefined ->
					ok;
				Pid ->
					exit(Pid, kill)
			end
		end,
		Managers),
		ok
	end,
	fun(_) ->
		{foreach,
		fun() ->
			dets:delete_all_objects(?DETS)
		end,
		fun(_) ->
			ok
		end,
		[{"Deleting only medias that are dead and older than a day",
		fun() ->
			Objs = [{{media, "dead"}, util:now() - ( (24 * 60 * 60) + (10 * 60) ), [], [{type, dummy}], {inbound, [{queued, 10}]}},
			{{media, "alive"}, util:now() - ( (24 * 60 * 60) + (10 * 60) ), [], [{type, dummy}], {inbound, [{queued, 10}]}}],
			dets:insert(?DETS, Objs),
			gen_server_mock:named({local, dummy_media_manager}),
			gen_server_mock:expect_call(dummy_media_manager, fun({get_media, "dead"}, _, State) ->
				{ok, none, State}
			end),
			gen_server_mock:expect_call(dummy_media_manager, fun({get_media, "alive"}, _, State) ->
				Pid = spawn(fun() -> ok end),
				{ok, {"alive", Pid}, State}
			end),
			prune_dets_medias(),
			?assertEqual([], dets:lookup(?DETS, {media, "dead"})),
			?assertEqual(1, length(dets:lookup(?DETS, {media, "alive"})))
		end},
		{"Ending dead medias younger than a day",
		fun() ->
			Objs = [{{media, "dead"}, util:now() - 10 * 60, [], [{type, dummy}], {inbound, [{queued, 10}]}},
			{{media, "alive"}, util:now() - 10 * 60, [], [{type, dummy}], {inbound, [{queued, 10}]}}],
			dets:insert(?DETS, Objs),
			gen_server_mock:named({local, dummy_media_manager}),
			gen_server_mock:expect_call(dummy_media_manager, fun({get_media, "dead"}, _, State) ->
				{ok, none, State}
			end),
			gen_server_mock:expect_call(dummy_media_manager, fun({get_media, "alive"}, _, State) ->
				Pid = spawn(fun() -> ok end),
				{ok, {"alive", Pid}, State}
			end),
			prune_dets_medias(),
			[Deadres] = dets:lookup(?DETS, {media, "dead"}),
			[AliveRes] = dets:lookup(?DETS, {media, "alive"}),
			?DEBUG("dead:  ~p;  Alive:  ~p", [Deadres, AliveRes]),
			?assertNot(undefined == proplists:get_value(ended, element(2, element(5, Deadres)))),
			?assertEqual(undefined, proplists:get_value(ended, element(2, element(5, AliveRes))))
		end},
		{"Types other than dummy",
		fun() ->
			Fpid = fun() -> spawn(fun() -> ok end) end,
			Objs = [
				{{media, "dead-old-mail"}, util:now() - (24 * 60 * 60 + 10 * 60), [], [{type, email}], {inbound, [{queued, 10}]}},
				{{media, "dead-old-voice"}, util:now() - (24 * 60 * 60 + 10 * 60), [], [{type, voice}], {inbound, [{queued, 10}]}},
				{{media, "dead-new-mail"}, util:now() - (10 * 60), [], [{type, email}], {inbound, [{queued, 10}]}},
				{{media, "dead-new-voice"}, util:now() - (10 * 60), [], [{type, voice}], {inbound, [{queued, 10}]}},
				{{media, "live-old-mail"}, util:now() - (24 * 60 * 60 + 10 * 60), [], [{type, email}], {inbound, [{queued, 10}]}},
				{{media, "live-new-voice"}, util:now() - (10 * 60), [], [{type, voice}], {inbound, [{queued, 10}]}}
			],
			dets:insert(?DETS, Objs),
			gen_server_mock:named({local, freeswitch_media_manager}),
			gen_server_mock:named({local, email_media_manager}),
			Fsfun = fun({get_media, Id}, _From, State) ->
				case Id of
					"live-new-voice" ->
						{ok, Fpid(), State};
					_ ->
						{ok, none, State}
				end
			end,
			Emailfun = fun({get_media, Id}, _From, State) ->
				case Id of
					"live-old-mail" ->
						{ok, Fpid(), State};
					_ ->
						{ok, none, State}
				end
			end,
			gen_server_mock:expect_call(freeswitch_media_manager, Fsfun),
			gen_server_mock:expect_call(freeswitch_media_manager, Fsfun),
			gen_server_mock:expect_call(freeswitch_media_manager, Fsfun),
			gen_server_mock:expect_call(email_media_manager, Emailfun),
			gen_server_mock:expect_call(email_media_manager, Emailfun),
			gen_server_mock:expect_call(email_media_manager, Emailfun),
			prune_dets_medias(),
			Expectedids = ["dead-new-mail", "dead-new-voice", "live-old-mail", "live-new-voice"],
			Gotids = qlc:e(qlc:q([Id || {{_Type, Id}, _, _, _, _} <- dets:table(?DETS)])),
			?assertEqual(lists:sort(Expectedids), lists:sort(Gotids))
		end}]}
	end}.

init_test_() ->
	[{"no passive cache, but cpx monitor has old data",
	fun() ->
		dets:delete_all_objects(?DETS),
		{ok, Mon} = gen_leader_mock:start(cpx_monitor),
		Monmedias = [{{media, "media1"}, [{inqueue, 100.0}], [{queued_at, {timestamp,  util:now() - 86460}}, {queue, "a_queue"}, {direction, inbound}]},
		{{media, "media2"}, [{inqueue, 100.0}], [{queued_at, {timestamp, util:now() - 86460}}, {queue, "b_queue"}, {direction, inbound}]}],
		Expectfun = fun({get, What}, _From, State, _Elec) ->
			case What of
				media ->
					{ok, {ok, Monmedias}, State};
				_ ->
					{ok, {ok, []}, State}
			end
		end,
		gen_leader_mock:expect_leader_call(Mon, Expectfun),
		gen_leader_mock:expect_leader_call(Mon, Expectfun),
		gen_leader_mock:expect_leader_cast(Mon, fun({subscribe, _Pid, _Fun}, _State, _Elec) ->
			ok
		end),
		Getmedia = fun(_, _, State) ->
			P = spawn(fun() -> ok end),
			{ok, P, State}
		end,
		{ok, MMmock} = gen_server_mock:named({local, dummy_media_manager}),
		gen_server_mock:expect_call(dummy_media_manager, Getmedia),
		gen_server_mock:expect_call(dummy_media_manager, Getmedia),
		{ok, _State} = init([]),
		?assertEqual(2, length(qlc:e(qlc:q([X || X <- dets:table(?DETS)])))),
		[Media1, Media2] = qlc:e(qlc:q([X || X <- dets:table(?DETS)])),
		?DEBUG("m1:  ~p;  m2:  ~p", [Media1, Media2]),
		gen_leader_mock:stop(Mon),
		gen_server_mock:stop(MMmock)
	end}].

% commented out because the file this is depdant on is not being put into
% git.  If someone wants to make it portible and useful, go to it.
%goober_test_() ->
%	{ok, [Medias]} = file:consult("mediasdump.hrl"),
%	goober_gen(Medias).
%
%goober_gen([]) ->
%	[];
%goober_gen([Media | Tail]) ->
%	Fun = fun() ->
%		dets:delete_all_objects(?DETS),
%		{ok, Mon} = gen_leader_mock:start(cpx_monitor),
%		Expectfun = fun({get, What}, _From, State, _Elec) ->
%			case What of
%				media ->
%					{ok, {ok, [Media]}, State};
%				_ ->
%					{ok, {ok, []}, State}
%			end
%		end,
%		gen_leader_mock:expect_leader_call(Mon, Expectfun),
%		gen_leader_mock:expect_leader_call(Mon, Expectfun),
%		gen_leader_mock:expect_leader_cast(Mon, fun({subscribe, _Pid, _Fun}, _State, _Elec) ->
%			ok
%		end),
%		Getmedia = fun(_, _, State) ->
%			P = spawn(fun() -> ok end),
%			{ok, P, State}
%		end,
%		Managername = case proplists:get_value(type, element(3, Media)) of
%			email ->
%				email_media_manager;
%			voice ->
%				freeswitch_media_manager;
%			_ ->
%				dummy_media_manager
%		end,
%		{ok, MMmock} = gen_server_mock:named({local, Managername}),
%		gen_server_mock:expect_call(Managername, Getmedia),
%		{ok, _State} = init([]),
%		?assertEqual(1, length(qlc:e(qlc:q([X || X <- dets:table(?DETS)])))),
%		%[Media1, Media2] = qlc:e(qlc:q([X || X <- dets:table(?DETS)])),
%		%?DEBUG("m1:  ~p;  m2:  ~p", [Media1, Media2]),
%		%?assert(false),
%		gen_leader_mock:stop(Mon),
%		gen_server_mock:stop(MMmock),
%		timer:sleep(10)
%	end,
%	{generator, fun() -> [Fun, goober_gen(Tail)] end}.
	

-endif.
