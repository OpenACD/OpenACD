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

%% @doc Module for simulating the activities within a call center.

-module(dummy_callcenter).
-author(micahw).

-behaviour(gen_server).

-define(SERVER, callcenter).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").

%% API
-export([
	start/1,
	start_link/1,
	stop/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(conf, {
	call_frequency :: pos_integer(),
	call_deviation_limit :: non_neg_integer(),
	agents :: pos_integer(),
	agent_max_calls :: pos_integer() | 'infinity',
	agent_max_calls_deviation_limit :: non_neg_integer(),
	simulation_life :: pos_integer | 'infinity',
	queues :: [string()] | 'any',
	agent_opts :: [any()]
}).

-record(state, {
	conf :: #conf{},
	life_timer,
	call_timer,
	agent_pids,
	agent_names,
	media_pids
}).

%%====================================================================
%% API
%%====================================================================

-type(frequency() :: {pos_integer(), pos_integer()} | pos_integer()).
-type(call_frequency() :: {call_frequency, frequency()}).
%-type(call_deviation_limit() :: {call_deviation_limit, non_neg_integer()}).
-type(agents() :: {agents, pos_integer()}).
-type(agent_max_calls() :: {agent_max_calls, frequency() | 'infinity'}).
%-type(agent_max_calls_deviation_limit() :: {agent_max_calls_deviation_limit, non_neg_integer()}).
-type(simulation_life() :: {simulation_life, pos_integer() | 'infinity'}).
-type(queues() :: {queues, [string()] | 'any'}).
-type(agent_opts() :: {agent_opts, [any()]}).

-type(start_option() :: 
	call_frequency() | 
	agents() | 
	agent_max_calls() | 
	simulation_life() | 
	queues()
).
-type(start_options() :: [start_option()]).

%% @doc Start the simulator with the given options.
%% * call_frequency: {60, 600}
%%		how many seconds to wait before putting another call into queue.
%% * agents: 10
%%		How many agents to have running at any given time.  If an agent 
%%		reaches it's max calls, a new one it automatically started.  Also 
%%		sets the intial number of calls in queue(s).
%% * agent_max_calls: {20, 40}
%%		How many calls on average an agent will take before dying.
%% * simullation_life: infinity
%%		How many minutes the the simulation will run.
%% * queues: any
%%		The queues to place calls in.
%% * agent_opts:  The agent options used to start dummy agents.  A scale of 1000
%%		is automatically added, overriding whatever is set.  Defaults to [].
%%
%% The following agent options are overridden:
%% * scale: replaced by 1000, so all agent durations are in seconds.
%%
%% The defaults for a dummy_agent are:
%% * ringing: {5, 30}
%% * oncall: {30, 900}
%% * wrapup: {5, 120}
%% * release_frequency: {3600, 10800}
%% * release_percent: 1

-spec(start/1 :: (Options :: start_options()) -> {'ok', pid()}).
start(Options) ->
	gen_server:start({local, ?SERVER}, ?MODULE, Options, []).

%% @doc.  @see start/1
-spec(start_link/1 :: (Options :: start_options()) -> {'ok', pid()}).
start_link(Options) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% stops the simulation.
-spec(stop/0 :: () -> 'ok').
stop() ->
	gen_server:cast(?SERVER, stop).
	
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%--------------------------------------------------------------------
init(Options) ->
	process_flag(trap_exit, true),
	crypto:start(),
	Protoconf = #conf{
		call_frequency = proplists:get_value(call_frequency, Options, {60, 600}),
		agents = proplists:get_value(agents, Options, 10),
		agent_max_calls = proplists:get_value(agent_max_calls, Options, {20, 40}),
		simulation_life = proplists:get_value(simulation_life, Options, infinity),
		queues = proplists:get_value(queues, Options, any),
		agent_opts = proplists:get_value(agent_opts, Options, [
			{ringing, {5, 30}},
			{oncall, {30, 900}},
			{wrapup, {5, 120}},
			{release_frequency, {3600, 10800}},
			{release_percent, 1}
		])
	},
	Newagentopts = proplists_replace(scale, 1000, Protoconf#conf.agent_opts),
	Conf = Protoconf#conf{agent_opts = Newagentopts},
	Lifetime = case Conf#conf.simulation_life of
		infinity ->
			infinity;
		Number ->
			Life = Number * 1000 * 60,
			{ok, Timer} = timer:send_after(Life, endoflife),
			Timer
	end,
	case Conf#conf.queues of
		any ->
			dummy_media:q_x(Conf#conf.agents);
		Queues ->
			dummy_media:q_x(Conf#conf.agents, Queues)
	end,
	Makename = fun(Num) ->
		Listnum = integer_to_list(Num),
		lists:append(Listnum, "_dummy_agent")
	end,
	Names = lists:map(Makename, lists:seq(1, Conf#conf.agents * 2)),
	Spawnagent = fun(_, {Pidlist, [Nom | Tail]}) ->
		O = spawn_agent(Conf, Nom),
		?INFO("agent spawned:  ~p", [O]),
		{ok, Pid} = O,
		{[{Pid, Nom} | Pidlist], Tail}
	end,
	{Pidlist, Namelist} = lists:foldl(Spawnagent, {[], Names}, lists:seq(1, Conf#conf.agents)),
	Medias = lists:map(fun(_) -> [Pid] = queue_media(Conf#conf.queues), Pid end, lists:seq(1, Conf#conf.agents)),
	{ok, Spawncall} = timer:send_after(get_number(Conf#conf.call_frequency) * 1000, spawn_call),
	State = #state{
		life_timer = Lifetime,
		agent_pids = Pidlist,
		agent_names = Namelist,
		media_pids = Medias,
		call_timer = Spawncall,
		conf = Conf
	},
    {ok, State}.

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
handle_info({'EXIT', Pid, Why}, #state{conf = Conf} = State) ->
	case lists:member(Pid, State#state.media_pids) of
		true ->
			Newmedias = lists:delete(Pid, State#state.media_pids),
			{noreply, State#state{media_pids = Newmedias}};
		false ->
			case proplists:get_value(Pid, State#state.agent_pids) of
				undefined ->
					?INFO("no idea where pid ~p came from", [Pid]),
					{noreply, State};
				Nom ->
					[Headname | Newnames] = lists:append(State#state.agent_names, [Nom]),
					{ok, Newagentpid} = spawn_agent(Conf, Headname),
					Newagentlist = [{Newagentpid, Headname} | State#state.agent_pids],
					{noreply, State#state{agent_pids = Newagentlist, agent_names = Newnames}}
			end
	end;
handle_info(spawn_call, #state{conf = Conf} = State) ->
	[Pid] = queue_media(Conf#conf.queues),
	Medialist = [Pid | State#state.media_pids],
	Time = get_number(Conf#conf.call_frequency),
	{ok, Timer} = timer:send_after(Time * 1000, spawn_call),
	{noreply, State#state{media_pids = Medialist, call_timer = Timer}};
handle_info(endoflife, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

proplists_replace(Key, Newvalue, Proplist) ->
	Midlist = proplists:delete(Key, Proplist),
	[{Key, Newvalue} | Proplist].

queue_media(any) ->
	dummy_media:q_x(1);
queue_media(Queues) ->
	dummy_media:q_x(1, Queues).

spawn_agent(#conf{agent_opts = Baseopts} = Conf, Login) ->
	Midopts = proplists:delete(maxcalls, Baseopts),
	Midopts2 = case Conf#conf.agent_max_calls of
		infinity ->
			Midopts;
		Number ->
			Rand = get_number(Number),
			[{maxcalls, Rand} | Midopts]
	end,
	Profiles = agent_auth:get_profiles(),
	Profile = element(1, lists:nth(crypto:rand_uniform(1, length(Profiles) + 1), Profiles)),
	Midopts3 = proplists_replace(login, Login, Midopts2),
	Midopts4 = proplists_replace(profile, Profile, Midopts3),
	Midopts5 = proplists_replace(id, Login, Midopts4),
	agent_dummy_connection:start_link(Midopts5).

get_number({Min, Max}) ->
	crypto:rand_uniform(Min, Max);
get_number(random) ->
	get_number({0, 2000});
get_number(Num) ->
	Num.
