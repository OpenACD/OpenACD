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
%%	Micah Warren <micahw at lordnull dot com>
%%

%% @doc Module for simulating the activities within a call center.

-module(dummy_callcenter).
-author(micahw).

-behaviour(gen_server).

-define(SERVER, callcenter).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("cpx.hrl").

%% API
-export([
	start/0,
	start/1,
	start_supped/1,
	start_link/0,
	start_link/1,
	stop/0,
	stop_supped/0,
	set_option/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(conf, {
	call_frequency :: pos_integer(),
	agents :: pos_integer(),
	call_max_life :: pos_integer() | {pos_integer(), pos_integer()},
	agent_max_calls :: pos_integer() | 'infinity',
	simulation_life :: pos_integer() | 'infinity',
	queues :: [string()] | 'any',
	additional_queues = [] :: [{string, any()}],
	agent_opts :: [any()],
	call_priority :: any()
}).

-record(state, {
	conf :: #conf{},
	life_timer :: any(),
	agent_pids :: [pid()],
	agent_names :: [string()]
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================

-type(frequency() :: {pos_integer(), pos_integer()} | pos_integer()).
-type(call_frequency() :: {'call_frequency', frequency()}).
%-type(call_deviation_limit() :: {call_deviation_limit, non_neg_integer()}).
-type(call_max_life() :: {'call_max_life', frequency()}).
-type(agents() :: {'agents', pos_integer()}).
-type(agent_max_calls() :: {'agent_max_calls', frequency() | 'infinity'}).
%-type(agent_max_calls_deviation_limit() :: {agent_max_calls_deviation_limit, non_neg_integer()}).
-type(simulation_life() :: {'simulation_life', pos_integer() | 'infinity'}).
-type(queues() :: {'queues', [string()] | 'any'}).
-type(additonal_queues() :: {'additonal_queues', [{string(), [any()]}]}).
-type(agent_opts() :: {'agent_opts', [any()]}).

-type(start_option() :: 
	call_frequency() |
	call_max_life() | 
	agents() | 
	agent_max_calls() | 
	agent_opts() |
	simulation_life() | 
	queues() |
	additonal_queues()
).
-type(start_options() :: [start_option()] | {'file', string()}).

%% @doc Start the simulator with the given options.
%% <dl>
%% <dt>call_frequency: {30, 900}</dt>
%%		<dd>how many seconds to wait before putting another call into 
%%		queue.</dd>
%% <dt>call_max_life: {60, 9000}</dt>
%%		<dd>How many seconds before the call hangs up.</dd>
%% <dt>agents: 10</dt>
%%		<dd>How many agents to have running at any given time.  If an agent 
%%		reaches it's max calls, a new one it automatically started.  Initial
%%		number of calls will be 80% of this.</dd>
%% <dt>agent_max_calls: {20, 40}</dt>
%%		<dd>How many calls on average an agent will take before dying.</dd>
%% <dt>simullation_life: infinity</dt>
%%		<dd>How many minutes the the simulation will run.</dd>
%% <dt>queues: any</dt>
%%		<dd>The queues to place calls in.</dd>
%% <dt>additional_queues: `[{QName::string(), QueueOptions::list()}]'</dt>
%%		<dd>Additional queues to start, and thier config options.
%%		{@link queue_manager:add_queue/2. See `queue_manager:add_queue/2'}
%%		</dd>
%% <dt>agent_opts: []</dt>
%% 		<dd>The agent options used to start dummy agents.  A scale of 1000
%%		is automatically added, overriding whatever is set.</dd>
%% </dl>
%%
%% The following agent options are overridden:
%% <dl>
%% <dt>scale</dt>
%%		<dd>replaced by 1000, so all agent durations are in seconds.</dd>
%% </dl>
%%
%% The defaults for a dummy_agent are:
%% <ul>
%% <li>ringing: {5, 30}</li>
%% <li>oncall: {30, 900}</li>
%% <li>wrapup: {5, 120}</li>
%% <li>release_frequency: {3600, 10800}</li>
%% <li>release_percent: 1</li>
%% </ul>
%%
%% You can put the options list in a file and call start with 
%% `{file, Filename::string()}'.  Erlang will file:consult/1 the passed
%% file name and send the resulting list as the options list.  Note that
%% a file for consult expects terms seperated by '. ' (dot then whitespace).
%%
%% Example consult file:
%%	<pre>% comments start with percent sign
%%	% and are thankfully ignored.
%%	{simulation_life, infinity}.
%%	{agents, 15}.</pre>
-spec(start/1 :: (Options :: start_options()) -> {'ok', pid()}).
start({file, Consult}) ->
	{ok, Options} = file:consult(Consult),
	start(Options);
start(Options) ->
	gen_server:start({local, ?SERVER}, ?MODULE, Options, []).

-spec(start/0 :: () -> {'ok', pid()}).
start() ->
	start([]).

%% @doc.  @see start/1
-spec(start_link/1 :: (Options :: start_options()) -> {'ok', pid()}).
start_link({file, Consult}) ->
	{ok, Options} = file:consult(Consult),
	start_link(Options);
start_link(Options) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% @doc Puts this in the supervisor tree.  @see start/1
-spec(start_supped/1 :: (Options :: start_options()) -> {'ok', pid()}).
start_supped(Options) ->
	Conf = #cpx_conf{
		id = ?MODULE,
		module_name = ?MODULE,
		start_function = start_link,
		start_args = [Options]
	},
	cpx_middle_supervisor:add_with_middleman(management_sup, 3, 5, Conf).

%% @doc Stops when it be supervised.
-spec(stop_supped/0 :: () -> 'ok').
stop_supped() ->
	cpx_middle_supervisor:drop_child(management_sup, ?MODULE).

-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() ->
	start_link([]).
	
%% stops the simulation.
-spec(stop/0 :: () -> 'ok').
stop() ->
	gen_server:cast(?SERVER, stop).

%% @doc Sets the passed option.  When an agent option, overwrites only the
%% given options.
-spec(set_option/2 :: (Option :: atom(), Value :: any()) -> 'ok').
set_option(Option, Value) ->
	gen_server:cast(?SERVER, {set_option, Option, Value}).
	
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%--------------------------------------------------------------------
init(Options) ->
	?INFO("Starting with options:  ~p", [Options]),
	process_flag(trap_exit, true),
	crypto:start(),
	Protoconf = #conf{
		call_frequency = proplists:get_value(call_frequency, Options, {distribution, 110}), % 32 calls per hour
		call_max_life = proplists:get_value(call_max_life, Options, {distribution, 900}), % 15 minute calls,
		call_priority = proplists:get_value(call_priority, Options, {distribution, 20}),
		agents = proplists:get_value(agents, Options, 6),
		agent_max_calls = proplists:get_value(agent_max_calls, Options, {20, 40}),
		simulation_life = proplists:get_value(simulation_life, Options, infinity),
		queues = proplists:get_value(queues, Options, any),
		additional_queues = proplists:get_value(additional_queues, Options, []),
		agent_opts = proplists:get_value(agent_opts, Options, [
			{ringing, {distribution, 15}},
			{oncall, {distribution, 330}},
			{wrapup, {distribution, 120}},
			{release_frequency, {3600, 10800}},
			{release_percent, 1}
		])
	},
	% start any additional queues
	[queue_manager:add_queue(Qnom, Qoptions) || {Qnom, Qoptions} <- Protoconf#conf.additional_queues],
	%dmm is dummy_media_manager
	DmmOpts = [
		{call_frequency, Protoconf#conf.call_frequency},
		{call_max_life, Protoconf#conf.call_max_life},
		{start_count, proplists:get_value(start_count, Options, round(Protoconf#conf.agents * 0.8))},
		{queues, Protoconf#conf.queues},
		{call_priority, Protoconf#conf.call_priority}
	],
	case whereis(dummy_media_manager) of
		undefined ->
			dummy_media_manager:start_supervised(DmmOpts);
		_DmmPid ->
			lists:foreach(fun({Key, Val}) ->
				dummy_media_manager:set_option(Key, Val)
			end, DmmOpts),
			[dummy_media_manager ! spawn_call || _X <- lists:seq(1, proplists:get_value(start_count, DmmOpts))]
	end,
	Newagentopts = proplists_replace(scale, 1000, Protoconf#conf.agent_opts),
	Conf = Protoconf#conf{agent_opts = Newagentopts},
	Lifetime = case Conf#conf.simulation_life of
		infinity ->
			infinity;
		Number when is_integer(Number) ->
			Life = Number * 1000 * 60,
			{ok, Timer} = timer:send_after(Life, endoflife),
			Timer
	end,
%	case Conf#conf.queues of
%		any ->
%			dummy_media:q_x(Conf#conf.agents);
%		Queues ->
%			dummy_media:q_x(Conf#conf.agents, Queues)
%	end,
	Names = [make_name(Num) || Num <- lists:seq(1, Conf#conf.agents) ],
%	Spawnagent = fun(_, {Pidlist, [Nom | Tail]}) ->
%		O = spawn_agent(Conf, Nom),
%		?INFO("agent spawned:  ~p", [O]),
%		{ok, Pid} = O,
%		{[{Pid, Nom} | Pidlist], Tail}
%	end,
%	{Pidlist, Namelist} = lists:foldl(Spawnagent, {[], Names}, lists:seq(1, Conf#conf.agents)),
	spawn(fun() -> [ callcenter ! spawn_agent || _ <- lists:seq(1, Conf#conf.agents)] end),
	%Medias = lists:map(fun(_) -> [Pid] = queue_media(Conf), Pid end, lists:seq(1, round(Conf#conf.agents * 0.8))),
	%{ok, Spawncall} = timer:send_after(get_number(Conf#conf.call_frequency) * 1000, spawn_call),
	State = #state{
		life_timer = Lifetime,
		agent_pids = [],
		agent_names = Names,
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
handle_cast({set_option, agent_opts, Options}, #state{conf = Conf} = State) ->
	Agentopts = Conf#conf.agent_opts,
	F = fun({Key, Value}, Acc) ->
		proplists_replace(Key, Value, Acc)
	end,
	New = lists:foldl(F, Agentopts, Options),
	Newconf = Conf#conf{agent_opts = New},
	{noreply, State#state{conf = Newconf}};
handle_cast({set_option, simulation_life, Value}, #state{life_timer = Timer} = State) ->
	case Timer of
		undefined ->
			ok;
		_ ->
			timer:cancel(Timer)
	end,
	Newtimer = case Value of
		infinity ->
			infinity;
		_ ->
			{ok, R} = timer:send_after(Value * 60 * 1000, endoflife),
			R
	end,
	{noreply, State#state{life_timer = Newtimer}};
handle_cast({set_option, agents, Value}, #state{conf = Conf} = State) ->
	Newconf = Conf#conf{agents = Value},
	case {Value, Conf#conf.agents} of
		{X, Y} when X > Y ->
			Self = self(),
			[ Self ! spawn_agent || _ <- lists:seq(1, X - Y) ],
			{noreply, State#state{conf = Newconf}};
		_ ->
			{noreply, State#state{conf = Newconf}}
	end;
handle_cast({set_option, Key, Value}, #state{conf = Conf} = State) ->
	Newconf = case Key of
		agent_max_calls ->
			Conf#conf{agent_max_calls = Value};
		_Else ->
			dummy_media_manager:set_option(Key, Value),
			Conf
	end,
	{noreply, State#state{conf = Newconf}};
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_info(spawn_agent, #state{conf = Conf} = State) ->
	case {length(State#state.agent_pids), Conf#conf.agents} of
		{X, Y} when X < Y ->
			[Headname | Newnames] = case State#state.agent_names of
				[] ->
					[make_name(length(State#state.agent_pids) + 1)];
				Else ->
					Else
			end,
			case spawn_agent(Conf, Headname) of
				{ok, Newagentpid} ->
					Newagentlist = [{Newagentpid, Headname} | State#state.agent_pids],
					{noreply, State#state{agent_pids = Newagentlist, agent_names = Newnames}};
				OrElse ->
					?NOTICE("Retrying a failed agent start ~p due to ~p", [Headname, OrElse]),
					erlang:send_after(500, callcenter, spawn_agent),
					{noreply, State}
			end;
		{_X, _Y} ->
			{noreply, State}
	end;
handle_info({'EXIT', Pid, Why}, #state{conf = Conf} = State) ->
	case proplists:get_value(Pid, State#state.agent_pids) of
		undefined ->
			?INFO("no idea where pid ~p came from", [Pid]),
			{noreply, State};
		Nom ->
			?NOTICE("agent ~p (~p) died due to ~p", [Pid, Nom, Why]),
			[Headname | Newnames] = lists:append(State#state.agent_names, [Nom]),
			Midlist = proplists:delete(Pid, State#state.agent_pids),
			case {length(Midlist), Conf#conf.agents} of
				{X, Y} when X < Y ->
					case spawn_agent(Conf, Headname) of
						{ok, Newagentpid} ->
							Newagentlist = [{Newagentpid, Headname} | Midlist],
							{noreply, State#state{agent_pids = Newagentlist, agent_names = Newnames}};
						Else ->
							?NOTICE("Retrying a failed agent start due to ~p", [Else]),
							callcenter ! spawn_agent,
							{noreply, State}
					end;
				_ ->
					{noreply, State#state{agent_pids = Midlist, agent_names = [Headname | Newnames]}}
			end
	end;
handle_info(endoflife, State) ->
	?NOTICE("My life is over.", []),
	{stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(_Reason, #state{conf = Conf} = _State) ->
	[begin 
		Qpid = queue_manager:get_queue(Qnom),
		case Qpid of
			undefined ->
				ok;
			_ ->
				call_queue:stop(Qpid)
		end
	end || {Qnom, _Opts} <- Conf#conf.additional_queues],
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
	[{Key, Newvalue} | Midlist].


spawn_agent(#conf{agent_opts = Baseopts} = Conf, Login) ->
	Midopts = proplists:delete(maxcalls, Baseopts),
	Midopts2 = case Conf#conf.agent_max_calls of
		infinity ->
			Midopts;
		Number ->
			Rand = util:get_number(Number),
			[{maxcalls, Rand} | Midopts]
	end,
	Profiles = agent_auth:get_profiles(),
	#agent_profile{name = Profile} = lists:nth(crypto:rand_uniform(1, length(Profiles) + 1), Profiles),
	Midopts3 = proplists_replace(login, Login, Midopts2),
	Midopts4 = proplists_replace(profile, Profile, Midopts3),
	Midopts5 = proplists_replace(id, Login, Midopts4),
	agent_dummy_connection:start_link(Midopts5).

make_name(N) ->
	Listnum = integer_to_list(N),
	lists:append(Listnum, "_dummy_agent@" ++ atom_to_list(node())).



%get_number({distribution, Number}) ->
%	trunc(util:distribution(Number));
%get_number({Min, Max}) ->
%	crypto:rand_uniform(Min, Max);
%get_number(random) ->
%	get_number({0, 2000});
%get_number(Num) ->
%	Num.
