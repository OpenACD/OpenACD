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

%% @doc Intercepts events from cpx_monitor on behalf of an odbc connection.  
%% The odbc connection is supervised by a proper supervisor which is 
%% started by this module.
%%
%% When an event comes in, it transforms it into a record which mirrors a
%% database row.  It then sends that record with a reference to the odbc
%% writer process.  It expects an ack back for each record it sends.  If
%% the writer dies, when it comes back, it resends each record it has not
%% recieved an ack for.  If the writer dies enough, it will take the sub-
%% supervisor process with it, thus requiring manual intervention.  Events
%% will continue to queue up.

-module(cpx_monitor_odbc_supervisor).

-behaviour(gen_server).

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
	-define(stop_writer(Pid), gen_server_mock:stop(whereis(test_odbc_writer))).
-else.
	-define(stop_writer(Pid), 	cpx_monitor_kgb_odbc:stop(State#state.odbc_pid)).
-endif.

-define(DEFAULT_ANI, "Unknown").

-include("log.hrl").
-include("call.hrl").
-include("cpx.hrl").
-include("odbc_kgb.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([
	start/1,
	start/2,
	start_link/1,
	start_link/2,
	stop/0,
	start_odbc/0,
	status/0
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

-type(trace_opt() :: 'trace').
-type(max_r() :: {'max_r', pos_integer()}).
-type(max_t() :: {'max_t', pos_integer()}). % seconds
-type(start_opt() :: [
	trace_opt() |
	max_r() |
	max_t()
]).
-type(start_opts() :: [start_opt()]).

-record(state, {
	odbc_pid :: 'undefined' | pid(),
	odbc_sup_pid :: 'undefiend' | pid(),
	event_cache = [] :: [{reference(), #event_log_row{}}],
	max_r :: non_neg_integer(),
	max_t :: non_neg_integer(),
	dsn :: string(),
	trace :: 'undefined' | 'trace',
	agents :: dict(),
	calls :: dict(),
	callqueuemap :: dict(),
	callagentmap :: dict()
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

% how many milliseconds to wait before checking if the odbc process lives.
-define(Check_interval, 100).

% =====
% API
% =====

%% @doc Start the supervisor and child with default options unlinked to 
%% calling process.
-spec(start/1 :: (Dsn :: dsn()) -> {'ok', pid()}).
start(Dsn) ->
	start(Dsn, []).

%% @doc Start the supervisor and child with given options unlinked 5o the
%% calling process.
-spec(start/2 :: (Dsn :: dsn(), Opts :: start_opts()) -> {'ok', pid()}).
start(Dsn, Opts) ->
	gen_server:start({local, ?MODULE}, ?MODULE, [Dsn, Opts], []).

%% @doc Start the supervisor and child with default options linked to the
%% calling process.
-spec(start_link/1 :: (Dsn :: dsn()) -> {'ok', pid()}).
start_link(Dsn) ->
	start_link(Dsn, []).

%% @doc Start the supervisor and child with given options linked to the 
%% calling process.
-spec(start_link/2 :: (Dsn :: dsn(), Opts :: start_opts()) -> {'ok', pid()}).
start_link(Dsn, Opts) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Dsn, Opts], []).

%% @doc Stops the supervisor and implicilty stops the child.
-spec(stop/0 :: () -> 'ok').
stop() ->
	gen_server:cast(?MODULE, stop).

%% @doc Starts the odbc process and sends cached events to it.  If the process is
%% already started, nothing happens.
-spec(start_odbc/0 :: () -> 'ok').
start_odbc() ->
	gen_server:cast(?MODULE, start_odbc).

%% @doc Returns the supervisor and writer pids.
-spec(status/0 :: () -> {pid() | 'undefined', pid() | 'undefined'}).
status() ->
	gen_server:call(?MODULE, status).

% =====
% init 
% =====
init([Dsn, Opts]) ->
	process_flag(trap_exit, true),
	cpx_monitor:subscribe(fun subscription_filter/1),
	Trace = proplists:get_value(trace, Opts),
	MaxRestarts = proplists:get_value(max_r, Opts, 3),
	MaxTime = proplists:get_value(max_t, Opts, 5),
	{ok, SupOdbc, Odbc} = init_sup_pids(MaxRestarts, MaxTime, Dsn, Trace),
	link(Odbc),
	inet_config:do_load_resolv(os:type(), longnames),
	% hopefully the below won't take too long if this module is started on a busy system.
	CallQMap = init_call_queue_map(),
	CallAgentMap = dict:new(), % TODO bootstrap this to avoid false positives on abandon
	E = build_event_log(acd_start, os:timestamp(), []),
	Cache = send_events(Odbc, [E], []),
	{ok, #state{
		odbc_pid = Odbc,
		odbc_sup_pid = SupOdbc,
		dsn = Dsn,
		trace = Trace,
		max_r = MaxRestarts,
		max_t = MaxTime,
		agents = dict:new(),
		calls = dict:new(),
		callqueuemap = CallQMap,
		callagentmap = CallAgentMap,
		event_cache = Cache
	}}.

% =====
% Call
% =====
handle_call(status, _From, #state{odbc_sup_pid = Sup, odbc_pid = Kid} = State) ->
	{reply, {Sup, Kid}, State};
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State, hibernate}.

% =====
% Cast
% =====
handle_cast(start_odbc, #state{odbc_sup_pid = undefined} = State) ->
	{ok, SupOdbc} = start_odbc_super(State#state.max_r, State#state.max_t),
	{ok, Odbc} = start_odbc_process(SupOdbc, State#state.dsn, State#state.trace),
	?INFO("New sup:  ~p;  new odbc:  ~p.", [SupOdbc, Odbc]),
	link(Odbc),
	Resend = lists:reverse(State#state.event_cache),
	[Odbc ! X || X <- Resend],
	{noreply, State#state{odbc_pid = Odbc, odbc_sup_pid = SupOdbc}};
handle_cast(start_odbc, State) ->
	?INFO("Supervisor for odbc still up", []),
	{noreply, State};
handle_cast(stop, #state{odbc_sup_pid = undefined} = State) ->
	?WARNING("Stopping while writer process is stopped.", []),
	{stop, normal, State};
handle_cast(stop, State) ->
	Event = build_event_log(acd_stop, os:timestamp(), []),
	NewCache = send_events(State#state.odbc_pid, [Event], State#state.event_cache),
	?stop_writer(State#state.odbc_pid),
	{stop, normal, State#state{odbc_pid = undefined, event_cache = NewCache}};
handle_cast(_Msg, State) ->
	{noreply, State, hibernate}.

% =====
% Info
% =====

%% === handle exits ===
handle_info({'EXIT', Pid, _Reason} = Msg, #state{odbc_sup_pid = Pid, odbc_pid = OdbcPid} = State) when is_pid(OdbcPid) ->
	?DEBUG("bing", []),
	MidState = State#state{odbc_pid = undefined},
	handle_info(Msg, MidState);
handle_info({'EXIT', Pid, _Reason} = Msg, #state{odbc_sup_pid = Pid, odbc_pid = Ref} = State) when is_reference(Ref) ->
	?DEBUG("bing", []),
	erlang:cancel_timer(Ref),
	MidState = State#state{odbc_pid = undefined},
	handle_info(Msg, MidState);
handle_info({'EXIT', Pid, Reason}, #state{odbc_sup_pid = Pid, odbc_pid = undefined} = State) ->
	?ERROR("Odbc supervisor process ~p has died due to ~p; manual intervention required.", [Pid, Reason]),
	NewState = State#state{odbc_sup_pid = undefined},
	{noreply, NewState};
handle_info({'EXIT', Pid, Reason}, #state{odbc_sup_pid = undefined} = State) ->
	?INFO("Likely a late exit for odbc process ~p due to ~p.", [Pid, Reason]),
	{noreply, State};
handle_info({'EXIT', Pid, Reason}, #state{odbc_pid = Pid} = State) ->
	?INFO("Got exit for odbc process ~p due to ~p; scheduling resend of cache.", [Pid, Reason]),
	Self = self(),
	Ref = erlang:send_after(?Check_interval, Self, check_odbc),
	{noreply, State#state{odbc_pid = Ref}};

% === exit recovery ===
handle_info(check_odbc, #state{odbc_sup_pid = undefined} = State) ->
	?INFO("likely a late check_odbc since the supervisor is dead.", []),
	{noreply, State#state{odbc_pid = undefined}};
handle_info(check_odbc, #state{odbc_sup_pid = Sup} = State) when is_pid(Sup) ->
	?DEBUG("Checking for odbc recover", []),
	case supervisor:which_children(Sup) of
		[{cpx_monitor_kgb_odbc, undefined, _, _}] ->
			Ref = erlang:send_after(?Check_interval, ?MODULE, check_odbc),
			{noreply, State#state{odbc_pid = Ref}};
		[{cpx_monitor_kgb_odbc, Pid, _, _}] ->
			case is_process_alive(Pid) of
				true ->
					link(Pid),
					Resend = lists:reverse(State#state.event_cache),
					[Pid ! X || X <- Resend],
					{noreply, State#state{odbc_pid = Pid}};
				false ->
					Ref = erlang:send_after(?Check_interval, ?MODULE, check_odbc),
					{noreply, State#state{odbc_pid = Ref}}
			end
	end;

% === event handling ===
handle_info({ack, Ref}, #state{event_cache = Cache} = State) ->
	Pred = fun({TestRef, _E}) ->
		TestRef /= Ref
	end,
	NewCache = lists:filter(Pred, Cache),
	{noreply, State#state{event_cache = NewCache}};
handle_info({cpx_monitor_event, {set, Timestamp, {{agent, Key}, Details, _Node}}}, State) ->
	AgentEventCache = case dict:find(Key, State#state.agents) of
		error ->
			Astart = build_event_log(agent_start, Timestamp, Details),
			Logins = build_event_log(agent_login, Timestamp, Details),
			Events = [Astart | Logins],
			send_events(State#state.odbc_pid, Events, State#state.event_cache);
		{ok, Current} ->
			%?NOTICE("Udating agent ~p from  ~p to ~p", [Key, Current, Details]),
			Events = agent_diff(Key, Details, Current, Timestamp, State),
			send_events(State#state.odbc_pid, Events, State#state.event_cache)
	end,
	case {proplists:get_value(statedata, Details), proplists:get_value(state, Details)} of
		{X, Y} when is_record(X, call), Y =/= ringing ->
			% ok, an agent is definitely interacting with this call
			{noreply, State#state{agents = dict:store(Key, Details, State#state.agents), callagentmap = dict:store(X#call.id, Key, State#state.callagentmap), event_cache = AgentEventCache}};
		_ ->
			{noreply, State#state{agents = dict:store(Key, Details, State#state.agents), event_cache = AgentEventCache}}
	end;
handle_info({cpx_monitor_event, {drop, Timestamp, {agent, Key}}}, State) ->
	case dict:find(Key, State#state.agents) of
		error ->
			{noreply, State};
		{ok, Current} ->
			Stop = build_event_log(agent_stop, Timestamp, Current),
			Logouts = build_event_log(agent_logout, Timestamp, Current),
			Events = [Stop | Logouts],
			NewCache = send_events(State#state.odbc_pid, Events, State#state.event_cache),
			{noreply, State#state{agents = dict:erase(Key, State#state.agents), event_cache = NewCache}}
	end;
handle_info({cpx_monitor_event, {set, Timestamp, {{media, Key}, Details, _Node}}}, State) ->
	case proplists:get_value(queue, Details) of
		undefined ->
			{noreply, State#state{calls = dict:store(Key, Details, State#state.calls)}};
		Queue ->
			Event = build_event_log(call_enqueue, Timestamp, [{mediaid, Key} | Details]),
			NewCache = send_events(State#state.odbc_pid, [Event], State#state.event_cache),
			{noreply, State#state{callqueuemap = dict:store(Key, Queue, State#state.callqueuemap), calls = dict:store(Key, Details, State#state.calls), event_cache = NewCache}}
	end;
handle_info({cpx_monitor_event, {drop, Timestamp, {media, Key}}}, State) ->
	% setting up a delay as there may be messages about agents that need the 
	% info.
	NewCache = case dict:find(Key, State#state.callagentmap) of
		error -> % An abandonment!
			Queue = case dict:find(Key, State#state.callqueuemap) of
				error -> "Unknown Queue";
				{ok, Value} -> Value
			end,
			case dict:find(Key, State#state.calls) of
				{ok, New} ->
					?INFO("~p abandoned", [Key]),
					UseableProps = [{cache_queue, Queue}, {mediaid, Key} | New],
					Event = build_event_log(call_terminate, Timestamp, UseableProps),
					send_events(State#state.odbc_pid, [Event], State#state.event_cache);
				error ->
					?ERROR("unknown call ~p abandoned", [Key]),
					State#state.event_cache
			end;
		{ok, _Agent} ->
			State#state.event_cache
	end,
	Self = self(),
	erlang:send_after(5000, Self, {redrop, {media, Key}}),
	{noreply, State#state{calls = dict:erase(Key, State#state.calls), event_cache = NewCache}};
handle_info({redrop, {media, Key}}, #state{callqueuemap = Callqmap, callagentmap = CallAgentMap, calls = Calls} = State) ->
	Newcalls = dict:erase(Key, Calls),
	Newcmap = dict:erase(Key, Callqmap),
	Newamap = dict:erase(Key, CallAgentMap),
	{noreply, State#state{callqueuemap = Newcmap, callagentmap = Newamap, calls = Newcalls}};
handle_info(Info, State) ->
	?DEBUG("Got message: ~p", [Info]),
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

start_odbc_super(MaxR, MaxT) ->
	supervisor:start_link(cpx_middle_supervisor, [MaxR, MaxT]).

% for testing purposes (ensureing the write process gets all the correct events,
% one starts the 'proper' module, the other starts a mock and preps an expect
% info for the appropriate time.
-ifdef(TEST).
start_odbc_process(SupPid, _, _) ->
	Spec = #cpx_conf{
			id = cpx_monitor_kgb_odbc,
			module_name = gen_server_mock,
			start_function = named,
			start_args = [{local, test_odbc_writer}]
		},
		cpx_middle_supervisor:add_directly(SupPid, Spec).
		
init_sup_pids(Maxr, Maxt, _, _) ->
	{ok, Sup} = start_odbc_super(Maxr, Maxt),
	{ok, Odbc} = start_odbc_process(Sup, undefined, undefined),
	gen_server_mock:expect_info(test_odbc_writer,
		fun({_Ref, #event_log_row{event_type = acd_start} = In}, _State) ->
			undefined = In#event_log_row.id,
			Fqdn = get_FQDN(),
			Fqdn = In#event_log_row.hostname,
			Fqdn = In#event_log_row.acd_name,
			ok
		end
	),
	{ok, Sup, Odbc}.
-else.
start_odbc_process(SupPid, Dsn, Trace) ->
	Spec = #cpx_conf{
		id = cpx_monitor_kgb_odbc,
		module_name = cpx_monitor_kgb_odbc,
		start_function = start_link,
		start_args = [Dsn, Trace]
	},
	cpx_middle_supervisor:add_directly(SupPid, Spec).

init_sup_pids(Maxr, Maxt, Dsn, Trace) ->
	{ok, Sup} = start_odbc_super(Maxr, Maxt),
	{ok, Pid} = start_odbc_process(Sup, Dsn, Trace),
	{ok, Sup, Pid}.
-endif.

agent_diff(_Agent, New, Old, Timestamp, State) ->
	% has the agent's state changed?
	case proplists:get_value(state, New) == proplists:get_value(state, Old) of
		true ->
			% ok, so not a state change, is it a profile change?
			case proplists:get_value(profile, New) == proplists:get_value(profile, Old) of
					true ->
						% hell if I know what changed
						[];
					false ->
						% ok, now diff the skill lists to see if we've changed queue membership
						Lost = proplists:get_value(skills, Old) -- proplists:get_value(skills, New),
						Gained = proplists:get_value(skills, New) -- proplists:get_value(skills, Old),
						LostProps = [{skills, Lost} | proplists:delete(skills, New)],
						LostEvents = build_event_log(agent_logout, Timestamp, LostProps),
						GainProps = [{skills, Gained} | proplists:delete(skills, New)],
						GainEvents = build_event_log(agent_login, Timestamp, GainProps),
						LostEvents ++ GainEvents
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
						Event = build_event_log(call_terminate, Timestamp, [{cached_queue, Queue} | New]),
						[Event];
					{wrapup, NextState} ->
						Call = proplists:get_value(statedata, Old),
						Queue = case dict:find(Call#call.id, State#state.callqueuemap) of
							error -> "Unknown Queue";
							{ok, Value} -> Value
						end,
						% faking the state data because the call_complete needs call rec.
						Events1 = build_event_log(call_complete, Timestamp, [{cached_queue, Queue}, {statedata, Call} | New]),
						case NextState of
							idle ->
								Events2 = build_event_log(agent_available, Timestamp, New),
								[Events1 | Events2];
							_ ->
								[Events1]
						end;	
					{_, oncall} ->
						Call = proplists:get_value(statedata, New),
						Queue = case dict:find(Call#call.id, State#state.callqueuemap) of
							error -> "Unknown Queue";
							{ok, Value} -> Value
						end,
						Event = build_event_log(call_answer, Timestamp, [{cached_queue, Queue} | New]),
						[Event];
					{idle, released} ->
						build_event_log(agent_unavailable, Timestamp, New);
					{released, idle} ->
						build_event_log(agent_available, Timestamp, New);
					{_, _} ->
						[]
				end
		end.

-spec(build_event_log/3 :: (Type :: event_type(), Time :: {integer(), integer(), integer()}, Props :: [any()]) -> #event_log_row{} | [#event_log_row{}]).
build_event_log(Type, Time, Props) ->
	Base = #event_log_row{
		acd_type = "openacd",
		acd_name = get_FQDN(),
		hostname = get_FQDN(),
		created_at = iso8601_timestamp(Time),
		event_type = Type
	},
	build_event_log(Base, Props).

build_event_log(#event_log_row{event_type = acd_start} = E, _Props) ->
	E;
build_event_log(#event_log_row{event_type = acd_stop} = E, _Props) ->
	E;
build_event_log(#event_log_row{event_type = agent_start} = E, Props) ->
	E#event_log_row{
		acd_agent_id = proplists:get_value(login, Props)
	};
build_event_log(#event_log_row{event_type = agent_login} = E, Props) ->
	BaseEvent = E#event_log_row{
		acd_agent_id = proplists:get_value(login, Props)
	},
	Skills = proplists:get_value(skills, Props),
	[BaseEvent#event_log_row{queue_name = Q} || {'_queue', Q} <- Skills];
build_event_log(#event_log_row{event_type = agent_stop} = E, Props) ->
	E#event_log_row{
		acd_agent_id = proplists:get_value(login, Props)
	};
build_event_log(#event_log_row{event_type= agent_logout} = E, Props) ->
	BaseEvent = E#event_log_row{
		acd_agent_id = proplists:get_value(login, Props)
	},
	Skills = proplists:get_value(skills, Props),
	[BaseEvent#event_log_row{queue_name = Q} || {'_queue', Q} <- Skills];
build_event_log(#event_log_row{event_type = call_enqueue} = E , Props) ->
	build_event_log_call_base(E, Props);
build_event_log(#event_log_row{event_type = call_answer} = E, Props) ->
	MidEvent = build_event_log_call_base(E, Props),
	MidEvent#event_log_row{
		acd_agent_id = proplists:get_value(login, Props)
	};
build_event_log(#event_log_row{event_type = call_terminate} = E, Props) ->
	MidEvent = build_event_log_call_base(E, Props),
	case proplists:get_value(login, Props) of
		undefined ->
			MidEvent;
		Login ->
			MidEvent#event_log_row{
				acd_agent_id = Login
			}
	end;
build_event_log(#event_log_row{event_type = call_complete} = E, _Props) ->
	E#event_log_row{
		acd_name = undefined
	};
build_event_log(#event_log_row{event_type = agent_available} = E, Props) ->
	Login = proplists:get_value(login, Props),
	Skills = proplists:get_value(skills, Props),
	[E#event_log_row{
		acd_agent_id = Login,
		queue_name = Q
	} || {'_queue', Q} <- Skills];
build_event_log(#event_log_row{event_type = agent_unavailable} = E, Props) ->
	Login = proplists:get_value(login, Props),
	Skills = proplists:get_value(skills, Props),
	[E#event_log_row{
		acd_agent_id = Login,
		queue_name = Q
	} || {'_queue', Q} <- Skills].

build_event_log_call_base(E, Props) ->
	Queue = case proplists:get_value(cached_queue, Props) of
		undefined ->
			proplists:get_value(queue, Props);
		Q ->
			Q
	end,
	{ProtoAltFrom, ProtoFromHeads} = case {proplists:get_value(callerid, Props), proplists:get_value(statedata, Props)} of
		{undefined, #call{callerid = {A, H}}} ->
			{A, H};
		{{A, H}, undefined} ->
			{A,H}
	end,
	AltHeader = case ProtoAltFrom of
		[$* | _] -> ?DEFAULT_ANI ++ ProtoAltFrom;
		_ -> ProtoAltFrom
	end,
	FromHeader = case ProtoFromHeads of
		[$* | _] -> ?DEFAULT_ANI ++ ProtoFromHeads;
		_ -> ProtoFromHeads
	end,
	[Ani, Uci, OriginCode, Did | _TailFromHeader] = case string:tokens(FromHeader, "*") of
		[_, _, _, _ | _] = Out ->
			Out;
		_ ->
			case string:tokens(AltHeader, "*") of
				[_, _, _, _ | _] = Out ->
					Out;
				_ ->
					["Unknown", "Unknown", "Unknown", "Unknown"]
			end
	end,
	[ChoppedDid | _]  = string:tokens(Did, "@"),
	E#event_log_row{
		queue = Queue,
		queue_name = Queue,
		from_header = FromHeader,
		ani = Ani,
		uci = Uci ++ "*" ++ OriginCode,
		did = ChoppedDid,
		origin_code = OriginCode,
		freeswitch_id = proplists:get_value(mediaid, Props, "")
	}.
	
send_events(_Pid, [], Acc) ->
	Acc;
send_events(Pid, [Head | Tail], Acc) when is_pid(Pid) ->
	Ref = make_ref(),
	Pid ! {Ref, Head},
	NewAcc = [{Ref, Head} | Acc],
	send_events(Pid, Tail, NewAcc);
send_events(undefined, [Head | Tail], Acc) -> 
	NewAcc = [{make_ref(), Head} | Acc],
	send_events(undefined, Tail, NewAcc);
send_events(Nom, List, Acc) when is_atom(Nom)->
	send_events(whereis(Nom), List, Acc);
send_events(_Pid, [Head | Tail], Acc) ->
	NewAcc = [{make_ref(), Head} | Acc],
	send_events(undefined, Tail, NewAcc).
	
%-spec monotonic_counter() -> float().
%monotonic_counter() ->
%	{MegaSeconds, Seconds, MicroSeconds} = erlang:now(),
%	MegaSeconds * 1000000 + Seconds + MicroSeconds / 1000000.

% generates time stamps like 2010-07-29T12:31:02.776357Z according to ISO8601
%-spec iso8601_timestamp() -> string().
%iso8601_timestamp() ->
%	iso8601_timestamp(os:timestamp()).

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

-record(test_conf, {
	ets  :: any(),
	queue_man :: pid(),
	odbc_sup :: pid(),
	timestamp = os:timestamp() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}
}).

ignore_infos(Mock, Count) ->
	[gen_server_mock:expect_info(Mock, fun(_, _) -> ok end) || _ <- lists:seq(1, Count)].

all_test_() ->
	[transform_events_tests(),
	murder_tests()].

receive_oks(N) ->
	receive_oks(N, 20).

receive_oks(N, Timeout) ->
	[receive ok -> ok; X -> ?assert(X) after Timeout -> ?assert(timeout) end || _ <- lists:seq(1, N)].

transform_events_tests() ->
	{inorder, {foreach,
	fun() ->
		Ets = ets:new(cpx_monitor, [named_table]),
		{ok, Qm} = gen_leader_mock:start(queue_manager),
		gen_leader_mock:expect_leader_call(Qm, fun(_, _, State, _) -> 
			{ok, [], State}
		end),	
		{ok, Pid} = start("fake_dsn"),
		#test_conf{
			ets = Ets,
			queue_man = Qm,
			odbc_sup = Pid
		}
	end,
	fun(#test_conf{ets = Ets}) ->
		try gen_server_mock:expect_info(test_odbc_writer,
			fun({_Ref, #event_log_row{event_type = acd_stop}}, _State) ->
				ok
			end
		)
		catch
			_:_ -> ok
		end,
		cpx_monitor_odbc_supervisor:stop(),
		try gen_server_mock:stop(whereis(test_odbc_writer))
		catch
			_:_ -> ok
		end,
		gen_leader_mock:stop(whereis(queue_manager)),
		ets:delete(Ets),
		% give it time to die
		timer:sleep(10)
	end,
	[fun(_Rec) -> {"start_acd", fun() ->
		% the start should be sent automattically on start, so
		% just assert expectations.
		?assertEqual(ok, gen_server_mock:assert_expectations(whereis(test_odbc_writer)))
	end} end,
	fun(_Rec) -> {"stop_acd", fun() ->
		Self = self(),
		gen_server_mock:expect_info(test_odbc_writer, 
			fun({_Ref, #event_log_row{event_type = acd_stop} = In}, _State) ->
				Fqdn = get_FQDN(),
				Fqdn = In#event_log_row.hostname,
				Fqdn = In#event_log_row.acd_name,
				Self ! {ok, acd_stop},
				ok
			end
		),
		cpx_monitor_odbc_supervisor:stop(),
		receive
			{ok, acd_stop} ->
				?assert(true)
		after 20 ->
			?assert(timeout)
		end
	end} end,
	fun(Rec) -> {"agent_start", fun() ->
		Self = self(),
		% "316670"; "ca1acd05.infonxx.local"; "agent_start"; 
		% "ca1acd05.infonxx.local"; "5151"; ""; ""; ""; ""; ""; ""; ""; ""; ""; 
		% ""; "2010-12-17 08:08:27.487565-05"; ""; 
		% "openacd : 1292591307.488780 : ca1acd05.infonxx.local : 2010-12-17T13:08:27.487565Z : agent_start : testme@ca1acd05 : 5151";
		% ""; "openacd"
		gen_server_mock:expect_info(test_odbc_writer,
			fun({_Ref, In}, _State) ->
				Expected = #event_log_row{
					hostname = get_FQDN(),
					event_type = agent_start,
					acd_name = get_FQDN(),
					acd_agent_id = "testagent",
					acd_type = "openacd",
					created_at =  iso8601_timestamp(Rec#test_conf.timestamp)
				},
				Self ! ?assertEqual(Expected, In),
				ok
			end
		),
		% "316671"; "ca1acd05.infonxx.local"; "agent_login"; 
		% "ca1acd05.infonxx.local"; "5151"; ""; ""; ""; ""; ""; "SRVCC_English"; 
		% ""; ""; ""; ""; "2010-12-17 08:08:27.487565-05"; ""; 
		% "openacd : 1292591307.488943 : ca1acd05.infonxx.local : 2010-12-17T13:08:27.487565Z : agent_login : testme@ca1acd05 : 5151 : SRVCC_English"; 
		% ""; "openacd"
		gen_server_mock:expect_info(test_odbc_writer,
			fun({_, In}, _State) ->
				Expected = #event_log_row{
					hostname = get_FQDN(),
					event_type = agent_login,
					acd_name = get_FQDN(),
					acd_agent_id = "testagent",
					queue_name = "q1",
					acd_type = "openacd",
					created_at =  iso8601_timestamp(Rec#test_conf.timestamp)
				},
				Self ! ?assertEqual(Expected, In),
				ok
			end
		),
		gen_server_mock:expect_info(test_odbc_writer,
			fun({_, In}, _State) ->
				Expected = #event_log_row{
					hostname = get_FQDN(),
					event_type = agent_login,
					acd_name = get_FQDN(),
					acd_agent_id = "testagent",
					queue_name = "q2",
					acd_type = "openacd",
					created_at =  iso8601_timestamp(Rec#test_conf.timestamp)
				},
				Self ! ?assertEqual(Expected, In),
				ok
			end
		),
		CpxEvent = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{agent, "testagent"}, [
			{login, "testagent"},
			{skills, [{'_queue', "q1"}, {'_queue', "q2"}]}
		], "node"}}},
		cpx_monitor_odbc_supervisor ! CpxEvent,
		timer:sleep(10), % give it time to eat the event
		?assertEqual(ok, gen_server_mock:assert_expectations(whereis(test_odbc_writer))),
		receive_oks(3)
	end} end,
	fun(Rec) -> {"agent_available", fun() ->
		Self = self(),
		CpxEvent = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{agent, "testagent"}, [
			{login, "testagent"},
			{skills, [{'_queue', "q1"}, {'_queue', "q2"}]},
			{state, released},
			{statedata, default}
		], "node"}}},
		GoAvailEvent = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{agent, "testagent"}, [
			{login, "testagent"},
			{skills, [{'_queue', "q1"}, {'_queue', "q2"}]},
			{state, idle},
			{statedata, {}}
		], "node"}}},
		ignore_infos(test_odbc_writer, 3),
		% "316672"; "ca1acd05.infonxx.local"; "agent_available"; 
		% "ca1acd05.infonxx.local"; "5151"; ""; ""; ""; ""; ""; "SRVCC_English"; 
		% ""; ""; ""; ""; "2010-12-17 08:14:11.402181-05"; "";
		% "openacd : 1292591651.402580 : ca1acd05.infonxx.local : 2010-12-17T13:14:11.402181Z : agent_available : testme@ca1acd05 : 5151 : SRVCC_English";
		% ""; "openacd"
		gen_server_mock:expect_info(test_odbc_writer,
			fun({_, In}, _) ->
				Expected = #event_log_row{
					hostname = get_FQDN(),
					event_type = agent_available,
					acd_name = get_FQDN(),
					acd_agent_id = "testagent",
					queue_name = "q1",
					acd_type = "openacd",
					created_at =  iso8601_timestamp(Rec#test_conf.timestamp)
				},
				Self ! ?assertEqual(Expected, In),
				ok
			end
		),
		gen_server_mock:expect_info(test_odbc_writer,
			fun({_, In}, _) ->
				Expected = #event_log_row{
					hostname = get_FQDN(),
					event_type = agent_available,
					acd_name = get_FQDN(),
					acd_agent_id = "testagent",
					queue_name = "q2",
					acd_type = "openacd",
					created_at =  iso8601_timestamp(Rec#test_conf.timestamp)
				},
				Self ! ?assertEqual(Expected, In),
				ok
			end
		),
		cpx_monitor_odbc_supervisor ! CpxEvent,
		cpx_monitor_odbc_supervisor ! GoAvailEvent,
		timer:sleep(20), % nom nom my pretties
		receive_oks(2),
		?assertEqual(ok, gen_server_mock:assert_expectations(whereis(test_odbc_writer)))
	end} end,
	fun(Rec) -> {"agent_unavailable", fun() ->
		CpxEvent = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{agent, "testagent"}, [
			{login, "testagent"},
			{skills, [{'_queue', "q1"}, {'_queue', "q2"}]},
			{state, released},
			{statedata, default}
		], "node"}}},
		GoAvailEvent = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{agent, "testagent"}, [
			{login, "testagent"},
			{skills, [{'_queue', "q1"}, {'_queue', "q2"}]},
			{state, idle},
			{statedata, {}}
		], "node"}}},
		EndAvailEvent = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{agent, "testagent"}, [
			{login, "testagent"},
			{skills, [{'_queue', "q1"}, {'_queue', "q2"}]},
			{state, released},
			{statedata, default}
		], "node"}}},
		ignore_infos(test_odbc_writer, 5),
		Self = self(),
		% "316672"; "ca1acd05.infonxx.local"; "agent_available"; 
		% "ca1acd05.infonxx.local"; "5151"; ""; ""; ""; ""; ""; "SRVCC_English"; 
		% ""; ""; ""; ""; "2010-12-17 08:14:11.402181-05"; "";
		% "openacd : 1292591651.402580 : ca1acd05.infonxx.local : 2010-12-17T13:14:11.402181Z : agent_unavailable : testme@ca1acd05 : 5151 : SRVCC_English";
		% ""; "openacd"
		gen_server_mock:expect_info(test_odbc_writer,
			fun({_, In}, _) ->
				Expected = #event_log_row{
					hostname = get_FQDN(),
					event_type = agent_unavailable,
					acd_name = get_FQDN(),
					acd_agent_id = "testagent",
					queue_name = "q1",
					acd_type = "openacd",
					created_at =  iso8601_timestamp(Rec#test_conf.timestamp)
				},
				Self ! ?assertEqual(Expected, In),
				ok
			end
		),
		gen_server_mock:expect_info(test_odbc_writer,
			fun({_, In}, _) ->
				Expected = #event_log_row{
					hostname = get_FQDN(),
					event_type = agent_unavailable,
					acd_name = get_FQDN(),
					acd_agent_id = "testagent",
					queue_name = "q2",
					acd_type = "openacd",
					created_at =  iso8601_timestamp(Rec#test_conf.timestamp)
				},
				Self ! ?assertEqual(Expected, In),
				ok
			end
		),
		cpx_monitor_odbc_supervisor ! CpxEvent,
		cpx_monitor_odbc_supervisor ! GoAvailEvent,
		cpx_monitor_odbc_supervisor ! EndAvailEvent,
		timer:sleep(20), % nom nom my pretties
		receive_oks(2),
		?assertEqual(ok, gen_server_mock:assert_expectations(whereis(test_odbc_writer)))
	end} end,
	fun(Rec) -> {"agent_stop", fun() ->
		% build it up
		ignore_infos(test_odbc_writer, 3),
		CpxEvent = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{agent, "testagent"}, [
			{login, "testagent"},
			{skills, [{'_queue', "q1"}, {'_queue', "q2"}]}
		], "node"}}},
		cpx_monitor_odbc_supervisor ! CpxEvent,
		Self = self(),
		% and now tear it down.
		% "316673"; "ca1acd05.infonxx.local"; "agent_stop"; 
		% "ca1acd05.infonxx.local"; "5151"; ""; ""; ""; ""; ""; ""; ""; ""; ""; 
		% ""; "2010-12-17 08:39:37.656684-05"; ""; "openacd : 1292593177.657229 : ca1acd05.infonxx.local : 2010-12-17T13:39:37.656684Z : agent_stop : testme@ca1acd05 : 5151"; 
		% ""; "openacd"
		gen_server_mock:expect_info(test_odbc_writer,
			fun({_Ref, #event_log_row{event_type = agent_stop} = In}, _) ->
				Expected = #event_log_row{
					hostname = get_FQDN(),
					event_type = agent_stop,
					acd_name = get_FQDN(),
					acd_agent_id = "testagent",
					acd_type = "openacd",
					created_at = In#event_log_row.created_at
				},
				case {?assertEqual(Expected, In), ?assertNot(undefined =:= In#event_log_row.created_at)} of
					{ok, ok} ->
						Self ! ok
				end,
				ok
			end
		),
		% "316674"; "ca1acd05.infonxx.local"; "agent_logout"; 
		% "ca1acd05.infonxx.local"; "5151"; ""; ""; ""; ""; ""; "SRVCC_English"; 
		% ""; ""; ""; ""; "2010-12-17 08:39:37.656684-05"; ""; 
		% "openacd : 1292593177.657609 : ca1acd05.infonxx.local : 2010-12-17T13:39:37.656684Z : agent_logout : testme@ca1acd05 : 5151 : SRVCC_English";
		% ""; "openacd"
		gen_server_mock:expect_info(test_odbc_writer,
			fun({_Ref, #event_log_row{event_type = agent_logout} = In}, _) ->
				Expected = #event_log_row{
					hostname = get_FQDN(),
					event_type = agent_logout,
					acd_name = get_FQDN(),
					acd_agent_id = "testagent",
					queue_name = "q1",
					created_at =  In#event_log_row.created_at
				},
				case {?assertEqual(Expected, In), ?assertNot(undefined =:= In#event_log_row.created_at)} of
					{ok, ok} ->
						Self ! ok
				end
			end
		),
		gen_server_mock:expect_info(test_odbc_writer,
			fun({_Ref, #event_log_row{event_type = agent_logout} = In}, _) ->
				Expected = #event_log_row{
					hostname = get_FQDN(),
					event_type = agent_logout,
					acd_name = get_FQDN(),
					acd_agent_id = "testagent",
					queue_name = "q2",
					created_at = In#event_log_row.created_at
				},
				case{?assertEqual(Expected, In), ?assertNot(undefined =:= In#event_log_row.created_at)} of
					{ok, ok} ->
						Self ! ok
				end
			end
		),
		CpxDropEvent = {cpx_monitor_event, {drop, os:timestamp(), {agent, "testagent"}}},
		cpx_monitor_odbc_supervisor ! CpxDropEvent,
		timer:sleep(10),
		receive_oks(3),
		?assertEqual(ok, gen_server_mock:assert_expectations(whereis(test_odbc_writer)))
	end} end,
	fun(Rec) -> {"call_enqueue", fun() ->
		Self = self(),
		% "316653"; "ca1acd05.infonxx.local"; "call_enqueue"; 
		% "ca1acd05.infonxx.local"; ""; ""; "15162337*112"; "9080"; "112"; 
		% "SRVCC_English"; "SRVCC_English"; ""; "6108491500*15162337*112*9080"; 
		% ""; ""; "2010-12-17 07:14:03.361929-05"; ""; 
		% "openacd : 1292588043.362124 : ca1acd05.infonxx.local : 2010-12-17T12:14:03.361929Z : call_enqueue : testme@ca1acd05 : SRVCC_English : 6108491500*15162337*112*9080 : cf97c474-c691-4747-ba8f-8f05375e9a90 : Origin Code : CLS : Source IP : 9080";
		% "6108491500"; "openacd"
		gen_server_mock:expect_info(test_odbc_writer,
			fun({_, In}, _) ->
				Expected = #event_log_row{
					hostname = get_FQDN(),
					event_type = call_enqueue,
					acd_name = get_FQDN(),
					uci = "b*c",
					did = "d",
					origin_code = "c",
					queue = "A Queue",
					queue_name = "A Queue",
					from_header = "a*b*c*d",
					acd_type = "openacd",
					ani = "a",
					created_at = In#event_log_row.created_at
				},
				case {?assertEqual(Expected, In), ?assertNot(undefined =:= In#event_log_row.created_at)} of
					{ok, ok} ->
						Self ! ok
				end
			end
		),
		CpxEvent = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{media, "testmedia"}, [
			{queue, "A Queue"},
			{callerid, {"ignored", "a*b*c*d"}}
		], "node"}}},
		cpx_monitor_odbc_supervisor ! CpxEvent,
		timer:sleep(10),
		receive_oks(1),
		?assertEqual(ok, gen_server_mock:assert_expectations(whereis(test_odbc_writer)))
	end} end,
	fun(Rec) -> {"call_terminate in queue", fun() ->
		% build it up
		ignore_infos(test_odbc_writer, 1),
		CpxEvent = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{media, "testmedia"}, [
			{queue, "A Queue"},
			{callerid, {"ignored", "a*b*c*d"}}
		], "node"}}},
		cpx_monitor_odbc_supervisor ! CpxEvent,
		Self = self(),
		% tear it down
		% "316655"; "ca1acd05.infonxx.local"; "call_terminate";
		% "ca1acd05.infonxx.local"; "5045"; ""; "15162337*112"; "9080"; "112";
		% "SRVCC_English"; "SRVCC_English"; ""; "6108491500*15162337*112*9080";
		% ""; ""; "2010-12-17 07:24:21.30899-05"; ""; 
		% "openacd : 1292588661.309172 : ca1acd05.infonxx.local : 2010-12-17T12:24:21.308990Z : call_terminate : testme@ca1acd05 : SRVCC_English : 5045 : 6108491500*15162337*112*9080 : cf97c474-c691-4747-ba8f-8f05375e9a90 : Origin : CLS : Source IP : 9080";
		% "6108491500"; "openacd"
		gen_server_mock:expect_info(test_odbc_writer,
			fun({_, In}, _) ->
				Expected = #event_log_row{
					hostname = get_FQDN(),
					event_type = call_terminate,
					acd_name = get_FQDN(),
					uci = "b*c",
					did = "d",
					origin_code = "c",
					queue = "A Queue",
					queue_name = "A Queue",
					from_header = "a*b*c*d",
					acd_type = "openacd",
					ani = "a",
					created_at = In#event_log_row.created_at
				},
				case {?assertEqual(Expected, In), ?assertNot(undefined =:= In#event_log_row.created_at)} of
					{ok, ok} ->
						Self ! ok
				end
			end
		),
		CpxDropEvent = {cpx_monitor_event, {drop, Rec#test_conf.timestamp, {media, "testmedia"}}},
		cpx_monitor_odbc_supervisor ! CpxDropEvent,
		timer:sleep(10),
		receive_oks(1),
		?assertEqual(ok, gen_server_mock:assert_expectations(whereis(test_odbc_writer)))
	end} end,
	fun(Rec) -> {"call_answer", fun() ->
		Self = self(),
		CpxAgentEvent = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{agent, "testagent"}, [
			{login, "testagent"},
			{skills, [{'_queue', "Q"}]}
		], "node"}}},
		CpxMediaEvent = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{media, "testmedia"}, [
			{callerid, {"ignored", "a*b*c*d"}},
			{queue, "Q"}
		], "node"}}},
		Call = #call{id = "testmedia", source = self(), callerid = {"ignored", "a*b*c*d"}},
		CpxMediaAnswer = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{agent, "testagent"}, [
			{login, "testagent"},
			{state, oncall},
			{statedata, Call}
		], "node"}}},
		ignore_infos(test_odbc_writer, 3),
		% "316654"; "ca1acd05.infonxx.local"; "call_answer"; 
		% "ca1acd05.infonxx.local"; "5045"; ""; "15162337*112"; "9080"; "112";
		% "SRVCC_English"; "SRVCC_English"; ""; "6108491500*15162337*112*9080";
		% ""; ""; "2010-12-17 07:14:08.563499-05"; ""; 
		% "openacd : 1292588048.563678 : ca1acd05.infonxx.local : 2010-12-17T12:14:08.563499Z : call_pickup : testme@ca1acd05 : SRVCC_English : 5045 : 6108491500*15162337*112*9080 : cf97c474-c691-4747-ba8f-8f05375e9a90 : Origin : CLS : Source IP : 9080";
		% "6108491500"; "openacd"
		gen_server_mock:expect_info(test_odbc_writer, 
			fun({_, In}, _) ->
				Expected = #event_log_row{
					hostname = get_FQDN(),
					event_type = call_answer,
					acd_name = get_FQDN(),
					acd_agent_id = "testagent",
					uci = "b*c",
					did = "d",
					origin_code = "c",
					queue = "Q",
					queue_name = "Q",
					from_header = "a*b*c*d",
					acd_type = "openacd",
					ani = "a",
					created_at = In#event_log_row.created_at
				},
				case {?assertEqual(Expected, In), ?assertNot(undefined =:= In#event_log_row.created_at)} of
					{ok, ok} ->
						Self ! ok
				end
			end
		),
		Msgs = [CpxAgentEvent, CpxMediaEvent, CpxMediaAnswer],
		[cpx_monitor_odbc_supervisor ! X || X <- Msgs],
		timer:sleep(10),
		receive_oks(1),
		?assertEqual(ok, gen_server_mock:assert_expectations(whereis(test_odbc_writer)))
	end} end,
	fun(Rec) -> {"call_terminate with agent", fun() ->
		CpxAgentEvent = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{agent, "testagent"}, [
			{login, "testagent"},
			{skills, [{'_queue', "Q"}]}
		], "node"}}},
		CpxMediaEvent = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{media, "testmedia"}, [
			{callerid, {"ignored", "a*b*c*d"}},
			{queue, "Q"}
		], "node"}}},
		Call = #call{id = "testmedia", source = self(), callerid = {"ignored", "a*b*c*d"}},
		CpxMediaAnswer = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{agent, "testagent"}, [
			{state, oncall},
			{statedata, Call}
		], "node"}}},
		CpxMediaDie = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{agent, "testagent"}, [
			{login, "testagent"},
			{skills, [{'_queue', "Q"}]},
			{state, wrapup},
			{statedata, Call}
		], "node"}}},
		Self = self(),
		ignore_infos(test_odbc_writer, 4),
		% "316655"; "ca1acd05.infonxx.local"; "call_terminate"; 
		% "ca1acd05.infonxx.local"; "5045"; ""; "15162337*112"; "9080"; "112";
		% "SRVCC_English"; "SRVCC_English"; ""; "6108491500*15162337*112*9080";
		% ""; ""; "2010-12-17 07:24:21.30899-05"; ""; 
		% "openacd : 1292588661.309172 : ca1acd05.infonxx.local : 2010-12-17T12:24:21.308990Z : call_terminate : testme@ca1acd05 : SRVCC_English : 5045 : 6108491500*15162337*112*9080 : cf97c474-c691-4747-ba8f-8f05375e9a90 : Origin : CLS : Source IP : 9080";
		% "6108491500"; "openacd"
		gen_server_mock:expect_info(test_odbc_writer, 
			fun({_, In}, _) ->
				Expected = #event_log_row{
					hostname = get_FQDN(),
					event_type = call_terminate,
					acd_name = get_FQDN(),
					acd_agent_id = "testagent",
					uci = "b*c",
					did = "d",
					origin_code = "c",
					queue = "Q",
					queue_name = "Q",
					from_header = "a*b*c*d",
					acd_type = "openacd",
					ani = "a",
					created_at = In#event_log_row.created_at
				},
				case {?assertEqual(Expected, In), ?assertNot(undefined =:= In#event_log_row.created_at)} of
					{ok, ok} ->
						Self ! ok
				end
			end
		),
		Msgs = [CpxAgentEvent, CpxMediaEvent, CpxMediaAnswer, CpxMediaDie],
		[cpx_monitor_odbc_supervisor ! X || X <- Msgs],
		timer:sleep(10),
		receive_oks(1),
		?assertEqual(ok, gen_server_mock:assert_expectations(whereis(test_odbc_writer)))
	end} end,
	fun(Rec) -> {"call_complete", fun() ->
		Self = self(),
		CpxAgentEvent = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{agent, "testagent"}, [
			{login, "testagent"},
			{skills, [{'_queue', "Q"}]}
		], "node"}}},
		CpxMediaEvent = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{media, "testmedia"}, [
			{callerid, {"ignored", "a*b*c*d"}},
			{queue, "Q"}
		], "node"}}},
		Call = #call{id = "testmedia", source = self(), callerid = {"ignored", "a*b*c*d"}},
		CpxMediaAnswer = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{agent, "testagent"}, [
			{state, oncall},
			{statedata, Call}
		], "node"}}},
		CpxMediaDie = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{agent, "testagent"}, [
			{login, "testagent"},
			{skills, [{'_queue', "Q"}]},
			{state, wrapup},
			{statedata, Call}
		], "node"}}},
		CpxAgentEndwrap = {cpx_monitor_event, {set, Rec#test_conf.timestamp, {{agent, "testagent"}, [
			{login, "testagent"},
			{skills, [{'_queue', "Q"}]},
			{state, idle}
		], "node"}}},
		Msgs = [CpxAgentEvent, CpxMediaEvent, CpxMediaAnswer, CpxMediaDie, CpxAgentEndwrap],
		ignore_infos(test_odbc_writer, 5),
		% "316656"; "ca1acd05.infonxx.local"; "call_complete"; ""; ""; ""; "";
		% ""; ""; ""; ""; ""; ""; ""; ""; "2010-12-17 07:24:21.836007-05"; "";
		% "openacd : 1292588661.836533 : ca1acd05.infonxx.local : 2010-12-17T12:24:21.836007Z : call_complete : testme@ca1acd05 : SRVCC_English : 5045 : 6108491500*15162337*112*9080 : cf97c474-c691-4747-ba8f-8f05375e9a90 : Origin : CLS : Source IP : 9080";
		% ""; "openacd"
		gen_server_mock:expect_info(test_odbc_writer, 
			fun({_, In}, _) ->
				Expected = #event_log_row{
					hostname = get_FQDN(),
					event_type = call_complete,
					acd_type = "openacd",
					created_at = In#event_log_row.created_at
				},
				case {?assertEqual(Expected, In), ?assertNot(undefined =:= In#event_log_row.created_at)} of
					{ok, ok} ->
						Self ! ok
				end
			end
		),
		gen_server_mock:expect_info(test_odbc_writer,
			fun({_Ref, #event_log_row{event_type = agent_available}}, _) ->
				ok
			end
		),
		[cpx_monitor_odbc_supervisor ! X || X <- Msgs],
		timer:sleep(10),
		receive_oks(1),
		?assertEqual(ok, gen_server_mock:assert_expectations(whereis(test_odbc_writer)))
	end} end]}}.

spawn_jack(Target) ->
	spawn_link(fun() -> jack_the_ripper(Target) end).

jack_the_ripper(Target) when is_atom(Target) ->
	jack_the_ripper(Target, 4).

jack_the_ripper(Target, Max) when is_atom(Target) ->
	jack_the_ripper(Target, 0, Max).
	
jack_the_ripper(_Target, Max, Max) ->
	ok;
jack_the_ripper(Target, Count, Max) when Count < Max ->
	case whereis(Target) of
		undefined ->
			timer:sleep(10),
			jack_the_ripper(Target, Count, Max);
		Pid ->
			exit(Pid, kill),
			timer:sleep(10),
			jack_the_ripper(Target, Count + 1, Max)
	end.

murder_tests() ->
	{inorder, {foreach, 
	fun() ->
		Ets = ets:new(cpx_monitor, [named_table]),
		{ok, Qm} = gen_leader_mock:start(queue_manager),
		gen_leader_mock:expect_leader_call(Qm, fun(_, _, State, _) -> 
			{ok, [], State}
		end),
		#test_conf{
			ets = Ets,
			queue_man = Qm
		}
	end,
	fun(Conf) ->
		try gen_server_mock:stop(whereis(test_odbc_writer))
		catch
			_:_ -> ok
		end,
		gen_leader_mock:stop(whereis(queue_manager)),
		ets:delete(Conf#test_conf.ets),
		% give it time to die
		timer:sleep(10)
	end,
	[fun(_) -> {"Killing the writer is noticed", fun() ->
		{ok, #state{odbc_pid = Odbc} = State} = init(["fake_dsn", []]),
		gen_server_mock:crash(whereis(test_odbc_writer)),
		{noreply, NewState} = receive 
			{'EXIT', Odbc, Reason} = Msg ->
				?DEBUG("~p", [Reason]),
				handle_info(Msg, State)
		after 20 ->
			?assert("didn't get exit message")
		end,
		?assert(is_reference(NewState#state.odbc_pid))
	end} end,
	fun(_) -> {"Periodic check for writer resurrectoin", fun() ->
		{ok, #state{odbc_pid = Odbc} = State} = init(["fake_dsn", []]),
		gen_server_mock:crash(whereis(test_odbc_writer)),
		{noreply, WaitForCheck} = receive
			{'EXIT', Odbc, crash} = Msg ->
				handle_info(Msg, State)
		after 20 ->
			?assert("didn't get exit message")
		end,
		ignore_infos(test_odbc_writer, 1),
		{noreply, GotCheck} = receive
			check_odbc ->
				handle_info(check_odbc, WaitForCheck)
		after 120 ->
			?assert("didn't get check_odbc message")
		end,
		?assert(is_pid(GotCheck#state.odbc_pid))
	end} end,
	fun(_) -> {"Notices when writer is ressurected", fun() ->
		{ok, #state{odbc_pid = Odbc} = State} = init(["fake_dsn", []]),
		gen_server_mock:crash(whereis(test_odbc_writer)),
		WaitForUp = fun(W) ->
			timer:sleep(10),
			case whereis(test_odbc_writer) of
				undefined ->
					W(W);
				_P ->
					ignore_infos(test_odbc_writer, 1),
					ok
			end
		end,
		WaitForUp(WaitForUp),
		Consumer = fun
			(_C, _FState, 1000) ->
				?assert("1000 interations is too many, something's wrong");
			(C, FState, Count) ->
				receive
					Msg ->
						{noreply, NewState} = handle_info(Msg, FState),
						C(C, NewState, Count + 1)
				after (?Check_interval + 20) ->
					FState
				end
		end,
		ResState = Consumer(Consumer, State, 0),
		?assert(Odbc =/= ResState#state.odbc_pid),
		?assert(is_pid(ResState#state.odbc_pid)),
		?assertEqual(ok, gen_server_mock:assert_expectations(whereis(test_odbc_writer)))
	end} end,
	fun(_) -> {timeout, 20, {"Killing writer permanently is noticed", fun() ->
		{ok, #state{odbc_sup_pid = Sup} = State} = init(["fake_dsn", []]),
		Jack = spawn_jack(test_odbc_writer),
		receive
			{'EXIT', Jack, Reason} ->
				?assertEqual(normal, Reason)
		after 5020 ->
			?assert("Jack didn't kill fast enough")
		end,
		{noreply, _SupDeadState} = receive
			{'EXIT', Sup, _SupDeadY} = Msg ->
				handle_info(Msg, State)
		after 5020 ->
			?assert("Doubtful the exit from the supervisor was recieved")
		end
	end}} end]}}.

-endif.
