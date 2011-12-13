-module(cpx_agent_event).
-behavior(gen_event).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("agent.hrl").
-include_lib("stdlib/include/qlc.hrl").

% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	terminate/2, code_change/3]).

% api
-export([
	start/0,
	start_link/0,
	agent_init/1,
	agent_channel_init/2
]).

%% =====
%% API
%% =====

%% @doc starts the agent event server.
-spec(start/0 :: () -> {'ok', pid()}).
start() ->
	{ok, Nodes} = cpx:get_env(nodes, [node()]),
	start(Nodes).

%% @doc Starts the event server using the given nodes as part of the
%% mnesia cluster.
-spec(start/1 :: (Nodes :: [node()]) -> {'ok', pid()}).
start(Nodes) ->
	case build_tables(Nodes) of
		ok -> ok; 
		Else ->
			?WARNING("Some tables didn't build, this may crash later. ~p", [Else])
	end,
	gen_event:start({local, ?MODULE}).

%% @doc Starts teh agent event server linked.
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() ->
	{ok, Nodes} = cpx:get_env(nodes, [node()]),
	start_link(Nodes).

%% @doc Starts the agent event server linked using the given ndoes as part
%% of the mnesia cluster.
-spec(start_link/1 :: (Nodes :: [atom()]) -> {'ok', pid()}).
start_link(Nodes) ->
	case build_tables(Nodes) of
		ok -> ok;
		Else ->
			?WARNING("Some tables didn't build, this may crash later. ~p", [Else])
	end,
	gen_event:start_link({local, ?MODULE}).

%% @doc Create a handler specifically for the given agent.  Handles profile
%% login, logout, profile changes, release, and idle states.
-spec(agent_init/1 :: (Agent :: string()) -> 'ok' | {atom(), any()}).
agent_init(Agent) ->
	try begin
		Handlers = gen_event:which_handlers(?MODULE),
		Member = lists:member({?MODULE, Agent}, Handlers),
		case Member of
			false ->
				ok = gen_event:add_handler(?MODULE, {?MODULE, Agent}, Agent);
			_ ->
				?INFO("Agent Event already initialized for ~s", [Agent])
		end
	end catch
		What:Why ->
			?ERROR("Initializing Agent Event Failed due to ~p:~p", [What,Why]),
			{What, Why}
	end.

%% @doc Create a handler specifically for the given agent channel.
-spec(agent_channel_init/2 :: (Agent :: string(), ChannelId :: reference()) -> 'ok' | {atom(), any()}).
agent_channel_init(Agent, ChannelId) ->
	try begin
		Handlers = gen_event:which_handlers(?MODULE),
		Member = lists:member({?MODULE, ChannelId}, Handlers),
		case Member of
			false ->
				ok = gen_event:add_handler(?MODULE, {?MODULE, ChannelId}, [Agent, ChannelId]);
			_ ->
				?INFO("Agent channel ~p already has event initialized", [ChannelId])
		end
	end catch
		What:Why ->
			?ERROR("Initialization failed for channel id ~s:~p due to ~p:~p", [Agent, ChannelId, What, Why]),
			{What, Why}
	end.

%% =====
%% gen_event callbacks
%% =====

%% -----
%% init
%% -----

%% @private
init([Agent, ChannelId]) when is_reference(ChannelId) ->
	{ok, {Agent, ChannelId}};

init(Agent) ->
	{ok, {Agent}}.

%% -----
%% handle_event
%% -----

%% @private
handle_event(Event, State) ->
	{ok, State}.

%% -----
%% handle_call
%% -----

%% @private
handle_call(Request, State) ->
	{ok, invalid, State}.

%% -----
%% handle_info
%% -----

%% @private
handle_info(Info, State) ->
	{ok, State}.

%% ----
%% terminate
%% -----

%% @private
terminate(Why, State) -> ok.

%% -----
%% code_change
%% -----

%% @private
code_change(OldVsn, State, Extra) ->
	{ok, State}.

%% =====
%% Internal
%% =====

build_tables(Nodes) ->
	Agent = util:build_table(agent_state, [
		{attributes, record_info(fields, agent_state)},
		{disc_copies, Nodes}
	]),
	Channel = util:build_table(agent_channel_state, [
		{attributes, record_info(fields, agent_channel_state)},
		{disc_copies, Nodes}
	]),
	Successes = [exists, copied, {atomic, ok}],
	case {lists:member(Agent, Successes), lists:member(Channel, Successes)} of
		{true, true} -> ok;
		_ ->
			?WARNING("One of the tables didn't build.  Agent:  ~p;  Channel:  ~p", [Agent, Channel]),
			{Agent, Channel}
	end.

%% =====
%% Test
%% =====


-spec(log_loop/4 :: (Id :: string(), Agentname :: string(), Nodes :: [atom()], Profile :: string() | {string(), string()}) -> 'ok').
log_loop(Id, Agentname, Nodes, ProfileTup) ->
	process_flag(trap_exit, true),
	Profile = case ProfileTup of
		{Currentp, _Queuedp} ->
			Currentp;
		_ ->
			ProfileTup
	end,
	receive
		{Agentname, login, State, {Skills, Statedata}} ->
			F = fun() ->
				Now = util:now(),
				Login = #agent_state{
					id = Id, 
					agent = Agentname, 
					oldstate = login, 
					state=State,
					statedata = Skills,
					start = Now, 
					ended = Now, 
					profile= Profile, 
					nodes = Nodes
				},
				StateRow = #agent_state{
					id = Id,
					agent = Agentname,
					oldstate = State,
					statedata = Statedata,
					start = Now,
					nodes = Nodes
				},
				mnesia:dirty_write(Login),
				mnesia:dirty_write(StateRow),
				gen_cdr_dumper:update_notify(agent_state),
				cpx_monitor:info({agent_state, Login}),
				cpx_monitor:info({agent_state, StateRow}),
				ok
			end,
			Res = mnesia:async_dirty(F),
			?DEBUG("res of agent state login:  ~p", [Res]),
			agent:log_loop(Id, Agentname, Nodes, ProfileTup);
		{'EXIT', _Apid, _Reason} ->
			F = fun() ->
				Now = util:now(),
				QH = qlc:q([Rec || Rec <- mnesia:table(agent_state), Rec#agent_state.id =:= Id, Rec#agent_state.ended =:= undefined]),
				Recs = qlc:e(QH),
				?DEBUG("Recs to loop through:  ~p", [Recs]),
				lists:foreach(
					fun(Untermed) ->
						mnesia:delete_object(Untermed), 
						Termed = Untermed#agent_state{ended = Now, timestamp = Now, state = logout},
						cpx_monitor:info({agent_state, Termed}),
						mnesia:write(Termed)
					end,
					Recs
				),
				gen_cdr_dumper:update_notify(agent_state),
				%Stateage = fun(#agent_state{start = A}, #agent_state{start = B}) ->
					%B =< A
				%end,
				%[#agent_state{state = Oldstate} | _] = lists:sort(Stateage, Recs),
				%Newrec = #agent_state{id = Id, agent = Agentname, state = logout, oldstate = State, statedata = "job done", start = Now, ended = Now, timestamp = Now, nodes = Nodes},
				%mnesia:write(Newrec),
				ok
			end,
			Res = mnesia:async_dirty(F),
			?DEBUG("res of agent state change log:  ~p", [Res]),
			ok;
		{change_profile, Newprofile, State} when State == idle; State == released ->
			agent:log_loop(Id, Agentname, Nodes, Newprofile);
		{change_profile, Newprofile, _State} ->
			case ProfileTup of
				{Current, _Queued} ->
					agent:log_loop(Id, Agentname, Nodes, {Current, Newprofile});
				Current ->
					agent:log_loop(Id, Agentname, Nodes, {Current, Newprofile})
			end;
		{Agentname, State, _OldState, Statedata} ->
			F = fun() ->
				Now = util:now(),
				QH = qlc:q([Rec || Rec <- mnesia:table(agent_state), Rec#agent_state.id =:= Id, Rec#agent_state.ended =:= undefined]),
				Recs = qlc:e(QH),
				lists:foreach(
					fun(Untermed) -> 
						mnesia:delete_object(Untermed),
						Termed = Untermed#agent_state{ended = Now, timestamp = Now, state = State},
						cpx_monitor:info({agent_state, Termed}),
						mnesia:write(Termed)
					end,
					Recs
				),
				gen_cdr_dumper:update_notify(agent_state),
				Newrec = #agent_state{id = Id, agent = Agentname, oldstate = State, statedata = Statedata, profile = Profile, start = Now, nodes = Nodes},
				cpx_monitor:info({agent_state, Newrec}),
				mnesia:write(Newrec),
				ok
			end,
			Res = mnesia:async_dirty(F),
			?DEBUG("res of agent ~p state change ~p log:  ~p", [Id, State, Res]),
			case {State, ProfileTup} of
				{State, {Profile, Queued}} when State == wrapup; State == idle; State == released ->
					agent:log_loop(Id, Agentname, Nodes, Queued);
				_ ->
					agent:log_loop(Id, Agentname, Nodes, ProfileTup)
			end
	end.





