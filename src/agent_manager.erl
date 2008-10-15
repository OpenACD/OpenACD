-module(agent_manager).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-export([start_link/0, start/0, stop/0, start_agent/1, query_agent/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("call.hrl").
-include("agent.hrl").

-type(mod_state() :: [{string(), pid()}]).

-spec(start_link/0 :: () -> 'ok').
start_link() -> 
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	
-spec(start/0 :: () -> 'ok').
start() -> 
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

-spec(stop/0 :: () -> 'ok').
stop() ->
	gen_server:call(?MODULE, stop).

init([]) -> 
	process_flag(trap_exit, true),
	case global:whereis_name(?MODULE) of
		undefined ->
				global:register_name(?MODULE, self(), {global, random_notify_name});
		GID -> 
			link(GID)
		end,
	{ok, dict:new()}.

-spec(start_agent/1 :: (Agent :: #agent{}) -> {'ok', pid()}).
start_agent(Agent) -> 
	gen_server:call(?MODULE, {start_agent, Agent}).

%-spec(find_agent/1 :: (Agent :: #agent{}) -> node() | 'undefined').
%find_agent(#agent{login=Login} = Agent) -> 
	

-spec(query_agent/1 ::	(Agent :: #agent{}) -> {'true', pid()} | 'false';
						(Login :: string()) -> {'true', pid()} | 'false').
query_agent(#agent{login=Login}) -> 
	gen_server:call(?MODULE, {exists, Login});
query_agent(Login) -> 
	gen_server:call(?MODULE, {exists, Login}).

-spec(sync_agents/1 :: (Dict :: mod_state()) -> mod_state()).
sync_agents(Dict) -> 
	Dict.

handle_call({start_agent, #agent{login=Login} = Agent}, _From, State) ->
	% starts a new agent and returns the state of that agent.
	case dict:find(Login, State) of 
		{ok, Pid} -> 
			{reply, {exists, Pid}, State};
		error -> 
			Self = self(),
			case global:whereis_name(?MODULE) of
				Self -> 
					{ok, Pid} = agent:start(Agent),
					{reply, {ok, Pid}, dict:store(Login, Pid, State)};
				undefined -> 
					global:register_name(?MODULE, self(), {global, random_notify_name}),
					{ok, Pid} = agent:start(Agent),
					{reply, {ok, Pid}, dict:store(Login, Pid, State)};
				_ -> 
					try gen_server:call({global, ?MODULE}, {exists, Login}) of 
						{true, Pid} ->
							{reply, {exists, Pid}, State};
						false -> 
							{ok, Pid} = agent:start(Agent),
							gen_server:call({global, ?MODULE}, {notify, Login, Pid}), % like the queue manager, handle a timeout.
							{reply, {ok, Pid}, dict:store(Login, Pid, State)}
					catch
						exit:{timeout, _} -> 
							global:register_name(?MODULE, self(), {global, random_notify_name}),
							{ok, Pid} = agent:start(Agent),
							{reply, {ok, Pid}, dict:store(Login, Pid, State)}
					end
			end
	end;

handle_call({exists, Login}, _From, State) ->
	case dict:find(Login, State) of 
		{ok, Pid} -> 
			{reply, {true, Pid}, State};
		error -> 
			{reply, false, State}
	end;

handle_call({notify, Login, Pid}, _From, State) -> 
	{reply, ok, dict:store(Login, Pid, State)};

handle_call({get_process, Login}, _From, State) -> 
	case dict:is_key(Login, State) of 
		true -> 
			{reply, dict:find(Login, State), State};
		false -> 
			{reply, no_process, State}
	end;

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(Request, _From, State) ->
	{reply, {unknown_call, Request}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.


handle_info({'EXIT', From, _Reason}, State) -> 
	{noreply, dict:filter(
		fun(_Key, Val) -> 
			node(From) =/= node(Val)
		end,
	State)
	};
	
handle_info({global_name_conflict, _Name}, State) ->
	io:format("Node ~p lost election", [node()]),
	link(global:whereis_name(?MODULE)),
	{noreply, sync_agents(State)};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-ifdef('EUNIT').

handle_call_start_test() -> 
	?assertMatch({ok, _Pid}, start()),
	stop().

single_node_test_() -> 
	Agent = #agent{login="testagent"},
	{foreach,
		fun() -> 
			start(),
			{}
		end,
		fun({}) -> 
			stop()
		end,
		[
			{"Start New Agent", 
				fun() -> 
					{ok, Pid} = gen_server:call(?MODULE, {start_agent, Agent}),
					?assertMatch({ok, released}, agent:query_state(Pid))
				end
			},
			{"Start Existing Agent",
				fun() -> 
					{ok, Pid} = gen_server:call(?MODULE, {start_agent, Agent}),
					?assertMatch({exists, Pid}, gen_server:call(?MODULE, {start_agent, Agent}))
				end
			},
			{"Lookup agent by name",
				fun() -> 
					{ok, Pid} = gen_server:call(?MODULE, {start_agent, Agent}),
					Login = Agent#agent.login,
					?assertMatch({true, Pid}, query_agent(Login))
				end
			}
		]
	}.



get_nodes() ->
	[_Name, Host] = string:tokens(atom_to_list(node()), "@"),
	{list_to_atom(lists:append("master@", Host)), list_to_atom(lists:append("slave@", Host))}.

multi_node_test_() -> 
	{Master, Slave} = get_nodes(),
	Agent = #agent{login="testagent"},
	Agent2 = #agent{login="testagent2"},
	{
		foreach,
		fun() -> 
			slave:start(net_adm:localhost(), master, " -pa debug_ebin"), 
			slave:start(net_adm:localhost(), slave, " -pa debug_ebin"),
			cover:start([Master, Slave]),
			rpc:call(Master, global, sync, []),
			rpc:call(Slave, global, sync, []),
			rpc:call(Master, agent_manager, start, []),
			rpc:call(Slave, agent_manager, start, []),
			rpc:call(Master, global, sync, []),
			rpc:call(Slave, global, sync, []),
			{}
		end,
		fun({}) -> 
			cover:stop([Master, Slave]),
			slave:stop(Master),
			slave:stop(Slave),
			ok
		end,
		[
			{
				"Slave picks up added agent",
				fun() -> 
					{ok, Pid} = rpc:call(Master, agent_manager, start_agent, [Agent]),
					?assertMatch({exists, Pid}, rpc:call(Slave, agent_manager, start_agent, [Agent]))
				end
			},
			{
				"Slave continues after master dies",
				fun() -> 
					{ok, _Pid} = rpc:call(Master, agent_manager, start_agent, [Agent]),
					rpc:call(Master, erlang, disconnect_node, [Slave]),
					rpc:call(Slave, erlang, disconnect_node, [Master]),
					?assertMatch({ok, _NewPid}, rpc:call(Slave, agent_manager, start_agent, [Agent]))
				end
			},
			{
				"Slave becomes master after master dies",
				fun() -> 
					rpc:call(Master, erlang, disconnect_node, [Slave]),
					cover:stop([Master]),
					slave:stop(Master),
					
					?assertMatch(undefined, global:whereis_name(?MODULE)),
					?assertMatch({ok, _Pid}, rpc:call(Slave, agent_manager, start_agent, [Agent])),
					?assertMatch({true, _Pid}, rpc:call(Slave, agent_manager, query_agent, [Agent])),
					Globalwhere = global:whereis_name(agent_manager),
					Slaveself = rpc:call(Slave, erlang, whereis, [agent_manager]),
					?assertMatch(Globalwhere, Slaveself)
				end
			}, {
				"Net Split",
				fun() ->
					rpc:call(Master, erlang, disconnect_node, [Slave]),
					rpc:call(Slave, erlang, disconnect_node, [Master]),


					?assertMatch({ok, _Pid}, rpc:call(Master, agent_manager, start_agent, [Agent])),
					?assertMatch({ok, _Pid}, rpc:call(Slave, agent_manager, start_agent, [Agent])),

					Pinged = rpc:call(Master, net_adm, ping, [Slave]),
					Pinged = rpc:call(Slave, net_adm, ping, [Master]),

					?assert(Pinged =:= pong),

					rpc:call(Master, global, sync, []),
					rpc:call(Slave, global, sync, []),

					
					Newmaster = node(global:whereis_name(?MODULE)),

					receive after 1000 -> ok end,
					?assertMatch(Newmaster, Master)
				end
			}, {
				"Master removes agents for a dead node",
				fun() ->
					?assertMatch({ok, _Pid}, rpc:call(Slave, agent_manager, start_agent, [Agent])),
					?assertMatch({ok, _Pid}, rpc:call(Master, agent_manager, start_agent, [Agent2])),
					?assertMatch({true, _Pid}, rpc:call(Master, agent_manager, query_agent, [Agent])),
					rpc:call(Master, erlang, disconnect_node, [Slave]),
					cover:stop(Slave),
					slave:stop(Slave),
					?assertEqual(false, rpc:call(Master, agent_manager, query_agent, [Agent])),
					?assertMatch({true, _Pid}, rpc:call(Master, agent_manager, query_agent, [Agent2])),
					?assertMatch({ok, _Pid}, rpc:call(Master, agent_manager, start_agent, [Agent]))
				end
			}
		]
	}.

-endif.

