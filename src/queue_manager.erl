-module(queue_manager).

%% depends on call_queue

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-export([start_link/0, start/0, add_queue/1, get_queue/1, query_queue/1, stop/0, print/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-spec(start_link/0 :: () -> 'ok').
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec(start/0 :: () -> 'ok').
start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

-spec(add_queue/1 :: (Name :: atom()) -> {'ok', pid()} | {'exists', pid()}).
add_queue(Name) ->
	gen_server:call(?MODULE, {add, Name}, infinity).

-spec(get_queue/1 :: (Name :: atom()) -> pid() | undefined).
get_queue(Name) -> 
	try gen_server:call({global, ?MODULE}, {get_queue, Name}) of
		Foo -> Foo
	catch
		exit:{noproc, _} -> 
			global:register_name(?MODULE, whereis(?MODULE), {global, random_notify_name}),
			gen_server:call({global, ?MODULE}, {get_queue, Name})
	end.
	
-spec(query_queue/1 :: (Name :: atom()) -> bool()).
query_queue(Name) ->
	try gen_server:call({global, ?MODULE}, {exists, Name}) of
		Foo -> Foo
	catch
		exit:{noproc,_} ->
			global:register_name(?MODULE, whereis(?MODULE), {global, random_notify_name}),
			gen_server:call({global, ?MODULE}, {exists, Name})
	end.

-spec(stop/0 :: () -> 'ok').
stop() ->
	gen_server:call(?MODULE, stop).

-spec(print/0 :: () -> any()).
print() ->
	gen_server:call(?MODULE, print).

sync_queues([H|T]) ->
	{K,_V} = H,
	try gen_server:call({global, ?MODULE}, {exists, K}) of
		true ->
			io:format("Queue conflict detected for ~p~n", [K]),
			sync_queues(T);
		false ->
			io:format("notifying master of ~p~n", [K]),
			try gen_server:call({global, ?MODULE}, {notify, K}) of
				_ -> sync_queues(T)
			catch
				exit:{timeout, _} -> % What to do here?
					[]
			end
	catch
		exit:{timeout, _} -> % What to do here?
			[]
	end;
sync_queues([]) ->
	[].

init([]) ->
	process_flag(trap_exit, true),
	case global:whereis_name(?MODULE) of
		undefined ->
			global:register_name(?MODULE, self(), {global, random_notify_name});
		GID -> 
			link(GID)
	end,
	{ok, dict:new()}.

handle_call({add, Name}, _From, State) ->
	case dict:is_key(Name, State) of
		true ->
			{ok, Pid} = dict:find(Name, State),
			{reply, {exists, Pid}, State};
		false ->
			Self = self(),
			case global:whereis_name(?MODULE) of
				Self ->
					{ok, Pid} = call_queue:start_link(Name),
					{reply, {ok, Pid}, dict:store(Name, Pid, State)};
				undefined -> 
					global:register_name(?MODULE, self(), {global, random_notify_name}),
					{ok, Pid} = call_queue:start_link(Name),
					{reply, {ok, Pid}, dict:store(Name, Pid, State)};
				_ ->
					try gen_server:call({global, ?MODULE}, {exists, Name}) of
						true ->
							Pid = gen_server:call({global, ?MODULE}, {get_queue, Name}),
							{reply, {exists, Pid}, State};
						false ->
							{ok, Pid} = call_queue:start_link(Name),
							gen_server:call({global, ?MODULE}, {notify, Name, Pid}), % TODO - handle timeout exception
							{reply, {ok, Pid}, dict:store(Name, Pid, State)}
					catch
						exit:{timeout, _} ->
							global:register_name(?MODULE, self(), {global, random_notify_name}),
							{ok, Pid} = call_queue:start_link(Name),
							{reply, timeout, dict:store(Name, Pid, State)}
					end
			end
	end;
handle_call({exists, Name}, _From, State) ->
	{reply, dict:is_key(Name, State), State};
handle_call({get_queue, Name}, _From, State) ->
	case dict:find(Name, State) of
		{ok, Pid} ->
			{reply, Pid, State};
		error ->
			{reply, undefined, State}
	end;	
handle_call({notify, Name, Pid}, _From, State) ->
	{reply, ok, dict:store(Name, Pid, State)};
handle_call(print, _From, State) ->
	{reply, State, State};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'EXIT', From, _Reason}, State) ->
	% filter out any queues on the dead node
	{noreply, dict:filter(fun(K,V) -> io:format("Trying to remove ~p.~n", [K]), node(From) =/= node(V) end, State)};
handle_info({global_name_conflict, _Name}, State) ->
	io:format("Node ~p lost the election~n", [node()]),
	link(global:whereis_name(?MODULE)),
	% loop over elements
	sync_queues(dict:to_list(State)),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-ifdef('EUNIT').

get_nodes() ->
	[_Name, Host] = string:tokens(atom_to_list(node()), "@"),
	{list_to_atom(lists:append("master@", Host)), list_to_atom(lists:append("slave@", Host))}.

add_and_query_test() ->
	start_link(),
	?assertMatch({ok, _Pid}, add_queue(goober)),
	?assertMatch({exists, _Pid}, add_queue(goober)),
	?assertMatch(true, query_queue(goober)),
	?assertMatch(false, query_queue(foobar)),
	stop().

get_test() -> 
	start_link(), 
	{ok, Pid} = add_queue(goober),
	?assertMatch(Pid, get_queue(goober)),
	?assertMatch(undefined, get_queue(no_exists)),
	stop().


multi_node_test_() ->
	{Master, Slave} = get_nodes(),
	{
		foreach,
		fun() ->
			slave:start(net_adm:localhost(), master, " -pa debug_ebin"), 
			slave:start(net_adm:localhost(), slave, " -pa debug_ebin"), 
			cover:start([Master, Slave]),
			rpc:call(Master, global, sync, []),
			rpc:call(Slave, global, sync, []),
			rpc:call(Master, queue_manager, start, []),
			rpc:call(Slave, queue_manager, start, []),
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
				"Master Death", fun() ->
					rpc:call(Master, erlang, disconnect_node, [Slave]),
					cover:stop([Master]),
					slave:stop(Master),

					?assertMatch(undefined, global:whereis_name(?MODULE)),
					?assertMatch({ok, _Pid}, rpc:call(Slave, queue_manager, add_queue, [queue1])),
					?assertMatch(true, rpc:call(Slave, queue_manager, query_queue, [queue1]))
				end
			},{
				"Net Split",fun() ->
					rpc:call(Master, queue_manager, add_queue, [queue1]),

					?assertMatch(true, rpc:call(Slave, queue_manager, query_queue, [queue1])),

					rpc:call(Master, erlang, disconnect_node, [Slave]),
					rpc:call(Slave, erlang, disconnect_node, [Master]),

					?assertMatch({ok, _Pid}, rpc:call(Slave, queue_manager, add_queue, [queue2])),
					?assertMatch(true, rpc:call(Slave, queue_manager, query_queue, [queue2])),
					?assertMatch({ok, _Pid}, rpc:call(Slave, queue_manager, add_queue, [queue3])),
					?assertMatch({ok, _Pid}, rpc:call(Master, queue_manager, add_queue, [queue2])),
					?assertMatch(true, rpc:call(Master, queue_manager, query_queue, [queue2])),

					Pinged = rpc:call(Master, net_adm, ping, [Slave]),
					Pinged = rpc:call(Master, net_adm, ping, [Slave]),

					?assert(Pinged =:= pong),

					rpc:call(Master, global, sync, []),
					rpc:call(Slave, global, sync, []),

					Newmaster = node(global:whereis_name(?MODULE)),

					receive after 1000 -> ok end,

					?assertMatch(Newmaster, Master),
					?assertMatch(true, rpc:call(Master, queue_manager, query_queue, [queue1])),
					?assertMatch(true, rpc:call(Master, queue_manager, query_queue, [queue2])),
					?assertMatch({exists, _Pid}, rpc:call(Master, queue_manager, add_queue, [queue2])),
					?assertMatch({exists, _Pid}, rpc:call(Master, queue_manager, add_queue, [queue1]))
				end
			},{
				"Queues in sync", fun() ->
					rpc:call(Master, queue_manager, add_queue, [queue1]),

					?assertMatch(true, rpc:call(Master, queue_manager, query_queue, [queue1])),
					?assertMatch({exists, _Pid}, rpc:call(Slave, queue_manager, add_queue, [queue1])),
					?assertMatch({ok, _Pid}, rpc:call(Slave, queue_manager, add_queue, [queue2])), 
					?assertMatch(true, rpc:call(Master, queue_manager, query_queue, [queue2])),
					?assertMatch({exists, _Pid}, rpc:call(Master, queue_manager, add_queue, [queue2])),

					?assertMatch(ok, rpc:call(Master, queue_manager, stop, [])),
					?assertMatch(ok, rpc:call(Slave, queue_manager, stop, []))
				end
			},{
				"No proc", fun() ->
					slave:stop(Master),
					?assertMatch(false, rpc:call(Slave, queue_manager, query_queue, [queue1]))
				end
			}
		]
	}.


-endif.
