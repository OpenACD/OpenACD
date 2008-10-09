-module(queue_manager).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-export([start_link/0, start/0, add_queue/1, query_queue/1, stop/0, print/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

add_queue(Name) ->
	gen_server:call(?MODULE, {add, Name}, infinity).

query_queue(Name) ->
	try gen_server:call({global, ?MODULE}, {exists, Name}) of
		Foo -> Foo
	catch
		exit:{noproc,_} ->
			global:register_name(?MODULE, whereis(queue_manager), {global, random_notify_name}),
			gen_server:call({global, ?MODULE}, {exists, Name})
	end.

stop() ->
	gen_server:call(?MODULE, stop).

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
			{reply, exists, State};
		false ->
			Self = self(),
			case global:whereis_name(?MODULE) of
				Self ->
					{reply, ok, dict:append(Name, {}, State)};
				undefined -> 
					global:register_name(?MODULE, self(), {global, random_notify_name}),
					{reply, ok, dict:append(Name, {}, State)};
				_ ->
					try gen_server:call({global, ?MODULE}, {exists, Name}) of
						true ->
							{reply, exists, State};
						false ->
							gen_server:call({global, ?MODULE}, {notify, Name}), % TODO - handle timeout exception
							{reply, ok, dict:append(Name, {}, State)}
					catch
						exit:{timeout, _} ->
							global:register_name(?MODULE, self(), {global, random_notify_name}),
							{reply, timeout, dict:append(Name, {}, State)}
					end
			end
	end;
handle_call({exists, Name}, _From, State) ->
	{reply, dict:is_key(Name, State), State};
handle_call({notify, Name}, From, State) ->
	{reply, ok, dict:append(Name, From, State)};
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
	{noreply, dict:filter(fun(_K,[{}]) -> true; (K,[{V,_}]) -> io:format("Trying to remove ~p.~n", [K]), node(From) =/= node(V) end, State)};
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
	{list_to_atom(lists:append("master@", net_adm:localhost())), list_to_atom(lists:append("slave@", net_adm:localhost()))}.

add_and_query_test() ->
	start_link(),
	?assertMatch(ok, add_queue(goober)),
	?assertMatch(exists, add_queue(goober)),
	?assertMatch(true, query_queue(goober)),
	?assertMatch(false, query_queue(foobar)),
	stop().

multi_node_test_() ->
	{Master, Slave} = get_nodes(),
	{
		foreach,
		fun() ->
			slave:start(net_adm:localhost(), master), 
			slave:start(net_adm:localhost(), slave), 
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
					?assertMatch(ok, rpc:call(Slave, queue_manager, add_queue, [queue1])),
					?assertMatch(true, rpc:call(Slave, queue_manager, query_queue, [queue1]))
				end
			},{
				"Net Split",fun() ->
					rpc:call(Master, queue_manager, add_queue, [queue1]),

					?assertMatch(true, rpc:call(Slave, queue_manager, query_queue, [queue1])),

					rpc:call(Master, erlang, disconnect_node, [Slave]),
					rpc:call(Slave, erlang, disconnect_node, [Master]),

					?assertMatch(ok, rpc:call(Slave, queue_manager, add_queue, [queue2])),
					?assertMatch(true, rpc:call(Slave, queue_manager, query_queue, [queue2])),
					?assertMatch(ok, rpc:call(Slave, queue_manager, add_queue, [queue3])),
					?assertMatch(ok, rpc:call(Master, queue_manager, add_queue, [queue2])),
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
					?assertMatch(exists, rpc:call(Master, queue_manager, add_queue, [queue2])),
					?assertMatch(exists, rpc:call(Master, queue_manager, add_queue, [queue1]))
				end
			},{
				"Queues in sync", fun() ->
					rpc:call(Master, queue_manager, add_queue, [queue1]),

					?assertMatch(true, rpc:call(Master, queue_manager, query_queue, [queue1])),
					?assertMatch(exists, rpc:call(Slave, queue_manager, add_queue, [queue1])),
					?assertMatch(ok, rpc:call(Slave, queue_manager, add_queue, [queue2])), 
					?assertMatch(true, rpc:call(Master, queue_manager, query_queue, [queue2])),
					?assertMatch(exists, rpc:call(Master, queue_manager, add_queue, [queue2])),

					?assertMatch(ok, rpc:call(Master, queue_manager, stop, [])),
					?assertMatch(ok, rpc:call(Slave, queue_manager, stop, []))
				end
			}
		]
	}.


-endif.
