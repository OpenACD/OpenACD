-module(queue_manager).

%% depends on call_queue

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("queue.hrl").
-include("call.hrl").

-behaviour(gen_server).

-export([start_link/0, start/0, add_queue/1, add_queue/2, add_queue/3, get_queue/1, query_queue/1, stop/0, print/0, get_best_bindable_queues/0]).

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
	gen_server:call(?MODULE, {add, Name, ?DEFAULT_RECIPE, ?DEFAULT_WEIGHT}, infinity).

-spec(add_queue/2 :: (Name :: atom(), Recipe :: recipe()) -> {'ok', pid()} | {'exists', pid()};
	(Name :: atom(), Weight :: pos_integer()) -> {'ok', pid()} | {'exists', pid()}).
add_queue(Name, Recipe) when is_list(Recipe) ->
	gen_server:call(?MODULE, {add, Name, Recipe, ?DEFAULT_WEIGHT});
add_queue(Name, Weight) when is_integer(Weight), Weight > 0 ->
	gen_server:call(?MODULE, {add, Name, ?DEFAULT_RECIPE, Weight}).

-spec(add_queue/3 :: (Name :: atom(), Recipe :: recipe(), Weight :: pos_integer()) -> {'ok', pid()} | {'exists', pid()}).
add_queue(Name, Recipe, Weight) ->
	gen_server:call(?MODULE, {add, Name, Recipe, Weight}).

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

-spec(get_best_bindable_queues/0 :: () -> [{atom(), pid(), #call{}, pos_integer()}]).
get_best_bindable_queues() ->
	try gen_server:call({global, ?MODULE}, queues_as_list) of
		List ->
			% TODO - this should be a damned list comprehension
			% see what calls we can bind to on each queue and what weight they have
			List2 = lists:map(fun({K,V}) -> {K, V, call_queue:ask(V), call_queue:get_weight(V)} end, List),
			% filter queues that have no bindable calls
			List3 = lists:filter(fun({_K,_V,Call,_W}) -> Call =/= none end, List2),
			% sort queues by queuetime of first bindable call, longest first (lowest unix epoch time)
			List4 = lists:sort(fun({_K1,_V1,{{_P1,T1},_Call1},_W1}, {_K2,_V2,{{_P2,T2},_Call2},_W2}) -> T1 =< T2 end, List3),
			% sort queues by priority of first bindable call, lowest is higher priority
			List5 = lists:sort(fun({_K1,_V1,{{P1,_T1},_Call1},_W1}, {_K2,_V2,{{P2,_T2},_Call2},_W2}) -> P1 =< P2 end, List4),
			% sort queues by queue weight, hignest first and return the result
			lists:sort(fun({_K1,_V1,{{_P1,_T1},_Call1},W1}, {_K2,_V2,{{_P2,_T2},_Call2},W2}) -> W1 >= W2 end, List5)
	catch
		exit:{noproc,_} ->
			global:register_name(?MODULE, whereis(?MODULE), {global, random_notify_name}),
			get_best_bindable_queues()
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

handle_call({add, Name, Recipe, Weight}, _From, State) ->
	case dict:is_key(Name, State) of
		true ->
			{ok, Pid} = dict:find(Name, State),
			{reply, {exists, Pid}, State};
		false ->
			Self = self(),
			case global:whereis_name(?MODULE) of
				Self ->
					{ok, Pid} = call_queue:start_link(Name, Recipe, Weight),
					{reply, {ok, Pid}, dict:store(Name, Pid, State)};
				undefined -> 
					global:register_name(?MODULE, self(), {global, random_notify_name}),
					{ok, Pid} = call_queue:start_link(Name, Recipe, Weight),
					{reply, {ok, Pid}, dict:store(Name, Pid, State)};
				_ ->
					try gen_server:call({global, ?MODULE}, {exists, Name}) of
						true ->
							Pid = gen_server:call({global, ?MODULE}, {get_queue, Name}),
							{reply, {exists, Pid}, State};
						false ->
							{ok, Pid} = call_queue:start_link(Name, Recipe, Weight),
							gen_server:call({global, ?MODULE}, {notify, Name, Pid}), % TODO - handle timeout exception
							{reply, {ok, Pid}, dict:store(Name, Pid, State)}
					catch
						exit:{timeout, _} ->
							global:register_name(?MODULE, self(), {global, random_notify_name}),
							{ok, Pid} = call_queue:start_link(Name, Recipe, Weight),
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
handle_call(queues_as_list, _From, State) ->
		{reply, dict:to_list(State), State};
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

single_node_test_() ->
	{
		foreach,
		fun() ->
			start(),
			ok
		end,
		fun(_) ->
			stop()
		end,
		[
			{
				"Add and query test", fun() ->
					?assertMatch({ok, _Pid}, add_queue(goober)),
					?assertMatch({exists, _Pid}, add_queue(goober)),
					?assertMatch(true, query_queue(goober)),
					?assertMatch(false, query_queue(foobar))
				end
			},{
				"Get test", fun() ->
					{ok, Pid} = add_queue(goober),
					?assertMatch(Pid, get_queue(goober)),
					?assertMatch(undefined, get_queue(no_exists))
				end
			}, {
				"best bindable queues by weight test", fun() ->
					start_link(),
					{ok, Pid} = add_queue(goober),
					{ok, Pid2} = add_queue(goober2, 10), % higher weighted queue
					{ok, _Pid3} = add_queue(goober3),
					?assertMatch([], get_best_bindable_queues()),
					?assertEqual(ok, call_queue:add(0, #call{id="Call1"}, Pid)),
					?assertMatch([{goober, Pid, {{0,_},#call{id="Call1"}}, ?DEFAULT_WEIGHT}], get_best_bindable_queues()),
					?assertEqual(ok, call_queue:add(10, #call{id="Call2"}, Pid2)),
					?assertMatch([
							{goober2, Pid2, {{10,_},#call{id="Call2"}}, 10},
							{goober, Pid, {{0,_},#call{id="Call1"}}, ?DEFAULT_WEIGHT}],
						get_best_bindable_queues()),
					?assertEqual(ok, call_queue:add(0, #call{id="Call3"}, Pid2)),
					?assertMatch([
							{goober2, Pid2, {{0,_},#call{id="Call3"}}, 10},
							{goober, Pid, {{0,_},#call{id="Call1"}}, ?DEFAULT_WEIGHT}],
						get_best_bindable_queues())
				end
			},{
				"best bindable queues by priority test", fun() ->
					start_link(),
					{ok, Pid} = add_queue(goober),
					{ok, Pid2} = add_queue(goober2),
					?assertMatch([], get_best_bindable_queues()),
					?assertEqual(ok, call_queue:add(10, #call{id="Call1"}, Pid)),
					?assertMatch([{goober, Pid, {{10,_},#call{id="Call1"}}, ?DEFAULT_WEIGHT}], get_best_bindable_queues()),
					?assertEqual(ok, call_queue:add(0, #call{id="Call2"}, Pid2)), % higher priority
					?assertMatch([
							{goober2, Pid2, {{0,_},#call{id="Call2"}}, ?DEFAULT_WEIGHT},
							{goober, Pid, {{10,_},#call{id="Call1"}}, ?DEFAULT_WEIGHT}],
						get_best_bindable_queues())
				end
			},{
				"best bindable queues by queuetime test", fun() ->
					start_link(),
					{ok, Pid2} = add_queue(goober2),
					{ok, Pid} = add_queue(goober),
					?assertMatch([], get_best_bindable_queues()),
					?assertEqual(ok, call_queue:add(0, #call{id="Call1"}, Pid)),
					?assertMatch([{goober, Pid, {{0,_},#call{id="Call1"}}, ?DEFAULT_WEIGHT}], get_best_bindable_queues()),
					?assertEqual(ok, call_queue:add(0, #call{id="Call2"}, Pid2)), % higher priority
					?assertMatch([
							{goober, Pid, {{0,_},#call{id="Call1"}}, ?DEFAULT_WEIGHT},
							{goober2, Pid2, {{0,_},#call{id="Call2"}}, ?DEFAULT_WEIGHT}],
						get_best_bindable_queues())
				end
			}
		]
	}.

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
			},{
				"Best bindable queues with failed master", fun() ->
					{ok, Pid} = rpc:call(Slave, queue_manager, add_queue, [queue2]),
					?assertEqual(ok, call_queue:add(0, #call{id="Call1"}, Pid)),
					slave:stop(Master),
					?assertMatch([{queue2, Pid, {_, #call{id="Call1"}}, ?DEFAULT_WEIGHT}], rpc:call(Slave, queue_manager, get_best_bindable_queues, []))
				end
			}
		]
	}.


-endif.
