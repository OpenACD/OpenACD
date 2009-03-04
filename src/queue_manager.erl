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

%% @doc Manages queues across nodes.

-module(queue_manager).

%% depends on call_queue

% TODO - ability to remove a queue? migrate a queue to another node?
%        both of these operations need to ensure that the leader is notified

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("queue.hrl").
-include("call.hrl").

-behaviour(gen_leader).

-export([
	start_link/1,
	start/1,
	queues/0,
	add_queue/1,
	add_queue/2,
	add_queue/3,
	get_queue/1,
	query_queue/1,
	stop/0,
	print/0,
	get_best_bindable_queues/0
	]).

% gen_leader callbacks
-export([init/1,
		elected/2,
		surrendered/3,
		handle_DOWN/3,
		handle_leader_call/4,
		handle_leader_cast/3,
		from_leader/3,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/4]).

%% API

%% @doc start the queue_manager linked to the parent process.
-spec(start_link/1 :: (Nodes :: [atom(),...]) -> {'ok', pid()}).
start_link(Nodes) ->
	call_queue_config:build_tables(Nodes),
	gen_leader:start_link(?MODULE, Nodes, [], ?MODULE, [], []).

%% @doc start the queue_manager unlinked to the parent process.
-spec(start/1 :: (Nodes :: [atom(),...]) -> {'ok', pid()}).
start(Nodes) ->
	call_queue_config:build_tables(Nodes),
	gen_leader:start(?MODULE, Nodes, [], ?MODULE, [], []).

% TODO tie add_queue to the call_queue_config
%% @doc Add a queue named `Name' using the default weight and recipe.
-spec(add_queue/1 :: (Name :: string()) -> {'ok', pid()} | {'exists', pid()}).
add_queue(Name) when is_list(Name) ->
	add_queue(Name, ?DEFAULT_RECIPE, ?DEFAULT_WEIGHT).

%% @doc Add a queue named `Name' using a givien `Recipe' or `Weight'.
-spec(add_queue/2 :: (Name :: string(), Recipe :: recipe()) -> {'ok', pid()} | {'exists', pid()};
	(Name :: string(), Weight :: pos_integer()) -> {'ok', pid()} | {'exists', pid()}).
add_queue(Name, Recipe) when is_list(Name), is_list(Recipe) ->
	add_queue(Name, Recipe, ?DEFAULT_WEIGHT);
add_queue(Name, Weight) when is_list(Name), is_integer(Weight), Weight > 0 ->
	add_queue(Name, ?DEFAULT_RECIPE, Weight).

%% @doc Add a queue named `Name' using the given `Recipe' and `Weight'.
-spec(add_queue/3 :: (Name :: string(), Recipe :: recipe(), Weight :: pos_integer()) -> {'ok', pid()} | {'exists', pid()}).
add_queue(Name, Recipe, Weight) when is_list(Name) ->
	case gen_leader:call(?MODULE, {exists, Name}) of
		true ->
			?CONSOLE("Queue exists locally", []),
			Pid = gen_leader:call(?MODULE, {get_queue, Name}),
			{exists, Pid};
		false ->
			?CONSOLE("Queue does not exist locally", []),
			case gen_leader:leader_call(?MODULE, {exists, Name}) of
				true ->
					Pid = gen_leader:leader_call(?MODULE, {get_queue, Name}),
					?CONSOLE("queue exists by leader decree at ~p", [Pid]),
					{exists, Pid};
				false ->
					?CONSOLE("Queue does not exist at all, starting it", []),
					{ok, Pid} = call_queue:start(Name, Recipe, Weight),
					ok = gen_leader:call(?MODULE, {notify, Name, Pid}),
					ok = gen_leader:leader_call(?MODULE, {notify, Name, Pid}),
					{ok, Pid}
			end
	end.

%% @doc Get the pid of the passed queue name.  If there is no queue, returns 'undefined'.
-spec(get_queue/1 :: (Name :: string()) -> pid() | undefined).
get_queue(Name) ->
	gen_leader:leader_call(?MODULE, {get_queue, Name}).

%% @doc 'true' or 'false' if the passed queue name exists.
-spec(query_queue/1 :: (Name :: string()) -> bool()).
query_queue(Name) ->
	case gen_leader:call(?MODULE, {exists, Name}) of
		true ->
			 true;
		 false ->
			gen_leader:leader_call(?MODULE, {exists, Name})
	end.

%% @doc Spits out the queues as {[Qname :: string(), Qpid :: pid()}].
-spec(queues/0 :: () -> [{string(), pid()}]).
queues() ->
	gen_leader:leader_call(?MODULE, queues_as_list).

%% @doc Sort queues containing a bindable call.  The queues are sorted from most important to least by weight,
%% priority of first bindable call, then the time the first bindable call has been in queue.
-spec(get_best_bindable_queues/0 :: () -> [{string(), pid(), {{non_neg_integer(), any()}, #queued_call{}}, pos_integer()}]).
get_best_bindable_queues() ->
	List = gen_leader:leader_call(?MODULE, queues_as_list),
	List1 = [{K, V, Call, W} || {K, V} <- List, Call <- [call_queue:ask(V)], Call =/= none, W <- [call_queue:get_weight(V) * call_queue:call_count(V)]],
	% sort queues by queuetime of first bindable call, longest first (lowest unix epoch time)
	List2 = lists:sort(fun({_K1, _V1,{{_P1, T1}, _Call1}, _W1}, {_K2, _V2,{{_P2, T2}, _Call2}, _W2}) -> T1 =< T2 end, List1),
	% sort queues by priority of first bindable call, lowest is higher priority
	List3 = lists:sort(fun({_K1, _V1,{{P1, _T1}, _Call1}, _W1}, {_K2, _V2,{{P2, _T2}, _Call2}, _W2}) -> P1 =< P2 end, List2),
	% sort queues by queue weight, highest first and return the result
	List4 = lists:sort(fun({_K1, _V1,{{_P1, _T1}, _Call1}, W1}, {_K2, _V2,{{_P2, _T2}, _Call2}, W2}) -> W1 >= W2 end, List3),
	Len = length(List4),
	% C is the index/counter
	util:list_map_with_index(fun(C, {K, V, Call, Weight}) -> {K, V, Call, Weight + Len - C} end, List4).

-spec(stop/0 :: () -> 'ok').
stop() ->
	gen_leader:call(?MODULE, stop).

%% @doc Returns the state.
-spec(print/0 :: () -> any()).
print() ->
	gen_leader:call(?MODULE, print).

% gen_leader stuff

%% @private
init([]) ->
	?CONSOLE("queue manager starting at ~p", [node()]),
	process_flag(trap_exit, true),
	% subscribe to mnesia system events to handle inconsistant db events
	% load the queues in the db and start them.
	Queues = call_queue_config:get_all(),
	F = fun(Queuerec, Acc) ->
		{ok, Pid} = call_queue:start_link(Queuerec#call_queue.name, Queuerec#call_queue.recipe, Queuerec#call_queue.weight),
		dict:store(Queuerec#call_queue.name, Pid, Acc)
	end,
	{ok, lists:foldr(F, dict:new(), Queues)}.

elected(State, _Election) ->
	?CONSOLE("elected",[]),
	mnesia:subscribe(system),
	{ok, State, State}.

surrendered(State, LeaderState, _Election) ->
	?CONSOLE("surrendered.",[]),
	mnesia:unsubscribe(system),
	% any queues the leader has that do not match the pid we have
	F = fun(Key, Value, {Mestate, Tokill}) ->
		case dict:find(Key, Mestate) of
			error ->
				{Mestate, Tokill};
			{ok, Value} ->
				{Mestate, Tokill};
			{ok, Otherpid} ->
				?CONSOLE("slated to die: ~p at ~p", [Key, Value]),
				{dict:erase(Key, Mestate), [Value | Tokill]}
		end
	end,
	{Noleader, Todie} = dict:fold(F, {State, []}, LeaderState),
	% inform the leader of any queues left over
	F2 = fun({Name, Pid}) ->
		gen_leader:leader_cast(?MODULE, {notify, Name, Pid})
	end,
	lists:foreach(F2, dict:to_list(Noleader)),
	Killem = fun(Pid) ->
		timer:exit_after(100, Pid, normal)
		%call_queue:stop(Pid)
	end,
	lists:foreach(Killem, Todie),
	?CONSOLE("Lead: ~p.  Self: ~p", [LeaderState, Noleader]),
	{ok, Noleader}.

%% @private
handle_DOWN(Node, State, _Election) ->
	?CONSOLE("in handle_DOWN",[]),
	mnesia:set_master_nodes(call_queue, [node()]),
	mnesia:set_master_nodes(skill_rec, [node()]),
	{ok, dict:filter(fun(K,V) -> ?CONSOLE("Trying to remove ~p", [K]), Node =/= node(V) end, State)}.

%% @private
handle_leader_call(queues_as_list, _From, State, _Election) ->
		{reply, dict:to_list(State), State};
handle_leader_call({notify, Name, Pid}, _From, State, _Election) ->
	?CONSOLE("Leading storing queue ~p named ~p", [Pid, Name]),
	{reply, ok, dict:store(Name, Pid, State)};
handle_leader_call({get_queue, Name}, _From, State, _Election) ->
	case dict:find(Name, State) of
		{ok, Pid} ->
			?CONSOLE("Found queue ~p", [Name]),
			{reply, Pid, State};
		error ->
			?CONSOLE("No such queue ~p", [Name]),
			{reply, undefined, State}
	end;
handle_leader_call({exists, Name}, _From, State, _Election) ->
	?CONSOLE("got an exists request",[]),
	{reply, dict:is_key(Name, State), State};
handle_leader_call(_Msg, _From, State, _Election) ->
	{reply, unknown, State}.


%% @private
handle_call({notify, Name, Pid}, _From, State) ->
	link(Pid),
	{reply, ok, dict:store(Name, Pid, State)};
handle_call({exists, Name}, _From, State) ->
	{reply, dict:is_key(Name, State), State};
handle_call({get_queue, Name}, _From, State) ->
	?CONSOLE("get_queue start...", []),
	case dict:find(Name, State) of
		{ok, Pid} ->
			{reply, Pid, State};
		error ->
			{reply, undefined, State}
	end;
%handle_call({notify, Name, Pid}, _From, State) ->
	%{reply, ok, dict:store(Name, Pid, State)};
handle_call(print, _From, State) ->
	{reply, State, State};
handle_call(queues_as_list, _From, State) ->
	{reply, dict:to_list(State), State};
handle_call(stop, _From, State) ->
	?CONSOLE("stop requested",[]),
	{stop, normal, ok, State};
handle_call(_Request, _From, State) ->
	{reply, unknown, State}.


%% @private
handle_leader_cast({notify, Name, Pid}, State, _Election) ->
	?CONSOLE("leader alerted about new queue ~p at ~p", [Name, Pid]),
	{noreply, dict:store(Name, Pid, State)};
handle_leader_cast({notify, Name}, State, _Election) ->
	?CONSOLE("leader alerted about dead queue ~p", [Name]),
	{noreply, dict:erase(Name, State)};
handle_leader_cast(_Msg, State, _Election) ->
	{noreply, State}.

%% @private
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @private
handle_info({mnesia_system_event, {inconsistent_database, _Context, _Node}}, State) ->
	mnesia:set_master_nodes(call_queue, [node()]),
	mnesia:set_master_nodes(skill_rec, [node()]),
	{noreply, State};
handle_info({mnesia_system_event, _MEvent}, State) ->
	{noreply, State};
handle_info({'EXIT', Pid, normal}, State) ->
	?CONSOLE("~p died normally", [Pid]),
	case find_queue_name(Pid, State) of
		none ->
			{noreply, State};
		Qname ->
			gen_leader:leader_cast(?MODULE, {notify, Qname}),
			NewState = dict:erase(Qname, State),
			{noreply, NewState}
	end;
handle_info({'EXIT', Pid, shutdown}, State) ->
	?CONSOLE("~p was shutdown.", [Pid]),
	case find_queue_name(Pid, State) of
		none ->
			{noreply, State};
		Qname ->
			gen_leader:leader_cast(?MODULE, {notify, Qname}),
			NewState = dict:erase(Qname, State),
			{noreply, NewState}
	end;
handle_info({'EXIT', Pid, Reason}, State) ->
	?CONSOLE("~p died due to ~p.", [Pid, Reason]),
	case find_queue_name(Pid, State) of
		none ->
			?CONSOLE("Cannot find queue", []),
			{noreply, State};
		Qname ->
			case call_queue_config:get_queue(Qname) of
				noexists ->
					?CONSOLE("queue not in the config database", []),
					gen_leader:leader_cast(?MODULE, {notify, Qname}),
					{noreply, State};
				Queuerec ->
					?CONSOLE("Got call_queue_config of ~p", [Queuerec]),
					{ok, NewQPid} = call_queue:start_link(Queuerec#call_queue.name, Queuerec#call_queue.recipe, Queuerec#call_queue.weight),
					NewState = dict:store(Queuerec#call_queue.name, NewQPid, State),
					ok = gen_leader:leader_cast(?MODULE, {notify, Qname, NewQPid}),
					{noreply, NewState}
			end
	end;
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
from_leader(_Msg, State, _Election) ->
	{ok, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Election, _Extra) ->
	{ok, State}.

find_queue_name(_NeedlePid, []) ->
	none;
find_queue_name(NeedlePid, [{Qname, NeedlePid} | _Tail]) ->
	Qname;
find_queue_name(NeedlePid, [{_Qname, _Otherpid} | Tail]) ->
	find_queue_name(NeedlePid, Tail);
find_queue_name(NeedlePid, Dict) ->
	find_queue_name(NeedlePid, dict:to_list(Dict)).

-ifdef('EUNIT').

get_nodes() ->
	[_Name, Host] = string:tokens(atom_to_list(node()), "@"),
	{list_to_atom(lists:append("master@", Host)), list_to_atom(lists:append("slave@", Host))}.

single_node_test_() ->
	["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
	{
		foreach,
		fun() ->
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			mnesia:create_schema([node()]),
			mnesia:start(),
			%build_tables(),
			{ok, _Pid} = start([node()]),
			ok
		end,
		fun(_) ->
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			stop()
		end,
		[
			{
				"Add and query test", fun() ->
					?assertMatch({ok, _Pid2}, add_queue("goober")),
					?assertMatch({exists, _Pid2}, add_queue("goober")),
					?assertMatch(true, query_queue("goober")),
					?assertMatch(false, query_queue("foobar"))
				end
			},{
				"Get test", fun() ->
					{ok, Pid} = add_queue("goober"),
					?assertMatch(Pid, get_queue("goober")),
					?assertMatch(undefined, get_queue("no_exists"))
				end
			}, {
				"best bindable queues by weight test", fun() ->
					{ok, Pid} = add_queue("goober"),
					{ok, Pid2} = add_queue("goober2", 10), % higher weighted queue
					{ok, _Pid3} = add_queue("goober3"),
					?assertMatch([], get_best_bindable_queues()),
					{ok, Dummy1} = dummy_media:start("Call1"),
					?assertEqual(ok, call_queue:add(Pid, 0, Dummy1)),
					?assertMatch([{"goober", Pid, {{0,_},#queued_call{id="Call1"}}, ?DEFAULT_WEIGHT+1}], get_best_bindable_queues()),
					{ok, Dummy2} = dummy_media:start("Call2"),
					?assertEqual(ok, call_queue:add(Pid2, 10, Dummy2)),
					?assertMatch([
							{"goober2", Pid2, {{10,_},#queued_call{id="Call2"}}, 12},
							{"goober", Pid, {{0,_},#queued_call{id="Call1"}}, ?DEFAULT_WEIGHT+1}],
						get_best_bindable_queues()),
					{ok, Dummy3} = dummy_media:start("Call3"),
					?assertEqual(ok, call_queue:add(Pid2, 0, Dummy3)),
					?assertMatch([
							{"goober2", Pid2, {{0,_},#queued_call{id="Call3"}}, 22},
							{"goober", Pid, {{0,_},#queued_call{id="Call1"}}, ?DEFAULT_WEIGHT+1}],
						get_best_bindable_queues())
				end
			},{
				"best bindable queues by priority test", fun() ->
					{ok, Pid} = add_queue("goober"),
					{ok, Pid2} = add_queue("goober2"),
					?assertMatch([], get_best_bindable_queues()),
					{ok, Dummy1} = dummy_media:start("Call1"),
					?assertEqual(ok, call_queue:add(Pid, 10, Dummy1)),
					?assertMatch([{"goober", Pid, {{10,_},#queued_call{id="Call1"}}, ?DEFAULT_WEIGHT+1}], get_best_bindable_queues()),
					{ok, Dummy2} = dummy_media:start("Call2"),
					?assertEqual(ok, call_queue:add(Pid2, 0, Dummy2)), % higher priority
					?assertMatch([
							{"goober2", Pid2, {{0,_},#queued_call{id="Call2"}}, ?DEFAULT_WEIGHT+2},
							{"goober", Pid, {{10,_},#queued_call{id="Call1"}}, ?DEFAULT_WEIGHT+1}],
						get_best_bindable_queues())
				end
			},{
				"best bindable queues by queuetime test", fun() ->
					{ok, Pid2} = add_queue("goober2"),
					{ok, Pid} = add_queue("goober"),
					?assertMatch([], get_best_bindable_queues()),
					{ok, Dummy1} = dummy_media:start("Call1"),
					?assertEqual(ok, call_queue:add(Pid, 0, Dummy1)),
					?assertMatch([{"goober", Pid, {{0,_},#queued_call{id="Call1"}}, ?DEFAULT_WEIGHT+1}], get_best_bindable_queues()),
					{ok, Dummy2} = dummy_media:start("Call2"),
					?assertEqual(ok, call_queue:add(Pid2, 0, Dummy2)),
					?assertMatch([
							{"goober", Pid, {{0,_},#queued_call{id="Call1"}}, ?DEFAULT_WEIGHT+2},
							{"goober2", Pid2, {{0,_},#queued_call{id="Call2"}}, ?DEFAULT_WEIGHT+1}],
						get_best_bindable_queues())
				end
			},{
				"Dead queue restarted",
				fun() ->
					{exists, QPid} = add_queue("default_queue"),
					exit(QPid, kill),
					receive
					after 300 -> ok
					end,
					AddQueueRes = add_queue("default_queue"),
					?assertMatch({exists, _NewPid}, AddQueueRes),
					?assertNot(QPid =:= element(2, AddQueueRes))
				end
			}
		]
	}.

multi_node_test_() ->
	["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
	{Master, Slave} = get_nodes(),
	{
		foreach,
		fun() ->
			slave:start(net_adm:localhost(), master, " -pa debug_ebin"),
			slave:start(net_adm:localhost(), slave, " -pa debug_ebin"),

			mnesia:change_config(extra_db_nodes, [Master, Slave]),
			mnesia:delete_schema([node(), Master, Slave]),
			mnesia:create_schema([node(), Master, Slave]),

			cover:start([Master, Slave]),

			rpc:call(Master, mnesia, start, []),
			rpc:call(Slave, mnesia, start, []),
			mnesia:start(),

			mnesia:change_table_copy_type(schema, Master, disc_copies),
			mnesia:change_table_copy_type(schema, Slave, disc_copies),

			{ok, _Pid} = rpc:call(Master, ?MODULE, start, [[Master, Slave]]),
			{ok, _Pid2} = rpc:call(Slave, ?MODULE, start, [[Master, Slave]]),
			{}
		end,
		fun({}) ->

			cover:stop([Master, Slave]),

			%rpc:call(Master, mnesia, stop, []),
			%rpc:call(Slave, mnesia, stop, []),
			%rpc:call(Master, mnesia, delete_schema, [[Master]]),
			%rpc:call(Slave, mnesia, delete_schema, [[Slave]]),

			slave:stop(Master),
			slave:stop(Slave),
			mnesia:stop(),
			mnesia:delete_schema([node()]),

			ok
		end,
		[
			{
				"Master Death", fun() ->
					%rpc:call(Master, erlang, disconnect_node, [Slave]),
					%cover:stop([Master]),
					rpc:call(Master, ?MODULE, stop, []),

					%?assertMatch(undefined, global:whereis_name(?MODULE)),
					?assertMatch({ok, _Pid}, rpc:call(Slave, ?MODULE, add_queue, ["queue1"])),
					?assertMatch(true, rpc:call(Slave, ?MODULE, query_queue, ["queue1"]))
				end

			},{
				"Slave Death", fun() ->
					%rpc:call(Maste, erlang, disconnect_node, [Slave]),
					%cover:stop([Master]),
					?assertMatch({ok, _Pid}, rpc:call(Slave, ?MODULE, add_queue, ["queue1"])),
					ok = rpc:call(Slave, ?MODULE, stop, []),

					%?assertMatch(undefined, global:whereis_name(?MODULE)),
					?assertMatch(false, rpc:call(Master, ?MODULE, query_queue, ["queue1"]))
				end

			},{
				"Net Split",fun() ->
					rpc:call(Master, ?MODULE, add_queue, ["queue1"]),
					rpc:call(Slave, ?MODULE, add_queue, ["queue2"]),

					?assertMatch(true, rpc:call(Slave, ?MODULE, query_queue, ["queue1"])),
					?assertMatch(true, rpc:call(Master, ?MODULE, query_queue, ["queue2"])),

					%rpc:call(Master, erlang, disconnect_node, [Slave]),
					rpc:call(Slave, erlang, disconnect_node, [Master]),

					%receive after 300 -> ok end,

					?debugFmt("Master queues ~p~n", [rpc:call(Master, ?MODULE, queues, [])]),
					?debugFmt("Slave queues ~p~n", [rpc:call(Slave, ?MODULE, queues, [])]),

					?assertMatch(true, rpc:call(Slave, ?MODULE, query_queue, ["queue2"])),
					?assertMatch(true, rpc:call(Slave, ?MODULE, query_queue, ["queue1"])),

					%?assertMatch(Newmaster, Master),
					?assertMatch(true, rpc:call(Master, ?MODULE, query_queue, ["queue1"])),
					?assertMatch(true, rpc:call(Master, ?MODULE, query_queue, ["queue2"])),
					?assertMatch({exists, _Pid}, rpc:call(Master, ?MODULE, add_queue, ["queue2"])),
					?assertMatch({exists, _Pid}, rpc:call(Master, ?MODULE, add_queue, ["queue1"]))
				end
			},{
				"Queues in sync", fun() ->
					rpc:call(Master, ?MODULE, add_queue, ["queue1"]),

					?assertMatch(true, rpc:call(Master, ?MODULE, query_queue, ["queue1"])),
					?assertMatch({exists, _Pid}, rpc:call(Slave, ?MODULE, add_queue, ["queue1"])),
					?assertMatch({ok, _Pid}, rpc:call(Slave, ?MODULE, add_queue, ["queue2"])),
					?assertMatch(true, rpc:call(Master, ?MODULE, query_queue, ["queue2"])),
					?assertMatch({exists, _Pid}, rpc:call(Master, ?MODULE, add_queue, ["queue2"])),

					?assertMatch(ok, rpc:call(Master, ?MODULE, stop, [])),
					?assertMatch(ok, rpc:call(Slave, ?MODULE, stop, []))
				end
			},{
				"No proc", fun() ->
					slave:stop(Master),
					?assertMatch(false, rpc:call(Slave, ?MODULE, query_queue, ["queue1"]))
				end
			},{
				"Best bindable queues with failed master", fun() ->
					{ok, Pid} = rpc:call(Slave, ?MODULE, add_queue, ["queue2"]),
					{ok, Dummy1} = rpc:call(Slave, dummy_media, start, ["Call1"]),
					?assertEqual(ok, call_queue:add(Pid, 0, Dummy1)),
					slave:stop(Master),
					?assertMatch([{"queue2", Pid, {_, #queued_call{id="Call1"}}, ?DEFAULT_WEIGHT+1}], rpc:call(Slave, ?MODULE, get_best_bindable_queues, []))
				end
			}, {
				"Leader is told about a call_queue that dies and did not come back", fun() ->
					{ok, QPid} = rpc:call(Slave, ?MODULE, add_queue, ["queue2"]),
					?assertMatch({exists, QPid}, rpc:call(Master, ?MODULE, add_queue, ["queue2"])),
					gen_server:call(QPid, {stop, test_kill}),
					receive
					after 100 ->
						ok
					end,
					NewQPid = rpc:call(Slave, ?MODULE, get_queue, ["queue2"]),
					?CONSOLE("the pids:  ~p and ~p", [QPid, NewQPid]),
					?assertNot(QPid =:= NewQPid),
					?assertEqual(undefined, rpc:call(Master, ?MODULE, get_queue, ["queue2"]))
				end
			}, {
				"Leader is told about a call_queue that died but is reborn", fun() ->
					QPid = rpc:call(Slave, queue_manager, get_queue, ["default_queue"]),
					?CONSOLE("qpid: ~p", [QPid]),
					gen_server:call(QPid, {stop, test_kill}),
					receive
					after 100 ->
						ok
					end,
					NewQPid = rpc:call(Slave, queue_manager, get_queue, ["default_queue"]),
					?CONSOLE("Das pids:  ~p and ~p", [QPid, NewQPid]),
					?assertNot(QPid =:= NewQPid),
					?assertNot(NewQPid =:= undefined)
				end
			}, {
				"A queue is only started (or stays started) on one node", fun() ->
					% because a queue_manager starts every queue in the database on init,
					% a queue will always exist locally.
					% this is not desired behavior, so on a surrender, it must ditch any
					% empty queues it already has, and notify the leader of the rest.
					MasterQ = rpc:call(Master, queue_manager, get_queue, ["default_queue"]),
					SlaveQ = rpc:call(Slave, queue_manager, get_queue, ["default_queue"]),
					?CONSOLE("dah qs:  ~p and ~p", [MasterQ, SlaveQ]),
					?assert(node(MasterQ) =:= node(SlaveQ)),
					?assert(MasterQ =:= SlaveQ)
				end
			}
		]
	}.

-endif.
