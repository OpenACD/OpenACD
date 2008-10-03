-module(queue_manager).
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).

-export([start_link/0, add_queue/1, query_queue/1, stop/0, print/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
	%gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
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

dostuff(Name, State) ->
	try gen_server:call({global, ?MODULE}, {exists, Name}) of
		true ->
			{reply, exists, State};
		false ->
			gen_server:call({global, ?MODULE}, {notify, Name}), % TODO - handle timeout exception
			{reply, ok, dict:append(Name, {}, State)}
	catch
		_:_ ->
			global:register_name(?MODULE, self(), {global, random_notify_name}),
			{reply, timeout, dict:append(Name, {}, State)}
	end.

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
	Self = self(),
	case global:whereis_name(?MODULE) of
		undefined ->
			global:register_name(?MODULE, self(), {global, random_notify_name});
		Self ->
			Self;
		GID -> 
			%io:format("~p is at ~p on node ~p.~n", [?MODULE, GID, node(GID)])
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
					dostuff(Name, State)
			end
	end;
handle_call({exists, Name}, _From, State) ->
	{reply, dict:is_key(Name, State), State};
handle_call({notify, Name}, From, State) ->
	{reply, ok, dict:append(Name, From, State)};
handle_call(print, _From, State) ->
	{reply, State, State};
handle_call(stop, _From, State) ->
	{stop, getlost, please, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'EXIT', From, _Reason}, State) ->
	% filter out any queues on the dead node
	{noreply, dict:filter(fun(_K,[{}]) -> true; (_K,[{V,_}]) -> node(From) /= node(V) end, State)};
handle_info({global_name_conflict, _Name}, State) ->
	io:format("We lost the election~n"),
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

