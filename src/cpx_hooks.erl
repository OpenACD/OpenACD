-module(cpx_hooks).
-behavior(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include_lib("stdlib/include/qlc.hrl").

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).
% api
-export([start_link/0, set_hook/3, set_hook/6, drop_hook/1, trigger_hooks/2,
	trigger_hooks/3]).

%% =================================================================
%% API
%% =================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

set_hook(Id, Hook, {M, F, A}) ->
	set_hook(Id, Hook, M, F, A, 100).

set_hook(Id, Hook, M, F, A, Priority) when is_list(A) ->
	ets:insert(cpx_hooks, {Id, Hook, M, F, A, Priority}).
	%gen_server:cast(?MODULE, {set_hook, Id, Hook, M, F, A, Priority}).

drop_hook(Id) ->
	ets:delete(cpx_hooks, Id).
	%gen_server:cast(?MODULE, {drop_hook, Id}).

trigger_hooks(Hook, Args) ->
	trigger_hooks(Hook, Args, first).

trigger_hooks(Hook, Args, StopWhen) ->
	Hooks = qlc:e(qlc:q([{P, M, F, A, Id} || 
		{Id, EHook, M, F, A, P} <- ets:table(cpx_hooks),
		EHook == Hook
	])),
	Hooks0 = lists:sort(Hooks),
	case StopWhen of
		first -> run_hooks(Hooks0, Args, first);
		all -> run_hooks(Hooks0, Args, [])
	end.

stop() ->
	gen_server:call(?MODULE, stop).

%% =================================================================
%% gen_server
%% =================================================================

%% -----------------------------------------------------------------
%% init
%% -----------------------------------------------------------------

init(_) ->
	Ets = ets:new(cpx_hooks, [named_table, public]),
	{ok, Ets}.

%% -----------------------------------------------------------------
%% handle_call
%% -----------------------------------------------------------------

handle_call(stop, _, Ets) ->
	{stop, normal, ok, Ets};

handle_call(_, _, Ets) ->
	{reply, invalid, Ets}.


%% -----------------------------------------------------------------
%% handle_cast
%% -----------------------------------------------------------------

handle_cast({set_hook, Id, Hook, M, F, A, Priority} = H, Ets) ->
	?DEBUG("Setting ~p", [H]),
	ets:insert(Ets, {Id, Hook, M, F, A, Priority}),
	{noreply, Ets};

handle_cast({drop_hook, Id}, Ets) ->
	ets:delete(Ets, Id),
	{noreply, Ets}.


%% -----------------------------------------------------------------
%% handle_info
%% -----------------------------------------------------------------

handle_info(_, Ets) ->
	{noreply, Ets}.


%% -----------------------------------------------------------------
%% terminate
%% -----------------------------------------------------------------

terminate(_Reason, Ets) ->
	Ets.

%% -----------------------------------------------------------------
%% code_change
%% -----------------------------------------------------------------

code_change(_OldVsn, Ets, _Extra) ->
	{ok, Ets}.

%% =================================================================
%% internal
%% =================================================================

run_hooks([], _Args, first) ->
	{error, unhandled};

run_hooks([], _Args, Out) ->
	{ok, Out};

run_hooks([{_P, M, F, A, Id} | Tail], Args, StopWhen) ->
	Args0 = lists:append(Args, A),
	try {apply(M, F, Args0), StopWhen} of
		{{ok, Val}, first} ->
			?DEBUG("hook ~p supplied value", [Id]),
			{ok, Val};
		{{ok, Val}, Acc} ->
			run_hooks(Tail, Args, [Val | Acc]);
		{Else, _} ->
			?DEBUG("Hook ~p gave back a weird value:  ~p", [Id, Else]),
			run_hooks(Tail, Args, StopWhen)
	catch
		What:Why ->
			?NOTICE("Hook ~p failed with ~p:~p", [Id, What, Why]),
			drop_hook(Id),
			run_hooks(Tail, Args, StopWhen)
	end.


%% =================================================================
%% TEST
%% =================================================================

-ifdef(TEST).
-compile([export_all]).

hook_test_() ->
	{foreach, fun() ->
		meck:new(hook_tester),
		{ok, P} = start_link(),
		P
	end,
	fun(_P) ->
		meck:unload(hook_tester),
		stop(),
		timer:sleep(100)
	end, [
		fun(_P) ->
			{"no hooks", fun() ->
				Out = trigger_hooks(hook, []),
				?assertEqual({error, unhandled}, Out)
			end}
		end,

		fun(_) ->
			{"Hook stops another from happening", fun() ->
					meck:expect(hook_tester, good_skip, fun() ->
						ok
					end),
					meck:expect(hook_tester, good_return, fun() ->
						{ok, ok}
					end),
					meck:expect(hook_tester, donotwant, fun() ->
						erlang:error(donotwant_hit)
					end),
					set_hook(skipped, hook, hook_tester, good_skip, [], 1),
					set_hook(good, hook, hook_tester, good_return, [], 2),
					set_hook(error, hook, hook_tester, donotwant, [], 3),
					%timer:sleep(10),
					Out = trigger_hooks(hook, []),
					?assertEqual({ok, ok}, Out)
			end}
		end,

		fun(_) ->
			{"Bad hook removed", fun() ->
				meck:expect(hook_tester, donotwant, fun() ->
					erlang:error(donotwant_hit)
				end),
				set_hook(1, hook, {hook_tester, donotwant_hit, []}),
				trigger_hooks(hook, []),
				Out = qlc:e(qlc:q([X || X <- ets:table(cpx_hooks)])),
				?assertEqual([], Out)
			end}
		end,

		fun(_) ->
			{"hook fold works", fun() ->
				meck:expect(hook_tester, callback, fun(Action) ->
					case Action of
						$i -> {ok, $i};
						$h -> {ok, $h};
						{error, Err} -> erlang:error(Err);
						Else -> Else
					end
				end),
				set_hook(first, hook, hook_tester, callback, [$i], 1),
				set_hook(second, hook, hook_tester, callback, [{error, donotwant}], 2),
				set_hook(third, hook, hook_tester, callback, [jelly], 3),
				set_hook(forth, hook, hook_tester, callback, [$h], 4),
				Out = trigger_hooks(hook, [], all),
				?assertEqual({ok, "hi"}, Out)
			end}
		end

	]}.


-endif.
