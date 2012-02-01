%% @doc Interface for plugins or modules to provide or take action on 
%% some events.  When a hook is triggered, the list of callback functions
%% set for that hook are called in order of priority from least to greatest.
%% Processing stops at the first callback that returns `{ok, term()}'.  If
%% no callbacks return `{ok, term()}', `{error, unhandled}' is returned.
%% if a callback errors, it is removed from the list, never to be called
%% again.  Callbacks are run in the same process that triggered the hook.
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
-export([start_link/0, set_hook/3, set_hook/6, drop_hook/1, trigger_hooks/2]).

%% =================================================================
%% API
%% =================================================================

%% @doc Start up the process that manages the ets table.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

%% @doc Register a callback for a given hook with a priority of 100.
%% @see set_hook/6
set_hook(Id, Hook, {M, F, A}) ->
	set_hook(Id, Hook, M, F, A, 100).

%% @doc Register a callback for a given hook.  `Id' can be used to remove
%% the hook at a later time.  If a hook with `Id' already exists, it is
%% overwritten.  `M' is the module for the callback, `F' is the function
%% of that module, and `A' is a list of additional args appened to the
%% arguments passed in when the hook is triggered.  `Priority' determines
%% the order callbacks are called in.  The lower the priority, the earlier
%% the callback is called (as in 1st place, 2nd place, etc).
set_hook(Id, Hook, M, F, A, Priority) when is_list(A) ->
	ets:insert(cpx_hooks, {Id, Hook, M, F, A, Priority}).
	%gen_server:cast(?MODULE, {set_hook, Id, Hook, M, F, A, Priority}).

%% @doc Remove a callback with the given idea.
drop_hook(Id) ->
	ets:delete(cpx_hooks, Id).
	%gen_server:cast(?MODULE, {drop_hook, Id}).

%% @doc Trigger a hook, passing the the given args along.  Callbacks are
%% run in the calling process.  Generally a bad idea to trigger a hook
%% specified outside your application.
trigger_hooks(Hook, Args) ->
	Hooks = qlc:e(qlc:q([{P, M, F, A, Id} || 
		{Id, EHook, M, F, A, P} <- ets:table(cpx_hooks),
		EHook == Hook
	])),
	Hooks0 = lists:sort(Hooks),
	run_hooks(Hooks0, Args).

%% @doc Stop the hook server, thus removing the ets holding callback
%% information.
stop() ->
	gen_server:call(?MODULE, stop).

%% =================================================================
%% gen_server
%% =================================================================

%% -----------------------------------------------------------------
%% init
%% -----------------------------------------------------------------

%% @private
init(_) ->
	Ets = ets:new(cpx_hooks, [named_table, public]),
	{ok, Ets}.

%% -----------------------------------------------------------------
%% handle_call
%% -----------------------------------------------------------------

%% @private
handle_call(stop, _, Ets) ->
	{stop, normal, ok, Ets};

%% @private
handle_call(_, _, Ets) ->
	{reply, invalid, Ets}.


%% -----------------------------------------------------------------
%% handle_cast
%% -----------------------------------------------------------------

%% @private
handle_cast({set_hook, Id, Hook, M, F, A, Priority} = H, Ets) ->
	?DEBUG("Setting ~p", [H]),
	ets:insert(Ets, {Id, Hook, M, F, A, Priority}),
	{noreply, Ets};

%% @private
handle_cast({drop_hook, Id}, Ets) ->
	ets:delete(Ets, Id),
	{noreply, Ets}.


%% -----------------------------------------------------------------
%% handle_info
%% -----------------------------------------------------------------

%% @private
handle_info(_, Ets) ->
	{noreply, Ets}.


%% -----------------------------------------------------------------
%% terminate
%% -----------------------------------------------------------------

%% @private
terminate(_Reason, Ets) ->
	Ets.

%% -----------------------------------------------------------------
%% code_change
%% -----------------------------------------------------------------

%% @private
code_change(_OldVsn, Ets, _Extra) ->
	{ok, Ets}.

%% =================================================================
%% internal
%% =================================================================

%% @private
run_hooks([], _Args) ->
	{error, unhandled};

run_hooks([{_P, M, F, A, Id} | Tail], Args) ->
	Args0 = lists:append(Args, A),
	try apply(M, F, Args0) of
		{ok, Val} ->
			?DEBUG("hook ~p supplied value", [Id]),
			{ok, Val};
		Else ->
			?DEBUG("Hook ~p gave back a weird value:  ~p", [Id, Else]),
			run_hooks(Tail, Args)
	catch
		What:Why ->
			?NOTICE("Hook ~p failed with ~p:~p", [Id, What, Why]),
			drop_hook(Id),
			run_hooks(Tail, Args)
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
	fun(P) ->
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
		end

	]}.


-endif.
