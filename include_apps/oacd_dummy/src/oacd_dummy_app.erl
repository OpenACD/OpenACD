-module(oacd_dummy_app).
-behavior(application).

-include_lib("OpenACD/include/log.hrl").

-export([start/2, prep_stop/1, stop/1]).

%% ==================================================
%% API
%% ==================================================

start(_Type, Args) ->
	?INFO("Starting dummy plugin:  ~p", [Args]),
	Args0 = merge_env(Args),
	oacd_dummy_sup:start_link(Args0).

prep_stop(State) ->
	State.

stop(_State) ->
	ok.

merge_env(Args) ->
	Env = application:get_all_env(oacd_dummy),
	merge_env(lists:sort(Args), lists:sort(Env), []).

merge_env([], Env, Acc) ->
	lists:append(Acc, Env);

merge_env(Args, [], Acc) ->
	lists:append(Acc, Args);

merge_env([{Key, Val} | ATail], [{Key, _} | ETail], Acc) ->
	merge_env(ATail, ETail, [{Key, Val} | Acc]);

merge_env([{Akey, Val} | ATail], [{EKey, _} | _] = Env, Acc) when Akey > EKey ->
	merge_env(ATail, Env, [{Akey, Val} | Acc]);

merge_env(Args, [Opt | ETail], Acc) ->
	merge_env(Args, ETail, [Opt | Acc]).
