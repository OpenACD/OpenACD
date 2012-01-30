-module(oacd_freeswitch_app).
-behavior(application).

-include_lib("OpenACD/include/log.hrl").

-export([start/2, prep_stop/1, stop/1]).

%% ==================================================
%% API
%% ==================================================

start(_Type, Args) ->
	DefaultFsNode = case application:get_env(oacd_freeswitch, freeswitch_node) of
		{ok, N} -> N;
		_ -> undefined
	end,
	Node = proplists:get_value(freeswitch_node, Args, DefaultFsNode),
	DefaultArgs = application:get_all_env(oacd_freeswitch),
	Args0 = merge_args(DefaultArgs, Args),
	oacd_freeswitch_sup:start_link(Args).

prep_stop(State) ->
	State.

stop(_State) ->
	ok.

merge_args(Defaults, Overrides) ->
	merge_args(lists:sort(Defaults), lists:sort(Overrides), []).

merge_args([], Overrides, Acc) ->
	lists:append(Overrides, Acc);

merge_args(Defaults, [], Acc) ->
	lists:append(Defaults, Acc);

merge_args([{Key, _} | DTail], [{Key, Val} | OTail], Acc) ->
	Acc0 = [{Key, Val} | Acc],
	merge_args(DTail, OTail, Acc0);

merge_args([{Key, Val} = H | DTail], [{Key1, _} | _] = Overrides, Acc) when Key > Key1 ->
	Acc0 = [H | Acc],
	merge_args(DTail, Overrides, Acc0);

merge_args(Defaults, [H | OTail], Acc) ->
	Acc0 = [H | Acc],
	merge_args(Defaults, OTail, Acc0).
