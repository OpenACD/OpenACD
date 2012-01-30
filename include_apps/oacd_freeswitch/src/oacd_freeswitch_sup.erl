-module(oacd_freeswitch_sup).
-behavior(supervisor).

-export([init/1]).

init(Args) ->
	Node = proplists:get_value(node, Args),
	MFA = {freeswitch_media_manager, start_link, [Node, Args]},
	Kid = {freeswitch_media_manager, MFA, permanent, 1000, worker, [freeswitch_media_manager]},
	{ok, {{one_for_one, 5, 10}, [Kid]}}.
