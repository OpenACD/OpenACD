-module(oacd_dummy_sup).
-behavior(supervisor).

-export([init/1]).
-export([start_link/1]).

start_link(Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
	MFA = {dummy_media_manager, start_link, [Args]},
	Kid = {dummy_media_manager, MFA, permanent, 1000, worker, [dummy_media_manager]},
	{ok, {{one_for_one, 5, 10}, [Kid]}}.
