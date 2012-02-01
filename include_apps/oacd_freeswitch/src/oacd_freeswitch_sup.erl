-module(oacd_freeswitch_sup).
-behavior(supervisor).

-export([init/1]).
-export([start_link/2]).

start_link(FsNode, Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, {FsNode, Args}).

init({FsNode, Args}) ->
	MFA = {freeswitch_media_manager, start_link, [FsNode, Args]},
	Kid = {freeswitch_media_manager, MFA, permanent, 1000, worker, [freeswitch_media_manager]},
	{ok, {{one_for_one, 5, 10}, [Kid]}}.
