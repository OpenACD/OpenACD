-module(oacd_freeswitch_sup).
-behavior(supervisor).

-export([init/1]).
-export([start_link/2]).

start_link(FsNode, Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, {FsNode, Args}).

init({FsNode, Args}) ->
	MFA = {freeswitch_media_manager, start_link, [FsNode, Args]},
	Kid = {freeswitch_media_manager, MFA, permanent, 1000, worker, [freeswitch_media_manager]},

	SipAuth = case proplists:get_bool(sipauth,Args) of
		sipauth -> sip_auth;
		true -> sip_auth;
		_ -> no_sip_auth
	end,
	MFA2 = {freeswitch_fetch_handler, start_link, [FsNode,Args,SipAuth]},
	Kid2 = {freeswitch_fetch_handler, MFA2, permanent, 1000, worker, [freeswitch_fetch_handler]},

	{ok, {{one_for_one, 5, 10}, [Kid,Kid2]}}.
