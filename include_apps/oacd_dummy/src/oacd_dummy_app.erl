-module(oacd_dummy_app).
-behavior(application).

-include_lib("OpenACD/include/log.hrl").

-export([start/2, prep_stop/1, stop/1]).

%% ==================================================
%% API
%% ==================================================

start(_Type, Args) ->
	oacd_dummy_sup:start_link(Args).

prep_stop(State) ->
	State.

stop(_State) ->
	ok.
