-module(openacd).

-export([start/0]).

ensure_started(App) ->
	case application:start(App) of
		ok ->
			ok;
		{error, {already_started, _}} ->
			ok
	end.

start() ->
	lists:foreach(fun ensure_started/1, my_apps()).

my_apps() -> [
	kernel,
	stdlib,
	crypto,
	public_key,
	mnesia,
	syntax_tools,
	compiler,
	xmerl,
	ssl,
	inets,
	mochiweb,
	errd,
	gen_smtp,
	sasl,
	'OpenACD'
].

