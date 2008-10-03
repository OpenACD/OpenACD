-module(test_coverage).
-export([start/1]).

start(?MODULE) -> 
	ok;
start([Module|T]) ->
	start(Module),
	start(T);
start([]) ->
	ok;
start(Module) ->
	cover:start(),
	cover:compile_beam(string:concat("ebin/", atom_to_list(Module))),
	apply(Module, test, []),
	cover:analyse_to_file(Module, string:concat(string:concat("coverage/", atom_to_list(Module)), ".txt")),
	cover:analyse_to_file(Module, string:concat(string:concat("coverage/", atom_to_list(Module)), ".html"), [html]).
