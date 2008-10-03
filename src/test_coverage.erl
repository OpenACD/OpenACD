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
	cover:analyse_to_file(Module).
