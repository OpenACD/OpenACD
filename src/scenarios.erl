-module(scenarios).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-compile(export_all).

scenario1() ->
	queue_manager:add_queue(queue1, [], 5),
	queue_manager:add_queue(queue2, [], 1),
	{ok, Pid} = stub_media_manager:start(),
	stub_media_manager:create_and_queue_call(Pid, "Call1", voice, "Hello world", {client, 1, 1, "somebrand"}, [german], queue1, 0),
	stub_media_manager:create_and_queue_call(Pid, "Call2", voice, "Goodbye world", {client, 1, 1, "somebrand"}, [english], queue2, 0),
	Pid.

s1() -> 
	scenario1().
	
s2() -> 
	queue_manager:add_queue("L3-00170001", [], 1).
	
-ifdef(EUNIT).


-endif.
