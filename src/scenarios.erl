-module(scenarios).

-compile(export_all).

scenario1() ->
	queue_manager:add_queue(queue1, [], 5),
	queue_manager:add_queue(queue2, [], 0),
	{ok, Pid} = stub_media_manager:start(),
	stub_media_manager:create_and_queue_call(Pid, "Call1", voice, "Hello world", {client, 1, 1, "somebrand"}, [german], queue1, 0),
	stub_media_manager:create_and_queue_call(Pid, "Call2", voice, "Goodbye world", {client, 1, 1, "somebrand"}, [english], queue2, 0),
	Pid.
