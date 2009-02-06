
-ifdef(MYSERVERFUNC).
gen_server_test() ->
	{Server, StopFunc} = ?MYSERVERFUNC(),
	?assertEqual({unknown_call, garbage}, gen_server:call(Server, garbage)),
	?assertEqual(ok, gen_server:cast(Server, garbage)),
	% execute the code_change handler
	case Server of
		Pid when is_pid(Pid) ->
			sys:suspend(Pid),
			?assertEqual(ok, sys:change_code(Pid, "", ?MODULE, "")),
			sys:resume(Pid),
			Pid ! garbage;
		Server ->
			Pid = whereis(Server),
			sys:suspend(Pid),
			?assertEqual(ok, sys:change_code(Pid, "", ?MODULE, "")),
			sys:resume(Pid),
			Pid ! garbage
	end,
	StopFunc().

-endif.
