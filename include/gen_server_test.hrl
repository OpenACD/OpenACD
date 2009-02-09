
%% Macro to test generic gen_server behaviour. MYSERVERFUNC must be
%% defined as a fun which returns {pid(), fun()} or {atom(), fun()}.
%% The first element is the pid or registered name of the gen_server
%% and the second id the function to be run to stop the gen_server
%% after the test has run.

-ifdef(MYSERVERFUNC).
find_pid() ->
	case whereis(?MODULE) of
		undefined ->
			case whereis(genservertestpid) of
				undefined -> exit(busted);
				Pid -> Pid
			end;
		Pid -> Pid
	end.

gen_server_test_() ->
	{setup,

	fun() ->
		{Server, StopFunc} = ?MYSERVERFUNC(),
		case Server of
			Pid when is_pid(Pid) ->
				?debugFmt("registering ~p as genservertestpid~n", [Pid]),
				register(genservertestpid, Pid),
				ok;
			Server ->
				Pid = whereis(Server),
				ok
		end,
		{Pid, StopFunc}
	end,

	fun({Pid, StopFunc}) ->
		?assertMatch(ok, StopFunc()),
		?assertMatch(false, is_process_alive(Pid))
	end,

	[{ "handle_call with garbage value",
		fun() ->
			Server = find_pid(),
			?assertEqual({unknown_call, garbage}, gen_server:call(Server, garbage))
		end
	},

	{ "handle_cast with garbage value",
		fun() ->
			Server = find_pid(),
			?assertEqual(ok, gen_server:cast(Server, garbage))
		end
	},


	{ "code_change",
		fun() ->
			Pid = find_pid(),
			?assertEqual(ok, sys:suspend(Pid)),
			?assertEqual(ok, sys:change_code(Pid, "", ?MODULE, "")),
			?assertEqual(ok, sys:resume(Pid))
		end
	},

	{ "handle info with garbage value",
		fun() ->
			Pid = find_pid(),
			Pid ! garbage
		end
	}

	]}.

-endif.
