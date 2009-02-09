
%% Macro to test generic gen_server behaviour. MYSERVERFUNC must be
%% defined as a fun which returns {pid(), fun()} or {atom(), fun()}.
%% The first element is the pid or registered name of the gen_server
%% and the second id the function to be run to stop the gen_server
%% after the test has run.

-ifdef(MYSERVERFUNC).
gen_server_test_() ->
	% start
	{Server, StopFunc} = ?MYSERVERFUNC(),

	case Server of
		Pid when is_pid(Pid) ->
			ok;
		Server ->
			Pid = whereis(Server),
			ok
	end,

	[{ "handle_call with garbage value",
		fun() -> ?assertEqual({unknown_call, garbage}, gen_server:call(Server, garbage)) end
	},

	{ "handle_cast with garbage value",
		fun() -> ?assertEqual(ok, gen_server:cast(Server, garbage)) end
	},


	{ "code_change",
		fun() ->
			?assertEqual(ok, sys:suspend(Pid)),
			?assertEqual(ok, sys:change_code(Pid, "", ?MODULE, "")),
			?assertEqual(ok, sys:resume(Pid))
		end
	},

	{ "handle info with garbage value",
		fun() -> Pid ! garbage end
	},

	{ "testing stop",
		fun() ->
			?assertMatch(ok, StopFunc()),
			?assertMatch(false, is_process_alive(Pid))
		end
	}].

-endif.
