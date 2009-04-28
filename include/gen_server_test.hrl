%%	The contents of this file are subject to the Common Public Attribution
%%	License Version 1.0 (the “License”); you may not use this file except
%%	in compliance with the License. You may obtain a copy of the License at
%%	http://opensource.org/licenses/cpal_1.0. The License is based on the
%%	Mozilla Public License Version 1.1 but Sections 14 and 15 have been
%%	added to cover use of software over a computer network and provide for
%%	limited attribution for the Original Developer. In addition, Exhibit A
%%	has been modified to be consistent with Exhibit B.
%%
%%	Software distributed under the License is distributed on an “AS IS”
%%	basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%	License for the specific language governing rights and limitations
%%	under the License.
%%
%%	The Original Code is Spice Telephony.
%%
%%	The Initial Developers of the Original Code is 
%%	Andrew Thompson and Micah Warren.
%%
%%	All portions of the code written by the Initial Developers are Copyright
%%	(c) 2008-2009 SpiceCSM.
%%	All Rights Reserved.
%%
%%	Contributor(s):
%%
%%	Andrew Thompson <athompson at spicecsm dot com>
%%	Micah Warren <mwarren at spicecsm dot com>
%%

%% @hidden
%% Macro to test generic gen_server behaviour. MYSERVERFUNC must be
%% defined as a fun which returns {pid(), fun()} or {atom(), fun()}.
%% The first element is the pid or registered name of the gen_server
%% and the second id the function to be run to stop the gen_server
%% after the test has run.

-ifdef(MYSERVERFUNC).
gen_server_test_() ->
	{foreach,
	fun() ->
		{Server, StopFunc} = ?MYSERVERFUNC(),
		case Server of
			Pid when is_pid(Server) ->
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

	[
		fun({Pid, _StopFunc}) ->
			{ "handle_call with garbage value",
				fun() ->
					?assertEqual({unknown_call, garbage}, gen_server:call(Pid, garbage))
				end
			}
		end,
		fun({Pid, _StopFunc}) ->
			{ "handle_cast with garbage value",
				fun() ->
					?assertEqual(ok, gen_server:cast(Pid, garbage))
				end
			}
		end,
		fun({Pid, _StopFunc}) ->
			{ "code_change",
				fun() ->
					?assertEqual(ok, sys:suspend(Pid)),
					?assertEqual(ok, sys:change_code(Pid, "", ?MODULE, "")),
					?assertEqual(ok, sys:resume(Pid))
				end
			}
		end,
		fun({Pid, _StopFunc}) ->
			{ "handle info with garbage value",
				fun() ->
					Pid ! garbage
				end
			}
		end
	]}.

-endif.
