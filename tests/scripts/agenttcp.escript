#! /usr/bin/env escript
-mode(compile).

% Connect to an OpenACD instance and run through a series of interactions.

-record(state, {
	host = "localhost",
	port = 7331,
	username = "agent",
	password = "Password123",
	connect_mod = tcp,
	protocol = tcp,
	tests = ["login"]
}).

main(["help"]) ->
	io:format("Tests that can be run:
	login
		-username string() agent
		-password string() Password123
		-host string() localhost
		-port pos_integer() 7331
		-protocol tcp | ssl | ssl_upgrade tcp
");

main(Args) ->
	load_libraries(),
	State = state_from_args(Args),
	run_tests(State).

load_libraries() ->
	RawPath = escript:script_name(),
	Path = filename:dirname(RawPath),
	Appensions = ["../../", "../../deps/"],
	load_libraries(Appensions, Path).

load_libraries([], _Base) ->
	ok;

load_libraries([Appension | Tail], Base) ->
	{ok, Files} = file:list_dir(Base ++ "/" ++ Appension),
	Files0 = [Base ++ "/" ++ Appension ++ "/" ++ F || F <- Files],
	load_ebins(Files0),
	load_libraries(Tail, Base).

load_ebins([]) ->
	ok;

load_ebins([MaybeDir | Tail]) ->
	case filelib:is_dir(MaybeDir) of
		true ->
			case filelib:is_dir(MaybeDir ++ "/ebin") of
				true ->
					code:add_pathz(MaybeDir ++ "/ebin");
				_ ->
					ok
			end;
		_ ->
			ok
	end,
	load_ebins(Tail).

state_from_args(Args) ->
	state_from_args(Args, #state{}).

state_from_args([], State) ->
	Tests = lists:reverse(State#state.tests),
	State#state{tests = Tests};

state_from_args(["-username", Username | Tail], State) ->
	state_from_args(Tail, State#state{username = Username});

state_from_args(["-password", Password | Tail], State) ->
	state_from_args(Tail, State#state{password = Password});

state_from_args(["-host", Host | Tail], State) ->
	state_from_args(Tail, State#state{host = Host});

state_from_args(["-port", Port | Tail], State) ->
	state_from_args(Tail, State#state{port = list_to_integer(Port)});

state_from_args(["-protocol", Proto | Tail], State) ->
	Mod = case Proto of
		"tcp" -> gen_tcp;
		"ssl_upgrade" -> ssl_upgrade;
		"ssl" -> ssl
	end,
	state_from_args(Tail, State#state{protocol = Mod});

state_from_args(["-test", Test | Tail], State) ->
	Tests = [Test | State#state.tests],
	state_from_args(Tail, State#state{tests = Tests}).

run_tests(#state{tests = Tests} = State) ->
	run_tests(Tests, State).

run_tests([], State) ->
	ok;

run_tests(["login" | Tail], State) ->
	ok = test_login(State),
	run_tests(Tail, State).

test_login(State) ->
	io:format("***********************\n* Starting login test *\n***********************\n"),
	#state{host = Host, port = Port, protocol = Proto, username = Username,
		password = Password} = State,
	io:format("~s://~s:~s@~s:~p/\n", [Proto, Username, Password, Host, Port]),
	{ok, Agent} = cpx_agent_tcp_client:start([{host, Host}, {port, Port},
		{protocol, Proto}]),
	{ok, _} = cpx_agent_tcp_client:check_version(Agent),
	{ok, _} = cpx_agent_tcp_client:login(State#state.username, State#state.password).
