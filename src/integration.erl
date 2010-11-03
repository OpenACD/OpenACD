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
%%	The Original Code is OpenACD.
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
%%	Andrew Thompson <andrew at hijacked dot us>
%%	Micah Warren <micahw at lordnull dot com>
%%

%% @doc Helper module to facilitate speaking to an integration gen_server.  The
%% integration server is assumed to have already been started with the name of
%% `integration'.  If it has not, the functions will return 
%% `{error, nointegration}'.
%%
%% This tries to enforce valid returns from the integration server.  If it
%% detects an invalid reply, it will throw an exection with a tuple; the first
%% element being `badreturn'.

-module(integration).
-author(micahw).

-include("log.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
	agent_exists/1,
	agent_auth/3,
	client_exists/1,
	client_exists/2,
	get_client/2
]).

%% @doc Returns `true' if an agent with the loging `Agent' exists, `false'
%% otherwise.
-spec(agent_exists/1 :: (Agent :: string()) -> boolean()).
agent_exists(Agent) ->
	Out = do_call({agent_exists, Agent}),
	Test = fun(In) ->
		is_boolean(In)
	end,
	check_return(Out, Test).

%% @doc Attempts to authenticate `Agent' with plaintext password `Password'.
-type(profile() :: string()).
-type(skill() :: atom() | {atom(), any()}).
-type(skill_list() :: [skill()]).
-type(profile_data() :: {profile(), skill_list()} | profile() | skill_list()).
-type(security() :: 'admin' | 'agent' | 'supervisor').
-spec(agent_auth/3 :: (Agent :: string(), Password :: string(), Extended :: [{atom(), any()}]) -> {ok, string(), profile_data(), security(), [{atom(),any()}]} | 'deny' | 'destroy' | {'error', 'nointegration'}).
agent_auth(Agent, Password, Extended) ->
	Out = do_call({agent_auth, Agent, Password, Extended}),
	Test = fun
		({ok, _Id, _Profile, Security, _Extended}) ->
			case Security of
				Lvl when Lvl =:= admin; Lvl =:= agent; Lvl =:= supervisor ->
					true;
				_Else ->
					false
			end;
		(deny) ->
			true;
		(destroy) ->
			true;
		(_Else) ->
			false
	end,
	check_return(Out, Test).

%% @doc Same as `client_exists(label, Label)'.
-spec(client_exists/1 :: (Label :: string()) -> boolean()).
client_exists(Label) ->
	client_exists(label, Label).

%% @doc `true' or `false' based on the existance of a client who's `Key' is `Value'.
-type(client_key() :: 'label' | 'id').
-spec(client_exists/2 :: (Key :: client_key(), Value :: string()) -> boolean()).
client_exists(Key, Value) ->
	Out = do_call({client_exists, Key, Value}),
	Test = fun(In) ->
		is_boolean(In)
	end,
	check_return(Out, Test).

%% @doc Retrieve a client who's unique Attribute (label or comboid) is Value.
-type(label() :: string()).
-type(client_id() :: string()).
-type(url_option() :: {url_pop, string()}).
-type(client_option() :: url_option()).
-type(client_options() :: [client_option()]).
-spec(get_client/2 :: (Key :: client_key(), Value :: string()) -> 'none' | {ok, client_id(), label(), client_options()} | {'error', 'nointegration'}).
get_client(Key, Value) ->
	Out = do_call({get_client, Key, Value}),
	Test = fun
		(none) ->
			true;
		({ok, _Id, _Label, _Options}) ->
			true;
		(_Else) ->
			false
	end,
	check_return(Out, Test).

do_call(Message) ->
	try gen_server:call(integration, Message, 4000) of
		Reply ->
			Reply
	catch
		exit:{noproc, {gen_server, call, _Requestargs}} ->
			{error, nointegration};
		exit:{timeout, {gen_server, call, _Requestargs}} ->
			timeout
	end.

check_return({error, nointegration} = O, _Test) ->
	O;
check_return(Out, Test) when is_function(Test)->
	case Test(Out) of
		true ->
			Out;
		false ->
			throw({badreturn, Out})
	end.

-ifdef(TEST).

no_integration_test_() ->
	[?_assertEqual({error, nointegration}, do_call("a message")),
	?_assertEqual({error, nointegration}, get_client(label, "a_client")),
	?_assertEqual({error, nointegration}, client_exists(comboid, "00010001")),
	?_assertEqual({error, nointegration}, agent_auth("agent", "password", [])),
	?_assertEqual({error, nointegration}, agent_exists("agent"))].

bad_integration_test_() ->
	{foreach,
	fun() ->
		{ok, Mock} = gen_server_mock:named({local, integration}),
		gen_server_mock:expect_call(Mock, fun(_Whatever, _From, State) -> {ok, gooberpants, State} end),
		Mock
	end,
	fun(Mock) ->
		unregister(integration),
		gen_server_mock:stop(Mock)
	end,
	[?_assertThrow({badreturn, gooberpants}, get_client(label, "a_client")),
	?_assertThrow({badreturn, gooberpants}, client_exists(label, "a_client")),
	?_assertThrow({badreturn, gooberpants}, agent_auth("agent", "password", [])),
	?_assertThrow({badreturn, gooberpants}, agent_exists("agent")),
	fun(Mock) ->
		{"integration hits timeout",
		fun() ->
			gen_server:call(Mock, "clearing useless expect"),
			gen_server_mock:expect_call(Mock, fun(_Whatever, _From, State) ->
				timer:sleep(4500),
				{ok, gooberpants, State}
			end),
			?assertThrow({badreturn, timeout}, client_exists(label, "a_client"))
		end}
	end]}.

good_integration_test_() ->
	{foreach,
	fun() ->
		{ok, Mock} = gen_server_mock:named({local, integration}),
		Mock
	end,
	fun(Mock) ->
		unregister(integration),
		gen_server_mock:stop(Mock)
	end,
	[fun(Mock) ->
		{"agent exists",
		fun() ->
			gen_server_mock:expect_call(Mock, fun({agent_exists, "agent"}, _, State) -> {ok, true, State} end),
			?assertEqual(true, agent_exists("agent"))
		end}
	end,
	fun(Mock) ->
		{"agent doesn't exist",
		fun() ->
			gen_server_mock:expect_call(Mock, fun({agent_exists, "agent"}, _, State) -> {ok, false, State} end),
			?assertEqual(false, agent_exists("agent"))
		end}
	end,
	fun(Mock) ->
		{"agent auth success",
		fun() ->
			gen_server_mock:expect_call(Mock, fun({agent_auth, "agent", "password", []}, _, State) -> {ok, {ok, "testid", "Default", agent, []}, State} end),
			?assertEqual({ok, "testid", "Default", agent, []}, agent_auth("agent", "password", []))
		end}
	end,
	fun(Mock) ->
		{"agent auth fail",
		fun() ->
			gen_server_mock:expect_call(Mock, fun({agent_auth, "agent", "password", []}, _, State) -> {ok, deny, State} end),
			?assertEqual(deny, agent_auth("agent", "password", []))
		end}
	end,
	fun(Mock) ->
		{"client exists",
		fun() ->
			gen_server_mock:expect_call(Mock, fun({client_exists, label, "client"}, _, State) -> {ok, true, State} end),
			?assertEqual(true, client_exists(label, "client"))
		end}
	end,
	fun(Mock) ->
		{"client doesn't exist",
		fun() ->
			gen_server_mock:expect_call(Mock, fun({client_exists, label, "client"}, _, State) -> {ok, false, State} end),
			?assertEqual(false, client_exists(label, "client"))
		end}
	end,
	fun(Mock) ->
		{"can get client",
		fun() ->
			gen_server_mock:expect_call(Mock, fun({get_client, label, "client"}, _, State) -> {ok, {ok, "00010001", "client", []}, State} end),
			?assertEqual({ok, "00010001", "client", []}, get_client(label, "client"))
		end}
	end,
	fun(Mock) ->
		{"can't get the client",
		fun() ->
			gen_server_mock:expect_call(Mock, fun({get_client, label, "client"}, _, State) -> {ok, none, State} end),
			?assertEqual(none, get_client(label, "client"))
		end}
	end]}.	
		
-endif.
