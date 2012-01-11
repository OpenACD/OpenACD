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
%%
%% A module attempting to act as an integration point MUST respond properly
%% to handle_call, otherwise it might as well not event run.
%% <table border="1">
%%	<tr>
%% 		<th>Message</th>
%%		<th>Reply</th>
%%	</tr>
%% 	<tr>
%%		<td>`{agent_exists, LoginName :: string()}'</td>
%%		<td>`true | false'</td>
%%	</tr>
%%	<tr>
%%		<td>`{agent_auth, LoginName :: string(), PlainPassword :: string(), Extended :: [any()]}'</td>
%%		<td>`deny | destroy | {ok, Id :: string(), Profile :: string(), agent | admin | supervisor, Extended :: [any()]}'</td>
%%	</tr>
%%	<tr>
%% 		<td>`{client_exists, id, Id :: string()}'</td>
%%		<td>`true | false'</td>
%%	</tr>
%%	<tr>
%%		<td>`{client_exists, label, Label :: string()}'</td>
%%		<td>`true | false'</td>
%%	</tr>
%%	<tr>
%%		<td>`{get_client, id, Id :: string()}'</td>
%%		<td>`none | {ok, Id :: string(), Label :: string(), Options :: [any()]}'</td>
%%	</tr>
%%	<tr>
%%		<td>`{get_client, label, Label :: string()}'</td>
%%		<td>`none | {ok, Id :: string(), Label :: string(), Options :: [any()]}'</td>
%%	</tr>
%%	<tr>
%%		<td>`{get_queue, Queue :: string()}'</td>
%%		<td>`none | {ok, Name :: string(), Weight :: non_neg_integer(), Skills :: [skill()], HoldMusic :: string() | undefined, Group :: string()}'</td>
%%	</tr>
%%	<tr>
%%		<td>`{get_queue_group, QueueGroup :: string()}'</td>
%%		<td>`none | {ok, Name :: string(), Recipe :: recipe(), Skills :: [skill()], Sort :: non_neg_integer(), Protected :: boolean()}'</td>
%%	</tr>
%%	<tr>
%%		<td>`{get_skill, Skill :: atom()}'</td>
%%		<td>`none | {ok, SkillAtom :: atom(), Name :: string(), Protected :: boolean(), Desc :: string(), Group :: string()}'</td>
%%	</tr>
%%	<tr>
%%		<td>`{get_profile, Name :: string()}'</td>
%%		<td>`none | {ok, Name :: string(), Id :: string(), Order :: non_neg_integer(), Skills :: [skill()]}'</td>
%%	</tr>
%% </table>

-module(integration).
-author(micahw).

-include("log.hrl").
-include("queue.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
	agent_exists/1,
	agent_auth/3,
	client_exists/1,
	client_exists/2,
	get_client/2,
	get_queue/1,
	get_queue_group/1,
	get_skill/1,
	get_profile/1
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

%% @doc Retrieve a queue by name
-spec(get_queue/1 :: (Name :: string()) -> 'none' | {ok, string(), non_neg_integer(), [atom()], recipe() | undefined, string()} | {error, nointegration}).
get_queue(Queue) ->
	Out = do_call({get_queue, Queue}),
	Test = fun
		(none) ->
			true;
		({ok, _Name, _Weight, _Skills, _Recipe, _HoldMusic, _Group}) ->
			true;
		(_Else) ->
			false
	end,
	check_return(Out, Test).

%% @doc Retrieve a queue by name
-spec(get_queue_group/1 :: (Name :: string()) -> 'none' | {ok, string(), recipe(), [atom()], boolean()} | {error, nointegration}).
get_queue_group(QueueGroup) ->
	Out = do_call({get_queue_group, QueueGroup}),
	Test = fun
		(none) ->
			true;
		({ok, _Name, _Recipe, _Skills, _Sort, _Protected}) ->
			true;
		(_Else) ->
			false
	end,
	check_return(Out, Test).

%% @doc Retrieve a skill
-spec(get_skill/1 :: (Atom :: atom()) -> 'none' | {ok, atom(), string(), boolean(), string(), string()}).
get_skill(Skill) ->
	Out = do_call({get_skill, Skill}),
	Test = fun
		(none) ->
			true;
		({ok, _Skillatom, _Skillname, _Skillprotected, _Skilldesc, _Skillgroup}) ->
			true;
		(_Else) ->
			false
	end,
	check_return(Out, Test).

%% @doc Retrieve a profile (agent group)
get_profile(Profile) ->
	Out = do_call({get_profile, Profile}),
	Test = fun
		(none) ->
			true;
		({ok, _Name, _Id, _Order, _Options, _Skills}) ->
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
	?_assertEqual({error, nointegration}, agent_exists("agent")),
	?_assertEqual({error, nointegration}, get_queue("queue")),
	?_assertEqual({error, nointegration}, get_queue_group("queuegroup")),
	?_assertEqual({error, nointegration}, get_skill("skill"))].

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
	?_assertThrow({badreturn, gooberpants}, get_queue("queue")),
	?_assertThrow({badreturn, gooberpants}, get_queue_group("queuegroup")),
	?_assertThrow({badreturn, gooberpants}, get_skill("skill")),
	?_assertThrow({badreturn, gooberpants}, get_profile("profile")),
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
	end,
	fun(Mock) ->
		{"can get queue",
		fun() ->
			gen_server_mock:expect_call(Mock, fun({get_queue, "queue"}, _, State) -> {ok, {ok, "name", 10, [], [], undefined, "group"}, State} end),
			?assertEqual({ok, "name", 10, [], [], undefined, "group"}, get_queue("queue"))
		end}
	end,
	fun(Mock) ->
		{"can't get queue",
		fun() ->
			gen_server_mock:expect_call(Mock, fun({get_queue, "queue"}, _, State) -> {ok, none, State} end),
			?assertEqual(none, get_queue("queue"))
		end}
	end,
	fun(Mock) ->
		{"can get queue group",
		fun() ->
			gen_server_mock:expect_call(Mock, fun({get_queue_group, "queuegroup"}, _, State) -> {ok, {ok, "queuegroup", [], [], 10, true}, State} end),
			?assertEqual({ok, "queuegroup", [], [], 10, true}, get_queue_group("queuegroup"))
		end}
	end,
	fun(Mock) ->
		{"can't get queue group",
		fun() ->
			gen_server_mock:expect_call(Mock, fun({get_queue_group, "queuegroup"}, _, State) -> {ok, none, State} end),
			?assertEqual(none, get_queue_group("queuegroup"))
		end}
	end,
	fun(Mock) ->
		{"can get skill",
		fun() ->
			gen_server_mock:expect_call(Mock, fun({get_skill, skill}, _, State) -> {ok, {ok, skill, "skill", true, "desc", "misc"}, State} end),
			?assertEqual({ok, skill, "skill", true, "desc", "misc"}, get_skill(skill))
		end}
	end,
	fun(Mock) ->
		{"can't get skill",
		fun() ->
			gen_server_mock:expect_call(Mock, fun({get_skill, skill}, _, State) -> {ok, none, State} end),
			?assertEqual(none, get_skill(skill))
		end}
	end,
	fun(Mock) ->
		{"can get profile",
		fun() ->
			gen_server_mock:expect_call(Mock, fun({get_profile, "profile"}, _, State) -> {ok, {ok, "profile", "id", 10, [{option, a}], [skill]}, State} end),
			?assertEqual({ok, "profile", "id", 10, [{option, a}], [skill]}, get_profile("profile"))
		end}
	end,
	fun(Mock) ->
		{"can't get profile",
		fun() ->
			gen_server_mock:expect_call(Mock, fun({get_profile, profile}, _, State) -> {ok, none, State} end),
			?assertEqual(none, get_profile(profile))
		end}
	end
	]}.	
-endif.
