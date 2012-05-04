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

%% @doc Connection to the local authenication cache and integration to
%% another module.  Authentication is first checked by the integration 
%% module (if any).  If that fails, this module will fall back to it's 
%% local cache in the mnesia 'agent_auth' table.  The cache table is both 
%% ram and disc copies on all nodes.
%%
%% == Hooks ==
%%
%% Many many hooks; adding, droping, updating, and fetching.
%%
%% === agent_auth_profile_create ===
%%
%% Asynchronously triggers all hooks when a profile is created.
%% Args :: [Rec :: #agent_profile{}]
%%
%% === agent_auth_profile_update ===
%%
%% Aynchronously triggers all hooks when a profile is updated.
%% Args :: [OldName :: string(), NewRec :: #agent_profile{}]
%%
%% === agent_auth_profile_destroy ===
%%
%% Asynchronously triggers all hooks when a profile is removed.
%% Args :: [Name :: string()].
%%
%% === agent_auth_agent_update ===
%%
%% Asynchronously triggers all hooks when an agent is updated.
%% Args :: [OldAgent :: #agent_auth{}, NewAgent :: #agent_auth{}]
%%
%% === agent_auth_agent_set_endpoint ===
%%
%% Asynchronously triggers all hooks when an agent's endpoint is set.
%% Args :: [{Type :: 'id' | 'login', AgentVal :: string()},
%%     Endpoint :: atom(), Data :: any()]
%%
%% === agent_auth_agent_drop_endpoint ===
%%
%% Asynchronously triggers all hooks when an agent has an endpoint removed.
%% Args :: [{Type :: 'id' | 'login', AgentVal :: string()},
%%     Endpoint :: atom()]
%%
%% === agent_auth_agent_set_extended ===
%%
%% Asynchronously triggers all hooks when an agent has an extended property
%% set.
%% Args :: [{Type :: 'id' | 'login', AgentVal :: string()},
%%     Prop :: any(), Val :: any()]
%%
%% === agent_auth_agent_drop_extended ===
%%
%% Asynchrously triggers all hooks when an agent has an extended property
%% removed.
%% Args :: [{Type :: 'id' | 'login', AgentVal :: string()},
%%     Prop :: any()]
%%
%% === agent_auth_agent_add ===
%%
%% Aysnchronously triggers all hooks when an agent is created.
%% Args :: [Record :: #agent_auth{}]
%%
%% === auth_agent ===
%%
%% Synchronously triggers hooks until one returns a response.  If no hooks
%% return a response, a denial is returned.
%%
%% Args :: [Username :: string(), Password :: string()]
%% Returns :: 'deny' | {'allow', string(), skill_list(), security_level(),
%% profile_name()}).
%%
%% === auth_agent_success ===
%%
%% Asynchronously triggers all hooks when an agent is successfully 
%% authenticated.
%%
%% Args :: [Username :: string(), Password :: string(),
%% Return :: auth_agent_return()]

-module(agent_auth).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include_lib("stdlib/include/qlc.hrl").

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
-endif.


%% API
-export([
	start/0,
	auth/2
]).
-export([
	destroy/1,
	destroy/2,
	add_agent/7,
	add_agent/5,
	add_agent/1,
	set_agent/5,
	set_agent/6,
	set_agent/8,
	set_agent/2,
	get_agent/1,
	get_agent/2,
	get_agents/0,
	get_agents/1,
	set_endpoint/3,
	drop_endpoint/2,
	set_extended_prop/3,
	drop_extended_prop/2,
	get_extended_prop/2,
	encode_password/1
]).
-export([
	new_profile/1,
	new_profile/2,
	set_profile/2,
	set_profile/3,
	get_profile/1,
	get_profiles/0,
	destroy_profile/1
	]).
%% API for release options
-export([
	new_release/1,
	destroy_release/1,
	destroy_release/2,
	update_release/2,
	get_releases/0
	]).

%%====================================================================
%% API
%%====================================================================

start() ->
	Store = case application:get_env('OpenACD', agent_auth_storage) of
		{ok, St} -> St;
		undefined -> agent_auth_mnesia
	end,
	Store:start().

%% @doc Add `#release_opt{} Rec' to the database. 
-spec(new_release/1 :: (Rec :: #release_opt{}) -> {'atomic', 'ok'} | {'aborted', any()}).
new_release(Rec) when is_record(Rec, release_opt) ->
	case cpx_hooks:trigger_hooks(new_release, [Rec], first) of
		{ok, _} -> {atomic, ok};
		{error, Err} -> {aborted, Err}
	end.

%% @doc Remove the release option `string() Label' from the database.
-spec(destroy_release/1 :: (Label :: string()) -> {'atomic', 'ok'}).
destroy_release(Label) when is_list(Label) ->
	destroy_release(label, Label).

%% @doc Remove the release option with the key (id, label) of value from the 
%% database.
-spec(destroy_release/2 :: (Key :: 'id' | 'label', Value :: pos_integer() | string()) -> {'atomic', 'ok'} | {'aborted', any()}).
destroy_release(Type, Val) ->
	case cpx_hooks:trigger_hooks(destroy_release, [Type, Val], first) of
		{ok, _} -> {atomic, ok};
		{error, Err} -> {aborted, Err}
	end.

%% @doc Update the release option `string() Label' to `#release_opt{} Rec'.
-spec(update_release/2 :: (Label :: string(), Rec :: #release_opt{}) -> {'atomic', 'ok'}).
update_release(Label, Rec) when is_list(Label), is_record(Rec, release_opt) ->
	case cpx_hooks:trigger_hooks(update_release, [Label, Rec], first) of
		{ok, _} -> {atomic, ok};
		{error, Err} -> {aborted, Err}
	end.

%% @doc Get all `#release_opt'.
-spec(get_releases/0 :: () -> [#release_opt{}]).
get_releases() ->
	case cpx_hooks:trigger_hooks(get_releases, [], all) of
		{ok, Lists} -> lists:append(Lists);
		Err -> Err
	end.

%% @doc Create a new agent profile.
-spec(new_profile/1 :: (Rec :: #agent_profile{}) -> {'error', any()} | {'atomic', 'ok'}).
new_profile(#agent_profile{name = "Default"}) ->
	?ERROR("Default cannot be added as a new profile", []),
	{error, not_allowed};
new_profile(Rec) when is_record(Rec, agent_profile)->
	case cpx_hooks:trigger_hooks(new_profile, [Rec], first) of
		{ok, _} -> {atomic, ok};
		Err -> Err
	end.

%% @doc Create a new agent profile `string() Name' with `[atom()] Skills'.
-spec(new_profile/2 :: (Name :: string(), Skills :: [atom()]) -> {'atomic', 'ok'}).
new_profile(Name, Skills) ->
	Rec = #agent_profile{name = Name, skills = Skills},
	new_profile(Rec).

-spec(set_profile/3 :: (Oldname :: string(), Name :: string(), Skills :: [atom()]) -> {'atomic', 'ok'}).
set_profile(Oldname, Name, Skills) ->

	Old = agent_auth:get_profile(Oldname),
	New = Old#agent_profile{
		name = Name,
		skills = Skills
	},
	set_profile(Oldname, New).

%% @doc Update the profile `string() Oldname' to the given rec.
-spec(set_profile/2 :: (Oldname :: string(), Rec :: #agent_profile{}) -> {'atomic', 'ok'} | {error, any()}).
set_profile(Oldname, #agent_profile{name = Newname} = Rec) ->
	case Oldname =:= "Default" andalso Newname =/= "Default" of
		true ->
			?ERROR("Cannot change the name of the default profile", []),
			{error, not_allowed};
		_ ->
			case cpx_hooks:trigger_hooks(set_profile, [Oldname, Rec], first) of
				{ok, _} -> {atomic, ok};
				Err -> Err
			end
	end.

%% @doc Remove the profile `string() Name'.  Returns `error' if you try to remove the profile `"Default"'.
-spec(destroy_profile/1 :: (Name :: string()) -> {'error', any()} | {'atomic', 'ok'}).
destroy_profile("Default") ->
	{error, not_allowed};
destroy_profile(Name) ->
	case cpx_hooks:trigger_hooks(destroy_profile, [Name], first) of
		{ok, _} -> {atomic, ok};
		Err -> Err
	end.

%% @doc Gets the profile `string() Name'
-spec(get_profile/1 :: (Name :: string()) -> #agent_profile{} | 'undefined').
get_profile(Name) ->
	case cpx_hooks:trigger_hooks(get_profile, [Name], first) of
		{ok, Profile} -> Profile;
		_ -> undefined
	end.

%% @doc Return all agent profiles.
-spec(get_profiles/0 :: () -> [#agent_profile{}]).
get_profiles() ->
	case cpx_hooks:trigger_hooks(get_profiles, [], all) of
		{ok, ProfilesLists} ->
			IsUniqueFun = fun(#agent_profile{id=Id, name=Name}, {IdSet, NameSet}) ->
				not (gb_sets:is_member(Id, IdSet) orelse gb_sets:is_member(Name, NameSet))
			end,

			AddToChecksFun = fun(#agent_profile{id=Id, name=Name}, {IdSet, NameSet}) ->
				{gb_sets:add(Id, IdSet), gb_sets:add(Name, NameSet)}
			end,

			Init = {gb_sets:new(), gb_sets:new()},
			Profiles = get_uniques(IsUniqueFun, AddToChecksFun, Init, ProfilesLists),
			sort_profiles(Profiles);
		_ -> []
	end.

get_uniques(IsUniqueFun, AddToChecksFun, Init, EntriesLists) ->
	GetUniquesFun = fun(Entry, {UniqueCheckAcc, UniqueEntries}) ->
		case IsUniqueFun(Entry, UniqueCheckAcc) of
			true -> {AddToChecksFun(Entry, UniqueCheckAcc), [Entry|UniqueEntries]};
			false -> {UniqueCheckAcc, UniqueEntries}
		end
	end,

	{_, Uniques} = lists:foldl(fun(Entries, Acc) ->
		lists:foldl(GetUniquesFun, Acc, Entries)
	end, {Init, []}, EntriesLists),

	lists:reverse(Uniques).

%% @doc Update the agent `string() Oldlogin' without changing the password.
%% @deprecated Use {@link set_agent/2} instead.
-spec(set_agent/5 :: (Id :: string(), Newlogin :: string(), Newskills :: [atom()], NewSecurity :: security_level(), Newprofile :: string()) -> {'atomic', 'ok'} | {'aborted', any()}).
set_agent(Id, Newlogin, Newskills, NewSecurity, Newprofile) ->
	Props = [
		{login, Newlogin},
		{skills, Newskills},
		{securitylevel, NewSecurity},
		{profile, Newprofile}
	],
	set_agent(Id, Props).

%% @doc Sets the agent `string() Oldlogin' with new data in `proplist Props'; 
%% does not change data that is not in the proplist.  The proplist's 
%% `endpoints' field can also contain a partial list, preserving existing
%% settings.
-spec(set_agent/2 :: (Oldlogin :: string(), Props :: [{atom(), any()}]) -> {'atomic', 'ok'} | {'aborted', any()}).
set_agent(Id, Props) ->
	case cpx_hooks:trigger_hooks(set_agent, [Id, Props], first) of
		{ok, _} -> {atomic, ok};
		{error, Err} -> {aborted, Err}
	end.

%% @doc Update the agent `string() Oldlogin' with a new password (as well 
%% as everything else).
%% @deprecated Use {@link set_agent/2} instead.
-spec(set_agent/6 :: (Oldlogin :: string(), Newlogin :: string(), Newpass :: string(), Newskills :: [atom()], NewSecurity :: security_level(), Newprofile :: string()) -> {'atomic', 'error'} | {'atomic', 'ok'}).
set_agent(Id, Newlogin, Newpass, Newskills, NewSecurity, Newprofile) ->
	Props = [
		{login, Newlogin},
		{password, Newpass},
		{skills, Newskills},
		{securitylevel, NewSecurity},
		{profile, Newprofile}
	],
	set_agent(Id, Props).

%% @doc Update the agent `string() Oldlogin' with a new password (as well 
%% as everything else).
%% @deprecated Use {@link set_agent/2} instead.
-spec(set_agent/8 :: (Oldlogin :: string(), Newlogin :: string(), Newpass :: string(), Newskills :: [atom()], NewSecurity :: security_level(), Newprofile :: string(), Newfirstname :: string(), Newlastname :: string()) -> {'atomic', 'error'} | {'atomic', 'ok'}).
set_agent(Id, Newlogin, Newpass, Newskills, NewSecurity, Newprofile, Newfirstname, Newlastname) ->
	Props = [
		{login, Newlogin},
		{password, Newpass},
		{skills, Newskills},
		{securitylevel, NewSecurity},
		{profile, Newprofile},
		{firstname, Newfirstname},
		{lastname, Newlastname}
	],
	set_agent(Id, Props).

%% @doc Gets `#agent_auth{}' associated with `string() Login'.
-spec(get_agent/1 :: (Login :: string()) -> {'atomic', [#agent_auth{}]}).
get_agent(Login) ->
	get_agent(login, Login).

%% @doc Get an agent who's `Key' is `Value'.
-spec(get_agent/2 :: (Key :: 'id' | 'login', Value :: string()) -> {'atomic', [#agent_auth{}]}).
get_agent(login, Value) ->
	case cpx_hooks:trigger_hooks(get_agent, [login, Value], first) of
		{ok, Auth} -> {atomic, [Auth]};
		_ -> {atomic, []}
	end;
get_agent(id, Value) ->
	case cpx_hooks:trigger_hooks(get_agent, [id, Value], first) of
		{ok, Auth} -> {atomic, [Auth]};
		_ -> {atomic, []}
	end.

%% @doc Gets All the agents.
-spec(get_agents/0 :: () -> [#agent_auth{}]).
get_agents() ->
	case cpx_hooks:trigger_hooks(get_agents, [], all) of
		{ok, AuthsLists} ->
			IsUniqueFun = fun(#agent_auth{id=Id, login=Login}, {IdSet, LoginSet}) ->
				not (gb_sets:is_member(Id, IdSet) orelse gb_sets:is_member(Login, LoginSet))
			end,

			AddToChecksFun = fun(#agent_auth{id=Id, login=Login}, {IdSet, LoginSet}) ->
				{gb_sets:add(Id, IdSet), gb_sets:add(Login, LoginSet)}
			end,

			Init = {gb_sets:new(), gb_sets:new()},
			get_uniques(IsUniqueFun, AddToChecksFun, Init, AuthsLists);
		_ -> []
	end.

%% @doc Gets all the agents associated with `string() Profile'.
-spec(get_agents/1 :: (Profile :: string()) -> [#agent_auth{}]).
get_agents(Profile) ->
	%% TODO handle duplicate ids/logins. What if they're from a different profile?
	case cpx_hooks:trigger_hooks(get_agents_by_profile, [Profile], all) of
		{ok, Auths} -> lists:flatten(Auths);
		_ -> []
	end.

-spec(set_endpoint/3 :: (Key :: {'login' | 'id', string()}, Endpoint :: atom(), Data :: any()) -> {'atomic', 'ok'} | {'error', any()}).
set_endpoint({_Type, _Aval} = U, Endpoint, Data) ->
	case cpx_hooks:trigger_hooks(set_endpoint, [U, Endpoint, Data], first) of
		{ok, _} -> {atomic, ok};
		{error, _} = E -> E
	end.

-spec(drop_endpoint/2 :: (Key :: {'login' | 'id', string()}, Endpoint :: atom()) -> {'atomic', 'ok'}).
drop_endpoint({_Type, _Aval} = U, Endpoint) ->
	case cpx_hooks:trigger_hooks(drop_endpoint, [U, Endpoint], first) of
		{ok, _} -> {atomic, ok};
		{error, _} = E -> E
	end.

-spec(set_extended_prop/3 :: (Key :: {'login' | 'id', string()}, Prop :: atom(), Val :: any()) -> {'atomic', 'ok'} | {'error', any()}).
set_extended_prop({_, _} = U, Prop, Val) ->
	case cpx_hooks:trigger_hooks(set_extended_prop, [U, Prop, Val], first) of
		{ok, _} -> {atomic, ok};
		{error, unhandled} -> {error, noagent};
		Err -> Err
	end.

-spec(drop_extended_prop/2 :: (Key :: {'login' | 'id', string()}, Prop :: atom()) -> {'atomic', 'ok'}).
drop_extended_prop({_, _} = U, Prop) ->
	case cpx_hooks:trigger_hooks(drop_extended_prop, [U, Prop], first) of
		{ok, _} -> {atomic, ok};
		{error, unhandled} -> {error, noagent}
	end.

%% @doc Get an extened property either from the database or a record 
%% directly.
-spec(get_extended_prop/2 :: (Key :: {'login' | 'id', string()}, Prop :: atom()) -> {'ok', any()} | {'error', 'noagent'} | 'undefined').
get_extended_prop({_, _} = U, Prop) ->
	case cpx_hooks:trigger_hooks(get_extended_prop, [U, Prop], first) of
		{ok, undefined} -> undefined;
		{ok, _} = V -> V;
		{error, unhandled} -> {error, noagent}
	end.

%% @doc Take the plaintext username and password and attempt to 
%% authenticate the agent.
-type(profile_name() :: string()).
-spec(auth/2 :: (Username :: string(), Password :: string()) -> 'deny' | {'allow', string(), skill_list(), security_level(), profile_name()}).
auth(Username, Password) ->
	case cpx_hooks:trigger_hooks(auth_agent, [Username, Password], first) of
		{ok, deny} -> deny;
		{ok, Res} ->
			cpx_hooks:async_trigger_hooks(auth_agent_success, [Username, Password, Res]),
			Res;
		_ -> deny
	end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @doc adds a user to the local cache bypassing the integrated at check.  
%% Note that unlike {@link cache/4} this expects the password in plain 
%% text!
%% @deprecated Please use {@link add_agent/1} instead.
-spec(add_agent/5 :: 
	(Username :: string(), Password :: string(), Skills :: [atom()], Security :: 'admin' | 'agent' | 'supervisor', Profile :: string()) -> 
		{'atomic', 'ok'}).
add_agent(Username, Password, Skills, Security, Profile) ->
	Rec = #agent_auth{
		login = Username,
		password = util:bin_to_hexstr(erlang:md5(Password)),
		skills = Skills,
		securitylevel = Security,
		profile = Profile},
	add_agent(Rec).

%% @doc adds a user to the local cache bypassing the integrated at check.  
%% Note that unlike {@link cache/4} this expects the password in plain 
%% text!
%% @deprecated Please use {@link add_agent/1} instead.
-spec(add_agent/7 ::
	(Username :: string(), Firstname :: string(), Lastname :: string(), Password :: string(), Skills :: [atom()], Security :: 'admin' | 'agent' | 'supervisor', Profile :: string()) ->
		{'atomic', 'ok'}).
add_agent(Username, Firstname, Lastname, Password, Skills, Security, Profile) ->
	Rec = #agent_auth{
		login = Username,
		password = util:bin_to_hexstr(erlang:md5(Password)),
		skills = Skills,
		securitylevel = Security,
		profile = Profile,
		firstname = Firstname,
		lastname = Lastname},
	add_agent(Rec).

%% @doc adds a user to the local cache.  Accepts either `#agent_auth{}' or
%% a proplist as the initial argument.  If an agent with the given login
%% already exists, this throws an error.  An id is created for ye.  The
%% password should not be encoded.
-spec(add_agent/1 :: (Proplist :: [{atom(), any()}, ...] | #agent_auth{}) -> {'atomic', 'ok'} | {'abort', any()}).
add_agent(Proplist) when is_list(Proplist) ->
	Rec = build_agent_record(Proplist, #agent_auth{}),
	add_agent(Rec);
add_agent(Rec) when is_record(Rec, agent_auth) ->
	case cpx_hooks:trigger_hooks(add_agent, [Rec], first) of
		{ok, _} -> {atomic, ok};
		{error, Err} -> {abort, Err}
	end.

%% @doc Removes the passed user with login of `Username' from the local cache.  Called when integration returns a deny.
-spec(destroy/1 :: (Username :: string()) -> {'atomic', 'ok'} | {'aborted', any()}).
destroy(Username) ->
	destroy(login, Username).

%% @doc Destory either by id or login.
-spec(destroy/2 :: (Key :: 'id' | 'login', Value :: string()) -> {'atomic', 'ok'} | {'aborted', any()}).
destroy(Key, Value) ->
	case cpx_hooks:trigger_hooks(destroy_agent, [Key, Value], first) of
		{ok, _} -> {atomic, ok};
		{error, Err} -> {aborted, Err}
	end.

%% @doc Sorts the profiles based on sort order, then alphabetical.
-spec(sort_profiles/1 :: (List :: [#agent_profile{}]) -> [#agent_profile{}]).
sort_profiles(List) ->
	lists:sort(fun comp_profiles/2, List).
			
comp_profiles(#agent_profile{name = Aname, order = S}, #agent_profile{name = Bname, order = S}) ->
	Aname =< Bname;
comp_profiles(#agent_profile{order = Asort}, #agent_profile{order = Bsort}) ->
	Asort =< Bsort.

%% @doc Builds up an `#agent_auth{}' from the given `proplist() Proplist'.
%% Merges endpoints and extended props so old ones are not smashed.
-spec(build_agent_record/2 :: (Proplist :: [{atom(), any()}], Rec :: #agent_auth{}) -> #agent_auth{}).
build_agent_record([], Rec) ->
	Rec;

build_agent_record([{login, Login} | Tail], Rec) ->
	build_agent_record(Tail, Rec#agent_auth{login = Login});

build_agent_record([{password, Password} | Tail], Rec) ->
	build_agent_record(Tail, Rec#agent_auth{password = encode_password(Password)});

build_agent_record([{skills, Skills} | Tail], Rec) when is_list(Skills) ->
	build_agent_record(Tail, Rec#agent_auth{skills = Skills});

build_agent_record([{securitylevel, Sec} | Tail], Rec) ->
	build_agent_record(Tail, Rec#agent_auth{securitylevel = Sec});

build_agent_record([{profile, Profile} | Tail], Rec) ->
	build_agent_record(Tail, Rec#agent_auth{profile = Profile});

build_agent_record([{firstname, Name} | Tail], Rec) ->
	build_agent_record(Tail, Rec#agent_auth{firstname = Name});

build_agent_record([{lastname, Name} | Tail], Rec) ->
	build_agent_record(Tail, Rec#agent_auth{lastname = Name});

build_agent_record([{endpoints, Ends} | Tail], Rec) when is_list(Ends) ->
	case Rec#agent_auth.endpoints of
		undefined ->
			build_agent_record(Tail, Rec#agent_auth{endpoints = Ends});
		OldEnds ->
			NewEnds = proplist_overwrite(Ends, OldEnds),
			build_agent_record(Tail, Rec#agent_auth{endpoints = NewEnds})
	end;

build_agent_record([{extended_props, Props} | Tail], Rec) ->
	OldProps = Rec#agent_auth.extended_props,
	NewProps = proplist_overwrite(Props, OldProps),
	build_agent_record(Tail, Rec#agent_auth{extended_props = NewProps});

build_agent_record([{id, Id} | Tail], Rec) ->
	build_agent_record(Tail, Rec#agent_auth{id = Id}).

proplist_overwrite([], Acc) ->
	Acc;
proplist_overwrite([{Key, Value} | Tail], Acc) ->
	MidAcc = proplists:delete(Key, Acc),
	NewAcc = [{Key, Value} | MidAcc],
	proplist_overwrite(Tail, NewAcc);
proplist_overwrite([Atom | Tail], Acc) when is_atom(Atom) ->
	NewAcc = case proplists:get_value(Atom, Acc) of
		undefined ->
			[Atom | Acc];
		Atom ->
			Acc;
		_Else ->
			MidAcc = proplists:delete(Atom, Acc),
			[Atom | MidAcc]
	end,
	proplist_overwrite(Tail, NewAcc).

encode_password(Password) ->
	util:bin_to_hexstr(erlang:md5(Password)).

-ifdef(TEST).

%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------
start_test_() ->
	{setup, fun() ->
		application:set_env('OpenACD', agent_auth_storage, somestore),

		meck:new(somestore),
		meck:expect(somestore, start, fun() -> ok end)

	end,fun(_) -> [
	{"normal", fun() ->
		Release = #release_opt{label="a"},
		meck:expect(somestore, update_release, 2, {ok, ok}),
		?assertEqual({atomic, ok}, update_release("foo", Release)),
		?assert(meck:called(somestore, update_release, ["foo", Release])),
		?assert(meck:validate(somestore))
	end}]end}.

auth_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(auth),
		cpx_hooks:set_hook(a, auth_agent, somestore, auth_agent, [], 10),

		meck:new(somestore)
	end,
	fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, auth_agent, 2, none),
		?assertEqual(deny, auth("user", "password")),

		?assert(meck:validate(somestore))
	end},
	{"allow", fun() ->
		meck:expect(somestore, auth_agent, 2, {ok, {allow, "fooid",
			[foonglish], admin, "foobar"}}),
		?assertEqual({allow, "fooid", [foonglish], admin, "foobar"},
			auth("user", "password")),
		?assert(meck:validate(somestore))
	end},
	{"deny", fun() ->
		meck:expect(somestore, auth_agent, 2, {ok, deny}),
		?assertEqual(deny, auth("user", "wrongo")),
		?assert(meck:validate(somestore))
	end}]}.

encode_password_test_() ->
	[?_assertEqual("d41d8cd98f00b204e9800998ecf8427e", encode_password("")),
	?_assertEqual("9a618248b64db62d15b300a07b00580b", encode_password("supersecret"))].

-endif.
