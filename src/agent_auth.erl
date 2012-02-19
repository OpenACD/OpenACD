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

%% @doc Connection to the local authenication cache and integration to another module.
%% Authentication is first checked by the integration module (if any).  If that fails, 
%% this module will fall back to it's local cache in the mnesia 'agent_auth' table.
%% the cache table is both ram and disc copies on all nodes.

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
	F = fun() ->
		mnesia:delete({release_opt, Label}),
		mnesia:write(Rec)
	end,
	mnesia:transaction(F).

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
-spec(destroy_profile/1 :: (Name :: string()) -> 'error' | {'atomic', 'ok'}).
destroy_profile("Default") ->
	error;
destroy_profile(Name) ->
	F = fun() ->
		mnesia:delete({agent_profile, Name}),
		Agents = get_agents(Name),
		Update = fun(Arec) ->
			Newagent = Arec#agent_auth{profile = "Default"},
			destroy(Arec#agent_auth.login),
			mnesia:write(Newagent)
		end,
		lists:map(Update, Agents),
		ok
	end,
	mnesia:transaction(F).

%% @doc Gets the proflie `string() Name'
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
%% @depricated Use {@link set_agent/2} instead.
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
%% @decpricated Use {@link set_agent/2} instead.
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
%% @depricated Use {@link set_agent/2} instead.
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
		{ok, Res} -> Res;
		_ -> deny
	end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
	
%% @doc adds a user to the local cache bypassing the integrated at check.  
%% Note that unlike {@link cache/4} this expects the password in plain 
%% text!
%% @depricated Please use {@link add_agent/1} instead.
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
%% @depricated Please use {@link add_agent/1} instead.
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
-spec(add_agent/1 :: (Proplist :: [{atom(), any()}, ...] | #agent_auth{}) -> {'atomic', 'ok'}).
add_agent(Proplist) when is_list(Proplist) ->
	Rec = build_agent_record(Proplist, #agent_auth{}),
	add_agent(Rec);
add_agent(Rec) when is_record(Rec, agent_auth) ->
	Id = make_id(),
	F = fun() ->
		QH = qlc:q([Rec || #agent_auth{login = Nom} <- mnesia:table(agent_auth), Nom =:= Rec#agent_auth.login]),
		case qlc:e(QH) of
			[] ->
				mnesia:write(Rec#agent_auth{id = Id});
			_ ->
				erlang:error(duplicate_login, Rec)
		end
	end,
	mnesia:transaction(F).

make_id() ->
	Ref = erlang:ref_to_list(make_ref()),
	RemovedRef = string:sub_string(Ref, 6),
	FixedRef = string:strip(RemovedRef, right, $>),
	F = fun(Elem, Acc) ->
		case Elem of
			$. ->
				Acc;
			Else ->
				[Else | Acc]
		end
	end,
	lists:reverse(lists:foldl(F, [], FixedRef)).
	
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
	build_agent_record(Tail, Rec#agent_auth{extended_props = NewProps}).

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
	[fun() ->
		application:set_env('OpenACD', agent_auth_storage, somestore),

		meck:new(somestore),
		meck:expect(somestore, start, fun() -> ok end),

		agent_auth:start(),
		?assert(meck:called(somestore, start, [])),
		?assert(meck:validate(somestore)),
		meck:unload(somestore)
	end,
	fun() ->
		application:unset_env('OpenACD', agent_auth_storage),
		meck:new(agent_auth_mnesia),
		meck:expect(agent_auth_mnesia, start, fun() -> ok end),
		
		agent_auth:start(),
		?assert(meck:called(agent_auth_mnesia, start, [])),
		?assert(meck:validate(agent_auth_mnesia)),
		meck:unload(agent_auth_mnesia)				
	end

	].

get_agents_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(get_agents),
		cpx_hooks:set_hook(a, get_agents, somestore, get_agents, [], 10),

		meck:new(somestore)
	end,
	fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, get_agents, fun() -> none end),
		?assertEqual([], get_agents())
	end},
	{"normal", fun() ->
		meck:expect(somestore, get_agents, 0, 
			{ok, [#agent_auth{id="1", login="ali"},
			#agent_auth{id="2", login="baba"}]}),
		
		?assertEqual([#agent_auth{id="1", login="ali"},
			#agent_auth{id="2", login="baba"}], get_agents())
	end},
	{"multiple sources", fun() ->
		Agent1ali = #agent_auth{id="1", login="ali"},
		Agent2baba = #agent_auth{id="2", login="baba"},
		Agent3kazam = #agent_auth{id="3", login="kazam"},

		meck:expect(somestore, get_agents, 0,
			{ok, [Agent1ali, Agent2baba]}),
		
		meck:expect(somestore, get_agents2, 0, 
			{ok, [Agent3kazam]}),

		cpx_hooks:set_hook(b, get_agents, somestore, get_agents2, [], 5),
		
		?assertEqual([Agent1ali, Agent2baba, Agent3kazam], get_agents())
	end},
	{"id conflict", fun() ->
		Agent1ali = #agent_auth{id="1", login="ali"},
		Agent2baba = #agent_auth{id="2", login="baba"},
		Agent1kazam = #agent_auth{id="1", login="kazam"},

		meck:expect(somestore, get_agents, 0, 
			{ok, [Agent1ali, Agent2baba]}),
		
		meck:expect(somestore, get_agents2, 0, 
			{ok, [Agent1kazam]}),

		cpx_hooks:set_hook(b, get_agents, somestore, get_agents2, [], 5),
		
		?assertEqual([Agent1ali, Agent2baba], get_agents())
	end},
	{"login conflict", fun() ->
		Agent1ali = #agent_auth{id="1", login="ali"},
		Agent2baba = #agent_auth{id="2", login="baba"},
		Agent3ali = #agent_auth{id="3", login="ali"},

		meck:expect(somestore, get_agents, 0, 
			{ok, [Agent1ali, Agent2baba]}),
		
		meck:expect(somestore, get_agents2, 0, 
			{ok, [Agent3ali]}),

		cpx_hooks:set_hook(b, get_agents, somestore, get_agents2, [], 5),
		
		?assertEqual([Agent1ali, Agent2baba], get_agents())
	end}]}.

%% TODO: test for duplicates
get_agents_by_profile_test_() ->
	[{"no hook", fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(get_agents_by_profile),

		?assertEqual([], get_agents("devs"))
	end},
	{"with hooks", fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(get_agents_by_profile),
		cpx_hooks:set_hook(a, get_agents_by_profile, somestore, get_agents1, [], 2),
		cpx_hooks:set_hook(b, get_agents_by_profile, somestore, get_agents2, [], 1),

		Auths1 = [#agent_auth{id="1", login="ali", profile="devs"},
			#agent_auth{id="2", login="baba", profile="devs"}],
		Auths2 = [#agent_auth{id="3", login="mama", profile="devs"},
			#agent_auth{id="4", login="mia", profile="devs"}],

		meck:new(somestore),
		meck:expect(somestore, get_agents1, fun("devs") -> {ok, Auths1} end),
		meck:expect(somestore, get_agents2, fun("devs") -> {ok, Auths2} end),

		?assertEqual(Auths1 ++ Auths2, get_agents("devs")),

		?assert(meck:validate(somestore)),
		meck:unload(somestore)
	end}].

get_agent_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(get_agent),
		cpx_hooks:set_hook(a, get_agent, somestore, get_agent, [], 10),

		meck:new(somestore)
	end, fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, get_agent, fun(_, _) -> none end),

		?assertEqual({atomic, []}, get_agent("noone")),
		?assertEqual({atomic, []}, get_agent(login, "noone")),
		?assertEqual({atomic, []}, get_agent(id, "1")),

		?assert(meck:validate(somestore))
	end},
	{"by id", fun() ->
		cpx_hooks:set_hook(b, get_agent, somestore, get_agent2, [], 5),

		Agent = #agent_auth{id="1", login="ali"},

		meck:expect(somestore, get_agent, fun(id, _) -> none end),
		meck:expect(somestore, get_agent2, fun(id, "1") -> {ok, Agent} end),

		?assertEqual({atomic, [Agent]}, get_agent(id, "1")),

		?assert(meck:validate(somestore))
	end},
	{"by login", fun() ->
		cpx_hooks:set_hook(b, get_agent, somestore, get_agent2, [], 5),

		Agent = #agent_auth{id="1", login="ali"},

		meck:expect(somestore, get_agent, fun(login, _) -> none end),
		meck:expect(somestore, get_agent2, fun(login, "ali") -> {ok, Agent} end),

		?assertEqual({atomic, [Agent]}, get_agent("ali")),
		?assertEqual({atomic, [Agent]}, get_agent(login, "ali")),

		?assert(meck:validate(somestore))
	end}
	]}.

set_agent_test_() ->
	[{"no hooks", {setup, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(set_agent)
	end, fun(_) -> ok end,
	[?_assertEqual({aborted, unhandled}, set_agent("foo", "foologin", [foonglish], agent, "foobar")),
	?_assertEqual({aborted, unhandled}, set_agent("baz", "bazlogin", "bazsecret", [bazz], agent, "foobaz")),
	?_assertEqual({aborted, unhandled}, set_agent("fro", "frologin", "frosecret", [froo], agent, "frobar", "fr", "o")),
	?_assertEqual({aborted, unhandled}, set_agent("bar", []))
	]}},
	{"single hook", {foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(set_agent),
		cpx_hooks:set_hook(a, set_agent, somestore, set_agent, [], 1),

		meck:new(somestore),
		meck:expect(somestore, set_agent, fun(_, _) -> {ok, ok} end),

		ok
	end, fun(_) -> 
		meck:unload(somestore)
	end,
	[fun() ->
		% Deprecated: (Id, Newlogin, Newskills, NewSecurity, Newprofile)
		set_agent("foo", "foologin", [foonglish], agent, "foobar"),
		?assert(meck:called(somestore, set_agent,
			["foo",[{login, "foologin"},
				{skills, [foonglish]},
				{securitylevel, agent},
				{profile, "foobar"}]]))
	end,
	fun() ->
		% Deprecated: (Id, Newlogin, Newpass, Newskills, NewSecurity, Newprofile)
		set_agent("baz", "bazlogin", "bazsecret", [bazz], agent, "foobaz"),
		?assert(meck:called(somestore, set_agent,
			["baz",[{login, "bazlogin"},
				{password, "bazsecret"},
				{skills, [bazz]},
				{securitylevel, agent},
				{profile, "foobaz"}]]))
	end,
	fun() ->
		% Deprecated: (Id, Newlogin, Newpass, Newskills, NewSecurity, Newprofile, Firstname, Lastname)
		set_agent("fro", "frologin", "frosecret", [froo], agent, "frobar", "fr", "o"),
		?assert(meck:called(somestore, set_agent,
			["fro",[{login, "frologin"},
				{password, "frosecret"},
				{skills, [froo]},
				{securitylevel, agent},
				{profile, "frobar"},
				{firstname, "fr"},
				{lastname, "o"}]]))
	end,
	fun() ->
		set_agent("bar", [{login, "barlogin"},
			{password, "barsecret"},
			{skills, [bartending]},
			{securitylevel, admin},
			{profile, "foobar"},
			{firstname, "ba"},
			{lastname, "rr"},
			{endpoints, [{freeswitch_media, []}]},
			{extended_props, [someprop]}]),
		?assert(meck:called(somestore, set_agent,
			["bar", [{login, "barlogin"},
				{password, "barsecret"},
				{skills, [bartending]},
				{securitylevel, admin},
				{profile, "foobar"},
				{firstname, "ba"},
				{lastname, "rr"},
				{endpoints, [{freeswitch_media, []}]},
				{extended_props, [someprop]}]]))
	end
	]}}].

destroy_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(destroy_agent),
		cpx_hooks:set_hook(a, destroy_agent, somestore, destroy_agent, [], 1),

		meck:new(somestore)
	end, fun(_) ->
		meck:unload(somestore)
	end, [
	fun() ->
		meck:expect(somestore, destroy_agent, fun(_, _) -> {ok, ok} end),
		?assertEqual({atomic, ok}, destroy("fooid")),
		?assert(meck:called(somestore, destroy_agent, [login, "fooid"])),
		?assert(meck:validate(somestore))
	end,
	fun() ->
		meck:expect(somestore, destroy_agent, fun(_, _) -> {ok, ok} end),
		?assertEqual({atomic, ok}, destroy(id, "fooid")),
		?assert(meck:called(somestore, destroy_agent, [id, "fooid"])),
		?assert(meck:validate(somestore))
	end,
	fun() ->
		meck:expect(somestore, destroy_agent, fun(_, _) -> {ok, ok} end),
		?assertEqual({atomic, ok}, destroy(login, "fooid")),
		?assert(meck:called(somestore, destroy_agent, [login, "fooid"])),
		?assert(meck:validate(somestore))
	end,
	fun() ->
		meck:expect(somestore, destroy_agent, fun(_, _) -> pass end),
		?assertEqual({aborted, unhandled}, destroy(id, "fooid")),
		?assert(meck:called(somestore, destroy_agent, [id, "fooid"])),
		?assert(meck:validate(somestore))
	end
	]}.

set_endpoint_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(set_endpoint),
		cpx_hooks:set_hook(a, set_endpoint, somestore, set_endpoint, [], 1),

		meck:new(somestore)
	end, fun(_) ->
		meck:unload(somestore)
	end, [
	fun() ->
		meck:expect(somestore, set_endpoint, fun(_, _, _) -> {ok, ok} end),
		?assertEqual({atomic, ok}, set_endpoint({id, "fooid"}, email_media, inband)),
		?assert(meck:called(somestore, set_endpoint,
			[{id, "fooid"}, email_media, inband])),
		?assert(meck:validate(somestore))
	end,
	fun() ->
		%% Set endpoint call to a non existing agent also returns {error, unhandled}
		meck:expect(somestore, set_endpoint, fun(_, _, _) -> {error, noagent} end),
		?assertEqual({error, unhandled}, set_endpoint({id, "fooid"}, email_media, inband)),
		?assert(meck:called(somestore, set_endpoint,
			[{id, "fooid"}, email_media, inband])),
		?assert(meck:validate(somestore))
	end
	]}.

drop_endpoint_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(drop_endpoint),
		cpx_hooks:set_hook(a, drop_endpoint, somestore, drop_endpoint, [], 1),

		meck:new(somestore)
	end, fun(_) ->
		meck:unload(somestore)
	end, [
	fun() ->
		meck:expect(somestore, drop_endpoint, fun(_, _) -> {ok, ok} end),
		?assertEqual({atomic, ok}, drop_endpoint({id, "fooid"}, email_media)),
		?assert(meck:called(somestore, drop_endpoint,
			[{id, "fooid"}, email_media])),
		?assert(meck:validate(somestore))
	end,
	fun() ->
		%% Drop endpoint call to a non existing agent also returns {error, unhandled}
		meck:expect(somestore, drop_endpoint, fun(_, _) -> {error, noagent} end),
		?assertEqual({error, unhandled}, drop_endpoint({id, "fooid"}, email_media)),
		?assert(meck:called(somestore, drop_endpoint,
			[{id, "fooid"}, email_media])),
		?assert(meck:validate(somestore))
	end
	]}.

get_profiles_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(get_profiles),
		cpx_hooks:set_hook(a, get_profiles, somestore, get_profiles, [], 10),

		meck:new(somestore)
	end,
	fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, get_profiles, fun() -> none end),
		?assertEqual([], get_agents())
	end},
	{"normal", fun() ->
		Profiles = [#agent_profile{id="1", name="ali"},
			#agent_profile{id="2", name="baba"}],

		meck:expect(somestore, get_profiles, 0, 
			{ok, Profiles}),
		
		?assertEqual(Profiles, get_profiles())
	end},
	{"sorted", fun() ->
		Profile20b = #agent_profile{id="1", name="ali", order = 20},
		Profile10 = #agent_profile{id="2", name="baba", order = 10},
		Profile20a = #agent_profile{id="3", name="aka", order = 20},
		
		meck:expect(somestore, get_profiles, 0,
			{ok, [Profile20b, Profile10, Profile20a]}),
		
		?assertEqual([Profile10, Profile20a, Profile20b], get_profiles())
	end},
	{"multiple sources", fun() ->
		Profile1 = #agent_profile{id="1", name="ali"},
		Profile2 = #agent_profile{id="2", name="baba"},
		Profile3 = #agent_profile{id="3", name="mama"},

		meck:expect(somestore, get_profiles, 0, 
			{ok, [Profile1, Profile2]}),
		
		meck:expect(somestore, get_profiles2, 0, 
			{ok, [Profile3]}),

		cpx_hooks:set_hook(b, get_profiles, somestore, get_profiles2, [], 5),
		
		?assertEqual([Profile1, Profile2, Profile3], get_profiles())
	end},
	{"id conflict", fun() ->
		Profile1ali = #agent_profile{id="1", name="ali"},
		Profile2baba = #agent_profile{id="2", name="baba"},
		Profile1kazam = #agent_profile{id="1", name="kazam"},

		meck:expect(somestore, get_profiles, 0, 
			{ok, [Profile1ali, Profile2baba]}),
		
		meck:expect(somestore, get_profiles2, 0, 
			{ok, [Profile1kazam]}),

		cpx_hooks:set_hook(b, get_profiles, somestore, get_profiles2, [], 5),
		
		?assertEqual([Profile1ali, Profile2baba], get_profiles())
	end},
	{"name conflict", fun() ->
		Profile1ali = #agent_profile{id="1", name="ali"},
		Profile2baba = #agent_profile{id="2", name="baba"},
		Profile3ali = #agent_profile{id="3", name="ali"},

		meck:expect(somestore, get_profiles, 0, 
			{ok, [Profile1ali, Profile2baba]}),
		
		meck:expect(somestore, get_profiles2, 0, 
			{ok, [Profile3ali]}),

		cpx_hooks:set_hook(b, get_profiles, somestore, get_profiles2, [], 5),
		
		?assertEqual([Profile1ali, Profile2baba], get_profiles())
	end}]}.

get_profile_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(get_profile),
		cpx_hooks:set_hook(a, get_profile, somestore, get_profile, [], 10),

		meck:new(somestore)
	end, fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, get_profile, fun(_) -> none end),
		?assertEqual(undefined, get_profile("nogroup")),
		?assert(meck:validate(somestore))
	end},
	{"by name", fun() ->
		Profile = #agent_profile{id="1", name="baz"},
		meck:expect(somestore, get_profile, fun(_) -> {ok, Profile} end),
		?assertEqual(Profile, get_profile("baz")),
		?assert(meck:validate(somestore))
	end}
	]}.

set_profile_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(set_profile),
		cpx_hooks:set_hook(a, set_profile, somestore, set_profile, [], 10),

		meck:new(somestore)
	end, fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, set_profile, fun(_, _) -> none end),
		?assertEqual({error, unhandled}, set_profile("nothing", #agent_profile{name="baz"})),
		?assert(meck:validate(somestore))
	end},
	{"normal", fun() ->
		Profile = #agent_profile{id="1", name="baz"},
		meck:expect(somestore, set_profile, fun(_, _) -> {ok, Profile} end),
		?assertEqual({atomic, ok}, set_profile("foo", Profile)),
		?assert(meck:validate(somestore))
	end},
	{"disallow renaming of Default", fun() ->
		Profile = #agent_profile{id="1", name="baz"},
		?assertEqual({error, not_allowed}, set_profile("Default", Profile))
	end}
	%% TODO does not handle duplicate name
	]}.

new_profile_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(new_profile),
		cpx_hooks:set_hook(a, new_profile, somestore, new_profile, [], 10),

		meck:new(somestore)
	end, fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, new_profile, fun(_) -> none end),
		?assertEqual({error, unhandled}, new_profile(#agent_profile{name="baz"})),
		?assert(meck:validate(somestore))
	end},
	{"normal", fun() ->
		Profile = #agent_profile{id="1", name="baz"},
		meck:expect(somestore, new_profile, fun(_) -> {ok, Profile} end),
		?assertEqual({atomic, ok}, new_profile(Profile)),
		?assert(meck:validate(somestore))
	end},
	{"disallow adding Default", fun() ->
		Profile = #agent_profile{id="1", name="Default"},
		?assertEqual({error, not_allowed}, new_profile(Profile))
	end}
	%% TODO does not handle duplicate name
	]}.

get_extended_prop_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(get_extended_prop),
		cpx_hooks:set_hook(a, get_extended_prop, somestore, get_extended_prop, [], 10),

		meck:new(somestore)
	end, fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, get_extended_prop, fun(_, noprop) -> none end),
		?assertEqual({error, noagent}, get_extended_prop({id, "noone"}, noprop)),
		?assert(meck:validate(somestore))
	end},
	{"with prop", fun() ->
		meck:expect(somestore, get_extended_prop, fun(_, someprop) -> {ok, someval} end),
		?assertEqual({ok, someval}, get_extended_prop({id, "someone"}, someprop)),
		?assert(meck:validate(somestore))
	end},
	{"without prop", fun() ->
		meck:expect(somestore, get_extended_prop, fun(_, someprop) -> {ok, undefined} end),
		?assertEqual(undefined, get_extended_prop({id, "someone"}, someprop)),
		?assert(meck:validate(somestore))
	end}
	]}.

set_extended_prop_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(set_extended_prop),
		cpx_hooks:set_hook(a, set_extended_prop, somestore, set_extended_prop, [], 10),

		meck:new(somestore)
	end, fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, set_extended_prop, fun(_, noprop, val) -> none end),
		?assertEqual({error, noagent}, set_extended_prop({id, "noone"}, noprop, val)),
		?assert(meck:validate(somestore))
	end},
	{"with prop", fun() ->
		meck:expect(somestore, set_extended_prop, fun(_, someprop, val) -> {ok, ok} end),
		?assertEqual({atomic, ok}, set_extended_prop({id, "someone"}, someprop, val)),
		?assert(meck:validate(somestore))
	end}]}.

drop_extended_prop_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(drop_extended_prop),
		cpx_hooks:set_hook(a, drop_extended_prop, somestore, drop_extended_prop, [], 10),

		meck:new(somestore)
	end, fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, drop_extended_prop, fun(_, noprop) -> none end),
		?assertEqual({error, noagent}, drop_extended_prop({id, "noone"}, noprop)),
		?assert(meck:validate(somestore))
	end},
	{"with prop", fun() ->
		meck:expect(somestore, drop_extended_prop, fun(_, someprop) -> {ok, someval} end),
		?assertEqual({atomic, ok}, drop_extended_prop({id, "someone"}, someprop)),
		?assert(meck:validate(somestore))
	end}
	]}.

get_releases_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(get_releases),
		cpx_hooks:set_hook(a, get_releases, somestore, get_releases, [], 10),

		meck:new(somestore)
	end,
	fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, get_releases, fun() -> none end),
		?assertEqual([], get_releases())
	end},
	{"normal", fun() ->
		Releases = [#release_opt{label="a"},
			#release_opt{label="b"}],

		meck:expect(somestore, get_releases, 0, 
			{ok, Releases}),
		
		?assertEqual(Releases, get_releases())
	end},
	{"multiple sources", fun() ->
		Release1 = #release_opt{label="a"},
		Release2 = #release_opt{label="b"},
		Release3 = #release_opt{label="c"},

		meck:expect(somestore, get_releases, 0, 
			{ok, [Release1, Release2]}),
		
		meck:expect(somestore, get_releases2, 0, 
			{ok, [Release3]}),

		cpx_hooks:set_hook(b, get_releases, somestore, get_releases2, [], 5),
		
		?assertEqual([Release1, Release2, Release3], get_releases())
	end}]}. %% no conflict handling


new_release_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(new_release),
		cpx_hooks:set_hook(a, new_release, somestore, new_release, [], 10),

		meck:new(somestore)
	end,
	fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		Release = #release_opt{label="a"},
		meck:expect(somestore, new_release, 1, none),
		?assertEqual({aborted, unhandled}, new_release(Release))
	end},
	{"normal", fun() ->
		Release = #release_opt{label="a"},
		meck:expect(somestore, new_release, 1, {ok, ok}),
		?assertEqual({atomic, ok}, new_release(Release))
	end}]}.

destroy_release_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(destroy_release),
		cpx_hooks:set_hook(a, destroy_release, somestore, destroy_release, [], 10),

		meck:new(somestore)
	end,
	fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, destroy_release, 2, none),
		?assertEqual({aborted, unhandled}, destroy_release("rela")),
		?assertEqual({aborted, unhandled}, destroy_release(id, "relb")),
		?assertEqual({aborted, unhandled}, destroy_release(label, "relc")),

		?assert(meck:validate(somestore))
	end},
	{"normal", fun() ->
		meck:expect(somestore, destroy_release, 2, {ok, ok}),
		?assertEqual({atomic, ok}, destroy_release("rela")),
		?assertEqual({atomic, ok}, destroy_release(id, "relb")),
		?assertEqual({atomic, ok}, destroy_release(label, "relc")),

		?assert(meck:called(somestore, destroy_release, [label, "rela"])),
		?assert(meck:called(somestore, destroy_release, [id, "relb"])),
		?assert(meck:called(somestore, destroy_release, [label, "relc"])),

		?assert(meck:validate(somestore))
	end}]}.

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
