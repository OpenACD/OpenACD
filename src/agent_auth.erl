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
	auth/2,
	build_tables/0
]).
-export([
	cache/6,
	destroy/1,
	destroy/2,
	merge/3,
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
%% helper funcs for merge.
-export([
	query_agent_auth/1,
	query_profiles/1,
	query_release/1,
	upgrade_v1_table/0
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
	F = fun() ->
		mnesia:write(Rec)
	end,
	mnesia:transaction(F).

%% @doc Remove the release option `string() Label' from the database.
-spec(destroy_release/1 :: (Label :: string()) -> {'atomic', 'ok'}).
destroy_release(Label) when is_list(Label) ->
	destroy_release(label, Label).

%% @doc Remove the release option with the key (id, label) of value from the 
%% database.
-spec(destroy_release/2 :: (Key :: 'id' | 'label', Value :: pos_integer() | string()) -> {'atomic', 'ok'} | {'aborted', any()}).
destroy_release(id, Id) ->
	F = fun() ->
		mnesia:delete({release_opt, Id})
	end,
	mnesia:transaction(F);
destroy_release(label, Label) ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(release_opt), X#release_opt.label =:= Label]),
		case qlc:e(QH) of
			[] ->
				ok;
			[#release_opt{id = Id}] ->
				mnesia:delete({release_opt, Id});
			_Else ->
				{error, ambiguous_label}
		end
	end,
	mnesia:transaction(F).

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
	F = fun() ->
		Select = qlc:q([X || X <- mnesia:table(release_opt)]),
		qlc:e(Select)
	end,
	{atomic, Opts} = mnesia:transaction(F),
	lists:sort(Opts).

%% @doc Create a new agent profile.
-spec(new_profile/1 :: (Rec :: #agent_profile{}) -> 'error' | {'atomic', 'ok'}).
new_profile(#agent_profile{id = undefined} = Rec) ->
	new_profile(give_profile_id(Rec));
new_profile(#agent_profile{name = "Default"}) ->
	?ERROR("Default cannot be added as a new profile", []),
	error;
new_profile(Rec) ->
	F = fun() ->
		case qlc:e(qlc:q([Out || #agent_profile{name = N} = Out <- mnesia:table(agent_profile), N =:= Rec#agent_profile.name])) of
			[] ->
				mnesia:write(Rec);
			_ ->
				erlang:error(duplicate_name, Rec)
		end
	end,
	mnesia:transaction(F).

%% @doc Create a new agent profile `string() Name' with `[atom()] Skills'.
-spec(new_profile/2 :: (Name :: string(), Skills :: [atom()]) -> {'atomic', 'ok'}).
new_profile(Name, Skills) ->
	Rec = #agent_profile{name = Name, skills = Skills},
	new_profile(Rec).

-spec(set_profile/3 :: (Oldname :: string(), Name :: string(), Skills :: [atom()]) -> {'atomic', 'ok'}).
set_profile(Oldname, Name, Skills) ->
	_Old = agent_auth:get_profile(Oldname),
	New = #agent_profile{
		name = Name,
		skills = Skills
	},
	set_profile(Oldname, New).

%% @doc Update the profile `string() Oldname' to the given rec.
-spec(set_profile/2 :: (Oldname :: string(), Rec :: #agent_profile{}) -> {'atomic', 'ok'}).
set_profile(Old, #agent_profile{id = undefined} = Rec) ->
	Oldprof = get_profile(Old),
	set_profile(Old, Rec#agent_profile{id = Oldprof#agent_profile.id});
set_profile(Oldname, #agent_profile{name = Oldname} = Rec) ->
	F = fun() ->
		mnesia:write(Rec)
	end,
	mnesia:transaction(F);
set_profile("Default", _Rec) ->
	?ERROR("Cannot change the name of the default profile", []),
	error;
set_profile(Oldname, #agent_profile{name = Newname} = Rec) ->
	F = fun() ->
		case qlc:e(qlc:q([Found || #agent_profile{name = Nom} = Found <- mnesia:table(agent_profile), Nom =:= Newname])) of
			[] ->
				mnesia:delete({agent_profile, Oldname}),
				mnesia:write(Rec),
				qlc:e(qlc:q([mnesia:write(Arec#agent_auth{profile = Newname}) || Arec <- mnesia:table(agent_auth), Arec#agent_auth.profile =:= Oldname])),
				ok;
			_ ->
				erlang:error(duplicate_name, Rec)
		end
	end,
	mnesia:transaction(F).

%% @doc generate an id for the profile rec and return the 'fixed' rec.
give_profile_id(Rec) ->
	F = fun() ->
		qlc:e(qlc:q([Id || #agent_profile{id = Id} <- mnesia:table(agent_profile)]))
	end,
	{atomic, Ids} = mnesia:transaction(F),
	Fold = fun(Elem, Acc) ->
		Newacc = try list_to_integer(Elem) of
			E when Acc < E->
				E;
			_ ->
				Acc
		catch
			error:badarg ->
				Acc
		end,
		Newacc
	end,
	Id = integer_to_list(lists:foldl(Fold, 0, Ids) + 1),
	Rec#agent_profile{id = Id}.
	

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
	try integration:get_profile(Name) of
		none ->
			?DEBUG("integration has no such profile ~p", [Name]),
			destroy_profile(Name),
			undefined;
		{ok, Name, Id, Order, Options, Skills} ->
			?DEBUG("integration found profile ~p", [Name]),

			Rec = #agent_profile{
				name = Name,
				id = Id,
				order = Order,
				options = Options,
				skills = Skills,
				timestamp = util:now()
			},

			F = fun() -> mnesia:write(Rec) end,
			{atomic, ok} = mnesia:transaction(F),
			
			local_get_profile(Name);
		{error, nointegration} ->
			?DEBUG("No integration, falling back for ~p", [Name]),
			local_get_profile(Name)
	catch
		throw:{badreturn, Err} ->
			?WARNING("Integration failed with message:  ~p", [Err]),
			local_get_profile(Name)
	end.


-spec(local_get_profile/1 :: (Name :: string()) -> #agent_profile{} | 'undefined').
local_get_profile(Name) ->
	F = fun() ->
		mnesia:read({agent_profile, Name})
	end,
	case mnesia:transaction(F) of
		{atomic, []} ->
			undefined;
		{atomic, [Profile]} ->
			Profile
	end.

%% @doc Return all profiles as `[{string() Name, [atom] Skills}]'.
-spec(get_profiles/0 :: () -> [#agent_profile{}]).
get_profiles() ->
	F = fun() ->
		QH = qlc:q([ X || X <- mnesia:table(agent_profile)]),
		qlc:e(QH)
	end,
	{atomic, Profiles} = mnesia:transaction(F),
	sort_profiles(Profiles).

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
			GetUniqueAgentsFun = fun(Auth = #agent_auth{id=Id, login=Login},
						{IdSet, LoginSet, AuthAcc}) ->
				case gb_sets:is_member(Id, IdSet) orelse
						gb_sets:is_member(Login, LoginSet) of
					false -> {gb_sets:add(Id, IdSet),
						gb_sets:add(Login, LoginSet),
						[Auth|AuthAcc]};
					true -> {IdSet, LoginSet, AuthAcc}
				end
			end,

			{_, _, UAuths} = lists:foldl(fun(Auths, Acc) ->
				lists:foldl(GetUniqueAgentsFun, Acc, Auths)
			end, {gb_sets:new(), gb_sets:new(), []}, AuthsLists),

			lists:reverse(UAuths);
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
drop_endpoint({Type, Aval}, Endpoint) ->
	case get_agent(Type, Aval) of
		{atomic, [#agent_auth{endpoints = OldEnds} = Rec]} ->
			case proplists:delete(Endpoint, Rec#agent_auth.endpoints) of
				OldEnds ->
					{atomic, ok};
				Newends ->
					F = fun() ->
						mnesia:write(Rec#agent_auth{endpoints = Newends})
					end,
					mnesia:transaction(F)
			end
	end.

-spec(set_extended_prop/3 :: (Key :: {'login' | 'id', string()}, Prop :: atom(), Val :: any()) -> {'atomic', 'ok'}).
set_extended_prop({Type, Aval}, Prop, Val) ->
	case get_agent(Type, Aval) of
		{atomic, [Rec]} ->
			Midprops = proplists:delete(Prop, Rec#agent_auth.extended_props),
			Newprops = [{Prop, Val} | Midprops],
			F = fun() ->
				mnesia:write(Rec#agent_auth{extended_props = Newprops})
			end,
			mnesia:transaction(F);
		_ ->
			{error, noagent}
	end.

-spec(drop_extended_prop/2 :: (Key :: {'login' | 'id', string()}, Prop :: atom()) -> {'atomic', 'ok'}).
drop_extended_prop({Type, Aval}, Prop) ->
	case get_agent(Type, Aval) of
		{atomic, [Rec]} ->
			Newprops = proplists:delete(Prop, Rec#agent_auth.extended_props),
			F = fun() ->
				mnesia:write(Rec#agent_auth{extended_props = Newprops})
			end,
			mnesia:transaction(F);
		_ ->
			{error, noagent}
	end.

%% @doc Get an extened property either from the database or a record 
%% directly.
-spec(get_extended_prop/2 :: (Key :: {'login' | 'id', string()}, Prop :: atom()) -> {'ok', any()} | {'error', 'noagent'} | 'undefined').
get_extended_prop({Type, Aval}, Prop) ->
	case get_agent(Type, Aval) of
		{atomic, [Rec]} ->
			get_extended_prop(Rec, Prop);
		_ ->
			{error, noagent}
	end;
get_extended_prop(#agent_auth{extended_props = Props}, Prop) ->
	case proplists:get_value(Prop, Props) of
		undefined -> undefined;
		Else -> {ok, Else}
	end.

%% @doc Utility function to handle merging data after a net split.  Takes the 
%% given nodes, selects all records with a timestamp greater than the given 
%% time, merges them, and passes the resulting list back to Pid.  Best if used
%% inside a spawn.
-spec(merge/3 :: (Nodes :: [atom()], Time :: pos_integer(), Replyto :: pid()) -> 'ok' | {'error', any()}).
merge(Nodes, Time, Replyto) ->
	Auths = merge_results(query_nodes(Nodes, Time, query_agent_auth)),
	Profs = merge_results(query_nodes(Nodes, Time, query_profiles)),
	Rels = merge_results(query_nodes(Nodes, Time, query_release)),
%	Auths = merge_agent_auth(Nodes, Time),
%	Profs = merge_profiles(Nodes, Time),
%	Rels = merge_release(Nodes, Time),
	Recs = lists:append([Auths, Profs, Rels]),
	Replyto ! {merge_complete, agent_auth, Recs},
	ok.

-spec(query_agent_auth/1 :: (Time :: pos_integer()) -> {'atomic', [#agent_auth{}]}).
query_agent_auth(Time) ->
	F = fun() ->
		QH = qlc:q([Auth || Auth <- mnesia:table(agent_auth), Auth#agent_auth.timestamp >= Time]),
		qlc:e(QH)
	end,
	mnesia:transaction(F).

-spec(query_profiles/1 :: (Time :: pos_integer()) -> {'atomic', [#agent_profile{}]}).
query_profiles(Time) ->
	F = fun() ->
		QH = qlc:q([Prof || Prof <- mnesia:table(agent_profile), Prof#agent_profile.timestamp >= Time]),
		qlc:e(QH)
	end,
	mnesia:transaction(F).

-spec(query_release/1 :: (Time :: pos_integer()) -> {'atomic', [#release_opt{}]}).
query_release(Time) ->
	F = fun() ->
		QH = qlc:q([Rel || Rel <- mnesia:table(release_opt), Rel#release_opt.timestamp >= Time]),
		qlc:e(QH)
	end,
	mnesia:transaction(F).

%merge_agent_auth(Nodes, Time) ->
%	?DEBUG("Staring merge.  Nodes:  ~p.  Time:  ~B", [Nodes, Time]),
%	F = fun() ->
%		QH = qlc:q([Auth || Auth <- mnesia:table(agent_auth), Auth#agent_auth.timestamp >= Time]),
%		qlc:e(QH)
%	end,
%	merge_results(query_nodes(Nodes, F)).

merge_results(Res) ->
	?DEBUG("Merging:  ~p", [Res]),
	merge_results_loop([], Res).

merge_results_loop(Return, []) ->
	?DEBUG("Merge complete:  ~p", [Return]),
	Return;
merge_results_loop(Return, [{atomic, List} | Tail]) ->
	Newreturn = diff_recs(Return, List),
	merge_results_loop(Newreturn, Tail).

%merge_profiles(Nodes, Time) ->
%	F = fun() ->
%		QH = qlc:q([Prof || Prof <- mnesia:table(agent_profile), Prof#agent_profile.timestamp >= Time]),
%		qlc:e(QH)
%	end,
%	merge_results(query_nodes(Nodes, F)).
%
%merge_release(Nodes, Time) ->
%	F = fun() ->
%		QH = qlc:q([Rel || Rel <- mnesia:table(release_opt), Rel#release_opt.timestamp >= Time]),
%		qlc:e(QH)
%	end,
%	merge_results(query_nodes(Nodes, F)).

query_nodes(Nodes, Time, Func) ->
	query_nodes(Nodes, Time, Func, []).

query_nodes([], _, _, Acc) ->
	?DEBUG("Full acc:  ~p", [Acc]),
	Acc;
query_nodes([Node | Tail], Time, Func, Acc) ->
	Newacc = case rpc:call(Node, agent_auth, Func, [Time]) of
		{atomic, Rows} = Rez ->
			?DEBUG("Node ~w got rows ~p", [Node, Rows]),
			[Rez | Acc];
		Else ->
			?WARNING("unable to get rows during merge for node ~w due to ~p", [Node, Else]),
			Acc
	end,
	query_nodes(Tail, Time, Func, Newacc).



%query_nodes(Nodes, Fun) ->
%	query_nodes(Nodes, Fun, []).
%
%query_nodes([], _Fun, Acc) ->
%	?DEBUG("Full acc:  ~p", [Acc]),
%	Acc;
%query_nodes([Node | Tail], Fun, Acc) ->
%	Newacc = case rpc:call(Node, mnesia, transaction, [Fun]) of
%		{atomic, Rows} = Rez ->
%			?DEBUG("Node ~w Got the following rows:  ~p", [Node, Rows]),
%			[Rez | Acc];
%		_Else ->
%			?WARNING("Unable to get rows during merge for node ~w", [Node]),
%			Acc
%	end,
%	query_nodes(Tail, Fun, Newacc).

%% @doc Take the plaintext username and password and attempt to 
%% authenticate the agent.
-type(profile_name() :: string()).
-spec(auth/2 :: (Username :: string(), Password :: string()) -> 'deny' | {'allow', string(), skill_list(), security_level(), profile_name()}).
auth(Username, Password) ->
	Extended = case get_agent(Username) of
		{atomic, [Rec]} ->
			Rec#agent_auth.extended_props;
		_Else ->
			[]
	end,
	try integration:agent_auth(Username, Password, Extended) of
		deny ->
			?INFO("integration denial for ~p", [Username]),
			%destroy(Username),
			deny;
		destroy ->
			destroy(Username),
			deny;
		{ok, Id, Profile, Security, Newextended} ->
			?INFO("integration allow for ~p", [Username]),
			cache(Id, Username, Password, Profile, Security, Newextended),
			local_auth(Username, Password);
		{error, nointegration} ->
			?INFO("No integration, local authing ~p", [Username]),
			local_auth(Username, Password)
	catch
		throw:{badreturn, Err} ->
			?WARNING("Integration gave a bad return of ~p", [Err]),
			local_auth(Username, Password)
	end.

%% @doc Starts mnesia and creates the tables.  If the tables already exist,
%% returns `ok'.  Otherwise, a default username of `"agent"' is stored 
%% with password `"Password123"' and skill `[english]'.
-spec(build_tables/0 :: () -> 'ok').
build_tables() ->
	?DEBUG("building tables...", []),
%	Nodes = lists:append([[node()], nodes()]),
	A = util:build_table(agent_auth, [
				{attributes, record_info(fields, agent_auth)},
				{disc_copies, [node()]}
			]),
	case A of
		{atomic, ok} ->
			F = fun() ->
				mnesia:write(#agent_auth{id = "1", login="agent", password=util:bin_to_hexstr(erlang:md5("Password123")), skills=[english], profile="Default"}),
				mnesia:write(#agent_auth{id = "2", login="administrator", password=util:bin_to_hexstr(erlang:md5("Password123")), securitylevel=admin, skills=[english], profile="Default"})
			end,
			case mnesia:transaction(F) of
				{atomic, ok} -> 
					ok;
				Else -> 
					Else
			end;
		_Else when A =:= copied; A =:= exists ->
			ok;
		_Else -> 
			A
	end,
	B = util:build_table(release_opt, [
		{attributes, record_info(fields, release_opt)},
		{disc_copies, [node()]}
	]),
	case B of
		{atomic, ok} ->
			ok;
		_Else2 when B =:= copied; B =:= exists ->
			ok;
		_Else2 ->
			B
	end,
	C = util:build_table(agent_profile, [
		{attributes, record_info(fields, agent_profile)},
		{disc_copies, [node()]}
	]),
	case C of
		{atomic, ok} -> 
			G = fun() ->
				mnesia:write(?DEFAULT_PROFILE)
			end,
			case mnesia:transaction(G) of
				{atomic, ok} -> 
					ok;
				Else2 -> 
					Else2
			end;
		_Else3 when C =:= copied; C =:= exists ->
			ok;
		_Else3 ->
			C
	end.

upgrade_v1_table() ->
	NewAttributes = [id, login, password, skills, securitylevel, integrated,
		profile, firstname, lastname, endpoints, extended_props, timestamp],
	% old attributes: [id, login, password, skills, securitylevel,
	%    integrated, profile, firstname, lastname, extended_props, timestamp
	mnesia:transform_table(agent_auth, fun upgrade_transform/1, NewAttributes).

upgrade_transform({agent_auth, Id, Login, Password, Skills, Security,
	Integrated, Profile, First, Last, ExProps, Timestamp}) ->
	{agent_auth, Id, Login, Password, Skills, Security, Integrated, Profile,
		First, Last, [], ExProps, Timestamp}.




%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @doc Caches the passed `Username', `Password', `Skills', and `Security' 
%% type.  to the mnesia database.  `Username' is the plaintext name and 
%% used as the key.  `Password' is assumed to be plaintext; will be 
%% erlang:md5'ed.  `Security' is either `agent', `supervisor', or `admin'.
%% @depricated Use {@link cache/2} instead.
-type(profile() :: string()).
-type(profile_data() :: {profile(), skill_list()} | profile() | skill_list()).
-spec(cache/6 ::	(Id :: string(), Username :: string(), Password :: string(), Profile :: profile_data(), Security :: 'agent' | 'supervisor' | 'admin', Extended :: [{atom(), any()}]) -> 
						{'atomic', 'ok'} | {'aborted', any()}).
cache(Id, Username, Password, {Profile, Skills}, Security, Extended) ->
	cache(Id, [
		{id, Id},
		{login, Username},
		{password, Password},
		{profile, Profile},
		{skills, Skills},
		{securitylevel, Security},
		{extended_props, Extended}
	]);
cache(Id, Username, Password, [Isskill | _Tail] = Skills, Security, Extended) when is_atom(Isskill); is_tuple(Isskill) ->
	case get_agent(id, Id) of
		{atomic, [Agent]} ->
			cache(Id, Username, Password, {Agent#agent_auth.profile, Skills}, Security, Extended);
		{atomic, []} ->
			cache(Id, Username, Password, {"Default", Skills}, Security, Extended)
	end;
cache(Id, Username, Password, Profile, Security, Extended) ->
	cache(Id, Username, Password, {Profile, []}, Security, Extended).

cache(Id, Props) ->
	F = fun() ->
		QH = qlc:q([A || A <- mnesia:table(agent_auth), A#agent_auth.id =:= Id]),
		Writerec = case qlc:e(QH) of
			[] ->
				Midrec = build_agent_record(Props, #agent_auth{}),
				Midrec#agent_auth{id = Id, integrated = util:now()};
			[Baserec] ->
				Midrec = build_agent_record(Props, Baserec),
				Midrec#agent_auth{id = Id, integrated = util:now()}
		end,
		mnesia:write(Writerec)
	end,
	Out = mnesia:transaction(F),
	?DEBUG("Cache username result:  ~p", [Out]),
	Out.
	
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

%% @private 
% Checks the `Username' and prehashed `Password' using the given `Salt' for the cached password.
% internally called by the auth callback; there should be no need to call this directly (aside from tests).
-spec(local_auth/2 :: (Username :: string(), Password :: string()) -> {'allow', string(), skill_list(), security_level(), profile_name()} | 'deny').
local_auth(Username, BasePassword) -> 
	Password = util:bin_to_hexstr(erlang:md5(BasePassword)),
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(agent_auth), X#agent_auth.login =:= Username, X#agent_auth.password =:= Password]),
		qlc:e(QH)
	end,
	case mnesia:transaction(F) of
		{atomic, [Agent]} when is_record(Agent, agent_auth) ->
			?DEBUG("Auth is coolbeans for ~p", [Username]),
			Skills = lists:umerge(lists:sort(Agent#agent_auth.skills), lists:sort(['_agent', '_node'])),
			{allow, Agent#agent_auth.id, Skills, Agent#agent_auth.securitylevel, Agent#agent_auth.profile};
		Else ->
			?DEBUG("Denying auth due to ~p", [Else]),
			deny
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

diff_recs(Left, Right) ->
	Sort = fun(A, B) when is_record(A, agent_auth) ->
			A#agent_auth.id < B#agent_auth.id;
		(A, B) when is_record(A, release_opt) ->
			A#release_opt.label < B#release_opt.label;
		(A, B) when is_record(A, agent_profile) ->
			A#agent_profile.name < B#agent_profile.name
	end,
	Sleft = lists:sort(Sort, Left),
	Sright = lists:sort(Sort, Right),
	diff_recs_loop(Sleft, Sright, []).

diff_recs_loop([], [], Acc) ->
	lists:reverse(Acc);
diff_recs_loop([_H | _T] = Left, [], Acc) ->
	lists:append(lists:reverse(Acc), Left);
diff_recs_loop([], [_H | _T] = Right, Acc) ->
	lists:append(lists:reverse(Acc), Right);
diff_recs_loop([Lhead | LTail] = Left, [Rhead | Rtail] = Right, Acc) ->
	case nom_equal(Lhead, Rhead) of
		true ->
			case timestamp_comp(Lhead, Rhead) of
				false ->
					diff_recs_loop(LTail, Rtail, [Lhead | Acc]);
				true ->
					diff_recs_loop(LTail, Rtail, [Rhead | Acc])
			end;
		false ->
			case nom_comp(Lhead, Rhead) of
				true ->
					diff_recs_loop(LTail, Right, [Lhead | Acc]);
				false ->
					diff_recs_loop(Left, Rtail, [Rhead | Acc])
			end
	end.
	
nom_equal(A, B) when is_record(A, agent_auth) ->
	A#agent_auth.id =:= B#agent_auth.id;
nom_equal(A, B) when is_record(A, release_opt) ->
	B#release_opt.label =:= A#release_opt.label;
nom_equal(A, B) when is_record(A, agent_profile) ->
	A#agent_profile.name =:= B#agent_profile.name.
	
nom_comp(A, B) when is_record(A, agent_auth) ->
	A#agent_auth.id < B#agent_auth.id;
nom_comp(A, B) when is_record(A, release_opt) ->
	A#release_opt.label < B#release_opt.label;
nom_comp(A, B) when is_record(A, agent_profile) ->
	A#agent_profile.name < B#agent_profile.name.

timestamp_comp(A, B) when is_record(A, agent_auth) ->
	A#agent_auth.timestamp < B#agent_auth.timestamp;
timestamp_comp(A, B) when is_record(B, release_opt) ->
	A#release_opt.timestamp < B#release_opt.timestamp;
timestamp_comp(A, B) when is_record(A, agent_profile) ->
	A#agent_profile.timestamp < B#agent_profile.timestamp.

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
		meck:expect(somestore, get_agents, 0, 
			{ok, [#agent_auth{id="1", login="ali"},
			#agent_auth{id="2", login="baba"}]}),
		
		meck:expect(somestore, get_agents2, 0, 
			{ok, [#agent_auth{id="3", login="mama"}]}),

		cpx_hooks:set_hook(b, get_agents, somestore, get_agents2, [], 5),
		
		?assertEqual([#agent_auth{id="1", login="ali"},
			#agent_auth{id="2", login="baba"},
			#agent_auth{id="3", login="mama"}], get_agents())
	end},
	{"id conflict", fun() ->
		meck:expect(somestore, get_agents, 0, 
			{ok, [#agent_auth{id="1", login="ali"},
			#agent_auth{id="2", login="baba"}]}),
		
		meck:expect(somestore, get_agents2, 0, 
			{ok, [#agent_auth{id="1", login="mama"}]}),

		cpx_hooks:set_hook(b, get_agents, somestore, get_agents2, [], 5),
		
		?assertEqual([#agent_auth{id="1", login="ali"},
			#agent_auth{id="2", login="baba"}], get_agents())
	end},
	{"login conflict", fun() ->
		meck:expect(somestore, get_agents, 0, 
			{ok, [#agent_auth{id="1", login="ali"},
			#agent_auth{id="2", login="baba"}]}),
		
		meck:expect(somestore, get_agents2, 0, 
			{ok, [#agent_auth{id="3", login="ali"}]}),

		cpx_hooks:set_hook(b, get_agents, somestore, get_agents2, [], 5),
		
		?assertEqual([#agent_auth{id="1", login="ali"},
			#agent_auth{id="2", login="baba"}], get_agents())
	end}]}.

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
	[{"no hook", fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(get_agent),

		?assertEqual({atomic, []}, get_agent("noone")),
		?assertEqual({atomic, []}, get_agent(login, "noone")),
		?assertEqual({atomic, []}, get_agent(id, "1"))
	end},
	{"with hooks", fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(get_agent),
		cpx_hooks:set_hook(a, get_agent, somestore, get_agent1, [], 2),
		cpx_hooks:set_hook(b, get_agent, somestore, get_agent2, [], 1),

		Agent = #agent_auth{id="1", login="ali"},

		meck:new(somestore),
		meck:expect(somestore, get_agent1, fun(_) -> none end),
		meck:expect(somestore, get_agent2, fun("ali") -> {ok, Agent} end),
		meck:expect(somestore, get_agent2, fun(login, "ali") -> {ok, Agent};
			(id, "1") -> {ok, Agent};
			(_, _) -> none
		end),

		?assertEqual({atomic, [Agent]}, get_agent("ali")),
		?assertEqual({atomic, [Agent]}, get_agent(login, "ali")),
		?assertEqual({atomic, [Agent]}, get_agent(id, "1")),		

		?assertEqual({atomic, []}, get_agent("kazam")),
		?assertEqual({atomic, []}, get_agent(login, "kazam")),
		?assertEqual({atomic, []}, get_agent(id, "2")),

		?assert(meck:validate(somestore)),
		meck:unload(somestore)
	end}].

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

diff_recs_test_() ->
	[{"agent_auth records",
	fun() ->
		Left = [
			#agent_auth{id = "A", login = "A", timestamp = 1},
			#agent_auth{id = "B", login = "B", timestamp = 3},
			#agent_auth{id = "C", login = "C", timestamp = 5}
		],
		Right = [
			#agent_auth{id = "A", login = "A", timestamp = 5},
			#agent_auth{id = "B", login = "B", timestamp = 3},
			#agent_auth{id = "C", login = "C", timestamp = 1}
		],
		Expected = [
			#agent_auth{id = "A", login = "A", timestamp = 5},
			#agent_auth{id = "B", login = "B", timestamp = 3},
			#agent_auth{id = "C", login = "C", timestamp = 5}
		],
		?assertEqual(Expected, diff_recs(Left, Right))
	end},
	{"release_opts records",
	fun() ->
		Left = [
			#release_opt{label = "A", timestamp = 1},
			#release_opt{label = "B", timestamp = 3},
			#release_opt{label = "C", timestamp = 5}
		],
		Right = [
			#release_opt{label = "A", timestamp = 5},
			#release_opt{label = "B", timestamp = 3},
			#release_opt{label = "C", timestamp = 1}
		],
		Expected = [
			#release_opt{label = "A", timestamp = 5},
			#release_opt{label = "B", timestamp = 3},
			#release_opt{label = "C", timestamp = 5}
		],
		?assertEqual(Expected, diff_recs(Left, Right))
	end},
	{"agent_prof records",
	fun() ->
		Left = [
			#agent_profile{name = "A", timestamp = 1},
			#agent_profile{name = "B", timestamp = 3},
			#agent_profile{name = "C", timestamp = 5}
		],
		Right = [
			#agent_profile{name = "A", timestamp = 5},
			#agent_profile{name = "B", timestamp = 3},
			#agent_profile{name = "C", timestamp = 1}
		],
		Expected = [
			#agent_profile{name = "A", timestamp = 5},
			#agent_profile{name = "B", timestamp = 3},
			#agent_profile{name = "C", timestamp = 5}
		],
		?assertEqual(Expected, diff_recs(Left, Right))
	end},
	{"3 way merge",
	fun() ->
		One = [
			#agent_auth{id = "A", login = "A", timestamp = 1},
			#agent_auth{id = "B", login = "B", timestamp = 3}
		],
		Two = [
			#agent_auth{id = "B", login = "B", timestamp = 3},
			#agent_auth{id = "C", login = "C", timestamp = 5}
		],
		Three = [
			#agent_auth{id = "A", login = "A", timestamp = 5},
			#agent_auth{id = "C", login = "C", timestamp = 1}
		],
		Expected = [
			#agent_auth{id = "A", login = "A", timestamp = 5},
			#agent_auth{id = "B", login = "B", timestamp = 3},
			#agent_auth{id = "C", login = "C", timestamp = 5}
		],
		?assertEqual(Expected, merge_results([{atomic, One}, {atomic, Two}, {atomic, Three}]))
	end}].

-endif.
