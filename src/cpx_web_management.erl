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

%% @doc The web management module.  Uses mochiweb for the heavy lifting.  Listens on port 9999 by default.
-module(cpx_web_management).
-author("Micah").

-define(PORT, 9999).
-define(WEB_DEFAULTS, [{name, ?MODULE}, {port, ?PORT}]).

-include("call.hrl").
-include("agent.hrl").
-include("queue.hrl").
-include_lib("stdlib/include/qlc.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start/0, stop/0, loop/1]).

% TODO configure this to start by default with a default configureation (cpx_supervisor_conf)
%% @doc Start the web management server unlinked to the parent process.
-spec(start/0 :: () -> {'ok', pid()}).
start() ->
	?CONSOLE("Staring mochiweb...", []),
	ets:new(cpx_management_logins, [set, public, named_table]),
	mochiweb_http:start([{loop, {?MODULE, loop}} | ?WEB_DEFAULTS]).

%% @doc Stops the web management.
-spec(stop/0 :: () -> 'ok').
stop() -> 
	ets:delete(cpx_management_logins),
	mochiweb_http:stop(?MODULE).

-spec(loop/1 :: (Req :: atom()) -> any()).
loop(Req) ->
	Path = Req:get(path),
	Post = Req:parse_post(),
	case parse_path(Path) of
		{file, {File, Docroot}} ->
			Cookies = Req:parse_cookie(),
			case check_cookie(Cookies) of
				badcookie ->
					Ref = erlang:ref_to_list(make_ref()),
					Cookie = io_lib:format("cpx_management=~p; path=/", [Ref]),
					ets:insert(cpx_management_logins, {Ref, undefined, undefined}),
					Req:serve_file(File, Docroot, [{"Set-Cookie", Cookie}]);
				{_Reflist, _Salt, _Login} ->
					Req:serve_file(File, Docroot)
			end;
		{api, Api} ->
			Out = api(Api, check_cookie(Req:parse_cookie()), Post),
			Req:respond(Out)
	end.

api(checkcookie, Cookie, _Post) ->
	case Cookie of
		badcookie ->
			Reflist = erlang:ref_to_list(make_ref()),
			NewCookie = io_lib:format("cpx_management=~p; path=/", [Reflist]),
			ets:insert(cpx_management_logins, {Reflist, undefined, undefined}),
			{200, [{"Set-Cookie", NewCookie}], mochijson2:encode({struct, [{<<"success">>, false}]})};
		{_Reflist, _Salt, undefined} ->
			{200, [], mochijson2:encode({struct, [{<<"success">>, false}]})};
		{_Reflist, _Salt, Login} ->
			{200, [], mochijson2:encode({struct, [{<<"success">>, true}, {<<"login">>, list_to_binary(Login)}]})}
	end;
api(Apirequest, badcookie, _Post) ->
	Reflist = erlang:ref_to_list(make_ref()),
	Cookie = io_lib:format("cpx_management=~p; path=/", [Reflist]),
	ets:insert(cpx_management_logins, {Reflist, undefined, undefined}),
	{403, [{"Set-Cookie", Cookie}], <<"Cookie reset, retry.">>};
api(getsalt, {Reflist, _Salt, Conn}, _Post) ->
	Newsalt = integer_to_list(crypto:rand_uniform(0, 4294967295)),
	ets:insert(cpx_management_logins, {Reflist, Newsalt, Conn}),
	{200, [], mochijson2:encode({struct, [{success, true}, {message, <<"Salt created, check salt property">>}, {salt, list_to_binary(Newsalt)}]})};
api(login, {_Reflist, undefined, _Login}, _Post) ->
	{200, [], mochijson2:encode({struct, [{success, false}, {message, <<"No salt set">>}]})};
api(login, {Reflist, Salt, _Login}, Post) ->
	Username = proplists:get_value("username", Post, ""),
	Password = proplists:get_value("password", Post, ""),
	case agent_auth:auth(Username, Password, Salt) of
		deny ->
			{200, [], mochijson2:encode({struct, [{success, false}, {message, <<"login err">>}]})};
		{allow, _Skills, admin} ->
			ets:insert(cpx_management_logins, {Reflist, Salt, Username}),
			{200, [], mochijson2:encode({struct, [{success, true}, {message, <<"logged in">>}]})};
		{allow, _Skills, _Security} ->
			{200, [], mochijson2:encode({struct, [{success, false}, {message, <<"login err">>}]})}
	end;
api(logout, {Reflist, _Salt, _Login}, _Post) ->
	ets:delete(cpx_management_logins, Reflist),
	{200, [], mochijson2:encode({struct, [{success, true}]})};
	
api({agents, "modules", "update"}, {_Reflist, _Salt, _Login}, Post) ->
	Tcpout = case proplists:get_value("agentModuleTCPListen", Post) of
		undefined ->
			cpx_supervisor:destroy(agent_tcp_listener),
			{struct, [{success, true}, {<<"message">>, <<"TCP Server disabled">>}]};
		Tcpport ->
			try list_to_integer(Tcpport) of
				N when N >= 1024, N =< 49151 ->
					cpx_supervisor:update_conf(agent_tcp_listener, agent_tcp_listener, start_link, [N]),
					{struct, [{success, true}, {<<"message">>, <<"TCP Server enabled">>}]};
				_N ->
					{struct, [{success, false}, {<<"message">>, <<"Listen port out of range">>}]}
			catch
				error:badarg ->
					{struct, [{success, false}, {<<"message">>, <<"Listen port not a number">>}]}
			end
	end,
	Webout = case proplists:get_value("agentModuleWebListen", Post) of
		undefined ->
			cpx_supervisor:destroy(agent_web_listener),
			{struct, [{success, true}, {<<"message">>, <<"Web Server disabled">>}]};
		Webport ->
			try list_to_integer(Webport) of
				M when M >= 1024, M =< 49151 ->
					cpx_supervisor:update_conf(agent_web_listener, agent_web_listener, start_link, [M]),
					{struct, [{success, true}, {<<"message">>, <<"Web Server enabled">>}]};
				_M ->
					{struct, [{success, false}, {<<"message">>, <<"Listen port out of range">>}]}
			catch
				error:badarg ->
					{struct, [{success, false}, {<<"message">>, <<"Listen port not a number">>}]}
			end
	end,
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"results">>, [Tcpout, Webout]}]})};
api({agents, "modules", "get"}, {_Reflist, _Salt, _Login}, _Post) ->
	Tcpout = case cpx_supervisor:get_conf(agent_tcp_listener) of
		undefined ->
			[{"agentModuleTCPListen", 1337}, {"agentModuleTCPListenEnabled", false}];
		Tcplist ->
			[Tcport] = proplists:get_value(start_args, Tcplist, [1337]),
			[{"agentModuleTCPListen", Tcport}, {"agentModuleTCPListenEnabled", true}]
	end,
	Webout = case cpx_supervisor:get_conf(agent_web_listener) of
		undefined ->
			[{"agentModuleWebListen", 5050}, {"agentModuleWebListenEnabled", false}];
		Weblist ->
			[Webport] = proplists:get_value(start_args, Weblist, [5050]),
			[{"agentModuleWebListen", Webport}, {"agentModuleWebListenEnabled", true}]
	end,
	Full = lists:append([Tcpout, Webout]),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"result">>, {struct, Full}}]})};
api({agents, "profiles", "get"}, {_Reflist, _Salt, _Login}, _Post) ->
	Profiles = agent_auth:get_profiles(),
	Foreachprofile = fun({Pname, Pskills}) ->
		Agents = agent_auth:get_agents(Pname),
		{struct, [{<<"name">>, list_to_binary(Pname)}, {<<"type">>, <<"profile">>}, {<<"skills">>, encode_skills(Pskills)}, {<<"agents">>, encode_agents(Agents)}]}
	end,
	Items = lists:map(Foreachprofile, Profiles),
	Json = {struct, [{success, true}, {<<"items">>, Items}]},
	{200, [], mochijson2:encode(Json)};
api({agents, "profiles", Profile, "getskills"}, {_Reflist, _Salt, _Login}, _Post) ->
	{_Profilename, Skillatoms} = agent_auth:get_profile(Profile),
	Encoded = encode_skills(Skillatoms),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"items">>, Encoded}]})};

api({agents, "profiles", "new"}, {_Reflist, _Salt, _Login}, Post) ->
	Skillatoms = lists:map(fun(Skill) -> call_queue_config:skill_exists(Skill) end, proplists:get_all_values("skills", Post)),
	agent_auth:new_profile(proplists:get_value("name", Post), Skillatoms),
	{200, [], mochijson2:encode({struct, [{success, true}]})};
api({agents, "profiles", "Default", "update"}, {_Reflist, _Salt, _Login}, Post) ->
	case proplists:get_value("name", Post) of
		undefined ->
			Skillatoms = lists:map(fun(Skill) -> call_queue_config:skill_exists(Skill) end, proplists:get_all_values("skills", Post)),
			agent_auth:set_profile("Default", "Default", Skillatoms),
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		_Else ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Default is a protected profile and cannot be renamed">>}]})}
	end;
api({agents, "profiles", Profile, "update"}, {_Reflist, _Salt, _Login}, Post) ->
	Parseskills = fun(Skill) ->
		?CONSOLE("~p", [Skill]),
		case string:tokens(Skill, "{},") of
			["_brand", Brandname] ->
				{'_brand', Brandname};
			["_queue", Queuename] ->
				{'_queue', Queuename};
			[Skill] ->
				call_queue_config:skill_exists(Skill)
		end
	end,
	Skillatoms = lists:map(Parseskills, proplists:get_all_values("skills", Post)),
	agent_auth:set_profile(Profile, proplists:get_value("name", Post), Skillatoms),
	{200, [], mochijson2:encode({struct, [{success, true}]})};
api({agents, "profiles", "Default", "delete"}, {_Reflist, _Salt, _Login}, _Post) ->
	{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Default is a protected profile and cannot be deleted">>}]})};
api({agents, "profiles", Profile, "delete"}, {_Reflist, _Salt, _Login}, _Post) ->
	agent_auth:destroy_profile(Profile),
	{200, [], mochijson2:encode({struct, [{success, true}]})};
api({agents, "agents", Agent, "get"}, {_Reflist, _Salt, _Login}, _Post) ->
	{atomic, [Agentrec]} = agent_auth:get_agent(Agent),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"agent">>, encode_agent(Agentrec)}]})};

api({skills, "groups", "get"}, {_Reflist, _Salt, _Login}, _Post) ->
	Skills = call_queue_config:get_skills(),
	Proplist = dict:to_list(encode_skills_with_groups(Skills)),
	Convert = fun({Group, Skillrecs}) ->
		{struct, [{<<"name">>, list_to_binary(Group)}, {<<"type">>, <<"group">>}, {<<"skills">>, encode_skills(Skillrecs)}]}
	end,
	Json = {struct, [{success, true}, {<<"items">>, lists:map(Convert, Proplist)}]},
	{200, [], mochijson2:encode(Json)};
%api({skills, Profile}, {_Reflist, _Salt, _Login}, Post) ->
%	{_Profilename, Skillatoms} = agent_auth:get_profile(Profile),
%	Encoded = encode_skills(Skillatoms),
%	{200, [], mochijson2:encode({struct, [{success, true}, {<<"items">>, Encoded}]})};
api({skills, "skill", "_queue", "expand"}, {_Reflist, _Salt, _Login}, _Post) ->
	Queues = call_queue_config:get_queues(),
	F = fun(Qrec) ->
		list_to_binary(Qrec#call_queue.name)
	end,
	Converted = lists:map(F, Queues),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"items">>, Converted}]})};
api({skills, "skill", "_node", "expand"}, {_Reflist, _Salt, _Login}, _Post) ->
	Nodes = [node() | nodes()],
	F = fun(Atom) ->
		L = atom_to_list(Atom),
		list_to_binary(L)
	end,
	Converted = lists:map(F, Nodes),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"itmes">>, Converted}]})};
api({skills, "skill", "_agent", "expand"}, {_Reflist, _Salt, _Login}, _Post) ->
	Agents = agent_auth:get_agents(),
	F = fun(Arec) ->
		list_to_binary(Arec#agent_auth.login)
	end,
	Converted = lists:map(F, Agents),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"items">>, Converted}]})};
api({skills, "skill", "_brand", "expand"}, {_Reflist, _Salt, _Login}, _Post) ->
	Clients = call_queue_config:get_clients(),
	F = fun(Clientrec) ->
		list_to_binary(Clientrec#client.label)
	end,
	Converted = lists:map(F, Clients),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"items">>, Converted}]})}.

% path spec:
% /basiccommand
% /section/subsection/action
% /section/subsection/item/action
%
% So, this means to update the modules for agents:
% /agents/modules/update
% but to update an agent profile:
% /agents/profiles/profilename/update
parse_path(Path) ->
	case Path of
		"/" ->
			{file, {"index.html", "www/admin/"}};
		"/getsalt" ->
			{api, getsalt};
		"/login" ->
			{api, login};
		"/logout" ->
			{api, logout};
		"/checkcookie" ->
			{api, checkcookie};
		_Other ->
			% section/action (params in post data)
			case util:string_split(Path, "/") of
				["", "agents", "modules", Action] ->
					{api, {agents, "modules", Action}};
				["", "agents", "profiles", Action] ->
					{api, {agents, "profiles", Action}};
				["", "agents", "profiles", Profile, Action] ->
					{api, {agents, "profiles", Profile, Action}};
				["", "agents", "agents", Action] ->
					{api, {agents, "agents", Action}};
				["", "agents", "agents", Agent, Action] ->
					{api, {agents, "agents", Agent, Action}};
%				["", "agents", Action] ->
%					{api, {agents, Action}};
%				["", "agents", Profile, Action] ->
%					{api, {agents, Profile, Action}};
%				["", "skills", Profile] ->
%					{api, {skills, Profile}};
				["", "skills", "groups", Action] ->
					{api, {skills, "groups", Action}};
				["", "skills", "groups", Group, Action] ->
					{api, {skills, "groups", Group, Action}};
				["", "skills", "skill", Skill, Action] ->
					{api, {skills, "skill", Skill, Action}};
				["", "queues", Action] ->
					{api, {queues, Action}};
				["", "medias", Action] ->
					{api, {medias, Action}};
				_Allothers ->
					case filelib:is_regular(string:concat("www/admin", Path)) of
						true ->
							{file, {string:strip(Path, left, $/), "www/admin/"}};
						false ->
							{file, {string:strip(Path, left, $/), "www/contrib/"}}
					end
			end
	end.

check_cookie([]) ->
	badcookie;
check_cookie(Allothers) ->
	case proplists:get_value("cpx_management", Allothers) of
		undefined ->
			?CONSOLE("Cookie bad due to no cpx_managmenet.  ~p", [Allothers]),
			badcookie;
		Reflist ->
			case ets:lookup(cpx_management_logins, Reflist) of
				[] ->
					?CONSOLE("Cookie bad reflist not in ets.  ~p", [Allothers]),
					badcookie;
				[{Reflist, Salt, Login}] ->
					{Reflist, Salt, Login}
			end
	end.

encode_skill(Atom) when is_atom(Atom) ->
	Skill = call_queue_config:get_skill(Atom),
	encode_skill(Skill);
encode_skill({Atom, Value}) when is_atom(Atom), is_list(Value) ->
	encode_skill({Atom, list_to_binary(Value)});
encode_skill({Atom, Value}) ->
	Skill = call_queue_config:get_skill(Atom),
	{struct, [{name, list_to_binary(Skill#skill_rec.name)},
		{type, skill}, {atom, Skill#skill_rec.atom},
		{description, list_to_binary(Skill#skill_rec.description)},
		{protected, Skill#skill_rec.protected},
		{expanded, Value}]};
encode_skill(Skill) when is_record(Skill, skill_rec) ->
	{struct, [{name, list_to_binary(Skill#skill_rec.name)},
		{type, skill}, {atom, Skill#skill_rec.atom},
		{description, list_to_binary(Skill#skill_rec.description)},
		{protected, Skill#skill_rec.protected}]}.


encode_skills([]) ->
	[];
encode_skills([Skill|Skills]) ->
	[encode_skill(Skill) | encode_skills(Skills)].

encode_skills_with_groups([]) ->
	[];
encode_skills_with_groups(Skills) ->
	encode_skills_with_groups(Skills, dict:new()).

encode_skills_with_groups([], Acc) ->
	Acc;
encode_skills_with_groups([Skill | Skills], Acc) ->
	case dict:find(Skill#skill_rec.group, Acc) of
		error ->
			Sgroups = [];
		{ok, Sgroups} ->
			Sgroups
	end,
	Newacc = dict:store(Skill#skill_rec.group, [Skill | Sgroups], Acc),
	encode_skills_with_groups(Skills, Newacc).
	
%	ASkill = lists:nth(1, Group),
%	[{struct, [{name, list_to_binary(ASkill#skill_rec.group)},
%			{type, group},
%			{children, encode_skills(Group)}]} | encode_skills_with_groups(Groups)].

encode_queues([]) ->
	[];
encode_queues([Queue|Queues]) ->
	[{struct, [{name, list_to_binary(Queue#call_queue.name)},
			{type, queue}, {weight, Queue#call_queue.weight},
			{skills, Queue#call_queue.skills},
			{recipe, none}]} | encode_queues(Queues)].

encode_queues_with_groups([]) ->
	[];
encode_queues_with_groups([Group|Groups]) ->
	AQueue = lists:nth(1, Group),
	case call_queue_config:get_queue_group(AQueue#call_queue.group) of
		{atomic, [G]} ->
			[{struct, [{name, list_to_binary(G#queue_group.name)},
					{type, group}, {protected, G#queue_group.protected},
					{recipe, G#queue_group.recipe},
					{children, encode_queues(Group)}]} | encode_queues_with_groups(Groups)]
	end.

encode_agents([]) ->
	[];
encode_agents([Agent|Agents]) ->
	[encode_agent(Agent) | encode_agents(Agents)].

encode_agent(Agentrec) when is_record(Agentrec, agent_auth) ->
	{struct, [
		{name, list_to_binary(Agentrec#agent_auth.login)},
		{<<"type">>, <<"agent">>},
		{login, list_to_binary(Agentrec#agent_auth.login)},
		{skills, encode_skills(Agentrec#agent_auth.skills)},
		{securitylevel, Agentrec#agent_auth.securitylevel},
		{integrated, Agentrec#agent_auth.integrated},
		{profile, list_to_binary(Agentrec#agent_auth.profile)}
	]}.

encode_agents_with_profiles([]) ->
	dict:new();
encode_agents_with_profiles(Agents) ->
	encode_agents_with_profiles(Agents, dict:new()).

encode_agents_with_profiles([], Acc) ->
	Acc;
encode_agents_with_profiles([Agent | Agents], Acc) ->
	case dict:find(Agent#agent_auth.profile, Acc) of
		error ->
			Pagents = [];
		{ok, Pagents} ->
			Pagents
	end,
	Newacc = dict:store(Agent#agent_auth.profile, [Agent | Pagents], Acc),
	encode_agents_with_profiles(Agents, Newacc).

