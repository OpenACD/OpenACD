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

%% @private Take the request path and use that to determine what to return.
%% return data should be in json format whenever possible.
%% note that mochiweb only likes to handle numbers, lists, and atoms for it's json encode.
%% so be sure to massage the data befor attempting to ouput it.
%% more specically turn 'strings' to atoms
%%
%% The ok response tuple is of form:  {content-type, headers, body} or {content-type, body}
%% a raw response can be sent using respond.
%% it's tuple is {Http_response_code, [header()], body},
%% where header is {header_name, header_val}

%-spec(loop/3 :: (Req :: any(), Method :: string(), Path :: string()) -> any()).
%loop(Req, _Method, "/") -> 
%	Req:serve_file("index.html", "www/admin/");
%loop(Req, _Method, "/releaseoptions") ->
%	case agent_auth:get_releases() of
%		[] ->
%			Req:respond({200, [], mochijson2:encode({struct, [{success, false}, {message, <<"No release states">>}]})});
%		ReleaseOptions ->
%			Req:respond({200, [], mochijson2:encode({struct, [{label, label}, {identifier, label}, {items, lists:foldl(fun(X, A) -> [{struct, [{label, list_to_binary(X#release_opt.label)}, {id, X#release_opt.id}, {bias, X#release_opt.bias}]} | A] end, [], ReleaseOptions)}]})})
%	end;
%loop(Req, _Method, "/agents") ->
%	QH = qlc:q([X || X <- mnesia:table(agent_auth)]), % X#agent_auth.integrated =:= undefined]),
%	F = fun() -> qlc:e(QH) end,
%	case mnesia:transaction(F) of
%		{atomic, Agents} ->
%			GroupedAgents = util:group_by(fun(X) -> X#agent_auth.profile end, Agents),
%			Req:respond({200, [], mochijson2:encode({struct, [{label, name}, {identifier, name}, {items, encode_agents_with_profiles(GroupedAgents)}]})});
%		Else ->
%			Req:respond({200, [], mochijson2:encode({struct, [{success, false}, {message, <<"No agents">>}]})})
%	end;
%loop(Req, _Method, "/skills") ->
%	Skills = util:group_by(fun(X) -> X#skill_rec.group end, call_queue_config:get_skills()),
%	?CONSOLE("struct: ~p", [{struct, [{items, encode_skills_with_groups(Skills)}]}]),
%	Req:respond({200, [], mochijson2:encode({struct, [{label, name}, {identifier, name}, {items, encode_skills_with_groups(Skills)}]})});
%loop(Req, _Method, "/setskill") ->
%	Post = Req:parse_post(),
%	case proplists:get_value("action", Post) of
%		"set" ->
%			Stratom = proplists:get_value("skillatom", Post),
%			case call_queue_config:get_skill(Stratom) of
%				undefined ->
%					Req:respond({200, [], mochijson2:encode({struct, [{success, false}, {message, <<"Skill atom mismatch">>}]})});
%				Skillrec when is_record(Skillrec, skill_rec) ->
%					Newrec = #skill_rec{
%						atom = Skillrec#skill_rec.atom, 
%						name = proplists:get_value("skillname", Post), 
%						description = proplists:get_value("skilldesc", Post),
%						group = Skillrec#skill_rec.group},
%					call_queue_config:set_skill(Skillrec#skill_rec.atom, Newrec),
%					Req:respond({200, [], mochijson2:encode({struct, [{success, true}, {message, <<"Skill updated">>}]})})
%			end
%	end;
%loop(Req, _Method, "/queues") ->
%	Queues = util:group_by(fun(X) -> X#call_queue.group end, call_queue_config:get_queues()),
%	Req:respond({200, [], mochijson2:encode({struct, [{label, name}, {identifier, name}, {items, encode_queues_with_groups(Queues)}]})});
%loop(Req, _Method, "/web_dump") -> 
%	Req:ok({"text/html",io_lib:format("<pre>~p</pre>~n", [Req:dump()])});
%loop(Req, _Method, "/set_cookie") -> 
%	% TODO hardcoded cookie is hardcoded.
%	Req:respond({200, [{"Set-Cookie", "goober=foobar"}], io_lib:format("<pre>~p~p</pre>", [Req:dump(), Req:parse_cookie()])});
%loop(Req, _Method, Path) ->
%	?CONSOLE("path requested: ~p", [Path]),
%	% strip the leading / if any, and then check if it's a file in www/admin; if
%	% it's not then try blindly in www/contrib
%	RPath = string:strip(Path, left, $/),
%	case filelib:is_regular(string:concat("www/admin/", RPath)) of
%		true ->
%			Req:serve_file(RPath, "www/admin/");
%		false ->
%			Req:serve_file(RPath, "www/contrib/")
%	end.
%

-spec(loop/1 :: (Req :: atom()) -> any()).
loop(Req) ->
	Path = Req:get(path),
	Post = Req:parse_post(),
	case parse_path(Path) of
		{file, {File, Docroot}} ->
			Cookies = Req:parse_cookie(),
			case proplists:get_value("cpx_management", Cookies) of
				undefined ->
					Ref = erlang:ref_to_list(make_ref()),
					Cookie = io_lib:format("cpx_management=~p", [Ref]),
					ets:insert(cpx_management_logins, {Ref, undefined, undefined}),
					?CONSOLE("Setting cookie and serving file ~p", [string:concat(Docroot, File)]),
					Req:serve_file(File, Docroot, [{"Set-Cookie", Cookie}]);
				_Reflist ->
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
			NewCookie = io_lib:format("cpx_management=~p", [Reflist]),
			ets:insert(cpx_management_logins, {Reflist, undefined, undefined}),
			{200, [{"Sect-Cookie", NewCookie}], mochijson2:encode({struct, [{<<"success">>, false}]})};
		{_Reflist, _Salt, undefined} ->
			{200, [], mochijson2:encode({struct, [{<<"success">>, false}]})};
		{_Reflist, _Salt, Login} ->
			{200, [], mochijson2:encode({struct, [{<<"success">>, true}, {<<"login">>, list_to_binary(Login)}]})}
	end;
api(_Apirequest, badcookie, _Post) ->
	Reflist = erlang:ref_to_list(make_ref()),
	Cookie = io_lib:format("cpx_management=~p", [Reflist]),
	ets:insert(cpx_management_logins, {Reflist, undefined, undefined}),
	{403, [{"Set-Cookie", Cookie}], <<"Cookie reset, retry.">>};
api(getsalt, {Reflist, _Salt, Conn}, _Post) ->
	Newsalt = integer_to_list(crypto:rand_uniform(0, 4294967295)),
	ets:insert(cpx_management_logins, {Reflist, Newsalt, Conn}),
	?CONSOLE("created and sent salt for ~p", [Reflist]),
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
api(_Api, _Cookie, _Post) ->
	{200, [], mochijson2:encode({struct, [{success, false}, {message, <<"login requried">>}]})}.

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
				["", "skill", Action] ->
					{api, {skill, Action}};
				["", "agents", Action] ->
					{api, {agents, Action}};
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
			badcookie;
		Reflist ->
			case ets:lookup(cpx_management_logins, Reflist) of
				[] ->
					badcookie;
				[{Reflist, Salt, Login}] ->
					{Reflist, Salt, Login}
			end
	end.

encode_skills([]) ->
	[];
encode_skills([Skill|Skills]) ->
	[{struct, [{name, list_to_binary(Skill#skill_rec.name)},
			{type, skill}, {atom, Skill#skill_rec.atom},
			{description, list_to_binary(Skill#skill_rec.description)},
			{protected, Skill#skill_rec.protected}]} | encode_skills(Skills)].

encode_skills_with_groups([]) ->
	[];
encode_skills_with_groups([Group|Groups]) ->
	ASkill = lists:nth(1, Group),
	[{struct, [{name, list_to_binary(ASkill#skill_rec.group)},
			{type, group},
			{children, encode_skills(Group)}]} | encode_skills_with_groups(Groups)].

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
	[{struct, [{name, list_to_binary(Agent#agent_auth.login)},
			{type, agent}, {securitylevel, Agent#agent_auth.securitylevel},
			{skills, Agent#agent_auth.skills}]} | encode_agents(Agents)].

encode_agents_with_profiles([]) ->
	[];
encode_agents_with_profiles([Profile|Profiles]) ->
	AnAgent = lists:nth(1, Profile),
	{Name, Skills} = agent_auth:get_profile(AnAgent#agent_auth.profile),
	[{struct, [{name, list_to_binary(Name)},
			{type, profile}, 
			{skills, Skills},
			{children, encode_agents(Profile)}]} | encode_agents_with_profiles(Profiles)].

