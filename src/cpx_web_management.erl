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
%%	Micah Warren <micahw at fusedsolutions dot com>
%%

%% @doc The web management module.  Uses mochiweb for the heavy lifting.  Listens on port 9999 by default.
-module(cpx_web_management).
-author("Micah").

-ifdef(TEST).
-define(PORT, 59999).
-else.
-define(PORT, 9999).
-endif.
-define(WEB_DEFAULTS, [{name, ?MODULE}, {port, ?PORT}]).
-define(COOKIE, {_Reflist, _Salt, _Login}).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("queue.hrl").
-include("cpx.hrl").
-include("smtp.hrl").
-include_lib("stdlib/include/qlc.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/0, start_link/1, start/0, start/1, stop/0, loop/1]).

-export([
	encode_skill/1,
	encode_skills/1,
	encode_queue/1,
	encode_queues/1,
	encode_agent/1,
	encode_agents/1
]).

-type(simple_json() :: {'struct', [{atom() | binary(), atom() | binary()}]}).

%% @doc Start the web management server unlinked to the parent process.
-spec(start/0 :: () -> {'ok', pid()}).
start() ->
	start(?PORT).

-spec(start/1 :: (Port :: pos_integer()) -> {'ok', pid()}).
start(Port) ->
	?DEBUG("Starting mochiweb...", []),
	case ets:info(cpx_management_logins) of
		undefined ->
			ets:new(cpx_management_logins, [set, public, named_table]);
		Else when is_list(Else) ->
			?DEBUG("looks like the table exists already", []),
			ok
	end,
	mochiweb_http:start([{loop, {?MODULE, loop}}, {name, ?MODULE}, {port, Port}]).

-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() ->
	start_link(?PORT).

-spec(start_link/1 :: (Port :: pos_integer()) -> {'ok', pid()}).
start_link(Port) ->
	{ok, Pid} = Out = start(Port),
	link(Pid),
	Out.

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
					% now would be a good time to determine the language.
					Language = case Req:get_header_value("Accept-Language") of
						undefined ->
							"";
						Langlist ->
							determine_language(util:string_split(Langlist, ","))
					end,
					Langcookie = io_lib:format("cpx_lang=~s", [Language]),
					Req:serve_file(File, Docroot, [{"Set-Cookie", Cookie},{"Set-Cookie", Langcookie}]);
				{_Reflist, _Salt, _Login} ->
					Req:serve_file(File, Docroot)
			end;
		{api, Api} ->
			Out = api(Api, check_cookie(Req:parse_cookie()), Post),
			Req:respond(Out)
	end.

determine_language([]) ->
	"";
determine_language([Head | Tail]) ->
	[Lang |_Junk] = util:string_split(Head, ";"),
	case filelib:is_regular(string:concat(string:concat("www/admin/lang/nls/", Lang), "/labels.js")) of
		true ->
			Lang;
		false ->
			% try the "super language" (eg en vs en-us) in case it's not in the list itself
			[SuperLang | _SubLang] = util:string_split(Lang, "-"),
			case filelib:is_regular(string:concat(string:concat("www/admin/lang/nls/", SuperLang), "/labels.js")) of
				true ->
					SuperLang;
				false ->
					determine_language(Tail)
			end
	end.

%% =====
%% General requests
%% =====
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
api(_Apirequest, badcookie, _Post) ->
	Reflist = erlang:ref_to_list(make_ref()),
	Cookie = io_lib:format("cpx_management=~p; path=/", [Reflist]),
	ets:insert(cpx_management_logins, {Reflist, undefined, undefined}),
	{403, [{"Set-Cookie", Cookie}], <<"Cookie reset, retry.">>};
api(getsalt, {Reflist, _Salt, Login}, _Post) ->
	Newsalt = integer_to_list(crypto:rand_uniform(0, 4294967295)),
	ets:insert(cpx_management_logins, {Reflist, Newsalt, Login}),
	[E, N] = get_pubkey(),
	PubKey = {struct, [{<<"E">>, list_to_binary(erlang:integer_to_list(E, 16))}, {<<"N">>, list_to_binary(erlang:integer_to_list(N, 16))}]},
	{200, [], mochijson2:encode({struct, [{success, true}, {message, <<"Salt created, check salt property">>}, {salt, list_to_binary(Newsalt)}, {pubkey, PubKey}]})};
api(login, {_Reflist, undefined, _Conn}, _Post) ->
	{200, [], mochijson2:encode({struct, [{success, false}, {message, <<"No salt set">>}]})};
api(login, {Reflist, Salt, _Login}, Post) ->
	Username = proplists:get_value("username", Post, ""),
	Password = proplists:get_value("password", Post, ""),
	try decrypt_password(Password) of
		Decrypted ->
			Salt = string:substr(Decrypted, 1, length(Salt)),
			DecryptedPassword = string:substr(Decrypted, length(Salt) + 1),
			%Salted = util:bin_to_hexstr(erlang:md5(string:concat(Salt, util:bin_to_hexstr(erlang:md5(DecryptedPassword))))),
			case agent_auth:auth(Username, DecryptedPassword) of
				deny ->
					{200, [], mochijson2:encode({struct, [{success, false}, {message, <<"Authentication failed">>}]})};
				{allow, _id, _Skills, admin, _Profile} ->
					ets:insert(cpx_management_logins, {Reflist, Salt, Username}),
					{200, [], mochijson2:encode({struct, [{success, true}, {message, <<"logged in">>}]})};
				{allow, _id, _Skills, _Security, _Profile} ->
					{200, [], mochijson2:encode({struct, [{success, false}, {message, <<"Authentication failed">>}]})}
			end
	catch
		error:decrypt_failed ->
			{200, [], mochijson2:encode({struct, [{success, false}, {message, list_to_binary("Password decryption failed")}]})}
	end;
api(logout, {Reflist, _Salt, _Login}, _Post) ->
	ets:delete(cpx_management_logins, Reflist),
	%Newref = erlang:ref_to_list(make_ref()),
	Cookie = io_lib:format("cpx_management=~p; path=/; Expires=Tue, 29-Mar-2005 19:30: 42 GMT; Max-Age=86400", [Reflist]),
	%Cookie = io_lib:format("cpx_management=~p; path=/", [Newref]),
	ets:insert(cpx_management_logins, {Reflist, undefined, undefined}),
	{200, [{"Set-Cookie", Cookie}], mochijson2:encode({struct, [{success, true}]})};
api(_Api, {_Reflist, _Salt, undefined}, _Post) ->
	{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"login required">>}]})};
	
%% =====
%% agents -> modules
%% =====
api({agents, "modules", "update"}, ?COOKIE, Post) ->
	Tcpout = case proplists:get_value("agentModuleTCPListen", Post) of
		undefined ->
			cpx_supervisor:destroy(agent_tcp_listener),
			{struct, [{success, true}, {<<"message">>, <<"TCP Server disabled">>}]};
		Tcpport ->
			{OldTcpPort, _Action} = case cpx_supervisor:get_conf(agent_tcp_listener) of
				TcpRecord when is_record(TcpRecord, cpx_conf) ->
					{lists:nth(1, TcpRecord#cpx_conf.start_args), update};
				_Else1 ->
					{undefined, add}
			end,
			try list_to_integer(Tcpport) of
				OldTcpPort ->
					{struct, [{success, true}, {<<"message">>, <<"Nothing to do">>}]};
				N when N >= 1024, N =< 65535 ->
					Tcprec = #cpx_conf{
						id = agent_tcp_listener,
						module_name = agent_tcp_listener,
						start_function = start_link,
						start_args = [N],
						supervisor = agent_connection_sup
					},
					cpx_supervisor:update_conf(agent_tcp_listener, Tcprec),
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
			OldWebPort = case cpx_supervisor:get_conf(agent_web_listener) of
				WebRecord when is_record(WebRecord, cpx_conf) ->
					lists:nth(1, WebRecord#cpx_conf.start_args);
				_Else2 ->
					undefined
			end,
			try list_to_integer(Webport) of
				OldWebPort  ->
					{struct, [{success, true}, {<<"message">>, <<"Nothing to do">>}]};
				M when M >= 1024, M =< 65535 ->
					Webrec = #cpx_conf{
						id = agent_web_listener,
						module_name = agent_web_listener,
						start_function = start_link,
						start_args = [M],
						supervisor = agent_connection_sup
					},
					cpx_supervisor:update_conf(agent_web_listener, Webrec),
					{struct, [{success, true}, {<<"message">>, <<"Web Server enabled">>}]};
				_M ->
					{struct, [{success, false}, {<<"message">>, <<"Listen port out of range">>}]}
			catch
				error:badarg ->
					{struct, [{success, false}, {<<"message">>, <<"Listen port not a number">>}]}
			end
	end,
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"results">>, [Tcpout, Webout]}]})};
api({agents, "modules", "get"}, ?COOKIE, _Post) ->
	Tcpout = case cpx_supervisor:get_conf(agent_tcp_listener) of
		undefined ->
			[{"agentModuleTCPListen", 1337}, {"agentModuleTCPListenEnabled", false}];
		Tcplist ->
			[Tcport] = Tcplist#cpx_conf.start_args,
			[{"agentModuleTCPListen", Tcport}, {"agentModuleTCPListenEnabled", true}]
	end,
	Webout = case cpx_supervisor:get_conf(agent_web_listener) of
		undefined ->
			[{"agentModuleWebListen", 5050}, {"agentModuleWebListenEnabled", false}];
		Weblist ->
			[Webport] = Weblist#cpx_conf.start_args,
			[{"agentModuleWebListen", Webport}, {"agentModuleWebListenEnabled", true}]
	end,
	Full = lists:append([Tcpout, Webout]),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"result">>, {struct, Full}}]})};

%% =====
%% agents -> spiceintegration
%% =====
api({agents, "spiceintegration", "get"}, ?COOKIE, _Post) ->
	Settings = case cpx_supervisor:get_conf(spicecsm_integration) of
		undefined ->
			{struct, [{<<"spiceIntegrationEnabled">>, false}]};
		#cpx_conf{start_args = [Args | _]} = Conf ->
			?DEBUG("conf:  ~p", [Conf]),
			{struct, [
				{<<"spiceIntegrationEnabled">>, true},
				{<<"server">>, list_to_binary(proplists:get_value(server, Args))},
				{<<"username">>, list_to_binary(proplists:get_value(username, Args, ""))},
				{<<"password">>, list_to_binary(proplists:get_value(password, Args, ""))}
			]}
	end,
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"result">>, Settings}]})};
api({agents, "spiceintegration", "set"}, ?COOKIE, Post) ->
	case proplists:get_value("spiceIntegrationEnabled", Post) of
		undefined ->
			cpx_supervisor:destroy(spicecsm_integration),
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		_Else ->
			case proplists:get_value("server", Post) of
				undefined ->
					{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Server required">>}]})};
				Server ->
					Username = proplists:get_value("username", Post, ""),
					Password = proplists:get_value("password", Post, ""),
					case proplists:get_value("spiceIntegratoinImport", Post) of
						undefined ->
							Conf = #cpx_conf{
								id = spicecsm_integration,
								module_name = spicecsm_integration,
								start_function = start_link,
								start_args = [[
									{server, Server},
									{username, Username},
									{password, Password}
								]]},
							Addres = cpx_supervisor:update_conf(spicecsm_integration, Conf),
							case Addres of
								{atomic, _} ->
									{200, [], mochijson2:encode({struct, [{success, true}]})};
								Else ->
									?WARNING("Could not start spicecsm integration:  ~p", [Else]),
									{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"service errored on start">>}]})}
							end;
						"spiceIntegrationImport" ->
							Args = [
								{server, Server},
								{username, Username},
								{password, Password},
								add_conf
							],
							F = fun() ->
								spicecsm_integration:import(Args)
							end,
							spawn(F),
							{200, [], mochijson2:encode({struct, [{success, true}, {<<"message">>, <<"import in progress">>}]})}
					end
			end
	end;
						
%% =====
%% agents -> profiles
%% =====
api({agents, "profiles", "get"}, ?COOKIE, _Post) ->
	Profiles = agent_auth:get_profiles(),
	Foreachprofile = fun({Pname, Pskills}) ->
		Agents = agent_auth:get_agents(Pname),
		{struct, [{<<"name">>, list_to_binary(Pname)}, {<<"type">>, <<"profile">>}, {<<"skills">>, encode_skills(Pskills)}, {<<"agents">>, encode_agents(Agents)}]}
	end,
	Items = lists:map(Foreachprofile, Profiles),
	Json = {struct, [{success, true}, {<<"items">>, Items}]},
	{200, [], mochijson2:encode(Json)};
api({agents, "profiles", Profile, "getskills"}, ?COOKIE, _Post) ->
	{_Profilename, Skillatoms} = agent_auth:get_profile(Profile),
	Encoded = encode_skills(Skillatoms),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"items">>, Encoded}]})};
api({agents, "profiles", "new"}, ?COOKIE, Post) ->
	Skillatoms = parse_posted_skills(proplists:get_all_values("skills", Post)),
	agent_auth:new_profile(proplists:get_value("name", Post), Skillatoms),
	{200, [], mochijson2:encode({struct, [{success, true}]})};
api({agents, "profiles", "Default", "update"}, {_Reflist, _Salt, _Login}, Post) ->
	case proplists:get_value("name", Post) of
		undefined ->
			Skillatoms = parse_posted_skills(proplists:get_all_values("skills", Post)),
			agent_auth:set_profile("Default", "Default", Skillatoms),
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		_Else ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Default is a protected profile and cannot be renamed">>}]})}
	end;
api({agents, "profiles", Profile, "update"}, ?COOKIE, Post) ->
	Skillatoms = parse_posted_skills(proplists:get_all_values("skills", Post)),
	agent_auth:set_profile(Profile, proplists:get_value("name", Post), Skillatoms),
	{200, [], mochijson2:encode({struct, [{success, true}]})};
api({agents, "profiles", "Default", "delete"}, ?COOKIE, _Post) ->
	{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Default is a protected profile and cannot be deleted">>}]})};
api({agents, "profiles", Profile, "delete"}, ?COOKIE, _Post) ->
	agent_auth:destroy_profile(Profile),
	{200, [], mochijson2:encode({struct, [{success, true}]})};

%% =====
%% agents -> agents
%% =====
api({agents, "agents", Agent, "get"}, ?COOKIE, _Post) ->
	{atomic, [Agentrec]} = agent_auth:get_agent(id, Agent),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"agent">>, encode_agent(Agentrec)}]})};
api({agents, "agents", Agent, "delete"}, ?COOKIE, _Post) ->
	agent_auth:destroy(id, Agent),
	{200, [], mochijson2:encode({struct, [{success, true}]})};
api({agents, "agents", Agent, "update"}, ?COOKIE, Post) ->
	{atomic, [_Agentrec]} = agent_auth:get_agent(id, Agent),
	%{ok, Regex} = re:compile("^{(_\\w+),([-a-zA-Z0-9_ ]+)}$"),
	Postedskills = proplists:get_all_values("skills", Post),
	Fixedskills = parse_posted_skills(Postedskills),
	?DEBUG("~p", [Fixedskills]),
	Confirmpw = proplists:get_value("confirm", Post, {"notfilledin"}),
	case proplists:get_value("password", Post) of
		"" ->
			?DEBUG("Not updating password.", []),
			agent_auth:set_agent(Agent, [
				{login, proplists:get_value("login", Post)},
				{skills, Fixedskills},
				{securitylevel, list_to_existing_atom(proplists:get_value("security", Post))},
				{profile, proplists:get_value("profile", Post)},
				{lastname, proplists:get_value("lastname", Post)},
				{firstname, proplists:get_value("firstname", Post)}
			]);
		Confirmpw ->
			?DEBUG("Updating password", []),
			agent_auth:set_agent(Agent, [
				{login, proplists:get_value("login", Post)},
				{password, proplists:get_value("password", Post)},
				{skills, Fixedskills},
				{securitylevel, list_to_existing_atom(proplists:get_value("security", Post))},
				{profile, proplists:get_value("profile", Post)},
				{lastname, proplists:get_value("lastname", Post)},
				{firstname, proplists:get_value("firstname", Post)}
			])
	end,
	{200, [], mochijson2:encode({struct, [{success, true}]})};
api({agents, "agents", "new"}, ?COOKIE, Post) ->
	Confirmpw = proplists:get_value("confirm", Post, {"notfilledin"}),
	case proplists:get_value("password", Post) of
		"" ->
			erlang:error({badarg, proplists:get_value("password", Post)});
		Confirmpw ->
			Postedskills = proplists:get_all_values("skills", Post),
			Fixedskills = parse_posted_skills(Postedskills),
			agent_auth:add_agent([
				{login, proplists:get_value("login", Post)},
				{password, util:bin_to_hexstr(erlang:md5(Confirmpw))},
				{skills, Fixedskills},
				{securitylevel, list_to_existing_atom(proplists:get_value("security", Post))},
				{profile, proplists:get_value("profile", Post)},
				{lastname, proplists:get_value("lastname", Post)},
				{firstname, proplists:get_value("firstname", Post)}
			]),
			{200, [], mochijson2:encode({struct, [{success, true}]})}
	end;

%% =====
%% release_opts -> <command>
%% =====
api(["", "release_opts", "get_all"], ?COOKIE, _Post) ->
	Releases = agent_auth:get_releases(),
	Convert = fun(R) ->
		{struct, [
			{id, R#release_opt.id},
			{label, list_to_binary(R#release_opt.label)},
			{bias, R#release_opt.bias}
		]}
	end,
	Converted = lists:map(Convert, Releases),
	Json = {struct, [
		{<<"identifier">>, id},
		{<<"label">>, label},
		{<<"items">>, Converted}
	]},
	{200, [], mochijson2:encode(Json)};
api(["", "release_opts", "add"], ?COOKIE, Post) ->
	Releases = agent_auth:get_releases(),
	Maxid = case Releases of
		[] ->
			0;
		List ->
			#release_opt{id = Id} = lists:max(List),
			Id
	end,
	Newid = Maxid + 1,
	Rec = #release_opt{
		id = Newid, 
		label = proplists:get_value("label", Post, "unlabeled"),
		bias = list_to_integer(proplists:get_value("bias", Post, "0"))
	},
	case agent_auth:new_release(Rec) of
		{atomic, ok} ->
			Json = {struct, [
				{success, true},
				{id, Newid}
			]},
			{200, [], mochijson2:encode(Json)};
		Else ->
			?WARNING("add_release failed:  ~p", [Else]),
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"write failed">>}]})}
	end;
api(["", "release_opts", "update", Idstr], ?COOKIE, Post) ->
	Id = list_to_integer(Idstr),
	Rec = #release_opt{
		id = Id,
		label = proplists:get_value("label", Post, "unlabeled"),
		bias = list_to_integer(proplists:get_value("bias", Post, "0"))
	},
	case agent_auth:new_release(Rec) of
		{atomic, ok} ->
			Json = {struct, [
				{success, true}
			]},
			{200, [], mochijson2:encode(Json)};
		Else ->
			?WARNING("update release failed:  ~p", [Else]),
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"write failed">>}]})}
	end;
api(["", "release_opts", "drop", Idstr], ?COOKIE, _Post) ->
	Id = list_to_integer(Idstr),
	case agent_auth:destroy_release(id, Id) of
		{atomic, ok} ->
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		Else ->
			?WARNING("destroy failed:  ~p", [Else]),
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"delete failed">>}]})}
	end;
	
%% =====
%% skills -> groups
%% =====
api({skills, "groups", "get"}, ?COOKIE, _Post) ->
	Skills = call_queue_config:get_skills(),
	Proplist = dict:to_list(encode_skills_with_groups(Skills)),
	Convert = fun({Group, Skillrecs}) ->
		{struct, [{<<"name">>, list_to_binary(Group)}, {<<"type">>, <<"group">>}, {<<"skills">>, encode_skills(Skillrecs)}]}
	end,
	Json = {struct, [{success, true}, {<<"items">>, lists:map(Convert, Proplist)}]},
	{200, [], mochijson2:encode(Json)};
api({skills, "groups", Group, "update"}, ?COOKIE, Post) ->
	?DEBUG("Updating skill group ~p", [Group]),
	Newname = proplists:get_value("name", Post),
	call_queue_config:rename_skill_group(Group, Newname),
	{200, [], mochijson2:encode({struct, [{success, true}]})};
%api({skills, Profile}, {_Reflist, _Salt, _Login}, Post) ->
%	{_Profilename, Skillatoms} = agent_auth:get_profile(Profile),
%	Encoded = encode_skills(Skillatoms),
%	{200, [], mochijson2:encode({struct, [{success, true}, {<<"items">>, Encoded}]})};

%% =====
%% skills -> skill
%% =====
api({skills, "skill", "_queue", "expand"}, ?COOKIE, _Post) ->
	Queues = call_queue_config:get_queues(),
	F = fun(Qrec) ->
		list_to_binary(Qrec#call_queue.name)
	end,
	Converted = lists:map(F, Queues),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"items">>, Converted}]})};
api({skills, "skill", "_node", "expand"}, ?COOKIE, _Post) ->
	Nodes = [node() | nodes()],
	F = fun(Atom) ->
		L = atom_to_list(Atom),
		list_to_binary(L)
	end,
	Converted = lists:map(F, Nodes),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"items">>, Converted}]})};
api({skills, "skill", "_agent", "expand"}, {_Reflist, _Salt, _Login}, _Post) ->
	Agents = agent_auth:get_agents(),
	F = fun(Arec) ->
		list_to_binary(Arec#agent_auth.login)
	end,
	Converted = lists:map(F, Agents),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"items">>, Converted}]})};
api({skills, "skill", "_brand", "expand"}, ?COOKIE, _Post) ->
	Clients = call_queue_config:get_clients(),
	F = fun(Clientrec, Acc) ->
		case Clientrec#client.label of
			undefined ->
				Acc;
			Else ->
				[list_to_binary(Else) | Acc]
		end
	end,
	Converted = lists:foldl(F, [], Clients),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"items">>, Converted}]})};
api({skills, "skill", "_profile", "expand"}, ?COOKIE, _Post) ->
	ProfnSkills = agent_auth:get_profiles(),
	F = fun({Prof, _Skillz}) ->
		list_to_binary(Prof)
	end,
	Converted = lists:map(F, ProfnSkills),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"items">>, Converted}]})};
api({skills, "skill", Skill, "update"}, ?COOKIE, Post) ->
	case call_queue_config:get_skill(Skill) of
		Skillrec when is_record(Skillrec, skill_rec) ->
			case Skillrec#skill_rec.protected of
				false ->
					Rec = #skill_rec{
						atom = Skillrec#skill_rec.atom,
						name = proplists:get_value("name", Post),
						description = proplists:get_value("description", Post),
						group = proplists:get_value("group", Post)},
					call_queue_config:set_skill(Skillrec#skill_rec.atom, Rec),
					{200, [], mochijson2:encode({struct, [{success, true}]})}
			end
	end;
api({skills, "skill", "new"}, ?COOKIE, Post) ->
	call_queue_config:new_skill(
		list_to_atom(proplists:get_value("atom", Post)),
		proplists:get_value("name", Post),
		proplists:get_value("description", Post),
		proplists:get_value("group", Post)
	),
	{200, [], mochijson2:encode({struct, [{success, true}]})};

%% =====
%% queues -> groups
%% =====

api({queues, "groups", "get"}, ?COOKIE, _Post) ->
	Groups = call_queue_config:get_queue_groups(),
	List = encode_queues_with_groups(Groups),
	Json = {struct, [{success, true}, {<<"items">>, List}]},
	{200, [], mochijson2:encode(Json)};
api({queues, "groups", Group, "get"}, ?COOKIE, _Post) ->
	{atomic, [Qgroup]} = call_queue_config:get_queue_group(Group),
	Jrecipe = encode_recipe(Qgroup#queue_group.recipe),
	Json = {struct, [
		{<<"name">>, list_to_binary(Qgroup#queue_group.name)},
		{<<"sort">>, Qgroup#queue_group.sort},
		{<<"protected">>, Qgroup#queue_group.protected},
		{<<"recipe">>, Jrecipe}
	]},
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"queuegroup">>, Json}]})};
api({queues, "groups", Group, "update"}, ?COOKIE, Post) ->
	Newname = proplists:get_value("name", Post),
	Sort = list_to_integer(proplists:get_value("sort", Post)),
	Recipe = case proplists:get_value("recipe", Post) of
		"[]" ->
			[];
		Else ->
			decode_recipe(Else)
	end,
	call_queue_config:set_queue_group(Group, Newname, Sort, Recipe),
	{200, [], mochijson2:encode({struct, [{success, true}]})};
api({queues, "groups", "new"}, ?COOKIE, Post) ->
	Name = proplists:get_value("name", Post), 
	Sort = list_to_integer(proplists:get_value("sort", Post)),
	Recipe = case proplists:get_value("recipe", Post) of
		"[]" ->
			[];
		Else ->
			decode_recipe(Else)
	end,
	call_queue_config:new_queue_group(Name, Sort, Recipe),
	{200, [], mochijson2:encode({struct, [{success, true}]})};
api({queues, "groups", Group, "delete"}, ?COOKIE, _Post) ->
	case call_queue_config:destroy_queue_group(Group) of
		{atomic, ok} ->
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		{atomic, {error, protected}} ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Group is protected and cannot be deleted">>}]})}
	end;
api({queues, "queue", Queue, "get"}, ?COOKIE, _Post) ->
	case call_queue_config:get_queue(Queue) of
		noexists ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"No such queue">>}]})};
		Queuerec ->
			Jqueue = encode_queue(Queuerec),
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"queue">>, Jqueue}]})}
	end;
api({queues, "queue", Queue, "update"}, ?COOKIE, Post) ->
	Recipe = decode_recipe(proplists:get_value("recipe", Post)),
	Weight = list_to_integer(proplists:get_value("weight", Post)),
	Name = proplists:get_value("name", Post),
	Postedskills = proplists:get_all_values("skills", Post),
	Fixedskills = parse_posted_skills(Postedskills),
	Group = proplists:get_value("group", Post),
	Qrec = #call_queue{
		name = Name,
		weight = Weight,
		skills = Fixedskills,
		recipe = Recipe,
		group = Group
	},
	call_queue_config:set_queue(Queue, Qrec),
	queue_manager:load_queue(Queue),
	{200, [], mochijson2:encode({struct, [{success, true}]})};
api({queues, "queue", Queue, "delete"}, ?COOKIE, _Post) ->
	call_queue_config:destroy_queue(Queue),
	{200, [], mochijson2:encode({struct, [{success, true}]})};
api({queues, "queue", "new"}, ?COOKIE = Cookie, Post) ->
	Name = proplists:get_value("name", Post),
	Qrec = #call_queue{
		name = Name,
		weight = 1,
		skills = [],
		recipe = [],
		group = "Default"
	},
	call_queue_config:new_queue(Qrec),
	api({queues, "queue", Name, "update"}, Cookie, Post);
	
%% =====
%% media -> *
%% =====
api({medias, "poll"}, ?COOKIE, _Post) ->
	{ok, Appnodes} = application:get_env(cpx, nodes),
	Nodes = lists:filter(fun(N) -> lists:member(N, Appnodes) end, [node() | nodes()]),
	F = fun(Node) ->
		{Node, [
			{cpx_monitor_grapher, rpc:call(Node, cpx_supervisor, get_conf, [cpx_monitor_grapher], 2000)},
			{cpx_monitor_passive, rpc:call(Node, cpx_supervisor, get_conf, [cpx_monitor_passive], 2000)},
			{cpx_supervisor, rpc:call(Node, cpx_supervisor, get_value, [archivepath], 2000)},
			{email_media_manager, rpc:call(Node, cpx_supervisor, get_conf, [email_media_manager], 2000)},
			{freeswitch_media_manager, rpc:call(Node, cpx_supervisor, get_conf, [freeswitch_media_manager], 2000)},
			{gen_cdr_dumper, rpc:call(Node, cpx_supervisor, get_conf, [gen_cdr_dumper], 2000)}
		]}
	end,
	Rpcs = lists:map(F, Nodes),
	Json = encode_medias(Rpcs, []),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"identifier">>, <<"id">>}, {<<"label">>, <<"name">>}, {<<"items">>, Json}]})};

%% =====
%% media -> node -> media
%% =====

api({medias, Node, "cpx_monitor_grapher", "get"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	Json = case rpc:call(Atomnode, cpx_supervisor, get_conf, [cpx_monitor_grapher]) of
		undefined ->
			{struct, [
				{success, true}, 
				{<<"enabled">>, false},
				{<<"rrdPath">>, <<"rrd">>},
				{<<"imagePath">>, <<"rrd path">>}
			]};
		#cpx_conf{start_args = [Args]} ->
			Rrdpath = list_to_binary(proplists:get_value(rrd_dir, Args, "rrd")),
			Protoimagepath = list_to_binary(proplists:get_value(image_dir, Args, "rrd path")),
			Imagepath = case Protoimagepath of
				Rrdpath ->
					<<"rrd path">>;
				<<"../www/dynamic">> ->
					<<"Dynamic Files">>;
				Else ->
					Else
			end,
			{struct, [
				{success, true},
				{<<"enabled">>, true},
				{<<"rrdPath">>, Rrdpath},
				{<<"imagePath">>, Imagepath}
			]}
	end,
	{200, [], mochijson2:encode(Json)};
api({medias, Node, "cpx_monitor_grapher", "update"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
	Json = case proplists:get_value("enabled", Post) of
		undefined ->
			rpc:call(Atomnode, cpx_supervisor, destroy, [cpx_monitor_grapher], 2000),
			{struct, [{success, true}]};
		"grapherEnabled" ->
			Rrdpath = proplists:get_value("rrdPath", Post, "rrd"),
			Imagepath = case proplists:get_value("imagePath", Post, Rrdpath) of
				"rrd path" ->
					Rrdpath;
				"Dynamic Files" ->
					"../www/dynamic";
				Otherpath ->
					Otherpath
			end,
			Startargs = [
				{rrd_dir, Rrdpath},
				{image_dir, Imagepath}
			],
			case rpc:call(Atomnode, cpx_supervisor, update_conf, [cpx_monitor_grapher, #cpx_conf{
					id = cpx_monitor_grapher, 
					module_name = cpx_monitor_grapher, 
					start_function = start_link, 
					start_args = [Startargs], 
					supervisor = management_sup
				}]) of
				{atomic, {ok, Pid}} when is_pid(Pid) ->
					{struct, [{success, true}]};
				Else ->
					?WARNING("could not start cpx_web_grapher at ~p due to ~p", [Atomnode, Else]),
					{struct, [{success, false}]}
			end
	end,
	{200, [], mochijson2:encode(Json)};
api({medias, Node, "cpx_monitor_passive", "update"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
	Json = case proplists:get_value("enabled", Post, false) of
		false ->
			rpc:call(Atomnode, cpx_supervisor, destroy, [cpx_monitor_passive], 2000),
			{struct, [{success, true}]};
		"passiveEnabled" ->
			Interval = list_to_integer(proplists:get_value("interval", Post)),
			Decoded = mochijson2:decode(proplists:get_value("filters", Post)),
			SetFilter = fun
				(<<"all">>) -> 
					all; 
				(List) -> 
					lists:map(fun(I) -> binary_to_list(I) end, List)
			end,
			Convert = fun({struct, Props}) ->
				Fileout = case proplists:get_value(<<"outputdir">>, Props) of
					<<"dynamic">> ->
						"www/dynamic";
					Else ->
						binary_to_list(Else)
				end,
				{binary_to_list(proplists:get_value(<<"name">>, Props)), [
					{file_output, Fileout},
					{queues, SetFilter(proplists:get_value(<<"queues">>, Props))},
					{queue_groups, SetFilter(proplists:get_value(<<"queue_groups">>, Props))},
					{agents, SetFilter(proplists:get_value(<<"agents">>, Props))},
					{agent_profiles, SetFilter(proplists:get_value(<<"agent_profiles">>, Props))},
					{clients, SetFilter(proplists:get_value(<<"clients">>, Props))},
					{nodes, case SetFilter(proplists:get_value(<<"nodes">>, Props)) of all -> all; List -> lists:map(fun(I) -> list_to_existing_atom(I) end, List) end}
				]}
			end,
			Filters = lists:map(Convert, Decoded),
			Args = [
				{write_interval, Interval},
				{outputs, Filters}
			],
			{atomic, {ok, _Pid}} = rpc:call(Atomnode, cpx_supervisor, update_conf, [cpx_monitor_passive, #cpx_conf{
				id = cpx_monitor_passive, 
				module_name = cpx_monitor_passive,
				start_function = start_link, 
				start_args = [Args], 
				supervisor = management_sup
			}]),
			{struct, [{success, true}]}
	end,
	{200, [], mochijson2:encode(Json)};	
api({medias, Node, "cpx_monitor_passive", "get"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	case rpc:call(Atomnode, cpx_supervisor, get_conf, [cpx_monitor_passive]) of
		undefined ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"enabled">>, false}]})};
		#cpx_conf{start_args = [Args]} ->
			Protofilters = proplists:get_value(outputs, Args, []),
			Fixfilterlist = fun
				(all) -> 
					all; 
				(List) -> 
					lists:map(fun
						(I) when is_atom(I) ->
							I;
						(I) -> 
							list_to_binary(I) 
					end, List) 
			end,
			Fixfilter = fun({Name, Options}) ->
				{struct, [
					{<<"name">>, list_to_binary(Name)},
					{<<"queues">>, Fixfilterlist(proplists:get_value(queues, Options, all))},
					{<<"queue_groups">>, Fixfilterlist(proplists:get_value(queue_groups, Options, all))},
					{<<"agents">>, Fixfilterlist(proplists:get_value(agents, Options, all))},
					{<<"agent_profiles">>, Fixfilterlist(proplists:get_value(agent_profiles, Options, all))},
					{<<"clients">>, Fixfilterlist(proplists:get_value(clients, Options, all))},
					{<<"nodes">>, Fixfilterlist(proplists:get_value(nodes, Options, all))},
					{<<"outputdir">>, list_to_binary(proplists:get_value(file_output, Options, "./"))}
				]}
			end,
			Filters = lists:map(Fixfilter, Protofilters),
			?DEBUG("Fix filter:  ~p", [Filters]),
			Json = {struct, [
				{success, true},
				{<<"enabled">>, true},
				{<<"interval">>, proplists:get_value(write_interval, Args, 60)},
				{<<"filters">>, {struct, [
					{<<"identifier">>, <<"name">>},
					{<<"items">>, Filters}
				]}}
			]},
			{200, [], mochijson2:encode(Json)}
	end;
%%api({medias, Node, "cpx_monitor_passive", "update"}, ?COOKIE, Post) ->
%%	Atomnode = list_to_existing_atom(Node),
	
api({medias, Node, "gen_cdr_dumper", "get"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	Json = case rpc:call(Atomnode, cpx_supervisor, get_conf, [gen_cdr_dumper]) of
		undefined ->
			{struct, [
				{success, false},
				{<<"message">>, <<"no dumper enabled">>}
			]};
		#cpx_conf{start_args = Args} ->
			case Args of
				[] ->
					{struct, [
						{success, true},
						{<<"dumper">>, <<"null">>},
						{<<"agentFile">>, <<"">>},
						{<<"cdrFile">>, <<"">>},
						{<<"dsn">>, <<"">>},
						{<<"traceEnabled">>, []}
					]};
				[cdr_csv, Options] ->
					{struct, [
						{success, true},
						{<<"dumper">>, <<"csv">>},
						{<<"agentFile">>, list_to_binary(proplists:get_value(agent_file, Options, ""))},
						{<<"cdrFile">>, list_to_binary(proplists:get_value(cdr_file, Options, ""))},
						{<<"traceEnabled">>, []}
					]};
				[cdr_odbc, [DSN, Options]] ->
					Trace = case proplists:get_value(trace_driver, Options) of
						true ->
							[<<"on">>];
						undefined ->
							[]
					end,
					{struct, [
						{success, true},
						{<<"dumper">>, <<"odbc">>},
						{<<"agentFile">>, <<"">>},
						{<<"cdrFile">>, <<"">>},
						{<<"traceEnabled">>, Trace},
						{<<"dsn">>, list_to_binary(DSN)}
					]};
				[cdr_dets, [{logdir, Dir}]] ->
					Dirbin = case Dir of
						tmp ->
							<<"__tmp__">>;
						dynamic ->
							<<"__dynamic__">>;
						_ ->
							list_to_binary(Dir)
					end,
					{struct, [
						{success, true},
						{<<"dumper">>, <<"dets">>},
						{<<"destDir">>, Dirbin}
					]}
			end
	end,
	{200, [], mochijson2:encode(Json)};
api({medias, Node, "gen_cdr_dumper", "update"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
	%?DEBUG("post:  ~p", [Post]),
	Conf = #cpx_conf{
		id = gen_cdr_dumper,
		module_name = gen_cdr_dumper,
		start_function = start_link,
		start_args = [],
		supervisor = management_sup
	},
	Json = case proplists:get_value("dumper", Post) of
		"null" ->
			case rpc:call(Atomnode, cpx_supervisor, update_conf, [gen_cdr_dumper, Conf]) of
				{atomic, {ok, _Pid}} ->
					{struct, [{success, true}]};
				Else ->
					?WARNING("gen_cdr_dumper update:  ~p", [Else]),
					{struct, [{success, false}, {<<"message">>, <<"Could not start new dumper">>}]}
			end;
		"csv" ->
			Cdrfile = case proplists:get_value("cdrFile", Post) of
				[] ->
					[];
				undefined ->
					[];
				CsvElse ->
					[{cdr_file, CsvElse}]
			end,
			AgentFile = case proplists:get_value("agentFile", Post) of
				[] ->
					[];
				undefined ->
					[];
				CsvAlsoElse ->
					[{agent_file, CsvAlsoElse}]
			end,
			Args = lists:append([Cdrfile, AgentFile]),
			case rpc:call(Atomnode, cpx_supervisor, update_conf, [gen_cdr_dumper, Conf#cpx_conf{start_args = [cdr_csv, Args]}]) of
				{atomic, {ok, _Pid}} ->
					{struct, [{success, true}]};
				CsvRpcElse ->
					?WARNING("gen_cdr_dumper update:  ~p", [CsvRpcElse]),
					{struct, [{success, false}, {<<"message">>, <<"Could not start new dumper">>}]}
			end;
		"odbc" ->
			Options = case proplists:get_value("traceEnabled", Post) of
				undefined -> 
					[];
				"on" ->
					[trace_driver]
			end,
			Args = [proplists:get_value("dsn", Post), Options],
			case rpc:call(Atomnode, cpx_supervisor, update_conf, [gen_cdr_dumper, Conf#cpx_conf{start_args = [cdr_odbc, Args]}]) of
				{atomic, {ok, _Pid}} ->
					{struct, [{success, true}]};
				Else ->
					?WARNING("gen_cdr_dumper update:  ~p", [Else]),
					{struct, [{success, false}, {<<"message">>, <<"Could not start new dumper">>}]}
			end;
		"dets" ->
			Args = case proplists:get_value("destDir", Post) of
				undefined ->
					[];
				Else ->
					[{logdir, Else}]
			end,
			case rpc:call(Atomnode, cpx_supervisor, update_conf, [gen_cdr_dumper, Conf#cpx_conf{start_args = [cdr_dets, Args]}]) of
				{atomic, {ok, _Pid}} ->
					{struct, [{success, true}]};
				Error ->
					?WARNING("gen_cdr_dumper update:  ~p", [Error]),
					{struct, [{success, false}, {<<"message">>, <<"could not start new dumper">>}]}
			end
	end,
	{200, [], mochijson2:encode(Json)};
	
api({medias, Node, "cpx_supervisor", "get"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	case rpc:call(Atomnode, cpx_supervisor, get_value, [archivepath]) of
		none ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"result">>, <<"">>}]})};
		{ok, Value} ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"result">>, list_to_binary(Value)}]})}
	end;
api({medias, Node, "cpx_supervisor", "update"}, ?COOKIE, Post) ->
	Atomnode = list_to_atom(Node),
	{Func, Args} = case proplists:get_value("value", Post) of
		"" ->
			{drop_value, [archivepath]};
		Data ->
			{set_value, [archivepath, Data]}
	end,
	case rpc:call(Atomnode, cpx_supervisor, Func, Args) of
		{atomic, ok} ->
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		Else ->
			?INFO("dropping archivepath: ~p", [Else]),
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"shrug">>}]})}
	end;
api({medias, Node, "freeswitch_media_manager", "update"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
	case proplists:get_value("enabled", Post) of
		undefined ->
			rpc:call(Atomnode, cpx_supervisor, destroy, [freeswitch_media_manager], 2000),
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		_Else ->
			Args = [list_to_atom(proplists:get_value("cnode", Post)), [
				{voicegateway, proplists:get_value("voicegw", Post, "")}]],
			Conf = #cpx_conf{
				id = freeswitch_media_manager,
				module_name = freeswitch_media_manager,
				start_function = start_link,
				start_args = Args},
			rpc:call(Atomnode, cpx_supervisor, update_conf, [freeswitch_media_manager, Conf], 2000),
			{200, [], mochijson2:encode({struct, [{success, true}]})}
	end;
api({medias, Node, "freeswitch_media_manager", "get"}, ?COOKIE, _Post) ->
	Anode = list_to_existing_atom(Node),
	case rpc:call(Anode, cpx_supervisor, get_conf, [freeswitch_media_manager]) of
		undefined ->
			Json = {struct, [
				{success, true},
				{<<"enabled">>, false}
			]},
			{200, [], mochijson2:encode(Json)};
		Rec when is_record(Rec, cpx_conf) ->
			[Cnode, [Head | _Tail] = Args] = Rec#cpx_conf.start_args,
			?DEBUG("Args: ~p", [Args]),
			{_Domain, Voicegw} = case Head of
				X when is_tuple(X) ->
					{proplists:get_value(domain, Args, ""), proplists:get_value(voicegateway, Args, "")};
				X ->
					{X, ""}
			end,
			Json = {struct, [
				{success, true},
				{<<"enabled">>, true},
				{<<"cnode">>, list_to_binary(atom_to_list(Cnode))},
				{<<"voicegw">>, list_to_binary(Voicegw)}
			]},
			{200, [], mochijson2:encode(Json)}
	end;
	
api({medias, Node, "email_media_manager", "update"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
	case proplists:get_value("enabled", Post) of
		undefined ->
			rpc:call(Atomnode, cpx_supervisor, destroy, [email_media_manager], 2000),
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		_Else ->
			Aliases = [
				{"port", port},
				{"host", host},
				{"bindip", address},
				{"inrelays", relays},
				{"outrelay", outbound},
				{"outport", outport},
				{"auth", auth},
				{"username", username},
				{"password", password},
				{"ssl", ssl},
				{"tls", tls}
			],
			Newpost = proplists:substitute_aliases(Aliases, Post),
			Cleanpost = fun
				({_Key, []}, Acc) ->
					Acc;
				({ssl, "usessl"}, Acc) ->
					[{protocol, ssl}, {ssl, true} | Acc];
				({_Key, "never"}, Acc) ->
					Acc;
				({Key, "always"}, Acc) ->
					[{Key, always} | Acc];
				({Key, "ifcan"}, Acc) ->
					[{Key, if_available} | Acc];
				({port, Val}, Acc) ->
					[{port, list_to_integer(Val)} | Acc];
				({outport, Val}, Acc) ->
					[{outport, list_to_integer(Val)} | Acc];
				({address, Val}, Acc) ->
					Newval = case util:string_split(Val, ".") of
						Val ->
							list_to_tuple(util:string_split(Val, ":"));
						Else ->
							list_to_tuple(Else)
					end,
					[{address, Newval} | Acc];
				({"enabled", _}, Acc) ->
					Acc;
				(Val, Acc) ->
					[Val | Acc]
			end,
			Cleanedpost = lists:foldl(Cleanpost, [], Newpost),
			Conf = #cpx_conf{
				id = email_media_manager,
				module_name = email_media_manager,
				start_function = start_link,
				start_args = [Cleanedpost]
			},
			Json = case rpc:call(Atomnode, cpx_supervisor, update_conf, [email_media_manager, Conf], 2000) of
				{atomic, {ok, _Pid}} ->
					{struct, [{success, true}]};
				Error ->
					?WARNING("Could not update the conf on ~p due to ~p", [Atomnode, Error]),
					{struct, [{success, false}, {<<"message">>, <<"Could not start manager">>}]}
			end,
			{200, [], mochijson2:encode(Json)}
	end;
	
api({medias, Node, "email_media_manager", "get"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	case rpc:call(Atomnode, cpx_supervisor, get_conf, [email_media_manager]) of
		undefined ->
			Json = {struct, [
				{success, true},
				{<<"enabled">>, false}
			]},
			{200, [], mochijson2:encode(Json)};
		Rec when is_record(Rec, cpx_conf) ->
			[Args] = Rec#cpx_conf.start_args,
			Sendargs = [
				{<<"success">>, true},
				{<<"enabled">>, true},
				{<<"port">>, proplists:get_value(port, Args, <<"">>)},
				{<<"host">>, list_to_binary(proplists:get_value(host, Args, ""))},
				{<<"bindip">>, case proplists:get_value(address, Args) of
					undefined ->
						<<"0.0.0.0">>;
					{_, _, _, _} = Iptuple ->
						list_to_binary(string:join(tuple_to_list(Iptuple), "."))
				end},
				{<<"inrelays">>, lists:map(fun(I) -> list_to_binary(I) end, proplists:get_value(relays, Args, []))},
				{<<"outrelay">>, list_to_binary(proplists:get_value(outbound, Args, ""))},
				{<<"outport">>, proplists:get_value(outport, Args, <<"">>)},
				{<<"auth">>, case proplists:get_value(auth, Args, never) of
					if_available ->
						<<"ifcan">>;
					Else ->
						Else
				end},
				{<<"username">>, list_to_binary(proplists:get_value(username, Args, ""))},
				{<<"password">>, list_to_binary(proplists:get_value(password, Args, ""))},
				{<<"ssl">>, proplists:get_value(ssl, Args, false)},
				{<<"tls">>, case proplists:get_value(tls, Args, never) of
					if_available ->
						<<"ifcan">>;
					Else ->
						Else
				end}
			],
			{200, [], mochijson2:encode({struct, Sendargs})}
	end;
api({medias, _Node, "email_media_manager", "getMappings"}, ?COOKIE, _Post) ->
	case email_media_manager:get_mappings() of
		{atomic, Mappings} ->
			Encode = fun(Mapping) ->
				Client = case Mapping#mail_map.client of
					undefined ->
						"undefined";
					Else ->
						Else
				end,
				Skills = string:join(lists:map(fun
					(B) when is_binary(B) ->
						binary_to_list(B);
					(B) when is_atom(B) ->
						atom_to_list(B)
				end, encode_skills_simple(Mapping#mail_map.skills)), "|"),
				{struct, [
					{<<"address">>, list_to_binary(Mapping#mail_map.address)},
					{<<"queue">>, list_to_binary(Mapping#mail_map.queue)},
					{<<"client">>, list_to_binary(Client)},
					{<<"skills">>, list_to_binary(Skills)}
				]}
			end,
			Jarray = lists:map(Encode, Mappings),
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"items">>, Jarray}]})};
		_Else ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"no mappings">>}]})}
	end;
api({medias, _Node, "email_media_manager", "setMapping"}, ?COOKIE, Post) ->
	Translated = proplists:substitute_aliases([
		{"address", address},
		{"skills", skills},
		{"queue", queue},
		{"client", client}
	], Post),
	Skillslist = util:string_split(proplists:get_value(skills, Translated, []), "|"),
	Realskills = parse_posted_skills(Skillslist),
	Sanskills = proplists:delete(skills, Translated),
	Newprops = [{skills, Realskills} | Sanskills],
	case email_media_manager:set_mapping(proplists:get_value("oldaddress", Post), Newprops) of
		{aborted, Reason} ->
			Json = {struct, [{success, false}, {message, list_to_binary(lists:flatten(io_lib:format("~p", [Reason])))}]},
			{200, [], mochijson2:encode(Json)};
		{atomic, ok} ->
			{200, [], mochijson2:encode({struct, [{success, true}]})}
	end;
api({medias, _Node, "email_media_manager", "destroyMapping"}, ?COOKIE, Post) ->
	case email_media_manager:destroy_mapping(proplists:get_value("address", Post)) of
		{aborted, Reason} ->
			Json = {struct, [{success, false}, {message, list_to_binary(lists:flatten(io_lib:format("~p", [Reason])))}]},
			{200, [], mochijson2:encode(Json)};
		{atomic, ok} ->
			{200, [], mochijson2:encode({struct, [{success, true}]})}
	end;
api({medias, _Node, "email_media_manager", "new"}, ?COOKIE, Post) ->
	Translated = proplists:substitute_aliases([
		{"address", address},
		{"skills", skills},
		{"queue", queue},
		{"client", client}
	], Post),
	Sanskills = proplists:delete(skills, Translated),
	Newprops = [{skills, []} | Sanskills],
	case email_media_manager:new_mapping(Newprops) of
		{atomic, ok} ->
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		{aborted, Reason} ->
			Json = {struct, [{success, false}, {message, list_to_binary(lists:flatten(io_lib:format("~p", [Reason])))}]},
			{200, [], mochijson2:encode(Json)}
	end;

% =====
% clients => *
% =====

api({clients, "getDefault"}, ?COOKIE, _Post) ->
	Client = call_queue_config:get_client(undefined),
	Json = encode_client(Client),
	{200, [], mochijson2:encode(Json)};
api({clients, "setDefault"}, ?COOKIE, Post) ->
	Client = #client{
		label = undefined,
		id = undefined,
		options = [{url_pop, proplists:get_value("url_pop", Post, "")}]
	},
	call_queue_config:set_client(undefined, Client),
	{200, [], mochijson2:encode({struct, [{success, true}]})};
api({clients, "getClients"}, ?COOKIE, _Post) ->
	Clients = call_queue_config:get_clients(),
	Encoded = lists:map(fun(C) -> encode_client(C) end, Clients),
	Json = {struct, [
		{success, true},
		{identifier, <<"id">>},
		{items, Encoded}
	]},
	{200, [], mochijson2:encode(Json)};

%% =====
%% clients => Client => *
%% =====

api({clients, ClientId, "drop"}, ?COOKIE, _Post) ->
	case call_queue_config:destroy_client(ClientId) of
		{aborted, protected} ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"protected client">>}]})};
		{atomic, ok} ->
			{200, [], mochijson2:encode({struct, [{success, true}]})}
	end;
api({clients, "add"}, ?COOKIE, Post) ->
	case {proplists:get_value("id", Post), proplists:get_value("label", Post)} of
		{A, B} when A =:= undefined; B =:= undefined ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"invalid id or label">>}]})};
		{Id, Label} ->
			case call_queue_config:new_client(Label, Id, []) of
				{atomic, ok} ->
					{200, [], mochijson2:encode({struct, [{success, true}]})};
				Else ->
					?WARNING("Count not add client:  ~p", [Else]),
					{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"could not add client">>}]})}
			end
	end;
api({clients, ClientId, "set"}, ?COOKIE, Post) ->
	Label = proplists:get_value("label", Post),
	case call_queue_config:set_client(ClientId, Label, []) of
		{atomic, ok} ->
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		Else ->
			?WARNING("Count not set client:  ~p", [Else]),
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Count not set client">>}]})}
	end;

%% api catch-all
api(_, _, _) ->
	{404, [], <<"api command not found">>}.

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
				["", "dynamic" | Tail] ->
					File = string:join(Tail, "/"),
					case filelib:is_regular(string:concat("www/dynamic/", File)) of
						true ->
							{file, {File, "www/dynamic"}};
						false ->
							{api, {undefined, Path}}
					end;
				["", "agents", "modules", Action] ->
					{api, {agents, "modules", Action}};
				["", "agents", "spiceintegration", Action] ->
					{api, {agents, "spiceintegration", Action}};
				["", "agents", "profiles", Action] ->
					{api, {agents, "profiles", Action}};
				["", "agents", "profiles", Profile, Action] ->
					{api, {agents, "profiles", Profile, Action}};
				["", "agents", "agents", Action] ->
					{api, {agents, "agents", Action}};
				["", "agents", "agents", Agent, Action] ->
					{api, {agents, "agents", Agent, Action}};
				["", "skills", "groups", Action] ->
					{api, {skills, "groups", Action}};
				["", "skills", "groups", Group, Action] ->
					{api, {skills, "groups", Group, Action}};
				["", "skills", "skill", Action] ->
					{api, {skills, "skill", Action}};
				["", "skills", "skill", Skill, Action] ->
					{api, {skills, "skill", Skill, Action}};
				["", "queues", "groups", Action] ->
					{api, {queues, "groups", Action}};
				["", "queues", "groups", Group, Action] ->
					{api, {queues, "groups", Group, Action}};
				["", "queues", "queue", Queue, Action] ->
					{api, {queues, "queue", Queue, Action}};
				["", "queues", "queue", Action] ->
					{api, {queues, "queue", Action}};
				["", "medias", Action] ->
					{api, {medias, Action}};
				["", "medias", Node, Media, Action] ->
					{api, {medias, Node, Media, Action}};
				["", "clients", Action] ->
					{api, {clients, Action}};
				["", "clients", Client, Action] ->
					{api, {clients, Client, Action}};
				Allothers ->
					Adminpath = string:concat("www/admin", Path),
					Contribpath = string:concat("www/contrib", Path),
					case {filelib:is_regular(Adminpath), filelib:is_regular(Contribpath)} of
						{true, _} ->
							{file, {string:strip(Path, left, $/), "www/admin/"}};
						{false, true} ->
							{file, {string:strip(Path, left, $/), "www/contrib/"}};
						{false, false} ->
							{api, Allothers}
					end
			end
	end.

check_cookie([]) ->
	badcookie;
check_cookie(Allothers) ->
	case proplists:get_value("cpx_management", Allothers) of
		undefined ->
			?NOTICE("Cookie bad due to no cpx_management.", []),
			badcookie;
		Reflist ->
			case ets:lookup(cpx_management_logins, Reflist) of
				[] ->
					?NOTICE("Cookie bad reflist not in ets.", []),
					badcookie;
				[{Reflist, Salt, Login}] ->
					{Reflist, Salt, Login}
			end
	end.

get_pubkey() ->
	{ok,[Entry]} = public_key:pem_to_der("./key"),
	{ok,{'RSAPrivateKey', 'two-prime', N , E, _D, _P, _Q, _E1, _E2, _C, _Other}} =  public_key:decode_private_key(Entry),
	[E, N].

parse_posted_skills(PostedSkills) ->
	parse_posted_skills(PostedSkills, []).

parse_posted_skills([], Acc) ->
	Acc;
parse_posted_skills([Skill | Tail], Acc) ->
	{ok, Regex} = re:compile("^{(_\\w+),([-a-zA-Z0-9_ ]+)}$"),
	Newatom = case re:run(Skill, Regex, [{capture, all_but_first, list}]) of
		{match, [Atomstring, Expanded]} ->
			case call_queue_config:skill_exists(Atomstring) of
				undefined ->
					[];
				Atom ->
					{Atom, Expanded}
			end;
		nomatch ->
			case call_queue_config:skill_exists(Skill) of
				undefined ->
					[];
				Atom ->
					Atom
			end
	end,
	case Newatom of
		[] ->
			parse_posted_skills(Tail, Acc);
		_ ->
			parse_posted_skills(Tail, [Newatom | Acc])
	end.

decrypt_password(Password) ->
	{ok,[Entry]} = public_key:pem_to_der("./key"),
	{ok,{'RSAPrivateKey', 'two-prime', N , E, D, _P, _Q, _E1, _E2, _C, _Other}} =  public_key:decode_private_key(Entry),
	PrivKey = [crypto:mpint(E), crypto:mpint(N), crypto:mpint(D)],
	Bar = crypto:rsa_private_decrypt(util:hexstr_to_bin(Password), PrivKey, rsa_pkcs1_padding),
	binary_to_list(Bar).

encode_client(Client) ->
	Optionslist = encode_client_options(Client#client.options),
	{struct, [
		{<<"label">>, (case is_list(Client#client.label) of true -> list_to_binary(Client#client.label); false -> <<"">> end)},
		{<<"id">>, (case is_list(Client#client.id) of true -> list_to_binary(Client#client.id); false -> <<"">> end)},
		{<<"integration">>, Client#client.last_integrated},
		{<<"options">>, {struct, Optionslist}}
	]}.
		
encode_client_options(List) ->
	encode_client_options(List, []).

encode_client_options([], Acc) ->
	[{<<"_type">>, <<"json">>}, {<<"_value">>, {struct, lists:reverse(Acc)}}];
encode_client_options([{url_pop, Format} | Tail], Acc) ->
	encode_client_options(Tail, [{url_pop, list_to_binary(Format)} | Acc]);
encode_client_options([_Head | Tail], Acc) ->
	encode_client_options(Tail, Acc).

-type(raw_skill() :: atom() | {atom(), any()} | #skill_rec{}).

-spec(encode_skill/1 :: (Skill :: raw_skill()) -> simple_json()).
encode_skill(Atom) when is_atom(Atom), Atom =/= undefined ->
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
		{group, list_to_binary(Skill#skill_rec.group)},
		{expanded, Value}]};
encode_skill(Skill) when is_record(Skill, skill_rec) ->
	{struct, [{name, list_to_binary(Skill#skill_rec.name)},
		{type, skill}, {atom, Skill#skill_rec.atom},
		{description, list_to_binary(Skill#skill_rec.description)},
		{group, list_to_binary(Skill#skill_rec.group)},
		{protected, Skill#skill_rec.protected}]};
encode_skill(_) ->
	[].

-spec(encode_skills/1 :: (Skills :: [raw_skill()]) -> [simple_json()]).
encode_skills(Skills) ->
	encode_skills(Skills, []).

encode_skills([], Acc) ->
	lists:flatten(lists:reverse(Acc));
encode_skills([Skill|Skills], Acc) ->
	Head = encode_skill(Skill),
	encode_skills(Skills, [Head | Acc]).

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

encode_skills_simple(Skills) ->
	encode_skills_simple(Skills, []).

encode_skills_simple([], Acc) ->
	Acc;
encode_skills_simple([{Atom, Val} | Tail], Acc) ->
	NewVal = list_to_binary([${, atom_to_list(Atom), $,, Val, $}]),
	encode_skills_simple(Tail, [NewVal | Acc]);
encode_skills_simple([Atom | Tail], Acc) ->
	encode_skills_simple(Tail, [Atom | Acc]).
	
-spec(encode_queue/1 :: (Queue :: #call_queue{}) -> simple_json()).
encode_queue(Queue) ->
	SimplifySkills = fun
		({Atom, Value}) ->
			list_to_binary([${, atom_to_list(Atom), $,, Value, $}]);
		(Atom) when is_atom(Atom) ->
			Atom
	end,
	{struct, [{name, list_to_binary(Queue#call_queue.name)},
			{type, queue}, {weight, Queue#call_queue.weight},
			{skills, lists:map(SimplifySkills, Queue#call_queue.skills)},
			{recipe, {struct, [
				{<<"_type">>, <<"object">>},
				{<<"_value">>, encode_recipe(Queue#call_queue.recipe)}
			]}},
			{group, list_to_binary(Queue#call_queue.group)}]}.

-spec(encode_queues/1 :: (Queues :: [#call_queue{}]) -> [simple_json()]).
encode_queues(Queues) ->
	encode_queues(Queues, []).

encode_queues([], Acc) ->
	lists:reverse(Acc);
encode_queues([Queue | Queues], Acc) ->
	Head = encode_queue(Queue),
	encode_queues(Queues, [Head | Acc]).

encode_queues_with_groups(Groups) ->
	encode_queues_with_groups(Groups, []).

encode_queues_with_groups([], Acc) ->
	lists:reverse(Acc);
encode_queues_with_groups([Group | Groups], Acc) ->
	Queues = call_queue_config:get_queues(Group#queue_group.name),
	Head = {struct, [
		{name, list_to_binary(Group#queue_group.name)},
		{recipe, {struct, [
			{<<"_type">>, <<"object">>},
			{<<"_value">>, encode_recipe(Group#queue_group.recipe)}
		]}},
		{sort, Group#queue_group.sort},
		{protected, Group#queue_group.protected},
		{<<"type">>, <<"group">>},
		{queues, encode_queues(Queues)}]},
	encode_queues_with_groups(Groups, [Head | Acc]).

-spec(encode_agents/1 :: ([#agent{}]) -> [simple_json()]).
encode_agents([]) ->
	[];
encode_agents([Agent|Agents]) ->
	[encode_agent(Agent) | encode_agents(Agents)].

-spec(encode_agent/1 :: (Agentrec :: #agent_auth{}) -> simple_json()).
encode_agent(Agentrec) when is_record(Agentrec, agent_auth) ->
	{struct, [
		{name, list_to_binary(Agentrec#agent_auth.login)},
		{id, list_to_binary(Agentrec#agent_auth.id)},
		{<<"type">>, <<"agent">>},
		{login, list_to_binary(Agentrec#agent_auth.login)},
		{skills, encode_skills(Agentrec#agent_auth.skills)},
		{securitylevel, Agentrec#agent_auth.securitylevel},
		{integrated, Agentrec#agent_auth.integrated},
		{profile, list_to_binary(Agentrec#agent_auth.profile)},
		{lastname, list_to_binary(Agentrec#agent_auth.lastname)},
		{firstname, list_to_binary(Agentrec#agent_auth.firstname)}
	]}.
	
decode_recipe([Test | Tail]) when is_tuple(Test) ->
	decode_recipe([Test | Tail], []);
decode_recipe("[]") ->
	[];
decode_recipe(Json) ->
	Structed = mochijson2:decode(Json),
	decode_recipe(Structed).

decode_recipe([], Acc) ->
	lists:reverse(Acc);
decode_recipe([{struct, Proplist} | Tail], Acc) ->
	Action = case proplists:get_value(<<"action">>, Proplist) of
		<<"add_skills">> ->
			add_skills;
		<<"remove_skills">> ->
			remove_skills;
		<<"set_priority">> ->
			set_priority;
		<<"prioritize">> ->
			prioritize;
		<<"deprioritize">> ->
			deprioritize;
		<<"voicemail">> ->
			voicemail;
		<<"announce">> ->
			announce;
		<<"add_recipe">> ->
			add_recipe
	end,
	Args = decode_recipe_args(Action, proplists:get_value(<<"arguments">>, Proplist)),
	Runs = case proplists:get_value(<<"runs">>, Proplist) of
		<<"run_once">> ->
			run_once;
		<<"run_many">> ->
			run_many
	end,
	Conditions = decode_recipe_conditions(proplists:get_value(<<"conditions">>, Proplist)),
	decode_recipe(Tail, [{Conditions, Action, Args, Runs} | Acc]).

decode_recipe_args(add_skills, Args) ->
	decode_recipe_args(remove_skills, Args);
decode_recipe_args(remove_skills, Args) ->
	parse_posted_skills(lists:map(fun(I) -> binary_to_list(I) end, Args));
%	F = fun(Bin) ->
%		case call_queue_config:skill_exists(binary_to_list(Bin)) of
%			undefined ->
%				erlang:error(bararg, Bin);
%			Atom ->
%				Atom
%		end
%	end,
%	lists:map(F, Args);
decode_recipe_args(set_priority, Args) ->
	list_to_integer(binary_to_list(Args));
decode_recipe_args(prioritize, _Args) ->
	[];
decode_recipe_args(deprioritize, _Args) ->
	[];
decode_recipe_args(voicemail, Args) ->
	binary_to_list(Args);
decode_recipe_args(announce, Args) ->
	binary_to_list(Args);
decode_recipe_args(add_recipe, _Args) ->
	% TODO add support
	[].

decode_recipe_conditions(Conds) ->
	decode_recipe_conditions(Conds, []).

decode_recipe_conditions([], Acc) ->
	lists:reverse(Acc);
decode_recipe_conditions([{struct, Props} | Tail], Acc) ->
	Cond = proplists:get_value(<<"property">>, Props),
	Comp = case proplists:get_value(<<"comparison">>, Props) of
		<<"=">> ->
			'=';
		<<">">> ->
			'>';
		<<"<">> ->
			'<';
		<<"!=">> ->
			'!='
	end,
	Val = case proplists:get_value(<<"value">>, Props) of
		V when is_integer(V) ->
			V;
		V when is_float(V) ->
			V;
		V when is_binary(V) ->
			binary_to_list(V);
		V ->
			V
	end,
%	Toint = fun(L) ->
%		list_to_integer(L)
%	end,
	Tuple = case {Cond, Comp, Val} of
		{<<"ticks">>, '=', Val} when is_integer(Val) ->
			{ticks, Val};
		{<<"eligible_agents">>, Comp, Val} when is_integer(Val) ->
			{eligible_agents, Comp, Val};
		{<<"agents_avail">>, Comp, Val} when is_integer(Val) ->
			{available_agents, Comp, Val};
		{<<"queue_position">>, Comp, Val} when is_integer(Val) ->
			{queue_position, Comp, Val};
		{<<"calls_queued">>, Comp, Val} when is_integer(Val) ->
			{calls_queued, Comp, Val};
		{<<"client">>, Comp, Val} ->
			{client, Comp, Val};
		{<<"hour">>, Comp, Val} when is_integer(Val) ->
			{hour, Comp, Val};
		{<<"weekday">>, Comp, Val} when is_integer(Val) ->
			{weekday, Comp, Val};
		{<<"mediatype">>, Comp, Val} ->
			{type, Comp, Val}
	end,
	decode_recipe_conditions(Tail, [Tuple | Acc]).	
	
encode_recipe(Recipe) ->
	encode_recipe_steps(Recipe).

encode_recipe_steps(Steps) ->
	encode_recipe_steps(Steps, []).

encode_recipe_steps([], Acc) ->
	lists:reverse(Acc);
encode_recipe_steps([Step | Tail], Acc) ->
	Jstep = encode_recipe_step(Step),
	encode_recipe_steps(Tail, [Jstep | Acc]).

encode_recipe_step({Conditions, Action, Args, Runs}) ->
	Jcond = encode_recipe_conditions(Conditions),
	Jargs = case Action of
		add_skills ->
			encode_skills_simple(Args);
		remove_skills ->
			encode_skills_simple(Args);
		set_priority ->
			Args;
		prioritize ->
			<<"">>;
		deprioritize ->
			<<"">>;
		voicemail ->
			<<"">>;
		announce ->
			list_to_binary(Args);
		add_recipe ->
			% TODO:  more encoding
			<<"">>
	end,
	{struct, [
		{<<"conditions">>, Jcond},
		{<<"action">>, Action},
		{<<"arguments">>, Jargs},
		{<<"runs">>, Runs}
	]}.

encode_recipe_conditions(Conditions) ->
	encode_recipe_conditions(Conditions, []).

encode_recipe_conditions([], Acc) ->
	lists:reverse(Acc);
encode_recipe_conditions([{ticks, Num} | Tail], Acc) ->
	encode_recipe_conditions([{ticks, '=', Num} | Tail], Acc);
encode_recipe_conditions([{type, Comp, Num} | Tail], Acc) ->
	encode_recipe_conditions([{<<"mediatype">>, Comp, Num} | Tail], Acc);
encode_recipe_conditions([{Prop, Comp, Val} | Tail], Acc) ->
	Fixedval = case Val of
		Val when is_integer(Val) ->
			Val;
		Val when is_list(Val) ->
			list_to_binary(Val)
	end,
	Jcond = {struct, [
		{<<"property">>, Prop},
		{<<"comparison">>, Comp},
		{<<"value">>, Fixedval}
	]},
	encode_recipe_conditions(Tail, [Jcond | Acc]).

encode_medias([], Acc) ->
	lists:reverse(Acc);
encode_medias([{Node, Medias} | Tail], Acc) ->
	Json = {struct, [
		{<<"name">>, list_to_binary(atom_to_list(Node))},
		{<<"type">>, <<"node">>},
		{<<"id">>, list_to_binary(atom_to_list(Node))},
		{<<"medias">>, encode_medias_confs(Node, Medias, [])}
	]},
	encode_medias(Tail, [Json | Acc]).

encode_medias_confs(_Node, [], Acc) ->
	lists:reverse(Acc);
encode_medias_confs(Node, [{cpx_supervisor, BadConf} | Tail], Acc) ->
	Conf = case BadConf of
		none ->
			"";
		{ok, Else} ->
			Else
	end,
	Json = {struct, [
		{<<"id">>, list_to_binary(atom_to_list(Node) ++ "/cpx_supervisor")},
		{<<"mediatype">>, <<"cpx_supervisor">>},
		{<<"name">>, <<"cpx_supervisor">>},
		{<<"type">>, <<"conf">>},
		{<<"archivepath">>, list_to_binary(Conf)},
		{<<"enabled">>, true},
		{<<"node">>, list_to_binary(atom_to_list(Node))}
	]},
	encode_medias_confs(Node, Tail, [Json | Acc]);
encode_medias_confs(Node, [{Mod, Conf} | Tail], Acc) when is_record(Conf, cpx_conf) ->
	Json = {struct, [
		{<<"name">>, list_to_binary(atom_to_list(Conf#cpx_conf.module_name))},
		{<<"enabled">>, true},
		{<<"type">>, <<"conf">>},
		{<<"id">>, list_to_binary(atom_to_list(Node) ++ "/" ++ atom_to_list(Mod))},
		{<<"mediatype">>, list_to_binary(atom_to_list(Mod))},
		{<<"start">>, list_to_binary(atom_to_list(Conf#cpx_conf.start_function))},
		{<<"node">>, list_to_binary(atom_to_list(Node))}
	]},
	encode_medias_confs(Node, Tail, [Json | Acc]);
encode_medias_confs(Node, [{Mod, undefined} | Tail], Acc) ->
	Json = {struct, [
		{<<"name">>, list_to_binary(atom_to_list(Mod))},
		{<<"enabled">>, false},
		{<<"mediatype">>, list_to_binary(atom_to_list(Mod))},
		{<<"type">>, <<"conf">>},
		{<<"id">>, list_to_binary(atom_to_list(Node) ++ "/" ++ atom_to_list(Mod))},
		{<<"node">>, list_to_binary(atom_to_list(Node))}
	]},
	encode_medias_confs(Node, Tail, [Json | Acc]).

%% =====
%% tests
%% =====

-ifdef(TEST).

rec_equals(	#agent_auth{login = Ln, password = Pw, skills = SK, securitylevel = SL, integrated = In, profile = Prof, firstname = Fn, lastname = Lastn},
			#agent_auth{login = Ln, password = Pw, skills = SK, securitylevel = SL, integrated = In, profile = Prof, firstname = Fn, lastname = Lastn}) ->
	true;
rec_equals(	#agent_profile{name = Nom, skills = Skills},
			#agent_profile{name = Nom, skills = Skills}) ->
	true;
rec_equals(	#release_opt{label = Lb, id = Id, bias = Bias},
			#release_opt{label = Lb, id = Id, bias = Bias}) ->
	true;
rec_equals(	#cpx_conf{id = A, module_name = B, start_function = C, start_args = D, supervisor = E},
			#cpx_conf{id = A, module_name = B, start_function = C, start_args = D, supervisor = E}) ->
	true;
rec_equals(	#client{label = A, id = B},
			#client{label = A, id = B}) ->
	true;
rec_equals(	#call_queue{name = A, weight = B, skills = C, recipe = D, hold_music = E, group = F},
			#call_queue{name = A, weight = B, skills = C, recipe = D, hold_music = E, group = F}) ->
	true;
rec_equals(	#queue_group{name = A, recipe = B, sort = C, protected = D},
			#queue_group{name = A, recipe = B, sort = C, protected = D}) ->
	true;
rec_equals(	#mail_map{address = A, queue = B, skills = C, client = D},
			#mail_map{address = A, queue = B, skills = C, client = D}) ->
	true;
rec_equals(	#skill_rec{atom = A, name = B, protected = C, description = D, group = E},
			#skill_rec{atom = A, name = B, protected = C, description = D, group = E}) ->
	true;
rec_equals(_A, _B) ->
	false.

cookie_test_() ->
	{setup,
	fun() ->
		ets:new(cpx_management_logins, [set, public, named_table]),
		ok
	end,
	fun(ok) ->
		ets:delete(cpx_management_logins)
	end,
	[
		{"A blank cookie",
		fun() ->
			?assertEqual(badcookie, check_cookie([]))
		end},
		{"A cookie, but not in the ets",
		fun() ->
			?assertEqual(badcookie, check_cookie([{"cpx_management", erlang:ref_to_list(make_ref())}]))
		end},
		{"A cookie that is in the ets",
		fun() ->
			ets:insert(cpx_management_logins, {"ref", "salt", "login"}),
			?assertEqual({"ref", "salt", "login"}, check_cookie([{"cpx_management", "ref"}]))
		end}
	]}.

api_test_() ->
	{foreach,
	fun() -> 
		crypto:start(),
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		mnesia:create_schema([node()]),
		mnesia:start(),
		cpx_supervisor:start([node()]),
		% add a fake login for easy testing
		Cookie = {"ref", "salt", "login"},
		ets:insert(cpx_management_logins, {"ref", "salt", "login"}),

		Cookie
	end,
	fun(_Whatever) -> 
		cpx_supervisor:stop(),
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		ok
	end,
	[
		fun(Cookie) ->
			{"/checkcookie with value data",
			fun() ->
				Expected = 	{200, [], mochijson2:encode({struct, [{<<"success">>, true}, {<<"login">>, list_to_binary("login")}]})},
				Apires = api(checkcookie, Cookie, []),
				?assertEqual(Expected, Apires)
			end}
		end,
		fun(Cookie) ->
			{"/getsalt",
			fun() ->
				{200, [], Json} = api(getsalt, Cookie, []),
				{struct, Props} = mochijson2:decode(Json),
				?assertNot(undefined =:= proplists:get_value(<<"salt">>, Props)),
				?assertEqual(<<"Salt created, check salt property">>, proplists:get_value(<<"message">>, Props))
			end}
		end,
		fun(Cookie) ->
			{"/agents/modules/set disabling all",
			fun() ->
				{atomic, {ok, Atcppid}} = cpx_supervisor:update_conf(agent_tcp_listener, #cpx_conf{
					id = agent_tcp_listener,
					module_name = agent_tcp_listener, 
					start_function = start_link, 
					start_args = [51337], 
					supervisor = agent_connection_sup
				}),
				?assert(is_pid(Atcppid)),
				?assert(is_pid(whereis(agent_web_listener))),
				{200, [], Json} = api({agents, "modules", "update"}, Cookie, []),
				{struct, Props} = mochijson2:decode(Json),
				?assertEqual(true, proplists:get_value(<<"success">>, Props)),
				?assertEqual(undefined, cpx_supervisor:get_conf(agent_tcp_listener)),
				?assertEqual(undefined, cpx_supervisor:get_conf(agent_web_listener))
			end}
		end,
		fun(Cookie) ->
			{"/agents/modules/set enabling only tcp",
			fun() ->
				{200, [], _Json} = api({agents, "modules", "update"}, Cookie, [{"agentModuleTCPListen", "5678"}]),
				?assertMatch({ok, _Socket}, gen_tcp:connect(net_adm:localhost(), 5678, [list])),
				?assertNot(is_pid(whereis(agent_web_listener)))
			end}
		end,
		fun(Cookie) ->
			{"/agents/modules/set enabling only web",
			fun() ->
				{200, [], _Json} = api({agents, "modules", "update"}, Cookie, [{"agentModuleWebListen", "8787"}]),
				?assert(is_pid(whereis(agent_web_listener))),
				?assertEqual(undefined, cpx_supervisor:get_conf(agent_tcp_listener))
			end}
		end,
		fun(Cookie) ->
			{"/agents/modules/set enabling both",
			fun() ->
				{200, [], _Json} = api({agents, "modules", "update"}, Cookie, [{"agentModuleTCPListen", "8765"}, {"agentModuleWebListen", "8787"}]),
				?assert(is_pid(whereis(agent_web_listener))),
				?assertMatch({ok, _Socket}, gen_tcp:connect(net_adm:localhost(), 8765, [list]))
			end}
		end,
		fun(Cookie) ->
			{"/agents/profiles/new New profile",
			fun() ->
				{200, [], _Json} = api({agents, "profiles", "new"}, Cookie, [{"name", "newprofile"}, {"skills", "_all"}, {"skills", "english"}]),
				{Name, Skills} = agent_auth:get_profile("newprofile"),
				?assertEqual("newprofile", Name),
				?assert(lists:member('_all', Skills)),
				?assert(lists:member(english, Skills))
			end}
		end,
		fun(Cookie) ->
			{"/agents/profiles/Default/delete does nothing",
			fun() ->
				{200, [], _Json} = api({agents, "profiles", "Default", "delete"}, Cookie, []),
				?assertEqual({"Default", []}, agent_auth:get_profile("Default"))
			end}
		end,
		fun(Cookie) ->
			{"/agents/profiles/someprofile/delete kills the profile",
			fun() ->
				?CONSOLE("~p", [agent_auth:new_profile("someprofile", [])]),
				?assertEqual({"someprofile", []}, agent_auth:get_profile("someprofile")),
				{200, [], _Json} = api({agents, "profiles", "someprofile", "delete"}, Cookie, []),
				?assertEqual(undefined, agent_auth:get_profile("someprofile"))
			end}
		end,
		fun(Cookie) ->
			{"/agents/profiles/someprofile/update updates the profile, and corrects the agents",
			fun() ->
				agent_auth:new_profile("someprofile", ['_all', english]),
				agent_auth:add_agent("someagent", "", [], agent, "someprofile"),
				{200, [], _Json} = api({agents, "profiles", "someprofile", "update"}, Cookie, [{"name", "newprofile"}, {"skills", "_all"}]),
				?assertEqual({"newprofile", ['_all']}, agent_auth:get_profile("newprofile")),
				?assertEqual(undefined, agent_auth:get_profile("someprofile")),
				{atomic, [Agent]} = agent_auth:get_agent("someagent"),
				?assertEqual("newprofile", Agent#agent_auth.profile)
			end}
		end,
		fun(Cookie) ->
			{"/agents/agents/new adds a new agent (all fields right)",
			fun() ->
				?assertEqual({atomic, []}, agent_auth:get_agent("someagent")),
				Post = [
					{"password", "goober"},
					{"confirm", "goober"},
					{"skills", "english"},
					{"skills", "{_brand,Somebrand}"},
					{"login", "someagent"},
					{"security", "agent"},
					{"profile", "Default"}
				],
				{200, [], _Json} = api({agents, "agents", "new"}, Cookie, Post),
				{atomic, [Rec]} = agent_auth:get_agent("someagent"),
				?CONSOLE("~p", [Rec#agent_auth.skills]),
				?assertEqual(true, lists:member(english, Rec#agent_auth.skills)),
				?assertEqual(true, lists:member({'_brand', "Somebrand"}, Rec#agent_auth.skills)),
				agent_auth:destroy("someagent")
			end}
		end,
		fun(Cookie) ->
			{"/agents/agents/new adds a new agent (password mismatch)",
			fun() ->
				?assertEqual({atomic, []}, agent_auth:get_agent("someagent")),
				Post = [
					{"password", "goober"},
					{"confirm", "typoed"},
					{"skills", "english"},
					{"skills", "{_brand,Somebrand}"},
					{"login", "someagent"},
					{"security", "agent"},
					{"profile", "Default"}
				],
				?assertError({case_clause,"goober"}, api({agents, "agents", "new"}, Cookie, Post)),
				%{200, [], Json} = api({agents, "agents", "new"}, Cookie, Post),
				%?assertEqual({atomic, []}, agent_auth:get_agent("someagent")),
				agent_auth:destroy("someagent")
			end}
		end,
		fun(Cookie) ->
			{"/agents/agents/someagent/delete deleting an agent",
			fun() ->
				agent_auth:add_agent("someagent", "somepassword", [], agent, "Default"),
				{atomic, [Rec]} = agent_auth:get_agent("someagent"),
				{200, [], _Json} = api({agents, "agents", Rec#agent_auth.id, "delete"}, Cookie, []),
				?assertEqual({atomic, []}, agent_auth:get_agent("someagent"))
			end}
		end,
		fun(Cookie) ->
			{"/agents/agents/someagent/update updating an agent, but not password",
			fun() ->
				agent_auth:add_agent("someagent", "somepassword", [], supervisor, "Default"),
				{atomic, [Oldrec]} = agent_auth:get_agent("someagent"),
				Post = [
					{"skills", "{_brand,Somebrand}"},
					{"skills", "english"},
					{"password", ""},
					{"confirm", ""},
					{"security", "agent"},
					{"profile", "Default"},
					{"login", "renamed"}
				],
				api({agents, "agents", Oldrec#agent_auth.id, "update"}, Cookie, Post),
				?assertEqual({atomic, []}, agent_auth:get_agent("someagent")),
				{atomic, [Rec]} = agent_auth:get_agent("renamed"),
				?assertEqual(agent, Rec#agent_auth.securitylevel),
				?assert(lists:member(english, Rec#agent_auth.skills)),
				?assert(lists:member({'_brand', "Somebrand"}, Rec#agent_auth.skills)),
				?assertEqual(Oldrec#agent_auth.password, Rec#agent_auth.password),
				agent_auth:destroy("renamed")
			end}
		end,
		fun(Cookie) ->
			{"/agents/agents/someagent/update updating an agent and password",
			fun() ->
				agent_auth:add_agent("someagent", "somepassword", [], supervisor, "Default"),
				{atomic, [Oldrec]} = agent_auth:get_agent("someagent"),
				Post = [
					{"skills", "{_brand,Somebrand}"},
					{"skills", "english"},
					{"password", "newpass"},
					{"confirm", "newpass"},
					{"security", "agent"},
					{"profile", "Default"},
					{"login", "renamed"}
				],
				api({agents, "agents", Oldrec#agent_auth.id, "update"}, Cookie, Post),
				?assertEqual({atomic, []}, agent_auth:get_agent("someagent")),
				{atomic, [Rec]} = agent_auth:get_agent("renamed"),
				?assertEqual(agent, Rec#agent_auth.securitylevel),
				?assert(lists:member(english, Rec#agent_auth.skills)),
				?assert(lists:member({'_brand', "Somebrand"}, Rec#agent_auth.skills)),
				?assertNot(Oldrec#agent_auth.password =:= Rec#agent_auth.password),
				agent_auth:destroy("renamed")
			end}
		end,
		fun(Cookie) ->
			{"/agents/agents/someagent/update updating an agent fails w/ password mismtach",
			fun() ->
				agent_auth:add_agent("someagent", "somepassword", [], supervisor, "Default"),
				{atomic, [Oldrec]} = agent_auth:get_agent("someagent"),
				Post = [
					{"skills", "{_brand,Somebrand}"},
					{"skills", "english"},
					{"password", "newpass"},
					{"confirm", "typoed"},
					{"security", "agent"},
					{"profile", "Default"},
					{"login", "renamed"}
				],
				?assertError({case_clause, "newpass"}, api({agents, "agents", Oldrec#agent_auth.id, "update"}, Cookie, Post)),
				?assertEqual({atomic, []}, agent_auth:get_agent("renamed")),
				{atomic, [Rec]} = agent_auth:get_agent("someagent"),
				?assertEqual(supervisor, Rec#agent_auth.securitylevel),
				?assertNot(lists:member(english, Rec#agent_auth.skills)),
				?assertNot(lists:member({'_brand', "Somebrand"}, Rec#agent_auth.skills)),
				?assert(Oldrec#agent_auth.password =:= Rec#agent_auth.password),
				agent_auth:destroy("someagent")
			end}
		end,
		fun(Cookie) ->
			{"/agents/agents/someagent/update updating an agent fails w/o password change",
			fun() ->
				agent_auth:add_agent("someagent", "somepassword", [], supervisor, "Default"),
				{atomic, [Oldrec]} = agent_auth:get_agent("someagent"),
				Post = [
					{"skills", "{_brand,Somebrand}"},
					{"skills", "english"},
					{"password", ""},
					{"confirm", ""},
					{"security", "agent"},
					{"profile", "Default"},
					{"login", "renamed"}
				],
				api({agents, "agents", Oldrec#agent_auth.id, "update"}, Cookie, Post),
				?assertEqual({atomic, []}, agent_auth:get_agent("someagent")),
				{atomic, [Rec]} = agent_auth:get_agent("renamed"),
				?assertEqual(agent, Rec#agent_auth.securitylevel),
				?assert(lists:member(english, Rec#agent_auth.skills)),
				?assert(lists:member({'_brand', "Somebrand"}, Rec#agent_auth.skills)),
				?assert(Oldrec#agent_auth.password =:= Rec#agent_auth.password),
				agent_auth:destroy("renamed")
			end}
		end,
		fun(Cookie) ->
			{"/skills/skill/new Creating a skill",
			fun() ->
				Post = [
					{"atom", "testskill"},
					{"name", "Test Skill"},
					{"description", "Test skill for gooberness"},
					{"group", "Magic"}
				],
				api({skills, "skill", "new"}, Cookie, Post),
				Rec = call_queue_config:get_skill(testskill),
				Testrec = #skill_rec{
					atom = testskill,
					name = "Test Skill",
					protected = false,
					description = "Test skill for gooberness",
					group = "Magic"},
				?assert(rec_equals(Testrec, Rec)),
				F = fun() ->
					mnesia:delete({skill_rec, testskill})
				end,
				mnesia:transaction(F)
			end}
		end,
		fun(Cookie) ->
			{"/skills/skill/testskill/update Updating a skill",
			fun() ->
				Oldrec = #skill_rec{
					atom = testskill,
					name = "Test Skill",
					protected = false,
					description = "Test skill for gooberness",
					group = "Magic"},
				call_queue_config:new_skill(Oldrec),
				Post = [
					{"name", "New Name"},
					{"description", "Blah blah blah"},
					{"group", "Languages"}
				],
				Newrec = #skill_rec{
					atom = testskill,
					name = "New Name",
					protected = false,
					description = "Blah blah blah",
					group = "Languages"},
				api({skills, "skill", "testskill", "update"}, Cookie, Post),
				?assert(rec_equals(Newrec, call_queue_config:get_skill(testskill))),
				F = fun() ->
					mnesia:delete({skill_rec, testskill})
				end,
				mnesia:transaction(F)
			end}
		end,
		fun(Cookie) ->
			{"/queues/groups/new Creating queue group",
			fun() ->
				Recipe = [{[{ticks, 5}], prioritize, [], run_many}],
				Jrecipe = mochijson2:encode(encode_recipe(Recipe)),
				Post = [
					{"name", "Test Q Group"},
					{"sort", "27"},
					{"recipe", Jrecipe}
				],
				?assertEqual({atomic, []}, call_queue_config:get_queue_group("Test Q Group")),
				api({queues, "groups", "new"}, Cookie, Post),
				{atomic, [Rec]} = call_queue_config:get_queue_group("Test Q Group"),
				Testrec = #queue_group{
					name = "Test Q Group",
					recipe = Recipe,
					sort = 27,
					protected = false
				},
				?assert(rec_equals(Testrec, Rec)),
				call_queue_config:destroy_queue_group("Test Q Group")
			end}
		end,
		fun(Cookie) ->
			{"/queus/groups/Test Q Group/update Updating a queue group",
			fun() ->
				Recipe = [{[{ticks, 5}], prioritize, [], run_many}],
				Qgrouprec = #queue_group{
					name = "Test Q Group",
					recipe = Recipe,
					sort = 35
				},
				call_queue_config:new_queue_group(Qgrouprec),
				Newrecipe = [{[{calls_queued, '<', 200}], deprioritize, [], run_once}],
				Newjrecipe = mochijson2:encode(encode_recipe(Newrecipe)),
				Post = [
					{"name", "Renamed Q Group"},
					{"sort", "27"},
					{"recipe", Newjrecipe}
				],
				api({queues, "groups", "Test Q Group", "update"}, Cookie, Post),
				{atomic, [Rec]} = call_queue_config:get_queue_group("Renamed Q Group"),
				Testrec = #queue_group{
					name = "Renamed Q Group",
					recipe = Newrecipe,
					sort = 27,
					protected = false
				},
				?assertNot(rec_equals(Qgrouprec, Rec)),
				?DEBUG("~p", [Rec]),
				?assert(rec_equals(Testrec, Rec)),
				?assertEqual({atomic, []}, call_queue_config:get_queue_group("Test Q Group")),
				call_queue_config:destroy_queue_group("Renamed Q Group")
			end}
		end,
		fun(Cookie) ->
			{"/queues/groups/Test Q Group/delete Destroying queue group",
			fun() ->
				Recipe = [{[{ticks, 5}], prioritize, [], run_many}],
				Qgrouprec = #queue_group{
					name = "Test Q Group",
					recipe = Recipe,
					sort = 35
				},
				call_queue_config:new_queue_group(Qgrouprec),
				?assertMatch({atomic, [_Rec]}, call_queue_config:get_queue_group("Test Q Group")),
				api({queues, "groups", "Test Q Group", "delete"}, Cookie, []),
				?assertEqual({atomic, []}, call_queue_config:get_queue_group("Test Q Group"))
			end}
		end,
		fun(Cookie) ->
			{"/queues/queue/new Add a queue",
			fun() ->
				Post = [
					{"name", "test queue"},
					{"recipe", "[]"},
					{"weight", "1"},
					{"group", "Default"}
				],
				Q = #call_queue{
					name = "test queue",
					skills = [], 
					recipe = []
				},
				api({queues, "queue", "new"}, Cookie, Post),
				?assert(rec_equals(Q, call_queue_config:get_queue("test queue"))),
				call_queue_config:destroy_queue("test queue")
			end}
		end,
		fun(Cookie) ->
			{"/queues/queue/test queue/update Update an existing queue",
			fun() ->
				Post = [
					{"name", "test name"},
					{"recipe", "[]"},
					{"weight", "57"},
					{"group", "Group"},
					{"skills", "english"}
				],
				Q = #call_queue{
					name = "test name",
					skills = [english],
					recipe = [],
					weight = 57,
					group = "Group"
				},
				call_queue_config:new_queue("test queue", 1, [], [], "Default"),
				api({queues, "queue", "test queue", "update"}, Cookie, Post),
				?assertEqual(noexists, call_queue_config:get_queue("test queue")),
				?assert(rec_equals(Q, call_queue_config:get_queue("test name")))
			end}
		end,
		fun(Cookie) ->
			{"/queues/queue/test queue/delete Delete an existing queue",
			fun() ->
				call_queue_config:new_queue("test queue", 1, [], [], "Default"),
				api({queues, "queue", "test queue", "delete"}, Cookie, []),
				?assertEqual(noexists, call_queue_config:get_queue("test queue"))
			end}
		end%,
		%% for the next 2 tests to be meaningful, freeswitch needs to be running.
		%fun(Cookie) ->
%			{timeout,
%			120,
%			[
%				{"/medias/Thisnode/freeswitch_media_manager/update disabling",
%				fun() ->
%					Fsconf = #cpx_conf{
%						id = freeswitch_media_manager,
%						module_name = freeswitch_media_manager,
%						start_function = start_link,
%						start_args = [freeswitch@localhost, "localhost"],
%						supervisor = management_sup},
%					cpx_supervisor:destroy(freeswitch_media_manager),
%					?CONSOLE("post destroy", []),
%					cpx_supervisor:add_conf(Fsconf),
%					?CONSOLE("post add", []),
%					Res = cpx_supervisor:get_conf(freeswitch_media_manager),
%					?assert(rec_equals(Fsconf, Res)),
%					?CONSOLE("post get", []),
%					api({medias, atom_to_list(node()), "freeswitch_media_manager", "update"}, Cookie, []),
%					?CONSOLE("post api", []),
%					?assertEqual(undefined, cpx_supervisor:get_conf(freeswitch_media_manager)),
%					?CONSOLE("post assertEqual", []),
%					cpx_supervisor:destroy(freeswitch_media_manager)
%				end}
%			]}
%		end,
%		fun(Cookie) ->
%			{timeout,
%			120,
%			[
%				{"/medias/Thisnode/freeswitch_media_manager/update enabling",
%				fun() ->
%					?assertEqual(undefined, cpx_supervisor:get_conf(freeswitch_media_manager)),
%					Post = [
%						{"enabled", "some data whichd oesn't matter"},
%						{"cnode", "freeswitch@localhost"},
%						{"domain", "localhost"},
%						{"voicegw", "whatever"}
%					],
%					Fsconf = #cpx_conf{
%						id = freeswitch_media_manager,
%						module_name = freeswitch_media_manager,
%						start_function = start_link,
%						start_args = [freeswitch@localhost, [{voicegateway, "whatever"}]],
%						supervisor = management_sup},
%					api({medias, atom_to_list(node()), "freeswitch_media_manager", "update"}, Cookie, Post),
%					Res = cpx_supervisor:get_conf(freeswitch_media_manager),
%					?assert(rec_equals(Fsconf, Res)),
%					cpx_supervisor:destroy(freeswitch_media_manager)
%				end}
%			]}
%		end
	]}.			
		
-endif.
