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

%% @doc The web management module.  Uses mochiweb for the heavy lifting. 
%%  Listens on port 9999 by default.
%%
%% Handles json api requests on /api.  A well formed api request is a post
%% with one field:  "request".  "request" should consist of a json object
%% of three properties:  module, function, and args.  the module and 
%% function should both be strings.  Args is a json representation of
%% what the erlang module:function() would accept as arguments (as best as
%% can be represented in json).  Proplists should be represented as json 
%% objects, and will be decoded as such.  For any function of arity 1 
%% (takes one argument), the args should be that argument.  Otherwise it 
%% should be a list/array.
%%
%% The json_api is always enabled, but protected behind http basic 
%% authentication and an ip whitelist.  If either of those conditions are
%% met, the api call is allowed.  Obviously this is not something you want
%% to expose to the internet.
%% 
%% An error is generated if either the module or function do not exist, 
%% the function is not exported by the module.
%% 
%% The currently exported funtions are listed below.  In theory, there is
%% documentation on therm either in the auto generated docs, the built-in
%% documentation, or the wiki.
%% <ul><li>cpx:kick_agent("agent_login")</li>
%% <li> agent_manager:list()</li>
%% <li> freeswitch_dialer:start_fg/1</li>
%% <li> freeswitch_dialer:start_fg/6</li>
%% <li> freeswitch_monitor:monitor_agent("agent_login", 
%% "spy/dialstring", "freeswith@nodename")</li>
%% <li> freeswitch_monitor:monitor_client("client_label", 
%% "spy/dialstring", "freeswitch@nodename")</li></ul>

-module(cpx_web_management).
-author("Micah").

-ifdef(TEST).
-define(PORT, 59999).
-else.
-define(PORT, 9999).
-endif.
-define(WEB_DEFAULTS, [{name, ?MODULE}, {port, ?PORT}]).
-define(COOKIE, {_Reflist, _Salt, _Login}).
-define(DEFAULT_RINGOUT, 60).

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

-export([start_link/0, start_link/1, start/0, start/1, stop/0, loop/2]).

-export([
	encode_skill/1,
	encode_skills/1,
	encode_queue/1,
	encode_queues/1,
	encode_agent/1,
	encode_agents/1
]).
-export([
	parse_posted_skills/1
]).

%% DEBUG
-export([api/3, decode_endpoints/1]).

-type(simple_json() :: {'struct', [{atom() | binary(), atom() | binary()}]}).

%% @doc Start the web management server unlinked to the parent process.
-spec(start/0 :: () -> {'ok', pid()}).
start() ->
	start([]).

-spec(start/1 :: (Port :: pos_integer()) -> {'ok', pid()}).
start(Port) when is_integer(Port), Port > 1023 ->
	start([{port, Port}]);
start(Opts) ->
	Port = proplists:get_value(port, Opts, ?PORT),
	ProtoAuths = proplists:get_value(http_basic_peers, Opts, []),
	Midopts = proplists:delete(http_basic_peers, Opts),
	Auths = [base64:encode_to_string(User ++ ":" ++ Pass) || {User, Pass} <- ProtoAuths ],
	NewOpts = [{http_basic_peers, Auths} | Midopts],
	?DEBUG("Starting mochiweb...", []),
	case ets:info(cpx_management_logins) of
		undefined ->
			ets:new(cpx_management_logins, [set, public, named_table]);
		Else when is_list(Else) ->
			?DEBUG("looks like the table exists already", []),
			ok
	end,
	F = fun(Req) ->
		?MODULE:loop(Req, NewOpts)
	end,
	case proplists:get_value(ssl, Opts) of
		undefined ->
			mochiweb_http:start([{loop, F}, {name, ?MODULE}, {port, Port}]);
		true ->
			mochiweb_http:start([
				{loop, F},
				{name, ?MODULE},
				{port, Port},
				{ssl, true},
				{ssl_opts, [
					{certfile, util:get_certfile()},
					{keyfile, util:get_keyfile()}
				]}
			])
	end.

-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() ->
	start_link([]).

-spec(start_link/1 :: (Port :: pos_integer()) -> {'ok', pid()}).
start_link(Port) when is_integer(Port), Port > 1023 ->
	start_link([{port, Port}]);
start_link(Opts) ->
	{ok, Pid} = Out = start(Opts),
	link(Pid),
	Out.

%% @doc Stops the web management.
-spec(stop/0 :: () -> 'ok').
stop() -> 
	ets:delete(cpx_management_logins),
	mochiweb_http:stop(?MODULE).

file_handler(Name, ContentType) ->
	fun(N) -> file_data_handler(N, {Name, ContentType, <<>>}) end.

file_data_handler(eof, {Name, _ContentType, Acc}) ->
	?DEBUG("eof gotten", []),
	{Name, Acc};
file_data_handler(Data, {Name, ContentType, Acc}) ->
	Newacc = <<Acc/binary, Data/binary>>,
	fun(N) -> file_data_handler(N, {Name, ContentType, Newacc}) end.

-spec(loop/2 :: (Req :: atom(), Opts :: [any()]) -> any()).
loop(Req, Opts) ->
	Path = Req:get(path),
	Post = case Req:get_primary_header_value("content-type") of
		"application/x-www-form-urlencoded" ++ _ ->
			Req:parse_post();
		_ ->
			%% TODO Change this to a custom parser rather than mochi's default.
			try mochiweb_multipart:parse_form(Req, fun file_handler/2) of
				Whoa ->
					?NOTICE("whoa:  ~p", [Whoa]),
					Whoa
			catch
				_:_ ->
					%?DEBUG("Going with a blank post due to mulipart parse fail:  ~p:~p", [What, Why]),
					[]
			end
	end,
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
							"en"; %% not requested, assume english
						Langlist ->
							determine_language(util:string_split(Langlist, ","))
					end,
					Langcookie = io_lib:format("cpx_lang=~s; path=/", [Language]),
					Req:serve_file(File, Docroot, [{"Set-Cookie", Cookie},{"Set-Cookie", Langcookie}]);
				{_Reflist, _Salt, _Login} ->
					Req:serve_file(File, Docroot)
			end;
		{api, Api} ->
			Out = api(Api, check_cookie(Req:parse_cookie()), Post),
			Req:respond(Out);
		api ->
			Peercheck = api_check_peer(Req:get(peer), proplists:get_value(ip_peers, Opts, [])),
			Httpauthcheck = api_check_http_auth(Req:get_header_value("Authorization"), proplists:get_value(http_basic_peers, Opts, [])),
			case {Peercheck, Httpauthcheck} of
				{deny, deny} ->
					Req:respond({401, [{"WWW-Authenticate", "basic realm=cpx_api"}], "unauthorized access"});
				_ -> % if either says okay, we're good
					Out = try json_api(proplists:get_value("request", Post)) of
						O ->
							O
					catch
						error:badarg ->
							{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"No such module or function">>}, {<<"errcode">>, <<"MODULE_FUNCTION_NOEXISTS">>}]})}
					end,
					Req:respond(Out)
			end
	end.

api_check_peer(_Ip, []) ->
	deny;
api_check_peer(Ip, AllowedIPs) ->
	case lists:member(Ip, AllowedIPs) of
		false ->
			deny;
		true ->
			allowed
	end.

api_check_http_auth("Basic " ++ Authval, Auths) ->
	case lists:member(Authval, Auths) of
		false ->
			deny;
		true ->
			allow
	end;
api_check_http_auth(_, _) ->
	deny.

json_api({struct, Props}) ->
	ProtoModule = proplists:get_value(<<"module">>, Props),
	ProtoFunction = proplists:get_value(<<"function">>, Props),
	ProtoArgs = proplists:get_value(<<"args">>, Props),
	Module = list_to_existing_atom(binary_to_list(ProtoModule)),
	Function = list_to_existing_atom(binary_to_list(ProtoFunction)),
	?DEBUG("json_api handling ~p:~p(~p)", [Module, Function, ProtoArgs]),
	json_api(Module, Function, ProtoArgs);
json_api(undefined) ->
	{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"no request made">>}, {<<"errcode">>, <<"NO_REQUEST">>}]})};
json_api(Props) ->
	json_api(mochijson2:decode(Props)).

json_api(cpx, kick_agent, Agent) ->
	Json = case cpx:kick_agent(binary_to_list(Agent)) of
		none ->
			{struct, [{success, false}, {<<"message">>, <<"no such agent">>}, {<<"errcode">>, <<"AGENT_NOEXISTS">>}]};
		ok ->
			{struct, [{success, true}]}
	end,
	{200, [], mochijson2:encode(Json)};
json_api(agent_manager, list, _) ->
	EncodeSkill = fun
		({Key, Val}) when is_list(Val) ->
			{struct, [
				{atom, Key},
				{value, list_to_binary(Val)}
			]};
		({Key, Val}) ->
			{struct, [
				{atom, Key},
				{value, Val}
			]};
		(Key) ->
			{struct, [{atom, Key}]}
	end,
	RawList = agent_manager:list(),
	JsonList = [{struct, [
		{<<"login">>, list_to_binary(Agent)},
		{<<"id">>, list_to_binary(Id)},
		{<<"pid">>, list_to_binary(pid_to_list(Apid))},
		{<<"wentAvailable">>, TimeAvail},
		{<<"skills">>, lists:map(EncodeSkill, Skills)}
	]} || {Agent, {Apid, Id, TimeAvail, Skills}} <- RawList],
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"agents">>, JsonList}]})};
json_api(freeswitch_dialer, start_fg, {struct, Args}) ->
	Node = list_to_binary(proplists:get_value("node", Args, "")),
	Number = list_to_binary(proplists:get_value("number", Args, "")),
	Exten = list_to_binary(proplists:get_value("exten", Args, "")),
	Skills = proplists:get_value("skills", Args, []),
	Client = list_to_binary(proplists:get_value("client", Args, "")),
	Vars = proplists:get_value("vars", Args, {struct, []}),
	json_api(freeswitch_dialer, start_fg, [Node, Number, Exten, Skills, Client, Vars]);
json_api(freeswitch_dialer, start_fg, [Node, Number, Exten, Skills, Client, {struct, Vars}]) ->
	try list_to_existing_atom(binary_to_list(Node)) of
		AtomNode ->
			Fixskill = fun({struct, Props}) ->
				case proplists:get_value(value, Props) of
					undefined ->
						proplists:get_value(atom, Props);
					Val when is_binary(Val) ->
						{proplists:get_value(atom, Props), binary_to_list(Val)};
					Val ->
						{proplists:get_value(atom, Props), Val}
				end
			end,
			FixedSkills = [Fixskill(Skill) || Skill <- Skills],
			FixedVars = [{binary_to_list(Key), binary_to_list(Val)} || {Key, Val} <- Vars],
			case freeswitch_dialer:start_fg(AtomNode, binary_to_list(Number), binary_to_list(Exten), FixedSkills, binary_to_list(Client), FixedVars) of
				{ok, _Pid} ->
					{200, [], mochijson2:encode({struct, [{success, true}]})};
				_ ->
					{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"could not start dialer">>}, {<<"errcode">>, <<"DIALER_ERR">>}]})}
			end
	catch
		error:badarg ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"no such node">>}, {<<"errcode">>, <<"NODE_NOEXISTS">>}]})}
	end;
json_api(freeswitch_monitor, monitor_agent, [Agent, SpyDialstring, NodeBin]) ->
	Node = list_to_existing_atom(binary_to_list(NodeBin)),
	case freeswitch_monitor:monitor_agent(binary_to_list(Agent), binary_to_list(SpyDialstring), Node) of
		{ok, _Pid} ->
			{200, [], mochijson:encode({struct, [{success, true}]})};
		{error, no_agent} ->
			{200, [], mochijson:encode({struct, [{success, false}, {<<"message">>, <<"No such agent">>}, {<<"errcode">>, <<"AGENT_NOEXISTS">>}]})};
		Other ->
			?INFO("freeswitch_monitor:monitor_agent of agent ~s by ~s failed:  ~p", [Agent, SpyDialstring, Other]),
			{200, [], mochijson:encode({struct, [{success, false}, {<<"message">>, <<"Monitor failed to start">>}, {<<"errcode">>, <<"UNKNOWN_ERROR">>}]})}
	end;
json_api(freeswitch_monitor, monitor_client, [ClientLabel, SpyDialstring, NodeBin]) ->
	Node = list_to_existing_atom(binary_to_list(NodeBin)),
	case freesiwtch_monitor:monitor_client(binary_to_list(ClientLabel), binary_to_list(SpyDialstring), Node) of
		{ok, _Pid} ->
			{200, [], mochijson:encode({struct, [{success, true}]})};
		{error, no_client} ->
			{200, [], mochijson:encode({struct, [{success, false}, {<<"message">>, <<"client does not exist">>}, {<<"errcode">>, <<"CLIENT_NOEXISTS">>}]})};
		Other ->
			?INFO("freeswitch_monitor:monitor_client of client ~s by ~s failed: ~p", [ClientLabel, SpyDialstring, Other]),
			{200, [], mochijson:encode({struct, [{success, false}, {<<"message">>, <<"Monitor failed to start">>}, {<<"errcode">>, <<"UNKNOWN_ERROR">>}]})}
	end;
json_api(freeswitch_media_manager, monitor_agent, [Agent, SpyString]) ->
	case freeswitch_media_manager:monitor_agent(binary_to_list(Agent), binary_to_list(SpyString)) of
		{ok, _Pid} ->
			{200, [], mochijson:encode({struct, [{success, true}]})};
		{error, no_agent} ->
			{200, [], mochijson:encode({struct, [{success, false}, {<<"message">>, <<"agent does not exist">>}, {<<"errcode">>, <<"AGENT_NOEXISTS">>}]})};
		Other ->
			?INFO("freeswitch_media_manager:monitor_agent of agent ~s by ~s failed:  ~p", [Agent, SpyString, Other]),
			{200, [], mochijson:encode({struct, [{success, false}, {<<"message">>, <<"monitor failed to start">>}, {<<"errcode">>, <<"UNKNOWN_ERROR">>}]})}
	end;
json_api(freesiwtch_media_manager, monitor_client, [Client, SpyString]) ->
	case freeswitch_media_manager:monitor_client(binary_to_list(Client), binary_to_list(SpyString)) of
		{ok, _Pid} ->
			{200, [], mochijson:encode({struct, [{success, true}]})};
		{error, no_client} ->
			{200, [], mochijson:encode({struct, [{success, false}, {<<"message">>, <<"client does not exist">>}, {<<"errcode">>, <<"CLIENT_NOEXISTS">>}]})};
		Other ->
			?INFO("freeswitch_media_manager:monitor_client of ~s by ~s failed due to ~p", [Client, SpyString, Other]),
			{200, [], mochijson:encode({struct, [{success, false}, {<<"message">>, <<"monitor failed to start">>}, {<<"errcode">>, <<"UNKNOWN_ERROR">>}]})}
	end;
% TODO This is a nasty hack to a problem that shouldn't exist.  No-one but
% certain people (you know who you are) should use this, and should stop
% using this asap.
json_api(agent, set_state, [Login, State]) ->
	case agent_manager:query_agent(binary_to_list(Login)) of
		false ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"no such agent">>}, {<<"errcode">>, <<"AGENT_NOEXISTS">>}]})};
		{true, Pid} ->
			try agent:set_state(Pid, agent:list_to_state(binary_to_list(State))) of
				invalid ->
					{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"invalid state change">>}, {<<"errcode">>, <<"DISALLOWED">>}]})};
				ok ->
					{200, [], mochijson2:encode({struct, [{success, true}]})}
			catch
				What:Why ->
					?NOTICE("Trying to change state of agent ~s (~p) got ~s due to ~p", [Login, Pid, What, Why]),
					{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"error occured trying to change state">>}, {<<"errcode">>, <<"UNKNOWN_ERROR">>}]})}
			end
	end;
json_api(_, _, _) ->
	{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"module/function not available to json api">>}, {<<"errcode">>, <<"DISALLOWED">>}]})}.

determine_language([]) ->
	"";
determine_language([Head | Tail]) ->
	[Lang |_Junk] = util:string_split(Head, ";"),
	case filelib:is_regular(filename:join([util:priv_dir("www/admin/lang/nls/"), Lang, "labels.js"])) of
		true ->
			Lang;
		false ->
			% try the "super language" (eg en vs en-us) in case it's not in the list itself
			[SuperLang | _SubLang] = util:string_split(Lang, "-"),
			case filelib:is_regular(filename:join([util:priv_dir("www/admin/lang/nls/"), SuperLang, "labels.js"])) of
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
api(dialer, _, Post) ->
	Client = proplists:get_value("client", Post),
	Exten = proplists:get_value("exten", Post),
	NodeString = proplists:get_value("node", Post),
	Number = proplists:get_value("number", Post),
	SkillStrings = util:string_split(proplists:get_value("skills", Post, ""), ","),
	Vars = util:string_split(proplists:get_value("vars", Post, ""), ","),

	try
		?DEBUG("starting dialer w/ args ~p", [[NodeString, Number, Exten, SkillStrings, Client, Vars]]),
		Node = list_to_existing_atom(NodeString),
		Skills = [list_to_existing_atom(Skill) || Skill <- SkillStrings],
		case freeswitch_dialer:start_fg(Node, Number, Exten, Skills, Client, Vars) of
			{ok, _Pid} ->
				{200, [], mochijson2:encode({struct, [{success, true}, {message, <<"call started">>}]})};
			{error, Reason} ->
				{500, [], mochijson2:encode({struct, [{success, false}, {message, Reason}]})}
		end
	of
		Res -> Res
	catch
		What:Why ->
			?DEBUG("dialer failed: ~p:~p", [What, Why]),
			{500, [], mochijson2:encode({struct, [{success, false}, {message, bad_params}]})}
	end;
api(_Apirequest, badcookie, _Post) ->
	Reflist = erlang:ref_to_list(make_ref()),
	Cookie = io_lib:format("cpx_management=~p; path=/", [Reflist]),
	ets:insert(cpx_management_logins, {Reflist, undefined, undefined}),
	{403, [{"Set-Cookie", Cookie}], <<"Cookie reset, retry.">>};
api(getsalt, {Reflist, _Salt, Login}, _Post) ->
	Newsalt = integer_to_list(crypto:rand_uniform(0, 4294967295)),
	ets:insert(cpx_management_logins, {Reflist, Newsalt, Login}),
	[E, N] = util:get_pubkey(),
	PubKey = {struct, [{<<"E">>, list_to_binary(erlang:integer_to_list(E, 16))}, {<<"N">>, list_to_binary(erlang:integer_to_list(N, 16))}]},
	{200, [], mochijson2:encode({struct, [{success, true}, {message, <<"Salt created, check salt property">>}, {salt, list_to_binary(Newsalt)}, {pubkey, PubKey}]})};
api(login, {_Reflist, undefined, _Conn}, _Post) ->
	{200, [], mochijson2:encode({struct, [{success, false}, {message, <<"No salt set">>}]})};
api(login, {Reflist, Salt, _Login}, Post) ->
	Username = proplists:get_value("username", Post, ""),
	Password = proplists:get_value("password", Post, ""),
	case util:decrypt_password(Password) of
		{ok, Decrypted} ->
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
			end;
	{error, decrypt_failed} ->
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
	{403, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"login required">>}]})};
api(load_agents, ?COOKIE, Post) ->
	{_Filename, Binary} = proplists:get_value("loadAgentFile", Post),
	Overwrite = proplists:get_value("overwrite", Post),
	String = binary_to_list(Binary),
	Terms = list_to_terms(String),
	?NOTICE("Das terms:  ~p", [Terms]),
	CreateAgentFun = fun({Id, Login, Pass, Profile, Firstname, Lastname, Security, Skills}) ->
		case {agent_auth:get_agent(id, Id), agent_auth:get_agent(login, Login), Overwrite} of
			{{atomic, []}, {atomic, []}, _} ->
				agent_auth:cache(Id, Login, Pass, {Profile, Skills}, Security, [{<<"auto_loaded">>, util:now()}]),
				agent_auth:set_agent(Id, [{firstname, Firstname}, {lastname, Lastname}]);
			{_, _, undefined} ->
				{skipped, Login};
			{{atomic, [_Rec]}, {atomic, []}, _} ->
				agent_auth:destroy(id, Id),
				agent_auth:cache(Id, Login, Pass, {Profile, Skills}, Security, [{<<"auto_loaded">>, util:now()}]),
				agent_auth:set_agent(Id, [{firstname, Firstname}, {lastname, Lastname}]);
			{{atomic, []}, {atomic, [_Rec]}, _} ->
				agent_auth:destroy(login, Id),
				agent_auth:cache(Id, Login, Pass, {Profile, Skills}, Security, [{<<"auto_loaded">>, util:now()}]),
				agent_auth:set_agent(Id, [{firstname, Firstname}, {lastname, Lastname}]);
			{{atomic, [_Rec]}, {atomic, [_OtherRec]}, _} ->
				agent_auth:destroy(id, Id),
				agent_auth:destroy(login, Id),
				agent_auth:cache(Id, Login, Pass, {Profile, Skills}, Security, [{<<"auto_loaded">>, util:now()}]),
				agent_auth:set_agent(Id, [{firstname, Firstname}, {lastname, Lastname}])
		end
	end,
	Reses = [CreateAgentFun(X) || X <- Terms],
	?NOTICE("load agents res:  ~p", [Reses]),
	{301, [{"location", "/loadAgents.html"}], <<"yar?">>};
	
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
			?DEBUG("this is teh happy", []),
			cpx_supervisor:destroy(agent_web_listener),
			{struct, [{success, true}, {<<"message">>, <<"Web Server disabled">>}]};
		Webport ->
			?DEBUG("This is pure shit", []),
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
	Dialplanout = case proplists:get_value("agentModuleDialplanListenEnabled", Post) of
		undefined ->
			cpx_supervisor:destroy(agent_dialplan_listener),
			{struct, [{success, true}, {<<"message">>, <<"Dialplan Listener Disabled">>}]};
		_ ->
			case cpx_supervisor:get_conf(agent_dialplan_listener) of
				undefined ->
					DialplanConf = #cpx_conf{
						id = agent_dialplan_listener,
						module_name = agent_dialplan_listener,
						start_function = start_link,
						start_args = [],
						supervisor = agent_connection_sup
					},
					cpx_supervisor:update_conf(agent_dialplan_listener, DialplanConf),
					{struct, [{success, true}, {<<"message">>, <<"Dialplan Listener Enabled">>}]};
				_ ->
					{struct, [{success, true}, {<<"message">>< <<"Already enabled">>}]}
			end
	end,
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"results">>, [Tcpout, Webout, Dialplanout]}]})};
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
	Dialplanout = case cpx_supervisor:get_conf(agent_dialplan_listener) of
		undefined ->
			[{"agentModuleDialplanListenEnabled", false}];
		_ ->
			[{"agentModuleDialplanListenEnabled", true}]
	end,
	Full = lists:append([Tcpout, Webout, Dialplanout]),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"result">>, {struct, Full}}]})};

%% =====
%% agents -> profiles
%% =====
api({agents, "profiles", "get"}, ?COOKIE, _Post) ->
	Profiles = agent_auth:get_profiles(),
	Foreachprofile = fun(#agent_profile{name = Pname, skills = Pskills} = P) ->
		Agents = agent_auth:get_agents(Pname),
		{struct, [
			{<<"name">>, case Pname of undefined -> undefined; _ -> list_to_binary(Pname) end}, 
			{<<"type">>, <<"profile">>},
			{<<"order">>, P#agent_profile.order},
			{<<"id">>, case P#agent_profile.id of undefined -> undefined; _ -> list_to_binary(P#agent_profile.id) end},
			{<<"skills">>, encode_skills(Pskills)}, 
			{<<"agents">>, encode_agents(Agents)}]}
	end,
	Items = lists:map(Foreachprofile, Profiles),
	Json = {struct, [{success, true}, {<<"items">>, Items}]},
	{200, [], mochijson2:encode(Json)};
api({agents, "profiles", Profile, "getskills"}, ?COOKIE, _Post) ->
	#agent_profile{skills = Skillatoms} = agent_auth:get_profile(Profile),
	Encoded = encode_skills(Skillatoms),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"items">>, Encoded}]})};
api({agents, "profiles", "new"}, ?COOKIE, Post) ->
	Prof = #agent_profile{
		name = proplists:get_value("name", Post),
		skills = parse_posted_skills(proplists:get_all_values("skills", Post)),
		order = list_to_integer(proplists:get_value("order", Post, "10")),
		id = proplists:get_value("id", Post, "")
	},
	agent_auth:new_profile(Prof),
	{200, [], mochijson2:encode({struct, [{success, true}]})};
api({agents, "profiles", "Default", "update"}, {_Reflist, _Salt, _Login}, Post) ->
	Oldprof = agent_auth:get_profile("Default"),
	case proplists:get_value("name", Post) of
		undefined ->
			Prof = #agent_profile{
				name = "Default",
				skills = parse_posted_skills(proplists:get_all_values("skills", Post)),
				order = list_to_integer(proplists:get_value("order", Post, "10")),
				id = Oldprof#agent_profile.id
			},
			agent_auth:set_profile("Default", Prof),
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		_Else ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Default is a protected profile and cannot be renamed">>}]})}
	end;
api({agents, "profiles", Profile, "update"}, ?COOKIE, Post) ->
	Old = agent_auth:get_profile(Profile),
	New = #agent_profile{
		name = proplists:get_value("name", Post),
		skills = parse_posted_skills(proplists:get_all_values("skills", Post)),
		order = list_to_integer(proplists:get_value("order", Post, "10")),
		id = Old#agent_profile.id
	},
	agent_auth:set_profile(Profile, New),
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
	Out = case proplists:get_value("password", Post) of
		"" ->
			?DEBUG("Not updating password.", []),
			agent_auth:set_agent(Agent, [
				{login, proplists:get_value("login", Post)},
				{skills, Fixedskills},
				{securitylevel, list_to_existing_atom(proplists:get_value("security", Post))},
				{profile, proplists:get_value("profile", Post)},
				{lastname, proplists:get_value("lastname", Post)},
				{firstname, proplists:get_value("firstname", Post)},
				{endpoints, decode_endpoints(
					proplists:get_value("endpoints", Post))}
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
	case Out of
		{atomic, ok} ->
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		{aborted, {duplicate_login, _}} ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"login name already exists">>}]})};
		{aborted, Err} ->
			?ERROR("Updating agent ~s got ~p", [Agent, Err]),
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"could not update agent">>}]})}
	end;
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
				{password, Confirmpw},
				{skills, Fixedskills},
				{securitylevel, list_to_existing_atom(proplists:get_value("security", Post))},
				{profile, proplists:get_value("profile", Post)},
				{lastname, proplists:get_value("lastname", Post)},
				{firstname, proplists:get_value("firstname", Post)},
				{endpoints, decode_endpoints(
					proplists:get_value("endpoints", Post))}
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
	F = fun(#agent_profile{name = Prof}) ->
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
		{<<"skills">>, stringify_skills(Qgroup#queue_group.skills)},
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
	Postedskills = proplists:get_all_values("skills", Post),
	FixedSkills = parse_posted_skills(Postedskills),
	Rec = #queue_group{
		name = Newname,
		sort = Sort,
		recipe = Recipe,
		skills = FixedSkills
	},
	call_queue_config:set_queue_group(Group, Rec),
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
	Skills = parse_posted_skills(proplists:get_all_values("skills", Post)),
	Rec = #queue_group{
		name = Name,
		sort = Sort,
		recipe = Recipe,
		skills = Skills
	},
	call_queue_config:new_queue_group(Rec),
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
	case call_queue_config:get_queue(Queue) of
		noexists ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"No such queue">>}]})};
		Queuerec ->
			call_queue_config:destroy_queue(Queuerec),
			{200, [], mochijson2:encode({struct, [{success, true}]})}
	end;
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
%% modules -> *
%% =====
% TODO this function is ugly.  It's used to populate the modules list,
% but the result of the rpc call is not used ever.
api({modules, "poll"}, ?COOKIE, _Post) ->
	{ok, Appnodes} = application:get_env('OpenACD', nodes),
	Nodes = lists:filter(fun(N) -> lists:member(N, Appnodes) end, [node() | nodes()]),
	F = fun(Node) ->
		{Node, [
			{cpx_monitor_grapher, rpc:call(Node, cpx_supervisor, get_conf, [cpx_monitor_grapher], 2000)},
			{cpx_monitor_passive, rpc:call(Node, cpx_supervisor, get_conf, [cpx_monitor_passive], 2000)},
			{cpx_supervisor, rpc:call(Node, cpx_supervisor, get_value, [archivepath], 2000)},
			{email_media_manager, rpc:call(Node, cpx_supervisor, get_conf, [email_media_manager], 2000)},
			{freeswitch_media_manager, rpc:call(Node, cpx_supervisor, get_conf, [freeswitch_media_manager], 2000)},
			{gen_cdr_dumper, rpc:call(Node, cpx_supervisor, get_conf, [gen_cdr_dumper], 2000)},
			{cdr_tcp_pusher, #cpx_conf{
				id = cdr_tcp_pusher,
				module_name = cdr_tcp_pusher,
				start_function = start_link,
				start_args = []
			}},
			{cpx_monitor_kgb_eventlog, rpc:call(Node, cpx_supervisor, get_conf, [cpx_monitor_kgb_eventlog], 2000)},
			{cpx_monitor_odbc_supervisor, rpc:call(Node, cpx_supervisor, get_conf, [cpx_monitor_odbc_supervisor], 2000)},
			{cpx_web_management, rpc:call(Node, cpx_supervisor, get_conf, [cpx_web_management], 2000)},
			{agent_web_listener, rpc:call(Node, cpx_supervisor, get_conf, [agent_web_listener], 2000)},
			{agent_tcp_listener, rpc:call(Node, cpx_supervisor, get_conf, [agent_tcp_listener], 2000)},
			{agent_dialplan_listener, rpc:call(Node, cpx_supervisor, get_conf, [agent_dialplan_listener], 2000)}
		]}
	end,
	Rpcs = lists:map(F, Nodes),
	Json = encode_modules(Rpcs, []),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"identifier">>, <<"id">>}, {<<"label">>, <<"name">>}, {<<"items">>, Json}]})};
api({modules, "status"}, ?COOKIE, _Post) ->
	{ok, AppNodes} = cpx:get_env(nodes, []),
	Error = case lists:member(node(), AppNodes) of
		true ->
			[];
		false ->
			[{<<"error">>, list_to_binary(io_lib:format("This node (~s) is not a member of the cluster.", [node()]))}]
	end,
	DataRaw = [begin
		Uptime = rpc:call(Node, cpx, get_env, [uptime, false]),
		Confs = case rpc:call(Node, cpx_supervisor, get_conf, []) of
			undefined ->
				[];
			Else ->
				Else
		end,
		Plugins = rpc:call(Node, cpx, plugins_running, []),
		{Node, Uptime, Confs, Plugins}
	end || Node <- AppNodes],
	Jsonable = [begin
		IsUp = case Uptime of
			{ok, N} when is_integer(N) ->
				[{<<"isUp">>, true}, {<<"uptime">>, N * 1000}];
			_ ->
				[{<<"isUp">>, false}]
		end,
		UpConfs = [element(2, Conf) || Conf <- Confs],
		{Node, {struct, [{modules, UpConfs}, {plugins, Plugs} | IsUp]}}
	end || {Node, Uptime, Confs, Plugs} <- DataRaw],
	Json = {struct, [
		{success, true},
		{nodes, {struct, Jsonable}} | Error
	]},
	{200, [], mochijson2:encode(Json)};

%% =====
%% modules -> node -> modules
%% =====

api({modules, Node, "cdr_tcp_pusher", "get"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	case rpc:call(Atomnode, cpx_supervisor, get_conf, [cdr_tcp_pusher]) of
		#cpx_conf{start_args = Props} ->
			FixedProps = [case X of
				{server, S} ->
					{server, list_to_binary(S)};
				_ ->
					X
			end || X <- Props],
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"enabled">>, true} | FixedProps]})};
		_ ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"enabled">>, false}]})}
	end;
api({modules, Node, "cdr_tcp_pusher", "update"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
	case proplists:get_value("enabled", Post) of
		"true" ->
			FixProps = fun
				({"port", Val}, Acc) ->
					[{port, list_to_integer(Val)} | Acc];
				({"server", Val}, Acc) ->
					[{server, Val} | Acc];
				(_, Acc) ->
					Acc
			end,
			StartArgs = lists:foldl(FixProps, [], Post),
			Conf = #cpx_conf{
				id = cdr_tcp_pusher,
				module_name = cdr_tcp_pusher,
				start_function = start_link,
				start_args = [StartArgs]
			},
			case rpc:call(Atomnode, cpx_supervisor, update_conf, [cdr_tcp_pusher, Conf]) of
				{atomic, {ok, _Pid}} ->
					{200, [], mochijson2:encode({struct, [{success, true}]})};
				Else ->
					?WARNING("Updating cdr_tcp_pusher failed:  ~p", [Else]),
					{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"could not start cdr_tcp_pusher">>}]})}
			end;
		_ ->
			rpc:call(Atomnode, cpx_supervisor, destroy, [cdr_tcp_pusher]),
			{200, [], mochijson2:encode({struct, [{success, true}]})}
	end;
api({modules, Node, "agent_web_listener", "get"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	case rpc:call(Atomnode, cpx_supervisor, get_conf, [agent_web_listener]) of
		#cpx_conf{start_args = StartArgs} ->
			Json = case StartArgs of
				[] ->
					[{<<"httpEnabled">>, true}, {<<"port">>, 5050}];
				[N] when is_integer(N) ->
					[{<<"httpEnabled">>, true}, {<<"port">>, N}];
				[List] ->
					lists:flatten([case X of
						ssl -> {<<"httpsEnabled">>, true};
						{port, P} -> [{<<"port">>, P}, {<<"httpEnabled">>, true}];
						{ssl_port, P} -> {<<"httpsPort">>, P}
					end || X <- List])
			end,
			{200, [], mochijson2:encode({struct, [{success, true} | Json]})};
		undefined ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"enabled">>, false}, {<<"port">>, 5050}]})};
		Else ->
			?WARNING("Error getting agent_web_listener settings:  ~p", [Else]),
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Could not load settings">>}]})}
	end;
api({modules, Node, "agent_web_listener", "update"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
	StartArgs = lists:flatten([case X of
		{"port", P} -> {port, list_to_integer(P)};
		{"sslPort", P} -> {ssl_port, list_to_integer(P)};
		{"ssl", "true"} -> ssl;
		_ -> []
	end || X <- Post]),
	case StartArgs of
		[] ->
			rpc:call(Atomnode, cpx_supervisor, destroy, [agent_web_listener]),
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		_ ->
			Conf = #cpx_conf{
				id = agent_web_listener,
				module_name = agent_web_listener,
				start_function = start_link,
				start_args = [StartArgs],
				supervisor = agent_connection_sup
			},
			case rpc:call(Atomnode, cpx_supervisor, update_conf, [agent_web_listener, Conf]) of
				{atomic, {ok, _Pid}} ->
					{200, [], mochijson2:encode({struct, [{<<"success">>, true}]})};
				{aborted, {start_fail, Why}} ->
					?WARNING("Could not start agent_web_listener:  ~p", [Why]),
					{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Could not start listener">>}]})}
			end
	end;
api({modules, Node, "agent_tcp_listener", "get"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	Defaults = [
		{port, 1337},
		{radix, 10},
		{ssl_upgrade, false}
	],
	case rpc:call(Atomnode, cpx_supervisor, get_conf, [agent_tcp_listener]) of
		#cpx_conf{start_args = [Options]} ->
			Port = proplists:get_value(port, Options, false),
			Radix = proplists:get_value(radix, Options, false),
			Ssl = proplists:get_value(socket_type, Options, false),
			OutProps = [
				{success, true},
				{<<"enabled">>, true},
				{port, Port},
				{radix, Radix},
				{ssl_upgrade, Ssl},
				{<<"defaults">>, {struct, Defaults}}
			],
			{200, [], mochijson2:encode({struct, OutProps})};
		undefined ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"enabled">>, false}, {<<"defaults">>, {struct, Defaults}}]})};
		Else ->
			?WARNING("Error getting agent_tcp_listener settings:  ~p", [Else]),
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Could not load settings">>}]})}
	end;
api({modules, Node, "agent_tcp_listener", "update"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
	case proplists:get_value("enabled", Post) of
		"true" ->
			AccFun = fun
				({"port", Val}, Acc) ->
					[{port, list_to_integer(Val)} | Acc];
				({"ssl", "true"}, Acc) ->
					[{socket_type, ssl_upgrade} | Acc];
				({"radix", Val}, Acc) ->
					[{radix, list_to_integer(Val)} | Acc];
				(_, Acc) ->
					Acc
			end,
			StartArgs = lists:foldl(AccFun, [], Post),
			Conf = #cpx_conf{
				id = agent_tcp_listener,
				module_name = agent_tcp_listener,
				start_function = start_link, start_args = [StartArgs],
				supervisor = agent_connection_sup
			},
			case rpc:call(Atomnode, cpx_supervisor, update_conf, [agent_tcp_listener, Conf]) of
				{atomic, {ok, _Pid}} ->
					{200, [], mochijson2:encode({struct, [{<<"success">>, true}]})};
				{aborted, {start_fail, Why}} ->
					?WARNING("Could not start agent_tcp_listener:  ~p", [Why]),
					{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Could not start listener">>}]})}
			end;
		_ ->
			rpc:call(Atomnode, cpx_supervisor, destroy, [agent_tcp_listener]),
			{200, [], mochijson2:encode({struct, [{success, true}]})}
	end;
api({modules, Node, "agent_dialplan_listener", "get"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	case rpc:call(Atomnode, cpx_supervisor, get_conf, [agent_dialplan_listener]) of
		undefined ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"enabled">>, false}]})};
		#cpx_conf{start_args = Args} ->
			BaseJson = [{success, true}, {<<"enabled">>, true}],
			JsonProps = case Args of
				[] -> BaseJson;
				[{unavailable_timeout, N}] -> [{<<"unavailableTimeout">>, N} | BaseJson]
			end,
			{200, [], mochijson2:encode({struct, JsonProps})}
	end;
api({modules, Node, "agent_dialplan_listener", "update"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
	case proplists:get_value("enabled", Post) of
		"true" ->
			StateArgs = case catch list_to_integer(proplists:get_value("unavailableTimeout", Post)) of
				Time when is_integer(Time), Time > 0 -> [{unavailable_timeout, Time}];
				_ -> []
			end,
			Conf = #cpx_conf{
				id = agent_dialplan_listener,
				module_name = agent_dialplan_listener,
				start_function = start_link,
				start_args = StateArgs,
				supervisor = agent_connection_sup
			},
			case rpc:call(Atomnode, cpx_supervisor, update_conf, [agent_dialplan_listener, Conf]) of
				{atomic, {ok, _Pid}} ->
					{200, [], mochijson2:encode({struct, [{success, true}]})};
				{aborted, {start_fail, Err}} ->
					?WARNING("Could not start dp listener:  ~p", [Err]),
					{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"could not start listener">>}]})}
			end;
		_ ->
			rpc:call(Atomnode, cpx_supervisor, destroy, [agent_dialplan_listener]),
			{200, [], mochijson2:encode({struct, [{success, true}]})}
	end;
api({modules, Node, "cpx_web_management", "get"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	Json = case rpc:call(Atomnode, cpx_supervisor, get_conf, [cpx_web_management]) of
		undefined ->
			{struct, [
				{success, true},
				{<<"enabled">>, false},
				{<<"port">>, ?PORT},
				{<<"ip_peers">>, []},
				{<<"http_basic_peers">>, []}
			]};
		#cpx_conf{start_args = []} ->
			{struct, [
				{success, true},
				{<<"enabled">>, true},
				{<<"port">>, ?PORT},
				{<<"ip_peers">>, []},
				{<<"http_basic_peers">>, []}
			]};
		#cpx_conf{start_args = [Port]} when is_integer(Port) ->
			{struct, [
				{success, true},
				{<<"enabled">>, true},
				{<<"port">>, Port},
				{<<"ip_peers">>, []},
				{<<"http_basic_peers">>, []}
			]};
		#cpx_conf{start_args = [Args]} ->
			Ippeers = [list_to_binary(Ip) || Ip <- proplists:get_value(ip_peers, Args, [])],
			Httppeers = [{struct, [
				{<<"username">>, list_to_binary(Username)},
				{<<"password">>, list_to_binary(Password)}
			]} || {Username, Password} <- proplists:get_value(http_basic_peers, Args, [])],
			{struct, [
				{success, true},
				{<<"enabled">>, true},
				{<<"port">>, proplists:get_value(port, Args, ?PORT)},
				{<<"ip_peers">>, Ippeers},
				{<<"https">>, proplists:get_value(ssl, Args, false)},
				{<<"http_basic_peers">>, Httppeers}
			]}
	end,
	{200, [], mochijson2:encode(Json)};
api({modules, Node, "cpx_web_management", "update"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
	Json = case proplists:get_value("enabled", Post) of
		"true" ->
			Port = list_to_integer(proplists:get_value("port", Post)),
			Ips = mochijson2:decode(list_to_binary(proplists:get_value("ip_peers", Post))),
			Https = mochijson2:decode(list_to_binary(proplists:get_value("http_basic_peers", Post))),
			Midargs = case Ips of
				[] ->
					[{port, Port}];
				_ ->
					[{port, Port}, {ip_peers, [binary_to_list(I) || I <- Ips]}]
			end,
			Args = case Https of
				[] ->
					Midargs;
				_ ->
					Properhttps = [
						{binary_to_list(proplists:get_value(<<"username">>, Props)), binary_to_list(proplists:get_value(<<"password">>, Props))} || 
						{struct, Props} <- Https
					],
					[{http_basic_peers, Properhttps} | Midargs]
			end,
			FullArgs = case proplists:get_value("useHttps", Post) of
				undefined -> Args;
				_ -> [ssl | Args]
			end,
			Conf = #cpx_conf{
				id = cpx_web_management,
				module_name = cpx_web_management,
				start_function = start_link,
				start_args = [FullArgs],
				supervisor = management_sup
			},
			% spawning up as the supervisor will kill this process before it writes the new one.
			% this will cause the write to not happen, but the process not to restart.
			% thus, having a spawn do it.
			Rpcfun = fun() ->
				case rpc:call(Atomnode, cpx_supervisor, update_conf, [cpx_web_management, Conf]) of
					{atomic, {ok, Pid}} when is_pid(Pid) ->
						{struct, [{success, true}]};
					Else ->
						?ERROR("Error updating cpx_web_management from web management:  ~p", [Else]),
						{struct, [{success, false}]}
				end
			end,
			spawn(Rpcfun),
			{struct, [{success, true}]};
		_ ->
			rpc:call(Atomnode, cpx_supervisor, destroy, [cpx_web_management], 2000),
			{struct, [{success, true}]}
	end,
	{200, [], mochijson2:encode(Json)};
api({modules, Node, "cpx_monitor_odbc_supervisor", "get"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	Json = case rpc:call(Atomnode, cpx_supervisor, get_conf, [cpx_monitor_odbc_supervisor]) of
		undefined ->
			{struct, [
				{success, true},
				{<<"enabled">>, false}
			]};
		#cpx_conf{start_args = [RawDsn | RawArgs]} ->
			Dsn = list_to_binary(RawDsn),
			OtherOpts = case RawArgs of
				[] ->
					[];
				[Args] ->
					[case O of
						trace ->
							{trace, true};
						{max_r, N} ->
							{max_r, N};
						{max_t, N} ->
							{max_t, N}
					end || O <- Args]
			end,
			Opts = [{dsn, Dsn} | OtherOpts],
			{struct, [{success, true}, {<<"enabled">>, true} | Opts]}
	end,
	{200, [], mochijson2:encode(Json)};
api({modules, Node, "cpx_monitor_odbc_supervisor", "update"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
	Json = case proplists:get_value("enabled", Post) of
		"true" ->
			Dsn = proplists:get_value("dsn", Post),
			BuildOpts = fun
				(_R, [], Acc) ->
					Acc;
				(R, [{"trace", "true"} | Tail], Acc) ->
					R(R, Tail, [trace | Acc]);
				(R, [{"maxR", ListN} | Tail], Acc) ->
					R(R, Tail, [{max_r, list_to_integer(ListN)} | Acc]);
				(R, [{"maxT", ListN} | Tail], Acc) ->
					R(R, Tail, [{max_t, list_to_integer(ListN)} | Acc]);
				(R, [_ | Tail], Acc) ->
					R(R, Tail, Acc)
			end,
			Opts = BuildOpts(BuildOpts, Post, []),
			StartArgs = case Opts of
				[] ->
					[Dsn];
				_ ->
					[Dsn, Opts]
			end,
			Conf = #cpx_conf{
				id = cpx_monitor_odbc_supervisor,
				module_name  = cpx_monitor_odbc_supervisor,
				start_function = start_link,
				start_args = StartArgs,
				supervisor = management_sup
			},
			case rpc:call(Atomnode, cpx_supervisor, update_conf, [cpx_monitor_odbc_supervisor, Conf]) of
				{atomic, {ok, _Pid}} ->
					{struct, [{success, true}]};
				Else ->
					{struct, [
						{success, false},
						{<<"message">>, list_to_binary(io_lib:format("Could not start odbc_supervisor<pre>~n~p</pre>", [Else]))}
					]}
			end;
		_ ->
			rpc:call(Atomnode, cpx_supervisor, destroy, [cpx_monitor_odbc_supervisor]),
			{struct, [{success, true}]}
	end,
	{200, [], mochijson2:encode(Json)};
api({modules, Node, "cpx_monitor_kgb_eventlog", "get"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	Json = case rpc:call(Atomnode, cpx_supervisor, get_conf, [cpx_monitor_kgb_eventlog]) of
		undefined ->
			{struct, [
				{success, true},
				{<<"enabled">>, false},
				{<<"isDefaultLogfile">>, true},
				{<<"kgbLogfile">>, <<"events.log">>}
			]};
		#cpx_conf{start_args = [Args]} ->
			{Isdefault, Filepath} = case proplists:get_value(filename, Args) of
				undefined ->
					{true, <<"events.log">>};
				Else ->
					{false, list_to_binary(Else)}
			end,
			{struct, [
				{success, true},
				{<<"enabled">>, true},
				{<<"isDefaultLogFile">>, Isdefault},
				{<<"kgbLogfile">>, Filepath}
			]}
	end,
	{200, [], mochijson2:encode(Json)};
api({modules, Node, "cpx_monitor_kgb_eventlog", "update"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
	Json = case proplists:get_value("kgbEventsEnabled", Post) of
		"false" ->
			rpc:call(Atomnode, cpx_supervisor, destroy, [cpx_monitor_kgb_eventlog], 2000),
			{struct, [{success, true}]};
		"true" ->
			Args = case proplists:get_value("kgbFilename", Post) of
				"" ->
					[];
				Else ->
					[{filename, Else}]
			end,
			case rpc:call(Atomnode, cpx_supervisor, update_conf, [cpx_monitor_kgb_eventlog, #cpx_conf{
				id = cpx_monitor_kgb_eventlog,
				module_name = cpx_monitor_kgb_eventlog,
				start_function = start_link,
				start_args = [Args],
				supervisor = management_sup
			}]) of
				{atomic, {ok, Pid}} when is_pid(Pid) ->
					{struct, [{success, true}]};
				_ElseErr ->
					{struct, [{success, false}]}
			end
	end,
	{200, [], mochijson2:encode(Json)};
api({modules, Node, "cpx_monitor_grapher", "get"}, ?COOKIE, _Post) ->
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
			Dynamic = case application:get_env('OpenACD', webdir_dynamic) of
				undefined ->
					<<"../www/dynamic">>;
				{ok, WebDirDyn} ->
						WebDirDyn
			end,
			Imagepath = case Protoimagepath of
				Rrdpath ->
					<<"rrd path">>;
				Dynamic ->
				%<<"../www/dynamic">> ->
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
api({modules, Node, "cpx_monitor_grapher", "update"}, ?COOKIE, Post) ->
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
					case application:get_env('OpenACD', webdir_dynamic) of
						undefined ->
							"../www/dynamic";
						{ok, WebDirDyn} ->
							WebDirDyn
					end;
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
api({modules, Node, "cpx_monitor_passive", "update"}, ?COOKIE, Post) ->
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
						case application:get_env('OpenACD', webdir_dynamic) of
							undefined ->
								util:priv_dir("www/dynamic");
							{ok, WebDirDyn} ->
								WebDirDyn
						end;
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
api({modules, Node, "cpx_monitor_passive", "get"}, ?COOKIE, _Post) ->
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
%%api({modules, Node, "cpx_monitor_passive", "update"}, ?COOKIE, Post) ->
%%	Atomnode = list_to_existing_atom(Node),
	
api({modules, Node, "gen_cdr_dumper", "get"}, ?COOKIE, _Post) ->
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
api({modules, Node, "gen_cdr_dumper", "update"}, ?COOKIE, Post) ->
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
	
api({modules, _Node, "cpx_supervisor", "get"}, ?COOKIE, _Post) ->
	{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"cpx_supervisor get invalid, need to know which cpx_value key">>}]})};
api({modules, Node, "cpx_supervisor", "get", "archivepath"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	case rpc:call(Atomnode, cpx_supervisor, get_value, [archivepath]) of
		none ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"result">>, <<"">>}]})};
		{ok, Value} ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"result">>, list_to_binary(Value)}]})};
		Else ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, list_to_binary(io_lib:format("~p", [Else]))}]})}
	end;
api({modules, Node, "cpx_supervisor", "get", "exit_on_max_ring_fails"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	case rpc:call(Atomnode, cpx_supervisor, get_value, [exit_on_max_ring_fails]) of
		none ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"value">>, false}]})};
		{ok, _} ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"value">>, true}]})}
	end;
api({modules, Node, "cpx_supervisor", "get", "mantispath"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	case rpc:call(Atomnode, cpx_supervisor, get_value, [mantispath]) of
		none ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"result">>, <<"">>}]})};
		{ok, Value} ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"result">>, list_to_binary(Value)}]})};
		Else ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, list_to_binary(io_lib:format("~p", [Else]))}]})}
	end;
api({modules, Node, "cpx_supervisor", "get", "transferprompt"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	case rpc:call(Atomnode, cpx_supervisor, get_value, [transferprompt]) of
		none ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"skills">>, []}, {<<"prompts">>, []}]})};
		{ok, {Prompts, Skills}} ->
			EncodedSkills = [encode_skill(X) || X <- Skills],
			EncodedPrompts = [
				{struct, [
					{<<"name">>, Name},
					{<<"label">>, Label},
					{<<"regex">>, RegEx}
				]} ||
				{Name, Label, RegEx} <- Prompts
			],
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"skills">>, EncodedSkills}, {<<"prompts">>, EncodedPrompts}]})};
		Else ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, list_to_binary(io_lib:format("~p", [Else]))}]})}
	end;
api({modules, Node, "cpx_supervisor", "get", "default_ringout"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	Truedefault = ?DEFAULT_RINGOUT,
	case rpc:call(Atomnode, cpx, get_env, [default_ringout]) of
		undefined ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"isDefault">>, true}, {<<"default">>, ?DEFAULT_RINGOUT}]})};
		{ok, Truedefault} ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"isDefault">>, true}, {<<"default">>, ?DEFAULT_RINGOUT}]})};
		{ok, Val} ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"isDefault">>, false}, {<<"value">>, Val}, {<<"default">>, ?DEFAULT_RINGOUT}]})};
		Else ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, list_to_binary(io_lib:format("~p", [Else]))}]})}
	end;
api({modules, Node, "cpx_supervisor", "get", "max_ringouts"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	case rpc:call(Atomnode, cpx, get_env, [max_ringouts]) of
		undefined ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"isDefault">>, true}, {<<"default">>, infinity}]})};
		{ok, infinity} ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"isDefault">>, true}, {<<"default">>, infinity}]})};
		{ok, N} when is_integer(N) ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"isDefault">>, false}, {<<"value">>, N}, {<<"default">>, infinity}]})};
		Else ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, list_to_binary(io_lib:format("~p", [Else]))}]})}
	end;
api({modules, Node, "cpx_supervisor", "get", "plugin_dir"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	case rpc:call(Atomnode, cpx, get_env, [plugin_dir]) of
		undefined ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"isDefault">>, true}, {<<"default">>, false}]})};
		{ok, Dir} ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"isDefault">>, false}, {<<"default">>, false}, {<<"value">>, list_to_binary(Dir)}]})};
		Else ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, list_to_binary(io_lib:format("~p", [Else]))}]})}
	end;
api({modules, Node, "cpx_supervisor", "get", "plugins"}, ?COOKIE, _Post) ->
	Atomnode = list_to_existing_atom(Node),
	case rpc:call(Atomnode, cpx, plugins_running, []) of
		Apps when is_list(Apps) ->
			{200, [], mochijson2:encode({struct, [{success, true}, {<<"value">>, {struct, Apps}}]})};
		Else ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, list_to_binary(io_lib:format("~p", [Else]))}]})}
	end;
api({modules, Node, "cpx_supervisor", "update", "plugins"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
	Val = proplists:get_value("plugin", Post),
	Act = proplists:get_value("action", Post),
	case {Val, Act} of
		{Val, "unload"} ->
			{ok, Plugins} = rpc:call(Atomnode, cpx, get_env, [plugins, []]),
			case [X || X <- Plugins, Val == atom_to_list(X)] of
				[] ->
					{200, [], mochijson2:encode({struct, [{success, true}]})};
				[PluginAtom] ->
					rpc:call(Atomnode, cpx, unload_plugin, [PluginAtom]),
					{200, [], mochijson2:encode({struct, [{success, true}]})}
			end;
		{Val, "load"} ->
			case rpc:call(Atomnode, cpx, get_env, [plugin_dir]) of
				undefined ->
					{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"plugins not enabled">>}]})};
				{ok, Dir} ->
					{ok, Dirs} = rpc:call(Atomnode, file, list_dir, [Dir]),
					case [X || X <- Dirs, string:str(X, Val) > 0] of
						[] ->
							{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"no such plugin">>}]})};
						_ ->
							rpc:call(Atomnode, cpx, load_plugin, [list_to_atom(Val)]),
							{200, [], mochijson2:encode({struct, [{success, true}]})}
					end
			end
	end;
api({modules, Node, "cpx_supervisor", "update", "plugin_dir"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
	Res = case proplists:get_value("value", Post) of
		undefined ->
			rpc:call(Atomnode, cpx_supervisor, drop_value, [plugin_dir]);
		"" ->
			rpc:call(Atomnode, cpx_supervisor, drop_value, [plugin_dir]);
		Dir ->
			rpc:call(Atomnode, cpx_supervisor, set_value, [plugin_dir, Dir])
	end,
	case Res of
		{atomic, ok} ->
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		Else ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, list_to_binary(io_lib:format("~p", [Else]))}]})}
	end;
api({modules, Node, "cpx_supervisor", "update", "exit_on_max_ring_fails"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
	{Func, Args} = case proplists:get_value("value", Post) of
		"true" -> {set_value, [exit_on_max_ring_fails, true]};
		_ -> {drop_value, [exit_on_max_ring_fails]}
	end,
	case rpc:call(Atomnode, cpx_supervisor, Func, Args) of
		{atomic, ok} ->
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		Else ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, list_to_binary(io_lib:format("~p", [Else]))}]})}
	end;
api({modules, Node, "cpx_supervisor", "update", "max_ringouts"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
	Val = case proplists:get_value("value", Post) of
		undefined ->
			infinity;
		"infinity" ->
			infinity;
		X ->
			case list_to_integer(X) of
				0 ->
					infinity;
				Nx ->
					Nx
			end
	end,
	case rpc:call(Atomnode, cpx_supervisor, set_value, [max_ringouts, Val]) of
		{atomic, ok} ->
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		Else ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, list_to_binary(io_lib:format("~p", [Else]))}]})}
	end;
api({modules, _Node, "cpx_supervisor", "update"}, ?COOKIE, _Post) ->
	{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"update what now?">>}]})};
api({modules, Node, "cpx_supervisor", "update", "default_ringout"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
	Val = case proplists:get_value("value", Post) of
		undefined ->
			60;
		Else ->
			list_to_integer(Else)
	end,
	case rpc:call(Atomnode, cpx_supervisor, set_value, [default_ringout, Val]) of
		{atomic, ok} ->
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		Err ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, list_to_binary(io_lib:format("~p", [Err]))}]})}
	end;
api({modules, Node, "cpx_supervisor", "update", "archivepath"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
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
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, list_to_binary(io_lib:format("~p", [Else]))}]})}
	end;
api({modules, Node, "cpx_supervisor", "update", "mantispath"}, ?COOKIE, Post) ->
	Atomnode = list_to_atom(Node),
	{Func, Args} = case proplists:get_value("value", Post) of
		"" ->
			{drop_value, [mantispath]};
		Data ->
			{set_value, [mantispath, Data]}
	end,
	case rpc:call(Atomnode, cpx_supervisor, Func, Args) of
		{atomic, ok} ->
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		Else ->
			?INFO("dropping mantispath: ~p", [Else]),
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, list_to_binary(io_lib:format("~p", [Else]))}]})}
	end;
api({modules, Node, "cpx_supervisor", "update", "transferprompt"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
	{struct, Prompts} = mochijson2:decode(proplists:get_value("prompts", Post, "{\"label\":[],\"name\":[],\"regex\":[]}")),
	Skills = parse_posted_skills(proplists:get_all_values("skills", Post)),
	PromptNames = proplists:get_value(<<"name">>, Prompts),
	PromptLabels = proplists:get_value(<<"label">>, Prompts),
	PromptRegExs = proplists:get_value(<<"regex">>, Prompts),
	Zipped = lists:zip3(PromptNames, PromptLabels, PromptRegExs),
	{Func, Args} = case {Zipped, Skills} of
		{[], []} ->
			{drop_value, [transferprompt]};
		{_, _} ->
			{set_value, [transferprompt, {Zipped, Skills}]}
	end,
	case rpc:call(Atomnode, cpx_supervisor, Func, Args) of
		{atomic, ok} ->
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		Else ->
			?INFO("Setting transfer prompt:  ~p", [Else]),
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, list_to_binary(io_lib:format("~p", [Else]))}]})}
	end;
api({modules, Node, "freeswitch_media_manager", "update"}, ?COOKIE, Post) ->
	Atomnode = list_to_existing_atom(Node),
	case proplists:get_value("enabled", Post) of
		undefined ->
			rpc:call(Atomnode, cpx, unload_plugin, [oacd_freeswitch]),
			%rpc:call(Atomnode, cpx_supervisor, destroy, [freeswitch_media_manager], 2000),
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		_Else ->
			PostToProp = [
				{"dialstring", dialstring},
				{"sipEndpoint", sip},
				{"iax2Endpoint", iax2},
				{"h323Endpoint", h323},
				{"sipauth", sipauth},
				{"realms", realms}
			],
			Builder = fun({PostKey, RealKey}, Acc) ->
				case proplists:get_value(PostKey, Post) of
					undefined ->
						Acc;
					Else ->
						[{RealKey, Else} | Acc]
				end
			end,
			MidOptions = lists:foldl(Builder, [], PostToProp),
			Options = [X || {_K, Val} = X <- MidOptions, Val =/= ""],
			Args = [{freeswitch_node, list_to_atom(proplists:get_value("cnode", Post))} | Options],
			Args0 = case proplists:get_value(sipauth,Args) of
				"sipauth" -> [sipauth | proplists:delete(sipauth,Args)];
				_ -> proplists:delete(sipauth,Args)
			end,
			Realms1 = proplists:get_value(realms, Args0, []),
			Realms2 = lists:map(fun string:strip/1, string:tokens(Realms1, ",")),
			Args1 = [{realms, Realms2} | proplists:delete(realms, Args0)],
			%Conf = #cpx_conf{ id = freeswitch_media_manager, module_name = freeswitch_media_manager, start_function = start_link, supervisor = mediamanager_sup, start_args = Args},
			%rpc:call(Atomnode, cpx_supervisor, update_conf, [freeswitch_media_manager, Conf], 2000),
			rpc:call(Atomnode, cpx, set_plugin_env, [oacd_freeswitch, Args1]),
			Json = case rpc:call(Atomnode, cpx, load_plugin, [oacd_freeswitch]) of
				{error, badarg} ->
					{ok, PluginDir} = rpc:call(Atomnode, cpx, get_env, [plugin_dir, "plugins.d"]),
					{struct, [{success,false},{<<"errcode">>,<<"MISSING_PLUGIN_DIR">>},{<<"message">>,list_to_binary(io_lib:format("Could not load plugin.  Most likely the plugin directory ~p is missing", [PluginDir]))}]};
				{error,{error,eexist}} ->
					rpc:call(Atomnode, cpx, reload_plugin, [oacd_freeswitch]),
					rpc:call(Atomnode, application, stop, [oacd_freeswitch]),
					case rpc:call(Atomnode, application, start, [oacd_freeswitch]) of
						ok ->
							rpc:call(Atomnode, cpx, save_plugin_env, [oacd_freeswitch, Args1]),
							{struct, [{success, true}]};
						{error, Err} ->
							{struct, [{success, false},{<<"errcode">>,<<"UNKNOWN_ERROR">>},{<<"message">>, list_to_binary(io_lib:format("Unexpected error occured:  ~p", [Err]))}]}
					end
			end,
			{200, [], mochijson2:encode(Json)}
	end;
api({modules, Node, "freeswitch_media_manager", "get"}, ?COOKIE, _Post) ->
	Anode = list_to_existing_atom(Node),
	Settings = rpc:call(Anode, application, get_all_env, [oacd_freeswitch]),
	Running = rpc:call(Anode, cpx, plugin_status, [oacd_freeswitch]),
	PropToPost = [
		{dialstring, <<"dialstring">>},
		{sip, <<"sipEndpoint">>},
		{iax2, <<"iax2Endpoint">>},
		{h323, <<"h323Endpoint">>},
		{sipauth, <<"sipauth">>},
		{freeswitch_node, <<"cnode">>},
		{realms, <<"realms">>}
	],
	Builder = fun({Key,Newkey},Acc) ->
		case proplists:get_value(Key,Settings) of
			undefined -> Acc;
			Else when is_list(Else), Key =:= relams ->
				[{Newkey, list_to_binary(string:join(Else, ", "))} | Acc];
			Else when is_list(Else) -> [{Newkey, list_to_binary(Else)} | Acc];
			Else when Key =:= sipauth, Else =:= true -> [{sipauth,<<"sipauth">>}|Acc];
			Else when is_atom(Else) -> [{Newkey, Else} | Acc];
			Else -> Acc
		end
	end,
	Props0 = lists:foldl(Builder,[],PropToPost),
	Enabled = case Running of
		running -> true;
		_ -> false
	end,
	Props1 = [{success,true},{enabled,Enabled}|Props0],
	Json = {struct, Props1},
	{200,[],mochijson2:encode(Json)};
	
api({modules, Node, "email_media_manager", "update"}, ?COOKIE, Post) ->
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
				{"tls", tls},
				{"barenewline", barenewline}
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
					Newval = case io_lib:fread("~d.~d.~d.~d", Val) of
						{ok, L, []} ->
							list_to_tuple(L);
						Else ->
							list_to_tuple(Else)
					end,
					[{address, Newval} | Acc];
				({barenewline, Behaviour}, Acc) ->
					SessionOptions = proplists:get_value(sessionoptions, Acc, []),
					Val = case Behaviour of
						"ignore" ->
							ignore;
						"fix" ->
							fix;
						"strip" ->
							strip;
						"error" ->
							error
					end,
					[{sessionoptions, [{allow_bare_newlines, Val} | SessionOptions]} | proplists:delete(sessionoptions, Acc)];
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
				supervisor = mediamanager_sup,
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
	
api({modules, Node, "email_media_manager", "get"}, ?COOKIE, _Post) ->
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
					{A, B, C, D} ->
						iolist_to_binary(io_lib:format("~b.~b.~b.~b", [A, B, C, D]))
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
				end},
			{<<"barenewline">>, proplists:get_value(allow_bare_newlines, proplists:get_value(sessionoptions, Args, []), error)}
				%{<<"sessionoptions">>, {struct, [
							%{"barenewline", proplists:get_value(allow_bare_newlines, proplists:get_value(sessionoptions, Args, []), error)}
							% TODO more options are available
						%]}}
			],
			{200, [], mochijson2:encode({struct, Sendargs})}
	end;
api({modules, _Node, "email_media_manager", "getMappings"}, ?COOKIE, _Post) ->
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
api({modules, _Node, "email_media_manager", "setMapping"}, ?COOKIE, Post) ->
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
api({modules, _Node, "email_media_manager", "destroyMapping"}, ?COOKIE, Post) ->
	case email_media_manager:destroy_mapping(proplists:get_value("address", Post)) of
		{aborted, Reason} ->
			Json = {struct, [{success, false}, {message, list_to_binary(lists:flatten(io_lib:format("~p", [Reason])))}]},
			{200, [], mochijson2:encode(Json)};
		{atomic, ok} ->
			{200, [], mochijson2:encode({struct, [{success, true}]})}
	end;
api({modules, _Node, "email_media_manager", "new"}, ?COOKIE, Post) ->
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
	Opts0 = try list_to_integer(proplists:get_value("autowrapup", Post)) of
		I ->
			[{autoend_wrapup, I}]
	catch
		error:badarg ->
			[] 
	end,
	Opts1 = case proplists:get_value("url_pop", Post, "") of
		"" -> Opts0;
		Url -> [{url_pop, Url} | Opts0]
	end,
	Opts2 = try list_to_integer(proplists:get_value("ringout", Post, "60")) of
		I2 -> [{"ringout",I2}|Opts1]
	catch
		error:badarg -> Opts1
	end,
	Client = #client{
		label = undefined,
		id = undefined,
		options = Opts2
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
	AccFun = fun({Key, Value}, Acc) ->
		case Key of
			"autoend_wrapup" ->
				try list_to_integer(Value) of
					0 -> Acc;
					N -> [{autoend_wrapup, list_to_integer(Value)} | Acc]
				catch
					error:badarg -> Acc
				end;
			"url_pop" ->
				[{url_pop, Value} | Acc];
			_BadKey ->
				Acc
		end
	end,
	Options = lists:foldl(AccFun, [], Post),
	case call_queue_config:set_client(ClientId, Label, Options) of
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
			{file, {"index.html", util:priv_dir("www/admin/")}};
		"/getsalt" ->
			{api, getsalt};
		"/login" ->
			{api, login};
		"/logout" ->
			{api, logout};
		"/checkcookie" ->
			{api, checkcookie};
		"/dialer" ->
			{api, dialer};
		"/api" ->
			api;
		"/loadAgentsFromFile" ->
			{api, load_agents};
		_Other ->
			% section/action (params in post data)
			case util:string_split(Path, "/") of
				["", "dynamic" | Tail] ->
					File = string:join(Tail, "/"),
					Dynamic = case application:get_env('OpenACD', webdir_dynamic) of
						undefined ->
							util:priv_dir("www/dynamic");
						{ok, WebDirDyn} ->
							WebDirDyn
					end,
					case filelib:is_regular(Dynamic ++ "/" ++ File) of
						true ->
							{file, {File, Dynamic}};
						false ->
							{api, {undefined, Path}}
					end;
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
				["", "modules", Action] ->
					{api, {modules, Action}};
				["", "modules", Node, Media, Action] ->
					{api, {modules, Node, Media, Action}};
				["", "modules", Node, Media, Action, Other] ->
					{api, {modules, Node, Media, Action, Other}};
				["", "clients", Action] ->
					{api, {clients, Action}};
				["", "clients", Client, Action] ->
					{api, {clients, Client, Action}};
				Allothers ->
					Adminpath = string:concat(util:priv_dir("www/admin"), Path),
					Contribpath = string:concat(util:priv_dir("www/contrib"), Path),
					case {filelib:is_regular(Adminpath), filelib:is_regular(Contribpath)} of
						{true, _} ->
							{file, {string:strip(Path, left, $/), util:priv_dir("www/admin/")}};
						{false, true} ->
							{file, {string:strip(Path, left, $/), util:priv_dir("www/contrib/")}};
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

-spec(parse_posted_skills/1 :: (PostedSkills :: [string()]) -> [atom() | {atom(), string()}]).
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

encode_client(Client) ->
	FirstOptionsList = encode_client_options(Client#client.options),
	Optionslist = case proplists:get_value(autowrapup, FirstOptionsList) of
		undefined -> [{autowrapup, 0} | FirstOptionsList];
		_ -> FirstOptionsList
	end,
	{struct, [
		{<<"label">>, (case is_list(Client#client.label) of true -> list_to_binary(Client#client.label); false -> <<"">> end)},
		{<<"id">>, (case is_list(Client#client.id) of true -> list_to_binary(Client#client.id); false -> <<"">> end)},
		{<<"integration">>, Client#client.last_integrated} |
		Optionslist
	]}.
		
encode_client_options(List) ->
	encode_client_options(List, []).

encode_client_options([], Acc) ->
	lists:reverse(Acc);
encode_client_options([{url_pop, Format} | Tail], Acc) ->
	encode_client_options(Tail, [{url_pop, list_to_binary(Format)} | Acc]);
encode_client_options([{autoend_wrapup, N} | Tail], Acc) ->
	encode_client_options(Tail, [{autoend_wrapup, N} | Acc]);
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

-spec(stringify_skills/1 :: ([any()]) -> [string()]).
stringify_skills(List) ->
	stringify_skills(List, []).

stringify_skills([], Acc) ->
	lists:reverse(Acc);
stringify_skills([{Atom, Val} | Tail], Acc) ->
	H = list_to_binary([${, atom_to_list(Atom), $,, Val, $}]),
	stringify_skills(Tail, [H | Acc]);
stringify_skills([Atom | Tail], Acc) ->
	stringify_skills(Tail, [Atom | Acc]).

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
		{skills, stringify_skills(Group#queue_group.skills)},
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
		{firstname, list_to_binary(Agentrec#agent_auth.firstname)},
		{endpoints, encode_endpoints(Agentrec#agent_auth.endpoints, [])}
	]}.

-spec(encode_endpoints/2 :: (Endpoints :: [{atom(), any()}], Acc :: [simple_json()]) -> [simple_json()]).
encode_endpoints([], Acc) -> {struct, lists:reverse(Acc)};
encode_endpoints([{freeswitch_media, Props}|T], Acc) ->
	FwType = proplists:get_value(type, Props, null),
	FwData = case proplists:get_value(data, Props) of
		undefined -> null;
		Dat -> list_to_binary(Dat)
	end,
	
	Persistant = case proplists:get_value(persistant, Props) of
		true -> true;
		_ -> false
	end,

	Struct = {freeswitch_media, {struct, [{type, FwType}, {data, FwData},
		{persistant, Persistant}]}},
	encode_endpoints(T, [Struct|Acc]);
encode_endpoints([{email_media, _Props}|T], Acc) ->
	encode_endpoints(T, [{email_media, {struct, []}}|Acc]);
encode_endpoints([{dummy_media, Opt}|T], Acc) ->
	encode_endpoints(T, [{dummy_media, {struct, [{dummyMediaEndpoint, Opt}]}}|Acc]);
encode_endpoints([_E|T], Acc) ->
	?WARNING("cannot encode endpoint: ~p", [_E]),
	encode_endpoints(T, Acc).


	
decode_endpoints(undefined) -> [];
decode_endpoints(Bin) ->
	case catch mochijson2:decode(Bin) of
		{'EXIT', _} ->
			?WARNING("error decoding endpoint json: ", [Bin]);
		{struct, Props} ->
			decode_endpoints(Props, [])
	end.

decode_endpoints([], Acc) -> lists:reverse(Acc);
decode_endpoints([{<<"freeswitch_media">>, {struct, Props}}|T], Acc) ->
	FwType = case proplists:get_value(<<"type">>, Props) of
			<<"sip_registration">> -> sip_registration;
			<<"sip">> -> sip;
			<<"iax">> -> iax;
			<<"h323">> -> h323;
			<<"pstn">> -> pstn;
			_ -> undefined
		end,

	case FwType of
		undefined ->
			?WARNING("not adding fw endpoint. unknown type: ~p", [FwType]),
			Acc;
		_ ->
			FwData = binary_to_list(proplists:get_value(<<"data">>, 
				Props, <<>>)),
			Persistant = case proplists:get_value(<<"persistant">>, 
				Props) of
					true -> true;
					_ -> undefined
			end,
			decode_endpoints(T, 
				[{freeswitch_media, [{type, FwType},
					{data, FwData}, {persistant, Persistant}]} | Acc])
	end;
decode_endpoints([{<<"email_media">>, _}|T], Acc) ->
	decode_endpoints(T, [{email_media, []} | Acc]);
decode_endpoints([{<<"dummy_media">>, {struct, Props}}|T], Acc) ->
		Opt = case proplists:get_value(<<"dummyMediaEndpoint">>, Props) of
			<<"ring_channel">> ->  ring_channel;
			<<"inband">> -> inband;
			<<"outband">> -> outband;
			<<"persistant">> -> persistant;
			_ -> {error, unknown_dummy_endpoint}
		end,
		case Opt of
			{error, Err} ->
				?WARNING("not adding dummy_media. error: ~p", [Err]),
				Acc;
			_ ->
				decode_endpoints(T, [{dummy_media, Opt}|Acc])
		end;
decode_endpoints([_Other|T], Acc) ->
	?WARNING("unknown endpoint: ~p", [_Other]),
	Acc.


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
	Conditions = decode_recipe_conditions(proplists:get_value(<<"conditions">>, Proplist)),
	Actions = decode_recipe_actions(proplists:get_value(<<"actions">>, Proplist)),
	Runs = case proplists:get_value(<<"runs">>, Proplist) of
		<<"run_once">> ->
			run_once;
		<<"run_many">> ->
			run_many
	end,
	Comment = proplists:get_value(<<"comment">>, Proplist, <<"">>),
	decode_recipe(Tail, [{Conditions, Actions, Runs, Comment} | Acc]).

decode_recipe_actions(Actions) ->
	decode_recipe_actions(Actions, []).

decode_recipe_actions([], Acc) ->
	lists:reverse(Acc);
decode_recipe_actions([{struct, Proplist} | Tail], Acc) ->
	Actbin = proplists:get_value(<<"action">>, Proplist),
	Argbin = proplists:get_value(<<"arguments">>, Proplist),
	Head = case Actbin of
		<<"add_skills">> ->
			{add_skills, decode_recipe_args(add_skills, Argbin)};
		<<"remove_skills">> ->
			{remove_skills, decode_recipe_args(remove_skills, Argbin)};
		<<"set_priority">> ->
			{set_priority, decode_recipe_args(set_priority, Argbin)};
		<<"prioritize">> ->
			{prioritize, []};
		<<"deprioritize">> ->
			{deprioritize, []};
		<<"voicemail">> ->
			{voicemail, decode_recipe_args(voicemail, Argbin)};
		<<"announce">> ->
			{announce, decode_recipe_args(announce, Argbin)};
%% TODO added for testing only (implemented with focus on real Calls - no other media)
		<<"end_call">> ->
			{end_call, []};
		<<"add_recipe">> ->
			{add_recipe, decode_recipe_args(add_recipe, Argbin)}
	end,
	decode_recipe_actions(Tail, [Head | Acc]).

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
	%% TODO added for testing only (implemented with focus on real Calls - no other media)
decode_recipe_args(end_call, _Args) ->
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
		<<"comp-=">> ->
			'=';
		<<"comp->">> ->
			'>';
		<<"comp-<">> ->
			'<';
		<<"comp-!=">> ->
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
		{<<"prop-ticks">>, '=', Val} when is_integer(Val) ->
			{ticks, Val};
		{<<"prop-eligible_agents">>, Comp, Val} when is_integer(Val) ->
			{eligible_agents, Comp, Val};
		{<<"prop-agents_avail">>, Comp, Val} when is_integer(Val) ->
			{available_agents, Comp, Val};
		{<<"prop-queue_position">>, Comp, Val} when is_integer(Val) ->
			{queue_position, Comp, Val};
		{<<"prop-calls_queued">>, Comp, Val} when is_integer(Val) ->
			{calls_queued, Comp, Val};
		{<<"prop-client">>, Comp, Val} ->
			{client, Comp, Val};
		{<<"prop-hour">>, Comp, Val} when is_integer(Val) ->
			{hour, Comp, Val};
		{<<"prop-weekday">>, Comp, Val} when is_integer(Val) ->
			{weekday, Comp, Val};
		{<<"prop-mediatype">>, Comp, Val} ->
			{type, Comp, Val};
		{<<"prop-client_calls_queued">>, Comp, Val} when is_integer(Val) ->
			{client_calls_queued, Comp, Val};
		{<<"prop-caller_id">>, Comp, Val} ->
			{caller_id, Comp, Val};
		{<<"prop-caller_name">>, Comp, Val} ->
			{caller_name, Comp, Val}
	end,
	decode_recipe_conditions(Tail, [Tuple | Acc]).	
	
encode_recipe(Recipe) ->
	encode_recipe_steps(Recipe, []).

encode_recipe_steps([], Acc) ->
	lists:reverse(Acc);
encode_recipe_steps([Step | Tail], Acc) ->
	Jstep = encode_recipe_step(Step),
	encode_recipe_steps(Tail, [Jstep | Acc]).

encode_recipe_step({Conditions, Actions, Runs, Comment}) ->
	Jcond = encode_recipe_conditions(Conditions),
	Jactions = encode_recipe_actions(Actions),
	{struct, [
		{<<"conditions">>, Jcond},
		{<<"actions">>, Jactions},
		{<<"runs">>, Runs},
		{<<"comment">>, Comment}
	]}.

encode_recipe_conditions(Conditions) ->
	encode_recipe_conditions(Conditions, []).

encode_recipe_conditions([], Acc) ->
	lists:reverse(Acc);
encode_recipe_conditions([{ticks, Num} | Tail], Acc) ->
	encode_recipe_conditions([{ticks, '=', Num} | Tail], Acc);
%encode_recipe_conditions([{type, Comp, Num} | Tail], Acc) ->
%	encode_recipe_conditions([{<<"mediatype">>, Comp, Num} | Tail], Acc);
encode_recipe_conditions([{Prop, Comp, Val} | Tail], Acc) ->
	Fixedval = case Val of
		Val when is_integer(Val) ->
			Val;
		Val when is_list(Val) ->
			list_to_binary(Val);
		Val when is_atom(Val) ->
			Val
	end,
	Jcond = {struct, [
		{<<"property">>, encode_recipe_conditions_prop(Prop)},
		{<<"comparison">>, encode_recipe_conditions_operator(Comp)},
		{<<"value">>, Fixedval}
	]},
	encode_recipe_conditions(Tail, [Jcond | Acc]).

encode_recipe_conditions_operator(Op) ->
	list_to_binary(["comp-", atom_to_list(Op)]).

encode_recipe_conditions_prop(type) ->
	encode_recipe_conditions_prop(mediatype);
encode_recipe_conditions_prop(Prop) ->
	list_to_binary(["prop-", atom_to_list(Prop)]).

encode_recipe_actions(Actions) ->
	encode_recipe_actions(Actions, []).

encode_recipe_actions([], Acc) ->
	lists:reverse(Acc);
encode_recipe_actions([{Operation, Args} | Tail], Acc) ->
	Jargs = case Operation of
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
		eligible_agents ->
			Args;
		client ->
			list_to_binary(Args);
		add_recipe ->
			% TODO:  more encoding
			<<"">>;
		%% TODO added for testing only (implemented with focus on real Calls - no other media)
		end_call ->
			<<"">>
	end,
	Head = {struct, [
		{<<"action">>, Operation},
		{<<"arguments">>, Jargs}
	]},
	encode_recipe_actions(Tail, [Head | Acc]).

encode_modules([], Acc) ->
	lists:reverse(Acc);
encode_modules([{Node, Medias} | Tail], Acc) ->
	Json = {struct, [
		{<<"name">>, list_to_binary(atom_to_list(Node))},
		{<<"type">>, <<"node">>},
		{<<"id">>, list_to_binary(atom_to_list(Node))},
		{<<"modules">>, encode_modules_confs(Node, Medias, [])}
	]},
	encode_modules(Tail, [Json | Acc]).

encode_modules_confs(_Node, [], Acc) ->
	lists:reverse(Acc);
encode_modules_confs(Node, [{cpx_supervisor, BadConf} | Tail], Acc) ->
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
	encode_modules_confs(Node, Tail, [Json | Acc]);
encode_modules_confs(Node, [{Mod, Conf} | Tail], Acc) when is_record(Conf, cpx_conf) ->
	Json = {struct, [
		{<<"name">>, list_to_binary(atom_to_list(Conf#cpx_conf.module_name))},
		{<<"enabled">>, true},
		{<<"type">>, <<"conf">>},
		{<<"id">>, list_to_binary(atom_to_list(Node) ++ "/" ++ atom_to_list(Mod))},
		{<<"start">>, list_to_binary(atom_to_list(Conf#cpx_conf.start_function))},
		{<<"node">>, list_to_binary(atom_to_list(Node))}
	]},
	encode_modules_confs(Node, Tail, [Json | Acc]);
encode_modules_confs(Node, [{Mod, undefined} | Tail], Acc) ->
	Json = {struct, [
		{<<"name">>, list_to_binary(atom_to_list(Mod))},
		{<<"enabled">>, false},
		{<<"type">>, <<"conf">>},
		{<<"id">>, list_to_binary(atom_to_list(Node) ++ "/" ++ atom_to_list(Mod))},
		{<<"node">>, list_to_binary(atom_to_list(Node))}
	]},
	encode_modules_confs(Node, Tail, [Json | Acc]).

list_to_terms(List) ->
	list_to_terms(List, 1, []).

list_to_terms([], _Loc, Acc) ->
	lists:reverse(Acc);
list_to_terms(List, Location, Acc) ->
	case erl_scan:tokens([], List, Location) of
		{done, {ok, Tokens, _End}, Tail} ->
			{ok, Term} = erl_parse:parse_term(Tokens),
			NewAcc = [Term | Acc],
			list_to_terms(Tail, 1, NewAcc);
		{done, {eof, _Locaction}, _Tail} ->
			list_to_terms([], Location, Acc);
		{done, Error, _Tail} ->
			Error;
		{more, Continuation} ->
			?NOTICE("list to term ending early.  ~p", [Continuation]),
			list_to_terms([], Location, Acc)
	end.

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
	util:start_testnode(),
	N = util:start_testnode(cpx_web_management_cookie_tests),
	{spawn,
	N,
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
	]}}.

recipe_encode_decode_test_() ->
	[{"Simple encode",
	?_assertEqual([{struct, [
		{<<"conditions">>, [{struct, [
			{<<"property">>, ticks},
			{<<"comparison">>, '='},
			{<<"value">>, 3}
		]}]},
		{<<"actions">>, [{struct, [
			{<<"action">>, set_priority},
			{<<"arguments">>, 5}
		]}]},
		{<<"runs">>, run_once},
		{<<"comment">>, <<"commented">>}
	]}], encode_recipe([{[{ticks, 3}], [{set_priority, 5}], run_once, <<"commented">>}]))},
	{"Simple decode",
	?_assertEqual([{[{ticks, 3}], [{set_priority, 5}], run_once, <<"commented">>}], decode_recipe("[{\"conditions\":[{\"property\":\"ticks\",\"comparison\":\"=\",\"value\":3}],\"actions\":[{\"action\":\"set_priority\",\"arguments\":\"5\"}],\"runs\":\"run_once\",\"comment\":\"commented\"}]"))}].

decode_endpoints_test_() ->
	[{"decode freeswitch_media endpoint type " ++ Str,
		fun() ->
			[{freeswitch_media, P}] = 
				decode_endpoints(iolist_to_binary(["{\"freeswitch_media\":{\"type\": \"", Str, "\"}}"])),
			?assertEqual(Atm, proplists:get_value(type, P))
		end} || {Str, Atm} <- [{"sip_registration", sip_registration}, 
		{"sip", sip}, {"iax", iax}, {"h323", h323}, {"pstn", pstn}]] ++

	[{"decode freeswitch_media endpoint empty type",
		?_assertEqual([], decode_endpoints(<<"{\"freeswitch_media\":{}}">>))
	},
	{"decode freeswitch_media endpoint data",
		fun() ->
			[{freeswitch_media, P}] = 
				decode_endpoints(<<"{\"freeswitch_media\":{\"type\": \"sip\", \"data\":\"1001\"}}">>),
			?assertEqual("1001", proplists:get_value(data, P))
		end
	},
	{"decode freeswitch_media endpoint empty data",
		fun() ->
			[{freeswitch_media, P}] = 
				decode_endpoints(<<"{\"freeswitch_media\":{\"type\": \"sip\"}}">>),
			?assertEqual("", proplists:get_value(data, P))
		end
	},
	{"decode freeswitch_media endpoint persistant true",
		fun() ->
			[{freeswitch_media, P}] = 
				decode_endpoints(<<"{\"freeswitch_media\":{\"type\": \"sip\", \"persistant\": true}}">>),
			?assertEqual(true, proplists:get_value(persistant, P))
		end
	},
	{"decode freeswitch_media endpoint persistant false",
		fun() ->
			[{freeswitch_media, P}] = 
				decode_endpoints(<<"{\"freeswitch_media\":{\"type\": \"sip\", \"persistant\": false}}">>),
			?assertEqual(undefined, proplists:get_value(persistant, P))
		end
	},
	{"decode freeswitch endpoint empty persistant",
		fun() ->
			[{freeswitch_media, P}] = 
				decode_endpoints(<<"{\"freeswitch_media\":{\"type\": \"sip\"}}">>),
			?assertEqual(undefined, proplists:get_value(persistant, P))
		end
	},
	{"decode email_media endpoint",
		?_assertMatch([{email_media, _}],
			decode_endpoints(<<"{\"email_media\":{}}">>))
	},
	{"decode dummy_media endpoint",
		[?_assertEqual([{dummy_media, ring_channel}],
			decode_endpoints(<<"{\"dummy_media\":{\"dummyMediaEndpoint\":\"ring_channel\"}}">>)),
		?_assertEqual([{dummy_media, inband}],
			decode_endpoints(<<"{\"dummy_media\":{\"dummyMediaEndpoint\":\"inband\"}}">>)),
		?_assertEqual([{dummy_media, outband}],
			decode_endpoints(<<"{\"dummy_media\":{\"dummyMediaEndpoint\":\"outband\"}}">>)),
		?_assertEqual([{dummy_media, persistant}],
			decode_endpoints(<<"{\"dummy_media\":{\"dummyMediaEndpoint\":\"persistant\"}}">>)),
		?_assertEqual([], decode_endpoints(<<"{\"dummy_media\":{\"dummyMediaEndpoint\":\"foobaz\"}}">>))]
	}].

api_test_() ->
	util:start_testnode(),
	N = util:start_testnode(cpx_web_management_api_tests),
	{spawn,
	N,
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

		os:cmd("ssh-keygen -t rsa -f ../key -N \"\""),
		Cookie
	end,
	fun(_Whatever) -> 
		cpx_supervisor:stop(),
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		file:delete("../key"),
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
				#agent_profile{name = Name, skills = Skills} = agent_auth:get_profile("newprofile"),
				?assertEqual("newprofile", Name),
				?assert(lists:member('_all', Skills)),
				?assert(lists:member(english, Skills))
			end}
		end,
		fun(Cookie) ->
			{"/agents/profiles/Default/delete does nothing",
			fun() ->
				{200, [], _Json} = api({agents, "profiles", "Default", "delete"}, Cookie, []),
				?assertMatch(#agent_profile{name = "Default", skills = [], id = "0"}, agent_auth:get_profile("Default"))
			end}
		end,
		fun(Cookie) ->
			{"/agents/profiles/someprofile/delete kills the profile",
			fun() ->
				?CONSOLE("~p", [agent_auth:new_profile(#agent_profile{name = "someprofile", skills = []})]),
				?assertEqual(#agent_profile{name = "someprofile", skills = [], id = "1"}, agent_auth:get_profile("someprofile")),
				{200, [], _Json} = api({agents, "profiles", "someprofile", "delete"}, Cookie, []),
				?assertEqual(undefined, agent_auth:get_profile("someprofile"))
			end}
		end,
		fun(Cookie) ->
			{"/agents/profiles/someprofile/update updates the profile, and corrects the agents",
			fun() ->
				agent_auth:new_profile(#agent_profile{name = "someprofile", skills = ['_all', english]}),
				agent_auth:add_agent("someagent", "", [], agent, "someprofile"),
				{200, [], _Json} = api({agents, "profiles", "someprofile", "update"}, Cookie, [{"name", "newprofile"}, {"skills", "_all"}, {"order", "1"}]),
				?assertEqual(#agent_profile{name = "newprofile", id = "1", skills = ['_all']}, agent_auth:get_profile("newprofile")),
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
					{"profile", "Default"},
					{"endpoints", "{\"freeswitch_media\":{\"type\":\"sip\", \"data\":\"1001\", \"persistant\":true}}"}
				],
				{200, [], _Json} = api({agents, "agents", "new"}, Cookie, Post),
				{atomic, [Rec]} = agent_auth:get_agent("someagent"),
				?CONSOLE("~p", [Rec#agent_auth.skills]),

				?assertEqual(true, lists:member(english, Rec#agent_auth.skills)),
				?assertEqual(true, lists:member({'_brand', "Somebrand"}, Rec#agent_auth.skills)),
				?assertEqual([{data,"1001"}, {persistant,true}, {type,sip}],
				 	lists:sort(proplists:get_value(freeswitch_media, Rec#agent_auth.endpoints))),
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
					{"profile", "Default"},
					{"endpoints", "{\"freeswitch_media\":{\"type\":\"sip\", \"data\":\"1001\", \"persistant\":true}}"}
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
					{"login", "renamed"},
					{"endpoints", "{\"freeswitch_media\":{\"type\":\"sip\", \"data\":\"1001\", \"persistant\":true}}"}
				],
				api({agents, "agents", Oldrec#agent_auth.id, "update"}, Cookie, Post),
				?assertEqual({atomic, []}, agent_auth:get_agent("someagent")),
				{atomic, [Rec]} = agent_auth:get_agent("renamed"),
				?assertEqual(agent, Rec#agent_auth.securitylevel),
				?assert(lists:member(english, Rec#agent_auth.skills)),
				?assert(lists:member({'_brand', "Somebrand"}, Rec#agent_auth.skills)),
				?assertEqual(Oldrec#agent_auth.password, Rec#agent_auth.password),
				?assertEqual([{data,"1001"}, {persistant,true}, {type,sip}],
				 	lists:sort(proplists:get_value(freeswitch_media, Rec#agent_auth.endpoints))),
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
					{"login", "renamed"},
					{"endpoints", "{\"freeswitch_media\":{\"type\":\"sip\", \"data\":\"1001\", \"persistant\":true}}"}
				],
				api({agents, "agents", Oldrec#agent_auth.id, "update"}, Cookie, Post),
				?assertEqual({atomic, []}, agent_auth:get_agent("someagent")),
				{atomic, [Rec]} = agent_auth:get_agent("renamed"),
				?assertEqual(agent, Rec#agent_auth.securitylevel),
				?assert(lists:member(english, Rec#agent_auth.skills)),
				?assert(lists:member({'_brand', "Somebrand"}, Rec#agent_auth.skills)),
				?assertNot(Oldrec#agent_auth.password =:= Rec#agent_auth.password),
				?assertEqual([{data,"1001"}, {persistant,true}, {type,sip}],
				 	lists:sort(proplists:get_value(freeswitch_media, Rec#agent_auth.endpoints))),
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
					{"login", "renamed"},
					{"endpoints", "{\"freeswitch_media\":{\"type\":\"sip\", \"data\":\"1001\", \"persistant\":true}}"}
				],
				?assertError({case_clause, "newpass"}, api({agents, "agents", Oldrec#agent_auth.id, "update"}, Cookie, Post)),
				?assertEqual({atomic, []}, agent_auth:get_agent("renamed")),
				{atomic, [Rec]} = agent_auth:get_agent("someagent"),
				?assertEqual(supervisor, Rec#agent_auth.securitylevel),
				?assertNot(lists:member(english, Rec#agent_auth.skills)),
				?assertNot(lists:member({'_brand', "Somebrand"}, Rec#agent_auth.skills)),
				?assert(Oldrec#agent_auth.password =:= Rec#agent_auth.password),
				?assertEqual(undefined,
				 	proplists:get_value(freeswitch_media, Rec#agent_auth.endpoints)),
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
					{"login", "renamed"},
					{"endpoints", "{\"freeswitch_media\":{\"type\":\"sip\", \"data\":\"1001\", \"persistant\":true}}"}
				],
				api({agents, "agents", Oldrec#agent_auth.id, "update"}, Cookie, Post),
				?assertEqual({atomic, []}, agent_auth:get_agent("someagent")),
				{atomic, [Rec]} = agent_auth:get_agent("renamed"),
				?assertEqual(agent, Rec#agent_auth.securitylevel),
				?assert(lists:member(english, Rec#agent_auth.skills)),
				?assert(lists:member({'_brand', "Somebrand"}, Rec#agent_auth.skills)),
				?assert(Oldrec#agent_auth.password =:= Rec#agent_auth.password),
				?assertEqual(undefined,
				 	proplists:get_value(freeswitch_media, Rec#agent_auth.endpoints)),
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
				Recipe = [{[{ticks, 5}], [{prioritize, []}], run_many, <<"Comment">>}],
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
				Recipe = [{[{ticks, 5}], [{prioritize, []}], run_many, <<"comment">>}],
				Qgrouprec = #queue_group{
					name = "Test Q Group",
					recipe = Recipe,
					sort = 35
				},
				call_queue_config:new_queue_group(Qgrouprec),
				Newrecipe = [{[{calls_queued, '<', 200}], [{deprioritize, []}], run_once, <<"new comment">>}],
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
				Recipe = [{[{ticks, 5}], prioritize, [], run_many, <<"comment">>}],
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
%				{"/modules/Thisnode/freeswitch_media_manager/update disabling",
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
%					api({modules, atom_to_list(node()), "freeswitch_media_manager", "update"}, Cookie, []),
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
%				{"/modules/Thisnode/freeswitch_media_manager/update enabling",
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
%					api({modules, atom_to_list(node()), "freeswitch_media_manager", "update"}, Cookie, Post),
%					Res = cpx_supervisor:get_conf(freeswitch_media_manager),
%					?assert(rec_equals(Fsconf, Res)),
%					cpx_supervisor:destroy(freeswitch_media_manager)
%				end}
%			]}
%		end
	]}}.
		
-endif.
