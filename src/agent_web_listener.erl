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

%% @doc Listens for new web connections, then spawns an {@link agent_web_connection} to handle the details.
%% Uses Mochiweb for the heavy lifting.
%% @see agent_web_connection
-module(agent_web_listener).
-author("Micah").

-behaviour(gen_server).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("call.hrl").
-include("agent.hrl").

-define(PORT, 5050).
-define(WEB_DEFAULTS, [{name, ?MODULE}, {port, ?PORT}]).
-define(MOCHI_NAME, aweb_mochi).

%% API
-export([start_link/1, start/1, start/0, start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-type(salt() :: string() | 'undefined').
-type(connection_handler() :: pid() | 'undefined').
-type(web_connection() :: {string(), salt(), connection_handler()}).

-record(state, {
	connections, % ets table of the connections
	mochipid % pid of the mochiweb process.
}).

%%====================================================================
%% API
%%====================================================================

start() -> 
	start(?PORT).

start(Port) -> 
	gen_server:start({local, ?MODULE}, ?MODULE, [Port], []).
	
start_link() ->
	start_link(?PORT).

start_link(Port) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

stop() ->
	gen_server:call(?MODULE, stop).
	
%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Port]) ->
	?CONSOLE("Starting on port ~p", [Port]),
	process_flag(trap_exit, true),
	crypto:start(),
	Table = ets:new(web_connections, [set, public, named_table]),
	{ok, Mochi} = mochiweb_http:start([{loop, fun(Req) -> loop(Req, Table) end}, {name, ?MOCHI_NAME}, {port, Port}]),
    {ok, #state{connections=Table, mochipid = Mochi}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
	{stop, shutdown, ok, State};
handle_call(Request, _From, State) ->
    {reply, {unknown_call, Request}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_info(Info, State) ->
	?CONSOLE("Info:  ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(shutdown, _State) ->
	?CONSOLE("shutdown", []),
	mochiweb_socket_server:stop(?MOCHI_NAME),
	ets:delete(web_connections),
    ok;
terminate(normal, _State) ->
	?CONSOLE("normal exit", []),
	mochiweb_socket_server:stop(?MOCHI_NAME),
	ets:delete(web_connections),
    ok;
terminate(Reason, _State) ->
	?CONSOLE("Terminating dirty:  ~p", [Reason]),
    ok.


%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @doc listens for a new connection.
%% Based on the path, the loop can take several paths.
%% if the path is "/login" and there is post data, an attempt is made to start a new {@link agent_web_connection}.
%% On a successful start, a cookie is set that the key reference used by this module to link new connections
%% to the just started agent_web_connection.
%% 
%% On any other path, the cookie is checked.  The value of the cookie is looked up on an internal table to see 
%% if there is an active agent_web_connection.  If there is, further processing is done there, 
%% otherwise the request is denied.
loop(Req, Table) -> 
	Path = Req:get(path),
	case parse_path(Path) of
		{file, {File, Docroot}} ->
			Cookielist = Req:parse_cookie(),
			case proplists:get_value("cpx_id", Cookielist) of
				undefined ->
					Reflist = erlang:ref_to_list(make_ref()),
					Cookie = io_lib:format("cpx_id=~p", [Reflist]),
					ets:insert(Table, {Reflist, undefined, undefined}),
					?CONSOLE("Setting cookie and serving file ~p", [string:concat(Docroot, File)]),
					Req:serve_file(File, Docroot, [{"Set-Cookie", Cookie}]);
				_Reflist ->
					Req:serve_file(File, Docroot)
			end;
		{api, checkcookie} ->
			case check_cookie(Req:parse_cookie()) of
				{_Reflist, _Salt, Conn} when is_pid(Conn) ->
					?CONSOLE("Found agent_connection pid ~p", [Conn]),
					Agentrec = agent_web_connection:dump_agent(Conn),
					Json = {struct, [
						{<<"success">>, true},
						{<<"login">>, list_to_binary(Agentrec#agent.login)},
						{<<"state">>, Agentrec#agent.state},
						{<<"statedata">>, agent_web_connection:encode_statedata(Agentrec#agent.statedata)}]},
					Req:respond({200, [], mochijson2:encode(Json)});
				badcookie ->
					?CONSOLE("cookie not in ets", []),
					Reflist = erlang:ref_to_list(make_ref()),
					Cookie = io_lib:format("cpx_id=~p", [Reflist]),
					ets:insert(Table, {Reflist, undefined, undefined}),
					Json = {struct, [{<<"success">>, false}]},
					Req:respond({200, [{"Set-Cookie", Cookie}], mochijson2:encode(Json)});
				{_Reflist, _Salt, undefined} ->
					?CONSOLE("cookie found, no agent", []),
					Json = {struct, [{<<"success">>, false}]},
					Req:respond({200, [], mochijson2:encode(Json)})
			end;
		{api, Apirequest} ->
			% actions that don't care about the cookie
			% that would be none
			case check_cookie(Req:parse_cookie()) of
				badcookie ->
					?CONSOLE("bad cookie", []),
					Reflist = erlang:ref_to_list(make_ref()),
					Cookie = io_lib:format("cpx_id=~p", [Reflist]),
					ets:insert(Table, {Reflist, undefined, undefined}),
					Req:respond({403, [{"Set-Cookie", Cookie}], <<"Cookie reset, retry.">>});
				{Reflist, Salt, Conn} ->
					%% okay, now to handle the api stuff.
					case Apirequest of
						getsalt ->
							Newsalt = integer_to_list(crypto:rand_uniform(0, 4294967295)),
							ets:insert(web_connections, {Reflist, Newsalt, Conn}),
							?CONSOLE("created and sent salt for ~p", [Reflist]),
							Req:respond({200, [], mochijson2:encode({struct, [{success, true}, {message, <<"Salt created, check salt property">>}, {salt, list_to_binary(Newsalt)}]})});
						releaseopts ->
							Releaseopts = agent_auth:get_releases(),
							Converter = fun(#release_opt{label = Label, id = Id}) ->
								{struct, [{<<"label">>, list_to_binary(Label)}, {<<"id">>, Id}]}
							end,
							Jsons = lists:map(Converter, Releaseopts),
							Req:respond({200, [], mochijson2:encode(Jsons)});
						login ->
							case Salt of
								undefined ->
									?CONSOLE("Bad salt for login request for ~p", [Reflist]),
									Req:respond({200, [], mochijson2:encode({struct, [{success, false}, {message, <<"No salt set">>}]})});
								Salt ->
									Post = Req:parse_post(),
									Username = proplists:get_value("username", Post, ""),
									Password = proplists:get_value("password", Post, ""),
									case agent_auth:auth(Username, Password, Salt) of
										deny ->
											Req:respond({200, [], mochijson2:encode({struct, [{success, false}, {message, <<"login err">>}]})});
										{allow, Skills, Security} ->
											Agent = #agent{login = Username, skills = Skills},
											case agent_web_connection:start_link(Agent, Security) of
												{ok, Pid} ->
													ets:insert(web_connections, {Reflist, Salt, Pid}),
													?CONSOLE("connection started for ~p", [Reflist]),
													Req:respond({200, [], mochijson2:encode({struct, [{success, true}, {message, <<"logged in">>}]})});
												ignore ->
													?CONSOLE("Ignore message trying to start connection for ~p", [Reflist]),
													Req:respond({200, [], mochijson2:encode({struct, [{success, false}, {message, <<"login err">>}]})});
												{error, Error} ->
													?CONSOLE("Error ~p trying to start connection for ~p", [Error, Reflist]),
													Req:respond({200, [], mochijson2:encode({struct, [{success, false}, {message, <<"login err">>}]})})
											end
									end
							end;
						%% everying else, we need an actual connection to handle
						Apirequest ->
							case Conn of
								undefined ->
									?CONSOLE("Cannot answer api request ~p due to no connection", [Apirequest]),
									Req:respond({200, [], mochijson2:encode({struct, [{success, false}, {message, <<"Login required">>}]})});
								Conn ->
									case agent_web_connection:api(Conn, Apirequest) of
										{Code, Headers, Body} ->
											Req:respond({Code, Headers, Body})
									end
							end
					end
			end
	end.
		
		%{api, getsalt} ->
%			
%
%
%
%
%
%		{api, 
%		{api, _Any} ->
%			Req:respond({501, [], mochijson2:encode({struct, [{success, false},{message, <<"Not yet implemented!">>}]})})
%	end.
	
	
	
%	case Req:get(path) of
%		"/login" -> 
%			?CONSOLE("/login",[]),
%			Post = Req:parse_post(),
%			case Post of 
%				% normally this would check against a database and not just discard the un/pw.
%				[] -> 
%					?CONSOLE("empty post",[]),
%					Req:respond({403, [], mochijson2:encode({struct, [{success, false}, {message, <<"No post data supplied">>}]})});
%				_Any -> 
%					?CONSOLE("trying to start connection",[]),
%					Ref = make_ref(),
%					case agent_web_connection:start(Post, Ref, Table) of
%						{ok, _Aconnpid} -> 
%							Cookie = io_lib:format("cpx_id=~p", [erlang:ref_to_list(Ref)]),
%							Req:respond({200, [{"Set-Cookie", Cookie}], mochijson2:encode({struct, [{success, true}, {message, <<"Login successful">>}]})});
%						{error, Reason} -> 
%							Req:respond({403, [{"Set-Cookie", "cpx_id=0"}], mochijson2:encode({struct, [{success, false}, {message, list_to_binary(io_lib:format("~p", [Reason]))}]})});
%						ignore -> 
%							Req:respond({403, [{"Set-Cookie", "cpx_id=0"}], mochijson2:encode({struct, [{success, false}, {message, <<"ignored">>}]})})
%					end
%			end;
%		Path -> 
%			?CONSOLE("any other path",[]),
%			case Req:parse_cookie() of 
%				[{"cpx_id", Reflist}] -> 
%					?CONSOLE("cookie looks good~nReflist: ~p", [Reflist]),
%					Etsres = ets:lookup(Table, Reflist),
%					?CONSOLE("ets res:~p", [Etsres]),
%					[{_Key, Aconn, _Login} | _Rest] = Etsres,
%					Reqresponse = agent_web_connection:request(Aconn, Path, Req:parse_post(), Req:parse_cookie()),
%					Req:respond(Reqresponse);
%				_Allelse -> 
%					?CONSOLE("bad cookie",[]),
%					Req:respond({403, [], io_lib:format("Invalid cookie: ~p", [Req:parse_cookie()])})
%			end
%	end.

%% @doc determine if hte given cookie data is valid
-spec(check_cookie/1 :: ([{string(), string()}]) -> 'badcookie' | web_connection()).
check_cookie([]) ->
	badcookie;
check_cookie(Allothers) ->
	case proplists:get_value("cpx_id", Allothers) of
		undefined ->
			badcookie;
		Reflist ->
			case ets:lookup(web_connections, Reflist) of
				[] ->
					badcookie;
				[{Reflist, Salt, Conn}] ->
					{Reflist, Salt, Conn}
			end
	end.
	
%% @doc determine if the given path is an api call, or if it's a file request.
parse_path(Path) ->
	% easy tests first.
	case Path of
		"/" ->
			{file, {"index.html", "www/agent/"}};
		"/poll" ->
			{api, poll};
		"/logout" ->
			{api, logout};
		"/login" ->
			{api, login};
		"/getsalt" ->
			{api, getsalt};
		"/releaseopts" ->
			{api, releaseopts};
		"/checkcookie" ->
			{api, checkcookie};
		_Other ->
			case util:string_split(Path, "/") of 
				["", "state", Statename] ->
					{api, {set_state, Statename}};
				["", "state", Statename, Statedata] ->
					{api, {set_state, Statename, Statedata}};
				["", "ack", Counter] ->
					{api, {ack, Counter}};
				["", "err", Counter] ->
					{api, {err, Counter}};
				["", "err", Counter, Message] ->
					{api, {err, Counter, Message}};
				_Allother ->
					% is there an actual file to serve?
					case filelib:is_regular(string:concat("www/agent", Path)) of
						true ->
							{file, {string:strip(Path, left, $/), "www/agent/"}};
						false ->
							{file, {string:strip(Path, left, $/), "www/contrib/"}}
					end
			end
	end.

-ifdef(EUNIT).

cooke_file_test_() ->
	{
		foreach,
		fun() ->
			agent_web_listener:start(),
			inets:start(),
			{ok, Httpc} = inets:start(httpc, [{profile, test_prof}]),
			Httpc
		end,
		fun(Httpc) ->
			inets:stop(httpc, Httpc),
			inets:stop(),
			agent_web_listener:stop()
		end,
		[
			fun(_Httpc) ->
				{"Get a cookie on index page request",
				fun() ->
					{ok, Result} = http:request("http://127.0.0.1:5050/"),
					?assertMatch({_Statusline, _Headers, _Boddy}, Result),
					{_Line, Head, _Body} = Result,
					Cookie = proplists:get_value("set-cookie", Head),
					?assertNot(undefined =:= Cookie)
				end}
			end,
			fun(_Httpc) ->
				{"Try to get a page with a bad cookie",
				fun() ->
					{ok, {{_Httpver, Code, _Message}, Head, _Body}} = http:request(get, {"http://127.0.0.1:5050/", [{"Cookie", "goober=snot"}]}, [], []),
					?assertEqual(200, Code),
					?CONSOLE("~p", [Head]),
					Cookie = proplists:get_value("set-cookie", Head),
					?assertNot(undefinded =:= Cookie)
				end}
			end,
			fun(_Httpc) ->
				{"Get a cookie, then a request with that cookie",
				fun() ->
					{ok, {_Statusline, Head, _Body}} = http:request("http://127.0.0.1:5050/"),
					Cookie = proplists:get_value("set-cookie", Head),
					{ok, {{_Httpver, Code, _Message}, Head2, _Body2}} = http:request(get, {"http://127.0.0.1:5050", [{"Cookie", Cookie}]}, [], []),
					Cookie2 = proplists:get_value("set-cookie", Head2),
					?assertEqual(undefined, Cookie2),
					?assertEqual(200, Code)
				end}
			end
		]
	}.

cookie_api_test_() ->
	{
		foreach,
		fun() ->
			agent_web_listener:start(),
			inets:start(),
			{ok, Httpc} = inets:start(httpc, [{profile, test_prof}]),
			{ok, {_Statusline, Head, _Body}} = http:request("http://127.0.0.1:5050"),
			Cookie = proplists:get_value("set-cookie", Head),
			{Httpc, Cookie}
		end,
		fun({Httpc, _Cookie}) ->
			inets:stop(httpc, Httpc),
			inets:stop(),
			agent_web_listener:stop()
		end,
		[
			fun({_Httpc, Cookie}) ->
				{"Get a salt with a valid cookie",
				fun() ->
					{ok, {{_Ver, Code, _Msg}, _Head, Body}} = http:request(get, {"http://127.0.0.1:5050/getsalt", [{"Cookie", Cookie}]}, [], []),
					?CONSOLE("body:  ~p", [Body]),
					{struct, Pairs} = mochijson2:decode(Body),
					?assertEqual(200, Code),
					?assertEqual(true, proplists:get_value(<<"success">>, Pairs)),
					?assertEqual(<<"Salt created, check salt property">>, proplists:get_value(<<"message">>, Pairs)),
					?assertNot(undefined =:= proplists:get_value(<<"salt">>, Pairs))
				end}
			end,
			fun({_Httpc, _Cookie}) ->
				{"Get a salt with an invalid cookie",
				fun() ->
					{ok, {{_Ver, Code, _Msg}, Head, Body}} = http:request(get, {"http://127.0.0.1:5050/getsalt", [{"Cookie", "goober=snot"}]}, [], []),
					?assertEqual(403, Code),
					?assertNot(noexist =:= proplists:get_value("set-cookie", Head, noexist)),
					?assertEqual("Cookie reset, retry.", Body)
				end}
			end
		]
	}.
	
web_connection_login_test_() ->
	{
		foreach,
		fun() ->
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			mnesia:create_schema([node()]),
			mnesia:start(),
			agent_manager:start([node()]),
			agent_web_listener:start(),
			inets:start(),
			{ok, Httpc} = inets:start(httpc, [{profile, test_prof}]),
			{ok, {_Statusline, Head, _Body}} = http:request("http://127.0.0.1:5050"),
			Cookie = proplists:get_value("set-cookie", Head),
			agent_auth:start(),
			?CONSOLE("~p", [agent_auth:add_agent("testagent", "pass", [english], agent, "Default")]),
			Getsalt = fun() ->
				{ok, {_Statusline2, _Head2, Body2}} = http:request(get, {"http://127.0.0.1:5050/getsalt", [{"Cookie", Cookie}]}, [], []),
				{struct, Jsonlist} = mochijson2:decode(Body2),
				binary_to_list(proplists:get_value(<<"salt">>, Jsonlist))
			end,
			{Httpc, Cookie, Getsalt}
		end,
		fun({Httpc, _Cookie, _Getsalt}) ->
			inets:stop(httpc, Httpc),
			inets:stop(),
			agent_web_listener:stop(),
			agent_manager:stop(),
			agent_auth:destroy("testagent"),
			agent_auth:stop(),
			mnesia:stop(),
			mnesia:delete_schema([node()])
		end,
		[
			fun({_Httpc, Cookie, _Salt}) ->
				{"Trying to login before salt request",
				fun() ->
					Unsalted = util:bin_to_hexstr(erlang:md5("pass")),
					Salted = util:bin_to_hexstr(erlang:md5(string:concat("12345", Unsalted))),
					{ok, {_Statusline, _Head, Body}} = http:request(post, {"http://127.0.0.1:5050/login", [{"Cookie", Cookie}], "application/x-www-form-urlencoded", lists:append(["username=testagent&password=", Salted])}, [], []),
					{struct, Json} = mochijson2:decode(Body),
					?assertEqual(false, proplists:get_value(<<"success">>, Json)),
					?assertEqual(<<"No salt set">>, proplists:get_value(<<"message">>, Json))
				end}
			end,
			fun({_Httpc, Cookie, Salt}) ->
				{"Login with a bad pw",
				fun() ->
					Unsalted = util:bin_to_hexstr(erlang:md5("badpass")),
					Salted = util:bin_to_hexstr(erlang:md5(string:concat(Salt(), Unsalted))),
					{ok, {_Statusline, _Head, Body}} = http:request(post, {"http://127.0.0.1:5050/login", [{"Cookie", Cookie}], "application/x-www-form-urlencoded", lists:append(["username=testagent&password=", Salted])}, [], []),
					{struct, Json} = mochijson2:decode(Body),
					?assertEqual(false, proplists:get_value(<<"success">>, Json)),
					?assertEqual(<<"login err">>, proplists:get_value(<<"message">>, Json))
				end}
			end,
			fun({_Httpc, Cookie, Salt}) ->
				{"Login with bad un",
				fun() ->
					Unsalted = util:bin_to_hexstr(erlang:md5("pass")),
					Salted = util:bin_to_hexstr(erlang:md5(string:concat(Salt(), Unsalted))),
					{ok, {_Statusline, _Head, Body}} = http:request(post, {"http://127.0.0.1:5050/login", [{"Cookie", Cookie}], "application/x-www-form-urlencoded", lists:append(["username=badun&password=", Salted])}, [], []),
					{struct, Json} = mochijson2:decode(Body),
					?assertEqual(false, proplists:get_value(<<"success">>, Json)),
					?assertEqual(<<"login err">>, proplists:get_value(<<"message">>, Json))
				end}
			end
		]
	}.


% TODO add tests for interaction w/ agent, agent_manager

-define(PATH_TEST_SET, [
		{"/", {file, {"index.html", "www/agent/"}}},
		{"/poll", {api, poll}},
		{"/logout", {api, logout}},
		{"/login", {api, login}},
		{"/getsalt", {api, getsalt}},
		{"/state/teststate", {api, {set_state, "teststate"}}},
		{"/state/teststate/statedata", {api, {set_state, "teststate", "statedata"}}},
		{"/ack/7", {api, {ack, "7"}}},
		{"/err/89", {api, {err, "89"}}},
		{"/err/74/testmessage", {api, {err, "74", "testmessage"}}},
		{"/index.html", {file, {"index.html", "www/agent/"}}},
		{"/otherfile.ext", {file, {"otherfile.ext", "www/contrib/"}}},
		{"/other/path", {file, {"other/path", "www/contrib/"}}},
		{"/releaseopts", {api, releaseopts}},
		{"/checkcookie", {api, checkcookie}}
	]
).

path_parse_test_() ->
	{generator,
	fun() ->
		Test = fun({Path, Expected}) ->
			Name = string:concat("Testing path ", Path),
			{Name, fun() -> ?assertEqual(Expected, parse_path(Path)) end}
		end,
		lists:map(Test, ?PATH_TEST_SET)
	end}.

cookie_check_test_() ->
	[
		{"A blanke cookie",
		fun() ->
			?assertEqual(badcookie, check_cookie([]))
		end},
		{"An invalid cookie",
		fun() ->
			?assertEqual(badcookie, check_cookie([{"cookiekey", "cookievalue"}]))
		end},
		{"A well formed cookie, but not in ets",
		fun() ->
			ets:new(web_connections, [set, public, named_table]),
			Reflist = erlang:ref_to_list(make_ref()),
			?assertEqual(badcookie, check_cookie([{"cpx_id", Reflist}])),
			ets:delete(web_connections)
		end},
		{"A well formed cookie in the ets",
		fun() ->
			ets:new(web_connections, [set, public, named_table]),
			Reflist = erlang:ref_to_list(make_ref()),
			ets:insert(web_connections, {Reflist, undefined, undefined}),
			?assertEqual({Reflist, undefined, undefined}, check_cookie([{"cpx_id", Reflist}])),
			ets:delete(web_connections)
		end}
	].
	
	

-define(MYSERVERFUNC, fun() -> {ok, _Pid} = start_link(), {?MODULE, fun() -> stop() end} end).

-include("gen_server_test.hrl").


-endif.