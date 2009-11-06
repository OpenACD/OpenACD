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

-include_lib("public_key/include/public_key.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("call.hrl").
-include("queue.hrl").
-include("agent.hrl").

-define(PORT, 5050).
-define(WEB_DEFAULTS, [{name, ?MODULE}, {port, ?PORT}]).
-define(MOCHI_NAME, aweb_mochi).

%% API
-export([start_link/1, start/1, start/0, start_link/0, stop/0, linkto/1, linkto/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-ifndef(NOWEB).
-include("web.hrl").
-webconf([
	{label, "AGENTWEBCONNECTION"},
	{file, "agent_web_listener.html"},
	{callback, web_api}
]).
-endif.

-type(salt() :: string() | 'undefined').
-type(connection_handler() :: pid() | 'undefined').
-type(web_connection() :: {string(), salt(), connection_handler()}).

-ifdef(R13B).
-type(ref() :: reference()).
-endif.

-record(state, {
	connections:: any(), % ets table of the connections
	mochipid :: pid() % pid of the mochiweb process.
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================

%% @doc Starts the web listener on the default port of 5050.
-spec(start/0 :: () -> {'ok', pid()}).
start() -> 
	start(?PORT).

%% @doc Starts the web listener on the passed port.
-spec(start/1 :: (Port :: non_neg_integer()) -> {'ok', pid()}).
start(Port) -> 
	gen_server:start({local, ?MODULE}, ?MODULE, [Port], []).

%% @doc Start linked on the default port of 5050.
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() ->
	start_link(?PORT).

%% @doc Start linked on the given port.
-spec(start_link/1 :: (Port :: non_neg_integer()) -> {'ok', pid()}).
start_link(Port) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

%% @doc Stop the web listener.
-spec(stop/0 :: () -> 'ok').
stop() ->
	gen_server:call(?MODULE, stop).

%% @doc Link to the passed pid; usually an agent pid.
-spec(linkto/1 :: (Pid :: pid()) -> 'ok').
linkto(Pid) ->
	gen_server:cast(?MODULE, {linkto, Pid}).

%% @doc Register an already running web_connection.
-spec(linkto/3 :: (Ref :: ref(), Salt :: any(), Pid :: pid()) -> 'ok').
linkto(Ref, Salt, Pid) ->
	gen_server:cast(?MODULE, {linkto, Ref, Salt, Pid}).

-ifndef(NOWEB).
web_api(_Message, _Post) ->
	{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"nyi">>}]})}.
-endif.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Port]) ->
	?DEBUG("Starting on port ~p", [Port]),
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
handle_call(Request, From, State) ->
	?DEBUG("Call from ~p:  ~p", [From, Request]),
    {reply, {unknown_call, Request}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_cast({linkto, Pid}, State) ->
	?DEBUG("Linking to ~w", [Pid]),
	link(Pid),
	{noreply, State};
handle_cast({linkto, Reflist, Salt, Pid}, State) ->
	?DEBUG("Linking to ~w with ref ~w and salt ~p", [Pid, Reflist, Salt]),
	link(Pid),
	ets:insert(web_connections, {Reflist, Salt, Pid}),
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) ->
	?DEBUG("Doing a match_delete for pid ~w which died due to ~p", [Pid, Reason]),
	ets:match_delete(web_connections, {'$1', '_', Pid}),
	{noreply, State};
handle_info(Info, State) ->
	?DEBUG("Info:  ~p", [Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(shutdown, _State) ->
	?NOTICE("shutdown", []),
	mochiweb_http:stop(?MOCHI_NAME),
	ets:delete(web_connections),
	ok;
terminate(normal, _State) ->
	?NOTICE("normal exit", []),
	mochiweb_http:stop(?MOCHI_NAME),
	ets:delete(web_connections),
	ok;
terminate(Reason, _State) ->
	?NOTICE("Terminating dirty:  ~p", [Reason]),
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
	Post = case Req:get_primary_header_value("content-type") of
		"application/x-www-form-urlencoded" ++ _ ->
			Req:parse_post();
		_ ->
			%% TODO Change this to a custom parser rather than mochi's default.
			try mochiweb_multipart:parse_form(Req, fun file_handler/2) of
				Whoa ->
					Whoa
			catch
				What:Why ->
					?DEBUG("Going with a blank post due to mulipart parse fail:  ~p:~p", [What, Why]),
					[]
			end
	end,
	%?DEBUG("parsed posts:  ~p", [Post]),
	case parse_path(Path) of
		{file, {File, Docroot}} ->
			Cookielist = Req:parse_cookie(),
			%?DEBUG("Cookielist:  ~p", [Cookielist]),
			case proplists:get_value("cpx_id", Cookielist) of
				undefined ->
					Reflist = erlang:ref_to_list(make_ref()),
					Cookie = make_cookie(Reflist),
					ets:insert(Table, {Reflist, undefined, undefined}),
					Language = io_lib:format("cpx_lang=~s; path=/", [determine_language(Req:get_header_value("Accept-Language"))]),
					?DEBUG("Setting cookie and serving file ~p", [string:concat(Docroot, File)]),
					Req:serve_file(File, Docroot, [{"Set-Cookie", Cookie}, {"Set-Cookie", Language}]);
				_Reflist ->
					Language = io_lib:format("cpx_lang=~s; path=/", [determine_language(Req:get_header_value("Accept-Language"))]),
					Req:serve_file(File, Docroot, [{"Set-Cookie", Language}])
			end;
		{api, Api} ->
			Out = api(Api, check_cookie(Req:parse_cookie()), Post),
			Req:respond(Out)
	end.

file_handler(Name, ContentType) ->
	fun(N) -> file_data_handler(N, {Name, ContentType, <<>>}) end.

file_data_handler(eof, {Name, ContentType, Acc}) ->
	?DEBUG("eof gotten", []),
	{Name, Acc};
file_data_handler(Data, {Name, ContentType, Acc}) ->
	Newacc = <<Acc/binary, Data/binary>>,
	fun(N) -> file_data_handler(N, {Name, ContentType, Newacc}) end.

determine_language(undefined) ->
	"";
determine_language([]) ->
	"";
determine_language(String) ->
	[Head | Other] = util:string_split(String, ",", 2),
	[Lang |_Junk] = util:string_split(Head, ";"),
	case filelib:is_regular(string:concat(string:concat("www/agent/application/nls/", Lang), "/labels.js")) of
		true ->
			Lang;
		false ->
			% try the "super language" (eg en vs en-us) in case it's not in the list itself
			[SuperLang | _SubLang] = util:string_split(Lang, "-"),
			case filelib:is_regular(string:concat(string:concat("www/agent/application/nls/", SuperLang), "/labels.js")) of
				true ->
					SuperLang;
				false ->
					determine_language(Other)
			end
	end.

make_cookie(Value) ->
	io_lib:format("cpx_id=~p; path=/", [Value]).

api(checkcookie, Cookie, _Post) ->
	case Cookie of
		{_Reflist, _Salt, Conn} when is_pid(Conn) ->
			?DEBUG("Found agent_connection pid ~p", [Conn]),
			Agentrec = agent_web_connection:dump_agent(Conn),
			Json = {struct, [
				{<<"success">>, true},
				{<<"login">>, list_to_binary(Agentrec#agent.login)},
				{<<"profile">>, list_to_binary(Agentrec#agent.profile)},
				{<<"state">>, Agentrec#agent.state},
				{<<"statedata">>, agent_web_connection:encode_statedata(Agentrec#agent.statedata)},
				{<<"statetime">>, agent_web_connection:encode_statetime(Agentrec#agent.lastchangetimestamp)},
				{<<"timestamp">>, util:now()}]},
			{200, [], mochijson2:encode(Json)};
		badcookie ->
			?INFO("cookie not in ets", []),
			Reflist = erlang:ref_to_list(make_ref()),
			NewCookie = make_cookie(Reflist),
			ets:insert(web_connections, {Reflist, undefined, undefined}),
			Json = {struct, [{<<"success">>, false}, {<<"message">>, <<"cookie reset">>}]},
			{200, [{"Set-Cookie", NewCookie}], mochijson2:encode(Json)};
		{_Reflist, _Salt, undefined} ->
			?INFO("cookie found, no agent", []),
			Json = {struct, [{<<"success">>, false}, {<<"message">>, <<"have cookie, but no agent">>}]},
			{200, [], mochijson2:encode(Json)}
	end;
api(Apirequest, badcookie, _Post) ->
	?INFO("bad cookie for request ~p", [Apirequest]),
	Reflist = erlang:ref_to_list(make_ref()),
	Cookie = make_cookie(Reflist),
	ets:insert(web_connections, {Reflist, undefined, undefined}),
	{403, [{"Set-Cookie", Cookie}], <<"Cookie reset, retry.">>};
	
api(logout, {Reflist, _Salt, Conn}, _Post) ->
	Newref = erlang:ref_to_list(make_ref()),
	ets:insert(web_connections, {Reflist, undefined, undefined}),
	Cookie = io_lib:format("cpx_id=~p; path=/; Expires=Tue, 29-Mar-2005 19:30: 42 GMT; Max-Age=86400", [Reflist]),
	agent_web_connection:api(Conn, logout),
	{200, [{"Set-Cookie", Cookie}], mochijson2:encode({struct, [{success, true}]})};
api(login, {_Reflist, undefined, _Conn}, _Post) ->
	{200, [], mochijson2:encode({struct, [{success, false}, {message, <<"No salt set">>}]})};
api(login, {Reflist, Salt, _Conn}, Post) ->
	Username = proplists:get_value("username", Post, ""),
	Password = proplists:get_value("password", Post, ""),
	Endpointdata = case proplists:get_value("voipendpointdata", Post) of
		undefined ->
			error;
		[] ->
			error;
		Other ->
			Other
	end,
	Endpoint = case proplists:get_value("voipendpoint", Post) of
		"SIP Registration" ->
			case Endpointdata of
				error ->
					{sip_registration, Username};
				_Other ->
					{sip_registration, Endpointdata}
			end;
		"SIP URI" ->
			{sip, Endpointdata};
		"IAX2 URI" ->
			{iax2, Endpointdata};
		"H323 URI" ->
			{h323, Endpointdata};
		"PSTN Number" ->
			{pstn, Endpointdata}
	end,
	case Endpoint of
		{_, error} ->
			?WARNING("~s specified an invalid endpoint ~p when trying to log in", [Username, Endpoint]),
			{200, [], mochijson2:encode({struct, [{success, false}, {message, <<"Invalid endpoint">>}]})};
		_ ->
			try decrypt_password(Password) of
				Decrypted ->
					Salt = string:substr(Decrypted, 1, length(Salt)),
					DecryptedPassword = string:substr(Decrypted, length(Salt) + 1),
					Salted = util:bin_to_hexstr(erlang:md5(string:concat(Salt, util:bin_to_hexstr(erlang:md5(DecryptedPassword))))),
					case agent_auth:auth(Username, DecryptedPassword) of
						deny ->
							{200, [], mochijson2:encode({struct, [{success, false}, {message, <<"Authentication failed">>}]})};
						{allow, Id, Skills, Security, Profile} ->
							Agent = #agent{id = Id, login = Username, skills = Skills, profile=Profile, password=DecryptedPassword},
							case agent_web_connection:start(Agent, Security) of
								{ok, Pid} ->
									?WARNING("~s logged in with endpoint ~p", [Username, Endpoint]),
									gen_server:call(Pid, {set_endpoint, Endpoint}),
									linkto(Pid),
									#agent{lastchangetimestamp = StateTime} = agent_web_connection:dump_agent(Pid),
									ets:insert(web_connections, {Reflist, Salt, Pid}),
									?DEBUG("connection started for ~p", [Reflist]),
									{200, [], mochijson2:encode({struct, [
										{success, true}, 
										{message, <<"logged in">>}, 
										{<<"profile">>, list_to_binary(Profile)}, 
										{<<"statetime">>, agent_web_connection:encode_statetime(StateTime)},
										{<<"timestamp">>, util:now()}]})};
								ignore ->
									?WARNING("Ignore message trying to start connection for ~p", [Reflist]),
									{200, [], mochijson2:encode({struct, [{success, false}, {message, <<"login err">>}]})};
								{error, Error} ->
									?ERROR("Error ~p trying to start connection for ~p", [Error, Reflist]),
									{200, [], mochijson2:encode({struct, [{success, false}, {message, list_to_binary(Error)}]})}
							end
					end
			catch
				error:decrypt_failed ->
					{200, [], mochijson2:encode({struct, [{success, false}, {message, list_to_binary("Password decryption failed")}]})}
			end
	end;
api(getsalt, {Reflist, _Salt, Conn}, _Post) ->
	Newsalt = integer_to_list(crypto:rand_uniform(0, 4294967295)),
	ets:insert(web_connections, {Reflist, Newsalt, Conn}),
	agent_web_connection:set_salt(Conn, Newsalt),
	?DEBUG("created and sent salt for ~p", [Reflist]),
	[E, N] = get_pubkey(),
	PubKey = {struct, [{<<"E">>, list_to_binary(erlang:integer_to_list(E, 16))}, {<<"N">>, list_to_binary(erlang:integer_to_list(N, 16))}]},
	{200, [], mochijson2:encode({struct, [{success, true}, {message, <<"Salt created, check salt property">>}, {salt, list_to_binary(Newsalt)}, {pubkey, PubKey}]})};

api(_Api, {_Reflist, _Salt, undefined}, _Post) ->
	{403, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"no connection">>}]})};
	
api(brandlist, {_Reflist, _Salt, _Conn}, _Post) ->
	case call_queue_config:get_clients() of
	[] ->
		{200, [], mochijson2:encode({struct, [{success, false}, {message, <<"No brands defined">>}]})};
	Brands ->
		Converter = fun
			(#client{label = undefined}, Acc) ->
				Acc;
			(#client{label = Label, id = ID}, Acc) ->
				[{struct, [{<<"label">>, list_to_binary(Label)}, {<<"id">>, list_to_binary(ID)}]} | Acc]
		end,
		Jsons = lists:foldl(Converter, [], Brands),
		{200, [], mochijson2:encode({struct, [{success, true}, {<<"brands">>, Jsons}]})}
	end;
api(queuelist, {_Reflist, _Salt, _Conn}, _Post) ->
	case call_queue_config:get_queues() of
	[] ->
		{200, [], mochijson2:encode({struct, [{success, false}, {message, <<"No queues defined">>}]})};
	Brands ->
		Converter = fun
			(#call_queue{name = Name}, Acc) ->
				[{struct, [{<<"name">>, list_to_binary(Name)}]} | Acc]
		end,
		Jsons = lists:foldl(Converter, [], Brands),
		{200, [], mochijson2:encode({struct, [{success, true}, {<<"queues">>, Jsons}]})}
	end;

api(releaseopts, {_Reflist, _Salt, _Conn}, _Post) ->
	Releaseopts = agent_auth:get_releases(),
	Converter = fun(#release_opt{label = Label, id = Id, bias = Bias}) ->
		{struct, [{<<"label">>, list_to_binary(Label)}, {<<"id">>, Id}, {<<"bias">>, Bias}]}
	end,
	Jsons = lists:map(Converter, Releaseopts),
	{200, [], mochijson2:encode({struct, [{success, true}, {<<"options">>, Jsons}]})};	
	
api(poll, {_Reflist, _Salt, Conn}, []) when is_pid(Conn) ->
	agent_web_connection:poll(Conn, self()),
	receive
		{poll, Return} ->
			%?DEBUG("Got poll message, spitting back ~p", [Return]),
			 Return; 
		{kill, Headers, Body} -> 
			?DEBUG("Got a kill message with heads ~p and body ~p", [Headers, Body]),
			{408, Headers, Body}
	end;
api({undefined, Path}, Cookie, Post)  ->
	api({undefined, Path, Post}, Cookie, Post);
api(Api, {_Reflist, _Salt, Conn}, []) when is_pid(Conn) ->
	case agent_web_connection:api(Conn, Api) of
		{Code, Headers, Body} ->
			{Code, Headers, Body}
	end;
api(Api, {_Reflist, _Salt, Conn}, Post) when is_pid(Conn) ->
	case agent_web_connection:api(Conn, {Api, Post}) of
		{Code, Headers, Body} ->
			{Code, Headers, Body}
	end;
api(Api, Whatever, _Post) ->
	?DEBUG("Login required for api ~p with ref/salt/conn ~p", [Api, Whatever]),
	{200, [], mochijson2:encode({struct, [{success, false}, {message, <<"Login required">>}]})}.

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
	?DEBUG("Path:  ~s", [Path]),
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
		"/brandlist" ->
			{api, brandlist};
		"/queuelist" ->
			{api, queuelist};
		"/checkcookie" ->
			{api, checkcookie};
		_Other ->
			["" | Tail] = util:string_split(Path, "/"),
			case Tail of 
				["dynamic" | Moretail] ->
					File = string:join(Moretail, "/"),
					case filelib:is_regular(string:concat("www/dynamic/", File)) of
						true ->
							{file, {File, "www/dynamic"}};
						false ->
							{api, {undefined, Path}}
					end;
				["state", Statename] ->
					{api, {set_state, Statename}};
				["state", Statename, Statedata] ->
					{api, {set_state, Statename, Statedata}};
				["ack", Counter] ->
					{api, {ack, Counter}};
				["err", Counter] ->
					{api, {err, Counter}};
				["err", Counter, Message] ->
					{api, {err, Counter, Message}};
				["dial", Number] ->
					{api, {dial, Number}};
				["get_avail_agents"] ->
					{api, get_avail_agents};
				["agent_transfer", Agent] ->
					{api, {agent_transfer, Agent}};
				["media"] ->
					{api, media};
				["mediapull" | Pulltail] ->
					?DEBUG("pulltail:  ~p", [Pulltail]),
					{api, {mediapull, Pulltail}};
				["mediapush"] ->
					{api, mediapush};
				["warm_transfer", Number] ->
					{api, {warm_transfer, Number}};
				["queue_transfer", Number] ->
					{api, {queue_transfer, Number}};
				["init_outbound", Client, Type] ->
					{api, {init_outbound, Client, Type}};
				["supervisor" | Supertail] ->
					{api, {supervisor, Supertail}};
				_Allother ->
					% is there an actual file to serve?
					case {filelib:is_regular(string:concat("www/agent", Path)), filelib:is_regular(string:concat("www/contrib", Path))} of
						{true, false} ->
							{file, {string:strip(Path, left, $/), "www/agent/"}};
						{false, true} ->
							{file, {string:strip(Path, left, $/), "www/contrib/"}};
						{false, false} ->
							{api, {undefined, Path}}
					end
			end
	end.

get_pubkey() ->
	{ok,[Entry]} = public_key:pem_to_der("./key"),
	{ok,{'RSAPrivateKey', 'two-prime', N , E, _D, _P, _Q, _E1, _E2, _C, _Other}} =  public_key:decode_private_key(Entry),
	[E, N].

decrypt_password(Password) ->
	{ok,[Entry]} = public_key:pem_to_der("./key"),
	{ok,{'RSAPrivateKey', 'two-prime', N , E, D, _P, _Q, _E1, _E2, _C, _Other}} =  public_key:decode_private_key(Entry),
	PrivKey = [crypto:mpint(E), crypto:mpint(N), crypto:mpint(D)],
	Bar = crypto:rsa_private_decrypt(util:hexstr_to_bin(Password), PrivKey, rsa_pkcs1_padding),
	binary_to_list(Bar).

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
					?CONSOLE("Das head:  ~p", [Head]),
					Cookies = proplists:get_all_values("set-cookie", Head),
					Test = fun(C) ->
						case util:string_split(C, "=", 2) of
							["cpx_id", _Whatever] ->
								true;
							_Else ->
								false
						end
					end,
					?CONSOLE("Hmmm, cookie:  ~p", [Cookies]),
					?assert(lists:any(Test, Cookies))
				end}
			end,
			fun(_Httpc) ->
				{"Try to get a page with a bad cookie",
				fun() ->
					{ok, {{_Httpver, Code, _Message}, Head, _Body}} = http:request(get, {"http://127.0.0.1:5050/", [{"Cookie", "goober=snot"}]}, [], []),
					?assertEqual(200, Code),
					?CONSOLE("~p", [Head]),
					Cookies = proplists:get_all_values("set-cookie", Head),
					Test = fun(C) ->
						case util:string_split(C, "=", 2) of
							["cpx_id", _Whatever] ->
								true;
							_Else ->
								false
						end
					end,
					?assertEqual(true, lists:any(Test, Cookies))
				end}
			end,
			fun(_Httpc) ->
				{"Get a cookie, then a request with that cookie",
				fun() ->
					{ok, {_Statusline, Head, _Body}} = http:request("http://127.0.0.1:5050/"),
					Cookie = proplists:get_all_values("set-cookie", Head),
					Cookielist = lists:map(fun(I) -> {"Cookie", I} end, Cookie),
					{ok, {{_Httpver, Code, _Message}, Head2, _Body2}} = http:request(get, {"http://127.0.0.1:5050", Cookielist}, [], []),
					Cookie2 = proplists:get_all_values("set-cookie", Head2),
					Test = fun(C) ->
						case util:string_split(C, "=", 2) of
							["cpx_id", _Whatever] ->
								true;
							_Else ->
								false
						end
					end,
					?assertEqual(false, lists:any(Test, Cookie2)),
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
			Cookie = proplists:get_all_values("set-cookie", Head),
			?CONSOLE("cookie_api_test_ setup ~p", [Cookie]),
			Cookieproplist = lists:map(fun(I) -> {"Cookie", I} end, Cookie),
			?CONSOLE("cookie proplist ~p", [Cookieproplist]),
			{Httpc, Cookieproplist}
		end,
		fun({Httpc, _Cookie}) ->
			inets:stop(httpc, Httpc),
			inets:stop(),
			agent_web_listener:stop()
		end,
		[
			fun({_Httpc, Cookielist}) ->
				{"Get a salt with a valid cookie",
				fun() ->
					{ok, {{_Ver, Code, _Msg}, _Head, Body}} = http:request(get, {"http://127.0.0.1:5050/getsalt", Cookielist}, [], []),
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
			?CONSOLE("request head ~p", [Head]),
			Cookies = proplists:get_all_values("set-cookie", Head),
			Cookielist = lists:map(fun(I) -> {"Cookie", I} end, Cookies), 
			?CONSOLE("~p", [agent_auth:add_agent("testagent", "pass", [english], agent, "Default")]),
			Getsalt = fun() ->
				{ok, {_Statusline2, _Head2, Body2}} = http:request(get, {"http://127.0.0.1:5050/getsalt", Cookielist}, [], []),
				?CONSOLE("Body2:  ~p", [Body2]),
				{struct, Jsonlist} = mochijson2:decode(Body2),
				binary_to_list(proplists:get_value(<<"salt">>, Jsonlist))
			end,
			
			{Httpc, Cookielist, Getsalt}
		end,
		fun({Httpc, _Cookie, _Getsalt}) ->
			inets:stop(httpc, Httpc),
			inets:stop(),
			agent_web_listener:stop(),
			agent_manager:stop(),
			agent_auth:destroy("testagent"),
			mnesia:stop(),
			mnesia:delete_schema([node()])
		end,
		[
			fun({_Httpc, Cookie, _Salt}) ->
				{"Trying to login before salt request",
				fun() ->
					Unsalted = util:bin_to_hexstr(erlang:md5("pass")),
					Salted = util:bin_to_hexstr(erlang:md5(string:concat("12345", Unsalted))),
					{ok, {_Statusline, _Head, Body}} = http:request(post, {"http://127.0.0.1:5050/login", Cookie, "application/x-www-form-urlencoded", lists:append(["username=testagent&password=", Salted])}, [], []),
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
					{ok, {_Statusline, _Head, Body}} = http:request(post, {"http://127.0.0.1:5050/login", Cookie, "application/x-www-form-urlencoded", lists:append(["username=testagent&password=", Salted, "&voipendpoint=SIP Registration"])}, [], []),
					{struct, Json} = mochijson2:decode(Body),
					?assertEqual(false, proplists:get_value(<<"success">>, Json))
					%?assertEqual(<<"login err">>, proplists:get_value(<<"message">>, Json))
				end}
			end,
			fun({_Httpc, Cookie, Salt}) ->
				{"Login with bad un",
				fun() ->
					Unsalted = util:bin_to_hexstr(erlang:md5("pass")),
					Salted = util:bin_to_hexstr(erlang:md5(string:concat(Salt(), Unsalted))),
					{ok, {_Statusline, _Head, Body}} = http:request(post, {"http://127.0.0.1:5050/login", Cookie, "application/x-www-form-urlencoded", lists:append(["username=badun&password=", Salted, "&voipendpoint=SIP Registration"])}, [], []),
					?CONSOLE("BODY:  ~p", [Body]),
					{struct, Json} = mochijson2:decode(Body),
					?assertEqual(false, proplists:get_value(<<"success">>, Json))
					%?assertEqual(<<"login err">>, proplists:get_value(<<"message">>, Json))
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
		{"/otherfile.ext", {api, {undefined, "/otherfile.ext"}}},
		{"/other/path", {api, {undefined, "/other/path"}}},
		{"/releaseopts", {api, releaseopts}},
		{"/brandlist", {api, brandlist}},
		{"/queuelist", {api, queuelist}},
		{"/checkcookie", {api, checkcookie}},
		{"/dial/12345", {api, {dial, "12345"}}},
		{"/get_avail_agents", {api, get_avail_agents}},
		{"/agent_transfer/agent", {api, {agent_transfer, "agent"}}},
		{"/mediapush", {api, mediapush}},
		{"/dynamic/test.html", {file, {"test.html", "www/dynamic"}}}
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
	
	

-define(MYSERVERFUNC, fun() -> {ok, Pid} = start_link(), unlink(Pid), {?MODULE, fun() -> stop() end} end).

-include("gen_server_test.hrl").


-endif.
