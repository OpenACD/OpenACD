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

%% @doc Listens for new web connections, then spawns an 
%% {@link agent_web_connection} to handle the details.  Uses Mochiweb for 
%% the heavy lifting.
%% 
%% == Agent Api ==
%%
%% The listener and connection are designed to be able to function with
%% any ui that adheres to the api.  The api is broken up between the two
%% modules.  {@module} holds the functions that either doe not require a
%% speecific agent, or handle the login procedures.  For 
%% functions dealing with a specific agent, {@link agent_web_connection}.
%% 
%% This module uses {@link cpx_agent_connection} for many of it's
%% functions.  Login is handled in {@module} and
%% {@link agent_web_connection}.  The functions for that system are
%% tagged with {@agent_api}.
%%
%% Requests from a UI are made over HTTP using POST formatted in
%% application/x-www-form-urlencoded (the default on most modern 
%% browsers) to /api.  The json request object is put into a field labeled
%% 'request'.
%%
%% == Hooks ==
%%
%% Hooks are available when a request is made that an agent connection
%% cannot handle.
%%
%% @see agent_web_connection
%% @see cpx_web_management

-module(agent_web_listener).
-author("Micah").

-behaviour(gen_server).

-include_lib("public_key/include/public_key.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("call.hrl").
-include("queue.hrl").
-include("agent.hrl").
-include("web.hrl").

-ifdef(TEST).
-define(PORT, 55050).
-else.
-define(PORT, 5050).
-endif.
-define(WEB_DEFAULTS, [{name, ?MODULE}, {port, ?PORT}]).
-define(MOCHI_HTTP_NAME, aweb_http_mochi).
-define(MOCHI_HTTPS_NAME, aweb_https_mochi).

%% API
-export([start_link/1, start/1, start/0, start_link/0, stop/0, linkto/1, linkto/3]).
%% Web api
-export([
	check_cookie/1,
	get_salt/1,
	login/4,
	get_brand_list/0,
	get_queue_list/0,
	get_release_opts/0
]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-type(salt() :: string() | 'undefined').
-type(connection_handler() :: pid() | 'undefined').
-type(web_connection() :: {string(), salt(), connection_handler()}).

-record(state, {
	connections :: any(), % ets table of the connections
	mochipid_http :: pid() | 'undefined', % pid of the mochiweb process.
	mochipid_https :: pid() | 'undefined'
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

-type(web_reply() :: {non_neg_integer(), [any()], binary()}).
-type(cookie_res() :: 
	'badcookie' | 
	{'undefined', 'undefined', 'undefined'} |
	{string(), 'undefined', 'undefined'} |
	{string(), string(), 'undefined'} |
	{string(), string(), pid()}
).
%%====================================================================
%% API
%%====================================================================

%% @doc Starts the web listener on the default port of 5050.
-spec(start/0 :: () -> {'ok', pid()}).
start() -> 
	start(?PORT).

%% @doc Starts the web listener on the passed port.
-spec(start/1 :: (Port :: non_neg_integer()) -> {'ok', pid()}).
start(Port) when is_integer(Port) -> 
	gen_server:start({local, ?MODULE}, ?MODULE, [Port], []);
start(Options) ->
	gen_server:start({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Start linked on the default port of 5050.
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() ->
	start_link(?PORT).

%% @doc Start linked on the given port.
-spec(start_link/1 :: (Port :: non_neg_integer()) -> {'ok', pid()}).
start_link(Port) when is_integer(Port) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []);
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Stop the web listener.
-spec(stop/0 :: () -> 'ok').
stop() ->
	gen_server:cast(?MODULE, stop).

%% @doc Link to the passed pid; usually an agent pid.
-spec(linkto/1 :: (Pid :: pid()) -> 'ok').
linkto(Pid) ->
	gen_server:cast(?MODULE, {linkto, Pid}).

%% @doc Register an already running web_connection.
-spec(linkto/3 :: (Ref :: reference(), Salt :: any(), Pid :: pid()) -> 'ok').
linkto(Ref, Salt, Pid) ->
	gen_server:cast(?MODULE, {linkto, Ref, Salt, Pid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Port]) when is_integer(Port) ->
	?DEBUG("init init", []),
	init([{port, Port}]);
init(Options) ->
	?DEBUG("Init start ~p", [Options]),
	process_flag(trap_exit, true),
	crypto:start(),
	Table = ets:new(web_connections, [set, public, named_table]),
	Port = proplists:get_value(port, Options),
	Ssl = proplists:get_value(ssl, Options),
	DefaultSslPort = case Port of
		undefined -> ?PORT + 1;
		_ -> Port + 1
	end,
	SslPort = proplists:get_value(ssl_port, Options, DefaultSslPort),
	SslCertfile = util:get_certfile(),
	SslKeyfile = util:get_keyfile(),
	MochiNormalPid = case Port of
		undefined -> undefined;
		N when is_integer(N) ->
			?DEBUG("Starting plain on port ~p", [Port]),
			{ok, Mochi1} = mochiweb_http:start([
				{loop, fun(Req) -> loop(Req, Table) end},
				{name, ?MOCHI_HTTP_NAME},
				{port, Port}
			]),
			Mochi1
	end,
	MochiSslPid = case Ssl of
		undefined -> undefined;
		true ->
			?DEBUG("Starting ssl on port ~p", [SslPort]),
			{ok, Mochi2} = mochiweb_http:start([
				{loop, fun(Req) -> loop(Req, Table) end},
				{name, ?MOCHI_HTTPS_NAME},
				{ssl, true},
				{port, SslPort},
				{ssl_opts, [
					{certfile, SslCertfile},
					{keyfile, SslKeyfile}
				]}
			]),
			Mochi2
	end,
	case {MochiNormalPid, MochiSslPid} of
		{undefined, undefined} ->
			{stop, no_listeners};
		_ ->
			{ok, #state{
				connections=Table,
				mochipid_http = MochiNormalPid,
				mochipid_https = MochiSslPid
			}}
	end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------
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
handle_cast(stop, State) ->
	{stop, shutdown, State};
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
terminate(shutdown, State) ->
	?NOTICE("shutdown", []),
	exit_mochis(State),
	ets:delete(web_connections),
	ok;
terminate(normal, State) ->
	?NOTICE("normal exit", []),
	exit_mochis(State),
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

exit_mochis(State) ->
	case is_pid(State#state.mochipid_http) of
		true -> mochiweb_http:stop(?MOCHI_HTTP_NAME);
		_ -> ok
	end,
	case is_pid(State#state.mochipid_https) of
		true -> mochiweb_http:stop(?MOCHI_HTTPS_NAME);
		_ -> ok
	end.

%% @doc listens for a new connection.
%% Based on the path, the loop can take several paths.
%% if the path is "/login" and there is post data, an attempt is made to 
%% start a new {@link agent_web_connection}.
%% On a successful start, a cookie is set that the key reference used by 
%% this module to link new connections
%% to the just started agent_web_connection.
%% 
%% On any other path, the cookie is checked.  The value of the cookie is 
%% looked up on an internal table to see 
%% if there is an active agent_web_connection.  If there is, further 
%% processing is done there, otherwise the request is denied.
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
				_:_ ->
					%?DEBUG("Going with a blank post due to mulipart parse fail:  ~p:~p", [What, Why]),
					[]
			end
	end,
	%?DEBUG("parsed posts:  ~p", [Post]),
	case parse_path(Path) of
		{file, {File, Docroot}} ->
			Cookielist = Req:parse_cookie(),
			%?DEBUG("Cookielist:  ~p", [Cookielist]),
			GregSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
			FutureWeek = GregSecs + (60 * 60 * 24 * 7),
			DateTime = calendar:gregorian_seconds_to_datetime(FutureWeek),
			CacheControl = {"Expires", util:http_datetime(DateTime)},
			case proplists:get_value("cpx_id", Cookielist) of
				undefined ->
					Reflist = erlang:ref_to_list(make_ref()),
					Cookie = make_cookie(Reflist),
					ets:insert(Table, {Reflist, undefined, undefined}),
					Language = io_lib:format("cpx_lang=~s; path=/", [determine_language(Req:get_header_value("Accept-Language"))]),
					?DEBUG("Setting cookie and serving file ~p", [string:concat(Docroot, File)]),
					Req:serve_file(File, Docroot, [CacheControl, {"Set-Cookie", Cookie}, {"Set-Cookie", Language}]);
				_Reflist ->
					Language = io_lib:format("cpx_lang=~s; path=/", [determine_language(Req:get_header_value("Accept-Language"))]),
					Req:serve_file(File, Docroot, [CacheControl, {"Set-Cookie", Language}])
			end;
		{api, Api} ->
			Cookie = cookie_good(Req:parse_cookie()),
			keep_alive(Cookie),
			{Status, Headers, OutBin}  = Res = api(Api, Cookie, Post),
			case {Req:get_primary_header_value("If-None-Match"), proplists:get_value("ETag", Headers)}  of
				{X, X} when is_list(X) ->
					Req:respond({304, [{"ETag", X}], <<>>});
				_ ->
					case Status of
						200 ->
							ContentType = proplists:get_value("Content-Type", Headers, "text/plain"),
							Req:ok({ContentType, Headers, OutBin});
						_ ->
							Req:respond(Res)
					end
			end
	end.

file_handler(Name, ContentType) ->
	fun(N) -> file_data_handler(N, {Name, ContentType, <<>>}) end.

file_data_handler(eof, {Name, _ContentType, Acc}) ->
	?DEBUG("eof gotten", []),
	{Name, Acc};
file_data_handler(Data, {Name, ContentType, Acc}) ->
	Newacc = <<Acc/binary, Data/binary>>,
	fun(N) -> file_data_handler(N, {Name, ContentType, Newacc}) end.

determine_language(undefined) ->
	"en"; %% not requested, assume english
determine_language([]) ->
	"";
determine_language(String) ->
	[Head | Other] = util:string_split(String, ",", 2),
	[Lang | _Junk] = util:string_split(Head, ";"),
	case filelib:is_regular(filename:join([util:priv_dir("www/agent/application/nls/"), Lang, "labels.js"])) of
		true ->
			Lang;
		false ->
			% try the "super language" (eg en vs en-us) in case it's not in the list itself
			[SuperLang | _SubLang] = util:string_split(Lang, "-"),
			case filelib:is_regular(filename:join([util:priv_dir("www/agent/application/nls/"), SuperLang, "labels.js"])) of
				true ->
					SuperLang;
				false ->
					determine_language(Other)
			end
	end.

make_cookie(Value) ->
	io_lib:format("cpx_id=~p; path=/", [Value]).

keep_alive({_Reflist, _Salt, Conn}) when is_pid(Conn) ->
	agent_web_connection:keep_alive(Conn);
keep_alive(_) ->
	ok.

send_to_connection(_ApiArea, badcookie, _Function, _Args) ->
	?DEBUG("sent to connection with bad cookie", []),
	check_cookie(badcookie);
send_to_connection(_ApiArea, {_Ref, _Salt, undefined} = Cookie, _Function, _Args) ->
	?DEBUG("sent to connection with no connection pid", []),
	check_cookie(Cookie);
send_to_connection(api, {_Ref, _Salt, Conn}, <<"poll">>, _Args) ->
	agent_web_connection:poll(Conn, self()),
	receive
		{poll, Return} ->
			%?DEBUG("Got poll message, spitting back ~p", [Return]),
			 Return; 
		{kill, Headers, Body} -> 
			?DEBUG("Got a kill message with heads ~p and body ~p", [Headers, Body]),
			{408, Headers, Body}
	end;
send_to_connection(ApiArea, Cookie, Func, Args) when is_binary(Func) ->
	try list_to_existing_atom(binary_to_list(Func)) of
		Atom ->
			send_to_connection(ApiArea, Cookie, Atom, Args)
	catch
		error:badarg ->
			{_Ref, _Salt, Conn} = Cookie,
			Agent = agent_web_connection:dump_agent(Conn),
			case cpx_hooks:trigger_hooks(agent_web_call, [Agent, Conn, Func, Args]) of
				{error, unhandled} ->
					?reply_err(<<"no such function">>, <<"FUNCTION_NOEXISTS">>);
					{ok, ok} -> ?simple_success();
					{ok, {error, Code, Msg}} -> ?reply_err(Msg, Code);
					{ok, {ok, Json}} -> ?reply_success(Json)
				end
	end;
send_to_connection(ApiArea, Cookie, Func, Arg) when is_binary(Arg) ->
	send_to_connection(ApiArea, Cookie, Func, [Arg]);
send_to_connection(ApiArea, {Ref, _Salt, Conn}, Function, Args) when is_pid(Conn) ->
	case {is_process_alive(Conn), ApiArea} of
		{false, _} ->
			ets:delete(web_connections, Ref),
			api(checkcookie, badcookie, []);
		{true, api} ->
			?INFO("WebAPI: ~p ~p", [Function, Args]),
			case agent_web_connection:is_web_api(Function, length(Args) + 1) of
				false ->
					Agent = agent_web_connection:dump_agent(Conn),
					case cpx_hooks:trigger_hooks(agent_web_call, [Agent, Conn, Function, Args]) of
						{error, unhandled} ->
							?reply_err(<<"no such function">>, <<"FUNCTION_NOEXISTS">>);
						{ok, ok} -> ?simple_success();
						{ok, {error, Code, Msg}} -> ?reply_err(Msg, Code);
						{ok, {ok, Json}} -> ?reply_success(Json)
					end;
				true ->
					erlang:apply(agent_web_connection, Function, [Conn | Args])
			end;
		{true, supervisor} ->
			case supervisor_web_connection:is_web_api(Function, length(Args) + 1) of
				false ->
					?reply_err(<<"no such function">>, <<"FUNCTION_NOEXISTS">>);
				true ->
					erlang:apply(supervisor_web_connection, Function, [Conn | Args])
			end
	end.

%% @doc {@web} Determine if the cookie the client sent can be associated
%% with a logged in agent.  This should be the first step of a login 
%% process.  If it replies true, then the client can skip the 
%% {@link get_salt/1} and {@link login/4} steps to immediately start a
%% poll.  All other times, the client should set the given cookie with all
%% subsequent web calls.  The next call after this is usually
%% {@link get_salt/1}.
%%
%% There are no arguments as anything important happens in the http
%% headers.  If the cookie is invalid, the reply will have a set-cookie
%% directive in its headers.
%% 
%% The result json is:
%% <pre> {
%% 	"login":     string(),
%% 	"profile":   string(),
%%	"securityLevel":	 "agent" | "supervisor" | "admin",
%% 	"state":     string(),
%% 	"statedata": any(),
%% 	"statetime": timestamp(),
%% 	"timestamp": timestamp()
%%  "mediaload": any(); optional
%% }</pre>
-spec(check_cookie/1 :: (Cookie :: cookie_res()) -> web_reply()).
check_cookie({_Reflist, _Salt, Conn}) when is_pid(Conn) ->
	%?DEBUG("Found agent_connection pid ~p", [Conn]),
	Agentrec = agent_web_connection:dump_agent(Conn),
	%{_, PersistAtom, EpType} = Agentrec#agent.endpointtype,
%	Peristence = case PersistAtom of
%		persistent -> true;
%		_ -> false
%	end,
	Basejson = [
		{<<"login">>, list_to_binary(Agentrec#agent.login)},
		{<<"profile">>, list_to_binary(Agentrec#agent.profile)},
		{<<"securityLevel">>, Agentrec#agent.security_level},
		{<<"timestamp">>, util:now()}
	],
	% TODO when a new connection is established, the channels should do the
	% media load command.
	Fulljson = Basejson,
	Json = {struct, [
		{<<"success">>, true},
		{<<"result">>, {struct, Fulljson}} |
		Fulljson
	]},
	{200, [], mochijson2:encode(Json)};
check_cookie(badcookie) ->
	?INFO("cookie not in ets", []),
	Reflist = erlang:ref_to_list(make_ref()),
	NewCookie = make_cookie(Reflist),
	ets:insert(web_connections, {Reflist, undefined, undefined}),
	Json = {struct, [
		{<<"success">>, false}, 
		{<<"message">>, <<"Your cookie was expired, issueing you a new one">>}, 
		{<<"errcode">>, <<"BAD_COOKIE">>}
	]},
	{200, [{"Set-Cookie", NewCookie}], mochijson2:encode(Json)};
check_cookie({_Reflist, _Salt, undefined}) ->
	?INFO("cookie found, no agent", []),
	Json = {struct, [
		{<<"success">>, false}, 
		{<<"message">>, <<"have cookie, but no agent">>},
		{<<"errcode">>, <<"NO_AGENT">>}
	]},
	{200, [], mochijson2:encode(Json)}.

%% @doc {@web} Get the salt and public key information to encrypt the 
%% password.  Should be the second step in logging in.  Remember the client
%% must be able to send the same cookie it got in the check cookie step.
%% If the cookie does not pass inspection, a salt and public key info will
%% still be sent, but there will be a new cookie header sent as well.  This
%% means this function does not allow for state recovery like 
%% {@link check_cookie/1} does.
%%
%% After getting a successful response from this web api call, move on to
%% {@link login/4}.
%%
%% There are no arguments for this request.
%% 
%% A result is:
%% <pre> {
%% 	"salt":   string(),
%% 	"pubkey": {
%% 		"E":   string(),
%% 		"N":   string()
%% 	}
%% }</pre>
-spec(get_salt/1 :: (Cookie :: cookie_res()) -> web_reply()).
get_salt(badcookie) ->
	Conn = undefined,
	Reflist = erlang:ref_to_list(make_ref()),
	Cookie = make_cookie(Reflist),
	Newsalt = integer_to_list(crypto:rand_uniform(0, 4294967295)),
	ets:insert(web_connections, {Reflist, Newsalt, Conn}),
	?DEBUG("created and sent salt for ~p", [Reflist]),
	[E, N] = util:get_pubkey(),
	PubKey = {struct, [
		{<<"E">>, list_to_binary(erlang:integer_to_list(E, 16))}, 
		{<<"N">>, list_to_binary(erlang:integer_to_list(N, 16))}
	]},
	{200, [{"Set-Cookie", Cookie}], mochijson2:encode({struct, [
		{success, true}, 
		{message, <<"Salt created, check salt property">>}, 
		{salt, list_to_binary(Newsalt)}, 
		{pubkey, PubKey},
		{<<"result">>, {struct, [
			{salt, list_to_binary(Newsalt)},
			{pubkey, PubKey}
		]}}
	]})};
get_salt({Reflist, _Salt, Conn}) ->
	Newsalt = integer_to_list(crypto:rand_uniform(0, 4294967295)),
	ets:insert(web_connections, {Reflist, Newsalt, Conn}),
	agent_web_connection:set_salt(Conn, Newsalt),
	?DEBUG("created and sent salt for ~p", [Reflist]),
	[E, N] = util:get_pubkey(),
	PubKey = {struct, [
		{<<"E">>, list_to_binary(erlang:integer_to_list(E, 16))}, 
		{<<"N">>, list_to_binary(erlang:integer_to_list(N, 16))}
	]},
	{200, [], mochijson2:encode({struct, [
		{success, true}, 
		{message, <<"Salt created, check salt property">>}, 
		{salt, list_to_binary(Newsalt)}, 
		{pubkey, PubKey},
		{<<"result">>, {struct, [
			{salt, list_to_binary(Newsalt)},
			{pubkey, PubKey}
		]}}
	]})}.

%% @doc {@web} Login and start an {@link agent_web_connection}.  This is
%% the second to last step in logging in a web client (the final one 
%% starting a poll).  Using the salt and public key information recieved in
%% {@link get_salt/1}, encrypt the password.  Using the built-in gui as an
%% example, the password is encrypted by via the javascript library jsbn:
%% `
%% var getSaltPubKey = 	getSaltResult.pubkey;
%% var rsa = new RSAKey();
%% rsa.setPublic(getSaltPubKey.N, getSaltPubKey.E);
%% rsa.encrypt(getSaltResult.salt + password);
%% '
%% Order of the salt and password is important.
%%
%% If voipdata is not defined, then it is assumed the agent will register a
%% phone via sip using thier login name.
%% 
%% The web api for this actually only takes 3 arguments in the `"args"' 
%% property of the request:
%%
%% `[username, password, options]'
%%
%% `username' and `password' are both string().  `options' is a json
%% object:
%% <pre> {
%% 	"voipendpointdata":  string(),
%% 	"voipendpoint":  "sip_registration" | "sip" | "iax2" | "h323" | "pstn",
%% 	"useoutbandring":  boolean(); optional,
%%  "usepersistentring":  boolean(); optional
%%  "supervisor":boolean(); optional
%% }</pre>
%% 
%% If `"voipendpoint"' is defined but `"voipendpointdata"' is not,
%% `"username"' is used.
%%
%% If `"supervisor"' is `true', no agent is logged in and only supervisor
%% calls will be available ({@link supervisor_web_connection}).  A poll is 
%% still created.  If the credentials given do not point to an agent with 
%% `supervisor' or `admin' security level, an error is generated.  An 
%% agent with sufficient security clearance can request supervisor actions
%% and polling.
%%
%% Note an agent starts out in a relased state with reason of default.
%%  
%% A result is:
%% `{
%% 	"profile":   string(),
%%	"securityLevel":  "agent" | "supervisor" | "admin",
%% 	"statetime": timestamp(),
%% 	"timestamp": timestamp()
%% }'
-spec(login/4 :: (Cookie :: cookie_res(), Username :: string(), Password :: string(), Opts :: [any()]) -> web_reply()).
login(badcookie, _, _, _) ->
	?DEBUG("bad cookie", []),
	check_cookie(badcookie);
login({_Ref, undefined, _Conn}, _, _, _) ->
	?reply_err(<<"Your client is requesting a login without first requesting a salt.">>, <<"NO_SALT">>);
login({Ref, Salt, _Conn}, Username, Password, Opts) ->
	ProtoEndpointdata = proplists:get_value(voipendpointdata, Opts),
	Persistantness = case proplists:get_value(use_persistent_ring, Opts) of
		true -> persistent;
		_ -> transient
	end,
	?INFO("login opts:  ~p", [Opts]),
	{Endpoint, Endpointdata} = case {proplists:get_value(voipendpoint, Opts), ProtoEndpointdata} of
		{undefined, _} ->
			%{{persistent, sip_registration}, Username};
			{sip_registration, Username};
		{sip_registration, undefined} ->
			%{{persistent, sip_registration}, Username};
			{sip_registration, Username};
		{EndpointType, _} ->
			%{EndpointType, Endpointdata}
			{EndpointType, ProtoEndpointdata}
	end,
	Bandedness = case proplists:get_value(use_outband_ring, Opts) of
		true ->
			outband;
		_ ->
			inband
	end,
	case util:decrypt_password(Password) of
		{ok, Decrypted} ->
			try
				Salt = string:substr(Decrypted, 1, length(Salt)),
				string:substr(Decrypted, length(Salt) + 1)
			of
				DecryptedPassword ->
					AuthResponse = agent_auth:auth(Username, DecryptedPassword),
					SupervisorReq = proplists:get_value(supervisor, Opts),
					case {AuthResponse, SupervisorReq} of
						{deny, _} ->
							?reply_err(<<"Authentication failed">>, <<"AUTH_FAILED">>);
						{{allow, Id, Skills, agent, Profile}, true} ->
							?WARNING("Agent ~s tried to act as a supervisor.  Slap 'em.", [Id]),
							?reply_err(<<"Authentication failed">>, <<"AUTH_FAILED">>);
						{{allow, Id, Skills, _, Profile}, true} ->
							SupStartOpts = [
								{login, Username},
								{ring_path, Bandedness},
								{endpoint, Endpoint},
								{endpointdata, Endpointdata}
							],
							case supervisor_web_connection:start(SupStartOpts) of
								{ok, Pid} ->
									?INFO("~s (~s) supervising", [Username, Id]),
									linkto(Pid),
									ets:insert(web_connections, {Ref, Salt, Pid}),
									{200, [], mochijson2:encode({struct, [{success, true}]})};
								ignore ->
									?WARNING("Ignore message trying to start connection for ~p ~p", [Ref, Username]),
									?reply_err(<<"login error">>, <<"UNKNOWN_ERROR">>);
								{error, Error} ->
									?ERROR("Error ~p trying to start connection for ~p ~p", [Error, Ref, Username]),
									?reply_err(<<"login error">>, <<"UNKNOWN_ERROR">>)
							end;
						{{allow, Id, Skills, Security, Profile}, _} ->
							{atomic, [AgentAuth]} = agent_auth:get_agent(id, Id),
							Agent = #agent{
								id = Id, 
								login = Username, 
								skills = Skills, 
								profile=Profile, 
								security_level = Security
							},
							case agent_web_connection:start(Agent) of
								{ok, Pid} ->
									?INFO("~s logged in", [Username]),
									linkto(Pid),
									{true, Apid} = agent_manager:query_agent(Username),
									agent:set_endpoints(Apid, AgentAuth#agent_auth.endpoints),
									% TODO make real profile
%									#agent{profile = EffectiveProfile} = agent_web_connection:dump_agent(Pid),
									ets:insert(web_connections, {Ref, Salt, Pid}),
									?DEBUG("connection started for ~p ~p", [Ref, Username]),
									{200, [], mochijson2:encode({struct, [
										{success, true},
										{<<"result">>, {struct, [
											{<<"profile">>, list_to_binary(Profile)}, 
											%{<<"profile">>, list_to_binary(EffectiveProfile)},
											{<<"securityLevel">>, Security},
											{<<"timestamp">>, util:now()}]}}]})};
								ignore ->
									?WARNING("Ignore message trying to start connection for ~p ~p", [Ref, Username]),
									?reply_err(<<"login error">>, <<"UNKNOWN_ERROR">>);
								{error, Error} ->
									?ERROR("Error ~p trying to start connection for ~p ~p", [Error, Ref, Username]),
									?reply_err(<<"login error">>, <<"UNKNOWN_ERROR">>)
							end
					end
			catch
				error:{badmatch, _} ->
					?NOTICE("authentication failure for ~p using salt ~p (expected ~p)", [Username, string:substr(Decrypted, 1, length(Salt)), Salt]),
					?reply_err(<<"Invalid salt">>, <<"NO_SALT">>)
			end;
	{error, decrypt_failed} ->
		?reply_err(<<"Password decryption failed">>, <<"DECRYPT_FAILED">>)
	end.

%% @doc {@web} Returns a list of queues configured in the system.  Useful
%% if you want agents to be able to place media into a queue.
%% Result:
%% `[{
%% 	"name": string()
%% }]'
-spec(get_queue_list/0 :: () -> web_reply()).
get_queue_list() ->
	Queues = call_queue_config:get_queues(),
	QueuesEncoded = [{struct, [
		{<<"name">>, list_to_binary(Q#call_queue.name)}
	]} || Q <- Queues],
	?reply_success(QueuesEncoded).

%% @doc {@web} Returns a list of clients confured in the system.  Useful
%% to allow agents to make outbound media.
%% Result:
%% `[{
%% 	"label":  string(),
%% 	"id":     string()
%% }]'
-spec(get_brand_list/0 :: () -> web_reply()).
get_brand_list() ->
	Brands = call_queue_config:get_clients(),
	BrandsEncoded = [{struct, [
		{<<"label">>, list_to_binary(C#client.label)},
		{<<"id">>, list_to_binary(C#client.id)}
	]} || C <- Brands, C#client.label =/= undefined],
	?reply_success(BrandsEncoded).

%% @doc {@web} Returns a list of options for use when an agents wants to
%% go released.
%% Result:
%% `[{
%% 	"label":  string(),
%% 	"id":     string(),
%% 	"bias":   -1 | 0 | 1
%% }]'
-spec(get_release_opts/0 :: () -> web_reply()).
get_release_opts() ->
	Opts = agent_auth:get_releases(),
	Encoded = [{struct, [
		{<<"label">>, list_to_binary(R#release_opt.label)},
		{<<"id">>, R#release_opt.id},
		{<<"bias">>, R#release_opt.bias}
	]} || R <- Opts],
	?reply_success(Encoded).

api(ApiArea, Cookie, Post) when ApiArea =:= api; ApiArea =:= supervisor ->
	Request = proplists:get_value("request", Post),
	{struct, Props} = mochijson2:decode(Request),
	%?DEBUG("The request:  ~p", [Props]),
	case {proplists:get_value(<<"function">>, Props), proplists:get_value(<<"args">>, Props)} of
		{undefined, _} ->
			?reply_err(<<"no function to call">>, <<"NO_FUNCTION">>);
		{<<"check_cookie">>, _} ->
			check_cookie(Cookie);
		{<<"get_salt">>, _} ->
			get_salt(Cookie);
		{<<"login">>, [Username, Password]} ->
			login(Cookie, binary_to_list(Username), binary_to_list(Password), []);
		{<<"login">>, [Username, Password, {struct, LoginProps}]} ->
			?INFO("Login opts:  ~p", [LoginProps]),
			LoginOpts = lists:flatten([case X of
				{<<"voipendpointdata">>, <<>>} ->
					{voipendpointdata, undefined};
				{<<"voipendpointdata">>, Bin} ->
					{voipendpointdata, binary_to_list(Bin)};
				{<<"voipendpoint">>, <<"sip_registration">>} ->
					{voipendpoint, sip_registration};
				{<<"voipendpoint">>, <<"sip">>} ->
					{voipendpoint, sip};
				{<<"voipendpoint">>, <<"iax2">>} ->
					{voipendpoint, iax2};
				{<<"voipendpoint">>, <<"h323">>} -> 
					{voipendpoint, h323};
				{<<"voipendpoint">>, <<"pstn">>} ->
					{voipendpoint, pstn};
				{<<"useoutbandring">>, true} ->
					use_outband_ring;
				{<<"usepersistentringchannel">>, true} ->
					use_persistent_ring;
				{_, _} ->
					[]
			end || X <- LoginProps]),
			login(Cookie, binary_to_list(Username), binary_to_list(Password), LoginOpts);
		{<<"get_brand_list">>, _} ->
			get_brand_list();
		{<<"get_queue_list">>, _} ->
			get_queue_list();
		{<<"get_release_opts">>, _} ->
			get_release_opts();
		{Function, Args} ->
			send_to_connection(ApiArea, Cookie, Function, Args)
	end;
api(checkcookie, Cookie, _Post) ->
	check_cookie(Cookie);
api(getsalt, badcookie, _Post) -> %% badcookie when getting a salt
	get_salt(badcookie);
api(Apirequest, badcookie, _Post) ->
	?INFO("bad cookie for request ~p", [Apirequest]),
	Reflist = erlang:ref_to_list(make_ref()),
	Cookie = make_cookie(Reflist),
	ets:insert(web_connections, {Reflist, undefined, undefined}),
	{403, [{"Set-Cookie", Cookie}], <<"Your session was reset due to a lack of keepalive requests, please log back in.">>};
api(logout, {Reflist, _Salt, Conn}, _Post) ->
	ets:insert(web_connections, {Reflist, undefined, undefined}),
	Cookie = io_lib:format("cpx_id=~p; path=/; Expires=Tue, 29-Mar-2005 19:30: 42 GMT; Max-Age=86400", [Reflist]),
	catch agent_web_connection:api(Conn, logout),
	{200, [{"Set-Cookie", Cookie}], mochijson2:encode({struct, [{success, true}]})};
api(_Api, {_Reflist, _Salt, undefined}, _Post) ->
	{403, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"no connection">>}]})};
api({undefined, Path}, {_Reflist, _Salt, Conn}, Post) when is_pid(Conn) ->
	case Post of
		[] ->
			agent_web_connection:api(Conn, {undefined, Path});
		_ ->
			agent_web_connection:api(Conn, {undefined, Path, Post})
	end;
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
-spec(cookie_good/1 :: ([{string(), string()}]) -> 'badcookie' | web_connection()).
cookie_good([]) ->
	badcookie;
cookie_good(Allothers) ->
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
	%?DEBUG("Path:  ~s", [Path]),
	case Path of
		"/" ->
			{file, {"index.html", util:priv_dir("www/agent") ++ "/"}};
		"/api" ->
			{api, api};
		"/logout" ->
			{api, logout};
		"/supervisor" ->
			{api, supervisor};
		_Other ->
			["" | Tail] = util:string_split(Path, "/"),
			case Tail of 
				["dynamic" | Moretail] ->
					File = string:join(Moretail, "/"),
					Dynamic = case application:get_env('OpenACD', webdir_dynamic) of
						undefined ->
							util:priv_dir("www/dynamic") ++ "/";
						{ok, WebDirDyn} ->
							WebDirDyn
					end,
					case filelib:is_regular(string:join([Dynamic, File], "/")) of
						true ->
							{file, {File, Dynamic}};
						false ->
							{api, {undefined, Path}}
					end;
				_Allother ->
					% is there an actual file to serve?
					case {filelib:is_regular(string:concat(util:priv_dir("www/agent"), Path)), filelib:is_regular(string:concat(util:priv_dir("www/contrib"), Path))} of
						{true, false} ->
							{file, {string:strip(Path, left, $/), util:priv_dir("www/agent") ++ "/"}};
						{false, true} ->
							{file, {string:strip(Path, left, $/), util:priv_dir("www/contrib/") ++ "/"}};
						{true, true} ->
							{file, {string:strip(Path, left, $/), util:priv_dir("www/contrib/") ++ "/"}};
						{false, false} ->
							{api, {undefined, Path}}
					end
			end
	end.

-ifdef(TEST).

-define(url(Path), lists:append(["http://127.0.0.1:", integer_to_list(?PORT), Path])).

cookie_file_tests() ->
	{ foreach,
		fun() ->
			agent_web_listener:start(),
			inets:start(),
			{ok, Httpc} = inets:start(httpc, [{profile, test_prof}]),
			Httpc
		end,
		fun(Httpc) ->
			inets:stop(httpc, Httpc),
			inets:stop(),
			agent_web_listener:stop(),
			timer:sleep(10)
		end, [
		fun(_Httpc) -> {"Get a cookie on index page request", fun() ->
			{ok, Result} = httpc:request(?url("/")),
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
		end} end,
		fun(_Httpc) -> {"Try to get a page with a bad cookie", fun() ->
			{ok, {{_Httpver, Code, _Message}, Head, _Body}} = httpc:request(get, {?url("/"), [{"Cookie", "goober=snot"}]}, [], []),
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
		end} end,
		fun(_Httpc) -> {"Get a cookie, then a request with that cookie", fun() ->
			{ok, {_Statusline, Head, _Body}} = httpc:request(?url("/")),
			Cookie = proplists:get_all_values("set-cookie", Head),
			Cookielist = lists:map(fun(I) -> {"Cookie", I} end, Cookie),
			{ok, {{_Httpver, Code, _Message}, Head2, _Body2}} = httpc:request(get, {?url(""), Cookielist}, [], []),
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
		end} end
	]}.

get_salt_tests() ->
	{
		foreach,
		fun() ->
			agent_web_listener:start(),
			inets:start(),
			{ok, Httpc} = inets:start(httpc, [{profile, test_prof2}]),
			?CONSOLE("Listener:  ~p", [whereis(agent_web_listener)]),
			HttpRes = httpc:request(?url("")),
			{ok, {_Statusline, Head, _Body}} = HttpRes,
			Cookie = proplists:get_all_values("set-cookie", Head),
			?CONSOLE("cookie_api_test_ setup ~p", [Cookie]),
			Cookieproplist = lists:map(fun(I) -> {"Cookie", I} end, Cookie),
			?CONSOLE("cookie proplist ~p", [Cookieproplist]),
			os:cmd("ssh-keygen -t rsa -f ../key -N \"\""),
			{Httpc, Cookieproplist}
		end,
		fun({Httpc, _Cookie}) ->
			inets:stop(httpc, Httpc),
			inets:stop(),
			file:delete("../key"),
			agent_web_listener:stop(),
			timer:sleep(10)
		end,
		[
			fun({_Httpc, Cookielist}) ->
				{"Get a salt with a valid cookie",
				fun() ->
					?CONSOLE("Listener:  ~p", [whereis(agent_web_listener)]),
					{ok, {{_Ver, Code, _Msg}, _Head, Body}} = httpc:request(get, {?url("/getsalt"), Cookielist}, [], []),
					?CONSOLE("body:  ~p", [Body]),
					{struct, Pairs} = mochijson2:decode(Body),
					?assertEqual(200, Code),
					?assertEqual(true, proplists:get_value(<<"success">>, Pairs)),
					?assertEqual(<<"Salt created, check salt property">>, proplists:get_value(<<"message">>, Pairs)),
					?assertNot(undefined =:= proplists:get_value(<<"salt">>, Pairs))
				end}
			end,
			fun({_Httpc, _Cookie}) ->
				{"Get a salt with an invalid cookie should issue a new cookie",
				fun() ->
					{ok, {{_Ver, Code, _Msg}, Head, Body}} = httpc:request(get, {?url("/getsalt"), [{"Cookie", "cpx_id=snot"}]}, [], []),
					?assertEqual(200, Code),
					?assertNot(noexist =:= proplists:get_value("set-cookie", Head, noexist)),
					?assertMatch("{\"success\":true"++_, Body)
				end}
			end
		]
	}.
	
web_connection_login_tests() ->
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
			{ok, {_Statusline, Head, _Body}} = httpc:request(?url("")),
			?CONSOLE("request head ~p", [Head]),
			Cookies = proplists:get_all_values("set-cookie", Head),
			Cookielist = lists:map(fun(I) -> {"Cookie", I} end, Cookies), 
			?CONSOLE("~p", [agent_auth:add_agent("testagent", "pass", [english], agent, "Default")]),
			os:cmd("ssh-keygen -t rsa -f ../key -N \"\""),
			Getsalt = fun() ->
				{ok, {_Statusline2, _Head2, Body2}} = httpc:request(get, {?url("/getsalt"), Cookielist}, [], []),
				?CONSOLE("Body2:  ~p", [Body2]),
				{struct, Jsonlist} = mochijson2:decode(Body2),
				binary_to_list(proplists:get_value(<<"salt">>, Jsonlist))
			end,
			
			{Httpc, Cookielist, Getsalt}
		end,
		fun({Httpc, _Cookie, _Getsalt}) ->
			inets:stop(httpc, Httpc),
			inets:stop(),
			file:delete("../key"),
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
					Key = [crypto:mpint(N) || N <- util:get_pubkey()], % cheating a little here...
					Salted = crypto:rsa_public_encrypt(list_to_binary(string:concat("123345", "badpass")), Key, rsa_pkcs1_padding),
					{ok, {_Statusline, _Head, Body}} = httpc:request(post, {?url("/login"), Cookie, "application/x-www-form-urlencoded", lists:append(["username=badun&password=", util:bin_to_hexstr(Salted), "&voipendpoint=SIP Registration"])}, [], []),
					?CONSOLE("BODY:  ~p", [Body]),
					{struct, Json} = mochijson2:decode(Body),
					?assertEqual(false, proplists:get_value(<<"success">>, Json)),
					?assertEqual(<<"Your client is requesting a login without first requesting a salt.">>, proplists:get_value(<<"message">>, Json))
				end}
			end,
			fun({_Httpc, Cookie, _Salt}) ->
				{"Trying to login before salt request, refined api",
				fun() ->
					Key = [crypto:mpint(N) || N <- util:get_pubkey()], % cheating a little here...
					Salted = crypto:rsa_public_encrypt(list_to_binary(string:concat("123345", "badpass")), Key, rsa_pkcs1_padding),
					Request = mochijson2:encode({struct, [
						{<<"function">>, <<"login">>},
						{<<"args">>, [
							<<"badun">>,
							list_to_binary(util:bin_to_hexstr(Salted))
						]}
					]}),
					RequestBody = binary_to_list(list_to_binary(lists:flatten(["request=", Request]))),
					{ok, {_Statusline, _Head, Body}} = httpc:request(post, 
						{?url("/api"), 
						Cookie, 
						"application/x-www-form-urlencoded",
						RequestBody},
					[], []),
					?CONSOLE("BODY:  ~p", [Body]),
					{struct, Json} = mochijson2:decode(Body),
					?assertEqual(false, proplists:get_value(<<"success">>, Json)),
					?assertEqual(<<"Your client is requesting a login without first requesting a salt.">>, proplists:get_value(<<"message">>, Json))
				end}
			end,
			fun({_Httpc, Cookie, Salt}) ->
				{"Login with a bad pw",
				fun() ->
					Key = [crypto:mpint(N) || N <- util:get_pubkey()], % cheating a little here...
					Salted = crypto:rsa_public_encrypt(list_to_binary(string:concat(Salt(),"badpass")), Key, rsa_pkcs1_padding),
					{ok, {_Statusline, _Head, Body}} = httpc:request(post, {?url("/login"), Cookie, "application/x-www-form-urlencoded", lists:append(["username=testagent&password=", util:bin_to_hexstr(Salted), "&voipendpoint=SIP Registration"])}, [], []),
					?CONSOLE("BODY:  ~p", [Body]),
					{struct, Json} = mochijson2:decode(Body),
					?assertEqual(false, proplists:get_value(<<"success">>, Json)),
					?assertEqual(<<"Authentication failed">>, proplists:get_value(<<"message">>, Json))
				end}
			end,
			fun({_Httpc, Cookie, Salt}) ->
				{"Login with a bad pw refined api",
				fun() ->
					Key = [crypto:mpint(N) || N <- util:get_pubkey()], % cheating a little here...
					Salted = crypto:rsa_public_encrypt(list_to_binary(string:concat(Salt(),"badpass")), Key, rsa_pkcs1_padding),
					RequestJson = mochijson2:encode({struct, [
						{<<"function">>, <<"login">>},
						{<<"args">>, [
							<<"testagent">>,
							list_to_binary(util:bin_to_hexstr(Salted))
						]}
					]}),
					RequestBody = binary_to_list(list_to_binary(lists:flatten(["request=", RequestJson]))),
					{ok, {_Statusline, _Head, Body}} = httpc:request(post, {?url("/api"), Cookie, "application/x-www-form-urlencoded", RequestBody}, [], []),
					?CONSOLE("BODY:  ~p", [Body]),
					{struct, Json} = mochijson2:decode(Body),
					?assertEqual(false, proplists:get_value(<<"success">>, Json)),
					?assertEqual(<<"Authentication failed">>, proplists:get_value(<<"message">>, Json))
				end}
			end,
			fun({_Httpc, Cookie, Salt}) ->
				{"Login with bad un",
				fun() ->
					Key = [crypto:mpint(N) || N <- util:get_pubkey()], % cheating a little here...
					Salted = crypto:rsa_public_encrypt(list_to_binary(string:concat(Salt(),"pass")), Key, rsa_pkcs1_padding),
					{ok, {_Statusline, _Head, Body}} = httpc:request(post, {?url("/login"), Cookie, "application/x-www-form-urlencoded", lists:append(["username=badun&password=", util:bin_to_hexstr(Salted), "&voipendpoint=SIP Registration"])}, [], []),
					?CONSOLE("BODY:  ~p", [Body]),
					{struct, Json} = mochijson2:decode(Body),
					?assertEqual(false, proplists:get_value(<<"success">>, Json)),
					?assertEqual(<<"Authentication failed">>, proplists:get_value(<<"message">>, Json))
				end}
			end,
			fun({_Httpc, Cookie, Salt}) ->
				{"Login with bad un refined api",
				fun() ->
					Key = [crypto:mpint(N) || N <- util:get_pubkey()], % cheating a little here...
					Salted = crypto:rsa_public_encrypt(list_to_binary(string:concat(Salt(),"pass")), Key, rsa_pkcs1_padding),
					Request = mochijson2:encode({struct, [
						{<<"function">>, <<"login">>},
						{<<"args">>, [
							<<"badun">>,
							list_to_binary(util:bin_to_hexstr(Salted))
						]}
					]}),
					RequestBody = binary_to_list(list_to_binary(lists:flatten(["request=", Request]))),
					{ok, {_Statusline, _Head, Body}} = httpc:request(post, {?url("/api"), Cookie, "application/x-www-form-urlencoded", RequestBody}, [], []),
					?CONSOLE("BODY:  ~p", [Body]),
					{struct, Json} = mochijson2:decode(Body),
					?assertEqual(false, proplists:get_value(<<"success">>, Json)),
					?assertEqual(<<"Authentication failed">>, proplists:get_value(<<"message">>, Json))
				end}
			end,
			fun({_Httpc, Cookie, Salt}) ->
				{"Login with bad salt",
				fun() ->
					Key = [crypto:mpint(N) || N <- util:get_pubkey()], % cheating a little here...
					Salt(),
					Salted = crypto:rsa_public_encrypt(list_to_binary(string:concat("345678","pass")), Key, rsa_pkcs1_padding),
					{ok, {_Statusline, _Head, Body}} = httpc:request(post, {?url("/login"), Cookie, "application/x-www-form-urlencoded", lists:append(["username=testagent&password=", util:bin_to_hexstr(Salted), "&voipendpoint=SIP Registration"])}, [], []),
					?CONSOLE("BODY:  ~p", [Body]),
					{struct, Json} = mochijson2:decode(Body),
					?assertEqual(false, proplists:get_value(<<"success">>, Json)),
					?assertEqual(<<"Invalid salt">>, proplists:get_value(<<"message">>, Json))
				end}
			end,
			fun({_Httpc, Cookie, Salt}) ->
				{"Login with bad salt refined api",
				fun() ->
					Key = [crypto:mpint(N) || N <- util:get_pubkey()], % cheating a little here...
					Salt(),
					Salted = crypto:rsa_public_encrypt(list_to_binary(string:concat("345678","pass")), Key, rsa_pkcs1_padding),
					Request = mochijson2:encode({struct, [
						{<<"function">>, <<"login">>},
						{<<"args">>, [
							<<"testagent">>,
							list_to_binary(util:bin_to_hexstr(Salted))
						]}
					]}),
					RequestBody = binary_to_list(list_to_binary(lists:flatten(["request=", Request]))),
					{ok, {_Statusline, _Head, Body}} = httpc:request(post, {?url("/api"), Cookie, "application/x-www-form-urlencoded", RequestBody}, [], []),
					?CONSOLE("BODY:  ~p", [Body]),
					{struct, Json} = mochijson2:decode(Body),
					?assertEqual(false, proplists:get_value(<<"success">>, Json)),
					?assertEqual(<<"Invalid salt">>, proplists:get_value(<<"message">>, Json))
				end}
			end,





			fun({_Httpc, Cookie, Salt}) ->
				{"Login properly",
				fun() ->
					Key = [crypto:mpint(N) || N <- util:get_pubkey()], % cheating a little here...
					Salted = crypto:rsa_public_encrypt(list_to_binary(string:concat(Salt(),"pass")), Key, rsa_pkcs1_padding),
					{ok, {_Statusline, _Head, Body}} = httpc:request(post, {?url("/login"), Cookie, "application/x-www-form-urlencoded", lists:append(["username=testagent&password=", util:bin_to_hexstr(Salted), "&voipendpoint=SIP Registration"])}, [], []),
					?CONSOLE("BODY:  ~p", [Body]),
					{struct, Json} = mochijson2:decode(Body),
					?assertEqual(true, proplists:get_value(<<"success">>, Json))
				end}
			end,
			fun({_Httpc, Cookie, Salt}) ->
				{"Login proper with refined api",
				fun() ->
					Key = [crypto:mpint(N) || N <- util:get_pubkey()],
					Salted = crypto:rsa_public_encrypt(list_to_binary(string:concat(Salt(), "pass")), Key, rsa_pkcs1_padding),
					BodyJson = mochijson2:encode({struct, [
						{<<"function">>, <<"login">>},
						{<<"args">>, [
							<<"testagent">>,
 							list_to_binary(util:bin_to_hexstr(Salted))
						]}
					]}),
					{ok, {_, _, Body}} = httpc:request(post, {?url("/api"), Cookie, "application/x-www-form-urlencoded", binary_to_list(list_to_binary(lists:flatten(["request=", BodyJson])))}, [], []),
					?CONSOLE("BODY:  ~p", [Body]),
					{struct, Json} = mochijson2:decode(Body),
					?assertEqual(true, proplists:get_value(<<"success">>, Json))
				end}
			end
		]
	}.



% TODO add tests for interaction w/ agent, agent_manager

-define(PATH_TEST_SET, [
		{"/", {file, {"index.html", util:priv_dir("www/agent") ++ "/"}}},
		{"/poll", {api, poll}},
		{"/logout", {api, logout}},
		{"/login", {api, login}},
		{"/getsalt", {api, getsalt}},
		{"/state/teststate", {api, {set_state, "teststate"}}},
		{"/state/teststate/statedata", {api, {set_state, "teststate", "statedata"}}},
		{"/ack/7", {api, {ack, "7"}}},
		{"/err/89", {api, {err, "89"}}},
		{"/err/74/testmessage", {api, {err, "74", "testmessage"}}},
		{"/index.html", {file, {"index.html", util:priv_dir("www/agent/") ++ "/"}}},
		{"/otherfile.ext", {api, {undefined, "/otherfile.ext"}}},
		{"/other/path", {api, {undefined, "/other/path"}}},
		{"/releaseopts", {api, releaseopts}},
		{"/brandlist", {api, brandlist}},
		{"/queuelist", {api, queuelist}},
		{"/checkcookie", {api, checkcookie}},
		{"/dial/12345", {api, {dial, "12345"}}},
		{"/get_avail_agents", {api, get_avail_agents}},
		{"/agent_transfer/agent@domain", {api, {agent_transfer, "agent@domain"}}},
		{"/agent_transfer/agent@domain/1234", {api, {agent_transfer, "agent@domain", "1234"}}},
		{"/mediapush", {api, mediapush}},
		{"/dynamic/test.html", {file, {"test.html", util:priv_dir("www/dynamic") ++ "/"}}}
	]
).

path_parse_test_() ->
	[begin
		?_assertEqual(Expect, parse_path(Path))
	end || {Path, Expect} <- ?PATH_TEST_SET].

cookie_good_test_() ->
	[
		{"A blanke cookie",
		fun() ->
			?assertEqual(badcookie, cookie_good([]))
		end},
		{"An invalid cookie",
		fun() ->
			?assertEqual(badcookie, cookie_good([{"cookiekey", "cookievalue"}]))
		end},
		{"A well formed cookie, but not in ets",
		fun() ->
			ets:new(web_connections, [set, public, named_table]),
			Reflist = erlang:ref_to_list(make_ref()),
			?assertEqual(badcookie, cookie_good([{"cpx_id", Reflist}])),
			ets:delete(web_connections)
		end},
		{"A well formed cookie in the ets",
		fun() ->
			ets:new(web_connections, [set, public, named_table]),
			Reflist = erlang:ref_to_list(make_ref()),
			ets:insert(web_connections, {Reflist, undefined, undefined}),
			?assertEqual({Reflist, undefined, undefined}, cookie_good([{"cpx_id", Reflist}])),
			ets:delete(web_connections)
		end}
	].

all_test_() ->
	{inorder, [
	cookie_file_tests(),
	get_salt_tests(),
	web_connection_login_tests()
	]}.

-endif.
