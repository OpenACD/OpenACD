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
-export([start_link/1, start/1, start/0, start_link/0]).

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
	Table = ets:new(web_connections, [set, public, named_table]),
	Pmochie = mochiweb_http:start([{loop, fun(Req) -> loop(Req, Table) end}, {name, ?MOCHI_NAME}, {port, Port}]),
	?CONSOLE("pmochi ~p", [Pmochie]),
	{ok, Mochi} = Pmochie,
    {ok, #state{connections=Table, mochipid = Mochi}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
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
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
	?CONSOLE("Terminating ~p", [Reason]),
	mochiweb_socket_server:stop(?MOCHI_NAME),
	ets:delete(web_connections),
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
	?CONSOLE("loop start",[]),
	
	
	Path = Req:get(path),
	case parse_path(Path) of
		{file, {File, Docroot}} ->
			case Req:parse_cookie() of
				[{"cpx_id", Reflist}] ->
					Req:serve_file(File, Docroot);
				[] ->
					Reflist = erlang:ref_to_list(make_ref()),
					Cookie = io_lib:format("cpx_id=~p", [Reflist]),
					ets:insert(Table, {Reflist, undefined, undefined}),
					Req:serve_file(File, Docroot, [{"Set-Cookie", Cookie}]);
				Allelse ->
					Req:respond({403, [], io_lib:format("Invalid cookie: ~p", [Allelse])})
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
check_cookie(Cookie) ->
	ok.

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
		Other ->
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

-define(MYSERVERFUNC, fun() -> {ok, _Pid} = start_link(), {?MODULE, fun() -> stop() end} end).

-include("gen_server_test.hrl").


-endif.