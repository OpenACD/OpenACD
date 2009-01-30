%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Original Code is Spice Telphony.
%% 
%% The Initial Developer of the Original Code is 
%% Andrew Thompson and Micah Warren.
%% Portions created by the Initial Developers are Copyright (C) 
%% SpiceCSM. All Rights Reserved.

%% Contributor(s): 

%% Andrew Thompson <athompson at spicecsm dot com>
%% Micah Warren <mwarren at spicecsm dot com>
%% 

%% @doc The web management module.  Uses mochiweb for the heavy lifting.  Listens on port 9999 by default.
-module(cpx_web_management).
-author("Micah").

-define(PORT, 9999).
-define(WEB_DEFAULTS, [{name, ?MODULE}, {port, ?PORT}]).

-include("call.hrl").
-include("agent.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start/0, stop/0, loop/3, loop/1]).

% TODO configure this to start by default with a default configureation (cpx_supervisor_conf)
%% @doc Start the web management server unlinked to the parent process.
-spec(start/0 :: () -> {'ok', pid()}).
start() -> 
	mochiweb_http:start([{loop, {?MODULE, loop}} | ?WEB_DEFAULTS]).

%% @doc Stops the web management.
-spec(stop/0 :: () -> 'ok').
stop() -> 
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

-spec(loop/3 :: (Req :: any(), Method :: string(), Path :: string()) -> any()).
loop(Req, _Method, "/") -> 
	Req:ok({"text/html", "Welcome to the management interface."});
loop(Req, _Method, "/queues") ->
	Queues = queue_manager:queues(),
	io:format("Queues:  ~p~n", [Queues]),
	Jqs = [{name, list_to_binary(atom_to_list(Qname))} || {Qname, _Pid} <- Queues],
	io:format("Jqs:  ~p~n", [Jqs]),
	Struct = {struct, Jqs},
	io:format("Struct:  ~p~n", [Struct]),
	try mochijson2:encode(Struct) of 
		Out -> 
			io:format("Out:  ~p~n", [Out]),
			Req:ok({"text/html", Out})
	catch
		exit:{json_encode, {bad_term, Bad}} -> 
			io:format("Catching a json parse error of ~p because ~p is bad.~n", [exit, Bad]),
			Req:respond({500, [], "Bad Json term"})
	end;
loop(Req, _Method, "/web_dump") -> 
	Req:ok({"text/html",io_lib:format("<pre>~p</pre>~n", [Req:dump()])});
loop(Req, _Method, "/set_cookie") -> 
	% TODO hardcoded cookie is hardcoded.
	Req:respond({200, [{"Set-Cookie", "goober=foobar"}], io_lib:format("<pre>~p~p</pre>", [Req:dump(), Req:parse_cookie()])});
loop(Req, _Method, _Path) -> 
	Req:respond({501, [{"Content-Type", "text/plain"}], <<"Not yet implemented">>}).

%% @doc Simply takes the request, yanks out the method and path, and shoves it to loop/3
-spec(loop/1 :: (Req :: any()) -> any()).
loop(Request) -> 
	loop(Request, Request:get(method), Request:get(path)).
