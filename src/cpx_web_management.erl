%%%-------------------------------------------------------------------
%%% File          : cpx_web_management.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : A very simply implemented web management.  Uses paths for requests.  Heavy lifting done by Mochiweb
%%%
%%% Created       :  10/29/08
%%%-------------------------------------------------------------------

%% @doc The web management module.  Used mochiweb for the heavy lifting.

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

-spec(start/0 :: () -> {'ok', pid()}).
start() -> 
	mochiweb_http:start([{loop, {?MODULE, loop}} | ?WEB_DEFAULTS]).

-spec(stop/0 :: () -> 'ok').
stop() -> 
	mochiweb_http:stop(?MODULE).

%% @doc Take the request path and use that to determine what to return.
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
	Req:ok({"text/html", "Welcome to the managemnet interface."});
loop(Req, _Method, "/queues") ->
	Queues = queue_manager:queues(),
	Queues2 = make_struct(Queues, {name, pid}),
	Req:ok({"text/html", mochijson2:encode(Queues2)});
loop(Req, _Method, "/web_dump") -> 
	Req:ok({"text/html",io_lib:format("<pre>~p</pre>~n", [Req:dump()])});
loop(Req, _Method, "/set_cookie") -> 
	Req:respond({200, [{"Set-Cookie", "goober=foobar"}], io_lib:format("<pre>~p~p</pre>", [Req:dump(), Req:parse_cookie()])});
loop(Req, _Method, _Path) -> 
	Req:respond({501, [{"Content-Type", "text/plain"}], <<"Not yet implemented">>}).

%% @doc Simply takes the request, yanks out the method and path, and shoves it to loop/3
-spec(loop/1 :: (Req :: any()) -> any()).
loop(Request) -> 
	loop(Request, Request:get(method), Request:get(path)).

%% helper functions

%% @doc make a mochiweb_json2 compatible structure tuple from a value list and a names list
-spec(make_struct/2 :: (Items :: [any()], Names :: [atom()]) -> [{struct, [{atom(), any()}]}]).
make_struct([], _Names) -> 
	[];
make_struct([Item | Tail] , Names) when size(Item) =:= size(Names) -> 
	[{struct, make_proplist(Item, Names)} | make_struct(Tail, Names)].

%% @doc make a [{Key, Value}] suitbale for make_struct
-spec(make_proplist/2 :: (Items :: [any()], Names :: [atom()]) -> [{atom(), any()}]).
make_proplist(Items, Names) when size(Items) =:= size(Names) -> 
	Litems = tuple_to_list(Items),
	LNames = tuple_to_list(Names),
	io:format("Items:  ~p~n", [Litems]),
	FixedItems = lists:map(fun(X) -> fix_item(X) end, Litems),
	lists:zip(LNames, FixedItems).

%% @doc try to coherce some types into mochiweb_json2 compliant types.
%%  mochiweb_json2 types seem to be numbers, lists, and atoms.
%% A typle of type {struct, [{atom, any()}]} will create a json object.
-spec(fix_item/1 :: (I :: pid()) -> atom();
					(I :: tuple()) -> [any()];
					(I :: any()) -> any()).
fix_item(I) when is_pid(I) -> 
	list_to_atom(pid_to_list(I));
fix_item(I) when is_tuple(I) -> 
	tuple_to_list(I);
fix_item(I) -> 
	I.
