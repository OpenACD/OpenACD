%%%-------------------------------------------------------------------
%%% File          : cpx_json.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  10/31/08
%%%-------------------------------------------------------------------

%% @doc Massages data so that it is better suited for mochiweb's json output.

-module(cpx_json).
-author("Micah").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("call.hrl").
-include("agent.hrl").

% API
-export([make_struct/2, make_proplist/2, fix_item/1]).

%% @doc make a mochiweb_json2 compatible structure tuple from a value list and a names list
-spec(make_struct/2 :: (Items :: [any()], Names :: [atom()]) -> [{struct, [{atom(), any()}]}]).
make_struct([], _Names) -> 
	[];
make_struct([Item | Tail] , Names) when size(Item) =:= size(Names) -> 
	[{struct, make_proplist(Item, Names)} | make_struct(Tail, Names)].

%% @doc make a [{Key, Value}] suitable for make_struct
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
