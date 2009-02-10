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

%% @doc A utility module containing several handy functions not provided by
%% erlang's standard library.
-module(util).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-record(test_table, {name, data}).
-define(TEST_TABLE, 
	[
		{attributes, record_info(fields, test_table)},
		{ram_copies, lists:append([nodes(), [node()]])}
	]
).
-endif.

-include("call.hrl").

-export([
	string_split/3,
	string_split/2,
	string_chomp/1,
	list_contains_all/2,
	list_map_with_index/2,
	bin_to_hexstr/1,
	hexstr_to_bin/1,
	build_table/2]).

-spec(string_split/3 :: (String :: [], Separator :: [integer()], SplitCount :: pos_integer()) -> [];
                        %(String :: [integer(),...], Separator :: [], SplitCount :: 1) -> [integer(),...];
                        %(String :: [integer(),...], Separator :: [integer(),...], SplitCount :: 1) -> [integer(),...];
                        (String :: [integer(),...], Separator :: [], SplitCount :: pos_integer()) -> [[integer()],...];
                        (String :: [integer(),...], Separator :: [integer(),...], SplitCount :: pos_integer()) -> [[integer()],...]).
%% @doc Split `String' string by `Separator' into a list of strings at most `SplitCount' long.
%% If `Separator' is a blank string `String' is split into a list of `SplitCount' single character
%% strings followed by the remainder of `String', if any.
string_split("", _Separator, _SplitCount) ->
	[];
string_split(String, "", 1) ->
	[String];
string_split(String, _Separator, 1) ->
	[String];
string_split(String, "", SplitCount) ->
	[string:substr(String, 1, 1) | string_split(string:substr(String, 2), "", SplitCount - 1)];
string_split(String, Separator, SplitCount) ->
	case string:str(String, Separator) of
		0 ->
			[String];
		Index ->
			Head = string:substr(String, 1, Index - 1),
			Tailpresplit = string:substr(String, Index + string:len(Separator)),
			[Head | string_split(Tailpresplit, Separator, SplitCount - 1)]
	end.

-spec(string_split/2 :: (String :: [], Separator :: [integer()]) -> [];
                        (String :: [integer(),...], Separator :: []) -> [[integer()]];
                        (String :: [integer(),...], Separator :: [integer(),...]) -> [[integer()]]).
%% @doc Split `String' by `Separator'.
%% If `Separator' is a blank string `String' is split into a list of single character strings.
string_split("", _Separator) ->
	[];
string_split(String, "") ->
	[string:substr(String, 1, 1) | string_split(string:substr(String, 2), "")];
string_split(String, Separator) ->
	case string:str(String, Separator) of
		0 ->
			[String];
		Index ->
			[string:substr(String, 1, Index - 1) | string_split(string:substr(String, Index + string:len(Separator)), Separator)]
	end.

-spec(string_chomp/1 :: (String :: string()) -> string()).
%% @doc Remove any trailing newlines or carraige returns from `String'.
string_chomp(String) ->
	string:strip(string:strip(String, right, $\n), right, $\r).

-spec(list_contains_all/2 :: (List :: [any()], Members :: []) -> 'true';
                             (List :: [any()], Members :: [any(),...]) -> 'true' | 'false').
%% @doc Check whether `List' contains all elements of `Members'.
list_contains_all(_List, []) ->
	true;
list_contains_all(List, [H | Members]) when is_list(List) ->
	case lists:member(H, List) of
		true -> true =:= list_contains_all(List, Members);
		false -> false
	end.

-spec(list_map_with_index/2 :: (Fun :: fun((Counter :: non_neg_integer(), Elem :: any()) -> any()), List :: [any()]) -> [any()]).
%% @doc Apply the `Fun(Index, Element)' to each element of `List' along with the element's index in `List'.
%% @see lists:map/2
list_map_with_index(Fun, List) when is_function(Fun), is_list(List) ->
	list_map_with_index(Fun, List, 0).

-spec(list_map_with_index/3 :: (Fun :: fun((Counter :: non_neg_integer(), Elem :: any()) -> any()), List :: [any(),...], Counter :: non_neg_integer()) -> [any(), ...];
					(Fun :: fun((Counter :: non_neg_integer(), Elem :: any()) -> any()), List :: [], Counter :: non_neg_integer()) -> []).
list_map_with_index(_Fun, [], _Counter) ->
	[];
list_map_with_index(Fun, [H|T], Counter) ->
	[Fun(Counter, H) | list_map_with_index(Fun, T, Counter + 1)].

%% code below shamelessly 'borrowed' from Steve Vinoski in his comments at
% http://necrobious.blogspot.com/2008/03/binary-to-hex-string-back-to-binary-in.html
% changing it to lower case was added by micah on dec 1st, 2008
% doc and spec added by same.

%% @doc Converts a bin to a lowercase hexidecimal string.
-spec(bin_to_hexstr/1 :: (Bin :: binary()) -> string()).
bin_to_hexstr(Bin) ->
	string:to_lower(lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)])).

%% @doc Converts a hexidecimal string in any case to a binary.
-spec(hexstr_to_bin/1 :: (S :: string()) -> binary()).
hexstr_to_bin(S) ->
	hexstr_to_bin(S, []).

%% @private
-spec(hexstr_to_bin/2 :: (string(), Acc :: string()) -> binary()).
hexstr_to_bin([], Acc) ->
	list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X, Y | T], Acc) ->
	{ok, [V], []} = io_lib:fread("~16u", [X, Y]),
	hexstr_to_bin(T, [V | Acc]).

%end 'borrowed' code

%% @doc build the given mnesia table `Tablename' with `Options'.
%% This will exit the calling process if mnesia is not started or if the schema is not stored on disc.  
%% If you trying to create the same table twice, it simply returns ok, otherwise returns the raw mnesia create_table result.
%% Takes the same parameters as mnesia:create_table.
%% @see mnesia:create_table/2
-spec(build_table/2 :: (atom(), [any()]) -> 'ok' | {'atomic', 'ok'}).
build_table(Tablename, Options) when is_atom(Tablename) ->
	case mnesia:system_info(is_running) of
		yes -> 
			case mnesia:system_info(use_dir) of
				true -> 					
					case lists:member(Tablename, mnesia:system_info(tables)) of
						true -> 
							?CONSOLE("Table '~p' already exists.", [Tablename]),
							ok;
						false ->
							mnesia:create_table(Tablename, Options)
					end;
				false -> 
					?CONSOLE("Mnesia does not have a disc based schema.", []),
					exit(mnesia_schema_not_found)
			end;
		Else -> 
			?CONSOLE("Mnesia is not running, in state ~p.", [Else]),
			exit(mnesia_stopped)
	end.
	

-ifdef(EUNIT).
split_empty_string_test() ->
	?assertEqual([] , string_split("", " ")).
split_by_empty_string_test() ->
	?assertEqual(["f", "o", "o"], string_split("foo", "")).
split_by_empty_string_with_limit_test() ->
	?assertEqual(["f", "oo"], string_split("foo", "", 2)).
split_string_test() ->
	?assertEqual(["hello", "world", ":)"], string_split("hello world :)", " ")).
split_string_with_limit_test() ->
	?assertEqual(["hello", "world :)"], string_split("hello world :)", " ", 2)).
split_empty_string_with_limit_test() ->
	?assertEqual([], string_split("", " ", 3)).
split_by_nonexistant_string_test() ->
	?assertEqual(["Hello world"], string_split("Hello world", "batman")).
split_by_nonexistant_string_with_limit_test() ->
	?assertEqual(["Hello world"], string_split("Hello world", "batman", 3)).
split_by_ambiguous_pattern_test() ->
	?assertEqual([[], ";abc", "de;f;g"], string_split(";;;abc;;de;f;g;;", ";;")).
chomp_test() ->
	?assertEqual("Fooo", string_chomp("Fooo\r\n")).
list_contains_all_test() ->
	?assertEqual(true, list_contains_all([foo, bar, baz], [foo])),
	?assertEqual(true, list_contains_all([foo, bar, baz], [foo, bar])),
	?assertEqual(true, list_contains_all([foo, bar, baz], [foo, bar, baz])),
	?assertEqual(true, list_contains_all([foo, bar, baz], [foo, bar, baz, foo])),
	?assertEqual(false, list_contains_all([foo, bar, baz], [foo, bar, baz, foo, bam])),
	?assertEqual(true, list_contains_all([foo, bar, baz], [])),
	?assertEqual(true, list_contains_all([], [])),
	?assertEqual(false, list_contains_all([], [bar])).
list_map_with_index_test() -> 
	L = [1, 2, 3, 4, 5],
	L2 = list_map_with_index(fun(C, Elem) -> C + Elem end, L),
	?assertEqual([1, 3, 5, 7, 9], L2).

hex_bin_conversion_test_() ->
	% the bin is generated by doing erlang:md5 on "teststring"
	% the hex is from ruby's Digest::MD5.hexdigest("teststring")
	[
		{
			"To Hex from Bin",
			fun() -> 
				Bin = erlang:md5("teststring"),
				?assertMatch("d67c5cbf5b01c9f91932e3b8def5e5f8", bin_to_hexstr(Bin))
			end
		},
		{
			"To Bin from lowercase Hex",
			fun() -> 
				Bin = erlang:md5("teststring"),
				?assertMatch(Bin, hexstr_to_bin("d67c5cbf5b01c9f91932e3b8def5e5f8"))
			end
		},
		{
			"To Bin from uppercase Hex",
			fun() -> 
				Bin = erlang:md5("teststring"),
				?assertMatch(Bin, hexstr_to_bin("D67C5CBF5B01C9F91932E3B8DEF5E5F8"))
			end
		}
	].

build_table_test_() -> 
	["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
	{
		foreach,
		fun() -> 
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			
			ok
		end,
		fun(_Whatever) ->
			mnesia:stop(),
			mnesia:delete_schema([node()])
		end,
		[
			{
				"Everything is okay",
				fun() -> 
					mnesia:create_schema([node()]),
					mnesia:start(),
					?assertMatch({atomic, ok}, build_table(test_table, ?TEST_TABLE))
				 end
			},
			{
				"Mnesia not started",
				fun() -> 
					?assertExit(mnesia_stopped, build_table(test_table, ?TEST_TABLE))
				end
			},
			{
				"Mnesia Has Ram Schema",
				fun() ->
					mnesia:start(),
					?assertExit(mnesia_schema_not_found, build_table(test_table, ?TEST_TABLE))
				end
			}
		]
	}.
				
-endif.
