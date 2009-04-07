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

-export([
	string_split/3,
	string_split/2,
	string_chomp/1,
	list_contains_all/2,
	list_map_with_index/2,
	bin_to_hexstr/1,
	hexstr_to_bin/1,
	build_table/2,
	group_by/2,
	group_by_with_key/2,
	merge_skill_lists/2,
	subtract_skill_lists/2]).

-spec(string_split/3 :: (String :: [], Separator :: [integer()], SplitCount :: pos_integer()) -> [];
                        %(String :: [integer(),...], Separator :: [], SplitCount :: 1) -> [integer(),...];
                        %(String :: [integer(),...], Separator :: [integer(),...], SplitCount :: 1) -> [integer(),...];
                        (String :: [integer(),...], Separator :: [], SplitCount :: pos_integer()) -> [[integer()],...];
                        (String :: [integer(),...], Separator :: [integer(),...], SplitCount :: pos_integer()) -> [[integer()],...]).
%% @doc Split `String' string by `Separator' into a list of strings at most `SplitCount' long.
%% If `Separator' is a blank string `String' is split into a list of `SplitCount' single character
%% strings followed by the remainder of `String', if any.
string_split(String, Separator, SplitCount) ->
	string_split_(String, Separator, SplitCount, []).

string_split_("", _Separator, _SplitCount, Acc) ->
	lists:reverse(Acc);
string_split_(String, "", 1, Acc) ->
	lists:reverse([String | Acc]);
string_split_(String, _Separator, 1, Acc) ->
	lists:reverse([String | Acc]);
string_split_(String, "", SplitCount, Acc) ->
	string_split_(string:substr(String, 2), "", SplitCount - 1, [string:substr(String, 1, 1) | Acc]);
string_split_(String, Separator, SplitCount, Acc) ->
	case string:str(String, Separator) of
		0 ->
			lists:reverse([String | Acc]);
		Index ->
			Head = string:substr(String, 1, Index - 1),
			Tailpresplit = string:substr(String, Index + string:len(Separator)),
			string_split_(Tailpresplit, Separator, SplitCount - 1, [Head | Acc])
	end.

-spec(string_split/2 :: (String :: [], Separator :: [integer()]) -> [];
                        (String :: [integer(),...], Separator :: []) -> [[integer()]];
                        (String :: [integer(),...], Separator :: [integer(),...]) -> [[integer()]]).
%% @doc Split `String' by `Separator'.
%% If `Separator' is a blank string `String' is split into a list of single character strings.

string_split(String, Separator) ->
	string_split_(String, Separator, []).

string_split_("", _Separator, Acc) ->
	lists:reverse(Acc);
string_split_(String, "", Acc) ->
	string_split_(string:substr(String, 2), "", [string:substr(String, 1, 1) | Acc]);
string_split_(String, Separator, Acc) ->
	case string:str(String, Separator) of
		0 ->
			lists:reverse([String | Acc]);
		Index ->
			string_split_(string:substr(String, Index + string:len(Separator)), Separator, [string:substr(String, 1, Index - 1) | Acc])
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
	list_map_with_index(Fun, List, 0, []).

-spec(list_map_with_index/4 :: (Fun :: fun((Counter :: non_neg_integer(), Elem :: any()) -> any()), List :: [any(),...], Counter :: non_neg_integer(), Acc :: [any(),...]) -> [any(), ...];
	(Fun :: fun((Counter :: non_neg_integer(), Elem :: any()) -> any()), List :: [], Counter :: non_neg_integer(), Acc :: []) -> []).
list_map_with_index(_Fun, [], _Counter, Acc) ->
	lists:reverse(Acc);
list_map_with_index(Fun, [H|T], Counter, Acc) ->
	list_map_with_index(Fun, T, Counter + 1, [Fun(Counter, H) | Acc]).

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
	hexstr_to_bin(string:to_upper(S), []).

-define(HEX, [$0, $1, $2, $3, $5, $6, $7, $8, $9, $1, $A, $B, $C, $D, $E, $F]).

%% @private
-spec(hexstr_to_bin/2 :: (string(), Acc :: string()) -> binary() | 'error').
hexstr_to_bin([], Acc) ->
	list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X, Y | T], Acc) ->
	case {lists:member(X, ?HEX), lists:member(Y, ?HEX)} of
		{true, true} ->
			{ok, [V], []} = io_lib:fread("~16u", [X, Y]),
			hexstr_to_bin(T, [V | Acc]);
		_Else ->
			error
	end.

%end 'borrowed' code

%% @doc groups a list into a list of lists where each sublist contains
%% the elements where `Fun(Element)' returned the same value.
-spec(group_by_with_key/2 :: (Fun :: fun((Value :: any()) -> any()), List :: [any()]) -> [{any, any()}]).
group_by_with_key(Fun, List) ->
	dict:to_list(lists:foldl(fun(X, Acc) ->
		V = Fun(X),
		case dict:is_key(V, Acc) of
			true ->
				dict:append(V, X, Acc);
			false ->
				dict:store(V, [X], Acc)
		end
	end, dict:new(), List)).

-spec(group_by/2 :: (Fun :: fun((Value :: any()) -> any()), List :: [any()]) -> [any()]).
group_by(Fun, List) ->
	lists:map(fun({_Key, Value}) -> Value end, group_by_with_key(Fun, List)).

%% @doc Merges 2 skill lists using lists:umerge and then ensures that only one
%% instance of each magic skill is present, at maximum.
-spec(merge_skill_lists(List1 :: [any()], List2 :: [any()]) -> [any()]).
merge_skill_lists(List1, List2) ->
	lists:foreach(fun({Key, Value}) ->
		case length(Value) of
			1 ->
				ok;
			_Else ->
				io:format("duplicate magic skill ~p in first argument", [Key]),
				erlang:error(badarg)
		end
	end, group_by_with_key(fun({SkillAtom, _SkillString}) -> SkillAtom end, lists:filter(fun(X) -> is_tuple(X) end, List1))),
	
	lists:foreach(fun({Key, Value}) ->
		case length(Value) of
			1 ->
				ok;
			_Else ->
				io:format("duplicate magic skill ~p in second argument", [Key]),
				erlang:error(badarg)
		end
	end, group_by_with_key(fun({SkillAtom, _SkillString}) -> SkillAtom end, lists:filter(fun(X) -> is_tuple(X) end, List2))),

	NewList = lists:umerge(lists:sort(List1), lists:sort(List2)),
	MagicSkills = lists:filter(fun(X) -> is_tuple(X) end, NewList),

	Grouped = group_by_with_key(fun({SkillAtom, _SkillString}) -> SkillAtom end, MagicSkills),
	
	lists:filter(fun({SkillAtom, _SkillString} = Val) ->
				case length(proplists:get_value(SkillAtom, Grouped)) of
					1 ->
						true;
					2 ->
						lists:member(Val, List2)
				end;
		(_Skill) -> true
	end, NewList).

subtract_skill_lists(List1, List2) ->
	FilterList = lists:map(fun({SkillAtom, _SkillString}) -> SkillAtom; (Skill) -> Skill end, List2),
	lists:filter(fun({SkillAtom, _SkillString}) -> lists:member(SkillAtom, FilterList); (Skill) -> lists:member(Skill, FilterList) end, List1).

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
							io:format("Table '~p' already exists.~n", [Tablename]),
							ok;
						false ->
							mnesia:create_table(Tablename, Options)
					end;
				false -> 
					io:format("Mnesia does not have a disc based schema.~n", []),
					exit(mnesia_schema_not_found)
			end;
		Else -> 
			io:format("Mnesia is not running, in state ~p~n.", [Else]),
			exit(mnesia_stopped)
	end.

%% @doc Find the position of `Needle' (`any()') in a list.  Remember, the first element in an erlang list is 1.
-spec(list_index/2 :: (Needle :: any(), Haystack :: [any()]) -> non_neg_integer()).
list_index(Needle, List) ->
	F = fun(Needle, Item) ->
		Needle =:= Item
	end,
	list_index_(F, Needle, List, 1).

%% @doc Find the position of `Needle' (`any()') in a list using `Fun' (`fun()') as the comparison.
-spec(list_index/3 :: (Fun :: fun((any) -> bool()), Needle :: any(), List :: [any()]) -> non_neg_integer()).
list_index(Fun, Needle, List) ->
	list_index_(Fun, Needle, List, 1).

%% @private
list_index_(_Fun, _Needle, [], _Index) ->
	0;
list_index_(Fun, Needle, [Head | Tail], Index) ->
	case Fun(Needle, Head) of
		false ->
			list_index_(Fun, Needle, Tail, Index + 1);
		true ->
			Index
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
		},
		{
			"To bin from invalid Hex",
			fun() ->
				?assertEqual(error, hexstr_to_bin("abcdefghijklmnop")),
				?assertEqual(error, hexstr_to_bin("12345"))
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
			},
			{
				"Table already exists",
				fun() ->
					mnesia:create_schema([node()]),
					mnesia:start(),
					?assertMatch({atomic, ok}, build_table(test_table, ?TEST_TABLE)),
					?assertMatch(ok, build_table(test_table, ?TEST_TABLE))
				end
			}
		]
	}.

group_by_test() ->
	?assertEqual([[2,4],[1,3,5]], util:group_by(fun(X) -> (X rem 2) =:= 0 end, [1, 2, 3, 4, 5])).

merge_skill_lists_test_() ->
	[
		{
			"Retains magic skills from the first list but overrides duplicates with magic skills from the second list",
			fun () ->
				?assertEqual([foo, {'_agent', "Steve"}, {'_brand', "Test"}], util:merge_skill_lists([{'_agent', "Fred"}, foo, {'_brand', "Test"}], [{'_agent', "Steve"}, foo]))
			end
		},
		{
			"errors out with badarg if one of the lists contains a duplicate magic skill",
			fun() ->
				?assertError(badarg, util:merge_skill_lists([{'_agent', "Foo"}, {'_agent', "Bar"}], [{'_agent', "Baz"},foo])),
				?assertError(badarg, util:merge_skill_lists([{'_agent', "Foo"}], [{'_agent', "Baz"},foo, {'_agent', "Bar"}])),
				?assertError(badarg, util:merge_skill_lists([{'_agent', "Foo"}, {'_agent', "Bar"}], [foo])),
				?assertError(badarg, util:merge_skill_lists([foo], [{'_agent', "Foo"}, {'_agent', "Bar"}]))
			end
		}
	].

subtract_skill_lists_test() ->
	?assertEqual([{'_agent',"Foo"},baz], util:subtract_skill_lists([{'_agent', "Foo"}, bar, baz], ['_agent', baz])),
	?assertEqual([{'_agent',"Foo"},baz], util:subtract_skill_lists([{'_agent', "Foo"}, bar, baz], [{'_agent', "Whatever"}, baz])),
	?assertEqual([baz], util:subtract_skill_lists([bar, baz], [{'_agent', "Whatever"}, baz])).

list_index_test_() ->
	[{"Default check of =:=",
	fun() ->
		?assertEqual(0, list_index(a, [b, c, d])),
		?assertEqual(1, list_index(b, [b, c, d])),
		?assertEqual(2, list_index(c, [b, c, d])),
		?assertEqual(0, list_index(a, []))
	end},
	{"Custom fun check",
	fun() ->
		F = fun({A, B}, {C, _}) ->
			?debugFmt("~p ~p ~p", [A, B, C]),
			A =:= C
		end,
		?assertEqual(0, list_index(F, {a, b}, [{c, d}, {e, f}, {g, h}])),
		?assertEqual(1, list_index(F, {a, b}, [{a, c}, {e, f}, {g, h}])),
		?assertEqual(2, list_index(F, {a, b}, [{c, d}, {a, f}, {g, h}])),
		?assertEqual(0, list_index(F, {a, b}, []))
	end}].

-endif.
