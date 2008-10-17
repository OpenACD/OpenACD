-module(util).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([string_split/3, string_split/2, string_chomp/1, list_contains_all/2]).

-spec(string_split/3 :: (String :: [], Separator :: [integer()], SplitCount :: pos_integer()) -> [];
                        %(String :: [integer(),...], Separator :: [], SplitCount :: 1) -> [integer(),...];
                        %(String :: [integer(),...], Separator :: [integer(),...], SplitCount :: 1) -> [integer(),...];
                        (String :: [integer(),...], Separator :: [], SplitCount :: pos_integer()) -> [[integer()],...];
                        (String :: [integer(),...], Separator :: [integer(),...], SplitCount :: pos_integer()) -> [[integer()],...]).
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
			[string:substr(String, 1, Index - 1) | string_split(string:substr(String, Index + string:len(Separator)), Separator, SplitCount - 1)]
	end.

-spec(string_split/2 :: (String :: [], Separator :: [integer()]) -> [];
                        (String :: [integer(),...], Separator :: []) -> [[integer()]];
                        (String :: [integer(),...], Separator :: [integer(),...]) -> [[integer()]]).
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
string_chomp(String) ->
	string:strip(string:strip(String,right, $\n), right, $\r).

-spec(list_contains_all/2 :: (List :: [any()], Members :: []) -> 'true';
                             (List :: [any()], Members :: [any(),...]) -> 'true' | 'false').
list_contains_all(_List, []) ->
	true;
list_contains_all(List, [H|T]) when is_list(List) ->
	case lists:member(H, List) of
		true -> true =:= list_contains_all(List, T);
		false -> false
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
-endif.
