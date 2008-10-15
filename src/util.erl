-module(util).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([string_split/3, string_split/2, string_chomp/1]).

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

string_chomp(String) ->
	string:strip(string:strip(String,right, $\n), right, $\r).

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
-endif.
