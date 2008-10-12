-module(util).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([string_split/3, string_split/2]).

string_split(String, _Separator, 1) ->
	[String];
string_split(String, Separator, SplitCount) ->
	case string:str(String, Separator) of
		0 ->
			[String];
		Index ->
			[string:substr(String, 1, Index - 1) | string_split(string:substr(String, Index + string:len(Separator)), Separator, SplitCount - 1)]
	end.

string_split(String, Separator) ->
	case string:str(String, Separator) of
		0 ->
			[String];
		Index ->
			[string:substr(String, 1, Index - 1) | string_split(string:substr(String, Index + string:len(Separator)), Separator)]
	end.
