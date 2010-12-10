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

%% @doc A utility module containing several handy functions not provided by
%% erlang's standard library.
-module(util).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-record(test_table, 
	{name :: any(), 
	data :: any()}).
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
	string_interpolate/2,
	list_contains_all/2,
	list_map_with_index/2,
	bin_to_hexstr/1,
	hexstr_to_bin/1,
	build_table/2,
	group_by/2,
	group_by_with_key/2,
	merge_skill_lists/2,
	merge_skill_lists/3,
	subtract_skill_lists/2,
	list_index/2,
	list_index/3,
	now/0,
	reload/1,
	reload/2,
	reload_all/0,
	reload_all/1,
	distribution/1,
	get_number/1,
	find_first_arc/2,
	floor/1,
	priv_dir/0,
	priv_dir/1,
	run_dir/0
]).
%% time tracking util functions
-export([
	timemark/0,
	timemark/1,
	timemark/2,
	timemark_clear/0,
	timemark_clear/1
]).

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

-type(word_entry() :: {string(), string()}).
-type(word_list() :: [word_entry()]).
-spec(string_interpolate/2 :: (String :: string(), Words :: word_list()) -> string()).
string_interpolate(String, Words) ->
	{ok, Re} = re:compile("#{([\\w_]+)}"),
	string_interpolate(re:run(String, Re, [{offset, 0}]), String, Re, Words, 0).

string_interpolate(nomatch, String, _Re, _Words, _Offset) ->
	String;
string_interpolate({match, [{Fullmin1, FullLen}, {Startmin1, Len}]}, String, Re, Words, Offset) ->
	Pre = string:substr(String, 1, Fullmin1),
	Key = string:substr(String, Startmin1 + 1, Len),
	Post = string:substr(String, Fullmin1 + 1 + FullLen),
	Val = proplists:get_value(Key, Words, ""),
	Newstring = lists:flatten(lists:append([Pre, Val, Post])),
	string_interpolate(re:run(Newstring, Re, [{offset, Offset + length(Val)}]), Newstring, Re, Words, Offset + FullLen).

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
	% erlang uses 1 index for lists.
	list_map_with_index(Fun, List, 1, []).

-spec(list_map_with_index/4 :: (Fun :: fun((Counter :: non_neg_integer(), Elem :: any()) -> any()), List :: [any(),...], Counter :: non_neg_integer(), Acc :: [any()]) -> [any(), ...];
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
-spec(bin_to_hexstr/1 :: (Bin :: binary()) -> [48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 97 | 98 | 99 | 100 | 101 | 102]).
bin_to_hexstr(Bin) ->
	string:to_lower(lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)])).

%% @doc Converts a hexidecimal string in any case to a binary.
-spec(hexstr_to_bin/1 :: (S :: string()) -> binary() | 'error').
hexstr_to_bin(S) ->
	hexstr_to_bin(string:to_lower(S), []).

-define(HEX, [$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a, $b, $c, $d, $e, $f]).

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
%% instance of each magic skill is present, at maximum. If the same skill is present
%% in both lists, the instance from `List2' is the one retained. Any magic skills should
%% be expanded before calling this function.
-spec(merge_skill_lists/2 :: (List1 :: [any()], List2 :: [any()]) -> [any()]).
merge_skill_lists(List1, List2) ->
	merge_skill_lists(List1, List2, []).

%% @doc merge_skill_lists/2 with an optional whitelist of unexpanded magic skills to ignore.
%% This allows us to strip any duplicates of `_agent' (for example) but not i `_brand'.
-spec(merge_skill_lists/3 :: (List1 :: [any()], List2 :: [any()], Whitelist ::[atom()]) -> [any()]).
merge_skill_lists(List1, List2, Whitelist) ->
	Dupscan = fun({Key, Value}) ->
		case {lists:member(Key, Whitelist), length(Value)} of
			{true, _} ->
				ok;
			{false, 1} ->
				ok;
			{false, _} ->
				erlang:error(badarg)
		end
	end,
	
	FilterFun = fun(X) ->
		is_tuple(X)
	end,
	
	GroupBy = fun({Key, _}) ->
		Key
	end,
	
	lists:foreach(Dupscan, group_by_with_key(GroupBy, lists:filter(FilterFun, List1))),
	lists:foreach(Dupscan, group_by_with_key(GroupBy, lists:filter(FilterFun, List2))),


	%lists:foreach(fun({_Key, Value}) ->
%		case length(Value) of
%			1 ->
%				ok;
%			_Else ->
%				erlang:error(badarg)
%		end
%	end, group_by_with_key(fun({SkillAtom, _SkillString}) -> SkillAtom end, lists:filter(fun(X) -> is_tuple(X) end, List1))),
%	
%	lists:foreach(fun({_Key, Value}) ->
%		case length(Value) of
%			1 ->
%				ok;
%			_Else ->
%				erlang:error(badarg)
%		end
%	end, group_by_with_key(fun({SkillAtom, _SkillString}) -> SkillAtom end, lists:filter(fun(X) -> is_tuple(X) end, List2))),

	NewList = lists:umerge(lists:sort(List1), lists:sort(List2)),
	MagicSkills = lists:filter(fun(X) -> is_tuple(X) end, NewList),

	Grouped = group_by_with_key(fun({SkillAtom, _SkillString}) -> SkillAtom end, MagicSkills),
	
	lists:filter(fun({SkillAtom, _SkillString} = Val) ->
				case length(proplists:get_value(SkillAtom, Grouped)) of
					1 ->
						true;
					2 ->
						case lists:member(SkillAtom, Whitelist) of
							true ->
								true;
							false ->
								lists:member(Val, List2)
						end;
					N when N > 2 ->
						lists:member(SkillAtom, Whitelist)
				end;
		(_Skill) -> true
	end, NewList).

%% @doc Returns all skills in `[{atom(), string()} | atom()] List1' where the
% skills' atom  does not exist in `[{atom(), string()} | atom()} List2'.
-spec(subtract_skill_lists/2 :: (List1 :: [{atom(), string()} | atom()], List2 :: [{atom(), string()} | atom()]) -> [{atom(), string()} | atom()]).
subtract_skill_lists(List1, List2) ->
	io:format("List1:  ~p~nList2:  ~p~n", [List1, List2]),
	Filter = fun
		({Magic, _Val} = Skill) ->
			case {lists:member(Magic, List2), lists:member(Skill, List2)} of
				{false, false} ->
					true;
				_ ->
					false
			end;
		(Skill) ->
			not lists:member(Skill, List2)
	end,
	lists:filter(Filter, List1).

%% @doc build the given mnesia table `Tablename' with `Options'.
%% This will exit the calling process if mnesia is not started or if the schema is not stored on disc.  
%% If you trying to create the same table twice, it returns `exists'.  If the table was copied from another node,
%% it returns `copied'.  Otherwise returns the raw mnesia create_table result, usually `{atomic, ok}'.
%% Takes the same parameters as mnesia:create_table.
%% @see mnesia:create_table/2
-spec(build_table/2 :: (atom(), [any()]) -> 'exists' | {'atomic', 'ok'} | 'copied').
build_table(Tablename, Options) when is_atom(Tablename) ->
	case mnesia:system_info(is_running) =:= yes of
		false ->
			exit(mnesia_stopped);
		true ->
			ok
	end,
	case mnesia:system_info(use_dir) of
		false ->
			exit(mnesia_schema_not_found);
		true ->
			ok
	end,
	case lists:member(Tablename, mnesia:system_info(local_tables)) of
		true -> 
			mnesia:wait_for_tables([Tablename], 5000),
			exists;
		false ->
			case lists:member(Tablename, mnesia:system_info(tables)) of
				true ->
					mnesia:add_table_copy(Tablename ,node(), disc_copies),
					copied;
				false ->
					mnesia:create_table(Tablename, Options)
		end
	end.

%% @doc Find the position of `Needle' (`any()') in a list.  Remember, the first element in an erlang list is 1.
-spec(list_index/2 :: (Needle :: any(), Haystack :: [any()]) -> non_neg_integer()).
list_index(Needle, List) ->
	F = fun(Needl, Item) ->
		Needl =:= Item
	end,
	list_index_(F, Needle, List, 1).

%% @doc Find the position of `Needle' (`any()') in a list using `Fun' (`fun()') as the comparison.
-spec(list_index/3 :: (Fun :: fun((any(), any()) -> boolean()), Needle :: any(), List :: [any()]) -> non_neg_integer()).
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

%% @doc For those times when {Macro, Sec, Micro} is too much, this smooshes
%% Macro and Sec together.
-spec(now/0 :: () -> pos_integer()).
now() ->
	{Mega, Sec, _} = os:timestamp(),
	Mega * 1000000 + Sec.

%% @doc For those times when you don't need a code reload with release files
%% and version.  For obvious reasons, this should be used for developement only.
-spec(reload/1 :: (Module :: atom()) -> {'ok', atom()} | {'error', any()}).
reload(Module) ->
	reload(Module, soft).

%% @doc Does a code reload for the given module.  if Mode is `soft' a soft purge
%% is done; if it's `hard' a straight purge is done.
-spec(reload/2 :: (Module :: atom(), Mode :: 'soft' | 'hard') -> {'ok', atom()} | {'error', any()}).
reload(Module, Mode) ->
	Purging = case Mode of
		soft ->
			code:soft_purge(Module);
		hard ->
			code:purge(Module)
	end,
	case Purging of
		false ->
			{error, {purging, Purging, Module}};
		true ->
			case code:load_file(Module) of
				{module, Module} ->
					{ok, Module};
				Else ->
					{error, {Purging, Module, Else}}
			end
	end.

%% @doc Reloads code for all modules in the OpenACD path using soft purge.
-spec(reload_all/0 :: () -> 'ok' | {'error', any()}).
reload_all() ->
	reload_all(soft).

-ifdef(TEST).
-define(BEAM_DIR, "OpenACD/debug_ebin").
-else.
-define(BEAM_DIR, "OpenACD/ebin").
-endif.

%% @doc Reloads code for all modules using either the hard or soft method for 
%% purge, whichever `Mode' happens to be.
-spec(reload_all/1 :: (Mode :: 'hard' | 'soft') -> 'ok' | {'error', any()}).
reload_all(Mode) ->
	Modules = [M || {M, Path} <- code:all_loaded(), is_list(Path) andalso string:str(Path, ?BEAM_DIR) > 0],
	Out = [reload(M, Mode) || M <- Modules],
	case lists:all(fun ({ok, _E}) -> true; (_) -> false end, Out) of
		true ->
			ok;
		false ->
			Errors = [E || {Ok, E} <- Out, Ok == error],
			{error, Errors}
	end.

-spec(distribution/1 :: (Mean :: pos_integer()) -> float()).
distribution(Mean) ->
	(Mean)*math:log(1 - crypto:rand_uniform(1, 65535) / 65535) * -1.

%% @doc Generally only used in the 'dummy' collection, these allow multiple ways
%% to get a random (or not) number.
-spec(get_number/1 :: (N :: {'distribution', pos_integer()} | 'random' | {non_neg_integer(), pos_integer()} | pos_integer()) -> pos_integer()).
get_number({distribution, N}) ->
	trunc(distribution(N));
get_number(random) ->
	get_number({0, 2000});
get_number({Min, Max}) ->
	crypto:rand_uniform(Min, Max);
get_number(N) ->
	N.

-spec(find_first_arc/2 :: (Base :: string(), Ext :: string()) -> string()).
find_first_arc(Base, Ext) ->
	case file:read_file_info(Base ++ Ext) of
		{error, enoent} ->
			Base ++ Ext;
		_Else ->
			find_first_arc(Base, Ext, 2)
	end.

find_first_arc(Base, Ext, Count) ->
	CountList = integer_to_list(Count),
	Path = lists:append([Base, "-", CountList, Ext]),
	case file:read_file_info(Path) of
		{error, enoent} ->
			Path;
		_Else ->
			find_first_arc(Base, Ext, Count + 1)
	end.

-spec(floor/1 :: (X :: integer() | float()) -> integer()).
floor(X) ->
	T = trunc(X),
	if 
		X < T ->
			T - 1;
		true  -> 
			T
	end.

-spec(timemark/0 :: () -> 'ok').
timemark() ->
	timemark(default).

-spec(timemark/1 :: (Markname :: atom()) -> 'ok').
timemark(Markname) ->
	timemark(Markname, "").

-spec(timemark/2 :: (Markname :: atom(), Message :: string()) -> 'ok').
timemark(Markname, Message) ->
	case get({timemark, Markname}) of
		undefined ->
			put({timemark, Markname}, util:now()),
			io:format("*** timemark ~p created ***~n", [Markname]);
		Time ->
			Now = util:now(),
			Diff = Now - Time,
			put({timemark, Markname}, Now),
			io:format("*** timemark ~p ~s Duration: ~p~n", [Markname, Message, Diff])
	end,
	ok.

-spec(timemark_clear/0 :: () -> 'ok').
timemark_clear() ->
	timemark_clear(default).

-spec(timemark_clear/1 :: (Markname :: atom()) -> 'ok').
timemark_clear(Markname) ->
	io:format("*** timemark ~p cleared~n", [Markname]),
	erase({timemark, Markname}),
	ok.

-spec(priv_dir/0 :: () -> string() | {'error', 'enoent'}).
priv_dir() ->
	case code:priv_dir('OpenACD') of
		{error, _} ->
			case filelib:is_dir("priv") of
				true ->
					"priv";
				false ->
					{error, enoent}
			end;
		Dir ->
			Dir
	end.

-spec(priv_dir/1 :: (Suffix :: string()) -> string() | {'error', 'enoent'}).
priv_dir(Suffix) ->
	case priv_dir() of
		{error, _} = Error ->
			Error;
		Dir ->
			filename:join(Dir, Suffix)
	end.

run_dir() ->
	case os:getenv("OPENACD_RUN_DIR") of
		false ->
			".";
		Dir ->
			Dir
	end.

-ifdef(TEST).

code_reload_test_() ->
	% Using dummy_media because using util kills coverage reporting.
	[{spawn, [{"Standard reload", ?_assertEqual({ok, dummy_media}, reload(dummy_media))}]},
	{spawn, [{"Hard reload", ?_assertEqual({error, {purging, false, dummy_media}}, reload(dummy_media, hard))}]},
	{spawn, [{"Reloading everything", ?_assertEqual(ok, reload_all())}]}].

now_test() ->
	{Mega, Sec, _} = erlang:now(),
	Res = ?MODULE:now(),
	?assertEqual(Mega * 1000000 + Sec, Res).

split_test_() ->
	[{"splitting an empty string",
	fun() ->
		?assertEqual([], string_split("", " "))
	end},
	{"splitting by an empty string",
	fun() ->
		?assertEqual(["f", "o", "o"], string_split("foo", ""))
	end},
	{"splitting by an empty string with a limit",
	fun() ->
		?assertEqual(["f", "oo"], string_split("foo", "", 2))
	end},
	{"splitting a tring",
	fun() ->
		?assertEqual(["hello", "world", ":)"], string_split("hello world :)", " "))
	end},
	{"splitting a string with a limit",
	fun() ->
		?assertEqual(["hello", "world :)"], string_split("hello world :)", " ", 2))
	end},
	{"splitting an empty string with a limit",
	fun() ->
		?assertEqual([], string_split("", " ", 3))
	end},
	{"splitting by a string that's not in the string to be split",
	fun() ->	
		?assertEqual(["Hello world"], string_split("Hello world", "batman"))
	end},
	{"splitting by a string that's not in the string to be split with a limit",
	fun() ->
		?assertEqual(["Hello world"], string_split("Hello world", "batman", 3))
	end},
	{"splitting by an ambiguous pattern",
	fun() ->
		?assertEqual([[], ";abc", "de;f;g"], string_split(";;;abc;;de;f;g;;", ";;"))
	end}].

chomp_test() ->
	?assertEqual("Fooo", string_chomp("Fooo\r\n")).

string_interpolate_test_() ->
	[{"interpolate nominal", ?_assertEqual("word:  goober", string_interpolate("word:  #{word}", [{"word", "goober"}]))},
	{"interpolate missing", ?_assertEqual("word:  ", string_interpolate("word:  #{word}", []))},
	{"grab bag", ?_assertEqual("1:uno;  none:; under:untder", string_interpolate("1:#{one};  none:#{none}; under:#{un_der}", 
		[{"one", "uno"}, {"un_der", "untder"}]))},
	{"replacemnet no looping", ?_assertEqual("1:#{one}, 2:duo", string_interpolate("1:#{one}, 2:#{two}", 
		[{"one", "#{one}"}, {"two", "duo"}]))},
	{"Previous long replacements don't cause skips", ?_assertEqual("1:1234; 2:dot", string_interpolate("1:#{verylong}; 2:#{d}", [{"verylong", "1234"}, {"d", "dot"}]))}].

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
	?assertEqual([2, 4, 6, 8, 10], L2).

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
	%["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
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
					?assertMatch(exists, build_table(test_table, ?TEST_TABLE))
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
		},
		{
			"Whitelisting works",
			fun() ->
					?assertEqual([foo, {'_agent', "Steve"}, {'_brand', "Test 2"}], util:merge_skill_lists([{'_agent', "Fred"}, foo, {'_brand', "Test"}], [{'_agent', "Steve"}, foo, {'_brand', "Test 2"}], [])),
					?assertEqual([foo, {'_agent', "Steve"}, {'_brand', "Test"}, {'_brand', "Test 2"}], util:merge_skill_lists([{'_agent', "Fred"}, foo, {'_brand', "Test"}], [{'_agent', "Steve"}, foo, {'_brand', "Test 2"}], ['_brand']))
			end
		},
		{
			"Two white listed in the same lists",
			fun() ->
				List1 = [],
				List2 = [{'_queue', "q1"}, {'_queue', "q2"}],
				Expected = List2,
				Out = util:merge_skill_lists(List1, List2, ['_queue']),
				?assertEqual(Expected, Out)
			end
		},
		{
			"Three expanded magic skills whitelisted",
			fun() ->
				List1 = [], 
				List2 = [{'_queue', "q1"}, {'_queue', "q2"}, {'_queue', "q3"}],
				Expected = List2,
				Out = util:merge_skill_lists(List1, List2, ['_queue']),
				?assertEqual(Expected, Out)
			end
		},
		{
			"Three expanded magic skills not whitelisted",
			fun() ->
				List1 = [],
				List2 = [{'_queue', "q1"}, {'_queue', "q2"}, {'_queue', "q3"}],
				?assertError(badarg, util:merge_skill_lists(List1, List2, []))
			end
		}
	].

subtract_skill_lists_test_() ->
	[{"simple subtract",
	?_assertEqual([bar], util:subtract_skill_lists([foo, bar, baz], [foo, baz]))},
	{"subtract all given magics",
	?_assertEqual([bar], util:subtract_skill_lists([{'_queue', "Foo"}, {'_queue', "bar"}, bar], ['_queue']))},
	{"subtract specific expanded magic skills",
	?_assertEqual([bar, {'_queue', "baz"}], util:subtract_skill_lists([{'_queue', "foo"}, bar, {'_queue', "baz"}], [{'_queue', "foo"}]))},
	{"mixed expanded and unexpanded",
	?_assertEqual([{'_queue', "foo"}], util:subtract_skill_lists([{'_queue', "foo"}, {'_queue', "bar"}, {'_profile', "foo"}, {'_profile', "bar"}], [{'_queue', "bar"}, '_profile']))},
	{"attempt to remove a skill that doesn't exist",
	?_assertEqual([bar], util:subtract_skill_lists([bar, baz], [{'_agent', "Whatever"}, baz]))}].

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
