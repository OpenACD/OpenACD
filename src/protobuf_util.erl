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

%% @doc Common protobuf conversion functions.  Most take an internal data 
%% structure and return a record used by the protobufs.

-module(protobuf_util).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("cpx_base_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
	call_to_protobuf/1,
	client_to_protobuf/1,
	proplist_to_protobuf/1,
	release_to_protobuf/1,
	statename_to_enum/1,
	enum_to_statename/1,
	skill_to_protobuf/1,
	bin_to_netstring/1,
	bin_to_netstring/2,
	bins_to_netstring/1,
	bins_to_netstring/2,
	netstring_to_bins/1,
	netstring_to_bins/2
]).

%% @doc Turn a non protobuf `#call{}' into a protobuf friendly 
%% `#callrecord{}'.
-spec(call_to_protobuf/1 :: (Call :: #call{}) -> #callrecord{}).
call_to_protobuf(Call) ->
	#callrecord{
		id = Call#call.id,
		type = atom_to_list(Call#call.type),
		caller_id = #callerid{
			name = element(1, Call#call.callerid),
			data = element(2, Call#call.callerid)
		},
		dnis = Call#call.dnis,
		client = client_to_protobuf(Call#call.client),
		ring_path = case Call#call.ring_path of
			outband -> 'OUTBAND_RING';
			inband -> 'INBAND_RING';
			any -> 'ANY_RING'
		end,
		media_path = case Call#call.media_path of
			outband -> 'OUTBAND_PATH';
			inband -> 'INBAND_PATH'
		end,
		direction = case Call#call.direction of
			inbound -> 'INBOUND';
			outbound -> 'OUTBOUND'
		end
	}.

%% @doc From `#client{}' to protobuf friendly `#clientrecord{}'.
-spec(client_to_protobuf/1 :: (Client :: #client{}) -> #clientrecord{}).
client_to_protobuf(Client) ->
	#clientrecord{
		is_default = case Client#client.id of undefined -> true; _ -> false end,
		name = Client#client.label,
		id = Client#client.id,
		options = proplist_to_protobuf(Client#client.options)
	}.

%% @doc Turns a simple proplist into a list of `#simplekeyvalue{}', which
%% is protobuf friendly.  The proplist can only have atoms, binaries, or
%% strings as the key.  The value can only be atoms, binary, or lists.  If
%% the value to anything else, it emits a warning and is skipped.
-type(simple_key() :: atom() | binary() | string()).
-type(simple_value() :: atom() | binary() | string()).
-type(simple_proplist() :: [{simple_key(), simple_value()}]).
-spec(proplist_to_protobuf/1 :: (List :: simple_proplist()) -> [#simplekeyvalue{}]).
proplist_to_protobuf(List) ->
	proplist_to_protobuf(List, []).

proplist_to_protobuf([], Acc) ->
	lists:reverse(Acc);
proplist_to_protobuf([{Key, Value} | Tail], Acc) ->
	FixedKey = if
		is_atom(Key) -> atom_to_list(Key);
		is_binary(Key) -> binary_to_list(Key);
		true -> Key
	end,
	FixedValue = if
		is_atom(Value) -> atom_to_list(Value);
		is_binary(Value) -> binary_to_list(Value);
		is_list(Value) -> Value;
		true ->
			?WARNING("Skipping ~p as value ~p is too complex", [Key, Value]),
			false
	end,
	case FixedValue of
		false ->
			proplist_to_protobuf(Tail, Acc);
		_ ->
			Rec = #simplekeyvalue{key = FixedKey, value = FixedValue},
			proplist_to_protobuf(Tail, [Rec | Acc])
	end;
proplist_to_protobuf([Key | Tail], Acc) when is_atom(Key) ->
	Rec = #simplekeyvalue{key = atom_to_list(Key), value = "true"},
	proplist_to_protobuf(Tail, [Rec | Acc]).

%% @doc Turn a release tuple or `#release_opt{}' into a protobuf friendly
%% `#release{}'.
-spec(release_to_protobuf/1 :: (Release :: 'default' | #release_opt{} | {string(), 'default' | string(), -1 | 0 | 1}) -> #release{}).
release_to_protobuf(default) ->
	release_to_protobuf({"default", default, 0});
release_to_protobuf(R) when is_record(R, release_opt) ->
	release_to_protobuf({R#release_opt.id, R#release_opt.label, R#release_opt.bias});
release_to_protobuf({Id, RawLabel, Bias}) ->
	Label = case RawLabel of default -> "Default"; _ -> RawLabel end,
	#release{
		id = Id,
		name = Label,
		bias = Bias
	}.

%% @doc Turns an agent statename into a protobuf enum.
-spec(statename_to_enum/1 :: (Statename :: atom()) -> atom()).
statename_to_enum(Statename) ->
	case Statename of
		idle -> 'IDLE';
		ringing -> 'RINGING';
		precall -> 'PRECALL';
		oncall -> 'ONCALL';
		outgoing -> 'OUTGOING';
		released -> 'RELEASED';
		warmtransfer -> 'WARMTRANSFER';
		wrapup -> 'WRAPUP';
		_ -> undefined
	end.

%% @doc Turns a protobuf enum of an agent statename into the internal atom.
-spec(enum_to_statename/1 :: (Enum :: atom()) -> atom()).
enum_to_statename(Enum) ->
	case Enum of
		'PRELOGIN' -> prelogin;
		'IDLE' -> idle;
		'RINGING' -> ringing;
		'PRECALL' -> precall;
		'ONCALL' -> oncall;
		'OUTGOING' -> outgoing;
		'RELEASED' -> released;
		'WARMTRANSFER' -> warmtransfer;
		'WRAPUP' -> wrapup;
		_ -> undefined
	end.

%% @doc Turn a skill tuple or atom into a protobuf friendly `#skill{}'.
-spec(skill_to_protobuf/1 :: 
	({Atom :: atom(), Expanded :: string()}) -> #skill{};
	(Atom :: atom()) -> #skill{}).
skill_to_protobuf({Atom, Expanded}) when is_list(Expanded) ->
	#skill{atom = atom_to_list(Atom), expanded = Expanded};
skill_to_protobuf(Atom) when is_atom(Atom) ->
	#skill{atom = atom_to_list(Atom)}.

%% @doc Wraps the given binary in a base 10 netstring.
-spec(bin_to_netstring/1 :: (Bin :: binary()) -> binary()).
bin_to_netstring(Bin) ->
	bin_to_netstring(Bin, 10).

%% @doc Wraps the given binary in a netstring using the given radix.
bin_to_netstring(Bin, Radix) ->
	Size = list_to_binary(erlang:integer_to_list(size(Bin), Radix)),
	<<Size/binary, $:, Bin/binary, $,>>.

%% @doc Produces a series of netstrings in one big bin using the default 
%% radix of 10.
-spec(bins_to_netstring/1 :: (Bins :: binary()) -> binary()).
bins_to_netstring(Bins) ->
	bins_to_netstring(Bins, 10).

%% @doc Produces a series of netstrings in one big bin using the given 
%% radix.
bins_to_netstring(Bins, Radix) ->
	list_to_binary([bin_to_netstring(X, Radix) || X <- Bins]).

%% @doc Gather the binaries separated by netstrings encoded using the radix
%% of 10.  Returns left over binary and the binaries extracted.
-spec(netstring_to_bins/1 :: (Bin :: binary()) -> {binary(), [binary()]}).
netstring_to_bins(Bin) ->
	netstring_to_bins(Bin, 10).

%% @doc Gather the binaries separated by netstrings encoded using the given
%% radix.  Returns left over binary and the binaries extracted.
netstring_to_bins(Bin, Radix) ->
	netstring_to_bins(Bin, Radix, [], []).

netstring_to_bins(<<>>, _Radix, NumberAcc, Acc) ->
	Rest = list_to_binary(lists:reverse(NumberAcc)),
	{Rest, lists:reverse(Acc)};
netstring_to_bins(<<$:, Rest/binary>>, Radix, NumberAcc, Acc) ->
	Size = erlang:list_to_integer(lists:reverse(NumberAcc), Radix),
	case Rest of
		<<NetBin:Size/binary, $,, NewRest/binary>> ->
			netstring_to_bins(NewRest, Radix, [], [NetBin | Acc]);
		_ ->
			Top = list_to_binary(lists:reverse([$: | NumberAcc])),
			{<<Top/binary, Rest/binary>>, lists:reverse(Acc)}
	end;
netstring_to_bins(<<N/integer, Rest/binary>>, Radix, NumAcc, Acc) ->
	netstring_to_bins(Rest, Radix, [N | NumAcc], Acc).

-ifdef(TEST).
bin_to_netstring_test_() ->
	[?_assertEqual(<<"3:hi!,">>, bin_to_netstring(<<"hi!">>)),
	?_assertEqual(<<"6:3:hi!,,">>, bin_to_netstring(<<"3:hi!,">>)), 
	?_assertEqual(<<"G:this is the life,">>, bin_to_netstring(<<"this is the life">>, 36))].

bins_to_netstring_test_() ->
	[?_assertEqual(<<"3:hi!,3:hi!,">>, bins_to_netstring([<<"hi!">>, <<"hi!">>])),
	?_assertEqual(<<"6:3:hi!,,3:hi!,">>, bins_to_netstring([<<"3:hi!,">>, <<"hi!">>])),
	?_assertEqual(<<"3:hi!,G:This is the life,">>, bins_to_netstring([<<"hi!">>, <<"This is the life">>], 36))].

netstring_to_bins_test_() ->
	[?_assertEqual({<<>>, [<<"hi!">>]}, netstring_to_bins(<<"3:hi!,">>)),
	?_assertEqual({<<>>, [<<"3:hi!,">>]}, netstring_to_bins(<<"6:3:hi!,,">>)),
	?_assertEqual({<<>>, [<<"hi!">>, <<"3:hi!,">>]}, netstring_to_bins(<<"3:hi!,6:3:hi!,,">>)),
	?_assertEqual({<<"123:rest">>, [<<"hi!">>]}, netstring_to_bins(<<"3:hi!,123:rest">>)),
	?_assertEqual({<<"1">>, [<<"hi!">>]}, netstring_to_bins(<<"3:hi!,1">>)),
	?_assertEqual({<<>>, [<<"this is the life">>]}, netstring_to_bins(<<"G:this is the life,">>, 36)),
	?_assertEqual({<<"1">>, [<<"this is the life">>]}, netstring_to_bins(<<"G:this is the life,1">>, 36)),
	?_assertEqual({<<"123:rest">>, [<<"this is the life">>, <<"hi!">>]}, netstring_to_bins(<<"G:this is the life,3:hi!,123:rest">>, 36))].

proplist_to_protobuf_test_() ->
	[{"list key, list val", fun() ->
		Expected = [#simplekeyvalue{key = "key", value= "value"}],
		In = [{"key", "value"}],
		?assertEqual(Expected, proplist_to_protobuf(In))
	end},
	{"binary key, binary val", fun() ->
		Expected = [#simplekeyvalue{key = "key", value="value"}],
		In = [{<<"key">>, <<"value">>}],
		?assertEqual(Expected, proplist_to_protobuf(In))
	end},
	{"atom key, no value", fun() ->
		Expected = [#simplekeyvalue{key = "key", value = "true"}],
		In = [key],
		?assertEqual(Expected, proplist_to_protobuf(In))
	end},
	{"atom key, complex value", fun() ->
		Expected = [],
		In = [{key, {"This", "is", "too", "complex"}}],
		?assertEqual(Expected, proplist_to_protobuf(In))
	end}].

skill_to_protobuf_test_() ->
	[?_assertEqual(#skill{atom = "skill1"}, skill_to_protobuf(skill1)),
	?_assertEqual(#skill{atom = "_skill1", expanded = "Expanded"}, skill_to_protobuf({'_skill1', "Expanded"}))].

statename_enum_translates_test_() ->
	TransTable = [
		{idle, 'IDLE'},
		{ringing, 'RINGING'},
		{precall, 'PRECALL'},
		{oncall, 'ONCALL'},
		{outgoing, 'OUTGOING'},
		{released, 'RELEASED'},
		{warmtransfer, 'WARMTRANSFER'},
		{wrapup, 'WRAPUP'}
	],
	Tests1 = [[?_assertEqual(A, enum_to_statename(B)),
	?_assertEqual(B, statename_to_enum(A))] ||
	{A, B} <- TransTable],
	Tests2 = [
	?_assertEqual(undefined, statename_to_enum(otherAtom)),
	?_assertEqual(prelogin, enum_to_statename('PRELOGIN')),
	?_assertEqual(undefined, enum_to_statename('OTHERENUM'))
	],
	lists:append(Tests1, Tests2).

release_to_protobuf_test_() ->
	[?_assertEqual(#release{id = "default", name = "Default", bias = 0}, release_to_protobuf(default)),
	?_assertEqual(#release{id = "id", name = "nom", bias = -1}, release_to_protobuf({"id", "nom", -1})),
	?_assertEqual(#release{id = "id", name = "nom", bias = -1}, release_to_protobuf(#release_opt{id = "id", label = "nom", bias = -1}))].

-endif.
