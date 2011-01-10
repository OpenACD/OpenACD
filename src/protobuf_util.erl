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
	bins_to_netstring/1,
	netstring_to_bins/1
]).

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

client_to_protobuf(Client) ->
	#clientrecord{
		is_default = case Client#client.id of undefined -> true; _ -> false end,
		name = Client#client.label,
		id = Client#client.id,
		options = proplist_to_protobuf(Client#client.options)
	}.

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
	proplist_to_protobuf(Tail, [Rec, Acc]).

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

statename_to_enum(Statename) ->
	case Statename of
		idle -> 'IDLE';
		ringing -> 'RINGING';
		precall -> 'PRECALL';
		oncall -> 'ONCALL';
		outgoing -> 'OUTGOING';
		released -> 'RELEASED';
		warm_transfer -> 'WARMTRANSFER';
		wrapup -> 'WRAPUP';
		_ -> undefined
	end.

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

skill_to_protobuf({Atom, Expanded}) when is_list(Expanded) ->
	#skill{atom = atom_to_list(Atom), expanded = Expanded};
skill_to_protobuf(Atom) when is_atom(Atom) ->
	#skill{atom = atom_to_list(Atom)}.

bin_to_netstring(Bin) ->
	Size = list_to_binary(integer_to_list(size(Bin))),
	<<Size/binary, $:, Bin/binary, $,>>.

bins_to_netstring(Bins) ->
	list_to_binary([bin_to_netstring(X) || X <- Bins]).

%% @doc Gather the binaries separated by netstrings.  Returns left over
%% binary and the binaries extracted.
-spec(netstring_to_bins/1 :: (Bin :: binary()) -> {binary(), [binary()]}).
netstring_to_bins(Bin) ->
	netstring_to_bins(Bin, [], []).

netstring_to_bins(<<>>, NumberAcc, Acc) ->
	Rest = list_to_binary(lists:reverse(NumberAcc)),
	{Rest, lists:reverse(Acc)};
netstring_to_bins(<<$:, Rest/binary>>, NumberAcc, Acc) ->
	Size = list_to_integer(lists:reverse(NumberAcc)),
	case Rest of
		<<NetBin:Size/binary, $,, NewRest/binary>> ->
			netstring_to_bins(NewRest, [], [NetBin | Acc]);
		_ ->
			Top = list_to_binary(lists:reverse([$: | NumberAcc])),
			{<<Top/binary, Rest/binary>>, lists:reverse(Acc)}
	end;
netstring_to_bins(<<N/integer, Rest/binary>>, NumAcc, Acc) ->
	netstring_to_bins(Rest, [N | NumAcc], Acc).

-ifdef(TEST).
bin_to_netstring_test_() ->
	[?_assertEqual(<<"3:hi!,">>, bin_to_netstring(<<"hi!">>)),
	?_assertEqual(<<"6:3:hi!,,">>, bin_to_netstring(<<"3:hi!,">>))].

bins_to_netstring_test_() ->
	[?_assertEqual(<<"3:hi!,3:hi!,">>, bins_to_netstring([<<"hi!">>, <<"hi!">>])),
	?_assertEqual(<<"6:3:hi!,,3:hi!,">>, bins_to_netstring([<<"3:hi!,">>, <<"hi!">>]))].

netstring_to_bins_test_() ->
	[?_assertEqual({<<>>, [<<"hi!">>]}, netstring_to_bins(<<"3:hi!,">>)),
	?_assertEqual({<<>>, [<<"3:hi!,">>]}, netstring_to_bins(<<"6:3:hi!,,">>)),
	?_assertEqual({<<>>, [<<"hi!">>, <<"3:hi!,">>]}, netstring_to_bins(<<"3:hi!,6:3:hi!,,">>)),
	?_assertEqual({<<"123:rest">>, [<<"hi!">>]}, netstring_to_bins(<<"3:hi!,123:rest">>)),
	?_assertEqual({<<"1">>, [<<"hi!">>]}, netstring_to_bins(<<"3:hi!,1">>))].

-endif.
