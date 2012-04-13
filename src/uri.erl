%%% -------------------------------------------------------------------
%%% File    : uri.erl
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Erlang implementation of
%%%              javascript encodeURIComponent and decodeURIComponent.
%%%              Full unicode support.
%%%              Based on ECMA-262 15.1.3 and google V8 code.
%%% Created : 08.02.2011
%%% -------------------------------------------------------------------

-module(uri).

-export([encode_uri_component/1, decode_uri_component/1]).

-define(HEX_CHAR_CODES, [48, 49, 50, 51, 52, 53, 54, 55, 56, 57,
                         65, 66, 67, 68, 69, 70]).

%% @doc Encode string according to ECMA-262 15.1.3
-spec(encode_uri_component(string()) -> string()).

encode_uri_component(String) ->
    UnescapePred = fun(X) when X == $!;
                               X == 39; % '
                               X == $(;
                               X == $);
                               X == $*;
                               X == $-;
                               X == $.;
                               X == $_;
                               X == $~ ->
                           true;
                      (X) ->
                           is_alpha_num(X)
                   end,
                           
    encode(String, UnescapePred, []).

%% @doc Decode string encoded with encodeURIComponent
-spec(decode_uri_component(string()) -> string()).

decode_uri_component(String) ->
    decode(String, []).

%%% Private functions ----------------------------------------

is_alpha_num(Char) ->
    if
        Char >= $a andalso Char =< $z ->
            true;
        Char >= $A andalso Char =< $Z ->
            true;
        Char >= $0 andalso Char =< $9 ->
            true;
        true ->
            false
    end.

encode([], _, Acc) ->
    lists:flatten(lists:reverse(Acc));
encode([Char | T], Unescape, Acc) ->
    NewAcc = case Unescape(Char) of
                 true ->
                     [Char | Acc];
                 false ->
                     Code = if
                                Char >= 16#DC00 andalso Char =< 16#DFFF ->
                                    throw(uri_error);
                                true ->
                                    uri_encode(Char)
                            end,

                     [Code | Acc]
             end,
    
    encode(T, Unescape, NewAcc).

decode([], Acc) ->
    lists:flatten(lists:reverse(Acc));
decode([Char | T], Acc) ->
    {NewAcc, Next} = if
                         %% - '%' char
                         Char == 37 ->
                             {NChar, NT} = decode_char(T),
                             {[NChar | Acc], NT};
                         true ->
                             {[Char | Acc], T}
                     end,
    
    decode(Next, NewAcc).

decode_char([Char1, Char2 | T]) ->
    Code = uri_hex_chars_to_char_code(Char1, Char2),

    if
        (Code bsr 7) /= 0 ->
            N = calc_n(Code, 1),

            if
                N == 1 orelse N > 4 ->
                    throw(uri_error);
                true ->
                    {Octets, NT} = try
                                 {Codes, NewT} = decode_octets(T, N),
                                 {[Code] ++ Codes, NewT}
                             catch
                                 _:_ ->
                                     throw(uri_error)
                             end,

                    {uri_decode_octets(Octets), NT}
            end;
        true ->
            {Code, T}
    end;

decode_char(_) ->
    throw(uri_error).

decode_octets(Octets, N) ->
    {Res, X} = 
        lists:foldl(fun(_, {Acc, K}) ->
                            KK = K + 1,
                            Code = uri_hex_chars_to_char_code(
                                     lists:nth(KK + 1, Octets),
                                     lists:nth(KK + 2, Octets)),
                            
                            {[Code | Acc], KK + 2}
                    end, {[], 0}, lists:seq(1, N - 1)),

    {lists:reverse(Res), lists:nthtail(X, Octets)}.

calc_n(0, N) ->
    N;
calc_n(Code, NOld) ->
    N = NOld + 1,
    X = ((Code bsl N) band 16#80),

    calc_n(X, N).

uri_encode(Char) ->
    X = (Char bsr 12) band 15,
    Y = (Char bsr 6) band 63,
    Z = Char band 63,

    Octets = if
                 Char =< 16#007F ->
                     [Char, undefined, undefined];
                 Char =< 16#07FF ->
                     [Y + 192, Z + 128, undefined];
                 true ->
                     [X + 224, Y + 128, Z + 128]
             end,
    
    uri_encode_octets(Octets).
    
uri_encode_octets(Octets) ->
    lists:foldr(fun(undefined, Acc) ->
                        Acc;
                   (Octet, Acc) ->
                        [uri_add_encoded_octet(Octet) | Acc]
                end, [], Octets).

uri_add_encoded_octet(Octet) ->
    [37,
     lists:nth((Octet bsr 4) + 1, ?HEX_CHAR_CODES),
     lists:nth((Octet band 15) + 1, ?HEX_CHAR_CODES)
    ].

uri_hex_chars_to_char_code(Char1, Char2) ->
    Code1 = hex_value(Char1),
    Code2 = hex_value(Char2),

    if
        Code1 == undefined orelse Code2 == undefined ->
            throw(uri_error);
        true ->
            hex_str_to_char_code([Char1, Char2])
    end.

hex_value(Char) ->
    if
        Char >= $0 andalso Char =< $9 ->
            Char - 48;
        Char >= $A andalso Char =< $F ->
            Char - 55;
        Char >= $a andalso Char =< $f ->
            Char - 87;
        true ->
            undefined
    end.

hex_str_to_char_code(String) ->
    {Res, _} = lists:foldr(fun(Char, {R, M}) ->
                                   RNew = R + (hex_value(Char) bsl M),
                                   MNew = M + 4,

                                   {RNew, MNew}
                           end, {0, 0}, String),
    
    Res.

uri_decode_octets(Octets) ->
    [Oct0 | _] = Octets,

    Value = if
        Oct0 < 16#80 ->
            Oct0;
        Oct0 < 16#C2 ->
            throw(uri_error);
        true ->
            Oct1 = lists:nth(2, Octets),
            
            if
                Oct0 < 16#E0 ->
                    A = Oct0 band 16#1F,
                    
                    if
                        (Oct1 < 16#80);
                        (Oct1 > 16#BF) ->
                            throw(uri_error);
                        true ->
                            ok
                    end,

                    B = Oct1 band 16#3F,
                    Val = (A bsl 6) + B,
                    
                    if
                        Val < 16#80 orelse Val > 16#7FF ->
                            throw(uri_error);
                        true ->
                            Val
                    end;
                true ->
                    Oct1 = lists:nth(2, Octets),
                    Oct2 = lists:nth(3, Octets),
            
                    if
                        Oct0 < 16#F0 ->
                            A = Oct0 band 16#0F,
                            if
                                Oct1 < 16#80 orelse Oct1 > 16#BF ->
                                    throw(uri_error);
                                true ->
                                    ok
                            end,

                            B = Oct1 band 16#3F,

                            if
                                Oct2 < 16#80 orelse Oct2 > 16#BF ->
                                    throw(uri_error);
                                true ->
                                    ok
                            end,

                            C = Oct2 band 16#3F,

                            Val = (A bsl 12) + (B bsl 6) + C,

                            if
                                Val < 16#800 orelse Val > 16#ffff ->
                                    throw(uri_error);
                                true ->
                                    Val
                            end;
                        true ->
                            Oct1 = lists:nth(2, Octets),
                            Oct2 = lists:nth(3, Octets),
                            Oct3 = lists:nth(4, Octets),
            
                            if
                                Oct0 < 16#F8 ->
                                    A = (Oct0 band 16#07),
                    
                                    if
                                        Oct1 < 16#80 orelse Oct1 > 16#BF ->
                                            throw(uri_error);
                                        true ->
                                            ok
                                    end,
                    
                                    B = (Oct1 band 16#3F),
                    
                                    if
                                        Oct2 < 16#80 orelse Oct2 > 16#BF ->
                                            throw(uri_error);
                                        true ->
                                            ok
                                    end,

                                    C = (Oct2 band 16#3F),
                    
                                    if
                                        Oct3 < 16#80 orelse Oct3 > 16#BF ->
                                            throw(uri_error);
                                        true ->
                                            ok
                                    end,

                                    D = (Oct3 band 16#3F),
                                    Val = (A bsl 18) + (B bsl 12)
                                        + (C bsl 6) + D,

                                    if
                                        Val < 16#10000 ;Val > 16#10FFFF ->
                                            throw(uri_error);
                                        true ->
                                            Val
                                    end;
                                true ->
                                    throw(uri_error)
                            end
                    end
            end
    end,

    if
        Value < 16#10000 ->
            Value;
        true ->
            [(Value bsr 10) + 16#D7C0,
             (Value band 16#3FF) + 16#DC00]
    end.
