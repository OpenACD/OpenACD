%%%-------------------------------------------------------------------
%%% File          : cpx_json.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  10/31/08
%%%-------------------------------------------------------------------

%% @doc Massages data so that it is better suited for mochiweb's json2 output.
%% As stated in the comments for mochiwebjson2:
%% "mochijson2 works with binaries as strings, arrays as lists (without an {array, _})
%% wrapper and it only knows how to decode UTF-8 (and ASCII).
%% Therefore, this will attempt to muscle general data to fit that requirement when the mochiweb cannot.


-module(cpx_json).
-author("Micah").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("call.hrl").
-include("agent.hrl").

% API
-export([handler/1]).

%% @doc Attempts to change the record into data for a client
handler(Rec) when is_record(Rec, call) -> 
	{struct, [{id, Rec#call.id}, 
		{type, Rec#call.type}, 
		{callerid, list_to_binary(Rec#call.callerid)}, 
		{client, handler(Rec#call.client)}]};
handler(Rec) when is_record(Rec, client) -> 
	{struct, [{tenant, Rec#client.tenant},
		{brand, Rec#client.brand},
		{label, list_to_binary(Rec#client.label)}]};
handler(Rec) when is_record(Rec, agent) -> 
	{struct, [{login, list_to_binary(Rec#agent.login)},
		{state, Rec#agent.state}]}.