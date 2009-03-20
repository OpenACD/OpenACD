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
-export([handler/1, encode_trap/1]).

%% @doc Attempts to change the record into data for a client
handler(Rec) when is_record(Rec, agent_auth) ->
	{struct, [{login, list_to_binary(Rec#agent_auth.login)},
		{skills, Rec#agent_auth.skills},
		{securitylevel, Rec#agent_auth.securitylevel}]};
handler(Rec) when is_record(Rec, call) -> 
	{struct, [{id, list_to_binary(Rec#call.id)}, 
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

encode_trap(Data) -> 
	Encoder = mochijson2:encoder([{handler, fun(I) -> handler(I) end}]),
	try Encoder(Data) of
		Out ->
			{200, [], Out}
	catch
		_:_ ->
			{500, [], <<"Bad json term">>}
	end.

-ifdef(EUNIT).

call_record_id_test() -> 
	Idtest = #call{id="Idtest", type=voice, callerid="callerid", source=self(), client=#client{tenant=1, brand=2, label="tenantlabel"}},
	Idtestj = handler(Idtest),
	{struct, [{id, T} | _Rest]} = Idtestj,
	?assertMatch(<<"Idtest">>, T).
	
call_record_chat_test() -> 
	Chattype = #call{id="Idtest", type=chat, callerid="callerid", source=self(), client=#client{tenant=1, brand=2, label="tenantlabel"}},
	Chattypej = handler(Chattype),
	{struct, [_, {type, Type}, _, _]} = Chattypej,
	?assertMatch(chat, Type).
	
call_record_email_test() -> 
	Emailtype = #call{id="Idtest", type=email, callerid="callerid", source=self(), client=#client{tenant=1, brand=2, label="tenantlabel"}},
	Emailtypej = handler(Emailtype),
	{struct, [_, {type, Type}, _, _]} = Emailtypej,
	?assertMatch(email, Type).
	
client_record_test() -> 
	Rec = #client{tenant = 55, brand = 74, label = "Testlabel"},
	Coded = handler(Rec),
	?assertMatch({struct, [{tenant, 55}, {brand, 74}, {label, <<"Testlabel">>}]}, Coded).

agent_record_test() -> 
	Rec = #agent{login = "Testagent", state=idle},
	Coded = handler(Rec),
	?assertMatch({struct, [{login, <<"Testagent">>}, {state, idle}]}, Coded).


-endif.
