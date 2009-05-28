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

%% @doc When a new email connection is made, this is resposible for handling the
%% callbacks from gen_smtp_server.  When an email is completed / ready to be 
%% queued, this gen_server:cast's back to email_media_manager to create an
%% email_media and queue it.

-module(email_media_session).
-author(spicecsm).

-behaviour(gen_smtp_server_session).

%% gen_smtp callbacks
-export([
	init/3,
	handle_HELO/2,
	handle_EHLO/3,
	handle_MAIL/2,
	handle_MAIL_extension/2,
	handle_RCPT/2,
	handle_RCPT_extension/2,
	handle_DATA/5,
	handle_RSET/1,
	handle_VRFY/2,
	handle_other/3,
	terminate/2,
	code_change/3
]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("smtp.hrl").

-record(state, {
	mail_map :: #mail_map{}
}).

%% API

%% gen_smtp_server_session callbacks

init(Hostname, SessionCount, Address) when SessionCount > 20 ->
	?ERROR("Session limit exceeded at ~s by ~s", [Hostname, Address]),
	{stop, normal, io_lib:format("421 ~s is too busy to accecpt mail right now", [Hostname])};
init(Hostname, SessionCount, Address) ->
	Banner = io_lib:format("~s ESMTP spice_telephony", [Hostname]),
	{ok, Banner, #state{}}.

handle_HELO(Hostname, State) ->
	{ok, State}.

handle_EHLO(Hostname, Extensions, State) ->
	{ok, Extensions, State}.

handle_MAIL(From, State) ->
	{ok, State}.
	
handle_MAIL_extension(Extension, State) ->
	{ok, State}.

handle_RCPT(To, #state{mail_map = undefined} = State) ->
	F = fun() ->
		mnesia:read({mail_map, To})
	end,
	case mnesia:transaction(F) of
		{atomic, []} ->
			?WARNING("Could not find mapping for mail to ~s", [To]),
			{ok, State#state{mail_map = #mail_map{address = To}}};
		{atomic, [Mailmap]} ->
			{ok, State#state{mail_map = Mailmap}}
	end;
handle_RCPT(To, State) ->
	{error, "452 only one recipient, fool!", State}.

handle_RCPT_extension(Extension, State) ->
	{ok, State}.

handle_DATA(From, [To | _Allelse], Headers, Data, #state{mail_map = Mailmap} = State) when To =:= Mailmap#mail_map.address ->
	Reference = io_lib:format("~p", [make_ref()]),
	?INFO("headers:  ~p", [Headers]),
	gen_server:call(email_media_manager, {queue, Mailmap, Headers, Data}),
	%mimemail:decode(Headers, Data),
	{ok, Reference, State#state{mail_map = undefined}}.

handle_RSET(State) ->
	% reset any relevant internal state
	State#state{mail_map = undefined}.

handle_VRFY(Address, State) ->
	F = fun() ->
		mnesia:read({mail_map, Address})
	end,
	case mnesia:transaction(F) of
		{atomic, []} ->
			?WARNING("Could not find mapping for mail to ~s", [Address]),
			{error, "252 unabled to verify address, will be put in default queue", State};
		{atomic, [Mailmap]} ->
			{ok, io_lib:format("250 will queue the mail to ~s", [Mailmap#mail_map.queue]), State}
	end.

handle_other(_Verb, _Args, State) ->
	{"500 Error: command not recognized", State}.

terminate(_Reason, _State) ->
	ok.

code_change(OldVsn, State, Extra) ->
	{ok, State}.
