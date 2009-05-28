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

%% @doc A dummy media process designed to aid testing by mimicking a real call media process.

-module(email_media).
-author(spicecsm).

-behaviour(gen_media).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("stdlib/include/qlc.hrl").

-include("log.hrl").
-include("queue.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("smtp.hrl").

%% API
-export([
	start_link/3,
	start/3
]).

%% gen_media callbacks
-export([
	init/1, 
	handle_call/3, 
	handle_cast/2, 
	handle_info/2,
	terminate/2, 
	code_change/3,
	handle_ring/3, 
	handle_answer/3, 
	handle_voicemail/1, 
	handle_announce/2, 
	handle_ring_stop/1,
	handle_agent_transfer/4,
	handle_queue_transfer/1,
	handle_wrapup/1
]).

-record(state, {
	headers,
	data
}).

-type(state() :: #state{}).
-define(GEN_MEDIA, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================

start(Mailmap, Headers, Data) ->
	gen_media:start(?MODULE, [Mailmap, Headers, Data]).

start_link(Mailmap, Headers, Data) ->
	gen_media:start(?MODULE, [Mailmap, Headers, Data]).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Mailmap, Headers, Data]) ->
	Callerid = case proplists:get_value("From", Headers) of
		undefined -> 
			"unknown";
		Else ->
			Else
	end,
	?INFO("callerid:  ~s", [Callerid]),
	?INFO("headers:  ~p", [Headers]),
	Proto = #call{
		id = proplists:get_value("Message-ID", Headers), 
		type = email,
		callerid = Callerid,
		client = Mailmap#mail_map.client,
		skills = Mailmap#mail_map.skills,
		ring_path = inband,
		media_path = inband,
		source = self()
	},
	{ok, {#state{headers = Headers, data = Data}, {Mailmap#mail_map.queue, Proto}}}.
	
	
%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------

handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_info(Info, State) ->
	?DEBUG("Info: ~p", [Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% gen_media specific callbacks
handle_announce(_Annouce, State) ->
	{ok, State}.

handle_answer(_Agent, _Call, State) ->
	{ok, State}.

handle_ring(_Agent, _Call, State) ->
	{ok, State}.

handle_voicemail(State) ->
	{invalid, State}.

handle_agent_transfer(_Agent, _Call, _Timeout, State) ->
	{ok, State}.

handle_queue_transfer(State) ->
	{ok, State}.

handle_ring_stop(State) ->
	{ok, State}.

handle_wrapup(State) ->
	{hangup, State}.
	
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

