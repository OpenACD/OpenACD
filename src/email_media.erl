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
	data,
	foragent,
	links
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
	?DEBUG("callerid:  ~s", [Callerid]),
	?DEBUG("headers:  ~p", [Headers]),
	Mimed = mimemail:decode(Headers, Data),
	Looped = loop_mail(Mimed, []),
	{Foragent, Links} = clean_display(Looped, [], []),
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
	{ok, {#state{headers = Headers, data = Data, foragent = Foragent, links = Links}, {Mailmap#mail_map.queue, Proto}}}.
	
	
%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------

handle_call({get_linked, Name}, _From, State) ->
	Out = proplists:get_value(Name, State#state.links),
	{reply, Out, State};
handle_call(get_agent_display, _From, State) ->
	{reply, State#state.foragent, State};
handle_call(get_headers, _From, State) ->
	{reply, State#state.headers, State};
handle_call(get_data, _From, State) ->
	{reply, State#state.data, State};
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

-type(display_type() :: 'link' | 'html' | 'text').
-type(mail_display() :: [{display_type(), any()}]).

loop_mail({"multipart", _, _, _, []}, Acc) ->
	lists:reverse(Acc);
loop_mail({"multipart", _, _, _, [Head | Tail]}, Acc) ->
	Onto = loop_mail(Head, Acc),
	Newacc = lists:append(Onto, Acc),
	loop_mail(Tail, Newacc);
loop_mail({"text", "plain", _, _, Body}, Acc) ->
	[{text, Body} | Acc];
loop_mail({"text", "html", _, _, Body}, Acc) ->
	[{html, Body} | Acc];
loop_mail(Tuple, Acc) ->
	[{link, Tuple} | Acc].

clean_display([], Foragent, Links) ->
	{lists:reverse(Foragent), lists:reverse(Links)};
clean_display([{link, {_, _, Headers, Properties, Body} = Int} | Tail], Foragent, Links) ->
	Len = length(Links) + 1,
	Name = lists:flatten(io_lib:format("~B~s", [Len, proplists:get_value("name", Properties, "unnamed")])),
	clean_display(Tail, [{link, Name} | Foragent], [{Name, Int} | Links]);
clean_display([Head | Tail], Foragent, Links) ->
	clean_display(Tail, [Head | Foragent], Links).
	
-ifdef(EUNIT).

loop_mail_test_() ->
	Getmail = fun(File) ->
		{ok, Bin} = file:read_file(string:concat("contrib/gen_smtp/testdata/", File)),
		Email = binary_to_list(Bin),
		mimemail:decode(Email)
	end,
	[{"Simple plain text mail",
	fun() ->
		Decoded = Getmail("Plain-text-only.eml"),
		?assertEqual([{text, "This message contains only plain text.\r\n"}], loop_mail(Decoded, []))
%	end},
%	{"html text mail",
%	fun() ->
%		Decoded = Getmail("html.eml"),
%		?assertEqual([{html, "<html><body style=\"word-wrap: break-word; -webkit-nbsp-mode: space; -webkit-line-break: after-white-space; \"><ul class=\"MailOutline\"><li>this</li><li>is</li><li>html</li></ul></body></html>"}], loop_mail(Decoded, []))
	end}].
	


-endif.
