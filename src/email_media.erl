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
	html,
	files
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
	{Html, Files} = mime_to_html(Mimed),
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
	{ok, {#state{headers = Headers, data = Data, html = Html, files = Files}, {Mailmap#mail_map.queue, Proto}}}.
	
	
%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------

handle_call({get_file, Name}, _From, State) ->
	Out = proplists:get_value(Name, State#state.files),
	{reply, Out, State};
handle_call(get_agent_display, _From, State) ->
	{reply, State#state.html, State};
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

mime_to_html(Tuple) when is_tuple(Tuple) ->
	mime_to_html(Tuple, [], [], drop_none).

check_disposition(Headers) ->
	case proplists:get_value("Content-Disposition", Headers) of
		undefined ->
			inline;
		Text ->
			parse_disposition(Text)
	end.
						
parse_disposition(Text) ->
	[Disposition | Rest] = string:token(Text, ";"),
	Disatom = case Disposition of
		"inline" ->
			inline;
		_Else ->
			attachment
	end,
	F = fun(Str) ->
		[Key, Val] = util:string_split(Str, "=", 2),
		{Key, Val}
	end,
	Props = lists:map(F, Rest),
	case proplists:get_value("filename", Props) of
		undefined ->
			Disatom;
		Filename ->
			{Disatom, Filename}
	end.

append_file(Name, Headers, Content, []) ->
	{Name, [{Name, {Headers, Content}}]};
append_file(Name, Headers, Content, Files) ->
	case proplists:get_value(Name, Files) of
		undefined ->
			{Name, [{Name, {Headers, Content}} | Files]};
		{Headers, Content} ->
			{Name, Files};
		{_Other, _Data} ->
			Newname = name_loop(Name, Files, 1),
			{Newname, [{Newname, {Headers, Content}} | Files]}
	end.

mochi_parse_lower({Tag, Attr, Kids}) ->
	NewTag = list_to_binary(string:to_lower(binary_to_list(Tag))),
	Newkids = mochi_parse_lower(Kids, []),
	{NewTag, Attr, Newkids}.

mochi_parse_lower([], Acc) ->
	lists:reverse(Acc);
mochi_parse_lower([{Tag, Attr, []} | Rest], Acc) ->
	NewTag = list_to_binary(string:to_lower(binary_to_list(Tag))),
	mochi_parse_lower(Rest, [{NewTag, Attr, []} | Acc]);
mochi_parse_lower([{Tag, Attr, Kids} | Rest], Acc) ->
	Newkids = mochi_parse_lower(Kids, []),
	NewTag = list_to_binary(string:to_lower(binary_to_list(Tag))),
	mochi_parse_lower(Rest, [{NewTag, Attr, Newkids} | Acc]);
mochi_parse_lower([Bin | Rest], Acc) when is_binary(Bin) ->
	mochi_parse_lower(Rest, [Bin, Acc]).	

html_strip_heads(In) ->
	html_strip_heads(In, {undefined, []}).
	
html_strip_heads({<<"html">>, _Attr, Kids}, Acc)->
	?DEBUG("html tag, diving deeper", []),
	html_strip_heads(Kids, Acc);
html_strip_heads({Tag, Attr, Kids} = Tuple, Acc) ->
	html_strip_heads([Tuple], Acc);
html_strip_heads([{<<"head">>, _Attr, Kids} | Rest], {Acctitle, AccStyle}) ->
	?DEBUG("head tag, diving into then out", []),
	Folder = fun
		({<<"style">>, _, [Body]}, {Title, Style}) ->
			{Title, lists:concat([Style, binary_to_list(Body)])};
		({<<"title">>, _, [Body]}, {Title, Style}) ->
			{Body, Style};
		({_, _, _}, {Title, Style}) ->
			{Title, Style}
	end,
	{Title, Style} = lists:foldl(Folder, {Acctitle, []}, Kids),
	?DEBUG("Style thus far:  ~p", [Style]),
	html_strip_heads(Rest, {Title, lists:concat([AccStyle, Style])});
html_strip_heads([{<<"body">>, Attr, Kids} | Rest], {Title, AccStyle}) ->
	case proplists:get_value(<<"style">>, Attr) of
		undefined ->
			{Kids, Title, list_to_binary(AccStyle), undefined};
		Style ->
			{Kids, Title, list_to_binary(AccStyle), Style}
	end;
html_strip_heads(Nodes, {Title, AccStyle}) ->
	{Nodes, Title, list_to_binary(AccStyle), undefined}.

			
html_append(Html, Appendments) ->
	lists:flatten(io_lib:format("~s~s", [Html, Appendments])).

download_link(Href, Display) ->
	lists:flatten(io_lib:format("<span class=\"download\"><a href=\"mediapull/~s\" target=\"_blank\">~s</a></span>")).

name_loop(Name, Props, Count) ->
	Testname = string:concat(Name, integer_to_list(Count)),
	case proplists:get_value(Testname, Props) of
		undefined ->
			Testname;
		_Else ->
			name_loop(Name, Props, Count + 1)
	end.
			

mime_to_html({"multipart", _, _, _, []}, Html, Files, _Anyflag) ->
	?DEBUG("multipart complete", []),
	{Html, Files};
mime_to_html({"multipart", "alternative", Headers, Props, [Head | Tail]}, Html, Files, Anyflag) ->
	?DEBUG("multipart/alternative", []),
	{Newhtml, Newfiles} = mime_to_html(Head, Html, Files, drop_plain),
	mime_to_html({"multipart", "alternative", Headers, Props, Tail}, Newhtml, Newfiles, Anyflag);
mime_to_html({"text", "plain", _, _, _}, Html, Files, drop_plain) ->
	?DEBUG("dropping text/plain", []),
	{Html, Files};
mime_to_html({"text", "plain", Headers, _, Body}, Html, Files, _Anydrop) ->
	?DEBUG("text/plain", []),
	case check_disposition(Headers) of
		inline ->
			{lists:append([Html, "<pre>", Body, "</pre>"]), Files};
		{inline, Filename} ->
			{Newname, Newfiles} = append_file(Filename, Headers, Body, Files),
			Newhtml = html_append(Html, [download_link(Newname, Filename), Body]),
			{Newhtml, Newfiles};
		{attachment, Filename} ->
			{Newname, Newfiles} = append_file(Filename, Headers, Body, Files),
			Newhtml = html_append(Html, [download_link(Newname, Filename)]),
			{Newhtml, Newfiles}
	end;
mime_to_html({"text", "html", Headers, _, Body}, Html, Files, _Anydrop) ->
	?DEBUG("text/html", []),
	Parsed = mochiweb_html:parse(Body),
	Tolower = mochi_parse_lower(Parsed),
	{Stripped, Title, Styleelem, Styleattr} = html_strip_heads(Tolower),
	Titlespan = case Title of
		undefined -> 
			{<<"span">>, [], []};
		Title ->
			{<<"span">>, [{<<"class">>, <<"mimehtmltitle">>}], [Title]}
	end,
	Styled = case Styleelem of
		<<"">> ->
			<<"">>;
		Styleelem ->
			{<<"style">>, [], [Styleelem]}
	end,
	Outerdiv = case Styleattr of
		undefined ->
			{<<"div">>, [{<<"class">>, <<"mimehtmlbody">>}], lists:append([[Styled], [Titlespan], Stripped])};
		Styleattr ->
			{<<"div">>, [{<<"class">>, <<"mimehtmlbody">>}, {<<"style">>, Styleattr}], lists:append([[Styled], [Titlespan], Stripped])}
	end,
	Fixedbody = lists:flatten(io_lib:format("~s", [mochiweb_html:to_html(Outerdiv)])),
	case check_disposition(Headers) of
		inline ->
			Newhtml = lists:append(Html, Fixedbody),
			{Newhtml, Files};
		{inline, Filename} ->
			{Newname, Newfiles} = append_file(Filename, Headers, Body, Files),
			Newhtml = html_append(Html, [download_link(Newname, Filename), Fixedbody]),
			{Newhtml, Newfiles};
		{attachment, Filename} ->
			{Newname, Newfiles} = append_file(Filename, Headers, Body, Files),
			Newhtml = html_append(Html, [download_link(Newname, Filename)]),
			{Newhtml, Newfiles}
	end;
mime_to_html({"message", "rfc822", Headers, Properties, Body}, Html, Files, _Anydrop) ->
	?DEBUG("message/rfc822", []),
	Name = proplists:get_value("name", Properties, "unnamed"),
	{Newname, Newfiles} = append_file(Name, Headers, Body, Files),
	Newhtml = html_append(Html, [download_link(Newname, Name)]),
	{Newhtml, Newfiles};
mime_to_html({"image", _, Headers, Properties, Body}, Html, Files, _Anydrop) ->
	?DEBUG("image.  I didn't bother w/ the subtype.", []),
	case check_disposition(Headers) of
		{inline, Filename} ->
			{Newname, Newfiles} = append_file(Filename, Headers, Body, Files),
			Newhtml = html_append(Html, ["<img src=\"mediapull/", Newname, "\">"]),
			{Newhtml, Newfiles};
		{attachment, Filename} ->
			{Newname, Newfiles} = append_file(Filename, Headers, Body, Files),
			Newhtml = html_append(Html, [download_link(Newname, Filename)]),
			{Newhtml, Newfiles}
	end;
mime_to_html({_Type, _Subtype, Headers, Props, Body}, Html, Files, _Anydrop) ->
	?DEBUG("some other type and subtype.", []),
	Name = proplists:get_value("name", Props, "unnamed"),
	{Newname, Newfiles} = append_file(Name, Headers, Body, Files),
	Newhtml = html_append(Html, [download_link(Newname, Name)]),
	{Newhtml, Newfiles}.
	
-ifdef(EUNIT).

mochi_parse_lower_test_() ->
	[{"Simple lowercasing of empty element",
	fun() ->
		Input = {<<"DIV">>, [], []},
		Res = mochi_parse_lower(Input),
		?assertEqual({<<"div">>, [], []}, Res)
	end},
	{"nested lowercasing",
	fun() ->
		Input = {<<"DIV">>, [], [{<<"SPAN">>, [], []}]},
		Res = mochi_parse_lower(Input),
		?assertEqual({<<"div">>, [], [{<<"span">>, [], []}]}, Res)
	end},
	{"Mixed cases",
	fun() ->
		Input = {<<"SpAn">>, [], []},
		Res = mochi_parse_lower(Input),
		?assertEqual({<<"span">>, [], []}, Res)
	end},
	{"attributes are maintained",
	fun() ->
		Input = {<<"DIV">>, [{<<"style">>, <<"background:grey">>}], []},
		Res = mochi_parse_lower(Input),
		?assertEqual({<<"div">>, [{<<"style">>, <<"background:grey">>}], []}, Res)
	end}].

html_strip_heads_test_() ->
	[{"grab the bodies style attribute",
	fun() ->
		Input = [{<<"body">>, [{<<"style">>, <<"background-color:grey">>}], []}],
		Res = html_strip_heads(Input),
		?assertEqual({[], undefined, <<"">>, <<"background-color:grey">>}, Res)
	end},
	{"grabs styles down to the body tag",
	fun() ->
		Input = {<<"html">>, [], [
			{<<"head">>, [], [
				{<<"style">>, [], [<<"background-color:pink">>]}
			]},
			{<<"body">>, [{<<"style">>, <<"background-color:grey">>}], [
				{<<"div">>, [], []},
				{<<"div">>, [], []}
			]}
		]},
		Res = html_strip_heads(Input),
		?assertEqual({[{<<"div">>, [], []}, {<<"div">>, [], []}], undefined, <<"background-color:pink">>, <<"background-color:grey">>}, Res)
	end},
	{"grabs the title",
	fun() ->
		Input = {<<"html">>, [], [
			{<<"head">>, [], [
				{<<"title">>, [], [<<"title of the song">>]}
			]},
			{<<"body">>, [], [
				{<<"div">>, [], [<<"hi!">>]}
			]}
		]},
		Res = html_strip_heads(Input),
		?assertEqual({[{<<"div">>, [], [<<"hi!">>]}], <<"title of the song">>, <<"">>, undefined}, Res)
	end},
	{"jump straignt to below a body",
	fun() ->
		Input = {<<"div">>, [], [<<"hi!">>]},
		Res = html_strip_heads(Input),
		?assertEqual({[{<<"div">>, [], [<<"hi!">>]}], undefined, <<"">>, undefined}, Res)
	end}].

mime_to_html_test_() ->
	Getmail = fun(File) ->
		{ok, Bin} = file:read_file(string:concat("contrib/gen_smtp/testdata/", File)),
		Email = binary_to_list(Bin),
		mimemail:decode(Email)
	end,
	[{"Simple plain text mail",
	fun() ->
		Decoded = Getmail("Plain-text-only.eml"),
		?assertEqual({"<pre>This message contains only plain text.\r\n</pre>", []}, mime_to_html(Decoded))
%	end},
%	{"html text mail",
%	fun() ->
%		Decoded = Getmail("html.eml"),
%		?DEBUG("~p", [Decoded]),
%		{Html, Files} = mime_to_html(Decoded),
%		?assertEqual([], Files),
%		?assertEqual(<body style="word-wrap: break-word; -webkit-nbsp-mode: space; -webkit-line-break: after-white-space; "><ul class="MailOutline"><li>this</li><li>is</li><li>html</li></ul></body>
%		
%		
%		Res = loop_mail(Decoded, []),
%		?DEBUG("res:  ~p", [Res]),
%		Expected = [
%			{text,"this\r\nis\r\nhtml"},
%			{html,"<html><body style=\"word-wrap: break-word; -webkit-nbsp-mode: space; -webkit-line-break: after-white-space; \"><ul class=\"MailOutline\"><li>this</li><li>is</li><li>html</li></ul></body></html>"}
%		],
%		?assertEqual(Expected, Res)
%	end},
%	{"email with an image",
%	fun() ->
%		Decoded = Getmail("image-attachment-only.eml"),
%		?DEBUG("~p", [Decoded]),
%		Res = loop_mail(Decoded, []),
%		?DEBUG("res:  ~p", [Res]),
%		[Inside] = element(5, Decoded),
%		Expected = [{link, Inside}],
%		?assertEqual(Expected, Res)
%	end},
%	{"The gamut",
%	fun() ->
%		% multipart/alternative
%		%	text/plain
%		%	multipart/mixed
%		%		text/html
%		%		message/rf822
%		%			multipart/mixed
%		%				message/rfc822
%		%					text/plain
%		%		text/html
%		%		message/rtc822
%		%			text/plain
%		%		text/html
%		%		image/jpeg
%		%		text/html
%		%		text/rtf
%		%		text/html
%		Decoded = Getmail("the-gamut.eml"),
%		?DEBUG("decoded ~p:  ", [Decoded]),
%		Expected = [
%			{text, "text"},
%			{html, "html"},
%			{link, "message1"},
%			{link, "message2"},
%			{text, "text"},
%			{html, "html"},
%			{link, "message3"},
%			{text, "text"},
%			{html, "html"},
%			{link, "image"},
%			{html, "html"},
%			{link, "rtf"},
%			{html, "html"}
%		],
%		Res = loop_mail(Decoded),
%		?DEBUG("res:  ~p", [Res]),
%		?assertEqual(Expected, Res)
	end}].
	


-endif.
