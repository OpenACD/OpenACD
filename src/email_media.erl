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
	start_link/2,
	start/3,
	start/2,
	get_disposition/1
]).

%% Web interface helper functions
-export([
	post_to_api/1
]).

%% gen_media callbacks
-export([
	init/1, 
	handle_call/4, 
	handle_cast/3, 
	handle_info/3,
	terminate/3, 
	code_change/4,
	handle_ring/3, 
	handle_answer/3, 
	handle_ring_stop/2,
	handle_agent_transfer/4,
	handle_queue_transfer/2,
	handle_wrapup/2
]).

-type(tref() :: any()).

-record(state, {
	initargs :: any(),
	mimed :: {string(), string(), [{string(), string()}], [{string(), string()}], any()},
	skeleton :: any(),
	file_map = [] :: [{string(), [pos_integer()]}],
	manager :: pid() | tref()
}).

-type(state() :: #state{}).
-define(GEN_MEDIA, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================
-spec(start/3 :: (Mailmap :: #mail_map{}, Headers :: [any()], Data :: string()) -> {'ok', pid()}).
start(Mailmap, Headers, Data) ->
	gen_media:start(?MODULE, [Mailmap, Headers, Data]).

-spec(start/2 :: (Mailmap :: #mail_map{}, Rawmessage :: string()) -> {'ok', pid()}).
start(Mailmap, Rawmessage) ->
	gen_media:start(?MODULE, [Mailmap, Rawmessage]).
	
-spec(start_link/3 :: (Mailmap :: #mail_map{}, Headers :: [any()], Data :: string()) -> {'ok', pid()}).
start_link(Mailmap, Headers, Data) ->
	gen_media:start(?MODULE, [Mailmap, Headers, Data]).

-spec(start_link/2 :: (Mailmap :: #mail_map{}, Rawmessage :: string()) -> {'ok', pid()}).
start_link(Mailmap, Rawmessage) ->
	gen_media:start_link(?MODULE, [Mailmap, Rawmessage]).

-spec(post_to_api/1 :: (Post :: [{string(), string()}]) -> {'cast', any()} | {'call', any()}).
post_to_api(Post) ->
	case proplists:get_value("command", Post) of
		"get_path" ->
			Path = proplists:get_value("path", Post, ""),
			Tokenpath = string:tokens(Path, [$.]),
			PathProper = lists:map(fun(Elem) -> list_to_integer(Elem) end, Tokenpath),
			{call, {get_path, PathProper}};
		"get_id" ->
			Id = proplists:get_value("id", Post),
			{call, {get_id, Id}};
		_Else ->
			none
	end.

-spec(get_disposition/1 :: (Mime :: tuple()) -> 'inline' | {'inline', string()} | {'attachment', string()}).
get_disposition({_, _, _, Properties, _}) ->
	Params = proplists:get_value(<<"disposition-params">>, Properties),
	case proplists:get_value(<<"disposition">>, Properties, inline) of
		inline ->
			inline;
		"inline" ->
			case proplists:get_value(<<"filename">>, Params) of
				undefined ->
					inline;
				Name ->
					{inline, Name}
			end;
		_Else ->
			case proplists:get_value(<<"filename">>, Params) of
				undefined ->
					TypeParams = proplists:get_value(<<"content-type-params">>, Properties),
					case proplists:get_value(<<"name">>, TypeParams) of
						undefined ->
							Nom = util:bin_to_hexstr(erlang:md5(erlang:ref_to_list(make_ref()))),
							{attachment, Nom};
						Nom ->
							{attachment, Nom}
					end;
				Nom ->
					{attachment, Nom}
			end
	end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Args) ->
	process_flag(trap_exit, true),
	{_Type, _Subtype, Mheads, _Properties, _Body} = Mimed = case Args of
		[Mailmap, Rawmessage] ->
			mimemail:decode(Rawmessage);
		[Mailmap, Headers, Data] ->
			mimemail:decode(Headers, Data)
	end,
	{Skeleton, Files} = skeletonize(Mimed),
	Callerid = case proplists:get_value("From", Mheads) of
		undefined -> 
			"unknown";
		Else ->
			Else
	end,
	?DEBUG("callerid:  ~s", [Callerid]),
	Ref = erlang:ref_to_list(make_ref()),
	Refstr = util:bin_to_hexstr(erlang:md5(Ref)),
	[Domain, _To] = util:string_split(lists:reverse(Mailmap#mail_map.address), "@", 2),
	Defaultid = lists:flatten(io_lib:format("~s@~s", [Refstr, lists:reverse(Domain)])),
	Proto = #call{
		id = proplists:get_value("Message-ID", Mheads, Defaultid), 
		type = email,
		callerid = Callerid,
		client = Mailmap#mail_map.client,
		skills = Mailmap#mail_map.skills,
		ring_path = inband,
		media_path = inband,
		source = self()
	},
	{ok, {#state{initargs = Args, skeleton = Skeleton, file_map = Files, mimed = Mimed, manager = whereis(email_media_manager)}, {Mailmap#mail_map.queue, Proto}}}.
	
	
%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------

handle_call(get_init, _From, _Callrec, State) ->
	{reply, State#state.initargs, State};
handle_call({get_path, Path}, _From, _Callrec, #state{mimed = Mime} = State) when is_list(Path) ->
	Reply = get_part(Path, Mime),
	{reply, Reply, State};
handle_call({get_id, Id}, From, Callrec, #state{file_map = Map} = State) when is_list(Id) ->
	case proplists:get_value(Id, Map) of
		undefined ->
			{reply, none, State};
		Path ->
			?DEBUG("path:  ~p", [Path]),
			handle_call({get_path, Path}, From, Callrec, State)
	end;
handle_call({get_blind, Key}, _From, Callrec, #state{file_map = Map, mimed = Mime} = State) when is_list(Key) ->
	case proplists:get_value(Key, Map) of
		undefined ->
			Splitpath = util:string_split(Key, "/"),
			Intpath = lists:map(fun(E) -> list_to_integer(E) end, Splitpath),
			Out = get_part(Intpath, Mime),
			{reply, Out, State};
		Path ->
			Reply = get_part(Path, Mime),
			{reply, Reply, State}
	end;
handle_call({mediapush, _Data}, _From, _Callrec, State) ->
	?WARNING("pushing data out is NYI", []),
	{reply, invalid, State};
handle_call(dump, _From, _Callrec, State) ->
	{reply, State, State};
	
%% now the web calls.
handle_call({"get_skeleton", undefined}, _From, _Callrec, State) ->
	{reply, State#state.skeleton, State};
handle_call({"get_path", Path}, _From, _Callrec, #state{mimed = Mime} = State) ->
	Splitpath = util:string_split(Path, "/"),
	Intpath = lists:map(fun(E) -> list_to_integer(E) end, Splitpath),
	Out = get_part(Intpath, Mime),
	{reply, Out, State};
	
%% and anything else
handle_call(Msg, _From, _Callrec, State) ->
	?INFO("unhandled mesage ~p", [Msg]),
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_cast(_Msg, _Callrec, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_info(check_manager, _Callrec, State) ->
	case whereis(email_media_manager) of
		Pid when is_pid(Pid) ->
			link(Pid),
			{noreply, State#state{manager = Pid}};
		_Else ->
			{ok, Tref} = timer:send_after(1000, check_manager),
			{noreply, State#state{manager = Tref}}
	end;
handle_info({'EXIT', Pid, Reason}, _Callrec, #state{manager = Pid} = State) ->
	?WARNING("Handling media manager ~w death of ~p", [Pid, Reason]),
	{ok, Tref} = timer:send_after(1000, check_manager),
	{noreply, State#state{manager = Tref}};
handle_info(Info, _Callrec, State) ->
	?DEBUG("Info: ~p", [Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(_Reason, _Callrec, _State) ->
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, _Callrec, State, _Extra) ->
	{ok, State}.

%% gen_media specific callbacks
handle_answer(Agent, Call, State) ->
	%?DEBUG("Shoving ~w to the agent ~w", [State#state.html, Agent]),
	agent:conn_cast(Agent, {mediaload, Call}),
	{ok, State}.

handle_ring(_Agent, _Call, State) ->
	{ok, State}.

handle_voicemail(_Whatever, _Callrec, State) ->
	{invalid, State}.

handle_agent_transfer(_Agent, _Timeout, _Callrec, State) ->
	{ok, State}.

handle_queue_transfer(_Callrec, State) ->
	{ok, State}.

handle_ring_stop(_Callrec, State) ->
	{ok, State}.

handle_wrapup(_Callrec, State) ->
	{hangup, State}.
	
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

% -type(display_type() :: 'link' | 'html' | 'text').
% -type(mail_display() :: [{display_type(), any()}]).

skeletonize(Mime) ->
	%% skeletonize(Mime, Path, Filemap) -> {Skeleton, Filemap}
	skeletonize(Mime, [], []).

skeletonize({<<"multipart">>, Subtype, Headers, Properties, List}, Path, Files) ->
	{Subskel, Newfiles} = skeletonize(List, [1 | Path], Files),
	{{<<"multipart">>, Subtype, Headers, Properties, Subskel}, Newfiles};
skeletonize({<<"message">>, Subtype, Headers, Properties, Body}, Path, Files) ->
	{Subskel, Midfiles} = skeletonize([Body], [1 | Path], Files),
	Newfiles = append_files(Headers, Properties, Path, Midfiles),
	{{<<"message">>, Subtype, Headers, Properties, Subskel}, Newfiles};
skeletonize({Type, Subtype, Headers, Properties, _Body}, Path, Files) ->
	Newfiles = append_files(Headers, Properties, Path, Files),
	{{Type, Subtype, Headers, Properties}, Newfiles};
skeletonize(List, Path, Files) when is_list(List) ->
	skeletonize(List, Path, Files, []).

skeletonize([], Path, Files, Acc) ->
	{lists:reverse(Acc), Files};
skeletonize([{<<"multipart">>, Subtype, Headers, Properties, List} | Tail], [Count | Ptail] = Path, Files, Acc) ->
	{Sublist, Newfiles} = skeletonize(List, [1 | Path], Files),
	Newacc = [{<<"multipart">>, Subtype, Headers, Properties, Sublist} | Acc],
	skeletonize(Tail, [Count + 1 | Ptail], Newfiles, Newacc);
skeletonize([{<<"message">>, Subtype, Headers, Properties, Body} | Tail], [Count | Ptail] = Path, Files, Acc) ->
	{Sublist, Midfiles} = skeletonize([Body], [1 | Path], Files),
	Newacc = [{<<"message">>, Subtype, Headers, Properties, Sublist} | Acc],
	Newfiles = append_files(Headers, Properties, Path, Midfiles),
	skeletonize(Tail, [Count + 1 | Ptail], Newfiles, Newacc);
skeletonize([Head | Tail], [Count | Ptail] = Path, Files, Acc) ->
	{Skel, Newfiles} = skeletonize(Head, Path, Files),
	Newacc = [{element(1, Head), element(2, Head), element(3, Head), element(4, Head)} | Acc],
	skeletonize(Tail, [Count + 1 | Ptail], Newfiles, Newacc).

get_part([], {<<"multipart">>, Subtype, _Headers, _Properties, List}) ->
	?DEBUG("[], \"multipart\"", []),
	{multipart, List};
get_part([], {<<"message">>, Subtype, _Headers, _Properties, Body}) ->
	?DEBUG("[], \"message\"", []),
	{message, Body};
get_part([], Mime) ->
	?DEBUG("[], ~p/~p", [element(1, Mime), element(2, Mime)]),
	{ok, Mime};
get_part([Child | Tail], {<<"multipart">>, Subtype, _Headers, _Properties, List}) when Child =< length(List) ->
	?DEBUG("[~p | ~p], multipart/~p", [Child, Tail, Subtype]),
	Part = lists:nth(Child, List),
	get_part(Tail, Part);
get_part([1 | Tail], {<<"message">>, Subtype, _Headers, _Properties, Body}) ->
	?DEBUG("[1 | ~p], message/~p", [Tail, Subtype]),
	get_part(Tail, Body);
get_part(Path, Mime) ->
	?INFO("Invalid path ~p.  ~p/~p", [Path, element(1, Mime), element(2, Mime)]),
	none.

check_disposition(Properties) ->
	case get_disposition({1, 1, 1, Properties, 1}) of
		inline ->
			inline;
		{inline, Name} ->
			{inline, Name, [{<<"Content-Disposition">>, list_to_binary(lists:flatten(io_lib:format("inline; filename=\"~s\"", [Name])))}]};
		{attachment, Name} ->
			{attachment, Name, [{<<"Content-Disposition">>, list_to_binary(lists:flatten(io_lib:format("attachment; filename=\"~s\"", [Name])))}]}
	end.

append_files(Headers, Properties, Path, Files) ->
	?DEBUG("append_files:  ~p, ~p", [Headers, Properties]),
	case {proplists:get_value(<<"Content-ID">>, Headers), get_disposition({1, 1, 1, Properties, 1})} of
		{undefined, inline} ->
			Files;
		{undefined, {_Linedness, Name}} ->
			[{Name, lists:reverse(Path)} | Files];
		{Contentidbin, Dispo} ->
			Contentid = binary_to_list(Contentidbin),
			Len = length(Contentid),
			Id = list_to_binary(lists:append(["cid:", string:sub_string(Contentid, 2, Len - 1)])),
			Midfiles = [{Id, lists:reverse(Path)} | Files],
			case Dispo of
				inline ->
					Midfiles;
				{_Linedness, Name} ->
					Fixedname = case is_binary(Name) of
						true -> Name;
						false -> list_to_binary(Name)
					end,
					[{Fixedname, lists:reverse(Path)} | Midfiles]
			end
	end.

-ifdef(EUNIT).

%parse_disposition_test_() ->
%	[{"Basic parse",
%	fun() ->
%		Res = parse_disposition("inline;filename=spice-logo.jpg"),
%		?assertEqual({inline, "spice-logo.jpg"}, Res)
%	end}].

check_disposition_test_() ->
	[{"The header exists",
	fun() ->
		Res = check_disposition([{<<"disposition">>,"inline"}, {<<"disposition-params">>, [{<<"filename">>, <<"spice-logo.jpg">>}]}]),
		?DEBUG("das res:  ~p", [Res]),
		?assertEqual({inline, <<"spice-logo.jpg">>, [{<<"Content-Disposition">>, <<"inline; filename=\"spice-logo.jpg\"">>}]}, Res)
	end},
	{"The header doesn't exists",
	fun() ->
		Res = check_disposition([]),
		?assertEqual(inline, Res)
	end}].
		
getmail(File) ->
	{ok, Bin} = file:read_file(string:concat("contrib/gen_smtp/testdata/", File)),
	%Email = binary_to_list(Bin),
	mimemail:decode(Bin).
	
skeletonize_test_() ->
	[{"Simple plain text mail",
	fun() ->
		Decoded = getmail("Plain-text-only.eml"),
		{Skel, []} = skeletonize(Decoded),
		?assertMatch({<<"text">>, <<"plain">>, _, _}, Skel)
	end},
	{"html text mail",
	fun() ->
		Decoded = getmail("html.eml"),
		{Skel, []} = skeletonize(Decoded),
		?DEBUG("Skel:  ~p", [Skel]),
		?assertMatch({<<"multipart">>, <<"alternative">>, _Head, _Props, [{<<"text">>, <<"plain">>, _H2, _P2}, {<<"text">>, <<"html">>, _H3, P3}]}, Skel)
	end},
	{"email with image",
	fun() ->
		Decoded = getmail("image-attachment-only.eml"),
		{Skel, Files} = skeletonize(Decoded),
		?assertMatch({<<"multipart">>, <<"mixed">>, _, _, [{<<"image">>, <<"jpeg">>, _, _}]}, Skel),
		?assertEqual([{<<"spice-logo.jpg">>, [1]}], Files)
	end},
	{"the gamut",
	fun() ->
		% multipart/alternative
		%	text/plain
		%	multipart/mixed
		%		text/html
		%		message/rf822
		%			multipart/mixed
		%				message/rfc822
		%					text/plain
		%		text/html
		%		message/rtc822
		%			text/plain
		%		text/html
		%		image/jpeg
		%		text/html
		%		text/rtf
		%		text/html
		Decoded = getmail("the-gamut.eml"),
		{Skel, Files} = skeletonize(Decoded),
		%{"multipart", "alternative", [
%			{"text", "plain"},
%			{"multipart", "mixed", [
%				{"text", "html"},
%				{"message", "rfc822", [
%					{"multipart", "mixed", [
%						{"message", "rfc822", [
%							{"text", "plain"}
%						]}
%					]}
%				]},
%				{"text", "html"},
%				{"message", "rfc822", [
%					{"text", "plain"}
%				]},
%				{"text", "html"},
%				{"image", "jpeg"},
%				{"text", "html"},
%				{"text", "rtf"},
%				{"text", "html"}
%			]}
%		]},
		?DEBUG("Skel:  ~p", [Skel]),
		?DEBUG("Files:  ~p", [Files]),
		?assertMatch(
		{<<"multipart">>, <<"alternative">>, _, _, [
			{<<"text">>, <<"plain">>, _, _},
			{<<"multipart">>, <<"mixed">>, _, _, [
				{<<"text">>, <<"html">>, _, _},
				{<<"message">>, <<"rfc822">>, _, _, [
					{<<"multipart">>, <<"mixed">>, _, _, [
						{<<"message">>, <<"rfc822">>, _, _, [
							{<<"text">>, <<"plain">>, _, _}
						]}
					]}
				]},
				{<<"text">>, <<"html">>, _, _},
				{<<"message">>, <<"rfc822">>, _, _, [
					{<<"text">>, <<"plain">>, _, _}
				]},
				{<<"text">>, <<"html">>, _, _},
				{<<"image">>, <<"jpeg">>, _, _},
				{<<"text">>, <<"html">>, _, _},
				{<<"text">>, <<"rtf">>, _, _},
				{<<"text">>, <<"html">>, _, _}
			]}
		]}, Skel),
		?assertEqual(5, length(Files)),
		?assertEqual([2, 2, 1, 1], proplists:get_value(<<"Plain text only">>, Files)),
		?assertEqual([2, 4], proplists:get_value(<<"Plain text only.eml">>, Files)),
		?assertEqual([2, 6], proplists:get_value(<<"spice-logo.jpg">>, Files)),
		?assertEqual([2, 8], proplists:get_value(<<"test.rtf">>, Files)),
		?assertEqual([2, 2], proplists:get_value(<<"message as attachment.eml">>, Files))
	end},
	{"Files with id's logged (testcase1)",
	fun() ->
		Decoded = getmail("testcase1"),
		{_Skel, Files} = skeletonize(Decoded),
		?assertEqual([2, 1, 2, 2], proplists:get_value(<<"cid:part1.03050108.02070304@gmail.com">>, Files)),
		?assertEqual([2, 1, 2, 2], proplists:get_value(<<"moz-screenshot-1.jpg">>, Files))
	end}].
	

% multipart/alternative []
%	text/plain [1]
%	multipart/mixed [2]
%		text/html [2, 1]
%		message/rf822 [2, 2]
%			multipart/mixed [2, 2, 1]
%				message/rfc822 [2, 2, 1, 1]
%					text/plain [2, 2, 1, 1, 1]
%		text/html [2, 3]
%		message/rtc822 [2, 4]
%			text/plain [2, 4, 1]
%		text/html [2, 5]
%		image/jpeg [2, 6]
%		text/html [2, 7]
%		text/rtf [2, 8]
%		text/html [2, 9]
get_part_test_() ->
	{setup,
	fun() ->
		getmail("the-gamut.eml")
	end,
	fun(Gamut) ->
		[{"A simple path",
		fun() ->
			Path = [1],
			?assertMatch({ok, {<<"text">>, <<"plain">>, _Head, _Prop, _Body}}, get_part(Path, Gamut))
		end},
		{"Getting the base", 
		fun() ->
			Path = [],
			?assertMatch({multipart, [Tuple1, Tuple2]}, get_part(Path, Gamut))
		end},
		{"Getting a multipart",
		fun() ->
			Path = [2],
			?assertMatch({multipart, List}, get_part(Path, Gamut))
		end},
		{"Getting a message",
		fun() ->
			Path = [2, 2],
			?assertMatch({message, {<<"multipart">>, <<"mixed">>, _Headers, _Properties, _List}}, get_part(Path, Gamut))
		end},
		{"Getting below a message",
		fun() ->
			Path = [2, 2, 1],
			?assertMatch({multipart, [{<<"message">>, <<"rfc822">>, _Headers, _Properties, _Body}]}, get_part(Path, Gamut))
		end},
		{"Getting a deep text",
		fun() ->
			Path = [2, 2, 1, 1, 1],
			?assertMatch({ok, {<<"text">>, <<"plain">>, _Headers, _Properties, _Body}}, get_part(Path, Gamut))
		end},
		{"getting a far away text",
		fun() ->
			Path = [2, 9],
			?assertMatch({ok, {<<"text">>, <<"html">>, _Headers, _Properties, _Body}}, get_part(Path, Gamut))
		end},
		{"getting a path that doesn't exist",
		fun() ->
			Path = [299, 768, 124],
			?assertMatch(none, get_part(Path, Gamut))
		end},
		{"trying with testcase1",
		fun() ->
			Testcase1 = getmail("testcase1"),
			Path = [2, 1 , 2, 2],
			?assertMatch({ok, {<<"image">>, <<"jpeg">>, _Head, _Prop, _Body}}, get_part(Path, Testcase1))
		end}]
	end}.
		
-endif.
