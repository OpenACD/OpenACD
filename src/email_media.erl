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

%% @doc Media process for handling email.  This parses and handles 
%% web requests from the agent, as well as sending a reply.
%% @see email_media_manager
%% @see gen_media

-module(email_media).
-author(openacd).

-behaviour(gen_media).

-define(DEFAULT_PRIORITY, 30).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("stdlib/include/qlc.hrl").
-include_lib("kernel/include/file.hrl").

-include("log.hrl").
-include("queue.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("smtp.hrl").

%% API
-export([
	start_link/1,
	start/1,
	get_disposition/1
]).

%% Web interface helper functions
-export([
	post_to_api/1
]).

%% gen_media callbacks
-export([
	init/1,
	prepare_endpoint/2,
	handle_call/4, 
	handle_cast/3, 
	handle_info/5,
	terminate/5, 
	code_change/4,
	handle_ring/4, 
	handle_answer/5, 
	handle_ring_stop/4,
	handle_agent_transfer/4,
	handle_queue_transfer/5,
	handle_wrapup/5,
	handle_spy/3,
	format_status/2
]).

-type(tref() :: any()).

-record(state, {
	send_args = [] :: any(),
	mimed :: {binary(), binary(), [{binary(), binary()}], [{binary(), binary()}], binary() | [{binary(), binary(), [{binary(), binary()}], [{binary(), binary()}], binary() | [tuple()]}] | tuple()},
	mail_map_address = "unknown@example.com" :: string(),
	skeleton :: any(),
	file_map = [] :: [{string(), [pos_integer()]}],
	manager :: pid() | tref(),
	outgoing_attachments = [] :: [{string(), binary()}],
	attachment_size = 0 :: integer(),
	sending_pid :: pid() | 'undefined',
	caseid :: string()
}).

-type(state() :: #state{}).
-define(GEN_MEDIA, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================

-type(mail_map_opt() :: {mail_map, #mail_map{}}).
-type(raw_message_opt() :: {raw, binary()}).
-type(send_opts() :: [any()]).
-type(start_opt() :: mail_map_opt() | raw_message_opt() | send_opts()).
-type(start_opts() :: [start_opt()]).
-spec(start/1 :: (Options :: start_opts()) -> {'ok', pid()} | {'error', any()}).
start(Options) ->
	gen_media:start(?MODULE, Options).

-spec(start_link/1 :: (Options :: start_opts()) -> {'ok', pid()} | {'error', any()}).
start_link(Options) ->
	gen_media:start_link(?MODULE, Options).

-spec(post_to_api/1 :: (Post :: [{string(), string()}]) -> {'cast', any()} | {'call', any()}).
post_to_api(Post) ->
	case proplists:get_value("command", Post) of
		"get_path" ->
			Path = proplists:get_value("path", Post, []),
			{call, {get_path, Path}};
		"get_id" ->
			Id = proplists:get_value("id", Post),
			{call, {get_id, Id}};
		_Else ->
			none
	end.

-spec(get_disposition/1 :: (Mime :: tuple()) -> 'inline' | {'inline', binary()} | {'attachment', binary()}).
get_disposition({_, _, _, Properties, _}) ->
	Params = proplists:get_value(<<"disposition-params">>, Properties),
	case proplists:get_value(<<"disposition">>, Properties, inline) of
		inline ->
			inline;
		<<"inline">> ->
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
							Nom = list_to_binary(util:bin_to_hexstr(erlang:md5(erlang:ref_to_list(make_ref())))),
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

init(Options) ->
	process_flag(trap_exit, true),
	Rawmessage = proplists:get_value(raw, Options),
	{_Type, _Subtype, Mheads, _Properties, _Body} = Mimed = try mimemail:decode(Rawmessage) of
		Result ->
			Result
	catch
		error:Why ->
			?WARNING("Failed to parse message with error: ~p", [Why]),
			{Headers, Body} = mimemail:parse_headers(Rawmessage),
			{<<"text">>, <<"plain">>, Headers, [],
				list_to_binary(["***This message failed to parse correctly, displaying it as plain text***\r\n\r\n", Body])}
	end,
	Mailmap = case proplists:get_value(mail_map, Options) of
		undefined ->
			case mimemail:get_header_value(<<"To">>, Mheads) of
				undefined ->
					#mail_map{address = "Undisclosed recipients"}; % TODO - this fails the 'RawDomain' parsing below!
				Address ->
					F = fun() ->
						QH = qlc:q([X || X <- mnesia:table(mail_map), X#mail_map.address =:= binary_to_list(Address)]),
						qlc:e(QH)
					end,
					case mnesia:transaction(F) of
						{atomic, []} ->
							#mail_map{address = binary_to_list(Address)};
						{atomic, [Map]} ->
							Map
					end
			end;
		Other ->
			Other
	end,
	{Skeleton, Files} = skeletonize(Mimed),
	Callerid = case mimemail:get_header_value(<<"From">>, Mheads) of
		undefined -> 
			{"Unknown", "Unknown"};
		Else ->
			case re:run(Else, "(?:\"*(.+?)\"* |)<([-a-zA-Z0-9._@]+)>", [{capture, all_but_first, list}]) of
				{match, [Name, Number]} ->
					{Name, Number};
				nomatch ->
					{"", binary_to_list(Else)}
			end
	end,
	Defaultid = case proplists:get_value(id, Options) of
		undefined ->
			Ref = erlang:ref_to_list(make_ref()),
			Refstr = util:bin_to_hexstr(erlang:md5(Ref)),
			[RawDomain, _To] = binstr:split(binstr:reverse(list_to_binary(Mailmap#mail_map.address)), <<"@">>, 2),
			Domain = case RawDomain of
				<<$>, Text/binary>> ->
					Text;
				_Else ->
					RawDomain
			end,
			lists:flatten(io_lib:format("~s@~s", [Refstr, binstr:reverse(Domain)]));
		Otherid ->
			Otherid
	end,
	CaseID = case re:run(mimemail:get_header_value(<<"Subject">>, Mheads, <<>>), "\\[Case:(\\d+)\\]", [{capture, all_but_first, list}]) of
		{match, [CID]} ->
			?DEBUG("CaseID for ~p is ~s", [Defaultid, CID]),
			CID;
		_ ->
			undefined
	end,
	Proto = #call{
		id = Defaultid,
		type = slow_text,
		callerid = Callerid,
		client = Mailmap#mail_map.client,
		skills = Mailmap#mail_map.skills,
		ring_path = inband,
		media_path = inband,
		priority = ?DEFAULT_PRIORITY, % TODO - allow this to be set via the address map?
		source = self()
	},
	?DEBUG("started ~p for callerid:  ~p", [Defaultid, Callerid]),
	[_, _ | FakingAncestors] = get('$ancestors'),
	put('$ancestors', FakingAncestors),
	{ok, {#state{
				send_args = proplists:get_value(send_opts, Options),
				skeleton = Skeleton,
				file_map = Files,
				mimed = Mimed,
				manager = whereis(email_media_manager),
				caseid = CaseID,
				mail_map_address = Mailmap#mail_map.address},
			{Mailmap#mail_map.queue, Proto}}}.

%%--------------------------------------------------------------------
%% prepare_endpoint
%%--------------------------------------------------------------------

prepare_endpoint(Agent, _Data) ->
	{ok, {module, dummy_media}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------

handle_call({get_path, Path}, _From, _Callrec, #state{mimed = Mime} = State) when is_list(Path) ->
	Reply = get_part(Path, Mime),
	{reply, Reply, State};
handle_call({get_id, Id}, From, Callrec, #state{file_map = Map} = State) when is_list(Id) ->
	case proplists:get_value(Id, Map) of
		undefined ->
			{reply, none, State};
		Path ->
			?DEBUG("path:  ~p (~p)", [Path, Callrec#call.id]),
			handle_call({get_path, Path}, From, Callrec, State)
	end;
handle_call({get_blind, Key}, _From, Callrec, #state{file_map = Map, mimed = Mime} = State) when is_list(Key) ->
	?DEBUG("get blind: ~p (~p)", [Key, Callrec#call.id]),
	case proplists:get_value(Key, Map) of
		undefined ->
			Splitpath = util:string_split(Key, "/"),
			Test = fun(I) ->
				try list_to_integer(I) of
					_Integer ->
						true
				catch
					error:badarg ->
						false
				end
			end,
			case lists:all(Test, Splitpath) of
				true ->
					Intpath = lists:map(fun(E) -> list_to_integer(E) end, Splitpath),
					Out = get_part(Intpath, Mime),
					{reply, Out, State};
				false when is_list(element(5, Mime)) ->
					% TODO better searching by content-location
					Out = get_part_by_content_location(element(5, Mime), list_to_binary(Key)),
					{reply, Out, State};
				false ->
					{reply, none, State}
			end;
		Path ->
			Reply = get_part(Path, Mime),
			{reply, Reply, State}
	end;
handle_call(dump, _From, _Callrec, State) ->
	{reply, State, State};
	
%% now the web calls.
handle_call({agent_web_connection, Command, Args}, _From, Callrec, State) ->
	handle_web_call(Command, Args, Callrec, State);

handle_call({peek, PeekingApid}, _From, Callrec, State) ->
	agent:conn_cast(PeekingApid, {mediaload, Callrec}),
	{reply, ok, State};
	
%% and anything else
handle_call(Msg, _From, Callrec, State) ->
	?INFO("unhandled mesage ~p (~p)", [Msg, Callrec#call.id]),
	{reply, invalid, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_cast({<<"send">>, Post}, Callrec, #state{sending_pid = undefined} = State) ->
	{struct, Args} = proplists:get_value("args", Post),
	?DEBUG("Starting Send (~p)", [Callrec#call.id]),
	[{_, BaseTo}, {_, From}, _] = BaseHeaders = [
		{<<"To">>, proplists:get_value(<<"to">>, Args)},
		{<<"From">>, proplists:get_value(<<"from">>, Args)},
		{<<"Subject">>, proplists:get_value(<<"subject">>, Args)}
	],
	{Bcc, BccHeader} = case proplists:get_value(<<"bcc">>, Args) of
		[<<>>] ->
			{[], []};
		BccElse when is_list(BccElse), length(BccElse) > 0 ->
			{BccElse, {<<"Bcc">>, binstr:join(BccElse, $,)}};
		_ ->
			{[], []}
	end,
	{Cc, CcHeader} = case proplists:get_value(<<"cc">>, Args) of
		[<<>>] ->
			{[], []};
		CcElse when is_list(CcElse), length(CcElse) > 0 ->
			{CcElse, {<<"Cc">>, binstr:join(CcElse, $,)}};
		_ ->
			{[], []}
	end,
	HtmlPosted = proplists:get_value(<<"body">>, Args),
	Plaintext = scrub_send_html(HtmlPosted),
	FirstBody = [
		{<<"text">>, <<"plain">>, [], [], Plaintext},
		{<<"text">>, <<"html">>, [], [], HtmlPosted}
	],
	{Type, Subtype, Body} = case length(State#state.outgoing_attachments) of
		0 ->
			{<<"multipart">>, <<"alternative">>, FirstBody};
		_Else ->
			Attachments = lists:map(
				fun({Name, Bin}) -> 
					{<<"application">>, 
					<<"octet-stram">>, 
					[
						{<<"Content-Disposition">>, list_to_binary([<<"attachment; filename=\"">>, Name, "\""])},
						{<<"Content-Type">>, list_to_binary([<<"application/octet-stream; name=\"">>, Name, "\""])},
						{<<"Content-Transfer-Encoding">>, <<"base64">>}
					],
					[],
					Bin}
				end, State#state.outgoing_attachments),
			Fullbody = [{<<"multipart">>, <<"alternative">>, [], [], FirstBody} | Attachments],
			{<<"multipart">>, <<"mixed">>, Fullbody}
	end,
	BinTo = lists:append([Bcc, [BaseTo], Cc]),
	To = lists:foldl(fun
		([], Acc) ->
			Acc;
		(Bin, Acc) ->
			case binary_to_list(Bin) of
				[] ->
					Acc;
				Binned ->
					[Binned | Acc]
			end
	end, [], BinTo),
	Headers = lists:flatten(lists:append([BaseHeaders, [BccHeader, CcHeader]])),
	% Other headers maybe:  in-reply-to and references (if exists)
	Fullout = {Type, Subtype, Headers, [], Body},
	?DEBUG("Starting encode (~p)", [Callrec#call.id]),
	Encoded = mimemail:encode(Fullout),
	?DEBUG("Encoding complete (~p)", [Callrec#call.id]),
	?DEBUG("Trying to send to ~p from ~p (~p)", [To, From, Callrec#call.id]),
	Mail = {
		binary_to_list(From),
		To,
		binary_to_list(Encoded)
	},
	{ok, Sendopts} = email_media_manager:get_send_opts(),
	case email_media_session:archive(Encoded, Callrec, outbound) of
		ok ->
			ok;
		{error, ArcErr} ->
			?WARNING("Could not write archive due to ~p (~p)", [ArcErr, Callrec#call.id]),
			ok
	end,

	case gen_smtp_client:send(Mail, Sendopts) of
		{ok, Pid} = ClientRes ->
			?DEBUG("Client res:  ~p;  Send opts:  ~p, From/To:  ~p (~p)", [ClientRes, Sendopts, {From, To}, Callrec#call.id]),
			{noreply, State#state{sending_pid = Pid}};
		ClientRes ->
			?DEBUG("Client res:  ~p;  Send opts:  ~p, From/To:  ~p (~p)", [ClientRes, Sendopts, {From, To}, Callrec#call.id]),
			{{mediapush, {send_fail, ClientRes}}, State}
	end;
%	{noreply, State};
%handle_cast({"send", _Post}, _Callrec, State) ->
handle_cast(print_file_map, _Callrec, #state{file_map = Out} = State) ->
	io:format("Printing filemap: ~n~p", [Out]),
	{noreply, State};
handle_cast({set_caseid, CaseID}, _Callrec, State) ->
	{noreply, State#state{caseid = CaseID}};
handle_cast(Msg, Callrec, State) ->
	?WARNING("cast msg:  ~p (~p)", [Msg, Callrec#call.id]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_info(check_manager, _Statename, _Callrec, _Gmstate, State) ->
	case whereis(email_media_manager) of
		Pid when is_pid(Pid) ->
			link(Pid),
			{noreply, State#state{manager = Pid}};
		_Else ->
			{ok, Tref} = timer:send_after(1000, check_manager),
			{noreply, State#state{manager = Tref}}
	end;

handle_info({'EXIT', Pid, normal}, _Statename, Callrec, _GmState, #state{sending_pid = Pid} = State) ->
	?DEBUG("sending complete (~p)", [Callrec#call.id]),
	{{mediapush, send_done}, State#state{sending_pid = undefined}};

handle_info({'EXIT', Pid, Notnormal}, _Statename, Callrec, _GmState, #state{sending_pid = Pid} = State) ->
	?INFO("Sending failed:  ~p (~p)", [Notnormal, Callrec#call.id]),
	{{mediapush, {send_fail, Notnormal}}, State#state{sending_pid = undefined}};

handle_info({'EXIT', Pid, Reason}, _Statename, Callrec, _GmState, #state{manager = Pid} = State) ->
	?WARNING("Handling media manager ~w death of ~p (~p)", [Pid, Reason, Callrec#call.id]),
	{ok, Tref} = timer:send_after(1000, check_manager),
	{noreply, State#state{manager = Tref}};

handle_info(Info, _Statename, Callrec, _GmState, State) ->
	?DEBUG("Info: ~p (~p)", [Info, Callrec#call.id]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(_Reason, _Statename, _Callrec, _GmState, _State) ->
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, _Callrec, State, _Extra) ->
	{ok, State}.

-spec(format_status/2 :: (Any :: any(), List :: [any()]) -> #state{}).
format_status(_, [_PDict, State]) ->
	% strip large data out of the state
	State#state{mimed = {}, skeleton = undefined, file_map = [], outgoing_attachments = []}.

%% gen_media specific callbacks
handle_answer(Agent, _Statename, Call, _Gmstate, State) ->
	%?DEBUG("Shoving ~w to the agent ~w", [State#state.html, Agent]),
	agent_channel:media_push(Agent, {mediaload, Call}),
	%agent_channel:conn_cast(Agent, {mediaload, Call}),
	{ok, State}.

handle_ring(_Agent, _RingData, _Call, #state{caseid = CaseID} = State) ->
	{ok, [{"caseid", CaseID}], State}.

handle_agent_transfer(_Agent, _Timeout, _Callrec, State) ->
	{ok, [{"caseid", State#state.caseid}], State}.

handle_queue_transfer(_Queue, _Statename, _Callrec, _Gmstate, State) ->
	{ok, State}.

handle_ring_stop(_Statename, _Callrec, _Gmstate, State) ->
	{ok, State}.

handle_wrapup(_From, _Statename, _Callrec, _Gmstate, State) ->
	{hangup, State}.

-spec(handle_spy/3 :: (Spy :: {pid(), #agent{}}, Callrec :: #call{}, State :: #state{}) -> 'ok').
handle_spy({Spy, _AgentRec}, Callrec, State) ->
	%agent:conn_cast(Spy, {mediaload, Callrec}),
	{ok, State}.
	
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

handle_web_call(<<"get_skeleton">>, _Post, _Callrec, State) ->
	{reply, State#state.skeleton, State};

handle_web_call(<<"get_path">>, Post, _Callrec, #state{mimed = Mime} = State) ->
	Path = lists:flatten(proplists:get_value("args", Post)),
	Out = get_part(Path, Mime),
	{reply, Out, State};

handle_web_call(<<"attach">>, Postdata, Callrec, #state{outgoing_attachments = Oldattachments} = State) ->
	case proplists:get_value("attachFiles", Postdata) of
		{_Name, Bin} = T ->
			case byte_size(Bin) + State#state.attachment_size of
				Toobig when Toobig > 5242880 ->
					%Size = State#state.attachment_size,
					{reply, {error, toobig}, State};
				Size ->
					Attachments = [T | Oldattachments],
					Keys = get_prop_keys(Attachments),
					?DEBUG("files list:  ~p (~p)", [Keys, Callrec#call.id]),
					{reply, {ok, Keys}, State#state{outgoing_attachments = Attachments, attachment_size = Size}}
			end;
		_Else ->
			?INFO("Uploading attempted with no file uploaded (~p)", [Callrec#call.id]),
			{reply, {error, nofile}, State}
	end;

handle_web_call(<<"detach">>, Postdata, _Callrec, #state{outgoing_attachments = Attachments} = State) ->
	case mochijson2:decode(proplists:get_value("arguments", Postdata, "false")) of
		false ->
			{reply, {error, badarg}, State};
		[Nth, Namebin] ->
			Name = binary_to_list(Namebin),
			case (Nth > length(Attachments)) of
				true ->
					{reply, {error, out_of_range, Nth}, State};
				false ->
					case lists:split(Nth - 1, Attachments) of
						{Toplist, [{Name, _Bin} | Tail]} ->
							Newattaches = lists:append(Toplist, Tail),
							%Newsize = State#state.attachment_size - byte_size(Bin),
							Keys = get_prop_keys(Newattaches),
							{reply, {ok, Keys}, State#state{outgoing_attachments = Newattaches}};
						{_, [{Gotname, _} | _]} ->
							{reply, {error, bad_name, Gotname}, State}
					end
			end
	end;

handle_web_call(<<"get_from">>, _Post, #call{client = Client}, State) ->
	#client{options = Options} = Client,
	case proplists:get_value(emailfrom, Options) of
		undefined ->
			{reply, undefined, State};
		{Label, Address} when is_atom(Label), is_atom(Address) ->
			{reply, undefined, State};
		{_Label, Address} when is_atom(Address) ->
			{reply, undefined, State};
		{Label, Address} ->
			FixedLabel = case {Label, Client#client.label} of
				{Label, undefined} when is_atom(Label) ->
					undefined;
				{Label, _} when is_list(Label) ->
					list_to_binary(Client#client.label);
				{Label, _} ->
					Label
			end,
			{reply, {FixedLabel, Address}, State}
	end.

%% @doc get the keys of a proplist, preserving order.
%% proplists:get_keys/1 does not have predictable order.
get_prop_keys(List) ->
	get_prop_keys(List, []).

get_prop_keys([], Acc) ->
	lists:reverse(Acc);
get_prop_keys([{Key, _Val} | Tail], Acc) ->
	get_prop_keys(Tail, [Key | Acc]).

% -type(display_type() :: 'link' | 'html' | 'text').
% -type(mail_display() :: [{display_type(), any()}]).

skeletonize(Mime) ->
	%% skeletonize(Mime, Path, Filemap) -> {Skeleton, Filemap}
	skeletonize(Mime, [], []).

skeletonize({<<"multipart">>, Subtype, Headers, Properties, List}, Path, Files) ->
	{Subskel, Newfiles} = skeletonize(List, [1 | Path], Files),
	{{<<"multipart">>, Subtype, downcase_headers(Headers), Properties, Subskel}, Newfiles};
skeletonize({<<"message">>, Subtype, Headers, Properties, Body}, Path, Files) ->
	{Subskel, Midfiles} = skeletonize([Body], [1 | Path], Files),
	Newfiles = append_files(Headers, Properties, Path, Midfiles),
	{{<<"message">>, Subtype, downcase_headers(Headers), Properties, Subskel}, Newfiles};
skeletonize({Type, Subtype, Headers, Properties, _Body}, Path, Files) ->
	Newfiles = append_files(Headers, Properties, Path, Files),
	{{Type, Subtype, downcase_headers(Headers), Properties}, Newfiles};
skeletonize(List, Path, Files) when is_list(List) ->
	skeletonize(List, Path, Files, []).

skeletonize([], _Path, Files, Acc) ->
	{lists:reverse(Acc), Files};
skeletonize([{<<"multipart">>, Subtype, Headers, Properties, List} | Tail], [Count | Ptail] = Path, Files, Acc) ->
	{Sublist, Newfiles} = skeletonize(List, [1 | Path], Files),
	Newacc = [{<<"multipart">>, Subtype, downcase_headers(Headers), Properties, Sublist} | Acc],
	skeletonize(Tail, [Count + 1 | Ptail], Newfiles, Newacc);
skeletonize([{<<"message">>, <<"rfc822">>, Headers, Properties, Body} | Tail], [Count | Ptail] = Path, Files, Acc) ->
	{Sublist, Midfiles} = skeletonize([Body], [1 | Path], Files),
	Newacc = [{<<"message">>, <<"rfc822">>, downcase_headers(Headers), Properties, Sublist} | Acc],
	Newfiles = append_files(Headers, Properties, Path, Midfiles),
	skeletonize(Tail, [Count + 1 | Ptail], Newfiles, Newacc);
skeletonize([{<<"message">>, Subtype, Headers, Properties, _Body} | Tail], [Count | Ptail], Files, Acc) ->
	Newacc = [{<<"message">>, Subtype, downcase_headers(Headers), Properties} | Acc],
	skeletonize(Tail, [Count + 1 | Ptail], Files, Newacc);
skeletonize([Head | Tail], [Count | Ptail] = Path, Files, Acc) ->
	{_Skel, Newfiles} = skeletonize(Head, Path, Files),
	Newacc = [{element(1, Head), element(2, Head), element(3, Head), element(4, Head)} | Acc],
	skeletonize(Tail, [Count + 1 | Ptail], Newfiles, Newacc).

downcase_headers(Headers) ->
	downcase_headers(Headers, []).

downcase_headers([], Acc) ->
	lists:reverse(Acc);
downcase_headers([{Key, Value} | Tail], Acc) ->
	downcase_headers(Tail, [{binstr:to_lower(Key), Value} | Acc]).

get_part([], {<<"multipart">>, _Subtype, _Headers, _Properties, List}) ->
	%?DEBUG("[], \"multipart\"", []),
	{multipart, List};
get_part([], {<<"message">>, _Subtype, _Headers, _Properties, Body}) ->
	%?DEBUG("[], \"message\"", []),
	{message, Body};
get_part([], Mime) ->
	%?DEBUG("[], ~p/~p", [element(1, Mime), element(2, Mime)]),
	{ok, Mime};
get_part([Child | Tail], {<<"multipart">>, _Subtype, _Headers, _Properties, List}) when Child =< length(List) ->
	%?DEBUG("[~p | ~p], multipart/~p", [Child, Tail, Subtype]),
	Part = lists:nth(Child, List),
	get_part(Tail, Part);
get_part([1 | Tail], {<<"message">>, _Subtype, _Headers, _Properties, Body}) ->
	%?DEBUG("[1 | ~p], message/~p", [Tail, Subtype]),
	get_part(Tail, Body);
get_part(Path, Mime) ->
	?INFO("Invalid path ~p.  ~p/~p", [Path, element(1, Mime), element(2, Mime)]),
	none.

get_part_by_content_location([], _Key) ->
	none;
get_part_by_content_location([Part | Parts], Key) ->
	% TODO this is a hack
	Key2 = list_to_binary(edoc_lib:escape_uri(binary_to_list(Key))),
	case mimemail:get_header_value(<<"Content-Location">>, element(3, Part)) of
		Key ->
			{ok, Part};
		Key2 ->
			{ok, Part};
		_X ->
			get_part_by_content_location(Parts, Key)
	end.

append_files(Headers, Properties, Path, Files) ->
	?DEBUG("append_files:  ~p, ~p", [Headers, Properties]),
	case {mimemail:get_header_value(<<"Content-ID">>, Headers), get_disposition({1, 1, 1, Properties, 1})} of
		{undefined, inline} ->
			Files;
		{undefined, {_Linedness, Name}} ->
			[{Name, lists:reverse(Path)} | Files];
		{Contentidbin, Dispo} ->
			Contentid = binary_to_list(Contentidbin),
			Len = length(Contentid),
			Id = lists:append(["cid:", string:sub_string(Contentid, 2, Len - 1)]),
			Midfiles = [{Id, lists:reverse(Path)} | Files],
			case Dispo of
				inline ->
					Midfiles;
				{_Linedness, Name} ->
					Fixedname = binary_to_list(Name),
					[{Fixedname, lists:reverse(Path)} | Midfiles]
			end
	end.

scrub_send_html(Html) ->
	HtmlList = lists:append(["<span>", binary_to_list(Html), "</span>"]),
	HtmlParse = mochiweb_html:parse(HtmlList),
	?DEBUG("~p", [HtmlParse]),
	list_to_binary(lists:reverse(scrub_send_html([HtmlParse], []))).

scrub_send_html([], Acc) ->
	Acc;
scrub_send_html([{comment, _} | Tail], Acc) ->
	scrub_send_html(Tail, Acc);
scrub_send_html([{Tag, Props, Children} | Tail], Acc) ->
	Lowertag = binstr:to_lower(Tag),
	scrub_send_html_sub([{Lowertag, downcase_headers(Props), Children} | Tail], Acc);
scrub_send_html([Bin | Tail], Acc) ->
	scrub_send_html(Tail, [Bin | Acc]).

scrub_send_html_sub([{<<"img">>, Props, []} | Tail], Acc) ->
	Text = case {proplists:get_value(<<"src">>, Props), proplists:get_value(<<"alt">>, Props)} of
		{undefined, undefined} ->
			<<"">>;
		{Bin, undefined} ->
			list_to_binary([<<"image:  ">>, Bin]);
		{_, Bin} ->
			Bin
	end,
	scrub_send_html(Tail, [Text | Acc]);
scrub_send_html_sub([{<<"a">>, Props, Children} | Tail], Acc) ->
	Text = case proplists:get_value(<<"href">>, Props) of
		undefined ->
			<<"">>;
		Else ->
			list_to_binary(["(href:  ", Else, ") "])
	end,
	Midacc = [Text | Acc],
	Thirdacc = scrub_send_html(Children, Midacc),
	scrub_send_html(Tail, Thirdacc);
scrub_send_html_sub([{<<"li">>, _Props, Children} | Tail], Acc) ->
	Midacc = scrub_send_html(Children, Acc),
	scrub_send_html(Tail, [<<"\n">> | Midacc]);
scrub_send_html_sub([{<<"br">>, _Props, Children} | Tail], Acc) ->
	Midacc = scrub_send_html(Children, Acc),
	scrub_send_html(Tail, [<<"\n">> | Midacc]);
scrub_send_html_sub([{_Tag, _Props, Children} | Tail], Acc) ->
	Midacc = scrub_send_html(Children, Acc),
	scrub_send_html(Tail, Midacc).
	
-ifdef(TEST).

scrub_send_html_test_() ->
	[{"Simple scrub",
	fun() ->
		?assertEqual(<<"hi">>, scrub_send_html(<<"hi">>))
	end},
	{"Slightly more complex",
	fun() ->
		?assertEqual(<<"123">>, scrub_send_html(<<"<div>1</div><div>2</div><div>3</div>">>))
	end},
	{"img scrub",
	fun() ->
		?assertEqual(<<"alt text">>, scrub_send_html(<<"<span><img src=\"src\" alt=\"alt text\" /></span>">>))
	end},
	{"img no alt text",
	fun() ->
		?assertEqual(<<"image:  src">>, scrub_send_html(<<"<span><img src=\"src\" /></span>">>))
	end},
	{"a with href",
	fun() ->
		?assertEqual(<<"(href:  href) text">>, scrub_send_html(<<"<span><a href=\"href\">text</a></span>">>))
	end},
	{"a without href",
	fun() ->
		?assertEqual(<<"text">>, scrub_send_html(<<"<span><a name=\"goober\" />text</span>">>))
	end},
	{"a list",
	fun() ->
		?assertEqual(<<"1\n2\n3\n">>, scrub_send_html(<<"<ul><li>1</li><li>2</li><li>3</li></ul>">>))
	end},
	{"br tags",
	fun() ->
		?assertEqual(<<"1\n2\n3">>, scrub_send_html(<<"<span>1<br />2<br />3</span>">>))
	end}].

getmail(File) ->
	Pre = case file:read_file_info("deps/gen_smtp/testdata") of
		{ok, #file_info{type = directory}} ->
			[];
		_ ->
			"../"
	end,
	{ok, Bin} = file:read_file(Pre ++ "deps/gen_smtp/testdata/" ++ File),
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
		?assertEqual([{<<"chili-pepper.jpg">>, [1]}], Files)
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
		?assertEqual([2, 6], proplists:get_value(<<"chili-pepper.jpg">>, Files)),
		?assertEqual([2, 8], proplists:get_value(<<"test.rtf">>, Files)),
		?assertEqual([2, 2], proplists:get_value(<<"message as attachment.eml">>, Files))
	end},
	{"Files with id's logged (testcase1)",
	fun() ->
		Decoded = getmail("testcase1"),
		{_Skel, Files} = skeletonize(Decoded),
		?DEBUG("Files:  ~p", [Files]),
		?assertEqual([2, 1, 2, 2], proplists:get_value("cid:part1.03050108.02070304@gmail.com", Files)),
		?assertEqual([2, 1, 2, 2], proplists:get_value("moz-screenshot-1.jpg", Files))
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
