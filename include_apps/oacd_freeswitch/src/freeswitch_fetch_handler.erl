%% @doc Fetch handler for freeswitch sip authentication.  Adds trigger for
%% the auth_agent_success hook to nab the agent password and generate some
%% a1 hashes for it.

-module(freeswitch_fetch_handler).
-behavior(gen_server).

-include_lib("OpenACD/include/log.hrl").

-define(default_dial_string(Type), case Type of
		sip_registration -> "sofia/internal/$1%";
		sip -> "sofia/internal/sip:$1";
		iax2 -> "iax2/$1";
		h323 -> "opal/h3232:$1";
		pstn -> "";
		rtmp -> "rtmp/$1"
end).
-define(EMPTYRESPONSE, "<document type=\"freeswitch/xml\"></document>").
-define(NOTFOUNDRESPONSE,
"<document type=\"freeswitch/xml\">
	<section name=\"result\">
		<result status=\"not found\" />
	</section>
</document>").

-define(DIALUSERRESPONSE,
"<document type=\"freeswitch/xml\">
	<section name=\"directory\">
		<domain name=\"~s\">
			<user id=\"~s\">
				<params>
					<param name=\"dial-string\" value=\"~s\"/>
				</params>
			</user>
		</domain>
	</section>
</document>").

-define(REGISTERRESPONSE,
"<document type=\"freeswitch/xml\">
	<section name=\"directory\">
		<domain name=\"~s\">
			<user id=\"~s\">
				<params>
					<param name=\"a1-hash\" value=\"~s\"/>
				</params>
				<variables>
					<variable name=\"user_context\" value=\"default\"/>
				</variables>
			</user>
		</domain>
	</section>
</document>").

-define(USERRESPONSE,
"<document type=\"freeswitch/xml\">
	<section name=\"directory\">
		<domain name=\"~s\">
			<user id=\"~s\">
			</user>
		</domain>
	</section>
</document>").

-define(ets, oacd_freeswitch_a1).

% gen_server
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,
	code_change/3]).
% public api
-export([start_link/3,hook_callback/3,set_hook/0]).

%% ======================================================================
%% API
%% ======================================================================


start_link(Freeswitch,DialstringBases,SipAuth) when SipAuth =:= sip_auth; SipAuth =:= no_sip_auth ->
	Pid = proc_lib:spawn_link(fun() ->
		{foo, Freeswitch} ! {bind, directory},
		receive
			ok ->
				erlang:register(?MODULE,self()),
				{ok, State} = init({Freeswitch,DialstringBases,SipAuth}),
				gen_server:enter_loop(?MODULE,[],State,{local, ?MODULE});
			{error,Reason} ->
				exit(Reason)
		after 5000 ->
			exit(timeout)
		end
	end),
	{ok, Pid}.

hook_callback(User, Pass, _Res) ->
	?DEBUG("Hook triggered for user auth:  ~p", [User]),
	{ok, Ifs} = inet:getif(),
	IfsToIpStrings = fun({Ip,_,_},Acc) ->
		{A,B,C,D} = Ip,
		Ips = lists:flatten(io_lib:format("~p.~p.~p.~p",[A,B,C,D])),
		[Ips | Acc]
	end,
	Localhost = net_adm:localhost(),
	IpStrings = lists:foldl(IfsToIpStrings,[Localhost],Ifs),
	Hashes = [{IPString,util:bin_to_hexstr(erlang:md5(User++":"++IPString++":"++Pass))} || IPString <- IpStrings],
	ets:insert(?ets, {User, Hashes}).

set_hook() ->
	cpx_hooks:set_hook(?MODULE, auth_agent_success, {?MODULE, hook_callback, []}).

%% ======================================================================
%% gen_server
%% ======================================================================

%% ----------------------------------------------------------------------
%% init
%% ----------------------------------------------------------------------

init({Freeswitch,Dialstrings,SipAuth}) ->
	ets:new(?ets, [named_table, public]),
	set_hook(),
	?INFO("Started",[]),
	{ok, {Freeswitch,Dialstrings,SipAuth}}.

%% ----------------------------------------------------------------------
%% handle_call
%% ----------------------------------------------------------------------

handle_call(_Msg,_From,State) ->
	{reply, {error, invalid},State}.

%% ----------------------------------------------------------------------
%% handle_cast
%% ----------------------------------------------------------------------

handle_cast(_Msg,State) ->
	{noreply,State}.

%% ----------------------------------------------------------------------
%% handle_info
%% ----------------------------------------------------------------------

handle_info({fetch, directory, "domain", "name", _Value, Id, [undefined | Data]}, State) ->
	%?DEBUG("The data:  ~p", [Data]),
	case proplists:get_value("as_channel", Data) of
		"true" ->
			fetch_as_channel(Id,Data,State);
		_Else ->
			case proplists:get_value("action",Data) of
				"sip_auth" ->
					fetch_sip_auth(Id,Data,State);
				_ ->
					fetch_user_lookup(Id,Data,State)
			end
	end,
	{noreply, State};

handle_info({fetch, _Section, _Something, _Key, _Value, ID, [undefined | _Data]},{Node,_,_}=State) ->
	freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE),
	{noreply,State};

handle_info({nodedown,Node},{Node,_,_}=State) ->
	{stop,nodedown,State};

handle_info(_Msg,State) ->
	{noreoply,State}.

%% ----------------------------------------------------------------------
%% terminate
%% ----------------------------------------------------------------------

terminate(_Why,_State)-> ok.

%% ----------------------------------------------------------------------
%% code_change
%% ----------------------------------------------------------------------

code_change(_Old,State,_Extra) ->
	{ok,State}.

%% ======================================================================
%% Internal
%% ======================================================================

fetch_user_lookup(Id,_Data,{Node,_,no_sip_auth}) ->
	?DEBUG("Lookup when not doing sip auth, empty response",[]),
	% I guess we're just looking up a user?
	% Looking up for first part of an auth most likely.
	% only auth we support is sip (which is above) so we'll
	% depend on that.
	freeswitch:fetch_reply(Node, Id, ?EMPTYRESPONSE);

fetch_user_lookup(ID,Data,{Node,_,_}) ->
	User = proplists:get_value("user", Data),
	Domain = proplists:get_value("domain", Data),
	case agent_manager:query_agent(User) of
		{true, _Pid} ->
			?DEBUG("Lookup succeeded for user ~s@~s", [User,Domain]),
			freeswitch:fetch_reply(Node, ID, lists:flatten(io_lib:format(?USERRESPONSE, [Domain, User])));
		false ->
			freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE)
	end.

fetch_sip_auth(ID,_Data,{Node,_,no_sip_auth}) ->
	%% not doing sip auth, return nothing
	?DEBUG("Not doing SIP auth", []),
	freeswitch:fetch_reply(Node, ID, "");

fetch_sip_auth(ID,Data,{Node,_,sip_auth}) ->
	User = proplists:get_value("user", Data),
	Domain = proplists:get_value("domain", Data),
	Realm = proplists:get_value("sip_auth_realm", Data),
	% TODO Can this be done w/o dealing w/ a plain text pw?
	case ets:lookup(?ets, User) of
		[] -> 
			?DEBUG("Sip auth ~s@~s for relam ~s is not found",[User,Domain,Realm]),
			freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE);
		[{User,Hashes}] ->
			case proplists:get_value(Realm,Hashes) of
				undefined ->
					?DEBUG("Sip auth ~s@~s for relam ~s has no hash",[User,Domain,Realm]),
					freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE);
				Hash ->
					?DEBUG("Sip auth ~s@~s for relam ~s found hash ~s",[User,Domain,Realm,Hash]),
					freeswitch:fetch_reply(Node, ID, lists:flatten(io_lib:format(?REGISTERRESPONSE,[Domain,User,Hash])))
			end
	end.

fetch_as_channel(ID,Data,{Node,State,_}) ->
	User = proplists:get_value("user", Data),
	Domain = proplists:get_value("domain", Data),
	case agent_manager:query_agent(User) of
		{true, Pid} ->
			try agent:dump_state(Pid) of
				Agent ->
					{ok, {Opts, _}} = agent:get_endpoint(freeswitch_media, Agent),
					Type = proplists:get_value(type, Opts),
					Dat = proplists:get_value(data, Opts),

					DialString = case {Type, Dat} of
						{sip_registration, undefined} ->
							"${sofia_contact("++User++"@"++Domain++")}";
						{sip_registration, D} ->
							"${sofia_contact("++re:replace(D, "@", "_", [{return, list}])++"@"++Domain++")}";
						{sip, D} ->
							freeswitch_media_manager:do_dial_string(proplists:get_value(sip, State, "sofia/internal/sip:"), D, []);
						{iax2, D} ->
							freeswitch_media_manager:do_dial_string(proplists:get_value(iax2, State, "iax2/"), D, []);
						{h323, D} ->
							freeswitch_media_manager:do_dial_string(proplists:get_value(h323, State, "opal/h323:"), D, []);
						{pstn, D} ->
							freeswitch_media_manager:do_dial_string(proplists:get_value(dialstring, State, ""), D, []);
						{rtmp,D} ->
							freeswitch_media_manager:do_dial_string(proplists:get_value(rtmp, State, "rtmp/$1"), D, [])
					end,
					?NOTICE("returning ~s for user directory entry ~s", [DialString, User]),
					freeswitch:fetch_reply(Node, ID, lists:flatten(io_lib:format(?DIALUSERRESPONSE, [Domain, User, DialString])))
			catch
				_:_ -> % agent pid is toast?
					freeswitch:fetch_reply(Node, ID, ?NOTFOUNDRESPONSE)
			end;
		false ->
			freeswitch:fetch_reply(Node, ID, ?NOTFOUNDRESPONSE)
	end.
