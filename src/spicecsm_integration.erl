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

%% @doc Module to integrate with SpiceCSM.  See the integration module for what
%% the base API is.
-module(spicecsm_integration).
-author(micahw).

-behaviour(gen_server).

-include("log.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
	start/1,
	start_link/1
]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-record(state, {
	server :: string(),
	session :: string(),
	count = 1 :: pos_integer()
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================

-type(option_key() :: 'server' | 'username' | 'password').
-type(option() :: [{option_key(), string()}]).
-type(option_list() :: [option()]).
%% @doc Start linked with given options.
-spec(start_link/1 :: (Options :: option_list()) -> {'ok', pid()}).
start_link(Options) ->
    gen_server:start_link({local, integration}, ?MODULE, Options, []).

%% @doc Start unlinked.
-spec(start/1 :: (Options :: option_list()) -> {'ok', pid()}).
start(Options) ->
	gen_server:start({local, integration}, ?MODULE, Options, []).

raw_request(Apicall, Params) ->
	gen_server:call(integration, {raw_request, Apicall, Params}).
	
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%--------------------------------------------------------------------
init(Options) ->
	BaseServer = proplists:get_value(server, Options),
	Server = case BaseServer of
		undefined ->
			erlang:error(no_server);
		Else ->
			lists:append([Else, "/api/spiceAPIServer/spiceAPIServer.php"])
	end,
	application:start(inets),
	Username = proplists:get_value(username, Options, ""),
	Password = proplists:get_value(password, Options, ""),
	{ok, Count, Submitbody} = build_request(<<"connect">>, [Username, Password], 1),
	{ok, {{_Version, 200, _Ok}, _Headers, Body}} = http:request(post, {Server, [], "application/x-www-form-urlencoded", Submitbody}, [], []),
	{struct, Results} = mochijson2:decode(Body),
	case proplists:get_value(<<"result">>, Results) of
		null ->
			?WARNING("auth failure starting integration.  ~p", [Results]),
			{stop, {auth_fail, proplists:get_value(<<"error">>, Results, no_message)}};
		undefined ->
			{stop, {auth_fail, no_results, Results}};
		{struct, Details} ->
			Sid = proplists:get_value(<<"session">>, Details),
			Access = proplists:get_value(<<"security_level">>, Details),
			case {Sid, Access} of
				{undefined, _} ->
					?WARNING("No sid was defined in the details.  ~p", [Details]),
					{stop, {auth_fail, no_sid}};
				{_, Num} when Num < 4 ->
					{stop, {auth_fail, insufficient_access}};
				{_Otherwise, _Goodnumm} ->
					?INFO("Starting integration with server ~w as user ~w", [Server, Username]),
					{ok, #state{count = Count, server = Server, session = Sid}}
			end
	end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------
handle_call({agent_exists, Agent}, _From, State) when is_list(Agent) ->
	Query = lists:append(["SELECT Login FROM tblAgent WHERE Login=\"", Agent, "\" AND Active=1 LIMIT 1"]),
	{ok, Count, Reply} = request(State, <<"query">>, [Query]),
	case Reply of
		[] ->
			{reply, false, State#state{count = Count}};
		_Else ->
			{reply, true, State#state{count = Count}}
	end;
handle_call({raw_request, Apicall, Params}, _From, State) ->
	{ok, Count, Reply} = request(State, Apicall, Params),
	{reply, Reply, State#state{count = Count}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

build_request(Apicall, Params, Count) when is_binary(Apicall) ->
	Clean = fun
		(Elem, Acc) when is_list(Elem) ->
			[list_to_binary(Elem) | Acc];
		(Elem, Acc) when is_atom(Elem); is_binary(Elem); is_float(Elem), is_integer(Elem) ->
			[Elem | Acc]
	end,
	Cleanparams = lists:reverse(lists:foldl(Clean, [], Params)),
	Struct = {struct, [
		{<<"method">>, Apicall},
		{<<"params">>, Cleanparams},
		{<<"id">>, Count}
	]},
	Body = iolist_to_binary(lists:append(["request=", mochijson2:encode(Struct)])),
	{ok, Count + 1, Body}.
	
request(State, Apicall, Params) ->
	{ok, Count, Submitbody} = build_request(Apicall, lists:append(Params, State#state.session), State#state.count),
	{ok, {{_Version, 200, _Ok}, _Headers, Body}} = http:request(post, {State#state.server, [], "application/x-www-form-urlencoded", Submitbody}, [], []),
	{struct, Reply} = mochijson2:decode(Body),
	{ok, Count, Reply}.
