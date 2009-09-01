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

%% @doc The media manager for handling email.  This starts the email server
%% listener.  The listener passes off to the email_media_session callback.
%% When the email_media_session callback is ready for an email to enter queue, 
%% it sends a message back here for an email_media to be created and queued.
-module(email_media_manager).
-author(spicecsm).

-behaviour(gen_server).

%% API
-export([
	start_link/1,
	start/1,
	stop/0,
	get_mappings/0,
	set_mapping/2,
	new_mapping/1,
	destroy_mapping/1,
	requeue/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("smtp.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(state, {
	mails = [] :: [pid()],
	relays = [] :: [string()],
	server :: pid()
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================
-spec(start_link/1 :: (Options :: [any()]) -> {'ok', pid()}).
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).

-spec(start/1 :: (Options :: [any()]) -> {'ok', pid()}).
start(Options) ->
	gen_server:start({local, ?MODULE}, ?MODULE, [Options], []).

-spec(stop/0 :: () -> 'ok').
stop() ->
	gen_server:call(?MODULE, stop).

-spec(get_mappings/0 :: () -> {'atomic', [#mail_map{}]}).
get_mappings() ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(mail_map)]),
		qlc:e(QH)
	end,
	mnesia:transaction(F).

-spec(set_mapping/2 :: (Address :: string(), Options :: #mail_map{} | [any()]) -> {'atomic', 'ok'} | {'aborted', any()}).
set_mapping(Address, Options) when is_list(Options) ->
	Rec = #mail_map{
		address = proplists:get_value(address, Options),
		queue = proplists:get_value(queue, Options, "default_queue"),
		skills = proplists:get_value(skills, Options, []),
		client = proplists:get_value(client, Options)
	},
	set_mapping(Address, Rec);		
set_mapping(Address, Rec) when is_record(Rec, mail_map) ->
	F = fun() ->
		mnesia:delete({mail_map, Address}),
		mnesia:write(Rec)
	end,
	mnesia:transaction(F).

-spec(new_mapping/1 :: (Options :: #mail_map{} | [any()]) -> {'atomic', 'ok'} | {'aborted', any()}).
new_mapping(Options) when is_list(Options) ->
	Rec = #mail_map{
		address = proplists:get_value(address, Options),
		queue = proplists:get_value(queue, Options, "default_queue"),
		skills = proplists:get_value(skills, Options, []),
		client = proplists:get_value(client, Options)
	},
	new_mapping(Rec);
new_mapping(Rec) when is_record(Rec, mail_map) ->
	F = fun() ->
		mnesia:write(Rec)
	end,
	mnesia:transaction(F).

-spec(destroy_mapping/1 :: (Address :: string()) -> {'atomic', 'ok'} | {'aborted', any()}).
destroy_mapping(Address) ->
	F = fun() ->
		mnesia:delete({mail_map, Address})
	end,
	mnesia:transaction(F).

-spec(requeue/1 :: (Filename :: string()) -> 'ok').
requeue(Filename) ->
	gen_server:call(email_media_manager, {queue, Filename}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%--------------------------------------------------------------------
init([Options]) ->
	process_flag(trap_exit, true),
	build_table(),
	%% the smtp server doesn't die along w/ this (it's trapping exits and
	%% throwing away anything not from a session).  So, when this starts, if
	%% something's not already registered, it will attempt to start the server
	%% and register.  If the server fails to start for any reason, this will
	%% die a horrible death (which is good!).
	Pid = case whereis(cpx_smtp_server) of
		undefined ->
			{ok, Server} = gen_smtp_server:start(email_media_session, Options),
			register(cpx_smtp_server, Server),
			link(Server),
			Server;
		Reg when is_pid(Reg) ->
			link(Reg),
			Reg
	end,
	case proplists:get_value(relays, Options) of
		undefined ->
			{ok, #state{server = Pid}};
		Else ->
			?INFO("Reserved for future use.", []),
			{ok, #state{relays = Else, server=Pid}}
	end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------
handle_call({queue, Mailmap, Headers, Data}, _From, #state{mails = Mails} = State) ->
	{ok, Mpid} = email_media:start(Mailmap, Headers, Data),
	link(Mpid),
	{reply, ok, State#state{mails = [Mpid | Mails]}};
handle_call({queue, Filename}, _From, #state{mails = Mails} = State) ->
	{ok, Bin} = file:read_file(Filename),
	Email = binary_to_list(Bin),
	{_, _, Headers, _, _} = mimemail:decode(Email),
	Mailmap = case proplists:get_value("From", Headers) of
		undefined ->
			#mail_map{address = "unknown@example.com"};
		Address ->
			F = fun() ->
				QH = qlc:q([X || X <- mnesia:table(mail_map), X#mail_map.address =:= Address]),
				qlc:e(QH)
			end,
			case mnesia:transaction(F) of
				{atomic, []} ->
					#mail_map{address = Address};
				{atomic, [Map]} ->
					Map
			end
	end,
	{ok, Mpid} = email_media:start(Mailmap, Email),
	link(Mpid),
	{reply, ok, State#state{mails = [Mpid | Mails]}};
handle_call(stop, From, State) ->
	?INFO("Received request to stop from ~p", [From]),
	{stop, normal, ok, State};
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
handle_info({'EXIT', Pid, Reason}, #state{server = Pid} = State) ->
	?WARNING("The server at ~w exited due to ~p", [Pid, Reason]),
	{stop, Reason, State#state{server = undefined}};
handle_info({'EXIT', From, Reason}, #state{mails = Mails} = State) ->
	?DEBUG("Handling exit from ~w due to ~p", [From, Reason]),
	Newmail = lists:delete(From, Mails),
	{noreply, State#state{mails = Newmail}};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(Reason, #state{server = undefined}) ->
	?INFO("Terminating due to ~p; no known server to drag down", [Reason]),
	ok;
terminate(Reason, State) ->
	?INFO("terminating due to ~p, taking server w/ us.", [Reason]),
	gen_smtp_server:stop(State#state.server),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

build_table() ->
	?DEBUG("Buidling table...", []),
	A = util:build_table(mail_map, [
		{attributes, record_info(fields, mail_map)},
		{disc_copies, [node()]}
	]),
	case A of
		{atomic, ok} ->
			?INFO("Writing default data...", []),
			F = fun() ->
				mnesia:write(#mail_map{address = "support@example.com"})
			end,
			mnesia:transaction(F);
		_Else when A =:= copied; A =:= exists ->
			?DEBUG("Copied the table from elsewhere.", []),
			ok;
		_Else ->
			A
	end.

