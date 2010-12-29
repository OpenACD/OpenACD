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

%% @doc A reference implementation of the integration system.  When an agent
%% requests to log in, or a client is looked up, opens the appropriate file
%% and then returns the result.  Obviously not good for a production setup.

-module(integration_file).
-author(micahw).

-behaviour(gen_server).

-include("log.hrl").
-include("call.hrl").
-include("cpx.hrl").
-include("agent.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
	start/0,
	start/1,
	start_link/0,
	start_link/1,
	import/1
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
	file :: string()
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

-define(DEFAULT_FILE, "priv/integration_file.example").

%%====================================================================
%% API
%%====================================================================

-type(file_opt() :: {'file', string()}).
-type(start_opt() :: file_opt()).
-type(start_opts() :: [start_opt()]).

%% @doc Start linked with given options.
-spec(start_link/1 :: (Options :: start_opts()) -> {'ok', pid()}).
start_link(Options) ->
    gen_server:start_link({local, integration}, ?MODULE, Options, []).
%% @doc start linked with default options.
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() ->
	start_link([]).

%% @doc Start unlinked.
-spec(start/1 :: (Options :: start_opts()) -> {'ok', pid()}).
start(Options) ->
	gen_server:start({local, integration}, ?MODULE, Options, []).

%% @doc Start unlinked with default options.
-spec(start/0 :: () -> {'ok', pid()}).
start() ->
	start([]).

-type(truncate_opt() :: 'truncate').
-type(import_opts() :: 
	file_opt() | 
	truncate_opt()
).
%% @doc Import the information in the file into mnesia.  If `truncate' is
%% set, the tables are emptied before being filled.
-spec(import/1 :: (Options :: import_opts()) -> {'ok', pid()}).
import(Options) ->
	spawn(fun() -> import_spawn(Options) end).

import_spawn(Options) ->
	case proplists:get_value(truncate, Options) of
		true ->
			Tables = [agent_auth, client],
			[mnesia:clear_table(X) || X <- Tables];
		_ ->
			ok
	end,
	File = proplists:get_value(file, Options, ?DEFAULT_FILE),
	{ok, Terms} = file:consult(File),
	import_loop(Terms).

import_loop([]) ->
	ok;
import_loop([{client, ID, Label, Options} | Tail]) ->
	call_queue_config:destroy_client(id, ID),
	call_queue_config:destroy_client(label, Label),
	call_queue_config:set_client(#client{
		id = ID,
		label = Label,
		options = Options
	}),
	import_loop(Tail);
import_loop([{agent, Id, Login, Password, Security, Profile, Props} | Tail]) ->
	agent_auth:destroy(id, Id),
	agent_auth:destroy(login, Login),
	agent_auth:add_agent(#agent_auth{
		id = Id,
		login = Login,
		password = Password,
		skills = [],
		securitylevel = Security,
		profile = Profile,
		extended_props = Props
	}),
	import_loop(Tail).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%--------------------------------------------------------------------
init(Options) ->
	File = proplists:get_value(file, Options, ?DEFAULT_FILE),
	% will fail nicely if the file is unparaseable, which is yay!
	{ok, _Terms} = file:consult(File),
	{ok, #state{file = File}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------
handle_call({agent_exists, Agent}, _From, #state{file = File} = State) when is_list(Agent) ->
	{ok, Terms} = file:consult(File),
	case [X || {agent, _, Nom, _, _, _, _} = X <- Terms, Nom =:= Agent] of
		[] ->
			{reply, false, State};
		_ ->
			{reply, true, State}
	end;
handle_call({agent_auth, Agent, PlainPassword, _Extended}, _From, #state{file = File} = State) ->
	Password = erlang:md5(lists:flatten([Agent, ":", PlainPassword])),
	{ok, Terms} = file:consult(File),
	case [X || {agent, _, Nom, _, _, _, _} = X <- Terms, Nom =:= Agent] of
		[] ->
			{reply, destroy, State};
		[{agent, Id, Agent, Password, Security, Profile, Props}] ->
			Out = {ok, Id, Profile, Security, Props},
			{reply, Out, State};
		[_Tuple] ->
			{reply, deny, State}
	end;
handle_call({client_exists, id, Value}, _From, State) ->
	{ok, Terms} = file:consult(State#state.file),
	case [X || {client, Id, _, _} = X <- Terms, Id =:= Value] of
		[] ->
			{reply, false, State};
		_ ->
			{reply, true, State}
	end;
handle_call({client_exists, label, Value}, _From, State) ->
	{ok, Terms} = file:consult(State#state.file),
	case [X || {client, _, Label, _} = X <- Terms, Value =:= Label] of
		[] ->
			{reply, false, State};
		_ ->
			{reply, true, State}
	end;
handle_call({get_client, id, Value}, _From, State) ->
	{ok, Terms} = file:consult(State#state.file),
	case [X || {client, Id, _, _} = X <- Terms, Id =:= Value] of
		[] ->
			{reply, none, State};
		[{client, Value, Label, Props}] ->
			{reply, {ok, Value, Label, Props}, State}
	end;
handle_call({get_client, label, Value}, _From, State) ->
	{ok, Terms} = file:consult(State#state.file),
	case [X || {client, _, Label, _} = X <- Terms, Label =:= Value] of
		[] ->
			{reply, none, State};
		[{client, Id, Value, Props}] ->
			{reply, {ok, Id, Value, Props}, State}
	end;
handle_call(_Request, _From, State) ->
    Reply = invalid,
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
terminate(Reason, _State) ->
	?NOTICE("termination cause:  ~p", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
