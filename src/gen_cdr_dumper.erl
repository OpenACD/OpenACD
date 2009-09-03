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

%% @doc Behavior for cdr dumping modules.  A cdr dumper is a module that takes 
%% the erlang record stored in mnesia and stores it in another place, be it a 
%% flat file, database, or the great datastore in the sky.  This module handles
%% the communication with mnesia, and merely passes the record to be written to
%% any handlers.  If a handler errors or fails, it is uncerimoniously dropped.

-module(gen_cdr_dumper).
-author(micahw).

-behaviour(gen_server).

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
	behaviour_info/1,
	add_handler/2,
	drop_handler/2,
	start_link/0,
	start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	callbacks = [] :: [{atom(), any()}]
}).
-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================

-spec(behaviour_info/1 :: (Info :: 'callbacks') -> [{atom(), non_neg_integer()}];
						(Info :: any()) -> 'undefined').
behaviour_info(callbacks) ->
	[{dump, 2},
	{init, 1},
	{terminate, 2},
	{code_change, 3}];
behaviour_info(_Other) ->
	undefined.

%% @doc Add a new handler for cdr dumping.
-spec(add_handler/2 :: (Module :: atom(), Options :: any()) -> 'ok').
add_handler(Module, Options) ->
	gen_server:cast(?MODULE, {add_handler, Module, Options}).

%% @doc Drop an existing handler for cdr dumping.
-spec(drop_handler/2 :: (Module :: atom(), Reason :: any()) -> 'ok').
drop_handler(Module, Reason) ->
	gen_server:cast(?MODULE, {drop_handler, Module, Reason}).

%% @doc Start using the default options, and no handlers.
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() ->
	start_link([]).

-type(callback_init() :: {atom(), any()}).
%% @doc given `[{Module :: atom(), Startargs :: any()}]' start the dumper with
%% a handler for each `Module' started with `Startargs'
-spec(start_link/1 :: (Callbacks :: [callback_init()]) -> {'ok', pid()}).
start_link(Callbacks) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Callbacks], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%--------------------------------------------------------------------
init([Callbacks]) ->
	F = fun({Module, Args}, Acc) ->
		Newacc = try Module:init(Args) of
			{ok, Substate} ->
				[{Module, Substate} | Acc];
			Else ->
				?WARNING("callback ~w failed to start:  ~p", [Module, Else]),
				Acc
		catch
			What:Why ->
				?WARNING("callback ~w failed to start:  ~w:~p", [Module, What, Why]),
				Acc
		end,
		Newacc
	end,
	Validmods = lists:foldl(F, [], Callbacks),
	mnesia:subscribe({table, agent_state, simple}),
    {ok, #state{callbacks = Validmods}, hibernate}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State, hibernate}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_cast({add_handler, Module, Init}, #state{callbacks = Callbacks} = State) ->
	Newcallbacks = try Module:init(Init) of
		{ok, Substate} ->
			[{Module, Substate} | Callbacks];
		Else ->
			?INFO("Couldn't add handler ~w.  Bad return:  ~w", [Module, Else]),
			Callbacks
	catch
		What:Why ->
			?INFO("Couln't add handler ~w.  ~w:~p", [Module, What, Why]),
			Callbacks
	end,
	{noreply, State#state{callbacks = Newcallbacks}, hibernate};
handle_cast({drop_handler, Module, Reason}, #state{callbacks = Callbacks} = State) ->
	case proplists:get_value(Module, Callbacks) of
		undefined ->
			ok;
		Substate ->
			try Module:terminate(Reason, Substate) of
				_Whatever ->
					ok
			catch
				_:_ ->
					ok
			end
	end,
	Newcallbacks = proplists:delete(Module, Callbacks),
	{noreply, State#state{callbacks = Newcallbacks}, hibernate};
handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_info({mnesia_table_event, {write, #agent_state{ended = undefined}, _Activityid}}, State) ->
	{noreply, State, hibernate};
handle_info({mnesia_table_event, {write, #agent_state{nodes = Nodes} = Staterec, _Activityid}}, State) ->
	case lists:delete(node(), Nodes) of
		Nodes ->
			{noreply, State, hibernate};
		Newnodes ->
			% TODO better error handling if a callback fails.
			F = fun({Callback, Substate}, Acc) ->
				Newacc = try Callback:dump(Staterec) of
					{ok, Newsub} ->
						[{Callback, Newsub} | Acc];
					{error, Error, Newsub} ->
						?WARNING("Graceful error from ~w:  ~p", [Callback, Error]),
						[{Callback, Newsub} | Acc]
				catch
					What:Why ->
						?WARNING("Not so graceful error from ~w:  ~w:~p", [Callback, What, Why]),
						% last chace for it to clean up after itself.
						try Callback:terminate({What, Why}, Substate) of
							_Whatever ->
								Acc
						catch
							_:_ ->
								% wow, this really fails hard-core.
								Acc
						end
				end,
				Newacc
			end,
			Newcallbacks = lists:foldl(F, [], State#state.callbacks),
			Transfun = fun() ->
				mnesia:delete_object(Staterec),
				case Newnodes of
					[] ->
						ok;
					_Else ->
						mnesia:write(Staterec#agent_state{nodes = Newnodes, timestamp = util:now()})
				end
			end,
			mnesia:transaction(Transfun),
			{noreply, State#state{callbacks = Newcallbacks}, hibernate}
	end;
handle_info(_Info, State) ->
    {noreply, State, hibernate}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(Reason, #state{callbacks = Callbacks}) ->
	F = fun({Module, Substate}) ->
		try Module:terminate(Reason, Substate) of
			_Whatever ->
				ok
		catch
			_:_ ->
				ok
		end
	end,
	lists:foreach(F, Callbacks),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, #state{callbacks = Callbacks} = State, Extra) ->
	F = fun({Module, Substate}, Acc) ->
		Newacc = try Module:code_change(OldVsn, Substate, Extra) of
			{ok, Newsub} ->
				[{Module, Newsub} | Acc];
			Else ->
				?INFO("Improper response to code change for ~w.  ~p", [Module, Else]),
				Acc
		catch
			What:Why ->
				?INFO("Complete failure for ~w.  ~w:~p", [Module, What, Why]),
				Acc
		end,
		Newacc
	end,
	Newsubs = lists:foldl(F, [], Callbacks),
    {ok, State#state{callbacks = Newsubs}}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
