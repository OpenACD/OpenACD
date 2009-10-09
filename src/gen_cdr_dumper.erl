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
-include_lib("stdlib/include/qlc.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
	behaviour_info/1,
	%add_handler/2,
	%drop_handler/2,
	%start_link/0,
	start_link/2,
	start_link/0,
	start/2,
	start/0,
	update_notify/1
]).

%% for null dumping
-export([
	dump/2,
	commit/1
]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {
		module :: atom(),
		substate :: any()
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
	{commit, 1},
	{init, 1},
	{terminate, 2},
	{code_change, 3}];
behaviour_info(_Other) ->
	undefined.

%% @doc Add a new handler for cdr dumping.
%-spec(add_handler/2 :: (Module :: atom(), Options :: any()) -> 'ok').
%add_handler(Module, Options) ->
	%gen_server:cast(?MODULE, {add_handler, Module, Options}).

%% @doc Drop an existing handler for cdr dumping.
%-spec(drop_handler/2 :: (Module :: atom(), Reason :: any()) -> 'ok').
%drop_handler(Module, Reason) ->
	%gen_server:cast(?MODULE, {drop_handler, Module, Reason}).

%% @doc Start using the default options, and no handlers.
%-spec(start_link/0 :: () -> {'ok', pid()}).
%start_link() ->
	%start_link([]).

-type(callback_init() :: {atom(), any()}).
%% @doc given `[{Module :: atom(), Startargs :: any()}]' start the dumper with
%% a handler for each `Module' started with `Startargs'
-spec(start_link/2 :: (Module :: atom(), Args :: list()) -> {'ok', pid()}).
start_link(Module, Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Module, Args], []).

%% @doc Start the cdr_dumper with a null dumpter.
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, null, []).

-spec(start/2 :: (Module :: atom(), Args :: list()) -> {'ok', pid()}).
start(Module, Args) ->
	gen_server:start({local, ?MODULE}, ?MODULE, [Module, Args], []).

-spec(start/0 :: () -> {'ok', pid()}).
start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, null, []).

update_notify(TableName) ->
	Nodes = [node() | nodes()],
	lists:foreach(fun(Node) -> {?MODULE, Node} ! {update, TableName} end, Nodes).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%--------------------------------------------------------------------
init(null) ->
	{ok, #state{substate = null, module = ?MODULE}, hibernate};
init([Module, Args]) ->
	{ok, Substate} = Module:init(Args),
	{ok, #state{substate = Substate, module = Module}, hibernate}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State, hibernate}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%--------------------------------------------------------------------
%handle_cast({add_handler, Module, Init}, #state{callbacks = Callbacks} = State) ->
	%Newcallbacks = try Module:init(Init) of
		%{ok, Substate} ->
			%[{Module, Substate} | Callbacks];
		%Else ->
			%?INFO("Couldn't add handler ~w.  Bad return:  ~w", [Module, Else]),
			%Callbacks
	%catch
		%What:Why ->
			%?INFO("Couln't add handler ~w.  ~w:~p", [Module, What, Why]),
			%Callbacks
	%end,
	%{noreply, State#state{callbacks = Newcallbacks}, hibernate};
%handle_cast({drop_handler, Module, Reason}, #state{callbacks = Callbacks} = State) ->
	%case proplists:get_value(Module, Callbacks) of
		%undefined ->
			%ok;
		%Substate ->
			%try Module:terminate(Reason, Substate) of
				%_Whatever ->
					%ok
			%catch
				%_:_ ->
					%ok
			%end
	%end,
	%Newcallbacks = proplists:delete(Module, Callbacks),
	%{noreply, State#state{callbacks = Newcallbacks}, hibernate};
handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%--------------------------------------------------------------------
%handle_info({mnesia_table_event, {write, #agent_state{ended = undefined}, _Activityid}}, State) ->
	%{noreply, State, hibernate};
%handle_info({mnesia_table_event, {write, #agent_state{nodes = Nodes} = Staterec, _Activityid}}, State) ->
	%?INFO("write table event: ~p", [Nodes]),
	%case lists:delete(node(), Nodes) of
		%Nodes ->
			%{noreply, State, hibernate};
		%Newnodes ->
			%NewState = dump_row(Staterec, State, Newnodes),
			%{noreply, NewState, hibernate}
	%end;
handle_info({update, TableName}, State) ->
	?NOTICE("got update for ~p", [TableName]),
	NewState = dump_table(TableName, State),
	{noreply, NewState, hibernate};
handle_info(_Info, State) ->
	{noreply, State, hibernate}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(Reason, #state{module = ?MODULE}) ->
	ok;
terminate(Reason, #state{module = Module, substate = SubState}) ->
	try Module:terminate(Reason, SubState) of
		_Whatever ->
			ok
		catch
			_:_ ->
				ok
		end,
		ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_Oldvsn, #state{module = ?MODULE} = State, _Extra) ->
	{ok, State};
code_change(OldVsn, #state{module = Module, substate = SubState} = State, Extra) ->
		NewSubState = try Module:code_change(OldVsn, SubState, Extra) of
			{ok, Newsub} ->
				Newsub;
			Else ->
				?INFO("Improper response to code change for ~w.  ~p", [Module, Else]),
				SubState
		catch
			What:Why ->
				?INFO("Complete failure for ~w.  ~w:~p", [Module, What, Why]),
				SubState
		end,
		{ok, State#state{substate = NewSubState}}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

dump_row(#agent_state{} = Staterec, #state{module = Callback,
		substate = SubState} = State) ->
	{ok, NewSub} = Callback:dump(Staterec, SubState),
	% return new state and record to write back
	{State#state{substate = NewSub},
		Staterec#agent_state{nodes = lists:delete(node(),
				Staterec#agent_state.nodes),
			timestamp = util:now()}};
dump_row(#cdr_rec{} = CDR, #state{module = Callback,
		substate = SubState} = State) ->
	{ok, NewSub} = Callback:dump(CDR, SubState),
	% return new state and record to write back
	{State#state{substate = NewSub},
		CDR#cdr_rec{nodes = lists:delete(node(),
				CDR#cdr_rec.nodes),
			timestamp = util:now()}}.

dump_rows(QC, State) ->
	case qlc:next_answers(QC, 1) of
		[] ->
			qlc:delete_cursor(QC),
			State;
		[Row] ->
			{NewState, NewRow} = dump_row(Row, State),
			mnesia:delete_object(Row),
			%?NOTICE("writing updated row ~p", [NewRow]),
			%?NOTICE("old row ~p", [Row]),
			case NewRow of
				#cdr_rec{nodes = []} ->
					ok;
				#agent_state{nodes = []} ->
					ok;
				_Else ->
					mnesia:write(NewRow)
			end,
			dump_rows(QC, NewState)
	end.

dump_table(cdr_rec, #state{module = Callback} = State) ->
	F = fun() ->
			mnesia:lock({table, cdr_rec}, write),
			QH = qlc:q([CDR || CDR <- mnesia:table(cdr_rec),
					lists:member(node(), CDR#cdr_rec.nodes),
					CDR#cdr_rec.transactions =/= inprogress
				]),
			QC = qlc:cursor(QH),
			NewState = dump_rows(QC, State),
			Callback:commit(NewState#state.substate),
			NewState
	end,
	{atomic, NewState} = mnesia:transaction(F),
	NewState;
dump_table(agent_state, #state{module = Callback} = State) ->
	F = fun() ->
			mnesia:lock({table, agent_state}, write),
			QH = qlc:q([AgentState || AgentState <- mnesia:table(agent_state),
					lists:member(node(), AgentState#agent_state.nodes),
					AgentState#agent_state.ended =/= undefined
				]),
			QC = qlc:cursor(QH),
			NewState = dump_rows(QC, State),
			Callback:commit(NewState#state.substate),
			NewState
	end,
	{atomic, NewState} = mnesia:transaction(F),
	NewState;
dump_table(TableName, State) ->
	?WARNING("Unknown table ~p", [TableName]),
	State.

commit(_) ->
	null.

dump(_, _) ->
	{ok, null}.
