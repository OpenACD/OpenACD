%%%-------------------------------------------------------------------
%%% File          : dispatcher.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  10/15/08
%%%-------------------------------------------------------------------
-module(dispatcher).
-author("Micah").

-include("call.hrl").
-include("agent.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, grab/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	call :: #call{} | 'undefined',
	agents = [] :: [pid()]}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link(?MODULE, [], []).
start() ->
	gen_server:start(?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({grab_from, Queue}, _From, State) -> 
	case call_queue:grab(Queue) of
		none -> 
			{reply, {ok, none}, State};
		{_Key, Value} -> 
			State2 = State#state{call=Value},
			{reply, {ok, Value}, State2}
	end;
handle_call(stop, _From, State) ->
	io:format("okay, okay, I'll die.~n"),
	{stop, normal, ok, State};
handle_call(_Request, _From, State) ->
	Reply = unknown,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec(grab/1 :: (Queue :: pid()) -> {'ok', 'none'} | {'ok', #call{}}).
grab(Queue) -> 
	gen_server:call(?MODULE, {grab_from, Queue}).
	
-spec(stop/1 :: (pid()) -> 'ok').
stop(Pid) -> 
	io:format("just before the gen_server handle call pid:  ~p.  Me:  ~p", [Pid, self()]),
	gen_server:call(Pid, stop).