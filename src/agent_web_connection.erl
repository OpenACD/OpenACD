%%%-------------------------------------------------------------------
%%% File          : agent_web_connection.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  10/30/08
%%%-------------------------------------------------------------------
-module(agent_web_connection).
-author("Micah").

-behaviour(gen_server).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("call.hrl").
-include("agent.hrl").

%% API
-export([start_link/2, start/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	salt,
	ref,
	agent_fsm,
	ack_queue = [],
	counter = 1,
	table
}).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Req, Table) ->
    gen_server:start_link(?MODULE, [Req, Table], []).
	
start(Req, Table) -> 
	io:format("web_connection started~n"),
	gen_server:start(?MODULE, [Req, Table], []).

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
init([Req, Table]) ->
	io:format("actual web connection init~n"),
	case Req:parse_post() of 
		[{"username", User},{"password", _Passwrd}] -> 
			io:format("seems like a well formed post~n"),
			Agent = #agent{login=User},
			Ref = make_ref(),
			Cookie = io_lib:format("cpx_id=~p", [erlang:ref_to_list(Ref)]),
			Self = self(),
			ets:insert(erlang:ref_to_list(Ref), Self),
			Req:response({200, [{"Set-Cookie", Cookie}], io_lib:format("<pre>~p~p</pre>", [Req:dump(), Req:parse_cookie()])}),
			{ok, Apid} = agent:start(Agent),
			{ok, #state{agent_fsm=Apid, ref=Ref, table=Table}};
		_Other -> 
			io:format("all other posts~n"),
			Req:response({403, [], "invalid post data"})
	end,
	io:format("error out"),
	{error, "Invalid Login"}.


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
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
terminate(_Reason, State) ->
	ets:delete(State#state.table, self()),
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
