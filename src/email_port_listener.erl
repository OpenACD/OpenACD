%%%-------------------------------------------------------------------
%%% File          : email_port_listener.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  1/8/09
%%%-------------------------------------------------------------------
-module(email_port_listener).
-author(micahw).

-behaviour(gen_server).

%% API
-export([start_link/1, start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	port, % the port to listen for messages from
	proto_mail, % email being built up
	external % external call to start up the email reciever
}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(External) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [External], []).
start(External) -> 
	gen_server:start({local, ?MODULE}, ?MODULE, [External], []).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([External]) ->
	Port = open_port({spawn, External}, [{packet, 4}, use_stdio, exit_status, binary]),
    {ok, #state{port=Port, external=External}}.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Port, {data, Data}}, State) when Port == State#state.port -> 
	case binary_to_term(Data) of
		{heartbeat, Text} -> 
			io:format("lub dub: ~p~n", [Text]),
			{noreply, State};
		Other -> 
			io:format("Did not understand message:  ~p~n", [Other]),
			{noreply, State}
	end;
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Description: This function is called by a gen_server when it is about to
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
