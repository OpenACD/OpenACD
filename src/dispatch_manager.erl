%%%-------------------------------------------------------------------
%%% File          : dispatch_manager.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  10/17/08
%%%-------------------------------------------------------------------
-module(dispatch_manager).
-author("Micah").

-include("call.hrl").
-include("agent.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	supervisor :: pid() | 'undefinced',
	dispatchers = [] :: [pid()],
	agents = [] :: [pid()]
	}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	
start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

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
	{ok, Super} = dispatch_supervisor:start_link(),
    {ok, #state{supervisor=Super}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call(stop, _From, State) -> 
	{stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({now_avail, AgentPid}, State) -> 
	io:format("Someone's avaialble now.~n"),
	case lists:member(AgentPid, State#state.agents) of
		true -> 
			{noreply, balance(State)};
		false -> 
			erlang:monitor(process, AgentPid),
			State2 = State#state{agents = [AgentPid | State#state.agents]},
			{noreply, balance(State2)}
	end;
handle_cast({end_avail, AgentPid}, State) -> 
	io:format("Slackers, the lot of you!~n"),
	State2 = State#state{agents = lists:delete(AgentPid, State#state.agents)},
	{noreply, balance(State2)};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', _MonitorRef, process, Object, _Info}, State) -> 
	io:format("Agent down!  Agent is down!~n"),
	State2 = State#state{agents = lists:delete(Object, State#state.agents)},
	{noreply, balance(State2)};
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

-spec(stop/0 :: () -> any()).
stop() -> 
	gen_server:call(?MODULE, stop).

-spec(balance/1 :: (State :: #state{}) -> #state{}).
balance(State) when length(State#state.agents) > length(State#state.dispatchers) -> 
	io:format("Starting new dispatcher~n"),
	Dispatchers = State#state.dispatchers,
	{ok, Pid} = supervisor:start_child(State#state.supervisor, []),
	State2 = State#state{dispatchers = [ Pid | Dispatchers]},
	balance(State2);
balance(State) when length(State#state.agents) < length(State#state.dispatchers) -> 
	io:format("Killing a dispatcher~n"),
	[Pid | Dispatchers] = lists:reverse(State#state.dispatchers),
	io:format("Pid I'm about to kill: ~p.  me:  ~p.  Dispatchers:  ~p~n", [Pid, self(), Dispatchers]),
	ok = dispatcher:stop(Pid),
	balance(State#state{dispatchers=Dispatchers});
balance(State) -> 
	io:format("It is nifty!~n"),
	State.