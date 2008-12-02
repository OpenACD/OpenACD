%%%-------------------------------------------------------------------
%%% File          : freeswitch_media_manager.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  11/18/08
%%%-------------------------------------------------------------------
-module(freeswitch_media_manager).
-author("Micah").

-behaviour(gen_server).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("queue.hrl").
-include("call.hrl").
-include("agent.hrl").

-define(TIMEOUT, 10000).

%% API
-export([start_link/2, start/2, listener/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	nodename :: atom(),
	freeswitch_c_pid :: pid(),
	watched_calls, % ets table of the call id's already queued.
	domain
	}).
	
% watched calls structure:
% callid, pid of gen_server handling it

% notes for tomorrow:  set up a process per call id to watch/process that call id

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Nodename, Domain) -> 
	gen_server:start({local, ?MODULE}, ?MODULE, [Nodename, Domain], []).
start_link(Nodename, Domain) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Nodename, Domain], []).

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
init([Nodename, Domain]) -> 
	process_flag(trap_exit, true),
	Self = self(),
	_Lpid = spawn(fun() -> 
		{freeswitchnode, Nodename} ! register_event_handler,
		receive
			ok ->
				Self ! {register_event_handler, {ok, self()}},
				listener(Nodename);
			{error, Reason} -> 
				Self ! {register_event_handler, {error, Reason}}
		after ?TIMEOUT -> 
			Self ! {register_event_handler, timeout}
		end
	end),
	T = freeswitch:event(Nodename, [channel_create, channel_answer, channel_destroy, channel_hangup, custom, 'fifo::info']),
	io:format("Attempted to start events in ffm's init:  ~p~n", [T]),
    {ok, #state{nodename=Nodename, watched_calls = ets:new(watched_calls, [named_table]), domain=Domain}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({ring_agent, AgentPid, Call}, _From, State) ->
	%% @todo A real media manager would do something substantial here, like try to
	%% send SIP to an agent's phone or whatever. We should do something about
	%% ringout here though...
	AgentRec = agent:dump_state(AgentPid),
	Args = "{dstchan=" ++ Call#call.id ++ "}sofia/default/" ++ AgentRec#agent.login ++ "%" ++ State#state.domain ++ " &park()",
	X = freeswitch:bgapi(State#state.nodename, originate, Args),
	io:format("Bgapi call res:  ~p~nWith args: ~p~n", [X, Args]),
	{reply, agent:set_state(AgentPid, ringing, Call), State};
handle_call(Request, _From, State) ->
	io:format("Sudden call at fmm:  ~p~n", [Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------


handle_cast([Callid | Rawcall], State) when is_list(Callid) -> 
	% is this a call we need to worry about?
	io:format("raw call in~n"),
	case ets:lookup(watched_calls, Callid) of
		[] -> 
			io:format("Call does not exist in fmm table~n"),
			Self = self(),
			{ok, Pid} = freeswitch_media:start_link(Callid, Rawcall, watched_calls, Self),
			ets:insert(watched_calls, {Callid, Pid}),
			{noreply, State};
		[{Callid, Pid}] -> 
			io:format("Call exists in fmm table~n"),
			gen_server:cast(Pid, {Callid, Rawcall}),
			{noreply, State};
		Otherwise -> 
			io:format("fmm ets seems corrupted, lookup got other result: ~p~n", [Otherwise]),
			{noreply, State}
	end;
handle_cast(Msg, State) ->
	io:format("freeswitch media manager got cast:  ~p~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({register_event_handler, {ok, Pid}}, State) -> 
	{noreply, State#state{freeswitch_c_pid = Pid}};
handle_info({'EXIT', Pid, normal}, State) -> 
	io:format("Trapping exit from ~p because of ~p.~n", [Pid, normal]),
	{noreply, State};
handle_info({'EXIT', Pid, _Reason}, State) -> 
	io:format("Bad exit, do clean up for ~p~n", [Pid]),
	{noreply, State};
handle_info(Info, State) ->
	io:format("Sudden info at the fmm:  ~p~n", [Info]),
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

listener(Node) ->
	receive
		{event, Event} ->
			gen_server:cast(?MODULE, Event), 
			listener(Node);
		{nodedown, Node} -> 
			gen_server:cast(?MODULE, nodedown);
		 Otherwise -> 
			 io:format("Uncertain reply received by the fmm listener:  ~p~n", [Otherwise]),
			 listener(Node)
	end.