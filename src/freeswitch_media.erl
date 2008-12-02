%%%-------------------------------------------------------------------
%%% File          : freeswitch_media.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  12/1/08
%%%-------------------------------------------------------------------
-module(freeswitch_media).
-author("Micah").

-behaviour(gen_server).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("queue.hrl").
-include("call.hrl").

-define(TIMEOUT, 10000).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {watched_calls, protocall = #call{}, queue, queue_pid, mode}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Callid, Rawcall, Watched_calls, Source) ->
	io:format("Source:  ~p~n", [Source]),
    gen_server:start_link(?MODULE, [Callid, Rawcall, Watched_calls, Source], []).

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
init([Callid, _Rawcall, Watched_calls, Source]) ->
	io:format("init source: ~p~n", [Source]),
	Protocall = #call{id=Callid, type=voice, source=Source},
	io:format("Protocall:  ~p~n", [Protocall]),
    {ok, #state{watched_calls=Watched_calls, protocall=Protocall}}.

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
handle_cast({Callid, Rawcall}, State) -> 
	io:format("fm Cast for call ~p.  I'm ~p.~n", [Callid, self()]),
	case freeswitch:get_event_name(Rawcall) of
		"CHANNEL_ANSWER" -> 
			io:format("Call started, variables available~n"),
			case has_dst_chan(Rawcall) of
				false -> 
					Q = freeswitch:get_event_header(Rawcall, "variable_queue"),
					Brand = freeswitch:get_event_header(Rawcall, "variable_brand"),
					Callerid = freeswitch:get_event_header(Rawcall, "Caller-Caller-ID-Name"),
					Protocall = State#state.protocall,
					Protocall2 = Protocall#call{id=Callid, client=Brand, callerid=Callerid},
					{noreply, State#state{protocall=Protocall2, queue=Q}};
				_Else ->
					io:format("Nothing needs to be done, as this is a bridge aid~n")
			end;
		"CUSTOM" -> 
			io:format("Custom event, begin with sub~n"),
			case has_dst_chan(Rawcall) of
				false -> 
					case freeswitch:get_event_header(Rawcall, "Event-Subclass") of
						"fifo::info" -> 
							fifo_parse(Rawcall, State);
						Otherwise -> 
							io:format("disregarding this custom: ~p~n", [Otherwise]),
							{noreply, State}
					end;
				_Else -> 
					io:format("Still, nothing needs to be done~n")
			end;
		"CHANNEL_HANGUP" -> 
			io:format("Call almost done, if it's in a queue, remove it.~n"),
			case State#state.queue_pid of
				undefined -> 
					{noreply, State};
				Q -> 
					call_queue:remove(Q, State#state.protocall),
					{noreply, State}
			end;
		"CHANNEL_DESTROY" -> 
			io:format("Call done, time to die.~n"),
			{stop, normal, State};
		Else -> 
			io:format("Not an event we care about: ~p~n", [Else])
	end;
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

has_dst_chan(Rawcall) -> 
	case freeswitch:get_event_header(Rawcall, "variable_dstchan") of
		{error, notfound} -> 
			false;
		Else -> 
			Else
	end.

fifo_parse(Rawcall, State) -> 
	io:format("fifo::info needs more processing.~n"),
	case freeswitch:get_event_header(Rawcall, "FIFO-Action") of
		"push" -> 
			io:format("put into queue~n"),
				case queue_manager:get_queue(State#state.queue) of
					undefined ->
						io:format("Uh oh, no queue of ~p~n", [State#state.queue]),
						{noreply, State};
					Qpid -> 
						io:format("Trying to add to queue...~n"),
						R = call_queue:add(Qpid, State#state.protocall),
						io:format("q response:  ~p~n", [R]),
						{noreply, State#state{queue_pid=Qpid}}
				end;
		"abort" ->
			io:format("happy hangup~n"),
			{noreply, State};
		Else -> 
			io:format("Uh...~p~n", [Else]),
			{noreply, State}
	end.