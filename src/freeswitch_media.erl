%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Original Code is Spice Telphony.
%% 
%% The Initial Developer of the Original Code is 
%% Andrew Thompson and Micah Warren.
%% Portions created by the Initial Developers are Copyright (C) 
%% SpiceCSM. All Rights Reserved.

%% Contributor(s): 

%% Andrew Thompson <athompson at spicecsm dot com>
%% Micah Warren <mwarren at spicecsm dot com>
%% 

%%%-------------------------------------------------------------------
%%% File          : freeswitch_media.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  12/1/08
%%%-------------------------------------------------------------------

%% @doc An on demand gen_server for watching a freeswitch call.
%% This is started by freeswitch_media_manager when a new call id is found.
%% This is responsible for:
%% <ul>
%% <li>Connecting an agent to a call</li>
%% <li>Moving a call into queue.</li>
%% <li>Removing a call from queue.</li>
%% <li>Signalling when a call has hung up.</li>
%% </ul>
%% @see freeswitch_media_manager

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
-export([start/0, start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {protocall = #call{}, queue, queue_pid, mode}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Leader) -> 
	gen_server:start(?MODULE, [whereis(Leader)], []).
start() ->
	gen_server:start(?MODULE, [whereis(user)], []).

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
init([Leader]) ->
	% erlang:group_leader(Leader, self()),
	io:format("freeswitch media start with leader ~p~n", [Leader]),
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
handle_call({ring_agent, AgentPid, Call}, _From, State) -> 
	Reply = freeswitch_media_manager:ring_agent(AgentPid, Call),
	{reply, Reply, State};
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
%	io:format("fm Cast for call ~p.  I'm ~p.~n", [Callid, self()]),
	case freeswitch:get_event_name(Rawcall) of
		"CHANNEL_ANSWER" -> 
			io:format("fm progress:  CHANNEL_ANSWER~n"),
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
			io:format("Not an event we care about: ~p~n", [Else]),
			{noreply, State}
	end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({call, {event, [UUID | Rest]}}, #state{protocall = Call} = State) -> 
	io:format("updating state for call ~p.  I'm ~p~n", [UUID, self()]),
	gen_server:cast(self(), {UUID, Rest}),
	Newcall = Call#call{id = UUID},
	{noreply, State#state{protocall=Newcall}};
handle_info({call_event, {event, [UUID | Rest]}}, State) -> 
	case freeswitch:get_event_name(Rest) of
		"CHANNEL_PARK" ->
			case State#state.queue_pid of
				undefined ->
					Queue = freeswitch:get_event_header(Rest, "variable_queue"),
					Brand = freeswitch:get_event_header(Rest, "variable_brand"),
					Callerid = freeswitch:get_event_header(Rest, "Caller-Caller-ID-Name"),
					Protocall = State#state.protocall,
					Protocall2 = Protocall#call{id=UUID, client=Brand, callerid=Callerid, source=self()},
					case queue_manager:get_queue(Queue) of
						undefined ->
							io:format("Uh oh, no queue of ~p~n", [Queue]),
							{noreply, State};
						Qpid -> 
							io:format("Trying to add to queue...~n"),
							R = call_queue:add(Qpid, Protocall2),
							io:format("q response:  ~p~n", [R]),
							{noreply, State#state{queue_pid=Qpid, queue=Queue, protocall=Protocall2}}
					end;
				 _Otherwise -> 
					 {noreply, State} 
			end;
		_Other_event -> 
			{noreply, State}
	end;
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
