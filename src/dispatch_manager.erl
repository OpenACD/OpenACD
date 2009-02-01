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

%% @doc Handles the creation and desctruction of dispatchers.
%% There is to be 1 dipatcher for every avaiable agent on a node.
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
	supervisor :: pid() | 'undefined',
	dispatchers = [] :: [pid()],
	agents = [] :: [pid()]
	}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	
start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%% @private
init([]) ->
	{ok, Super} = dispatch_supervisor:start_link(),
    {ok, #state{supervisor=Super}}.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
handle_call(stop, _From, State) -> 
	{stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast({now_avail, AgentPid}, State) -> 
	io:format("Someone's available now.~n"),
	case lists:member(AgentPid, State#state.agents) of
		true -> 
			{noreply, balance(State)};
		false -> 
			erlang:monitor(process, AgentPid),
			State2 = State#state{agents = [AgentPid | State#state.agents]},
			{noreply, balance(State2)}
	end;
handle_cast({end_avail, AgentPid}, State) -> 
	io:format("An agent is no longer available.~n"),
	State2 = State#state{agents = lists:delete(AgentPid, State#state.agents)},
	{noreply, balance(State2)};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info({'DOWN', _MonitorRef, process, Object, _Info}, State) -> 
	io:format("Announcement that an agent is down, balancing in response.~n"),
	State2 = State#state{agents = lists:delete(Object, State#state.agents)},
	{noreply, balance(State2)};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @private
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec(stop/0 :: () -> any()).
stop() -> 
	gen_server:call(?MODULE, stop).
	
% TODO roll the dispatch_supervisor into this process.
%% @private
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
	case is_process_alive(Pid) of
		true ->
			ok = dispatcher:stop(Pid);
		Else -> 
			% don't try to kill it.
			ok
	end,
	balance(State#state{dispatchers=Dispatchers});
balance(State) -> 
	io:format("It is fully balanced!~n"),
	State.
