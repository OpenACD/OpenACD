-module(agent).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("call.hrl").
-include("agent.hrl").

%% gen_fsm exports
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
%% defeined gen_fsm exports
-export([idle/3, ringing/3, precall/3, oncall/3, outgoing/3, released/3, warmtransfer/3, wrapup/3]).

%% other exports
-export([start/1, start_link/1, query_state/1, set_state/2, set_state/3]).

% gen_fsm:start_link
% gen_fsm:send_event
% gen_fsm:send_all_state_event
% gen_fsm:sync_send_event
%    gen_fsm:sync_send_all_state_event

start_link(Agent = #agent{}) -> 
	gen_fsm:start_link(?MODULE, [Agent], []).
	
start(Agent = #agent{}) -> 
	gen_fsm:start(?MODULE, [Agent], []).
	
init([State = #agent{}]) -> 
	State2 = State#agent{statedata=default,state=released},
	{ok, released, State2}.

% actual functions we'll call
query_state(Pid) -> 
  gen_fsm:sync_send_all_state_event(Pid, query_state).

set_state(Pid, State) ->
	gen_fsm:sync_send_event(Pid, State).

set_state(Pid, State, Data) ->
	gen_fsm:sync_send_event(Pid, {State, Data}).


% replace state_name with actual state names.
% state_name(_Event, State) ->
%	{next_state, state_name, State}.


% state_name(_Event, _From, State) ->
%	Reply = ok,
%	{reply, Reply, state_name, State}.

idle({precall, Client}, _From, State) ->
	{reply, ok, precall, State#agent{state=precall, statedata=Client, lastchangetimestamp=now()}};
idle({ringing, Call}, _From, State) ->
	{reply, ok, ringing, State#agent{state=ringing, statedata=Call, lastchangetimestamp=now()}};
idle({released, Reason}, _From, State) ->
	{reply, ok, released, State#agent{state=released, statedata=Reason, lastchangetimestamp=now()}};
idle(_Event, _From, State) ->
	{reply, invalid, idle, State}.

ringing({oncall, Call}, _From, State) ->
	{reply, ok, oncall, State#agent{state=oncall, statedata=Call, lastchangetimestamp=now()}};
ringing({released, Reason}, _From, State) ->
	{reply, ok, released, State#agent{state=released, statedata=Reason, lastchangetimestamp=now()}};
ringing(idle, _From, State) ->
	{reply, ok, idle, State#agent{state=idle, statedata={}, lastchangetimestamp=now()}};
ringing(_Event, _From, State) ->
	{reply, invalid, ringing, State}.

precall({outgoing, Call}, _From, State) ->
	{reply, ok, outgoing, State#agent{state=outgoing, statedata=Call, lastchangetimestamp=now()}};
precall(idle, _From, State) ->
	{reply, ok, idle, State#agent{state=idle, statedata={}, lastchangetimestamp=now()}};
precall({released, Reason}, _From, State) ->
	{reply, ok, released, State#agent{state=released, statedata=Reason, lastchangetimestamp=now()}};
precall(_Event, _From, State) -> 
	{reply, invalid, precall, State}.
	
	
oncall({released, undefined}, _From, State) -> 
	{reply, ok, oncall, State#agent{queuedrelease=undefined}};
oncall({released, Reason}, _From, State) -> 
	{reply, queued, oncall, State#agent{queuedrelease=Reason}};
oncall({wrapup, Call}, _From, State) ->
	{reply, ok, wrapup, State#agent{state=wrapup, statedata=Call, lastchangetimestamp=now()}};
oncall({warmtransfer, Transferto}, _From, State) ->
	{replay, ok, warmtransfer, State#agent{state=warmtransfer, statedata={onhold, State#agent.statedata, calling, Transferto}}}.
	
outgoing({released, undefined}, _From, State) -> 
	{reply, ok, outgoing, State#agent{queuedrelease=undefined}};
outgoing({released, Reason}, _From, State) -> 
	{reply, queued, outgoing, State#agent{queuedrelease=Reason}};
outgoing({wrapup, Call}, _From, State) ->
	{reply, ok, wrapup, State#agent{state=wrapup, statedata=Call}};
outgoing({warmtransfer, Transferto}, _From, State) -> 
	{reply, ok, warmtransfer, State#agent{state=warmtransfer, statedata={onhold, State#agent.statedata, calling, Transferto}}};
outgoing(_Event, _From, State) -> 
	{reply, invalid, outgoing, State}.

released({precall, Client}, _From, State) ->
	{reply, ok, precall, State#agent{state=precall, statedata=Client, lastchangetimestamp=now()}};
released(idle, _From, State) ->
	{reply, ok, idle, State#agent{state=idle, statedata={}, lastchangetimestamp=now()}};
released({released, Reason}, _From, State) ->
	{replay, ok, released, State#agent{statedata=Reason, lastchangetimestamp=now()}};
released(_Event, _From, State) ->
	{reply, invalid, released, State}.

warmtransfer({released, undefined}, _From, State) ->
	{reply, ok, warmtransfer, State#agent{queuedrelease=undefined}};
warmtransfer({released, Reason}, _From, State) -> 
	{reply, queued, warmtransfer, State#agent{queuedrelease=Reason}};
warmtransfer({wrapup, Call}, _From, State) -> 
	{reply, ok, wrapup, State#agent{state=wrapup,statedata=Call, lastchangetimestamp=now()}};
warmtransfer({oncall, Call}, _From, State) -> 
	{reply, ok, oncall, State#agent{state=oncall, statedata=Call, lastchangetimestamp=now()}};
warmtransfer({outgoing, Call}, _From, State) ->
	{reply, ok, outgoing, State#agent{state=outgoing, statedata=Call, lastchangetimestamp=now()}};
warmtransfer(_Event, _From, State) ->
	{reply, invalid, warmtransfer, State}.

wrapup({released, undefined}, _From, State) ->
	{reply, ok, wrapup, State#agent{queuedrelease=undefined}};
wrapup({released, Reason}, _From, State) -> 
	{reply, queued, wrapup, State#agent{queuedrelease=Reason}};
wrapup(idle, _From, State= #agent{queuedrelease = undefined}) -> 
	{reply, ok, idle, State#agent{state=idle, statedata={}, lastchangetimestamp=now()}};
wrapup(idle, _From, State) -> 
	{reply, ok, released, State#agent{state=released, statedata=State#agent.queuedrelease, queuedrelease=undefined, lastchangetimestamp=now()}};
wrapup(_Event, _From, State) -> 
	{reply, invalide, wrapup, State}.


% generic handlers independant of state
handle_event(_Event, StateName, State) ->
	{next_state, StateName, State}.

handle_sync_event(query_state, _From, StateName, State) -> 
	{reply, {ok, StateName}, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
	{reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
	{next_state, StateName, State}.

% obviousness below.
terminate(_Reason, _StateName, _State) ->
	ok.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

-ifdef(EUNIT).
state_change_test() ->
	{_, Pid} = start(#agent{login="testagent"}),
	?assertMatch({ok, released}, query_state(Pid)),
	?assertEqual(ok, set_state(Pid, idle)),
	?assertMatch({ok, idle}, query_state(Pid)),
	?assertEqual(invalid, set_state(Pid, oncall)),
	?assertMatch({ok, idle}, query_state(Pid)),
	?assertEqual(ok, set_state(Pid, ringing, #call{idnum="foo"})),
	?assertMatch({ok, ringing}, query_state(Pid)),
	?assertEqual(ok, set_state(Pid, oncall, #call{idnum="bar"})),
	?assertMatch({ok, oncall}, query_state(Pid)),
	?assertMatch(queued, set_state(Pid, released, {1, 0})).
-endif.
