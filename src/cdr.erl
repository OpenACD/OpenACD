-module(cdr).
-behaviour(gen_event).

-include("log.hrl").
-include("call.hrl").

-export([
	start/0,
	cdrinit/1,
	inqueue/2,
	ringing/2,
	oncall/2,
	hangup/2,
	wrapup/2,
	endwrapup/2
]).

-export([
	init/1,
	handle_event/2,
	handle_call/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {
	id
}).

%% API
start() ->
	gen_event:start({local, cdr}).

cdrinit(Call) ->
	try gen_event:add_handler(cdr, {?MODULE, Call#call.id}, [Call]) of
		ok ->
			ok;
		Else ->
			?ERROR("Initializing CDR for ~s failed with: ~p", [Call#call.id, Else]),
			error
	catch
		What:Why ->
			?ERROR("Initializing CDR for ~s failed with: ~p:~p", [Call#call.id, What, Why]),
			error
	end.

inqueue(Call, Queue) ->
	catch gen_event:notify(cdr, {inqueue, Call, Queue}).

ringing(Call, Agent) ->
	catch gen_event:notify(cdr, {ringing, Call, Agent}).

oncall(Call, Agent) ->
	catch gen_event:notify(cdr, {oncall, Call, Agent}).

hangup(Call, By) ->
	catch gen_event:notify(cdr, {hangup, Call, By}).

wrapup(Call, Agent) ->
	catch gen_event:notify(cdr, {wrapup, Call, Agent}).

endwrapup(Call, Agent) ->
	catch gen_event:notify(cdr, {endwrapup, Call, Agent}).

%% Gen event callbacks
init([Call]) ->
	?NOTICE("Starting new CDR handler for ~s", [Call#call.id]),
	{ok, #state{id=Call#call.id}}.

handle_event({inqueue, #call{id = CallID} = Call, Queue}, #state{id = CallID} = State) ->
	?NOTICE("~s has joined queue ~s", [CallID, Queue]),
	{ok, State};
handle_event({ringing, #call{id = CallID} = Call, Agent}, #state{id = CallID} = State) ->
	?NOTICE("~s is ringing to ~s", [CallID, Agent]),
	{ok, State};
handle_event({oncall, #call{id = CallID} = Call, Agent}, #state{id = CallID} = State) ->
	?NOTICE("~s is oncall with ~s", [CallID, Agent]),
	{ok, State};
handle_event({hangup, #call{id = CallID} = Call, By}, #state{id = CallID} = State) ->
	?NOTICE("~s hungup by ~s", [CallID, By]),
	{ok, State};
handle_event({wrapup, #call{id = CallID} = Call, Agent}, #state{id = CallID} = State) ->
	?NOTICE("~s started wrapup for ~s", [Agent, CallID]),
	{ok, State};
handle_event({endwrapup, #call{id = CallID} = Call, Agent}, #state{id = CallID} = State) ->
	?NOTICE("~s ended wrapup for ~s", [Agent, CallID]),
	{ok, State};
handle_event(_Event, State) ->
	{ok, State}.

handle_call(_Request, State) ->
	{ok, ok, State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(_Args, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

