-module(cdr).
-behaviour(gen_event).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
	endwrapup/2,
	transfer/2
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
	id,
	transactions = [],
	checkend = false :: 'false' | 'true'
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

transfer(Call, Transferto) ->
	catch gen_event:notify(cdr, {transfer, Call, Transferto}).

transactions(Call) ->
	gen_event:call(cdr, {?MODULE, Call#call.id}, transactions).

%% Gen event callbacks
init([Call]) ->
	?NOTICE("Starting new CDR handler for ~s", [Call#call.id]),
	{ok, #state{id=Call#call.id}}.

handle_event({inqueue, #call{id = CallID} = Call, Queue}, #state{id = CallID} = State) ->
	?NOTICE("~s has joined queue ~s", [CallID, Queue]),
	Newtrans = [{inqueue, {CallID, Queue}} | State#state.transactions],
	{ok, State#state{transactions = Newtrans}};
handle_event({ringing, #call{id = CallID} = Call, Agent}, #state{id = CallID} = State) ->
	?NOTICE("~s is ringing to ~s", [CallID, Agent]),
	Newtrans = [{ringing, {CallID, Agent}} | State#state.transactions],
	{ok, State#state{transactions = Newtrans}};
handle_event({oncall, #call{id = CallID} = Call, Agent}, #state{id = CallID} = State) ->
	?NOTICE("~s is oncall with ~s", [CallID, Agent]),
	Newtrans = [{oncall, {CallID, Agent}} | State#state.transactions],
	{ok, State#state{transactions = Newtrans}};
handle_event({hangup, #call{id = CallID} = Call, agent}, #state{id = CallID} = State) ->
	?NOTICE("~s hungup by agent", [CallID]),
	Newtrans = [{hangup, {CallID, agent}} | State#state.transactions],
	{ok, State#state{transactions = Newtrans, checkend = true}};
handle_event({hangup, #call{id = CallID} = Call, By}, #state{id = CallID} = State) ->
	?NOTICE("~s hungup by ~s", [CallID, By]),
	Newtrans = [{hangup, {CallID, By}} | State#state.transactions],
	case check_end(Newtrans) of
		{true, Num} when Num > 0 ->
			summarize(Newtrans),
			remove_handler;
		_Else ->
			{ok, State#state{transactions = Newtrans, checkend = true}}
	end;
handle_event({wrapup, #call{id = CallID} = Call, Agent}, #state{id = CallID} = State) ->
	?NOTICE("~s started wrapup for ~s", [Agent, CallID]),
	Newtrans = [{wrapup, {CallID, Agent}} | State#state.transactions],
	{ok, State#state{transactions = Newtrans}};
handle_event({endwrapup, #call{id = CallID} = Call, Agent}, #state{id = CallID} = State) ->
	?NOTICE("~s ended wrapup for ~s", [Agent, CallID]),
	Newtrans = [{endwrapup, {CallID, Agent}} | State#state.transactions],
	?NOTICE("Checkend ~s", [State#state.checkend]),
	case State#state.checkend of
		true ->
			case check_end(Newtrans) of
				{true, _Num} ->
					summarize(Newtrans),
					remove_handler;
				{false, _Num} ->
					{ok, State#state{transactions = Newtrans}}
			end;
		false ->
			{ok, State#state{transactions = Newtrans}}
	end;
handle_event({transfer, #call{id = CallID} = Call, Transferto}, #state{id = Callid} = State) ->
	?NOTICE("~s has gotten a transfer of ~s", [Transferto, Callid]),
	Newtrans = [{transfer, {CallID, Transferto}} | State#state.transactions],
	{ok, State#state{transactions = Newtrans}};
%handle_event({abandonqueue, #call{id = Callid} = Call, Queue}, #state{id = Callid} = State) ->
%	?NOTICE("~s has abandoned in queue ~s", [Callid, Queue]),
%	{ok, State};
handle_event(_Event, State) ->
	{ok, State}.

handle_call(transactions, State) ->
	{ok, State#state.transactions, State};
handle_call(_Request, State) ->
	{ok, ok, State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(Args, State) ->
	?NOTICE("terminating with args ~p", [Args]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

check_end(Transactions) ->
	Wrapups = length(proplists:get_all_values(wrapup, Transactions)),
	Endwraps = length(proplists:get_all_values(endwrapup, Transactions)),
	{Wrapups =:= Endwraps, Wrapups}.
	
summarize(Transactions) ->
	?NOTICE("summarizing transactions:  ~p", [Transactions]),
	ok.




%endpoints:
%abadon in queue
%abaond in ivr (nyi)
%hangup
%endwrapup
%
%
%
%hangup's will equal one, making it ready for summary.
%To be certain, check if count(endwrapup) =:= count(wrapup)

-ifdef(EUNIT).

check_end_test_() ->
	{foreach,
	fun() ->
		start(),
		#call{id = "testcall", source=self()}
	end,
	fun(#call{id = Id}) ->
		gen_event:delete_handler(cdr, {?MODULE, Id}, []),
		ok
	end,
	[fun(Call) ->
		{"Simple startup",
		fun() ->
			?assertEqual(ok, cdrinit(Call)),
			?assertEqual([], transactions(Call))
		end}
	end,
	fun(Call) ->
		{"Normal call flow:  inqueue -> ringing -> oncall -> agent hangup -> wrapup -> endwrap",
		fun() ->
			?assertEqual(ok, cdrinit(Call)),
			inqueue(Call, "testqueue"),
			ringing(Call, "agent"),
			oncall(Call, "agent"),
			hangup(Call, agent),
			wrapup(Call, "agent"),
			Transactions = transactions(Call),
			?assertEqual({true, 1}, check_end([{endwrapup, {Call#call.id, "agent"}} | Transactions])),
			Teststate = #state{id = Call#call.id, checkend = true, transactions = Transactions},
			?assertEqual(remove_handler, handle_event({endwrapup, Call, "agent"}, Teststate)),
			endwrapup(Call, "agent"),
			?assertEqual({error, bad_module}, transactions(Call))
		end}
	end,
	fun(Call) ->
		{"Normal call flow with caller hangup",
		fun() ->
			?assertEqual(ok, cdrinit(Call)),
			inqueue(Call, "testqueue"),
			ringing(Call, "agent"),
			oncall(Call, "agent"),
			hangup(Call, "caller"),
			wrapup(Call, "agent"),
			Transactions = transactions(Call),
			?assertEqual({true, 1}, check_end([{endwrapup, {Call#call.id, "agent"}} | Transactions])),
			Teststate = #state{id = Call#call.id, checkend = true, transactions = Transactions},
			?assertEqual(remove_handler, handle_event({endwrapup, Call, "agent"}, Teststate))
		end}
	end,
	fun(Call) ->
		{"Call flow with a transfer to an agent",
		fun() ->
			?assertEqual(ok, cdrinit(Call)),
			inqueue(Call, "testqueue"),
			ringing(Call, "agent"),
			oncall(Call, "agent"),
			transfer(Call, "agent"),
			oncall(Call, "agent2"),
			wrapup(Call, "agent"),
			endwrapup(Call, "agent"),
			Trans = transactions(Call),
			?assertEqual({true, 1}, check_end(Trans)),
			?assertMatch({ok, #state{checkend = true}}, handle_event({hangup, Call, agent}, #state{id = Call#call.id, transactions = Trans})),
			hangup(Call, agent),
			wrapup(Call, "agent2"),
			Trans2 = transactions(Call),
			?assertEqual({false, 2}, check_end(Trans2)),
			?assertEqual(remove_handler, handle_event({endwrapup, Call, "agent2"}, #state{id = Call#call.id, transactions = Trans2, checkend = true})),
			endwrapup(Call, "agent2"),
			?assertEqual({error, bad_module}, transactions(Call))
		end}
	end]}.




%do_test_() ->
%	{foreach,
%	fun() ->
%		ok
%	end,
%	fun(ok) ->
%		ok
%	end,
%	[fun(ok) ->
%		{"test",
%		fun() ->
%			?assert(false)
%		end}
%	end]}.
			
-endif.

	