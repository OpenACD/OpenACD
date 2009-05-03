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
	transfer/2,
	status/1
]).

-export([
	init/1,
	handle_event/2,
	handle_call/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).


-type(transaction_type() :: 'inqueue' | 'ringing' | 'oncall' | 'wrapup').
-type(callid() :: string()).
-type(time() :: integer()).
-type(datalist() :: [{atom(), string()}]).
-type(transaction() :: {transaction_type(), callid(), time(), datalist()}).
-type(transactions() :: [transaction()]).

%-type(proto_transactions() :: [proto_transaction()]).
%	-type(proto_transaction() :: {transaction_type(), callid(), time(), datalist()}).

-record(state, {
	id :: callid(),
	transactions = [] :: transactions(),
	unterminated = [] :: transactions(),
	hangup = false :: bool(),
	wrapup = false :: bool()
}).

%event -> terminated by
%inqueue -> ringing
%ringing -> ringing, oncall
%oncall -> wrapup
%wrapup -> endwrapup
%
%event -> initialted by
%inqueue ->
%ringing -> inqueue, ringing
%oncall -> ringing, transfer
%wrapup -> oncall
%endwrapup -> wrapup
%
%event -> branched by
%oncall -> transfer

check_split(Fun, List) ->
	?DEBUG("checking split with list ~p", [List]),
	case lists:splitwith(Fun, List) of
		{List, []} ->
			?ERROR("Split check failed!", []),
			erlang:error("Split check failed");
		{Head, [Tuple | Tail]} ->
			?DEBUG("Head:  ~p;  Tuple:  ~p;  Tail:  ~p", [Head, Tuple, Tail]),
			{Tuple, lists:append(Head, Tail)}
	end.

find_initiator({ringing, _Time, _Datalist}, Unterminated) ->
	F = fun(I) ->
		case I of
			{ringing, _Oldtime, _Data} ->
				false;
			{inqueue, _Oldtime, _Data} ->
				false;
			_Other ->
				true
		end
	end,
	check_split(F, Unterminated);
find_initiator({oncall, _Time, Agent}, Unterminated) ->
	F = fun(I) ->
		case I of
			{ringing, _Oldtime, Agent} ->
				false;
			_Other ->
				true
		end
	end,
	check_split(F, Unterminated);
find_initiator({wrapup, _Time, Agent}, Unterminated) ->
	F = fun(I) ->
		case I of
			{oncall, _Oldtime, Agent} ->
				false;
			_Other ->
				true
		end
	end,
	check_split(F, Unterminated);
find_initiator({endwrapup, _Time, Agent}, Unterminated) ->
	F = fun(I) ->
		case I of
			{wrapup, _Oldtime, Agent} ->
				false;
			_Other ->
				true
		end
	end,
	check_split(F, Unterminated).

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
	catch gen_event:notify(cdr, {inqueue, Call, nowsec(now()), Queue}).

ringing(Call, Agent) ->
	catch gen_event:notify(cdr, {ringing, Call, nowsec(now()), Agent}).
	
oncall(Call, Agent) ->
	catch gen_event:notify(cdr, {oncall, Call, nowsec(now()), Agent}).

hangup(Call, By) ->
	catch gen_event:notify(cdr, {hangup, Call, nowsec(now()), By}).

wrapup(Call, Agent) ->
	catch gen_event:notify(cdr, {wrapup, Call, nowsec(now()), Agent}).

endwrapup(Call, Agent) ->
	catch gen_event:notify(cdr, {endwrapup, Call, nowsec(now()), Agent}).

transfer(Call, Transferto) ->
	catch gen_event:notify(cdr, {transfer, Call, nowsec(now()), Transferto}).

status(Call) ->
	gen_event:call(cdr, {?MODULE, Call#call.id}, status).

%% Gen event callbacks
init([Call]) ->
	?NOTICE("Starting new CDR handler for ~s", [Call#call.id]),
	{ok, #state{id=Call#call.id}}.

handle_event({inqueue, #call{id = CallID}, Time, Queuename}, #state{id = CallID} = State) ->
	?NOTICE("~s has joined queue ~s", [CallID, Queuename]),
	Unterminated = [{inqueue, Time, Queuename} | State#state.unterminated],
	{ok, State#state{unterminated = Unterminated}};
handle_event({ringing, #call{id = CallID}, Time, Agent}, #state{id = CallID} = State) ->
	?NOTICE("~s is ringing to ~s", [CallID, Agent]),
	{{Event, Oldtime, Data}, Midunterminated} = find_initiator({ringing, Time, Agent}, State#state.unterminated),
	Newuntermed = [{ringing, Time, Agent} | Midunterminated],
	Newtrans = [{Event, Oldtime, Time, Time - Oldtime, Data} | State#state.transactions],
	{ok, State#state{transactions = Newtrans, unterminated = Newuntermed}};
handle_event({oncall, #call{id = CallID}, Time, Agent}, #state{id = CallID} = State) ->
	?NOTICE("~s is on call with ~s.", [Agent, CallID]),
	{{Event, Oldtime, Data}, Midunterminated} = find_initiator({oncall, Time, Agent}, State#state.unterminated),
	Newuntermed = [{oncall, Time, Agent} | Midunterminated],
	Newtrans = [{Event, Oldtime, Time, Time - Oldtime, Data} | State#state.transactions],
	{ok, State#state{transactions = Newtrans, unterminated = Newuntermed}};
handle_event({hangup, _Callrec, _Time, _By}, #state{hangup = true} = State) ->
	% already got a hangup, so we don't care.
	{ok, State};
handle_event({hangup, #call{id = CallID}, Time, agent}, #state{id = CallID} = State) ->
	?NOTICE("hangup for ~s", [CallID]),
	Newtrans = [{hangup, Time, Time, 0, agent} | State#state.transactions],
	{ok, State#state{transactions = Newtrans, hangup = true}};
handle_event({hangup, #call{id = CallID}, Time, By}, #state{id = CallID} = State) ->
	?NOTICE("Hangup for ~s", [CallID]),
	Newtrans = [{hangup, Time, Time, 0, By} | State#state.transactions],
	{ok, State#state{transactions = Newtrans, hangup = true}};
handle_event({wrapup, #call{id = CallID}, Time,  Agent}, #state{id = CallID} = State) ->
	?NOTICE("~s wrapup for ~s", [Agent, CallID]),
	{{Event, Oldtime, Data}, Midunterminated} = find_initiator({wrapup, Time, Agent}, State#state.unterminated),
	Newtrans = [{Event, Oldtime, Time, Time - Oldtime, Data} | State#state.transactions],
	Newuntermed = [{wrapup, Time, Agent} | Midunterminated],
	{ok, State#state{transactions = Newtrans, unterminated = Newuntermed}};
handle_event({endwrapup, #call{id = CallID}, Time, Agent}, #state{id = CallID} = State) ->
	?NOTICE("~s ended wrapup for ~s", [Agent, CallID]),
	{{Event, Oldtime, Data}, Midunterminated} = find_initiator({endwrapup, Time, Agent}, State#state.unterminated),
	Newtrans = [{Event, Oldtime, Time, Time - Oldtime, Data} | State#state.transactions],
	case {State#state.hangup, Midunterminated} of
		{true, []} ->
			summarize(Newtrans),
			?DEBUG("Summarize complete, now to remove the handler...", []),
			remove_handler;
		_Else ->
			{ok, State#state{unterminated = Midunterminated, transactions = Newtrans}}
	end;
handle_event({transfer, #call{id = CallID}, Time, Transferto}, #state{id = CallID} = State) ->
	?NOTICE("~s has gotten a transfer of ~s", [Transferto, CallID]),
	Newtrans = [{transfer, Time, Time, 0, Transferto} | State#state.transactions],
	{ok, State#state{transactions = Newtrans}};
handle_event(_Event, State) ->
	{ok, State}.

handle_call(status, State) ->
	{ok, {State#state.transactions, State#state.unterminated}, State};
handle_call(_Request, State) ->
	{ok, ok, State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(Args, _State) ->
	?NOTICE("terminating with args ~p", [Args]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Need to keep in mind that not all agents are to be billed the same,
%% so there does need to be some form of pair checking.
% pair checking is done as the transactions are built up.
% All this needs to do is sum up the data.
summarize(Transactions) ->
	?DEBUG("Summarizing ~p", [Transactions]),
	Acc = dict:from_list([
		{total, {0, 0, 0, 0}}
	]),
	Count = fun({Event, _State, _End, Duration, Data}, Dict) ->
		{ok, {Inqueue, Ringing, Oncall, Wrapup}} = dict:find(total, Dict),
		case Event of
			inqueue ->
				dict:store(total, {Inqueue + Duration, Ringing, Oncall, Wrapup}, Dict);
			ringing ->
				{_Aqueue, Aring, Aoncall, Awrapup} = case dict:find(Data, Dict) of
					error ->
						{0, 0, 0, 0};
					{ok, Tuple} when is_tuple(Tuple) ->
						Tuple
				end,
				Dict2 = dict:store(total, {Inqueue, Ringing + Duration, Oncall, Wrapup}, Dict),
				dict:store(Data, {0, Aring + Duration, Aoncall, Awrapup}, Dict2);
			oncall ->
				{_Aqueue, Aring, Aoncall, Awrapup} = case dict:find(Data, Dict) of
					error ->
						{0, 0, 0, 0};
					{ok, Tuple} when is_tuple(Tuple) ->
						Tuple
				end,
				Dict2 = dict:store(total, {Inqueue, Ringing, Oncall + Duration, Wrapup}, Dict),
				dict:store(Data, {0, Aring, Aoncall + Duration, Awrapup}, Dict2);
			wrapup ->
				{_Aqueue, Aring, Aoncall, Awrapup} = case dict:find(Data, Dict) of
					error ->
						{0, 0, 0, 0};
					{ok, Tuple} when is_tuple(Tuple) ->
						Tuple
				end,
				Dict2 = dict:store(total, {Inqueue, Ringing, Oncall, Wrapup + Duration}, Dict),
				dict:store(Data, {0, Aring, Aoncall, Awrapup + Duration}, Dict2);
			_Other ->
				?DEBUG("Can't summarize ~s", [Event]),
				Dict
		end
	end,
	lists:foldl(Count, Acc, Transactions).

nowsec({Mega, Sec, _Micro}) ->
	(Mega * 1000000) + Sec.

-ifdef(EUNIT).

check_split_test_() ->
	[{"List ends up all on the left",
	fun() ->
		F = fun(_I) ->
			true
		end,
		List = [{a}, {b}, {c}, {d}],
		?assertError("Split check failed", check_split(F, List))
	end},
	{"List ends up all on the right",
	fun() ->
		F = fun(_I) ->
			false
		end,
		List = [{a}, {b}, {c}, {d}],
		?assertEqual({{a}, [{b}, {c}, {d}]}, check_split(F, List))
	end},
	{"Get the correct split from a b b b c",
	fun() ->
		F = fun(I) ->
			case I of
				{b} ->
					false;
				_Else ->
					true
			end
		end,
		List = [{a}, {b}, {b}, {b}, {c}],
		?assertEqual({{b}, [{a}, {b}, {b}, {c}]}, check_split(F, List))
	end}].

find_initiator_test_() ->
	[{"ringing with only inqueue before it",
	fun() ->
		Unterminated = [{inqueue, 10, "queuename"}],
		?assertEqual({{inqueue, 10, "queuename"}, []}, find_initiator({ringing, 15, "agent"}, Unterminated))
	end},
	{"ringing with only ringing before it",
	fun() ->
		Unterminated = [{ringing, 10, "agent1"}],
		?assertEqual({{ringing, 10, "agent1"}, []}, find_initiator({ringing, 15, "agent2"}, Unterminated))
	end},
	{"oncall with a ringing before it",
	fun() ->
		Unterminated = [{ringing, 10, "agent"}],
		?assertEqual({{ringing, 10, "agent"}, []}, find_initiator({oncall, 15, "agent"}, Unterminated))
	end},
	{"wraupup with an oncall before it",
	fun() ->
		Unterminated = [{oncall, 10, "agent"}],
		?assertEqual({{oncall, 10, "agent"}, []}, find_initiator({wrapup, 15, "agent"}, Unterminated))
	end},
	{"endwrapup with a wrapup before it",
	fun() ->
		Unterminated = [{wrapup, 10, "agent"}],
		?assertEqual({{wrapup, 10, "agent"}, []}, find_initiator({endwrapup, 15, "agent"}, Unterminated))
	end},
	{"endwrapup with two wrapups before it",
	fun() ->
		Unterminated = [{wrapup, 10, "agent1"}, {wrapup, 5, "agent2"}],
		Res = find_initiator({endwrapup, 15, "agent1"}, Unterminated),
		?CONSOLE("res:  ~p", [Res]),
		?assertEqual({{wrapup, 10, "agent1"}, [{wrapup, 5, "agent2"}]}, Res)
	end},
	{"endwrapup with wrapup in the middle of the list",
	fun() ->
		Unterminated = [{oncall, 10, "ignore"}, {wrapup, 10, "catch"}, {wrapup, 5, "alsoignore"}],
		Res = find_initiator({endwrapup, 15, "catch"}, Unterminated),
		?CONSOLE("res:  ~p", [Res]),
		?assertEqual({{wrapup, 10, "catch"}, [{oncall, 10, "ignore"}, {wrapup, 5, "alsoignore"}]}, Res)
	end}].

handle_event_test_() ->
	[{"handle_event inqueue",
	fun() ->
		Call = #call{id = "testcall", source=self()},
		State = #state{id = "testcall"},
		{ok, Newstate} = handle_event({inqueue, Call, 10, "testqueue"}, State),
		?assertEqual([], Newstate#state.transactions),
		?assertEqual([{inqueue, 10, "testqueue"}], Newstate#state.unterminated)
	end},
	{"ringing",
	fun() ->
		Call = #call{id = "testcall", source=self()},
		Unterminated = [{inqueue, 10, "testqueue"}],
		State = #state{unterminated = Unterminated, id = "testcall"},
		{ok, Newstate} = handle_event({ringing, Call, 15, "agent"}, State),
		?assertEqual([{inqueue, 10, 15, 5, "testqueue"}], Newstate#state.transactions),
		?assertEqual([{ringing, 15, "agent"}], Newstate#state.unterminated)
	end},
	{"oncall",
	fun() ->
		Call = #call{id = "testcall", source=self()},
		Unterminated = [{ringing, 10, "agent"}],
		State = #state{unterminated = Unterminated, id = "testcall"},
		{ok, Newstate} = handle_event({oncall, Call, 15, "agent"}, State),
		?assertEqual([{ringing, 10, 15, 5, "agent"}], Newstate#state.transactions),
		?assertEqual([{oncall, 15, "agent"}], Newstate#state.unterminated)
	end},
	{"wrapup",
	fun() ->
		Call = #call{id = "testcall", source=self()},
		Unterminated = [{oncall, 10, "agent"}],
		State = #state{unterminated = Unterminated, id="testcall"},
		{ok, Newstate} = handle_event({wrapup, Call, 15, "agent"}, State),
		?assertEqual([{oncall, 10, 15, 5, "agent"}], Newstate#state.transactions),
		?assertEqual([{wrapup, 15, "agent"}], Newstate#state.unterminated)
	end},
	{"endwrapup",
	fun() ->
		Call = #call{id = "testcall", source=self()},
		Unterminated = [{wrapup, 10, "agent"}],
		State = #state{unterminated = Unterminated, id="testcall"},
		{ok, Newstate} = handle_event({endwrapup, Call, 15, "agent"}, State),
		?assertEqual([{wrapup, 10, 15, 5, "agent"}], Newstate#state.transactions),
		?assertEqual([], Newstate#state.unterminated)
	end},
	{"hangup from agent",
	fun() ->
		Call = #call{id = "testcall", source=self()},
		State = #state{id = "testcall"},
		{ok, Newstate} = handle_event({hangup, Call, 10, agent}, State),
		?assertEqual([{hangup, 10, 10, 0, agent}], Newstate#state.transactions),
		?assertEqual([], Newstate#state.unterminated),
		?assert(Newstate#state.hangup)
	end},
	{"hangup from caller",
	fun() ->
		Call = #call{id = "testcall", source=self()},
		State = #state{id = "testcall"},
		{ok, Newstate} = handle_event({hangup, Call, 10, "caller"}, State),
		?assertEqual([{hangup, 10, 10, 0, "caller"}], Newstate#state.transactions),
		?assertEqual([], Newstate#state.unterminated),
		?assert(Newstate#state.hangup)
	end},
	{"hangup from caller when a hangup is already received",
	fun() ->
		Call = #call{id = "testcall", source = self()},
		Protostate = #state{id = "testcall"},
		{ok, State} = handle_event({hangup, Call, 10, agent}, Protostate),
		{ok, Newstate} = handle_event({hangup, Call, 10, "notagent"}, State),
		?assert(Newstate#state.hangup),
		?assertEqual(State#state.transactions, Newstate#state.transactions)
	end},
	{"hangup from agent when hangup from caller is already recieved",
	fun() ->
		Call = #call{id = "testcall", source = self()},
		Protostate = #state{id = "testcall"},
		{ok, State} = handle_event({hangup, Call, 10, "notagent"}, Protostate),
		{ok, Newstate} = handle_event({hangup, Call, 10, agent}, State),
		?assert(Newstate#state.hangup),
		?assertEqual(State#state.transactions, Newstate#state.transactions)
	end},
	{"transfer event",
	fun() ->
		Call = #call{id = "testcall", source = self()},
		State = #state{id = "testcall"},
		{ok, Newstate} = handle_event({transfer, Call, 10, "target"}, State),
		?assertEqual([], Newstate#state.unterminated),
		?assertEqual([{transfer, 10, 10, 0, "target"}], Newstate#state.transactions)
	end},
	{"endwrapup when a hangup has already been recieved",
	fun() ->
		Call = #call{id = "testcall", source = self()},
		State = #state{id = "testcall", hangup = true, unterminated = [{wrapup, 10, "agent"}]},
		?assertEqual(remove_handler, handle_event({endwrapup, Call, 15, "agent"}, State))
	end},
	{"handling an event for a differnt call id (ie, not handling it)",
	fun() ->
		Call = #call{id = "testcall", source = self()},
		State = #state{id = "differs"},
		{ok, Newstate} = handle_event({inqueue, Call, 15, "queue"}, State),
		?assertEqual(State, Newstate)
	end}].

summarize_test_() ->
	[{"Simple summary, one call, one agent",
	fun() ->
		Transactions = [
			{inqueue, 5, 10, 5, "testqueue"},
			{ringing, 10, 13, 3, "agent"},
			{oncall, 13, 20, 7, "agent"},
			{wrapup, 20, 24, 4, "agent"}
		],
		Dict = summarize(Transactions),
		?assertEqual({5, 3, 7, 4}, dict:fetch(total, Dict)),
		?assertEqual({0, 3, 7, 4}, dict:fetch("agent", Dict))
	end},
	{"summary with a ringout",
	fun() ->
		Transactions = [
			{inqueue, 5, 10, 5, "testqueue"},
			{ringing, 10, 20, 10, "agent1"},
			{ringing, 20, 28, 8, "agent2"},
			{oncall, 28, 22, 5, "agent2"},
			{wrapup, 22, 23, 1, "agent2"}
		],
		Dict = summarize(Transactions),
		?assertEqual({5, 18, 5, 1}, dict:fetch(total, Dict)),
		?assertEqual({0, 10, 0, 0}, dict:fetch("agent1", Dict)),
		?assertEqual({0, 8, 5, 1}, dict:fetch("agent2", Dict))
	end},
	{"summary with a transfer to another agent",
	fun() ->
		Transactions = [
			{inqueue, 5, 10, 5, "testqueue"},
			{ringing, 10, 20, 10, "agent1"},
			{oncall, 20, 30, 10, "agent1"},
			{transfer, 30, 30, 0, "agent2"},
			{oncall, 30, 35, 5, "agent2"},
			{wrapup, 30, 40, 10, "agent1"},
			{wrapup, 35, 40, 5, "agent2"}
		],
		Dict = summarize(Transactions),
		?assertEqual({5, 10, 15, 15}, dict:fetch(total, Dict)),
		?assertEqual({0, 10, 10, 10}, dict:fetch("agent1", Dict)),
		?assertEqual({0, 0, 5, 5}, dict:fetch("agent2", Dict))
	end}].

-endif.

