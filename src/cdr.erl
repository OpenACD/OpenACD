%%	The contents of this file are subject to the Common Public Attribution
%%	License Version 1.0 (the “License”); you may not use this file except
%%	in compliance with the License. You may obtain a copy of the License at
%%	http://opensource.org/licenses/cpal_1.0. The License is based on the
%%	Mozilla Public License Version 1.1 but Sections 14 and 15 have been
%%	added to cover use of software over a computer network and provide for
%%	limited attribution for the Original Developer. In addition, Exhibit A
%%	has been modified to be consistent with Exhibit B.
%%
%%	Software distributed under the License is distributed on an “AS IS”
%%	basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%	License for the specific language governing rights and limitations
%%	under the License.
%%
%%	The Original Code is Spice Telephony.
%%
%%	The Initial Developers of the Original Code is 
%%	Andrew Thompson and Micah Warren.
%%
%%	All portions of the code written by the Initial Developers are Copyright
%%	(c) 2008-2009 SpiceCSM.
%%	All Rights Reserved.
%%
%%	Contributor(s):
%%
%%	Andrew Thompson <athompson at spicecsm dot com>
%%	Micah Warren <mwarren at spicecsm dot com>
%%

%% @doc A gen_event based module to create and record Call Data Records to mnesia.

-module(cdr).
-behaviour(gen_event).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("call.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([
	start/0,
	start_link/0,
	cdrinit/1,
	inqueue/2,
	ringing/2,
	oncall/2,
	hangup/2,
	wrapup/2,
	endwrapup/2,
	transfer/2,
	voicemail/2,
	agent_transfer/2,
	status/1,
	merge/3
]).

-export([
	init/1,
	handle_event/2,
	handle_call/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).




-type(transaction_type() :: 
	'cdrinit' | 
	'inivr' | 
	'dialoutgoing' |
	'inqueue' |
	'ringing' |
	'precall' |
	'oncall' |
	'outgoing' |
	'failedoutgoing' |
	'agent_transfer' |
	'queue_transfer' |
	'warmxfer' |
	'warmxfercomplete' |
	'warmxferfailed' |
	'warmxferleg' |
	'wrapup' |
	'endwrapup' |
	'abandonqueue' |
	'abandonivr' |
	'voicemail' |
	'hangup' |
	'undefined' |
	'cdrend'
).
-type(callid() :: string()).
-type(time() :: integer()).
-type(datalist() :: [{atom(), string()}]).
-type(proplist() :: [{any(), any()}]).
-type(transaction() :: {transaction_type(), callid(), time(), datalist()}).
-type(transactions() :: [transaction()]).
-type(raw_transaction() :: {transaction_type(), time(), any()}).
%-type(proto_transactions() :: [proto_transaction()]).
%	-type(proto_transaction() :: {transaction_type(), callid(), time(), datalist()}).

-record(state, {
	id :: callid(),
	callrec :: #call{},
	nodes = [] :: [atom()],
	limbo_wrapup_count = 0 :: non_neg_integer(),
	hangup = false :: 'false' | 'true'
}).
	
-record(cdr_rec, {
	media :: #call{},
	summary = inprogress :: 'inprogress' | proplist(),
	transactions = inprogress :: 'inprogress' | transactions(),
	timestamp = util:now() :: time(),
	nodes = [] :: [atom()]
}).

-record(cdr_raw, {
	id :: callid(),
	transaction :: transaction_type(),
	eventdata :: any(),
	start = util:now() :: time(),
	ended :: 'undefined' | time(),
	terminates = [] :: [transaction_type()] | 'infoevent',
	timestamp = util:now() :: time(),
	nodes = [] :: [atom()]
}).

-type(state() :: #state{}).
-define(GEN_EVENT, true).
-include("gen_spec.hrl").

%% =====
%% API
%% =====

%% @doc starts the cdr event server.
-spec(start/0 :: () -> {'ok', pid()}).
start() ->
	build_tables(),
	gen_event:start({local, ?MODULE}).

%% @doc Starts the cdr event server linked.
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() ->
	build_tables(),
	gen_event:start_link({local, ?MODULE}).

%% @doc Create a handler specifically for `#call{} Call' with default options.
-spec(cdrinit/1 :: (Call :: #call{}) -> 'ok' | 'error').
cdrinit(Call) ->
	try gen_event:add_handler(cdr, {?MODULE, Call#call.id}, [Call]) of
		ok ->
			ok;
		Else ->
			?ERROR("Initializing CDR for ~s erred with: ~p", [Call#call.id, Else]),
			error
	catch
		What:Why ->
			?ERROR("Initializing CDR for ~s erred with: ~p:~p", [Call#call.id, What, Why]),
			error
	end.
	
%% @doc Notify cdr handler that `#call{} Call' is now in queue `string() Queue'.
-spec(inqueue/2 :: (Call :: #call{}, Queue :: string()) -> 'ok').
inqueue(Call, Queue) ->
	event({inqueue, Call, util:now(), Queue}).

%% @doc Notify cdr handler that `#call{} Call' is now ringing to `string() Agent'.
-spec(ringing/2 :: (Call :: #call{}, Agent :: string() | pid()) -> 'ok').
ringing(Call, Agent) when is_pid(Agent) ->
	ringing(Call, agent_manager:find_by_pid(Agent));
ringing(Call, Agent) ->
	event({ringing, Call, util:now(), Agent}).

%% @doc Notify cdr handler that `#call{} Call' has rungout from `string() Agent'.
-spec(ringout/2 :: (Call :: #call{}, Agent :: string() | pid()) -> 'ok').
ringout(Call, Agent) when is_pid(Agent) ->
	ringout(Call, agent_manager:find_by_pid(Agent));
ringout(Call, Agent) ->
	event({ringout, Call, util:now(), Agent}).

%% @doc Notify cdr handler that `#call{} Call' is currently oncall with `string() Agent'.
-spec(oncall/2 :: (Call :: #call{}, Agent :: string() | pid()) -> 'ok').
oncall(Call, Agent) when is_pid(Agent) ->
	oncall(Call, agent_manager:find_by_pid(Agent));
oncall(Call, Agent) ->
	event({oncall, Call, util:now(), Agent}).

%% @doc Notify cdr handler that `#call{} Call' has been hungup by `string() | agent By'.
-spec(hangup/2 :: (Call :: #call{}, By :: string() | 'agent') -> 'ok').
hangup(Call, By) ->
	event({hangup, Call, util:now(), By}).

%% @doc Notify cdr handler that `#call{} Call' has been put in wrapup by `string() Agent'.
-spec(wrapup/2 :: (Call :: #call{}, Agent :: string() | pid()) -> 'ok').
wrapup(Call, Agent) when is_pid(Agent) ->
	wrapup(Call, agent_manager:find_by_pid(Agent));
wrapup(Call, Agent) ->
	event({wrapup, Call, util:now(), Agent}).

%% @doc Notify cdr handler that `#call{} Call' has had a wrapup ended by `string() Agent'.
-spec(endwrapup/2 :: (Call :: #call{}, Agent :: string()) -> 'ok').
endwrapup(Call, Agent) ->
	event({endwrapup, Call, util:now(), Agent}).

%% @doc Notify cdr handler that `#call{} Call' is to be transfered to `string() Transferto'.
-spec(transfer/2 :: (Call :: #call{}, Transferto :: string()) -> 'ok').
transfer(Call, Transferto) ->
	event({transfer, Call, util:now(), Transferto}).

%% @doc Notify cdr handler that `#call{} Call' is being offered by `string() Offerer'
%% to `string() Recipient'.
-spec(agent_transfer/2 :: (Call :: #call{}, {Offerer :: string() | pid(), Recipient :: string() | pid()}) -> 'ok').
agent_transfer(Call, {Offerer, Recipient}) when is_pid(Offerer) ->
	agent_transfer(Call, {agent_manager:find_by_pid(Offerer), Recipient});
agent_transfer(Call, {Offerer, Recipient}) when is_pid(Recipient) ->
	agent_transfer(Call, {Offerer, agent_manager:find_by_pid(Recipient)});
agent_transfer(Call, {Offerer, Recipient}) ->
	event({agent_transfer, Call, util:now(), {Offerer, Recipient}}).

%% @doc Notify the cdr handler the `#call{} Call' is being sent to voicemail
%% from `string() Queue'.
-spec(voicemail/2 :: (Call :: #call{}, Queue :: pid() | string()) -> 'ok').
voicemail(Call, Qpid) when is_pid(Qpid) ->
	List = queue_manager:queues(),
	{value, {Queue, Qpid}} = lists:keysearch(Qpid, 2, List),
	voicemail(Call, Queue);
voicemail(Call, Queue) ->
	event({voicemail, Call, util:now(), Queue}).

event(Tuple) ->
	catch gen_event:notify(cdr, Tuple).
%% @doc Return the completed and partial transactions for `#call{} Call'.

-spec(status/1 :: (Call :: #call{} | string()) -> {[tuple()], [tuple()]}).
status(#call{id = Cid} = Call) ->
	status(Cid);
status(Cid) ->
	gen_event:call(cdr, {?MODULE, Cid}, status).

%% =====
%% Gen event callbacks
%%=====

%% @private
init([Call]) ->
	?NOTICE("Starting new CDR handler for ~s", [Call#call.id]),
	Nodes = case application:get_env(cpx, nodes) of
		undefined ->
			[node()];
		Else ->
			Else
	end,
	Cdrrec = #cdr_rec{media = Call, nodes = Nodes},
	Initraw = #cdr_raw{
		id = Call#call.id,
		transaction = cdrinit,
		eventdata = Call,
		terminates = infoevent,
		nodes = Nodes
	},
	mnesia:transaction(fun() -> 
		mnesia:write(Cdrrec), 
		mnesia:write(Initraw)
	end),
	{ok, #state{id=Call#call.id, callrec = Call, nodes = Nodes}}.

%% @private
handle_event({mutate, Oldid, Newcallrec}, #state{id = Oldid} = State) when is_record(Newcallrec, call) ->
	case mutate(Oldid, Newcallrec) of
		{atomic, ok} ->
			{ok, State#state{id = Newcallrec#call.id}};
		Else ->
			?ERROR("could not update cdr handle ~p to ~p:  ~p", [Oldid, Newcallrec, Else]),
			{ok, State}
	end;
handle_event({Transaction, #call{id = Callid} = Call, Time, Data}, #state{id = Callid, limbo_wrapup_count = Limbocount} = State) ->
	Ended = case lists:member(Transaction, [hangup, agent_transfer, voicemail]) of
		true ->
			Time;
		false ->
			undefined
	end,
	Cdr = #cdr_raw{
		id = Callid,
		transaction = Transaction,
		eventdata = Data,
		start = Time,
		ended = Ended,
		nodes = State#state.nodes
	},
	Termed = push_raw(Call, Cdr),
	Extra = analyze(Transaction, Call, Time, Data, Termed),
	mnesia:transaction(fun() ->
		lists:foreach(fun(Rec) ->
			mnesia:write(Rec#cdr_raw{nodes = State#state.nodes})
		end, Extra)
	end),
	Newstate = case Transaction of
		endwrapup ->
			State#state{limbo_wrapup_count = Limbocount - 1};
		oncall ->
			State#state{limbo_wrapup_count = Limbocount + 1};
		hangup ->
			State#state{hangup = true};
		Else ->
			State
	end,
	case {Newstate#state.hangup, Newstate#state.limbo_wrapup_count} of
		{true, 0} ->
			remove_handler;
		_Anything_else ->
			{ok, Newstate}
	end.

%% @private
handle_call(status, State) ->
	catch toolbar:start(),
	{ok, ok, State};
handle_call(_Request, State) ->
	{ok, ok, State}.

%% @private
handle_info(_Info, State) ->
	{ok, State}.

%% @private
terminate(remove_handler, #state{callrec = Callrec} = State) ->
	Now = util:now(),
	Cdr = #cdr_raw{
		id = Callrec#call.id,
		transaction = cdrend,
		start = Now,
		ended = Now,
		nodes = State#state.nodes
	},
	push_raw(Callrec, Cdr),
	spawn_summarizer(Callrec);
terminate(Args, _State) ->
	?NOTICE("terminating with args ~p", [Args]),
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% =====
%% Internal Functions
%% =====

spawn_summarizer(#call{id = Id}) ->
	ok.

mutate(Oldid, Newcallrec) ->
	F = fun() ->
		RawsQh = qlc:q([ R || R <- mnesia:table(cdr_raw), R#cdr_raw.id =:= Oldid]),
		SumsQh = qlc:q([ S || #cdr_rec{media = Media} = S <- mnesia:table(cdr_rec), Media#call.id =:= Oldid]),
		Raws = qlc:e(RawsQh),
		lists:foreach(fun(Rawrec) ->
			mnesia:delete_object(Rawrec),
			mnesia:write(Rawrec#cdr_raw{id= Newcallrec#call.id})
		end, Raws),
		Sums = qlc:e(SumsQh),
		lists:foreach(fun(Sumrec) ->
			mnesia:delete_object(Sumrec),
			mnesia:write(Sumrec#cdr_rec{media = Newcallrec})
		end, Sums),
		ok
	end,
	mnesia:transaction(F).

%% @private Push the raw transaction into the cdr_raw table.
-spec(push_raw/2 :: (Callrec :: #call{}, Trans :: #cdr_raw{}) -> {'atomic', 'ok'}).
push_raw(#call{id = Cid} = Callrec, #cdr_raw{id = Cid, start = Now} = Trans) ->
	F = fun() ->
		Untermed = find_untermed(Trans#cdr_raw.transaction, Callrec, Trans#cdr_raw.eventdata),
		Termedatoms = lists:map(fun(#cdr_raw{transaction = T}) -> T end, Untermed),
		?DEBUG("closing cdr records ~p", [Untermed]),
		Terminate = fun(Rec) ->
			mnesia:delete_object(Rec),
			mnesia:write(Rec#cdr_raw{ended = Now})
		end,
		lists:foreach(Terminate, Untermed),
		?DEBUG("Writing ~p", [Trans]),
		mnesia:write(Trans#cdr_raw{terminates = Termedatoms}),
		Termedatoms
	end,
	Out = mnesia:transaction(F),
	?INFO("push_raw:  ~p", [Out]),
	Out.

%% @doc Determine any info messages that should be input based on what the last
%% actual message ended.
-spec(analyze/5 :: (Trans :: transaction_type(), Call :: #call{}, Time :: time(), Data :: any(), Termed :: transaction_type()) -> [#cdr_raw{}]).
analyze(hangup, #call{id = Cid}, Time, _, [inivr]) ->
	[#cdr_raw{id = Cid, start = Time, ended = Time, transaction = abandonivr}];
analyze(hangup, #call{id = Cid}, Time, _, [inqueue]) ->
	[#cdr_raw{id = Cid, start = Time, ended = Time, transaction = abandonequeue}];
analyze(_, _, _, _, _) ->
	[].
	
-spec(find_untermed/3 :: (Event :: transaction_type(), Call :: #call{}, Data :: any()) -> [#cdr_raw{}]).
find_untermed(inivr, _, _) ->
	% Does not terminate anything
	[];
find_untermed(dialoutgoing, _, _) ->
	% info event, precall terminated by outgoing/oncall
	[];
find_untermed(inqueue, #call{id = Cid} = Call, Queuename) ->
	% queue to queue transfers
	QH = qlc:q([X || 
		X <- mnesia:table(cdr_raw), 
		X#cdr_raw.id =:= Cid, 
		X#cdr_raw.transaction =:= inqueue,
		X#cdr_raw.eventdata =/= Queuename,
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(ringing, _, _) ->
	% doesn't term anything, but not info event
	[];
find_untermed(precall, _, _) ->
	% same as above,
	[];
find_untermed(oncall, #call{id = Cid} = Call, Agent) ->
	% terminates precall so I don't have to change it should outgoing state go away.
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id =:= Cid,
		( ( (X#cdr_raw.transaction =:= ringing) and (X#cdr_raw.eventdata =:= Agent) ) orelse (X#cdr_raw.transaction =:= inqueue) orelse (X#cdr_raw.transaction =:= precall) ),
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(outgoing, #call{id = Cid} = Call, Agent) ->
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id =:= Cid,
		X#cdr_raw.transaction =:= precall,
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(failedoutgoing, _, _) ->
	% just an info
	[];
find_untermed(agent_transfer, _, _) ->
	% the associated oncall and ringings do the terminations.  This is info
	[];
find_untermed(queue_transfer, _, _) ->
	% inqueue to inqueue for the actual terminations
	[];
find_untermed(warmxfer, _, _) ->
	% start of new events, terminates noting (like ringing)
	[];
find_untermed(warmxferfailed, #call{id = Cid} = Call, _) ->
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id =:= Cid,
		(X#cdr_raw.transaction =:= warmxfer orelse X#cdr_raw.transaction =:= warmxferleg),
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(warmxferleg, #call{id = Cid} = Call, _) ->
	QH = qlc:q([ X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id =:= Cid,
		X#cdr_raw.transaction =:= warmxfer,
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(warmxfercomplete, #call{id = Cid} = Call, _) ->
	% a wrapup terminates the oncall that the agent will be in
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id =:= Cid,
		X#cdr_raw.ended =:= undefined,
		X#cdr_raw.transaction =:= warmxferleg
	]),
	qlc:e(QH);
find_untermed(wrapup, #call{id = Cid} = Call, Agent) ->
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id =:= Cid,
		X#cdr_raw.transaction =:= oncall,
		X#cdr_raw.eventdata =:= Agent,
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(endwrapup, #call{id = Cid} = Call, Agent) ->
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id =:= Cid,
		X#cdr_raw.transaction =:= wrapup,
		X#cdr_raw.eventdata =:= Agent,
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(ringout, #call{id = Cid} = Call, Agent) ->
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id =:= Cid,
		X#cdr_raw.transaction =:= ringing,
		X#cdr_raw.eventdata =:= Agent,
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(voicemail, #call{id = Cid} = Call, _Whatever) ->
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id =:= Cid,
		X#cdr_raw.transaction =:= inqueue,
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(hangup, #call{id = Cid} = Call, _Whatever) ->
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id =:= Cid,
		( (X#cdr_raw.transaction =:= inqueue) orelse (X#cdr_raw.transaction =:= inivr) ),
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(cdrend, #call{id = Cid} = Call, _Whatever) ->
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id =:= Cid,
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(_, _, _) ->
	%% some other event, prolly an info.  unknowns terminate nothing.
	[].
	
build_tables() ->
	util:build_table(cdr_rec, [
		{attributes, record_info(fields, cdr_rec)},
		{disc_copies, [node() | nodes()]}
	]),
	util:build_table(cdr_raw, [
		{attributes, record_info(fields, cdr_raw)},
		{disc_copies, [node() | nodes()]},
		{type, bag}
	]),
	ok.
%spawn_summarizer(Transactions, CallID) ->
%	Summarize = fun() ->
%		Summary = summarize(Transactions),
%		F = fun() ->
%			Nodes = case whereis(cdr_exporter) of
%				undefined ->
%					[];
%				Pid ->
%					cdr_exporter:get_nodes()
%			end,
%			mnesia:delete({cdr_rec, CallID}),
%			mnesia:write(#cdr_rec{
%				id = CallID,
%				summary = Summary,
%				transactions = Transactions,
%				%timestamp = util:now(),
%				nodes = Nodes
%			})
%		end,
%		mnesia:transaction(F)
%	end,
%	?DEBUG("Summarize inprogress with ~w...", [Summarize]),
%	spawn(Summarize).
	
%% Need to keep in mind that not all agents are to be billed the same,
%% so there does need to be some form of pair checking.
% pair checking is done as the transactions are built up.
% All this needs to do is sum up the data.

%% @doc Given `[#cdr_raw{}] Transactions' summarize how long the call was
%% in each state, and a break down of each agent involved.
%% dict :: key :: `transaction_type()' of `inqueue, ringing, oncall, wrapup'.
%% dict :: value :: `{total(), breakdown()}'
%% breakdown :: `[{agent_login() | queue_name(), total()}]'
-spec(summarize/1 :: (Transactions :: [#cdr_raw{}]) -> [dict()]).
summarize(Transactions) ->
	?DEBUG("Summarizing ~p", [Transactions]),
	summarize(Transactions, dict:new()).

-spec(summarize/2 :: (Transactions :: [#cdr_raw{}], Sumacc :: dict()) -> any()).
summarize([], Acc) ->
	dict:to_list(Acc);
summarize([#cdr_raw{transaction = inqueue} = Cdr | Tail], Acc) ->
	Newacc = summarize(Cdr, inqueue, Cdr#cdr_raw.eventdata, Acc),
	summarize(Tail, Newacc);
summarize([#cdr_raw{transaction = ringing} = Cdr | Tail], Acc) ->
	Newacc = summarize(Cdr, ringing, Cdr#cdr_raw.eventdata, Acc),
	summarize(Tail, Newacc);
summarize([#cdr_raw{transaction = oncall} = Cdr | Tail], Acc) ->
	Newacc = summarize(Cdr, oncall, Cdr#cdr_raw.eventdata, Acc),
	summarize(Tail, Newacc);
summarize([#cdr_raw{transaction = wrapup} = Cdr | Tail], Acc) ->
	Newacc = summarize(Cdr, wrapup, Cdr#cdr_raw.eventdata, Acc),
	summarize(Tail, Newacc);
summarize([_ | Tail], Acc) ->
	summarize(Tail, Acc).

-spec(summarize/4 :: (Cdr :: #cdr_raw{}, Catagory :: transaction_type(), Individual :: any(), Acc :: dict()) -> dict()).
summarize(Cdr, Catagory, Individual, Acc) ->
	{Total, Propdict} = case dict:find(Catagory, Acc) of
		error ->
			{0, []};
		Else ->
			Else
	end,
	Duration = Cdr#cdr_raw.ended - Cdr#cdr_raw.start,
	Detail = proplists:get_value(Individual, Propdict, 0),
	Cleanedprops = proplists:delete(Individual, Propdict),
	Newdetail = Detail + Duration,
	Newtotal = Total + Duration,
	Newprops = [{Individual, Newdetail} | Cleanedprops],
	Newacc = dict:store(Catagory, {Newtotal, Newprops}, Acc),
	Newacc.

%% @doc Given the list of nodes, merge the cdrs.  Since this can take a long
%% time, it can help to just spawn a new process for it.
-spec(merge/3 :: (Nodes :: [atom()], Time :: pos_integer(), Replyto :: pid()) -> 'ok').
merge(Nodes, Time, Replyto) ->
	Raws = get_raws(Nodes, Time),
	Touchedcalls = get_ids(Raws),
	Sums = get_summaries(Nodes, Touchedcalls),
	Mergedraws = merge_raw(Raws),
	MergedSums = merge_sum(Sums),
	Replyto ! {merge_complete, ?MODULE, lists:append([Mergedraws, MergedSums])},
	ok.
	
%% @private
get_raws(Nodes, Time) ->
	get_raws(Nodes, Time, []).

%% @private
get_raws([], _Time, Acc) ->
	Acc;
get_raws([Node | Tail], Time, Acc) ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(cdr_raw), element(2, X#cdr_raw.transaction) =< Time]),
		qlc:e(QH)
	end,
	Out = rpc:call(Node, mnesia, transaction, [F]),
	get_raws(Tail, Time, [Out | Acc]).

%% @private
get_ids(Raws) ->
	get_ids(Raws, []).

%% @private
get_ids([], Acc) ->
	Acc;
get_ids([{atomic, Raws} | Tail], Acc) ->
	Newacc = get_ids_sub(Raws, Acc),
	get_ids(Tail, Newacc).

get_ids_sub([], Acc) ->
	Acc;
get_ids_sub([#cdr_raw{id = ID} | Tail], Acc) ->
	case lists:member(ID, Acc) of
		true ->
			get_ids_sub(Tail, Acc);
		false ->
			get_ids_sub(Tail, [ID | Acc])
	end.

%% @private
get_summaries(Nodes, Ids) ->
	get_summaries(Nodes, Ids, []).

%% @private
get_summaries([], _Ids, Acc) ->
	Acc;
get_summaries([Node | Nodes], Ids, Acc) ->
	F = fun() ->
		QH = qlc:q([X || #cdr_rec{media = Media} = X <- mnesia:table(cdr_rec), lists:member(Media#call.id, Ids)]),
		qlc:e(QH)
	end,
	case rpc:call(Node, mnesia, transaction, [F]) of
		{atomic, _Rows} = Rez ->
			get_summaries(Nodes, Ids, [Rez | Acc]);
		_Else ->
			?WARNING("Could not get cdr_rec from ~w", [Node]),
			get_summaries(Nodes, Ids, Acc)
	end.

%% @private
merge_raw(Recs) ->
	merge_raw(Recs, []).

merge_raw([], Acc) ->
	Acc;
merge_raw([{atomic, Raws} | Tail], Acc) ->
	Newacc = lists:append([Raws, Acc]),
	merge_raw(Tail, Newacc).

merge_sum(Sums) ->
	merge_sum(Sums, []).

merge_sum([], Acc) ->
	Acc;
merge_sum([{atomic, Sums} | Tail], Acc) ->
	Newacc = diff_sum(Sums, Acc),
	merge_sum(Tail, Newacc).

diff_sum(Left, Right) ->
	Sort = fun(#cdr_rec{media = MediaA} = A, #cdr_rec{media = MediaB} = B) ->
		MediaA#call.id < MediaB#call.id
	end,
	Sleft = lists:sort(Sort, Left),
	Sright = lists:sort(Sort, Right),
	diff_sum(Sleft, Sright, []).

diff_sum([], Right, Acc) ->
	lists:append([Right, Acc]);
diff_sum(Left, [], Acc) ->
	lists:append([Left, Acc]);
diff_sum([#cdr_rec{summary = inprogress, media = Media} = Hleft | Tleft], [#cdr_rec{summary = inprogress, media = Media} = _Hright | Tright], Acc) ->
	diff_sum(Tleft, Tright, [Hleft | Acc]);
diff_sum([#cdr_rec{summary = inprogress, media = Media} | Tleft], [#cdr_rec{media = Media} = Hright | Tright], Acc) ->
	diff_sum(Tleft, Tright, [Hright | Acc]);
diff_sum([#cdr_rec{media = Media} = Hleft | Tleft], [#cdr_rec{media = Media} = _Hright | Tright], Acc) ->
	diff_sum(Tleft, Tright, [Hleft | Acc]);
diff_sum([#cdr_rec{media = Lmedia} = Hleft | Tleft] = Left, [#cdr_rec{media = Rmedia} = Hright | Tright] = Right, Acc) ->
	case Lmedia#call.id < Rmedia#call.id of
		true ->
			diff_sum(Tleft, Right, [Hleft | Acc]);
		false ->
			diff_sum(Left, Tright, [Hright | Acc])
	end.

-ifdef(EUNIT).

analyze_test_() ->
	[{"hangup in inv",
	fun() ->
		Now = util:now(),
		Res = analyze(hangup, #call{id = "testcall", source = self()}, Now, "ivr", [inivr]),
		?assertMatch([#cdr_raw{id = "testcall", start = Now, ended = Now, transaction = abandonivr}], Res)
	end},
	{"hangup in q",
	fun() ->
		Now = util:now(),
		Res = analyze(hangup, #call{id = "testcall", source = self()}, Now, "queue", [inqueue]),
		?assertMatch([#cdr_raw{id = "testcall", start = Now, ended = Now, transaction = abandonequeue}], Res)
	end}].

push_raw_test_() ->
	{foreach,
	fun() ->
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		mnesia:create_schema([node()]),
		mnesia:start(),
		build_tables(),
		Pull = fun() ->
			{atomic, List} = mnesia:transaction(fun() -> mnesia:read(cdr_raw, "testcall") end),
			List
		end,
		Call = #call{id = "testcall", source = self()},
		init([Call]),
		Basedata = [
			{inivr, "na"},
			{dialoutgoing, "na"},
			{inqueue, "testqueue"},
			{ringing, "testagent"},
			{precall, "na"},
			{oncall, "testagent"},
			{outgoing, "na"},
			{failedoutgoing, "na"},
			{agent_transfer, "na"},
			{queue_transfer, "na"},
			{warmxfer, "na"},
			{warmxfercomplete, "na"},
			{warmxferfailed, "na"},
			{warmxferleg, "na"},
			{wrapup, "testagent"},
			{endwrapup, "testagent"},
			{abandonqueue, "testqueue"},
			{abandonivr, "na"},
			{ringout, "testagent"},
			{voicemail, "na"},
			{hangup, "na"},
			{annouce, "na"},
			{cdrend, "na"}
		],
		Seedfun = fun() ->
			lists:foreach(fun({Trans, Data}) ->
				Cdr = #cdr_raw{id = Call#call.id, transaction = Trans, eventdata = Data},
				mnesia:write(Cdr)
			end, Basedata)
		end,
		mnesia:transaction(Seedfun),
		Testfun = fun
			(_Fun, [], _Endedlist) ->
				ok;
			(_Fun, List, _Endedlist) when length(List) =:= 1 ->
				ok;
			(Fun, [Cdr | Tail], Endedlist) ->
				?DEBUG("~p", [Cdr#cdr_raw.transaction]),
				case lists:member(Cdr#cdr_raw.transaction, Endedlist) of
					true ->
						?assertNot(Cdr#cdr_raw.ended =:= undefined);
					false ->
						?assertEqual(undefined, Cdr#cdr_raw.ended)
				end,
				Fun(Fun, Tail, Endedlist)
		end,
		AssertEnded = fun(Input, ShouldbeEnded) ->
			Testfun(Testfun, Input, ShouldbeEnded)
		end,
		{Call, Pull, AssertEnded}
	end,
	fun(_Whatever) ->
		mnesia:stop(),
		mnesia:delete_schema([node()])
	end,
	[fun({Call, Pull, Ended}) ->
		{"ivr",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = inivr}),
			Ended(Pull(), [])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"dialoutgoing",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = dialoutgoing}),
			Ended(Pull(), [])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"inqueue",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = inqueue, eventdata = "newqueue"}),
			Testend = [inqueue],
			Ended(Pull(), Testend)
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"ringing",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = ringing, eventdata = "newagent"}),
			Testend = [],
			Ended(Pull(), Testend)
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"oncall",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = oncall, eventdata = "testagent"}),
			Testend = [inqueue, ringing, precall],
			Ended(Pull(), Testend)
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"outgoing",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = outgoing, eventdata = "testagent"}),
			Ended(Pull(), [precall])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"failedoutgoing",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = failedoutgoing}),
			Ended(Pull(), [])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"agent_transfer",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = agent_transfer}),
			Ended(Pull(), [])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"queue_transfer",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = queue_transfer}),
			Ended(Pull(), [])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"warmxfer",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = warmxfer}),
			Ended(Pull(), [])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"warmxferleg",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = warmxferleg}),
			Ended(Pull(), [warmxfer])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"warmxfercomplete",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = warmxfercomplete}),
			Ended(Pull(), [warmxferleg])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"warmxferfailed",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = warmxferfailed}),
			Ended(Pull(), [warmxferleg, warmxfer])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"wrapup",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = wrapup, eventdata = "testagent"}),
			Ended(Pull(), [oncall])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"endwrapup",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = endwrapup, eventdata = "testagent"}),
			Ended(Pull(), [wrapup])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"abandonequeue",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = abandonequeue}),
			Ended(Pull(), [])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"abandonivr",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = abandonivr}),
			Ended(Pull(), [])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"ringout",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = ringout, eventdata = "testagent"}),
			Ended(Pull(), [ringing])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"voicemail",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = voicemail, eventdata = "na"}),
			Ended(Pull(), [inqueue])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"hangup",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = hangup, eventdata = "na"}),
			Ended(Pull(), [inqueue, inivr])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"announce",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = announce, eventdata = "na"}),
			Ended(Pull(), [])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"cdrend",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = cdrend, eventdata = "na"}),
			Ended(Pull(), [
				cdrinit,
				inivr,
				dialoutgoing,
				inqueue,
				ringing,
				precall,
				oncall,
				outgoing,
				failedoutgoing,
				agent_transfer,
				queue_transfer,
				warmxfer,
				warmxfercomplete,
				warmxferfailed,
				warmxferleg,
				wrapup,
				endwrapup,
				abandonqueue,
				abandonivr,
				ringout,
				voicemail,
				hangup,
				annouce,
				cdrend
			])
		end}
	end]}.


% handle_event's primary duty is to see if the call is ready for summary.
handle_event_test_() ->
	{foreach,
	fun() ->
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		mnesia:create_schema([node()]),
		mnesia:start(),
		build_tables(),
		Call = #call{
			id = "testcall",
			source = self()
		},
		{ok, Basestate} = init([Call]),
		{Call, Basestate}
	end,
	fun(_) ->
		mnesia:stop(),
		mnesia:delete_schema([node()])
	end,
	[fun({Call, Basestate}) ->
		{"oncall increments",
		fun() ->
			{ok, Newstate} = handle_event({oncall, Call, util:now(), "testagent"}, Basestate),
			?assertEqual(Basestate#state.limbo_wrapup_count + 1, Newstate#state.limbo_wrapup_count),
			?assertNot(Newstate#state.hangup)
		end}
	end,
	fun({Call, #state{limbo_wrapup_count = C} = Basestate}) ->
		{"endwrapup decrements",
		fun() ->
			Instate = Basestate#state{limbo_wrapup_count = C + 1},
			{ok, Newstate} = handle_event({endwrapup, Call, util:now(), "testagent"}, Instate),
			?assertEqual(Instate#state.limbo_wrapup_count, Newstate#state.limbo_wrapup_count + 1)
		end}
	end]}.

merge_test_() ->
	[{"Get ids",
	fun() ->
		Raws = [{atomic, [
			#cdr_raw{id = "a"},
			#cdr_raw{id = "a"},
			#cdr_raw{id = "a"},
			#cdr_raw{id = "b"},
			#cdr_raw{id = "b"},
			#cdr_raw{id = "c"},
			#cdr_raw{id = "e"}]},
			{atomic, [
			#cdr_raw{id = "b"},
			#cdr_raw{id = "b"},
			#cdr_raw{id = "e"},
			#cdr_raw{id = "d"},
			#cdr_raw{id = "d"}]},
			{atomic, [
			#cdr_raw{id = "a"},
			#cdr_raw{id = "a"},
			#cdr_raw{id = "c"},
			#cdr_raw{id = "e"},
			#cdr_raw{id = "e"}]}],
		Sort = fun(A, B) ->
			A < B
		end,
		Expected = ["a", "b", "c", "d", "e"],
		Res = lists:sort(Sort, get_ids(Raws)),
		?assertEqual(Expected, Res)
	end},
	{"merge raws",
	fun() ->
		Raws = [{atomic, [
			#cdr_raw{id = "a"},
			#cdr_raw{id = "a"},
			#cdr_raw{id = "a"},
			#cdr_raw{id = "b"},
			#cdr_raw{id = "b"},
			#cdr_raw{id = "c"},
			#cdr_raw{id = "e"}]},
			{atomic, [
			#cdr_raw{id = "b"},
			#cdr_raw{id = "b"},
			#cdr_raw{id = "e"},
			#cdr_raw{id = "d"},
			#cdr_raw{id = "d"}]},
			{atomic, [
			#cdr_raw{id = "a"},
			#cdr_raw{id = "a"},
			#cdr_raw{id = "c"},
			#cdr_raw{id = "e"},
			#cdr_raw{id = "e"}]}],
		Sort = fun(A, B) ->
			A#cdr_raw.id < B#cdr_raw.id
		end,
		Expected = [
			#cdr_raw{id = "a"},
			#cdr_raw{id = "a"},
			#cdr_raw{id = "a"},
			#cdr_raw{id = "a"},
			#cdr_raw{id = "a"},
			#cdr_raw{id = "b"},
			#cdr_raw{id = "b"},
			#cdr_raw{id = "b"},
			#cdr_raw{id = "b"},
			#cdr_raw{id = "c"},
			#cdr_raw{id = "c"},
			#cdr_raw{id = "d"},
			#cdr_raw{id = "d"},
			#cdr_raw{id = "e"},
			#cdr_raw{id = "e"},
			#cdr_raw{id = "e"},
			#cdr_raw{id = "e"}],
		Res = lists:sort(Sort, merge_raw(Raws)),
		?assertEqual(Expected, Res)
	end},
	{"merge summaries",
	fun() ->
		Recs = [{atomic, [
			#cdr_rec{media = #call{id = "a", source = self()}, summary = inprogress, transactions = inprogress},
			#cdr_rec{
				media = #call{id = "b", source = self()},
				summary = [{"key", "value"}],
				transactions = [#cdr_raw{id = "b"}]
			},
			#cdr_rec{
				media = #call{id = "c", source = self()},
				summary = [{"key", "value"}],
				transactions = [#cdr_raw{id = "c"}]
			}]},
			{atomic, [
			#cdr_rec{media = #call{id = "a", source = self()}, summary = inprogress, transactions = inprogress},
			#cdr_rec{media = #call{id = "c", source = self()}, summary = inprogress, transactions = inprogress},
			#cdr_rec{media = #call{id = "d", source = self()}, summary = [{"key", "value"}], transactions = [#cdr_raw{id = "d"}]}
			]}],
		Expected = [
			#cdr_rec{media = #call{id = "a", source = self()}, summary = inprogress, transactions = inprogress},
			#cdr_rec{
				media = #call{id = "b", source = self()},
				summary = [{"key", "value"}],
				transactions = [#cdr_raw{id = "b"}]
			},
			#cdr_rec{
				media = #call{id="c", source = self()},
				summary = [{"key", "value"}],
				transactions = [#cdr_raw{id = "c"}]
			},
			#cdr_rec{media = #call{id = "d", source = self()}, summary = [{"key", "value"}], transactions = [#cdr_raw{id = "d"}]}],
		Sort = fun(#cdr_rec{media = Ma} = A, #cdr_rec{media = Mb} = B) ->
			Ma#call.id < Mb#call.id
		end,
		Res = lists:sort(Sort, merge_sum(Recs)),
		?assertEqual(Expected, Res)
	end}].
	
%summarize_test_() ->
%	[{"Simple summary, one call, one agent",
%	fun() ->
%		Transactions = [
%			{inqueue, 5, 10, 5, "testqueue"},
%			{ringing, 10, 13, 3, "agent"},
%			{oncall, 13, 20, 7, "agent"},
%			{wrapup, 20, 24, 4, "agent"}
%		],
%		Dict = summarize(Transactions),
%		?assertEqual({5, 3, 7, 4}, proplists:get_value(total, Dict)),
%		?assertEqual({0, 3, 7, 4}, proplists:get_value("agent", Dict))
%	end},
%	{"summary with a ringout",
%	fun() ->
%		Transactions = [
%			{inqueue, 5, 10, 5, "testqueue"},
%			{ringing, 10, 20, 10, "agent1"},
%			{ringing, 20, 28, 8, "agent2"},
%			{oncall, 28, 22, 5, "agent2"},
%			{wrapup, 22, 23, 1, "agent2"}
%		],
%		Dict = summarize(Transactions),
%		?assertEqual({5, 18, 5, 1}, proplists:get_value(total, Dict)),
%		?assertEqual({0, 10, 0, 0}, proplists:get_value("agent1", Dict)),
%		?assertEqual({0, 8, 5, 1}, proplists:get_value("agent2", Dict))
%	end},
%	{"summary with a transfer to another agent",
%	fun() ->
%		Transactions = [
%			{inqueue, 5, 10, 5, "testqueue"},
%			{ringing, 10, 20, 10, "agent1"},
%			{oncall, 20, 30, 10, "agent1"},
%			{transfer, 30, 30, 0, "agent2"},
%			{oncall, 30, 35, 5, "agent2"},
%			{wrapup, 30, 40, 10, "agent1"},
%			{wrapup, 35, 40, 5, "agent2"}
%		],
%		Dict = summarize(Transactions),
%		?assertEqual({5, 10, 15, 15}, proplists:get_value(total, Dict)),
%		?assertEqual({0, 10, 10, 10}, proplists:get_value("agent1", Dict)),
%		?assertEqual({0, 0, 5, 5}, proplists:get_value("agent2", Dict))
%	end}].
%
%mnesia_test_() ->
%	{foreach,
%	fun() ->
%		mnesia:stop(),
%		mnesia:delete_schema([node()]),
%		mnesia:create_schema([node()]),
%		mnesia:start(),
%		build_tables(),
%		#call{id = "testcall", source = self()}
%	end,
%	fun(_Whatever) ->
%		ok
%	end,
%	[fun(Call) ->
%		{"Summary gets written to db",
%		fun() ->
%			StateTrans = lists:reverse([
%				{inqueue, 5, 10, 5, "testqueue"},
%				{ringing, 10, 13, 3, "agent"},
%				{oncall, 13, 20, 7, "agent"},
%				{hangup, 20, 20, 0, agent}
%			]),
%			ExpectedTransactions = [ {wrapup, 20, 24, 4, "agent"} | StateTrans ],
%			Unterminated = [{wrapup, 20, "agent"}],
%			State = #state{unterminated = Unterminated, id="testcall", transactions = StateTrans, hangup=true},
%			remove_handler = handle_event({endwrapup, Call, 24, "agent"}, State),
%			timer:sleep(10), % give the spawned summarizer time to work
%			F = fun() ->
%				mnesia:read(cdr_rec, "testcall")
%			end,
%			{atomic, [Cdrrec]} = mnesia:transaction(F),
%			?assertEqual({5, 3, 7, 4}, proplists:get_value(total, Cdrrec#cdr_rec.summary)),
%			?assertEqual({0, 3, 7, 4}, proplists:get_value("agent", Cdrrec#cdr_rec.summary)),
%			?assertEqual(ExpectedTransactions, Cdrrec#cdr_rec.transactions)
%		end}
%	end,
%	fun(Call) ->
%		{"init creates an 'inprogress' summary",
%		fun() ->
%			init([Call]),
%			F = fun() ->
%				mnesia:read(cdr_rec, "testcall")
%			end,
%			{atomic, [Cdrrec]} = mnesia:transaction(F),
%			?assertEqual(inprogress, Cdrrec#cdr_rec.summary),
%			?assertEqual(inprogress, Cdrrec#cdr_rec.transactions)
%		end}
%	end]}.
%
%recover_test_() ->
%	[
%	{"Recover a call that's only been queued",
%	fun() ->
%		Transactions = [{inqueue, 5, "testqueue"}],
%		?assertEqual({[{inqueue, 5, "testqueue"}], [], false}, recover(Transactions, [], [], false))
%	end},
%	{"Recover a call that's ringing to an agent",
%	fun() ->
%		Transactions = [{inqueue, 5, "testqueue"}, {ringing, 10, "agent"}],
%		Expected = {[{ringing, 10, "agent"}], [{inqueue, 5, 10, 5, "testqueue"}], false},
%		?assertEqual(Expected, recover(Transactions, [], [], false))
%	end},
%	{"Recover a call that's ringing to two different agents",
%	fun() ->
%		Transactions = [{inqueue, 5, "testqueue"}, {ringing, 10, "agent1"}, {ringing, 15, "agent2"}],
%		Expected = {[{ringing, 15, "agent2"}], [{ringing, 10, 15, 5, "agent1"}, {inqueue, 5, 10, 5, "testqueue"}], false},
%		?assertEqual(Expected, recover(Transactions, [], [], false))
%	end},
%	{"Recover a call that's all but completed",
%	fun() ->
%		Transactions = [
%			{inqueue, 5, "testqueue"},
%			{ringing, 10, "agent1"},
%			{ringing, 15, "agent2"},
%			{oncall, 20, "agent2"},
%			{transfer, 25, "agent3"},
%			{oncall, 25, "agent3"},
%			{wrapup, 25, "agent2"},
%			{endwrapup, 30, "agent2"},
%			{hangup, 35, agent},
%			{wrapup, 35, "agent3"}
%		],
%		Expecteduntermed = [{wrapup, 35, "agent3"}],
%		Expectedtermed = [
%			{oncall, 25, 35, 10, "agent3"},
%			{hangup, 35, 35, 0, agent},
%			{wrapup, 25, 30, 5, "agent2"},
%			{oncall, 20, 25, 5, "agent2"},
%			{transfer, 25, 25, 0, "agent3"},
%			{ringing, 15, 20, 5, "agent2"},
%			{ringing, 10, 15, 5, "agent1"},
%			{inqueue, 5, 10, 5, "testqueue"}
%		],
%		Expected = {Expecteduntermed, Expectedtermed, true},
%		?DEBUG("recovery:  ~p", [recover(Transactions, [], [], false)]),
%		?assertEqual(Expected, recover(Transactions, [], [], false))
%	end}
%	].
%


-endif.

