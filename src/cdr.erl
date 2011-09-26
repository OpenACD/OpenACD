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
%%	The Original Code is OpenACD.
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
%%	Andrew Thompson <andrew at hijacked dot us>
%%	Micah Warren <micahw at lordnull dot com>
%%

%% @doc A gen_event based module to create and record Call Data Records to mnesia.

-module(cdr).
-behaviour(gen_event).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([
	start/0,
	start_link/0,
	cdrinit/1,
	inivr/2,
	precall/2,
	inqueue/2,
	dialoutgoing/2,
	ringing/2,
	ringout/2,
	oncall/2,
	hangup/2,
	wrapup/2,
	endwrapup/2,
	transfer/2,
	voicemail/2,
	agent_transfer/2,
	queue_transfer/2,
	warmxfer_begin/2,
	warmxfer_cancel/2,
	warmxfer_complete/2,
	warmxfer_fail/2,
	truncate/0,
	truncate/1,
	status/1,
	merge/3,
	get_raws/1,
	get_summaries/1,
	get_unsummarized/0,
	spawn_summarizer/1
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
	id :: callid(),
	callrec :: #call{},
	nodes = [] :: [atom()],
	limbo_wrapup_count = 0 :: non_neg_integer(),
	hangup = false :: 'false' | 'true',
	mon_ref :: reference()
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
	{ok, Nodes} = cpx:get_env(nodes, [node()]),
	start(Nodes).

%% @doc starts the cdr event server with mnesia on specified nodes.
start(Nodes) ->
	case build_tables(Nodes) of
		ok -> ok;
		Else ->
			?WARNING("Some tables didn't build, this may crash and burn later.  ~p", [Else])
	end,
	gen_event:start({local, ?MODULE}).

%% @doc Starts the cdr event server linked.
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() ->
	{ok, Nodes} = cpx:get_env(nodes, [node()]),
	start_link(Nodes).

%% @doc Starts the cdr event server with mnesia on specified nodes.
-spec(start_link/1 :: (Nodes :: [atom()]) -> {'ok', pid()}).
start_link(Nodes) ->
	case build_tables(Nodes) of
		ok -> ok;
		Else ->
			?WARNING("Some tables didn't build, this may crash and burn later.  ~p", [Else])
	end,
	gen_event:start_link({local, ?MODULE}).

%% @doc Create a handler specifically for `#call{} Call' with default options.
-spec(cdrinit/1 :: (Call :: #call{}) -> 'ok' | 'error').
cdrinit(Call) ->
	try lists:member({?MODULE, Call#call.id}, gen_event:which_handlers(cdr)) of
		false ->
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
			end;
		true ->
			?WARNING("CDR already initialized for ~s", [Call#call.id])
	catch
		What:Why ->
			?ERROR("Initializing CDR for ~s erred with: ~p:~p", [Call#call.id, What, Why]),
			error
	end.
	
%% @doc Notify cdr handler that `#call{} Call' is now in IVR for `string() DNIS'.
-spec(inivr/2 :: (Call :: #call{}, DNIS :: string()) -> 'ok').
inivr(Call, DNIS) ->
	event({inivr, Call, util:now(), DNIS}).

%% @doc Notify cdr handler that `#call{} Call' is now in precall for `string() Client'.
-spec(precall/2 :: (Call :: #call{}, Client :: string()) -> 'ok').
precall(Call, Client) ->
	event({precall, Call, util:now(), Client}).

%% @doc Notify cdr handler that `#call{} Call' is now in queue `string() Queue'.
-spec(inqueue/2 :: (Call :: #call{}, Queue :: string()) -> 'ok').
inqueue(Call, Queue) ->
	event({inqueue, Call, util:now(), Queue}).

%% @doc Notify cdr handler that `#call{} Call' is now is dialing outbound to `string() Number'.
-spec(dialoutgoing/2 :: (Call :: #call{}, Number :: string()) -> 'ok').
dialoutgoing(Call, Number) ->
	event({dialoutgoing, Call, util:now(), Number}).

%% @doc Notify cdr handler that `#call{} Call' is now ringing to `string() Agent'.
-spec(ringing/2 :: (Call :: #call{}, Agent :: string() | pid()) -> 'ok').
ringing(Call, Agent) when is_pid(Agent) ->
	ringing(Call, agent_manager:find_by_pid(Agent));
ringing(Call, Agent) ->
	event({ringing, Call, util:now(), Agent}).

%% @doc Notify cdr handler that `#call{} Call' has rungout from `string() Agent'.
-spec(ringout/2 :: (Call :: #call{}, Args :: {atom(), string() | pid()}) -> 'ok').
ringout(Call, {Reason, Agent}) when is_pid(Agent) ->
	ringout(Call, {Reason, agent_manager:find_by_pid(Agent)});
ringout(Call, {Reason, Agent}) ->
	event({ringout, Call, util:now(), {Reason, Agent}}).

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

%% @doc Notify cdr handler that `#cll{} Call' is starting to be warm transfered.
-spec(warmxfer_begin/2 :: (Call :: #call{}, {Agent :: string() | pid(), Numdialed :: string()}) -> 'ok').
warmxfer_begin(Call, {Agent, Numdialed}) when is_pid(Agent) ->
	warmxfer_begin(Call, {agent_manager:find_by_pid(Agent), Numdialed});
warmxfer_begin(Call, {Agent, Numdialed}) ->
	event({warmxfer_begin, Call, util:now(), {Agent, Numdialed}}).

-spec(warmxfer_cancel/2 :: (Call :: #call{}, Agent :: pid() | string()) -> 'ok').
warmxfer_cancel(Call, Agent) when is_pid(Agent) ->
	warmxfer_cancel(Call, agent_manager:find_by_pid(Agent));
warmxfer_cancel(Call, Agent) ->
	event({warmxfer_cancel, Call, util:now(), Agent}).

-spec(warmxfer_fail/2 :: (Call :: #call{}, Agent :: pid() | string()) -> 'ok').
warmxfer_fail(Call, Agent) when is_pid(Agent) ->
	warmxfer_fail(Call, agent_manager:find_by_pid(Agent));
warmxfer_fail(Call, Agent) ->
	event({warmxfer_fail, Call, util:now(), Agent}).

-spec(warmxfer_complete/2 :: (Call :: #call{}, Agent :: pid() | string()) -> 'ok').
warmxfer_complete(Call, Agent) when is_pid(Agent) ->
	warmxfer_complete(Call, agent_manager:find_by_pid(Agent));
warmxfer_complete(Call, Agent) ->
	event({warmxfer_complete, Call, util:now(), Agent}).

%% @doc Notify cdr handler that `#call{} Call' is being offered by `string() Offerer'
%% to `string() Recipient'.
-spec(agent_transfer/2 :: (Call :: #call{}, {Offerer :: string() | pid(), Recipient :: string() | pid()}) -> 'ok').
agent_transfer(Call, {Offerer, Recipient}) when is_pid(Offerer) ->
	agent_transfer(Call, {agent_manager:find_by_pid(Offerer), Recipient});
agent_transfer(Call, {Offerer, Recipient}) when is_pid(Recipient) ->
	agent_transfer(Call, {Offerer, agent_manager:find_by_pid(Recipient)});
agent_transfer(Call, {Offerer, Recipient}) ->
	event({agent_transfer, Call, util:now(), {Offerer, Recipient}}).

%% @doc Notify cdr handler that `#call{} Call' is being offered by `string() Offerer'
%% to `string() Recipient'.
-spec(queue_transfer/2 :: (Call :: #call{}, Queue :: string()) -> 'ok').
queue_transfer(Call, Queue) ->
	event({queue_transfer, Call, util:now(), Queue}).

%% @doc Notify the cdr handler the `#call{} Call' is being sent to voicemail
%% from `string() Queue'.
-spec(voicemail/2 :: (Call :: #call{}, Queue :: pid() | string()) -> 'ok').
voicemail(Call, Qpid) when is_pid(Qpid) ->
	List = queue_manager:queues(),
	Queue = case lists:keysearch(Qpid, 2, List) of
		{value, {Qnom, Qpid}} ->
			 Qnom; 
		 false -> 
			 undefined 
	end,
	voicemail(Call, Queue);
voicemail(Call, Queue) ->
	event({voicemail, Call, util:now(), Queue}).

-spec(truncate/0 :: () -> ['none' | 'ok' | pid()]).
truncate() ->
	Now = util:now(),
	{atomic, Deads} = mnesia:transaction(fun() -> 
		qlc:e(qlc:q([M || 
			#cdr_rec{media = M, summary = inprogress, timestamp = Time} <- mnesia:table(cdr_rec),
			begin
				N = node(),
				case node(M#call.source) of
					N ->
						is_process_alive(M#call.source) == false;
					_ ->
						true
				end
			end,
			Now - Time > 3600
		])) 
	end),
	[truncate(X) || X <- Deads].
	%Handles = [gen_event:delete_handler(cdr, {cdr, Id}, truncate) || {cdr, Id} <- gen_event:which_handlers(cdr), lists:member(Deads, Id)],

-spec(truncate/1 :: (Call :: string() | #call{}) -> 'ok' | 'none' | pid).
truncate(Callid) when is_list(Callid) ->
	Res = mnesia:transaction(fun() ->
		qlc:e(qlc:q([M || #cdr_rec{media = M} <- mnesia:table(cdr_rec), M#call.id =:= Callid]))
	end),
	case Res of
		{atomic, []} ->
			?INFO("no cdr_rec found for ~p; thus, no truncating", [Callid]),
			none;
		{atomic, [Callrec]} ->
			truncate(Callrec)
	end;
truncate(Callrec) when is_record(Callrec, call) ->
	?NOTICE("Beginning truncate for ~p", [Callrec#call.id]),
	{atomic, Raws} = mnesia:transaction(fun() ->
		qlc:e(qlc:q([X || #cdr_raw{id = Id} = X <- mnesia:table(cdr_raw), Id =:= Callrec#call.id]))
	end),
	case Raws of 
		[] ->
			?WARNING("~p has nothing in cdr_raws (already summarized?)", [Callrec#call.id]);
		_ ->
			[R | _] = Raws,
			case attached_agent(Raws) of
				true ->
					ok;
				false ->
					gen_event:delete_handler(cdr, {cdr, Callrec#call.id}, truncate),
					Time = cdr_big_time(Raws, 0),
					Cdr = #cdr_raw{
						id = Callrec#call.id,
						transaction = cdrend,
						start = Time,
						ended = Time,
						nodes = R#cdr_raw.nodes
					},
					{atomic, _} = push_raw(Callrec, Cdr),
					spawn_summarizer(Callrec)
			end
	end.

attached_agent([]) ->
	false;
attached_agent([Head | Tail]) ->
	case attached_agent(Head) of
		true ->
			true;
		_ ->
			attached_agent(Tail)
	end;
attached_agent(#cdr_raw{eventdata = D, id = _Id}) when is_list(D) ->
	case agent_manager:query_agent(D) of
		{true, _Pid} ->
			true;
		false ->
			false
	end;
attached_agent(_) ->
	false.

cdr_big_time([], Acc) ->
	Acc;
cdr_big_time([#cdr_raw{start = N, ended = undefined} | Tail], Acc) when is_integer(N), Acc < N ->
	cdr_big_time(Tail, N);
cdr_big_time([#cdr_raw{ended = N} | Tail], Acc) when is_integer(N), Acc < N ->
	cdr_big_time(Tail, N);
cdr_big_time([_H | Tail], Acc) ->
	cdr_big_time(Tail, Acc).

event(Tuple) ->
	catch gen_event:notify(cdr, Tuple).
%% @doc Return the completed and partial transactions for `#call{} Call'.

-spec(status/1 :: (Call :: #call{} | string()) -> {[tuple()], [tuple()]}).
status(#call{id = Cid}) ->
	status(Cid);
status(Cid) ->
	gen_event:call(cdr, {?MODULE, Cid}, status).

%% @doc return the call ids that are unsummarized
-spec(get_unsummarized/0 :: () -> {atomic, [string()]}).
get_unsummarized() ->
	F = fun() ->
		QH = qlc:q([Callrec#call.id || #cdr_rec{media = Callrec, summary = IsInprogress} <- mnesia:table(cdr_rec), IsInprogress == inprogress]),
		qlc:e(QH)
	end,
	mnesia:transaction(F).

%% =====
%% Gen event callbacks
%%=====

%% @private
init([Call]) ->
	?NOTICE("Starting new CDR handler for ~s", [Call#call.id]),
	Nodes = case application:get_env('OpenACD', nodes) of
		undefined ->
			[node()];
		{ok, List} ->
			List
	end,
	Cdrrec = #cdr_rec{media = Call, nodes = Nodes},
	Initraw = #cdr_raw{
		id = Call#call.id,
		transaction = cdrinit,
		eventdata = Call,
		terminates = infoevent,
		nodes = Nodes
	},
	cpx_monitor:info({cdr_raw, Initraw}),
	mnesia:transaction(fun() -> 
		mnesia:write(Cdrrec), 
		mnesia:write(Initraw)
	end),
	Monref = erlang:monitor(process, Call#call.source),
	{ok, #state{id=Call#call.id, callrec = Call, nodes = Nodes, mon_ref = Monref}}.

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
	Ended = case lists:member(Transaction, [hangup, agent_transfer, voicemail, endwrapup, queue_transfer, ringout]) of
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
	{atomic, Termed} = push_raw(Call, Cdr),
	%?DEBUG("Termed:  ~p", [Termed]),
	Extra = analyze(Transaction, Call, Time, Data, Termed),
	[cpx_monitor:info({cdr_raw, ExtraRaw}) || ExtraRaw <- Extra],
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
		warmxfer_cancel ->
			State#state{limbo_wrapup_count = Limbocount - 1};
		hangup ->
			State#state{hangup = true};
		_Else ->
			State
	end,
	case {Newstate#state.hangup, Newstate#state.limbo_wrapup_count} of
		{true, 0} ->
			remove_handler;
		_Anything_else ->
			{ok, Newstate}
	end;
handle_event({_Transaction, _Call, _Time, _Data}, State) ->
	% this is an event for a different CDR handler, ignore it
	{ok, State};
handle_event({truncate, Callid}, #state{id = Callid}) ->
	remove_handler;
handle_event({truncate, _Callid}, State) ->
	{ok, State}.

%% @private
handle_call(status, State) ->
	%catch toolbar:start(),
	{ok, ok, State};
handle_call(_Request, State) ->
	{ok, ok, State}.

%% @private
handle_info({'DOWN', Monref, process, Pid, Reason}, #state{mon_ref = Monref} = State) ->
	% handle this like a hangup, only the pid did the hanging up.
	case State#state.hangup of
		false ->
			handle_event({hangup, State#state.callrec, util:now(), "pid"}, State);
		true ->
			case State#state.limbo_wrapup_count of
				0 ->
					remove_handler;
				_ ->
					{ok, State}
			end
	end;
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

spawn_summarizer(#call{id = Id} = Call) ->
	{atomic, Transactions} = mnesia:transaction(fun() ->
		QH = qlc:q([X || X <- mnesia:table(cdr_raw), X#cdr_raw.id =:= Id]),
		qlc:e(QH)
	end),
	spawn_summarizer(Transactions, Call).

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
-spec(push_raw/2 :: (Callrec :: #call{}, Trans :: #cdr_raw{}) -> {'atomic', [{transaction_type(), any()}]}).
push_raw(#call{id = Cid} = Callrec, #cdr_raw{id = Cid, start = Now} = Trans) ->
	F = fun() ->
		Untermed = find_untermed(Trans#cdr_raw.transaction, Callrec, Trans#cdr_raw.eventdata),
		Termedatoms = lists:map(fun(#cdr_raw{transaction = T, eventdata = E}) -> {T, E} end, Untermed),
		%?DEBUG("closing cdr records ~p", [Untermed]),
		Terminate = fun(Rec) ->
			mnesia:delete_object(Rec),
			NewRec = Rec#cdr_raw{ended = Now},
			cpx_monitor:info({cdr_raw, NewRec}),
			mnesia:write(NewRec)
		end,
		lists:foreach(Terminate, Untermed),
		%?DEBUG("Writing ~p", [Trans]),
		FullTrans = Trans#cdr_raw{terminates = lists:map(fun({T, _}) -> T end, Termedatoms)},
		cpx_monitor:info({cdr_raw, FullTrans}),
		mnesia:write(FullTrans),
		Termedatoms
	end,
	mnesia:transaction(F).

%% @doc Determine any info messages that should be input based on what the last
%% actual message ended.
-spec(analyze/5 :: (Trans :: transaction_type(), Call :: #call{}, Time :: cpx_time(), Data :: any(), Termed :: [{transaction_type(), any()}]) -> [#cdr_raw{}]).
analyze(hangup, #call{id = Cid}, Time, _, [{inivr, _}]) ->
	[#cdr_raw{id = Cid, start = Time, ended = Time, transaction = abandonivr}];
analyze(hangup, #call{id = Cid}, Time, _, [{inqueue, Queuename}]) ->
	[#cdr_raw{id = Cid, start = Time, ended = Time, transaction = abandonqueue, eventdata = Queuename}];
analyze(_, _, _, _, _) ->
	[].
	
-spec(find_untermed/3 :: (Event :: transaction_type(), Call :: #call{}, Data :: any()) -> [#cdr_raw{}]).
find_untermed(inivr, _, _) ->
	% Does not terminate anything
	[];
find_untermed(dialoutgoing, _, _) ->
	% info event, precall terminated by outgoing/oncall
	[];
find_untermed(inqueue, #call{id = Cid}, Queuename) ->
	% queue to queue transfers, IVR time
	QH = qlc:q([X || 
		X <- mnesia:table(cdr_raw), 
		X#cdr_raw.id =:= Cid, 
		( X#cdr_raw.transaction =:= inqueue andalso X#cdr_raw.eventdata =/= Queuename) orelse
		( X#cdr_raw.transaction =:= inivr),
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(ringing, _, _) ->
	% doesn't term anything, but not info event
	[];
find_untermed(precall, _, _) ->
	% same as above,
	[];
find_untermed(oncall, #call{id = Cid}, Agent) ->
	% terminates precall so I don't have to change it should outgoing state go away.
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id =:= Cid,
		( ( (X#cdr_raw.transaction == ringing) andalso (X#cdr_raw.eventdata == Agent) )
			orelse X#cdr_raw.transaction == inqueue
			orelse X#cdr_raw.transaction == precall
			orelse X#cdr_raw.transaction == dialoutgoing
			orelse X#cdr_raw.transaction == warmxfer_cancel),
		X#cdr_raw.ended == undefined
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
find_untermed(warmxfer_begin, #call{id = Cid}, _) ->
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id == Cid,
		lists:member(X#cdr_raw.transaction, [oncall, warmxfer_fail]),
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(warmxfer_cancel, #call{id = Cid}, _) ->
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id == Cid,
		lists:member(X#cdr_raw.transaction, [warmxfer_begin, warmxfer_fail]),
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(warmxfer_fail, #call{id = Cid}, _) ->
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id == Cid,
		X#cdr_raw.transaction == warmxfer_begin,
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(warmxfer_complete, #call{id = Cid}, _) ->
	QH = qlc:q([X || 
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id == Cid,
		X#cdr_raw.transaction == warmxfer_begin,
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(wrapup, #call{id = Cid}, Agent) ->
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id =:= Cid,
		X#cdr_raw.transaction =:= oncall,
		X#cdr_raw.eventdata =:= Agent,
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(endwrapup, #call{id = Cid}, Agent) ->
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id =:= Cid,
		X#cdr_raw.transaction =:= wrapup,
		X#cdr_raw.eventdata =:= Agent,
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(ringout, #call{id = Cid}, {_Reason, _Agent}) ->
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id =:= Cid,
		X#cdr_raw.transaction =:= ringing,
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(voicemail, #call{id = Cid}, _Whatever) ->
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id =:= Cid,
		X#cdr_raw.transaction =:= inqueue,
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(hangup, #call{id = Cid}, _Whatever) ->
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id =:= Cid,
		lists:member(X#cdr_raw.transaction, [inqueue, inivr, precall, warmxfer_complete]),
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(cdrend, #call{id = Cid}, _Whatever) ->
	QH = qlc:q([X ||
		X <- mnesia:table(cdr_raw),
		X#cdr_raw.id =:= Cid,
		X#cdr_raw.ended =:= undefined
	]),
	qlc:e(QH);
find_untermed(_, _, _) ->
	%% some other event, prolly an info.  unknowns terminate nothing.
	[].
	
build_tables(Nodes) ->
	RecT = util:build_table(cdr_rec, [
		{attributes, record_info(fields, cdr_rec)},
		{disc_copies, Nodes}
	]),
	RawT = util:build_table(cdr_raw, [
		{attributes, record_info(fields, cdr_raw)},
		{disc_copies, Nodes},
		{type, bag}
	]),
	Successes = [exists, copied, {atomic, ok}],
	case {lists:member(RecT, Successes), lists:member(RawT, Successes)} of
		{true, true} ->
			ok;
		_ ->
			{RecT, RawT}
	end.

summarize_sorter(#cdr_raw{transaction = cdrinit}, _) ->
	true;
summarize_sorter(_, #cdr_raw{transaction = cdrinit}) ->
	false;
summarize_sorter(#cdr_raw{start = Start, ended = Aend}, #cdr_raw{start = Start, ended = Bend}) ->
	Aend =< Bend;
summarize_sorter(#cdr_raw{start = AStart}, #cdr_raw{start = BStart}) ->
	AStart =< BStart.

spawn_summarizer(UsortedTransactions, #call{id = CallID} = Callrec) ->
	Summarize = fun() ->
		Sort = fun summarize_sorter/2,
		Transactions = lists:sort(Sort, UsortedTransactions),
		?DEBUG("Summarize inprogress for ~p", [CallID]),
		Summary = summarize(Transactions),
		{ok, Nodes} = cpx:get_env(nodes, [node()]),
		CdrRec = #cdr_rec{
			media = Callrec,
			summary = Summary,
			transactions = Transactions,
			nodes = Nodes
		},
		F = fun() ->
			mnesia:delete({cdr_rec, Callrec}),
			mnesia:write(CdrRec),
			mnesia:delete({cdr_raw, CallID}),
			cpx_monitor:info({cdr_rec, CdrRec}),
			gen_cdr_dumper:update_notify(cdr_rec)
		end,
		mnesia:transaction(F)
	end,
	spawn(Summarize).
	
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
	%?DEBUG("Summarizing ~p", [Transactions]),
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
summarize([#cdr_raw{transaction = warmxfer_begin} = Cdr | Tail], Acc) ->
	Newacc = summarize(Cdr, oncall, element(1, Cdr#cdr_raw.eventdata), Acc),
	summarize(Tail, Newacc);
summarize([#cdr_raw{transaction = warmxfer_fail} = Cdr | Tail], Acc) ->
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
		{ok, Else} ->
			Else
	end,
	%?DEBUG("total:  ~p;  propdict:  ~p", [Total, Propdict]),
	Duration = Cdr#cdr_raw.ended - Cdr#cdr_raw.start,
	Detail = proplists:get_value(Individual, Propdict, 0),
	Cleanedprops = proplists:delete(Individual, Propdict),
	Newdetail = Detail + Duration,
	Newtotal = Total + Duration,
	Newprops = [{Individual, Newdetail} | Cleanedprops],
	%?DEBUG("newtotal:  ~p;  newpropdict:  ~p", [Newtotal, Newprops]),
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
get_raws([Node | Tail], Time, Acc) when Node == node() ->
	get_raws(Tail, [get_raws(Time) | Acc]);
get_raws([Node | Tail], Time, Acc) ->
	Out = rpc:call(Node, ?MODULE, get_raws, [Time]),
	get_raws(Tail, Time, [Out | Acc]).

-spec(get_raws/1 :: (Time :: pos_integer()) -> {'atomic', [#cdr_raw{}]}).
get_raws(Time) ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(cdr_raw), X#cdr_raw.timestamp =< Time]),
		qlc:e(QH)
	end,
	mnesia:transaction(F).

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
get_summaries([Node | Nodes], Ids, Acc) when Node == node() ->
	case get_summaries(Ids) of
		{atomic, _Rows} = Rez ->
			get_summaries(Nodes, Ids, [Rez | Acc]);
		_Else ->
			?WARNING("Could not get cdr_rec from ~w", [Node]),
			get_summaries(Nodes, Ids, Acc)
	end;
get_summaries([Node | Nodes], Ids, Acc) ->
	case rpc:call(Node, ?MODULE, get_summaries, [Ids]) of
		{atomic, _Rows} = Rez ->
			get_summaries(Nodes, Ids, [Rez | Acc]);
		_Else ->
			?WARNING("Could not get cdr_rec from ~w", [Node]),
			get_summaries(Nodes, Ids, Acc)
	end.

-spec(get_summaries/1 :: (IDs :: [string()]) -> {'atomic', [#cdr_rec{}]} | {'aborted', any()}).
get_summaries(IDs) ->
	F = fun() ->
		QH = qlc:q([X || #cdr_rec{media = Media} = X <- mnesia:table(cdr_rec), lists:member(Media#call.id, IDs)]),
		qlc:e(QH)
	end,
	mnesia:transaction(F).

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
	Sort = fun(#cdr_rec{media = MediaA}, #cdr_rec{media = MediaB}) ->
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

-ifdef(TEST).

analyze_test_() ->
	[{"hangup in inv",
	fun() ->
		Now = util:now(),
		Res = analyze(hangup, #call{id = "testcall", source = self()}, Now, "ivr", [{inivr, "garbage"}]),
		?assertMatch([#cdr_raw{id = "testcall", start = Now, ended = Now, transaction = abandonivr}], Res)
	end},
	{"hangup in q",
	fun() ->
		Now = util:now(),
		Res = analyze(hangup, #call{id = "testcall", source = self()}, Now, "queue", [{inqueue, "queue"}]),
		?assertMatch([#cdr_raw{id = "testcall", start = Now, ended = Now, transaction = abandonqueue, eventdata = "queue"}], Res)
	end}].

push_raw_test_() ->
	util:start_testnode(),
	N = util:start_testnode(cdr_push_raw_tests),
	{spawn, N, {foreach,
	fun() ->
		?DEBUG("node:  ~p", [node()]),
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		mnesia:create_schema([node()]),
		mnesia:start(),
		ok = build_tables([node()]),
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
			{failedoutgoing, "na"},
			{agent_transfer, "na"},
			{queue_transfer, "na"},
			{warmxfer_cancel, "na"},
			{warmxfer_complete, "na"},
			{warmxfer_failed, "na"},
			{warmxfer_begin, "na"},
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
			Testend = [inqueue, inivr],
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
		{"precall",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = precall, eventdata = "testagent"}),
			Testend = [],
			Ended(Pull(), Testend)
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"oncall",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = oncall, eventdata = "testagent"}),
			Testend = [inqueue, ringing, precall, warmxfer_cancel, dialoutgoing],
			Ended(Pull(), Testend)
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
		{"warmxfer_begin",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = warmxfer_begin}),
			Ended(Pull(), [oncall, warmxfer_fail])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"warmxfer_fail",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = warmxfer_fail}),
			Ended(Pull(), [warmxfer_begin])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"warmxfer_complete",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = warmxfer_complete}),
			Ended(Pull(), [warmxfer_begin])
		end}
	end,
	fun({Call, Pull, Ended}) ->
		{"warmxfer_cancel",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = warmxfer_cancel}),
			Ended(Pull(), [warmxfer_begin, warmxfer_fail])
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
		{"abandonqueue",
		fun() ->
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = abandonqueue}),
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
			push_raw(Call, #cdr_raw{id = Call#call.id, transaction = ringout, eventdata = {ringout, {badagent, "testagent"}}}),
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
			Ended(Pull(), [inqueue, inivr, precall, warmxfer_complete])
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
				failedoutgoing,
				agent_transfer,
				queue_transfer,
				warmxfer_begin,
				warmxfer_complete,
				warmxfer_failed,
				warmxfer_cancel,
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
	end]}}.


% handle_event's primary duty is to see if the call is ready for summary.
handle_event_test_() ->
	util:start_testnode(),
	N = util:start_testnode(cdr_handle_event_tests),
	{spawn, N, {foreach,
	fun() ->
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		mnesia:create_schema([node()]),
		mnesia:start(),
		build_tables([node()]),
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
	end,
	fun({Call, Basestate}) ->
		{"pid sudden death:  no limbo and no hangup yet",
		fun() ->
			Mpid = Call#call.source,
			Monref = Basestate#state.mon_ref,
			Out = handle_info({'DOWN', Monref, process, Mpid, <<"uberdeath">>}, Basestate),
			?assertEqual(remove_handler, Out)
		end}
	end,
	fun({Call, Basestate}) ->
		{"pid sudden death: hangup already happened, no limbos",
		fun() ->
			Mpid = Call#call.source,
			Monref = Basestate#state.mon_ref,
			InState = Basestate#state{hangup = true},
			Out = handle_info({'DOWN', Monref, process, Mpid, <<"uberdeath">>}, InState),
			?assertEqual(remove_handler, Out)
		end}
	end,
	fun({Call, Basestate}) ->
		{"pid sudden death:  limbos and hangups",
		fun() ->
			Mpid = Call#call.source,
			Monref = Basestate#state.mon_ref,
			InState = Basestate#state{hangup = true, limbo_wrapup_count = 1},
			{ok, Out} = handle_info({'DOWN', Monref, process, Mpid, <<"uberdeath">>}, InState),
			?assertEqual(InState, Out)
		end}
	end,
	fun({Call, Basestate}) ->
		{"pid sudden death:  no hangup, but limbos",
		fun() ->
			Mpid = Call#call.source,
			Monref = Basestate#state.mon_ref,
			InState = Basestate#state{limbo_wrapup_count = 1},
			{ok, Out} = handle_info({'DOWN', Monref, process, Mpid, <<"uberdeath">>}, InState),
			?assert(Out#state.hangup)
		end}
	end]}}.

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

summarize_test_() ->
	[{"Simple summary, one call, one agent",
	fun() ->
		Transactions = [
			#cdr_raw{id = "id", transaction = cdrinit, start = 5, ended = 35},
			#cdr_raw{id = "id", transaction = inqueue, start = 10, ended = 20, eventdata = "queue"},
			#cdr_raw{id = "id", transaction = ringing, start = 15, ended = 20, eventdata = "agent"},
			#cdr_raw{id = "id", transaction = oncall, start = 20, ended = 25, eventdata = "agent"},
			#cdr_raw{id = "id", transaction = hangup, start = 25, ended = 25, eventdata = ""},
			#cdr_raw{id = "id", transaction = wrapup, start = 25, ended = 30, eventdata = "agent"},
			#cdr_raw{id = "id", transaction = endwrapup, start = 30, ended = 30, eventdata = "agent"},
			#cdr_raw{id = "id", transaction = cdrend, start = 35, ended = 35}
		],
		Topprop = summarize(Transactions),
		Assertfun = fun
			(_F, []) ->
				ok;
			(F, [{inqueue, {Total, Props}} | Tail]) ->
				?assertEqual(10, proplists:get_value("queue", Props)),
				?assertEqual(10, Total),
				F(F, Tail);
			(F, [{ringing, {Total, Props}} | Tail]) ->
				?assertEqual(5, proplists:get_value("agent", Props)),
				?assertEqual(5, Total),
				F(F, Tail);
			(F, [{oncall, {Total, Props}} | Tail]) ->
				?assertEqual(5, proplists:get_value("agent", Props)),
				?assertEqual(5, Total),
				F(F, Tail);
			(F, [{wrapup, {Total, Props}} | Tail]) ->
				?assertEqual(5, proplists:get_value("agent", Props)),
				?assertEqual(5, Total),
				F(F, Tail)
		end,
		Assertfun(Assertfun, Topprop)
	end},
	{"summary with a ringout",
	fun() ->
		Transactions = [
			#cdr_raw{id = "id", transaction = cdrinit, start = 5, ended = 40},
			#cdr_raw{id = "id", transaction = inqueue, start = 10, ended = 25, eventdata = "queue"},
			#cdr_raw{id = "id", transaction = ringing, start = 15, ended = 20, eventdata = "agent1"},
			#cdr_raw{id = "id", transaction = ringout, start = 15, ended = 20, eventdata = {badagent, "agent1"}},
			#cdr_raw{id = "id", transaction = ringing, start = 20, ended = 25, eventdata = "agent2"},
			#cdr_raw{id = "id", transaction = oncall, start = 25, ended = 30, eventdata = "agent2"},
			#cdr_raw{id = "id", transaction = hangup, start = 30, ended = 30, eventdata = ""},
			#cdr_raw{id = "id", transaction = wrapup, start = 30, ended = 40, eventdata = "agent2"},
			#cdr_raw{id = "id", transaction = endwrapup, start = 40, ended = 40, eventdata = "agent2"},
			#cdr_raw{id = "id", transaction = cdrend, start = 40, ended = 40}
		],
		Dict = summarize(Transactions),
		Test = fun
			(_F, []) ->
				ok;
			(F, [{inqueue, {Total, Props}} | Tail]) ->
				?assertEqual(15, Total),
				?assertEqual(15, proplists:get_value("queue", Props)),
				F(F, Tail);
			(F, [{ringing, {Total, Props}} | Tail]) ->
				?assertEqual(10, Total),
				?assertEqual(5, proplists:get_value("agent1", Props)),
				?assertEqual(5, proplists:get_value("agent2", Props)),
				F(F, Tail);
			(F, [{oncall, {Total, Props}} | Tail]) ->
				?assertEqual(5, Total),
				?assertEqual(5, proplists:get_value("agent2", Props)),
				?assertEqual(undefined, proplists:get_value("agent1", Props)),
				F(F, Tail);
			(F, [{wrapup, {Total, Props}} | Tail]) ->
				?assertEqual(10, Total),
				?assertEqual(10, proplists:get_value("agent2", Props)),
				?assertEqual(undefined, proplists:get_value("agent1", Props)),
				F(F, Tail)
		end,
		Test(Test, Dict)
	end},
	{"big kahuna:  queue hop, agent ringout, agent transfer, to queue again, then an agent handles it fully.",
	fun() ->
		Transactions = [
			#cdr_raw{id = "id", transaction = cdrinit, start = 5, ended = 60},
			#cdr_raw{id = "id", transaction = inqueue, start = 10, ended = 25, eventdata = "queue"},
			#cdr_raw{id = "id", transaction = ringing, start = 15, ended = 20, eventdata = "ringout_agent"},
			#cdr_raw{id = "id", transaction = ringing, start = 20, ended = 25, eventdata = "from_agent"},
			#cdr_raw{id = "id", transaction = oncall, start = 25, ended = 35, eventdata = "from_agent"},
			#cdr_raw{id = "id", transaction = ringing, start = 30, ended = 35, eventdata = "target_agent"},
			#cdr_raw{id = "id", transaction = oncall, start = 35, ended = 40, eventdata = "target_agent"},
			#cdr_raw{id = "id", transaction = wrapup, start = 35, ended = 40, eventdata = "from_agent"},
			#cdr_raw{id = "id", transaction = endwrapup, start = 40, ended = 40, eventdata = "from_agent"},
			#cdr_raw{id = "id", transaction = wrapup, start = 40, ended = 45, eventdata = "target_agent"},
			#cdr_raw{id = "id", transaction = inqueue, start = 40, ended = 50, eventdata = "new_queue"},
			#cdr_raw{id = "id", transaction = endwrapup, start = 45, ended = 45, eventdata = "target_agent"},
			#cdr_raw{id = "id", transaction = ringing, start = 45, ended = 50, eventdata = "competent_agent"},
			#cdr_raw{id = "id", transaction = oncall, start = 50, ended = 55, eventdata = "competent_agent"},
			#cdr_raw{id = "id", transaction = wrapup, start = 55, ended = 60, eventdata = "competent_agent"},
			#cdr_raw{id = "id", transaction = endwrapup, start = 60, ended = 60, eventdata = "competent_agent"},
			#cdr_raw{id = "id", transaction = cdrend, start = 60, ended = 60}
		],
		Dict = summarize(Transactions),
		Test = fun
			(_F, []) ->
				ok;
			(F, [{inqueue, {Total, Props}} | Tail]) ->
				?assertEqual(25, Total),
				?assertEqual(15, proplists:get_value("queue", Props)),
				?assertEqual(10, proplists:get_value("new_queue", Props)),
				F(F, Tail);
			(F, [{ringing, {Total, Props}} | Tail]) ->
				?assertEqual(20, Total),
				?assertEqual(5, proplists:get_value("ringout_agent", Props)),
				?assertEqual(5, proplists:get_value("from_agent", Props)),
				?assertEqual(5, proplists:get_value("target_agent", Props)),
				?assertEqual(5, proplists:get_value("competent_agent", Props)),
				F(F, Tail);
			(F, [{oncall, {Total, Props}} | Tail]) ->
				?assertEqual(undefined, proplists:get_value("ringout_agent", Props)),
				?assertEqual(20, Total),
				?assertEqual(10, proplists:get_value("from_agent", Props)),
				?assertEqual(5, proplists:get_value("target_agent", Props)),
				?assertEqual(5, proplists:get_value("competent_agent", Props)),
				F(F, Tail);
			(F, [{wrapup, {Total, Props}} | Tail]) ->
				?assertEqual(15, Total),
				?assertEqual(5, proplists:get_value("from_agent", Props)),
				?assertEqual(5, proplists:get_value("target_agent", Props)),
				?assertEqual(5, proplists:get_value("competent_agent", Props)),
				?assertEqual(undefined, proplists:get_value("ringout_agent", Props)),
				F(F, Tail)
		end,
		Test(Test, Dict)
	end},
	{"a basic warm transfer",
	fun() ->
		Transactions = [
			#cdr_raw{id = "id", transaction = cdrinit, start = 5, ended = 35},
			#cdr_raw{id = "id", transaction = inqueue, start = 10, ended = 20, eventdata = "queue"},
			#cdr_raw{id = "id", transaction = ringing, start = 15, ended = 20, eventdata = "agent"},
			#cdr_raw{id = "id", transaction = oncall, start = 20, ended = 25, eventdata = "agent"},
			#cdr_raw{id = "id", transaction = warmxfer_begin, start = 25, ended = 30, eventdata = {"agent", "dialed"}},
			#cdr_raw{id = "id", transaction = warmxfer_complete, start = 30, ended = 30, eventdata = "agent"},
			#cdr_raw{id = "id", transaction = wrapup, start = 30, ended = 35, eventdata = "agent"},
			#cdr_raw{id = "id", transaction = endwrapup, start = 35, ended = 35, eventdata = "agent"},
			#cdr_raw{id = "id", transaction = hangup, start = 40, ended = 40, eventdata = undefined},
			#cdr_raw{id = "id", transaction = cdrend, start = 40, ended = 40}
		],
		Topprop = summarize(Transactions),
		Assertfun = fun
			(_F, []) ->
				ok;
			(F, [{inqueue, {Total, Props}} | Tail]) ->
				?assertEqual(10, proplists:get_value("queue", Props)),
				?assertEqual(10, Total),
				F(F, Tail);
			(F, [{ringing, {Total, Props}} | Tail]) ->
				?assertEqual(5, proplists:get_value("agent", Props)),
				?assertEqual(5, Total),
				F(F, Tail);
			(F, [{oncall, {Total, Props}} | Tail]) ->
				?assertEqual(10, proplists:get_value("agent", Props)),
				?assertEqual(10, Total),
				F(F, Tail);
			(F, [{wrapup, {Total, Props}} | Tail]) ->
				?assertEqual(5, proplists:get_value("agent", Props)),
				?assertEqual(5, Total),
				F(F, Tail)
		end,
		Assertfun(Assertfun, Topprop)
	end},
	{"a multi fail warm transfer",
	fun() ->
		Transactions = [
			#cdr_raw{id = "id", transaction = cdrinit, start = 5, ended = 35},
			#cdr_raw{id = "id", transaction = inqueue, start = 10, ended = 20, eventdata = "queue"},
			#cdr_raw{id = "id", transaction = ringing, start = 15, ended = 20, eventdata = "agent"},
			#cdr_raw{id = "id", transaction = oncall, start = 20, ended = 25, eventdata = "agent"},
			#cdr_raw{id = "id", transaction = warmxfer_begin, start = 25, ended = 30, eventdata = {"agent", "dialed"}},
			#cdr_raw{id = "id", transaction = warmxfer_fail, start = 30, ended = 35, eventdata="agent"},
			#cdr_raw{id = "id", transaction = warmxfer_begin, start = 35, ended = 40, eventdata = {"agent", "dialed2"}},
			#cdr_raw{id = "id", transaction = warmxfer_fail, start = 40, ended = 45, eventdata = "agent"},
			#cdr_raw{id = "id", transaction = warmxfer_complete, start = 45, ended = 55, eventdata = "agent"},
			#cdr_raw{id = "id", transaction = wrapup, start = 45, ended = 50, eventdata = "agent"},
			#cdr_raw{id = "id", transaction = hangup, start = 55, ended = 55},
			#cdr_raw{id = "id", transaction = cdrend, start = 55, ended = 55}
		],
		Topprop = summarize(Transactions),
		Assertfun = fun
			(_F, []) ->
				ok;
			(F, [{inqueue, {Total, Props}} | Tail]) ->
				?assertEqual(10, proplists:get_value("queue", Props)),
				?assertEqual(10, Total),
				F(F, Tail);
			(F, [{ringing, {Total, Props}} | Tail]) ->
				?assertEqual(5, proplists:get_value("agent", Props)),
				?assertEqual(5, Total),
				F(F, Tail);
			(F, [{oncall, {Total, Props}} | Tail]) ->
				?assertEqual(25, proplists:get_value("agent", Props)),
				?assertEqual(25, Total),
				F(F, Tail);
			(F, [{wrapup, {Total, Props}} | Tail]) ->
				?assertEqual(5, proplists:get_value("agent", Props)),
				?assertEqual(5, Total),
				F(F, Tail)
		end,
		Assertfun(Assertfun, Topprop)
	end},
	{"agent transfer",
	fun() ->
		Transactions = [
			#cdr_raw{id = "id", transaction = cdrinit, start = 5, ended = 80},
			#cdr_raw{id = "id", transaction = inivr, start = 5, ended = 10},
			#cdr_raw{id = "id", transaction = inqueue, start = 10, ended = 20, eventdata = "queue"},
			#cdr_raw{id = "id", transaction = ringing, start = 15, ended = 20, eventdata = "agent"},
			#cdr_raw{id = "id", transaction = oncall, start = 20, ended = 25, eventdata = "agent"},
			#cdr_raw{id = "id", transaction = agent_transfer, start = 25, ended = 25, eventdata = "agent2"},
			#cdr_raw{id = "id", transaction = ringing, start = 25, ended = 30, eventdata = "agent2"},
			#cdr_raw{id = "id", transaction = oncall, start = 30, ended = 70, eventdata = "agent2"},
			#cdr_raw{id = "id", transaction = wrapup, start = 30, ended = 40, eventdata = "agent"},
			#cdr_raw{id = "id", transaction = endwrapup, start = 40, ended = 40, eventdata = "agent"},
			#cdr_raw{id = "id", transaction = hangup, start = 70, ended = 70},
			#cdr_raw{id = "id", transaction = wrapup, start = 70, ended = 80, eventdata = "agent2"},
			#cdr_raw{id = "id", transaction = endwrapup, start = 80, ended = 80, eventdata = "agent2"},
			#cdr_raw{id = "id", transaction = cdrend, start = 80, ended = 80}
		],
		Topprop = summarize(Transactions),
		Assertfun = fun
			(_F, []) ->
				ok;
			(F, [{inqueue, {Total, Props}} | Tail]) ->
				?assertEqual(10, proplists:get_value("queue", Props)),
				?assertEqual(10, Total),
				F(F, Tail);
			(F, [{ringing, {Total, Props}} | Tail]) ->
				?assertEqual(5, proplists:get_value("agent", Props)),
				?assertEqual(5, proplists:get_value("agent2", Props)),
				?assertEqual(10, Total),
				F(F, Tail);
			(F, [{oncall, {Total, Props}} | Tail]) ->
				?assertEqual(5, proplists:get_value("agent", Props)),
				?assertEqual(40, proplists:get_value("agent2", Props)),
				?assertEqual(45, Total),
				F(F, Tail);
			(F, [{wrapup, {Total, Props}} | Tail]) ->
				?assertEqual(10, proplists:get_value("agent", Props)),
				?assertEqual(10, proplists:get_value("agent2", Props)),
				?assertEqual(20, Total),
				F(F, Tail)
		end,
		Assertfun(Assertfun, Topprop)
	end}
	].
%
%mnesia_test_() ->
%	{foreach,
%	fun() ->
%		mnesia:stop(),
%		mnesia:delete_schema([node()]),
%		mnesia:create_schema([node()]),
%		mnesia:start(),
%		build_tables([node()]),
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

