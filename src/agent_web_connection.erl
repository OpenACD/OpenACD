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

%% @doc Handles the internal (cpx interaction) part of an agent web connection.
%% (2008 12 09 : marking this for a refactoring.  Micah)
%% @see agent_web_listener
-module(agent_web_connection).
-author("Micah").

-behaviour(gen_server).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-define(TICK_LENGTH, 5000000000).
-else.
-define(TICK_LENGTH, 10000).
-endif.

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("queue.hrl").

%% API
-export([
	start_link/2,
	start/2,
	stop/1,
	api/2,
	dump_agent/1,
	encode_statedata/1,
	set_salt/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-ifdef(R13B).
-type(ref() :: reference()).
-else.
-type(dict() :: any()).
-endif.

-record(state, {
	salt :: any(),
	ref :: ref() | 'undefined',
	agent_fsm :: pid() | 'undefined',
	ack_queue = dict:new() :: dict(), % key = counter, value is {when_qed, tries, pollitem} so that a message can be resent
	poll_queue = [] :: [{struct, [{binary(), any()}]}],
		% list of json structs to be sent to the client on poll.
		% struct MUST contain a counter, used to handle acks/errs
	missed_polls = 0 :: non_neg_integer(),
	counter = 1 :: non_neg_integer(),
	ack_timer :: {'ok', {'interval', ref()}} | 'undefined',
	poll_state :: atom(),
	poll_statedata :: any(),
	securitylevel = agent :: 'agent' | 'supervisor' | 'admin',
	listener :: 'undefined' | pid()
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

-type(json_simple() :: {'struct', [{binary(), binary()}]}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Description: Starts the server
%%--------------------------------------------------------------------

%% @doc Starts the passed agent at the given security level.
-spec(start_link/2 :: (Agent :: #agent{}, Security :: security_level()) -> {'ok', pid()}).
start_link(Agent, Security) ->
	gen_server:start_link(?MODULE, [Agent, Security], [{timeout, 10000}]).

%% @doc Starts the passed agent at the given security level.
-spec(start/2 :: (Agent :: #agent{}, Security :: security_level()) -> {'ok', pid()}).
start(Agent, Security) ->
	gen_server:start(?MODULE, [Agent, Security], [{timeout, 10000}]).

%% @doc Stops the passed Web connection process.
-spec(stop/1 :: (Pid :: pid()) -> 'ok').
stop(Pid) ->
	gen_server:call(Pid, stop).

%% @doc Do a web api call.
-spec(api/2 :: (Pid :: pid(), Apicall :: any()) -> any()).
api(Pid, Apicall) ->
	gen_server:call(Pid, Apicall).

%% @doc Dump the state of agent associated with the passed connection.
-spec(dump_agent/1 :: (Pid :: pid()) -> #agent{}).
dump_agent(Pid) ->
	gen_server:call(Pid, dump_agent).

%% @doc Sets the salt.  Hmmm, salt....
-spec(set_salt/2 :: (Pid :: pid(), Salt :: any()) -> 'ok').
set_salt(Pid, Salt) ->
	gen_server:cast(Pid, {set_salt, Salt}).

%% @doc Encode the given data into a structure suitable for mochijson2:encode
-spec(encode_statedata/1 :: 
	(Callrec :: #call{}) -> json_simple();
	(Clientrec :: #client{}) -> json_simple();
	({'onhold', Holdcall :: #call{}, 'calling', any()}) -> json_simple();
	({Relcode :: string(), Bias :: non_neg_integer()}) -> json_simple();
	('default') -> {'struct', [{binary(), 'default'}]};
	(List :: string()) -> binary();
	({}) -> 'false').
encode_statedata(Callrec) when is_record(Callrec, call) ->
	case Callrec#call.client of
		undefined ->
			Brand = "unknown client";
		Clientrec when is_record(Clientrec, client) ->
			Brand = Clientrec#client.label
	end,
	{struct, [
		{<<"callerid">>, list_to_binary(Callrec#call.callerid)},
		{<<"brandname">>, list_to_binary(Brand)},
		{<<"ringpath">>, Callrec#call.ring_path},
		{<<"mediapath">>, Callrec#call.media_path},
		{<<"callid">>, list_to_binary(Callrec#call.id)},
		{<<"type">>, Callrec#call.type}]};
encode_statedata(Clientrec) when is_record(Clientrec, client) ->
	{struct, [
		{<<"brandname">>, list_to_binary(Clientrec#client.label)}]};
encode_statedata({onhold, Holdcall, calling, Calling}) ->
	Holdjson = encode_statedata(Holdcall),
	Callingjson = encode_statedata(Calling),
	{struct, [
		{<<"onhold">>, Holdjson},
		{<<"calling">>, Callingjson}]};
encode_statedata({Relcode, _Bias}) ->
	{struct, [{<<"reason">>, list_to_binary(Relcode)}]};
encode_statedata(default) ->
	{struct, [{<<"reason">>, default}]};
encode_statedata(List) when is_list(List) ->
	list_to_binary(List);
encode_statedata({}) ->
	false.


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Agent, Security]) ->
	?DEBUG("web_connection init ~p", [Agent]),
	process_flag(trap_exit, true),
	case agent_manager:start_agent(Agent) of
		{ok, Apid} ->
			ok;
		{exists, Apid} ->
			ok
	end,
	case agent:set_connection(Apid, self()) of
		error ->
			{stop, "Agent is already logged in"};
		_Else ->
			{ok, Tref} = timer:send_interval(?TICK_LENGTH, check_acks),
			agent_web_listener:linkto(self()),
			{ok, #state{agent_fsm = Apid, ack_timer = Tref, securitylevel = Security, listener = whereis(agent_web_listener)}}
	end.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
	{stop, shutdown, ok, State};
handle_call(poll, _From, #state{poll_queue = Pollq} = State) ->
	State2 = State#state{poll_queue=[], missed_polls = 0, ack_queue = build_acks(State#state.poll_queue, State#state.ack_queue)},
	Json2 = {struct, [{success, true}, {message, <<"Poll successful">>}, {data, lists:reverse(Pollq)}]},
	{reply, {200, [], mochijson2:encode(Json2)}, State2};
handle_call(logout, _From, State) ->
	{stop, normal, {200, [{"Set-Cookie", "cpx_id=dead"}], mochijson2:encode({struct, [{success, true}]})}, State};
handle_call(get_avail_agents, _From, State) ->
	Agents = agent_manager:find_avail_agents_by_skill(['_all']),
	Noms = [list_to_binary(N) || {N, _Pid, _Rec} <- Agents],
	{reply, {200, [], mochijson2:encode({struct, [{success, true}, {<<"agents">>, Noms}]})}, State};
handle_call({set_state, Statename}, _From, #state{agent_fsm = Apid} = State) ->
	case agent:set_state(Apid, agent:list_to_state(Statename)) of
		ok ->
			{reply, {200, [], mochijson2:encode({struct, [{success, true}, {<<"status">>, ok}]})}, State};
		invalid ->
			{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"status">>, invalid}]})}, State}
	end;
handle_call({set_state, Statename, Statedata}, _From, #state{agent_fsm = Apid} = State) ->
	case agent:set_state(Apid, agent:list_to_state(Statename), Statedata) of
		invalid ->
			{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"status">>, invalid}]})}, State};
		Status ->
			{reply, {200, [], mochijson2:encode({struct, [{success, true}, {<<"status">>, Status}]})}, State}
	end;
handle_call({set_endpoint, Endpoint}, _From, #state{agent_fsm = Apid} = State) ->
	{reply, agent:set_endpoint(Apid, Endpoint), State};
handle_call({dial, Number}, _From, #state{agent_fsm = AgentPid} = State) ->
	%% don't like it, but hardcoding freeswitch
	case whereis(freeswitch_media_manager) of
		undefined ->
			{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Freeswitch not available">>}]})}, State};
		_Pid ->
			AgentRec = agent:dump_state(AgentPid),
			freeswitch_media_manager:make_outbound_call(Number, AgentPid, AgentRec),
			{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State}
	end;
handle_call(dump_agent, _From, #state{agent_fsm = Apid} = State) ->
	Astate = agent:dump_state(Apid),
	{reply, Astate, State};
handle_call({agent_transfer, Agentname}, _From, #state{agent_fsm = Apid} = State) ->
	case agent_manager:query_agent(Agentname) of
		{true, Target} ->
			Reply = case agent:agent_transfer(Apid, Target) of
				ok ->
					{200, [], mochijson2:encode({struct, [{success, true}]})};
				invalid ->
					{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Could not start transfer">>}]})}
			end,
			{reply, Reply, State};
		false ->
			{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Agent not found">>}]})}, State}
	end;
handle_call({warm_transfer, Number}, _From, #state{agent_fsm = Apid} = State) ->
	?NOTICE("warm transfer to ~p", [Number]),
	Reply = case agent:warm_transfer_begin(Apid, Number) of
		ok ->
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		invalid ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Could not start transfer">>}]})}
	end,
	{reply, Reply, State};
handle_call({supervisor, Request}, _From, #state{securitylevel = Seclevel} = State) when Seclevel =:= supervisor; Seclevel =:= admin ->
	?DEBUG("Handing supervisor request ~s", [lists:flatten(Request)]),
	case Request of
		["requeue", Fromagent, Toqueue] ->
			Json = case agent_manager:query_agent(Fromagent) of
				{true, Apid} ->
					case agent:get_media(Apid) of
						invalid ->
							mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Agent isn't in call">>}]});
						{ok, #call{source = Mpid} = Mediarec} ->
							case gen_media:queue(Mpid, Toqueue) of
								invalid ->
									mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Media said it couldn't be queued">>}]});
								ok ->
									mochijson2:encode({struct, [{success, true}]})
							end
					end;
				_Whatever ->
					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Agent doesn't exists">>}]})
			end,
			{reply, {200, [], Json}, State};
		["agent_transfer", Fromagent, Toagent] ->
			Json = case {agent_manager:query_agent(Fromagent), agent_manager:query_agent(Toagent)} of
				{false, false} ->
					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Agents don't exist">>}]});
				{{true, From}, {true, To}} ->
					agent:agent_transfer(From, To),
					mochijson2:encode({struct, [{success, true}, {<<"message">>, <<"Transfer beginning">>}]});
				{false, _} ->
					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"From agent doesn't exist.">>}]});
				{_, false} ->
					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"To agent doesn't exist.">>}]})
			end,
			{reply, {200, [], Json}, State};
		["agent_ring", Fromqueue, Callid, Toagent] ->
			Json = case {agent_manager:query_agent(Toagent), queue_manager:get_queue(Fromqueue)} of
				{false, undefined} ->
					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Neither agent nor queue exist">>}]});
				{false, _Pid} ->
					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"agent doesn't exist">>}]});
				{_Worked, undefined} ->
					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"queue doesn't exist">>}]});
				{{true, Apid}, Qpid} ->
					case call_queue:get_call(Qpid, Callid) of
						none ->
							mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Call is not in the given queue">>}]});
						{_Key, #queued_call{media = Mpid} = Qcall} ->
							case gen_media:ring(Mpid, Apid, Qcall, 30000) of
								ok ->
									mochijson2:encode({struct, [{success, true}]});
								invalid ->
									mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Could not ring agent">>}]})
							end
					end
			end,
			{reply, {200, [], Json}, State};
		["status"] ->
			% nodes, agents, queues, media, and system.
			{ok, Nodestats} = cpx_monitor:get_health(node),
			{ok, Agentstats} = cpx_monitor:get_health(agent),
			{ok, Queuestats} = cpx_monitor:get_health(queue),
			{ok, Systemstats} = cpx_monitor:get_health(system),
			{ok, Mediastats} = cpx_monitor:get_health(media),
			Groupstats = extract_groups(lists:append(Queuestats, Agentstats)),
			Stats = lists:append([Nodestats, Agentstats, Queuestats, Systemstats, Mediastats]),
			{Count, Encodedstats} = encode_stats(Stats),
			{_Count2, Encodedgroups} = encode_groups(Groupstats, Count),
			Encoded = lists:append(Encodedstats, Encodedgroups),
			Json = mochijson2:encode({struct, [
				{success, true},
				{<<"data">>, {struct, [
					{<<"identifier">>, <<"id">>},
					{<<"label">>, <<"display">>},
					{<<"items">>, Encoded}
				]}}
			]}),
			{reply, {200, [], Json}, State};
		
		
%			case file:read_file_info("sup_test_data.js") of
%				{error, Error} ->
%					?WARNING("Couldn't get test data due to ~p", [Error]),
%					{reply, {500, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Could not get data">>}]})}, State};
%				_Else ->
%					{ok, Io} = file:open("sup_test_data.js", [raw, binary]),
%					Read = fun(F, Acc) ->
%						case file:read(Io, 20) of
%							{ok, Data} ->
%								F(F, [Data | Acc]);
%							eof ->
%								lists:flatten(lists:reverse(Acc));
%							{error, Err} ->
%								Err
%						end
%					end,
%					Data = Read(Read, []),
%					file:close(Io),
%					{reply, {200, [], Data}, State}
%			end;
		["nodes"] ->
			Nodes = lists:sort([node() | nodes()]),
			F = fun(I) ->
				list_to_binary(atom_to_list(I))
			end,
			{reply, {200, [], mochijson2:encode({struct, [{success, true}, {<<"nodes">>, lists:map(F, Nodes)}]})}, State};
		[Node | Do] ->
			Nodes = get_nodes(Node),
			{Success, Result} = do_action(Nodes, Do, []),
			{reply, {200, [], mochijson2:encode({struct, [{success, Success}, {<<"result">>, Result}]})}, State}
	end;
handle_call({supervisor, _Request}, _From, State) ->
	?NOTICE("Unauthorized access to a supervisor web call", []),
	{reply, {403, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"insufficient privledges">>}]})}, State};
handle_call({mediapull, Data}, _From, #state{agent_fsm = Apid} = State) ->
	?INFO("mediapull request is ~s", [Data]),
	case agent:media_pull(Apid, Data) of
		invalid ->
			{reply, {200, [], "Nodata"}, State};
		{Heads, Html} ->
			{reply, {200, Heads, Html}, State}
	end;
handle_call({mediapush, Data}, _From, #state{agent_fsm = Apid} = State) ->
	case agent:media_push(Apid, Data) of
		invalid ->
			{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Invalid request">>}]})}, State};
		_Else ->
			{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State}
	end;
handle_call(Allothers, _From, State) ->
	{reply, {unknown_call, Allothers}, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({set_salt, Salt}, State) ->
	{noreply, State#state{salt = Salt}};
handle_cast({change_state, AgState, Data}, #state{poll_queue = Pollq, counter = Counter} = State) ->
	?DEBUG("State:  ~p; Data:  ~p", [AgState, Data]),
	Newqueue =
		[{struct, [
			{<<"counter">>, Counter},
			{<<"command">>, <<"astate">>},
			{<<"state">>, AgState},
			{<<"statedata">>, encode_statedata(Data)}
		]} | Pollq],
	{noreply, State#state{counter = Counter + 1, poll_queue = Newqueue}};
handle_cast({change_state, AgState}, #state{poll_queue = Pollq, counter = Counter} = State) ->
	Newqueue =
		[{struct, [
			{<<"counter">>, Counter},
			{<<"command">>, <<"astate">>},
			{<<"state">>, AgState}
		]} | Pollq],
	{noreply, State#state{counter = Counter + 1, poll_queue = Newqueue}};
handle_cast(Msg, State) ->
	?DEBUG("Other case ~p", [Msg]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(check_acks, #state{missed_polls = Missedpolls} = State) when Missedpolls < 4 ->
	{noreply, State#state{missed_polls = Missedpolls + 1}};
handle_info(check_acks, State) ->
	?NOTICE("too many missed polls.",[]),
	{stop, missed_polls, State};
%handle_info(check_listener, State) ->
%	case whereis(agent_web_listener) of
%		undefined ->
%			{ok, _Tref} = timer:send_after(1000, check_listener),
%			{noreply, State#state{listener = undefined}};
%		Pid when is_pid(Pid) ->
%			agent_web_listener:linkto(State#state.ref, State#state.salt, self()),
%			{noreply, State#state{listener = Pid}}
%	end;
handle_info({'EXIT', Pid, Reason}, #state{listener = Pid} = State) ->
	?WARNING("The listener at ~w died due to ~p", [Pid, Reason]),
	%{ok, _Tref} = timer:send_after(1000, check_listener),
	{stop, {listener_exit, Reason}, State};
%	{noreply, State#state{listener = undefined}};
handle_info(Info, State) ->
	?DEBUG("info I can't handle:  ~p", [Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(Reason, State) ->
	?NOTICE("terminated ~p", [Reason]),
	%agent:stop(State#state.agent_fsm),
	timer:cancel(State#state.ack_timer),
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

get_nodes("all") ->
	[node() | nodes()];
get_nodes(Nodestring) ->
	case atom_to_list(node()) of
		Nodestring ->
			[node()];
		_Else ->
			F = fun(N) ->
				atom_to_list(N) =/= Nodestring
			end,
			[Out | _Tail] = lists:dropwhile(F, nodes()),
			[Out]
	end.

-spec(do_action/3 :: (Nodes :: [atom()], Do :: any(), Acc :: [any()]) -> {'true' | 'false', any()}).
do_action([], _Do, Acc) ->
	{true, Acc};
%% get a list of the agent profiles and how many agents are logged into each
do_action([Node | Tail], ["agent_profiles"] = Do, Acc) ->
	Profiles = agent_auth:get_profiles(),
	Makeprops = fun({Name, _Skills}) ->
		{Name, 0}
	end,
	Dict = dict:from_list(lists:map(Makeprops, Profiles)),
	Agents = case rpc:call(Node, agent_manager, list, [], 1000) of
		{badrpc, timeout} ->
			[];
		Else ->
			Else
	end,
	F = fun({_Login, Pid}, Accin) -> 
		#agent{profile = Profile} = agent:dump_state(Pid),
		dict:store(Profile, dict:fetch(Profile, Accin) + 1, Accin)
	end,
	Newdict = lists:foldl(F, Dict, Agents),
	Proplist = dict:to_list(Newdict),
	Makestruct = fun({Name, Count}) ->
		{struct, [{<<"name">>, list_to_binary(Name)}, {<<"count">>, Count}]}
	end,
	Newacc = [{struct, [{<<"node">>, list_to_binary(atom_to_list(Node))}, {<<"profiles">>, lists:map(Makestruct, Proplist)}]} | Acc],
	do_action(Tail, Do, Newacc);
%% get a list of the queues, and how many calls are in each.
do_action([Node | Tail], ["queues"] = Do, Acc) ->
	Queuedict = case rpc:call(Node, queue_manager, print, [], 1000) of
		{badrpc, timeout} ->
			dict:new();
		Else ->
			Else
	end,
	Queuelist = dict:to_list(Queuedict),
	Makeprops = fun({Qname, Qpid}) ->
		Count = call_queue:call_count(Qpid),
		{struct, [{<<"name">>, list_to_binary(Qname)}, {<<"count">>, Count}]}
	end,
	Queues = lists:map(Makeprops, Queuelist),
	Newacc = [{struct, [{<<"node">>, list_to_binary(atom_to_list(Node))}, {<<"queues">>, Queues}]} | Acc],
	do_action(Tail, Do, Newacc);
%% get the agents that are a member of the given profile, and thier data.
do_action([Node | Tail], ["agent", Profile] = Do, Acc) ->
	Binprof = list_to_binary(Profile),
	Agents = case rpc:call(Node, agent_manager, list, [], 1000) of
		{badrpc, timeout} ->
			[];
		Else ->
			Else
	end,
	States = lists:map(fun({_, Pid}) -> agent:dump_state(Pid) end, Agents),
	Filter = fun(#agent{profile = Aprof}) ->
		list_to_binary(Aprof) =:= Binprof
	end,
	Filtered = lists:filter(Filter, States),
	Agentstructs = encode_agents(Filtered, []),
	Newacc = [{struct, [{<<"node">>, list_to_binary(atom_to_list(Node))}, {<<"agents">>, Agentstructs}]} | Acc],
	do_action(Tail, Do, Newacc);
%% get the agent state data (id call).
do_action([_Node | _Tail], ["agent", Agent, "callid"], _Acc) ->
	case agent_manager:query_agent(Agent) of
		false ->
			{false, <<"agent not found">>};
		{true, Pid} ->
			#agent{statedata = Call} = agent:dump_state(Pid),
			case Call of
				Call when is_record(Call, call) ->
					{true, encode_call(Call)};
				_Else ->
					{false, <<"not a call">>}
			end
	end;
%% get a summary of the given queue
do_action([_Node | _Tail], ["queue", Queue], _Acc) ->
	case queue_manager:get_queue(Queue) of
		undefined ->
			{false, <<"no such queue">>};
		Pid when is_pid(Pid) ->
			Weight = call_queue:get_weight(Pid),
			Count = call_queue:call_count(Pid),
			Calls = encode_queue_list(call_queue:get_calls(Pid), []),
			Encoded = {struct, [
				{<<"weight">>, Weight},
				{<<"count">>, Count},
				{<<"calls">>, Calls}
			]},
			{true, Encoded}
	end;
%% get a call from the given queue
do_action([_Node | _Tail], ["queue", Queue, Callid], _Acc) ->
	case queue_manager:get_queue(Queue) of
		undefined ->
			{false, <<"no such queue">>};
		Pid when is_pid(Pid) ->
			case call_queue:get_call(Pid, Callid) of
				{{Weight, {Mega, Sec, _Micro}}, Call} ->
					{struct, Preweight} = encode_call(Call),
					Time = (Mega * 100000) + Sec,
					Props = lists:append([{<<"weight">>, Weight}, {<<"queued">>, Time}], Preweight),
					{true, {struct, Props}};
				none ->
					{false, <<"no such call">>}
			end
	end;
do_action(Nodes, Do, _Acc) ->
	?INFO("Bumping back unknown request ~p for nodes ~p", [Do, Nodes]),
	{false, <<"unknown request">>}.

encode_agent(Agent) when is_record(Agent, agent) ->
	{Mega, Sec, _Micro} = Agent#agent.lastchangetimestamp,
	Now = (Mega * 1000000) + Sec,
	%Remnum = case Agent#agent.remotenumber of
		%undefined ->
			%<<"undefined">>;
		%Else when is_list(Else) ->
			%list_to_binary(Else)
	%end,
	Prestatedata = [
		{<<"login">>, list_to_binary(Agent#agent.login)},
		{<<"skills">>, cpx_web_management:encode_skills(Agent#agent.skills)},
		{<<"profile">>, list_to_binary(Agent#agent.profile)},
		{<<"state">>, Agent#agent.state},
		{<<"lastchanged">>, Now}
		%{<<"remotenumber">>, Remnum}
	],
	Statedata = case Agent#agent.state of
		Call when is_record(Call, call) ->
			list_to_binary(Call#call.id);
		_Else ->
			<<"niy">>
	end,
	Proplist = [{<<"statedata">>, Statedata} | Prestatedata],
	{struct, Proplist}.

encode_agents([], Acc) -> 
	lists:reverse(Acc);
encode_agents([Head | Tail], Acc) ->
	encode_agents(Tail, [encode_agent(Head) | Acc]).

encode_call(Call) when is_record(Call, call) ->
	{struct, [
		{<<"id">>, list_to_binary(Call#call.id)},
		{<<"type">>, Call#call.type},
		{<<"callerid">>, list_to_binary(Call#call.callerid)},
		{<<"client">>, encode_client(Call#call.client)},
		{<<"skills">>, cpx_web_management:encode_skills(Call#call.skills)},
		{<<"ringpath">>, Call#call.ring_path},
		{<<"mediapath">>, Call#call.media_path}
	]};
encode_call(Call) when is_record(Call, queued_call) ->
	Basecall = gen_server:call(Call#queued_call.media, get_call),
	{struct, Encodebase} = encode_call(Basecall),
	Newlist = [{<<"skills">>, cpx_web_management:encode_skills(Call#queued_call.skills)} | proplists:delete(<<"skills">>, Encodebase)],
	{struct, Newlist}.

%encode_calls([], Acc) ->
%	lists:reverse(Acc);
%encode_calls([Head | Tail], Acc) ->
%	encode_calls(Tail, [encode_call(Head) | Acc]).

encode_client(undefined) ->
	undefined;
encode_client(Client) when is_record(Client, client) ->
	{struct, [
		{<<"label">>, list_to_binary(Client#client.label)},
		{<<"tenant">>, Client#client.tenant},
		{<<"brand">>, Client#client.brand}
	]}.

%encode_clients([], Acc) ->
%	lists:reverse(Acc);
%encode_clients([Head | Tail], Acc) ->
%	encode_clients(Tail, [encode_client(Head) | Acc]).

encode_queue_list([], Acc) ->
	lists:reverse(Acc);
encode_queue_list([{{Priority, {Mega, Sec, _Micro}}, Call} | Tail], Acc) ->
	Time = (Mega * 1000000) + Sec,
	Struct = {struct, [
		{<<"queued">>, Time},
		{<<"priority">>, Priority},
		{<<"id">>, list_to_binary(Call#queued_call.id)}
	]},
	Newacc = [Struct | Acc],
	encode_queue_list(Tail, Newacc).

encode_stats(Stats) ->
	encode_stats(Stats, 1, []).

encode_stats([], Count, Acc) ->
	{Count - 1, Acc};
encode_stats([Head | Tail], Count, Acc) ->
	Proplisted = cpx_monitor:to_proplist(Head),
	Id = [{<<"id">>, Count}],
	Display = case proplists:get_value(name, Proplisted) of
		Name when is_binary(Name) ->
			[{<<"display">>, Name}];
		Name when is_list(Name) ->
			[{<<"display">>, list_to_binary(Name)}];
		Name when is_atom(Name) ->
			[{<<"display">>, Name}]
	end,
	Type = [{<<"type">>, proplists:get_value(type, Proplisted)}],
	Protohealth = proplists:get_value(health, Proplisted),
	Protodetails = proplists:get_value(details, Proplisted),
	Node = case Type of
		[{_, system}] ->
			[];
		[{_, node}] ->
			[];
		[{_, _Else}] ->
			[{node, proplists:get_value(node, Protodetails)}]
	end,
	Parent = case Type of
		[{_, system}] ->
			[];
		[{_, node}] ->
			[];
		[{_, agent}] ->
			[{<<"profile">>, list_to_binary(proplists:get_value(profile, Protodetails))}];
		[{_, queue}] ->
			[{<<"group">>, list_to_binary(proplists:get_value(group, Protodetails))}];
		[{_, media}] ->
			case {proplists:get_value(agent, Protodetails), proplists:get_value(queue, Protodetails)} of
				{undefined, undefined} ->
					?WARNING("Even though this media seems fully orphaned, I'm saying it's under the default queue for now.", []),
					[{queue, <<"default_queue">>}];
				{undefined, Queue} ->
					[{queue, list_to_binary(Queue)}];
				{Agent, undefined} ->
					[{agent, list_to_binary(Agent)}]
			end
	end,
	Scrubbedhealth = scrub_proplist(Protohealth),
	Scrubbeddetails = scrub_proplist(Protodetails),
	Health = [{<<"health">>, {struct, [{<<"_type">>, <<"details">>}, {<<"_value">>, encode_proplist(Scrubbedhealth)}]}}],
	Details = [{<<"details">>, {struct, [{<<"_type">>, <<"details">>}, {<<"_value">>, encode_proplist(Scrubbeddetails)}]}}],
	Encoded = lists:append([Id, Display, Type, Node, Parent, Health, Details]),
	Newacc = [{struct, Encoded} | Acc],
	encode_stats(Tail, Count + 1, Newacc).

encode_groups(Stats, Count) ->
	?DEBUG("Stats to encode:  ~p", [Stats]),
	encode_groups(Stats, Count + 1, []).

encode_groups([], Count, Acc) ->
	{Count - 1, Acc};
encode_groups([{Type, Name} | Tail], Count, Acc) ->
	Out = {struct, [
		{<<"id">>, Count},
		{<<"type">>, list_to_binary(Type)},
		{<<"display">>, list_to_binary(Name)},
		{<<"health">>, {struct, [
			{<<"_type">>, <<"details">>},
			{<<"_value">>, {struct, []}}
		]}}
	]},
	encode_groups(Tail, Count + 1, [Out | Acc]).

scrub_proplist(Proplist) ->
	scrub_proplist(Proplist, []).

scrub_proplist([], Acc) ->
	Acc;
scrub_proplist([Head | Tail], Acc) ->
	Newacc = case Head of
		{Key, Value} ->
			case lists:member(Key, [queue, parent, node, agent, profile, group, type]) of
				true ->
					Acc;
				false ->
					[Head | Acc]
			end;
		Val when is_atom(Val) ->
			[{Val, true} | Acc];
		_Val ->
			Acc
	end,
	scrub_proplist(Tail, Newacc).
		
encode_proplist(Proplist) ->
	Struct = encode_proplist(Proplist, []),
	{struct, Struct}.
	
encode_proplist([], Acc) ->
	lists:reverse(Acc);
encode_proplist([Entry | Tail], Acc) when is_atom(Entry) ->
	Newacc = [{Entry, true} | Acc],
	encode_proplist(Tail, Newacc);
encode_proplist([{Key, Value} | Tail], Acc) when is_list(Value) ->
	Newval = list_to_binary(Value),
	Newacc = [{Key, Newval} | Acc],
	encode_proplist(Tail, Newacc);
encode_proplist([{Key, Value} = Head | Tail], Acc) when is_atom(Value), is_atom(Key) ->
	encode_proplist(Tail, [Head | Acc]);
encode_proplist([{Key, Value} | Tail], Acc) when is_binary(Value); is_float(Value); is_integer(Value) ->
	Newacc = [{Key, Value} | Acc],
	encode_proplist(Tail, Newacc);
encode_proplist([_Head | Tail], Acc) ->
	encode_proplist(Tail, Acc).

extract_groups(Stats) ->
	?DEBUG("Stats to extract groups from:  ~p", [Stats]),
	extract_groups(Stats, []).

extract_groups([], Acc) ->
	Acc;
extract_groups([Head | Tail], Acc) ->
	Proplisted = cpx_monitor:to_proplist(Head),
	Details = proplists:get_value(details, Proplisted, []),
	case proplists:get_value(type, Proplisted) of
		queue ->
			Display = proplists:get_value(group, Details),
			case lists:member({"queuegroup", Display}, Acc) of
				true ->
					extract_groups(Tail, Acc);
				false ->
					Top = {"queuegroup", Display},
					extract_groups(Tail, [Top | Acc])
			end;
		agent ->
			Display = proplists:get_value(profile, Details),
			case lists:member({"agentprofile", Display}, Acc) of
				true ->
					extract_groups(Tail, Acc);
				false ->
					Top = {"agentprofile", Display},
					extract_groups(Tail, [Top | Acc])
			end;
		Else ->
			?DEBUG("no group to extract for type ~w", [Else]),
			extract_groups(Tail, Acc)
	end.
	
build_acks([], Acks) -> 
	Acks;
build_acks([{struct, Pollprops} | Pollqueue], Acks) -> 
	%[{Counter, Tried, Type, Data} | Pollqueue]
	Counter = proplists:get_value(<<"counter">>, Pollprops),
	case dict:find(Counter, Acks) of
		error ->
			Acks2 = dict:store(Counter, {now(), 0, {struct, Pollprops}}, Acks);
		{ok, {Time, Tried, _Struct}} ->
			Acks2 = dict:store(Counter, {Time, Tried+1, {struct, Pollprops}}, Acks)
	end,
	build_acks(Pollqueue, Acks2).
	
-ifdef(TEST).

set_state_test_() ->
	{
		foreach,
		fun() ->
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			mnesia:create_schema([node()]),
			mnesia:start(),
			agent_auth:start(),
			agent_manager:start([node()]),
			{ok, Connpid} = agent_web_connection:start(#agent{login = "testagent", skills = [english]}, agent),
			{Connpid}
		end,
		fun({Connpid}) ->
			stop(Connpid),
			agent_auth:stop(),
			agent_manager:stop()
		end,
		[
			fun({Connpid}) ->
				{"Set state valid",
				fun() ->
					Reply = gen_server:call(Connpid, {set_state, "idle"}),
					?assertEqual({200, [], [123, [34,"success", 34], 58,<<"true">>, 44, [34,<<"status">>, 34], 58, [34,"ok", 34], 125]}, Reply),
					Reply2 = gen_server:call(Connpid, {set_state, "released", "default"}),
					?assertEqual({200, [], [123, [34,"success", 34], 58,<<"true">>, 44, [34,<<"status">>, 34], 58, [34,"ok", 34], 125]}, Reply2)
				end}
			end,
			fun({Connpid}) ->
				{"Set state invalid",
				fun() ->
					Reply = gen_server:call(Connpid, {set_state, "wrapup"}),
					?CONSOLE("~p", [Reply]),
					?assertEqual({200, [], [123, [34,"success", 34], 58,<<"false">>, 44, [34,<<"status">>, 34], 58, [34,"invalid", 34], 125]}, Reply),
					Reply2 = gen_server:call(Connpid, {set_state, "wrapup", "garbage"}),
					?CONSOLE("~p", [Reply2]),
					?assertEqual({200, [], [123, [34,"success", 34], 58,<<"false">>, 44, [34,<<"status">>, 34], 58, [34,"invalid", 34], 125]}, Reply2)
				end}
			end
		]
	}.

-define(MYSERVERFUNC, 
	fun() ->
		["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		mnesia:create_schema([node()]),
		mnesia:start(),
		agent_auth:start(),
		Agent = #agent{login = "agent", skills = [english]},
		agent_manager:start([node()]),
		agent_auth:start(),
		{ok, Pid} = start_link(Agent, agent),
		unlink(Pid),
		Stopfun = fun() ->
			?CONSOLE("stopping agent_auth", []),
			agent_auth:stop(),
			?CONSOLE("stopping agent_manager", []),
			agent_manager:stop(),
			?CONSOLE("stopping web_connection at ~p: ~p", [Pid, is_process_alive(Pid)]),
			stop(Pid),
			?CONSOLE("stopping mnesia", []),
			mnesia:stop(),
			?CONSOLE("deleting schema", []),
			mnesia:delete_schema([node()]),
			?CONSOLE("all done", [])
		end,
		{Pid, Stopfun}
	end
).

-include("gen_server_test.hrl").


-endif.
