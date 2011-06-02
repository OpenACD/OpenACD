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

%% @doc Helper module to represent an supervisor logged in, but not
%% always as a routable agent.
%% 
%% {@web}
%%
%% This is similar to the {@link agent_web_connection} in that it exposes an
%% api meant for web calls and not the shell.
%% 
%% The functions in this documentation will have {@web} in front of their 
%% description.  You should not call these functions in the shell as they
%% likely won't work.  They are exported only to aid documentation.  
%% To call a function is very similar to using the json_api
%% in {@link cpx_web_management}.  A request is a json object with a 
%% `"function"' property and an `"args"' property.  Note unlike the 
%% json api there is no need to define a `"module"' property.  In the 
%% documentation of specific functions, references to a proplist should
%% be sent as a json object.  The response is a json object with a 
%% `"success"' property.  If the `"success"' property is set to true, 
%% there may be a `"result"' property holding more data (defined in the 
%% functions below).  If something went wrong, there will be a `"message"' 
%% and `"errcode"' property.  Usually the `"message"' will have a human 
%% readable message, while `"errcode"' could be used for translation.
%% 
%% The first argument in the web api functions MUST NOT be in the json
%% request.  The {@link agent_web_listener} will be able to figure out
%% which agent the request is meant for (assuming you logged in properly).
%% So, the args list in your ajax request will be one shorter then the 
%% functions below.  If a function below has only `Conn' as it's arugment
%% the `"args"' property can be omitted completely.
%% 
%% To make a web api call, make a post request to path "/supervisor" with 
%% one field named `"request"'.  The value of the request field should be a 
%% a json object:
%% <pre> {
%% 	"function":  string(),
%% 	"args":      [any()]
%% }</pre>
%% See a function's documentation for what `"args"' should be.
%% 
%% A response will have 3 major forms.
%% 
%% A very simple success:
%% <pre> {
%% 	"success":  true
%% }</pre>
%% A success with a result:
%% <pre> {
%% 	"success":  true,
%% 	"result":   any()
%% }</pre>
%% A failure:
%% <pre> {
%% 	"success":  false,
%% 	"message":  string(),
%% 	"errcode":  string()
%% }</pre>
%% @see agent_web_listener
%% @see agent_web_connection
%% @see cpx_web_management
-module(supervisor_web_connection).
-author("Micah").

-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("queue.hrl").
-include("web.hrl").

-include_lib("stdlib/include/qlc.hrl").

-define(POLL_FLUSH_INTERVAL, 500).

%% API
-export([
%	api/2,
%	encode_statedata/1,
%	format_status/2,
	is_web_api/2
]).

%% Web api exports.
-export([
	get_profiles/1,
%	spy/2,
%	set_state/3,
%	set_state/4,
%	set_profile/3,
%	kick_agent/2,
%	blab/3,
	status/1,
	subscribe/1
%	get_motd/1,
%	set_motd/2,
%	remove_problem_recording/2,
%	start_problem_recording/3,
%	get_avail_agents/1,
%	ring_agent/4,
%	peek/3,
%	drop_call/3,
%	voicemail/2,
%	get_queues/1
]).

%					url:"/supervisor/get_profiles",
%			url:'/supervisor/spy/' + this.name,
%		var geturl = "/supervisor/agentstate/" + escape(this.name) + "/" + stateName + stateData;
%			url:"/supervisor/set_profile",
%		var geturl = "/supervisor/kick_agent/" + escape(this.name);
%			url:'/supervisor/spy/' + agent,
%			url:"/supervisor/blab",
%	url:'/profilelist',
%			url:'/supervisor/status',
%			url:'/supervisor/getmotd',
%						url:'/supervisor/motd',
%			url:'/brandlist',
%							url:'/supervisor/remove_problem_recording/' + escape(clientId),
%			url: '/supervisor/start_problem_recording/' + window.agentConnection.username + '/' + escape(clientId),
%			url:'/get_avail_agents',
%				url:"/supervisor/agent_ring/" + escape(queue) + "/" + escape(id) + "/" + escape(agent),
%			url: '/supervisor/peek/' + queue + '/' + id,
%			url:'/supervisor/drop_call/' + queue + '/' + id,
%			url:'/supervisor/voicemail/' + escape(queue) + '/' + escape(mediaid),
%	url:'/queuelist',
%			url:'/get_avail_agents',
%				url:"/supervisor/agent_ring/" + escape(queue) + "/" + escape(id) + "/" + escape(agent),
%			url: '/supervisor/peek/' + queue + '/' + id,
%			url:'/supervisor/drop_call/' + queue + '/' + id,
%			url:'/supervisor/voicemail/' + escape(queue) + '/' + escape(mediaid),
%	url:'/queuelist',

-web_api_functions([
	{get_profiles, 1},
%	{spy, 2},
%	{set_state, 3},
%	{set_state, 4},
%	{set_profile, 3},
%	{kick_agent, 2},
%	{blab, 3},
	{status, 1},
	{unsubscribe, 1}
%	{get_motd, 1},
%	{set_motd, 2},
%	{remove_problem_recording, 2},
%	{start_problem_recording, 3},
%	{get_avail_agents, 1},
%	{ring_agent, 4},
%	{peek, 3},
%	{drop_call, 3},
%	{voicemail, 2},
%	{get_queues, 1}
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-type(tref() :: any()).

%-record(state, {
%	salt :: any(),
%	agent_fsm :: pid() | 'undefined',
%	current_call :: #call{} | 'undefined' | 'expect',
%	mediaload :: any(),
%	poll_queue = [] :: [{struct, [{binary(), any()}]}],
%		% list of json structs to be sent to the client on poll.
%	poll_flush_timer :: any(),
%	poll_pid :: 'undefined' | pid(),
%	poll_pid_established = 1 :: pos_integer(),
%	ack_timer :: tref() | 'undefined',
%	securitylevel = agent :: 'agent' | 'supervisor' | 'admin',
%	listener :: 'undefined' | pid()
%}).
-record(state, {
	login :: string(),
	endpointtype :: 'sip' | 'sip_registration' | 'pstn' | 'h323' | 'iax2',
	endpointdata :: any(),
	ring_path = inband :: 'inband' | 'outband',
	poll_queue,
	poll_flush_timer
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

-type(json_simple() :: {'struct', [{binary(), binary()}]}).
-type(bin_string() :: binary()).

%%====================================================================
%% WEB API
%%====================================================================

%% @doc {@web} Get the overall status cached in cpx_monitor; ie, a 
%% snapshot of The system as it currently is.  Subscribes the connection to
%% The cpx feed as well.
-spec(status/1 :: (Conn :: pid()) -> any()).
status(Conn) ->
	gen_server:call(Conn, {supervisor, status}).

%% @doc {@web} Subscribe the supervisor to the cpx_monitor feed.
-spec(unsubscribe/1 :: (Conn :: pid()) -> any()).
unsubscribe(Conn) ->
	gen_server:call(Conn, {supervisor, unsubscribe}).

%% @doc {@web} Get a list of the agent profiles.
-spec(get_profiles/1 :: (Conn :: pid()) -> any()).
get_profiles(Conn) ->
	gen_server:call(Conn, {supervisor, get_profiles}).
%%====================================================================
%% API
%%====================================================================

-type(login_opt() :: {login, string()}).
-type(endpoint_opt() :: {endpoint, 'sip' | 'sip_registration' | 'pstn' | 'h323' | 'iax2'}).
-type(endpointdata_opt() :: {endpointdata, any()}).
-type(ring_path_opt() :: {ring_path, 'inband' | 'outband'}).
-type(start_opt() :: 
	login_opt() | 
	endpoint_opt() | 
	endpointdata_opt() | 
	ring_path_opt()
).
-type(start_opts() :: [start_opt()]).

%% @doc Start unlinked.
-spec(start/1 :: (Opts :: start_opts()) -> {'ok', pid()}).
start(Opts) ->
	gen_server:start(?MODULE, Opts, [{timeout, 10000}]).

%% @doc Start linked.
-spec(start_link/1 :: (Opts :: start_opts()) -> {'ok', pid()}).
start_link(Opts) ->
	gen_server:start_link(?MODULE, Opts, [{timeout, 10000}]).

%% @doc Get the list of funcions/arities exposed for web use.
-spec(get_web_api/0 :: () -> [{atom(), non_neg_integer()}]).
get_web_api() ->
	Attrs = ?MODULE:module_info(attributes),
	proplists:get_value(web_api_functions, Attrs).

%% @doc Return if a function of given arity is a valid web api call.
-spec(is_web_api/2 :: (Function :: atom(), Arity :: non_neg_integer()) -> boolean()).
is_web_api(Function, Arity) ->
	Api = get_web_api(),
	lists:member({Function, Arity}, Api).

%%====================================================================
%% Init
%%====================================================================

init(Opts) ->
	State = #state{
		login = proplists:get_value(login, Opts),
		endpointtype = proplists:get_value(endpointtype, Opts),
		endpointdata = proplists:get_value(endpointdata, Opts),
		ring_path = proplists:get_value(ring_path, Opts, inband)
	},
	{ok, State}.

%%====================================================================
%% handle_call
%%====================================================================

handle_call({supervisor, get_profiles}, _From, State) ->
	Profiles = agent_auth:get_profiles(),
	F = fun(#agent_profile{name = Nom}) ->
		list_to_binary(Nom)
	end,
	{reply, {200, [], mochijson2:encode({struct, [{success, true}, {<<"result">>, lists:map(F, Profiles)}]})}, State};
handle_call({supervisor, unsubscribe}, _From, State) ->
	cpx_monitor:unsubscribe(),
	{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State};
handle_call({supervisor, status}, _From, State) ->
	% nodes, agents, queues, media, and system.
	cpx_monitor:subscribe(),
	Nodestats = qlc:e(qlc:q([X || {{node, _}, _, _, _, _, _} = X <- ets:table(cpx_monitor)])),
	Agentstats = qlc:e(qlc:q([X || {{agent, _}, _, _, _, _, _} = X <- ets:table(cpx_monitor)])),
	Queuestats = qlc:e(qlc:q([X || {{queue, _}, _, _, _, _, _} = X <- ets:table(cpx_monitor)])),
	Systemstats = qlc:e(qlc:q([X || {{system, _}, _, _, _, _, _} = X <- ets:table(cpx_monitor)])),
	Mediastats = qlc:e(qlc:q([X || {{media, _}, _, _, _, _, _} = X <- ets:table(cpx_monitor)])),
	Groupstats = extract_groups(lists:append(Queuestats, Agentstats)),
	Stats = lists:append([Nodestats, Agentstats, Queuestats, Systemstats, Mediastats]),
	{Count, Encodedstats} = encode_stats(Stats),
	{_Count2, Encodedgroups} = encode_groups(Groupstats, Count),
	Encoded = lists:append(Encodedstats, Encodedgroups),
	Systemjson = {struct, [
		{<<"id">>, <<"system-System">>},
		{<<"type">>, <<"system">>},
		{<<"display">>, <<"System">>},
		{<<"details">>, {struct, [{<<"_type">>, <<"details">>}, {<<"_value">>, {struct, []}}]}}
	]},
	Json = mochijson2:encode({struct, [
		{success, true},
		{<<"result">>, {struct, [
			{<<"identifier">>, <<"id">>},
			{<<"label">>, <<"display">>},
			{<<"items">>, [Systemjson | Encoded]}
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

handle_call({Request, Args}, _From, State) ->
	{reply, {200, [], mochijson2:encode({struct, [
		{success, false},
		{<<"message">>, <<"unknown request">>},
		{<<"errcode">>, <<"UNKNOWN_REQUEST">>}
	]})}, State}.

%%====================================================================
%% handle_cast
%%====================================================================

handle_cast(_Msg, State) ->
	{noreply, State}.

%%====================================================================
%% handle_info
%%====================================================================
%%
handle_info({cpx_monitor_event, {info, _, _}}, State) ->
	% TODO fix the subscribe, or start using this.
	{noreply, State};
handle_info({cpx_monitor_event, Message}, State) ->
	%?DEBUG("Ingesting cpx_monitor_event ~p", [Message]),
	Json = case Message of
		{drop, _Timestamp, {Type, Name}} ->
			Fixedname = if 
				is_atom(Name) ->
					 atom_to_binary(Name, latin1); 
				 true -> 
					 list_to_binary(Name) 
			end,
			{struct, [
				{<<"command">>, <<"supervisortab">>},
				{<<"data">>, {struct, [
					{<<"action">>, drop},
					{<<"type">>, Type},
					{<<"id">>, list_to_binary([atom_to_binary(Type, latin1), $-, Fixedname])},
					{<<"name">>, Fixedname}
				]}}
			]};
		{set, _Timestamp, {{Type, Name}, Detailprop, _Node}} ->
			Encodeddetail = encode_proplist(Detailprop),
			Fixedname = if 
				is_atom(Name) ->
					 atom_to_binary(Name, latin1); 
				 true -> 
					 list_to_binary(Name) 
			end,
			{struct, [
				{<<"command">>, <<"supervisortab">>},
				{<<"data">>, {struct, [
					{<<"action">>, set},
					{<<"id">>, list_to_binary([atom_to_binary(Type, latin1), $-, Fixedname])},
					{<<"type">>, Type},
					{<<"name">>, Fixedname},
					{<<"display">>, Fixedname},
					{<<"details">>, Encodeddetail}
				]}}
			]}
	end,
	Newstate = push_event(Json, State),
	{noreply, Newstate};

handle_info(_Msg, State) ->
	{noreply, State}.

%%====================================================================
%% terminate
%%====================================================================

terminate(_Cause, State) ->
	ok.

%%====================================================================
%% code change
%%====================================================================

code_change(_Old, State, _Extra) ->
	{ok, State}.

%%====================================================================
%% internal functions
%%====================================================================

extract_groups(Stats) ->
	extract_groups(Stats, []).

extract_groups([], Acc) ->
	Acc;
extract_groups([{{queue, _Id}, Details, _Node, _Time, _Watched, _Monref} = _Head | Tail], Acc) ->
	Display = proplists:get_value(group, Details),
	case lists:member({"queuegroup", Display}, Acc) of
		true ->
			extract_groups(Tail, Acc);
		false ->
			Top = {"queuegroup", Display},
			extract_groups(Tail, [Top | Acc])
	end;
extract_groups([{{agent, _Id}, Details, _Node, _Time, _Watched, _Monref} = _Head | Tail], Acc) ->
	Display = proplists:get_value(profile, Details),
	case lists:member({"agentprofile", Display}, Acc) of
		true ->
			extract_groups(Tail, Acc);
		false ->
			Top = {"agentprofile", Display},
			extract_groups(Tail, [Top | Acc])
	end;
extract_groups([_Head | Tail], Acc) ->
	extract_groups(Tail, Acc).


		
encode_stats(Stats) ->
	encode_stats(Stats, 1, []).

encode_stats([], Count, Acc) ->
	{Count - 1, Acc};
encode_stats([{{Type, ProtoName}, Protodetails, Node, _Time, _Watched, _Mon} = _Head | Tail], Count, Acc) ->
	Display = case {ProtoName, Type} of
		{_Name, agent} ->
			Login = proplists:get_value(login, Protodetails),
			[{<<"display">>, list_to_binary(Login)}];
		{Name, _} when is_binary(Name) ->
			[{<<"display">>, Name}];
		{Name, _} when is_list(Name) ->
			[{<<"display">>, list_to_binary(Name)}];
		{Name, _} when is_atom(Name) ->
			[{<<"display">>, Name}]
	end,
	Id = case is_atom(ProtoName) of
		true ->
			list_to_binary(lists:flatten([atom_to_list(Type), "-", atom_to_list(ProtoName)]));
		false ->
			% Here's hoping it's a string or binary.
			list_to_binary(lists:flatten([atom_to_list(Type), "-", ProtoName]))
	end,
	Parent = case Type of
		system ->
			[];
		node ->
			[];
		agent ->
			[{<<"profile">>, list_to_binary(proplists:get_value(profile, Protodetails))}];
		queue ->
			[{<<"group">>, list_to_binary(proplists:get_value(group, Protodetails))}];
		media ->
			case {proplists:get_value(agent, Protodetails), proplists:get_value(queue, Protodetails)} of
				{undefined, undefined} ->
					?DEBUG("Ignoring ~p as it's likely in ivr (no agent/queu)", [ProtoName]),
					[];
				{undefined, Queue} ->
					[{queue, list_to_binary(Queue)}];
				{Agent, undefined} ->
					[{agent, list_to_binary(Agent)}]
			end
	end,
	Scrubbeddetails = Protodetails,
	Details = [{<<"details">>, {struct, [{<<"_type">>, <<"details">>}, {<<"_value">>, encode_proplist(Scrubbeddetails)}]}}],
	Encoded = lists:append([[{<<"id">>, Id}], Display, [{<<"type">>, Type}], [{node, Node}], Parent, Details]),
	Newacc = [{struct, Encoded} | Acc],
	encode_stats(Tail, Count + 1, Newacc).

-spec(encode_groups/2 :: (Stats :: [{string(), string()}], Count :: non_neg_integer()) -> {non_neg_integer(), [tuple()]}).
encode_groups(Stats, Count) ->
	%?DEBUG("Stats to encode:  ~p", [Stats]),
	encode_groups(Stats, Count + 1, [], [], []).

-spec(encode_groups/5 :: (Groups :: [{string(), string()}], Count :: non_neg_integer(), Acc :: [tuple()], Gotqgroup :: [string()], Gotaprof :: [string()]) -> {non_neg_integer(), [tuple()]}).
encode_groups([], Count, Acc, Gotqgroup, Gotaprof) ->
	F = fun() ->
		Qqh = qlc:q([{Qgroup, "queuegroup"} || #queue_group{name = Qgroup} <- mnesia:table(queue_group), lists:member(Qgroup, Gotqgroup) =:= false]),
		Aqh = qlc:q([{Aprof, "agentprofile"} || #agent_profile{name = Aprof} <- mnesia:table(agent_profile), lists:member(Aprof, Gotaprof) =:= false]),
		Qgroups = qlc:e(Qqh),
		Aprofs = qlc:e(Aqh),
		lists:append(Qgroups, Aprofs)
	end,
	Encode = fun({Name, Type}) ->
		{struct, [
			{<<"id">>, list_to_binary(lists:append([Type, "-", Name]))},
			{<<"type">>, list_to_binary(Type)},
			{<<"display">>, list_to_binary(Name)}
		]}
	end,
	{atomic, List} = mnesia:transaction(F),
	Encoded = lists:map(Encode, List),
	Newacc = lists:append([Acc, Encoded]),
	{Count + length(Newacc), Newacc};
encode_groups([{Type, Name} | Tail], Count, Acc, Gotqgroup, Gotaprof) ->
	Out = {struct, [
		{<<"id">>, list_to_binary(lists:append([Type, "-", Name]))},
		{<<"type">>, list_to_binary(Type)},
		{<<"display">>, list_to_binary(Name)}
	]},
	{Ngotqgroup, Ngotaprof} = case Type of
		"queuegroup" ->
			{[Name | Gotqgroup], Gotaprof};
		"agentprofile" ->
			{Gotqgroup, [Name | Gotaprof]}
	end,
	encode_groups(Tail, Count + 1, [Out | Acc], Ngotqgroup, Ngotaprof).


encode_proplist(Proplist) ->
	Struct = encode_proplist(Proplist, []),
	{struct, Struct}.
	
encode_proplist([], Acc) ->
	lists:reverse(Acc);
encode_proplist([Entry | Tail], Acc) when is_atom(Entry) ->
	Newacc = [{Entry, true} | Acc],
	encode_proplist(Tail, Newacc);
encode_proplist([{skills, _Skills} | Tail], Acc) ->
	encode_proplist(Tail, Acc);
encode_proplist([{Key, {timestamp, Num}} | Tail], Acc) when is_integer(Num) ->
	Newacc = [{Key, {struct, [{timestamp, Num}]}} | Acc],
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
encode_proplist([{Key, Value} | Tail], Acc) when is_record(Value, client) ->
	Label = case Value#client.label of
		undefined ->
			undefined;
		_ ->
			list_to_binary(Value#client.label)
	end,
	encode_proplist(Tail, [{Key, Label} | Acc]);
encode_proplist([{callerid, {CidName, CidDAta}} | Tail], Acc) ->
	CidNameBin = list_to_binary(CidName),
	CidDAtaBin = list_to_binary(CidDAta),
	Newacc = [{callid_name, CidNameBin} | [{callid_data, CidDAtaBin} | Acc ]],
	encode_proplist(Tail, Newacc);
encode_proplist([{Key, Media} | Tail], Acc) when is_record(Media, call) ->
	Simple = [{callerid, Media#call.callerid},
	{type, Media#call.type},
	{client, Media#call.client},
	{direction, Media#call.direction},
	{id, Media#call.id}],
	Json = encode_proplist(Simple),
	encode_proplist(Tail, [{Key, Json} | Acc]);
encode_proplist([{Key, {onhold, Media, calling, Number}} | Tail], Acc) when is_record(Media, call) ->
	Simple = [
		{callerid, Media#call.callerid},
		{type, Media#call.type},
		{client, Media#call.client},
		{direction, Media#call.direction},
		{id, Media#call.id},
		{calling, list_to_binary(Number)}
	],
	Json = encode_proplist(Simple),
	encode_proplist(Tail, [{Key, Json} | Acc]);
encode_proplist([_Head | Tail], Acc) ->
	encode_proplist(Tail, Acc).


-spec(push_event/2 :: (Eventjson :: json_simple(), State :: #state{}) -> #state{}).
push_event(Eventjson, State) ->
	Newqueue = [Eventjson | State#state.poll_queue],
	case State#state.poll_flush_timer of
		undefined ->
			Self = self(),
			State#state{poll_flush_timer = erlang:send_after(?POLL_FLUSH_INTERVAL, Self, poll_flush), poll_queue = Newqueue};
		_ ->
			State#state{poll_queue = Newqueue}
	end.



%%====================================================================
%% tests
%%====================================================================

-ifdef(TEST).

encode_proplist_test() ->
	Input = [
		boolean,
		{list, "This is a list"},
		{keyatom, valatom},
		{binary, <<"binary data">>},
		{integer, 42},
		{float, 23.5},
		{tuple, {<<"this">>, <<"gets">>, <<"stripped">>}}
	],
	Expected = {struct, [
		{boolean, true},
		{list, <<"This is a list">>},
		{keyatom, valatom},
		{binary, <<"binary data">>},
		{integer, 42},
		{float, 23.5}
	]},
	Out = encode_proplist(Input),
	?assertEqual(Expected, Out).

-endif.


%handle_call({{supervisor, Request}, Post}, _From, #state{securitylevel = Seclevel} = State) when Seclevel =:= supervisor; Seclevel =:= admin ->
%	?DEBUG("Supervisor request with post data:  ~s", [lists:flatten(Request)]),
%	case Request of
%		["set_profile"] ->
%			Login = proplists:get_value("name", Post),
%			%Id = proplists:get_value("id", Post),
%			Newprof = proplists:get_value("profile", Post),
%			Midgood = case agent_manager:query_agent(Login) of
%				{true, Apid} ->
%					agent:change_profile(Apid, Newprof);
%				false ->
%					{error, noagent}
%			end,
%			case {Midgood, proplists:get_value("makePerm", Post)} of
%				{{error, Err}, _} ->
%					Msg = case Err of
%						noagent ->
%							<<"unknown agent">>;
%						unknown_profile ->
%							<<"unknown profile">>%;
%%						_ ->
%%							list_to_binary(io_lib:format("~p", [Err]))
%					end,
%					{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, Msg}]})}, State};
%				{ok, "makePerm"} ->
%					case agent_auth:get_agent(login, Login) of
%						{atomic, [Arec]} ->
%							case agent_auth:set_agent(Arec#agent_auth.id, Login, Arec#agent_auth.skills, Arec#agent_auth.securitylevel, Newprof) of
%								{atomic, ok} ->
%									{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State};
%								{aborted, Err} ->
%									Msg = list_to_binary(io_lib:format("Profile changed, but not permanent:  ~p", [Err])),
%									{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, Msg}]})}, State}
%							end;
%						{atomic, [_A, _B | _]} ->
%							{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Multiple agent records found, not making a change">>}]})}, State};
%						{atomic, []} ->
%							{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Agent is not permanent, so not permanent change made">>}]})}, State}
%					end;
%				{ok, _} ->
%					{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State}
%			end;
%		["blab"] ->
%			Toagentmanager = case proplists:get_value("type", Post) of
%				"agent" ->
%					{agent, proplists:get_value("value", Post, "")};
%				"node" ->
%					case proplists:get_value("value", Post) of
%						"System" ->
%							all;
%						_AtomIsIt -> 
%							try list_to_existing_atom(proplists:get_value("value", Post)) of
%								Atom ->
%									case lists:member(Atom, [node() | nodes()]) of
%										true ->
%											{node, Atom};
%										false ->
%											{false, false}
%									end
%							catch
%								error:badarg ->
%									{false, false}
%							end
%					end;
%				"profile" ->
%					{profile, proplists:get_value("value", Post, "")};
%				"all" ->
%					all
%			end,
%			Json = case Toagentmanager of
%				{false, false} ->
%					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"bad type or value">>}]});
%				Else ->
%					agent_manager:blab(proplists:get_value("message", Post, ""), Else),
%					mochijson2:encode({struct, [{success, true}, {<<"message">>, <<"blabbing">>}]})
%			end,
%			{reply, {200, [], Json}, State};
%		["motd"] ->
%			{ok, Appnodes} = application:get_env('OpenACD', nodes),
%			Nodes = case proplists:get_value("node", Post) of
%				"system" ->
%					Appnodes;
%				Postnode ->
%					case lists:any(fun(N) -> atom_to_list(N) == Postnode end, Appnodes) of
%						true ->
%							[list_to_existing_atom(Postnode)];
%						false ->
%							[]
%					end
%			end,
%			case Nodes of
%				[] ->
%					{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"no known nodes">>}]})}, State};
%				_ ->
%					Fun = case proplists:get_value("message", Post) of
%						"" ->
%							fun(Node) ->
%								try rpc:call(Node, cpx_supervisor, drop_value, [motd]) of
%									{atomic, ok} ->
%										ok
%								catch
%									_:_ ->
%										?WARNING("Could not set motd on ~p", [Node])
%								end
%							end;
%						Message ->
%							fun(Node) ->
%								try rpc:call(Node, cpx_supervisor, set_value, [motd, Message]) of
%									{atomic, ok} ->
%										ok
%								catch
%									_:_ ->
%										?WARNING("Count not set motd on ~p", [Node])
%								end
%							end
%					end,
%					lists:foreach(Fun, Nodes),
%					{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State}
%			end
%	end;
%handle_call({supervisor, Request}, _From, #state{securitylevel = Seclevel} = State) when Seclevel =:= supervisor; Seclevel =:= admin ->
%	?DEBUG("Handing supervisor request ~s", [lists:flatten(Request)]),
%	case Request of
%		["startmonitor"] ->
%			cpx_monitor:subscribe(),
%			{reply, {200, [], mochijson2:encode({struct, [{success, true}, {<<"message">>, <<"subscribed">>}]})}, State};
%		["start_problem_recording", _Agentname, Clientid] ->
%			AgentRec = agent:dump_state(State#state.agent_fsm), % TODO - avoid
%			case whereis(freeswitch_media_manager) of
%				P when is_pid(P) ->
%					case freeswitch_media_manager:record_outage(Clientid, State#state.agent_fsm, AgentRec) of
%						ok ->
%							{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State};
%						{error, Reason} ->
%							{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, list_to_binary(io_lib:format("Initializing recording channel failed (~p)", [Reason]))}]})}, State}
%					end;
%				_ ->
%					{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"freeswitch is not available">>}]})}, State}
%			end;
%		["remove_problem_recording", Clientid] ->
%			case file:delete("/tmp/"++Clientid++"/problem.wav") of
%				ok ->
%					{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State};
%				{error, Reason} ->
%					{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, Reason}]})}, State}
%			end;
%		["voicemail", Queue, Callid] ->
%			Json = case queue_manager:get_queue(Queue) of
%				Qpid when is_pid(Qpid) ->
%					case call_queue:get_call(Qpid, Callid) of
%						{_Key, #queued_call{media = Mpid}} ->
%							case gen_media:voicemail(Mpid) of
%								invalid ->
%									{struct, [{success, false}, {<<"message">>, <<"media doesn't support voicemail">>}]};
%								ok ->
%									{struct, [{success, true}]}
%							end;
%						_ ->
%							{struct, [{success, false}, {<<"message">>, <<"call not found">>}]}
%					end;
%				_ ->
%					{struct, [{success, false}, {<<"message">>, <<"queue not found">>}]}
%			end,
%			{reply, {200, [], mochijson2:encode(Json)}, State};
%		["set_profile", Agent, Profile] ->
%			case agent_manager:query_agent(Agent) of
%				{true, Apid} ->
%					case agent:change_profile(Apid, Profile) of
%						ok ->
%							{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State};
%						{error, unknown_profile} ->
%							{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"unknown_profile">>}]})}, State}
%					end;
%				false ->
%					{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"unknown agent">>}]})}, State}
%			end;
%		["endmonitor"] ->
%			cpx_monitor:unsubscribe(),
%			{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State};
%		["spy", Agentname] ->
%			Current = State#state.current_call,
%			{Json, Newcurrent}  = case agent_manager:query_agent(Agentname) of
%				{true, Apid} ->
%					Mepid = State#state.agent_fsm,
%					case agent:spy(Mepid, Apid) of
%						ok ->
%							{mochijson2:encode({struct, [{success, true}]}), expect};
%						invalid ->
%							{mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"invalid action">>}]}), Current}
%					end;
%				false ->
%					{mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"no such agent">>}]}), Current}
%			end,
%			{reply, {200, [], Json}, State#state{current_call = Newcurrent}};
%		["agentstate" | [Agent | Tail]] ->
%			Json = case agent_manager:query_agent(Agent) of
%				{true, Apid} ->
%					%?DEBUG("Tail:  ~p", [Tail]),
%					Statechange = case Tail of
%						["released", "default"] ->
%							agent:set_state(Apid, released, default);
%						[Statename, Statedata] ->
%							Astate = agent:list_to_state(Statename),
%							agent:set_state(Apid, Astate, Statedata);
%						[Statename] ->
%							Astate = agent:list_to_state(Statename),
%							agent:set_state(Apid, Astate)
%					end,
%					case Statechange of
%						invalid ->
%							mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"invalid state change">>}]});
%						ok ->
%							mochijson2:encode({struct, [{success, true}, {<<"message">>, <<"agent state set">>}]});
%						queued ->
%							mochijson2:encode({struct, [{success, true}, {<<"message">>, <<"agent release queued">>}]})
%					end;
%				_Else ->
%					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Agent not found">>}]})
%			end,
%			{reply, {200, [], Json}, State};
%		["kick_agent", Agent] ->
%			Json = case agent_manager:query_agent(Agent) of
%				{true, Apid} ->
%					case agent:query_state(Apid) of
%						{ok, oncall} ->
%							mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Agent currently oncall">>}]});
%						{ok, _State} ->
%							agent:stop(Apid),
%							mochijson2:encode({struct, [{success, true}]})
%					end;
%				_Else ->
%					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Agent not found">>}]})
%			end,
%			{reply, {200, [], Json}, State};
%		["requeue", Fromagent, Toqueue] ->
%			Json = case agent_manager:query_agent(Fromagent) of
%				{true, Apid} ->
%					case agent:get_media(Apid) of
%						invalid ->
%							mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Agent isn't in call">>}]});
%						{ok, #call{source = Mpid} = _Mediarec} ->
%							case gen_media:queue(Mpid, Toqueue) of
%								invalid ->
%									mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Media said it couldn't be queued">>}]});
%								ok ->
%									mochijson2:encode({struct, [{success, true}]})
%							end
%					end;
%				_Whatever ->
%					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Agent doesn't exists">>}]})
%			end,
%			{reply, {200, [], Json}, State};
%		["agent_transfer", Fromagent, Toagent] ->
%			Json = case {agent_manager:query_agent(Fromagent), agent_manager:query_agent(Toagent)} of
%				{false, false} ->
%					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Agents don't exist">>}]});
%				{{true, From}, {true, To}} ->
%					agent:agent_transfer(From, To),
%					mochijson2:encode({struct, [{success, true}, {<<"message">>, <<"Transfer beginning">>}]});
%				{false, _} ->
%					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"From agent doesn't exist.">>}]});
%				{_, false} ->
%					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"To agent doesn't exist.">>}]})
%			end,
%			{reply, {200, [], Json}, State};
%		["agent_ring", Fromqueue, Callid, Toagent] ->
%			Json = case {agent_manager:query_agent(Toagent), queue_manager:get_queue(Fromqueue)} of
%				{false, undefined} ->
%					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Neither agent nor queue exist">>}]});
%				{false, _Pid} ->
%					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"agent doesn't exist">>}]});
%				{_Worked, undefined} ->
%					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"queue doesn't exist">>}]});
%				{{true, Apid}, Qpid} ->
%					case call_queue:get_call(Qpid, Callid) of
%						none ->
%							mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Call is not in the given queue">>}]});
%						{_Key, #queued_call{media = Mpid} = Qcall} ->
%							case gen_media:ring(Mpid, Apid, Qcall, ?getRingout) of
%								deferred ->
%									mochijson2:encode({struct, [{success, true}]});
%								 _ ->
%									mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Could not ring agent">>}]})
%							end
%					end
%			end,
%			{reply, {200, [], Json}, State};
%		["status"] ->
%			% nodes, agents, queues, media, and system.
%			cpx_monitor:subscribe(),
%			Nodestats = qlc:e(qlc:q([X || {{node, _}, _, _, _, _, _} = X <- ets:table(cpx_monitor)])),
%			Agentstats = qlc:e(qlc:q([X || {{agent, _}, _, _, _, _, _} = X <- ets:table(cpx_monitor)])),
%			Queuestats = qlc:e(qlc:q([X || {{queue, _}, _, _, _, _, _} = X <- ets:table(cpx_monitor)])),
%			Systemstats = qlc:e(qlc:q([X || {{system, _}, _, _, _, _, _} = X <- ets:table(cpx_monitor)])),
%			Mediastats = qlc:e(qlc:q([X || {{media, _}, _, _, _, _, _} = X <- ets:table(cpx_monitor)])),
%			Groupstats = extract_groups(lists:append(Queuestats, Agentstats)),
%			Stats = lists:append([Nodestats, Agentstats, Queuestats, Systemstats, Mediastats]),
%			{Count, Encodedstats} = encode_stats(Stats),
%			{_Count2, Encodedgroups} = encode_groups(Groupstats, Count),
%			Encoded = lists:append(Encodedstats, Encodedgroups),
%			Systemjson = {struct, [
%				{<<"id">>, <<"system-System">>},
%				{<<"type">>, <<"system">>},
%				{<<"display">>, <<"System">>},
%				{<<"details">>, {struct, [{<<"_type">>, <<"details">>}, {<<"_value">>, {struct, []}}]}}
%			]},
%			Json = mochijson2:encode({struct, [
%				{success, true},
%				{<<"data">>, {struct, [
%					{<<"identifier">>, <<"id">>},
%					{<<"label">>, <<"display">>},
%					{<<"items">>, [Systemjson | Encoded]}
%				]}}
%			]}),
%			{reply, {200, [], Json}, State};
%		
%		
%%			case file:read_file_info("sup_test_data.js") of
%%				{error, Error} ->
%%					?WARNING("Couldn't get test data due to ~p", [Error]),
%%					{reply, {500, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Could not get data">>}]})}, State};
%%				_Else ->
%%					{ok, Io} = file:open("sup_test_data.js", [raw, binary]),
%%					Read = fun(F, Acc) ->
%%						case file:read(Io, 20) of
%%							{ok, Data} ->
%%								F(F, [Data | Acc]);
%%							eof ->
%%								lists:flatten(lists:reverse(Acc));
%%							{error, Err} ->
%%								Err
%%						end
%%					end,
%%					Data = Read(Read, []),
%%					file:close(Io),
%%					{reply, {200, [], Data}, State}
%%			end;
%		["nodes"] ->
%			Nodes = lists:sort([node() | nodes()]),
%			F = fun(I) ->
%				list_to_binary(atom_to_list(I))
%			end,
%			{reply, {200, [], mochijson2:encode({struct, [{success, true}, {<<"nodes">>, lists:map(F, Nodes)}]})}, State};
%		["peek", Queue, Callid] ->
%			{UnencodedJson, Newstate} = case agent:dump_state(State#state.agent_fsm) of
%				#agent{state = released} ->
%					case queue_manager:get_queue(Queue) of
%						Qpid when is_pid(Qpid) ->
%							case call_queue:get_call(Qpid, Callid) of
%								none ->
%									{{struct, [{success, false}, {<<"message">>, <<"Call not queued">>}]}, State};
%								{_Key, #queued_call{media = Mpid}} ->
%									case gen_media:call(Mpid, {peek, State#state.agent_fsm}) of
%										ok ->
%											{{struct, [{success, true}]}, State#state{current_call = expect}};
%										_ ->
%											{{struct, [{success, false}, {<<"message">>, <<"media didn't peek">>}]}, State}
%									end
%							end;
%						_ ->
%							{{struct, [{success, false}, {<<"message">>, <<"Queue doesn't exist">>}]}, State}
%					end;
%				_ ->
%					{{struct, [{success, false}, {<<"message">>, <<"Can only peek while released">>}]}, State}
%			end,
%			{reply, {200, [], mochijson2:encode(UnencodedJson)}, Newstate};
%		["drop_call", Queue, Callid] ->
%			Json = case queue_manager:get_queue(Queue) of
%				undefined ->
%					{struct, [{success, false}, {<<"message">>, <<"queue not found">>}]};
%				Qpid when is_pid(Qpid) ->
%					case call_queue:get_call(Qpid, Callid) of
%						none ->
%							{struct, [{success, false}, {<<"message">>, <<"call not found">>}]};
%						{_, #queued_call{media = _Mpid}} ->
%							%%gen_media:cast(Mpid, email_drop), % only email should respond to this
%							% TODO finish this when hangup can take a reason.
%							{struct, [{success, false}, {<<"message">>, <<"nyi">>}]}
%					end
%			end,
%			{reply, {200, [], mochijson2:encode(Json)}, State};
%		["getmotd"] ->
%			Motd = case cpx_supervisor:get_value(motd) of
%				none ->
%					false;
%				{ok, Text} ->
%					list_to_binary(Text)
%			end,
%			{reply, {200, [], mochijson2:encode({struct, [{success, true}, {motd, Motd}]})}, State};
%		[Node | Do] ->
%			Nodes = get_nodes(Node),
%			{Success, Result} = do_action(Nodes, Do, []),
%			{reply, {200, [], mochijson2:encode({struct, [{success, Success}, {<<"result">>, Result}]})}, State}
%	end;
%handle_call({supervisor, _Request}, _From, State) ->
%	?NOTICE("Unauthorized access to a supervisor web call", []),
%	{reply, {403, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"insufficient privledges">>}]})}, State};
