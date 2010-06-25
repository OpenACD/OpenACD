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
%%	Micah Warren <micahw at fusedsolutions dot com>
%%

%% @doc A module designed to help load test OpenACD paired with freeswitch.
%% This is expected to be run on a different (but not isolated) node
%% from OpenACD, paired with it's own freeswitch instance.  Obviously,
%% separate hardware is preffered.
%%
%% Starts a given number of dummy agents, and then creates a config for 
%% freeswitch with attempts to do a sip registration to OpenACD's freeswitch.

-module(sip_bot_manager).
-author("Micah").

-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").

-define(TIMEOUT, 10000).
-define(EMPTYRESPONSE, "<document type=\"freeswitch/xml\"></document>").
-define(NOTFOUNDRESPONSE,
"<document type=\"freeswitch/xml\">
	<section name=\"result\">
		<result status=\"not found\" />
	</section>
</document>").

-define(DIALUSERRESPONSE,
"<document type=\"freeswitch/xml\">
	<section name=\"directory\">
		<domain name=\"~s\">
			<user id=\"~s\">
				<params>
					<param name=\"dial-string\" value=\"~s\"/>
				</params>
			</user>
		</domain>
	</section>
</document>").

-define(REGISTERRESPONSE,
"<document type=\"freeswitch/xml\">
	<section name=\"directory\">
		<domain name=\"~s\">
			<user id=\"~s\">
				<params>
					<param name=\"a1-hash\" value=\"~s\"/>
				</params>
			</user>
		</domain>
	</section>
</document>").

-define(USERRESPONSE,
"<document type=\"freeswitch/xml\">
	<section name=\"directory\">
		<domain name=\"~s\">
			<user id=\"~s\">
			</user>
		</domain>
	</section>
</document>").

%% API
-export([
	start/1,
	start/2,
	start_link/1,
	start_link/2,
	stop/0,
	configuration_server/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-type(acd_node() :: {'acd_node', atom()}).
-type(username() :: string()).
-type(password() :: string()).
-type(agent_tuple() :: {username(), password()}).
-type(agents() :: {'agents', [agent_tuple()]}).
-type(gateway() :: {'gateway', string()}).
-type(realm() :: {'realm', string()}).
-type(start_option() :: 
	gateway() |
	agents() |
	acd_node() |
	realm()
).
-type(start_options() :: [start_option()]).

-record(state, {
	nodename :: atom(),
	acd_node :: atom(),
	freeswitch_up = false :: boolean(),
	eventserver :: pid() | 'undefined',
	xmlserver :: pid() | 'undefined',
	xmlserver_opts = [] :: start_options(),
	bot_dict = dict:new() :: dict()
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================

-spec(start/1 :: (Node :: atom()) -> {'ok', pid()}).
start(Node) ->
	start(Node, []).

-spec(start/2 :: (Node :: atom(), Opts :: start_options() | {'file', string()}) -> {'ok', pid()}).
start(Node, {file, File}) ->
	{ok, Opts} = file:consult(File),
	start(Node, Opts);
start(Node, Opts) ->
	gen_server:start({local, ?MODULE}, ?MODULE, [Node, Opts], []).

-spec(start_link/1 :: (Node :: atom()) -> {'ok', pid()}).
start_link(Node) ->
	start_link(Node, []).

-spec(start_link/2 :: (Node :: atom(), Opts :: start_options() | {'file', string()}) -> {'ok', pid()}).
start_link(Node, {file, File}) ->
	{ok, Opts} = file:consult(File),
	start_link(Node, Opts);
start_link(Node, Opts) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Node, Opts], []).

-spec(stop/0 :: () -> 'ok').
stop() ->
	gen_server:cast(?MODULE, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%% @private
init([Nodename, Options]) -> 
	?DEBUG("starting...", []),
	Acd = proplists:get_value(acd_node, Options),
	Agents = proplists:get_value(agents, Options, []),
	process_flag(trap_exit, true),
	monitor_node(Nodename, true),
	spawn(fun() -> launch_agents(Agents, Acd) end),
	{Listenpid, DomainPid} = case net_adm:ping(Nodename) of
		pong ->
			?NOTICE("Starting with live freeswitch node", []),
			Lpid = start_listener(Nodename),
			%freeswitch:event(Nodename, ['CHANNEL_DESTROY']),
			{ok, Pid} = freeswitch:start_fetch_handler(Nodename, configuration, ?MODULE, configuration_server, Options),
			link(Pid),
			{Lpid, Pid};
		_ ->
			?NOTICE("Starting with dead freeswitch node", []),
			{undefined, undefined}
	end,
	{ok, #state{nodename=Nodename, acd_node = Acd, eventserver = Listenpid, xmlserver = DomainPid, xmlserver_opts = Options, freeswitch_up = true}}.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private

handle_call(Request, _From, State) ->
	?INFO("Unexpected call:  ~p", [Request]),
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------

%% @private
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info({'EXIT', Pid, Reason}, #state{eventserver = Pid, nodename = Nodename, freeswitch_up = true} = State) ->
	?NOTICE("listener pid exited unexpectedly: ~p", [Reason]),
	Lpid = start_listener(Nodename),
	%freeswitch:event(Nodename, ['CHANNEL_DESTROY']),
	{noreply, State#state{eventserver = Lpid}};
handle_info({'EXIT', Pid, Reason}, #state{xmlserver = Pid, xmlserver_opts = Opts, nodename = Nodename, freeswitch_up = true} = State) ->
	?NOTICE("XML pid exited unexpectedly: ~p", [Reason]),
	{ok, NPid} = freeswitch:start_fetch_handler(Nodename, configuration, ?MODULE, configuration_server, Opts),
	link(NPid),
	{noreply, State#state{xmlserver = NPid}};
handle_info({'EXIT', Pid, _Reason}, #state{eventserver = Pid} = State) ->
	{noreply, State#state{eventserver = undefined}};
handle_info({'EXIT', Pid, _Reason}, #state{xmlserver = Pid} = State) ->
	{noreply, State#state{xmlserver = undefined}};
handle_info({nodedown, Nodename}, #state{nodename = Nodename, xmlserver = Pid, eventserver = Lpid} = State) ->
	?WARNING("Freeswitch node ~p has gone down", [Nodename]),
	case is_pid(Pid) of
		true -> exit(Pid, kill);
		_ -> ok
	end,
	case is_pid(Lpid) of
		true -> exit(Lpid, kill);
		_ -> ok
	end,
	timer:send_after(1000, freeswitch_ping),
	{noreply, State#state{freeswitch_up = false}};
handle_info(freeswitch_ping, #state{nodename = Nodename} = State) ->
	case net_adm:ping(Nodename) of
		pong ->
			?NOTICE("Freeswitch node ~p is back up", [Nodename]),
			monitor_node(Nodename, true),
			Lpid = start_listener(Nodename),
			{ok, Pid} = freeswitch:start_fetch_handler(Nodename, configuration, ?MODULE, configuration_server, State#state.xmlserver_opts),
			link(Pid),
			%freeswitch:event(Nodename, ['CHANNEL_DESTROY']),
			{noreply, State#state{eventserver = Lpid, xmlserver = Pid, freeswitch_up = true}};
		pang ->
			timer:send_after(1000, freeswitch_ping),
			{noreply, State}
	end;
handle_info({get_pid, UUID, Ref, From}, #state{bot_dict = Dict} = State) ->
	Gotpid = case dict:find(UUID, Dict) of
		{ok, Pid} ->
			?NOTICE("bot set up for ~s already (~p)", [UUID, Pid]),
			Pid;
		error ->
			{ok, Pid} = sip_bot:start_link(State#state.nodename, [{uuid, UUID}]),
			Pid
	end,
	From ! {Ref, Gotpid},
	{noreply, State#state{bot_dict = dict:store(UUID, Gotpid, Dict)}};
handle_info(Info, State) ->
	?DEBUG("Unexpected info:  ~p", [Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @private
terminate(Reason, #state{eventserver = Epid, xmlserver = Xpid} = State) ->
	?NOTICE("Terminating for ~p", [Reason]),
	exit(Epid, Reason),
	exit(Xpid, Reason),
	spawn(fun() -> kill_agents(proplists:get_value(agents, State#state.xmlserver_opts, []), State#state.acd_node) end),
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

launch_agents([], _Acd) ->
	ok;
launch_agents([{User, Pass} | Tail], Acd) ->
	rpc:call(Acd, agent_dummy_connection, start, [[{login, User}, {password, Pass}, {scale, 10000}]]),
	launch_agents(Tail, Acd).

kill_agents([], _Acd) ->
	ok;
kill_agents([{User, _Pass} | Tail], Acd) ->
	case rpc:call(Acd, agent_manager, query_agent, [User]) of
		{true, Pid} ->
			exit(Pid, kill);
		_ ->
			?DEBUG("Not murdering ~s", [User]),
			ok
	end,
	kill_agents(Tail, Acd).

%% @private
start_listener(Nodename) ->
	Self = self(),
	spawn_link(fun() ->
		{freeswitchnode, Nodename} ! register_event_handler,
			receive
				ok ->
					Self ! {register_event_handler, {ok, self()}},
					listener(Nodename);
				{error, Reason} -> 
					Self ! {register_event_handler, {error, Reason}}
			after ?TIMEOUT -> 
					Self ! {register_event_handler, timeout}
			end
	end).

%% @private
% listens for info from the freeswitch c node.
listener(Node) ->
	receive
		{event, [UUID | Event]} ->
			?DEBUG("recieved event '~p' from c node.", [UUID]),
%			Ename = proplists:get_value("Event-Name", Event),
%			case Ename of
%				"CHANNEL_DESTROY" ->
%					gen_server:cast(?MODULE, {channel_destroy, UUID});
%				_ ->
%					ok
%			end,
			?MODULE:listener(Node);
		{nodedown, Node} -> 
			gen_server:cast(?MODULE, nodedown);
		 Otherwise -> 
			 ?INFO("Uncertain reply received by the fmm listener:  ~p", [Otherwise]),
			 ?MODULE:listener(Node)
	end.

make_gateway_conf(Proxy, User, Pass, Realm) when is_list(Proxy), is_list(User), is_list(Pass), is_list(Realm) ->
	make_gateway_conf(Proxy, list_to_binary([Proxy, "-", User]), list_to_binary(User), list_to_binary(Pass), list_to_binary(Realm)).
	
make_gateway_conf(Proxy, Proxyname, User, Pass, Realm) ->
	?DEBUG("making gw conf:  ~p", [{Proxy, Proxyname, User, Pass, Realm}]),
	{<<"gateway">>, [
		{<<"name">>, Proxyname}
	], [
		{<<"param">>, [
			{<<"name">>, <<"username">>},
			{<<"value">>, User}
		]},
		{<<"param">>, [
			{<<"name">>, <<"auth-username">>},
			{<<"value">>, User}
		]},
		{<<"param">>, [
			{<<"name">>, <<"realm">>},
			{<<"value">>, Realm}
		]},
		{<<"param">>, [
			{<<"name">>, <<"password">>},
			{<<"value">>, Pass}
		]},
		{<<"param">>, [
			{<<"name">>, <<"proxy">>}, 
			{<<"value">>, Proxy}
		]},
		{<<"param">>, [
			{<<"name">>, <<"register">>},
			{<<"value">>, true}
		]}
	]}.

-spec(configuration_server/2 :: (Node :: atom(), State :: start_options()) -> 'ok').
configuration_server(Node, State) ->
	receive
		{fetch, configuration, "configuration", "name", "sofia.conf", ID, _Params} ->
			?DEBUG("sofia conf (~s)", [ID]),
			Proxy = proplists:get_value(gateway, State, ""),
			Agents = proplists:get_value(agents, State, []),
			Realm = proplists:get_value(realm, State, Proxy),
			Gateways = [make_gateway_conf(Proxy, User, Pass, Realm) || {User, Pass} <- Agents],
			Out = {<<"document">>, [
				{<<"type">>, <<"freeswitch/xml">>}
			], [
				{<<"section">>, [
					{<<"name">>, <<"configuration">>}
				], [
					{<<"configuration">>, [
						{<<"name">>, <<"sofia.conf">>},
						{<<"description">>, <<"sip bots conf">>}
					], [
						{<<"profiles">>, [], [
							{<<"profile">>, [
								{<<"name">>, <<"default">>},
								{<<"domain">>, <<"localhost">>}
							], [
								{<<"settings">>, [], [
									{<<"param">>, [
										{<<"name">>, <<"stun-enabled">>},
										{<<"value">>, false}
									]},
									{<<"param">>, [
										{<<"name">>, <<"use-rtp-timer">>},
										{<<"value">>, true}
									]},
									{<<"param">>, [
										{<<"name">>, <<"rtp-timer-name">>},
										{<<"value">>, <<"soft">>}
									]}
								]},
								{<<"gateways">>, [], Gateways}
							]}
						]}
					]}
				]}
			]},
			Binout = list_to_binary(mochiweb_html:to_html(Out)),
			%?DEBUG("Binout:  ~s", [Binout]),
			freeswitch:send(Node, {fetch_reply, ID, binary_to_list(Binout)}),
			?MODULE:configuration_server(Node, State);
		{reconfig, Newstate} ->
			?MODULE:configuration_server(Node, Newstate);
		{fetch, _Section, _Something, _Key, _Value, ID, _Params} = T->
			Out = [element(I, T) || I <- lists:seq(2, 6)],
			?DEBUG("Something:  ~p", [Out]),
			freeswitch:send(Node, {fetch_reply, ID, ?EMPTYRESPONSE}),
			?MODULE:configuration_server(Node, State);
		{nodedown, Node} ->
			?DEBUG("Node we were serving XML search requests to exited", []),
			ok;
		Other ->
			?DEBUG("got other response: ~p", [Other]),
			?MODULE:configuration_server(Node, State)
	end.