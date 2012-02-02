%% "The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%%
%%	The Original Code is OpenACD.
%%
%%	The Initial Developers of the Original Code is 
%%	Andrew Thompson.
%%
%%	All portions of the code written by the Initial Developers are Copyright
%%	(c) 2008-2010 Andrew Thompson.
%%	All Rights Reserved.
%%
%%	Contributor(s):
%%
%%	Andrew Thompson <andrew at hijacked dot us>

%% @doc A simple outbound dialer, this is NOT a predictive dialer.

-module(freeswitch_dialer).

-behaviour(gen_server).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

%% API

-export([
		start/6,
		start_fg/6
	]).

-include_lib("OpenACD/include/log.hrl").
-include_lib("OpenACD/include/call.hrl").
-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
	-define(URL_POPS, [
		{"test 1", "http://www.example.com"}, 
		{"test 2", "http://subdomain.example.com"}
	]).
	-include("agent.hrl").
-else.
	-define(URL_POPS, [
		{"test 1", "http://subdomain.example.com?ivroption=#{ivroption}&callid=#{callid}"},
		{"test 2", "http://subdomain2.example.com?number=#{number}&clientid=#{clientid}"}
	]).
	-type(skill_list() :: [atom() | {atom(), any()}]).
-endif.

-record(state, {
		cnode :: atom(),
		number :: string(),
		exten :: string(),
		skills :: skill_list(),
		client :: #client{},
		vars :: [any()],
		uuid :: string(),
		agent :: pid(),
		call :: #call{},
		fg_pid :: pid()
	}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include_lib("OpenACD/include/gen_spec.hrl").

-spec(start/6 :: (Node :: atom(), Number :: string(), Exten :: string(), Skills :: skill_list(), Client :: string(), Vars :: [any()]) -> {'ok', pid()}).
start(Node, Number, Exten, Skills, Client, Vars) ->
	gen_server:start(?MODULE, [Node, Number, Exten, Skills, Client, Vars, false], []).

-spec(start_fg/6 :: (Node :: atom(), Number :: string(), Exten :: string(), Skills :: skill_list(), Client :: string(), Vars :: [any()]) -> {'ok', pid()}).
start_fg(Node, Number, Exten, Skills, Client, Vars) ->
	case gen_server:start(?MODULE, [Node, Number, Exten, Skills, Client, Vars, self()], []) of
		{ok, _Pid} ->
			receive
				{dialer_result, Result} ->
					Result
			after 30000 ->
					{error, timeout}
			end;
		Result ->
			Result
	end.

init([Node, Number, Exten, Skills, Client, Vars, Foreground]) ->
	case call_queue_config:get_client(id, Client) of
			none ->
				{stop, bad_client};
			ClientRec ->
				case freeswitch:api(Node, create_uuid) of
					{ok, UUID} ->
						State = #state{cnode = Node, number = Number, exten = Exten, skills = Skills, client = ClientRec, vars = Vars, uuid = UUID},
						case Foreground of
							false ->
								erlang:send_after(1000, self(), reserve_agent), % defer doing this to after we return control
								{ok, State};
							Pid ->
								reserve_agent(State#state{fg_pid = Pid})
						end;
					Else ->
						?WARNING("create_uuid returned ~p", [Else]),
						{stop, bad_uuid}
				end
	end.

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(reserve_agent, State) ->
	case reserve_agent(State) of
		{stop, no_agents} ->
			erlang:send_after(5000, self(), reserve_agent),
			{noreply, State};
		{ok, State} ->
			{noreply, State};
		{stop, Reason} ->
			{exit, Reason, State}
	end;
handle_info({call_event, {event, [UUID | Rest]}}, #state{cnode = Node, uuid = UUID} = State) ->
	Event = proplists:get_value("Event-Name", Rest),
	case Event of
		"CHANNEL_PARK" ->
			AgentRec = agent:dump_state(State#state.agent),
			?INFO("call got parked, send them to an agent at ~p", [freeswitch_media_manager:get_agent_dial_string(AgentRec, [])]),
			freeswitch:sendmsg(Node, UUID,
				[{"call-command", "execute"},
					{"execute-app-name", "bridge"},
					{"execute-app-arg",
						freeswitch_media_manager:get_agent_dial_string(AgentRec, [])}]),
			%agent:set_state(State#state.agent, ringing, State#state.call);
			{noreply, State};
		"CHANNEL_BRIDGE" ->
			%Hey, we bridged to an agent
			agent:set_state(State#state.agent, outgoing, State#state.call),
			Client = State#state.client,
			url_pop(State#state.agent, [{"callid", State#state.uuid}, {"number", State#state.number}, {"clientid", Client#client.id}, {"ivroption", proplists:get_value("variable_ivropt", Rest, "")}]), % the 2rd parameter is the interpolatable variables.
			{noreply, State};
		"CHANNEL_ANSWER" when is_pid(State#state.fg_pid) ->
			State#state.fg_pid ! {dialer_result, {ok, self()}},
			{noreply, State#state{fg_pid = false}};
		"CHANNEL_HANGUP" when is_pid(State#state.fg_pid) ->
			State#state.fg_pid ! {dialer_result, {error, no_answer}},
			{noreply, State#state{fg_pid = false}};
		_ ->
			?INFO("Got ~p", [Event]),
			{noreply, State}
	end;
handle_info(call_hangup, State) ->
	?DEBUG("Call hangup info", []),
	% channel hungup, set the agent back to a sane state
	agent:set_state(State#state.agent, idle),
	agent:set_state(State#state.agent, wrapup, State#state.call),
	{stop, normal, State};
handle_info(_Info, State) ->
	%?NOTICE("Got message: ~p", [Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_Oldvsn, State, _Extra) ->
	{ok, State}.

grab_agent([], _Call) ->
	none;
grab_agent([{_, {Pid, _ID, _Skills} = Agent} | Agents], Call) ->
	case agent:set_state(Pid, precall, Call) of
		ok ->
			{ok, Agent};
		_ ->
			grab_agent(Agents, Call)
	end.
	

reserve_agent(State) ->
	Call = #call{id = State#state.uuid, source = self(), client = State#state.client, skills = State#state.skills, direction = outbound},
	Agents = agent_manager:find_avail_agents_by_skill(State#state.skills),
	case grab_agent(Agents, Call) of
		none ->
			{stop, no_agents};
		{ok, {AgentPid, _ID, _Skills} = Agent} ->
			% so, we've reserved an agent, time to do make an outvound call
			% sadly freeswitch_ring isn't generic enough to be useful here, so we have to do it ourselves for now
			Client = State#state.client,
			CalleridArgs = case proplists:get_value(<<"callerid">>, Client#client.options) of
				undefined ->
					["origination_privacy=hide_namehide_number"];
				CalleridNum ->
					["origination_caller_id_name='"++Client#client.label++"'", "origination_caller_id_number='"++binary_to_list(CalleridNum)++"'"]
			end,

			DialVars = lists:append(CalleridArgs, State#state.vars),

			UUID = State#state.uuid,
			Dialstring = freeswitch_media_manager:do_dial_string(freeswitch_media_manager:get_default_dial_string(), State#state.number, ["origination_uuid="++UUID, "hangup_after_bridge=true" | DialVars]),
			?INFO("Dialstring: ~p", [Dialstring]),
			case freeswitch:bgapi(State#state.cnode, originate, Dialstring ++ " "++State#state.exten) of
				{ok, _JobID} ->
					Gethandle = fun(Recusef, Count) ->
						?DEBUG("Counted ~p", [Count]),
						case freeswitch:handlecall(State#state.cnode, UUID) of
							{error, badsession} when Count > 10 ->
								{error, badsession};
							{error, badsession} ->
								timer:sleep(100),
								Recusef(Recusef, Count+1);
							{error, Other} ->
								{error, Other};
							Else ->
								Else
						end
					end,
					case Gethandle(Gethandle, 0) of
						{error, badsession} ->
							?ERROR("bad uuid ~p when calling ~p", [UUID, State#state.number]),
							agent:set_state(AgentPid, idle),
							{stop, bad_uuid};
						{error, Other} ->
							?ERROR("other error starting; ~p for ~p", [Other, State#state.number]),
							agent:set_state(AgentPid, idle),
							{stop, unknown_error};
						_Else ->
							?DEBUG("starting for ~p", [UUID]),
							{ok, State#state{agent = AgentPid, call = Call}}
					end;
				Else ->
					?ERROR("originate failed with ~p  when calling ~p", [Else, State#state.number]),
					agent:set_state( AgentPid, idle),
					{stop, originate_failed}
			end
	end.

url_pop(Agent, Args) ->
	url_pop(Agent, Args, ?URL_POPS).

url_pop(_Agent, _Args, []) ->
	ok;
url_pop(Agent, Args, [{Window, Url} | Tail]) ->
	TrueUrl = util:string_interpolate(Url, Args),
	agent:url_pop(Agent, TrueUrl, Window),
	url_pop(Agent, Args, Tail).

-ifdef(TEST).

url_pop_test() ->
	{ok, Aconn} = gen_server_mock:new(),
	{ok, Apid} = agent:start(#agent{login = "agent", connection = Aconn}),
	gen_server_mock:expect_cast(Aconn, fun({url_pop,"http://www.example.com","test 1"}, _) -> ok end),
	gen_server_mock:expect_cast(Aconn, fun({url_pop,"http://subdomain.example.com","test 2"}, _) -> ok end),
	url_pop(Apid, []),
	gen_server_mock:assert_expectations(Aconn).
	
-endif.
