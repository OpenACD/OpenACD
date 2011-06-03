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

%% API
-export([
%	api/2,
%	encode_statedata/1,
%	format_status/2,
	start/1,
	start_link/1,
	nuke_poll_queue/1,
	is_web_api/2
]).

%% Web api exports.
-export([
	get_profiles/1,
%	spy/2,
	agent_state/3,
	agent_state/4,
	set_profile/3,
	kick_agent/2,
	blab/4,
	status/1,
	unsubscribe/1,
	poll/2,
	get_motd/1,
	set_motd/3,
	remove_problem_recording/2,
	start_problem_recording/2,
	agent_ring/4,
%	get_avail_agents/1,
%	peek/3,
	drop_media/3,
	voicemail/3
]).

-web_api_functions([
	{get_profiles, 1},
%	{spy, 2},
	{agent_state, 3},
	{agent_state, 4},
	{set_profile, 3},
	{kick_agent, 2},
	{blab, 4},
	{status, 1},
	{unsubscribe, 1},
	{poll, 2},
	{get_motd, 1},
	{set_motd, 3},
	{remove_problem_recording, 2},
	{start_problem_recording, 2},
	{agent_ring, 4},
%	{get_avail_agents, 1},
%	{peek, 3},
	{drop_media, 3},
	{voicemail, 3}
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-type(tref() :: any()).

-record(state, {
	login :: string(),
	endpointtype :: 'sip' | 'sip_registration' | 'pstn' | 'h323' | 'iax2',
	endpointdata :: any(),
	ring_path = inband :: 'inband' | 'outband',
	poll_queue = [] :: [{struct, [{binary(), any()}]}],
		% list of json structs to be sent to the client on poll.
	poll_flush_timer :: any(),
	poll_pid :: 'undefined' | 'suppress' | pid(),
	poll_pid_established = 1 :: pos_integer(),
	ack_timer :: tref() | 'undefined',
	securitylevel = agent :: 'agent' | 'supervisor' | 'admin',
	listener :: 'undefined' | pid()
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

%% @doc {@web} Get the message of the day (a blab automatically told to
%% agents on login).
-spec(get_motd/1 :: (Conn :: pid()) -> any()).
get_motd(Conn) ->
	gen_server:call(Conn, {supervisor, get_motd}).

%% @doc {@web} Set the message of the day (a blab automatically told to
%% agents on login).  If the message passed is an empty string, the motd
%% is cleared.  If `Node' is `<<"system">>' all nodes get the same motd.
-spec(set_motd/3 :: (Conn :: pid(), Message :: string(), Node :: string()) -> any()).
set_motd(Conn, Message, Node) ->
	gen_server:call(Conn, {supervisor, {set_motd, Message, Node}}).

%% @doc {@web} Start recording a message to be used for problem
%% Announcements on voice calls in queue.  Starts by trying to call the
%% agent/supervisor making the recording request.
-spec(start_problem_recording/2 :: (Conn :: pid(), Clientid :: string()) -> any()).
start_problem_recording(Conn, Clientid) ->
	gen_server:call(Conn, {supervisor, {start_problem_recording, Clientid}}).

%% @doc {@web} Removes a previously made recording for a given client.
-spec(remove_problem_recording/2 :: (Conn :: pid(), Clientid :: string()) -> any()).
remove_problem_recording(Conn, Clientid) ->
	gen_server:call(Conn, {supervisor, {remove_problem_recording, Clientid}}).

%% @doc {@web} Send a blab to an agent or group of agents.
-spec(blab/4 :: (Conn :: pid(), Message :: string(), Grouping :: string(), Target :: string()) -> any()).
blab(Conn, Message, Grouping, Target) ->
	gen_server:call(Conn, {supervisor, {blab, Message, Grouping, Target}}).

%% @doc {@web} Send a media to voicemail (if possible).
-spec(voicemail/3 :: (Conn :: pid(), Queue :: string(), MediaId :: string()) -> any()).
voicemail(Conn, Queue, MediaId) ->
	gen_server:call(Conn, {supervisor, {voicemail, Queue, MediaId}}).

%% @doc {@web} Force an agent to logout.  Does not force an agent currently
%% oncall to logout.  Lookup is done by login name.
-spec(kick_agent/2 :: (Conn :: pid(), Agent :: string()) -> any()).
kick_agent(Conn, Agent) ->
	gen_server:call(Conn, {supervisor, {kick_agent, Agent}}).

%% @doc {@web} Listen in or view media the specified agent is on call with.
%% You cannot spy on yourself.  Agent lookup is by login name.  If you are
%% logged in as an agent (rather than just supervisor mode), you must be
%% released.
-spec(spy/2 :: (Conn :: pid(), Agent :: string()) -> any()).
spy(Conn, Agent) ->
	gen_server:call(Conn, {supervisor, {spy, Agent}}).

%% @doc {@web} Send a call in queue to a specified agent.
-spec(agent_ring/4 :: (Conn :: pid(), Queue :: string(), MediaId :: string(), Agent :: string()) -> any()).
agent_ring(Conn, Queue, MediaId, Agent) ->
	gen_server:call(Conn, {supervisor, {agent_ring, Queue, MediaId, Agent}}).

%% @doc {@web} <b>Not yet fully implemented.</b>  Terminate the media
%% while it is in queue.
-spec(drop_media/3 :: (Conn :: pid(), Queue :: string(), MediaId :: string()) -> any()).
drop_media(Conn, Queue, MediaId) ->
	gen_server:call(Conn, {supervisor, {drop_media, Queue, MediaId}}).

%% @doc {@web} Change the agent's state.
-spec(agent_state/3 :: (Conn :: pid(), Agent :: string(), State :: string()) -> any()).
agent_state(Conn, Agent, State) ->
	agent_state(Conn, Agent, State, undefined). 

%% @doc {@web} Change the agent's state and state data.  Note when setting
%% an agent released from oncall or wrapup, the do not go released 
%% immediately.  They are flagged to go released instead of idle once the
%% call is completed.
-spec(agent_state/4 :: (Conn :: pid(), Agent :: string(), State :: string(), Statedata :: string()) -> any()).
agent_state(Conn, Agent, State, Statedata) ->
	gen_server:call(Conn, {supervisor, {agent_state, Agent, State, Statedata}}).

%% @doc {@web} Change the agent's profile for the duration of that agent's
%% session.
-spec(set_profile/3 :: (Conn :: pid(), Agent :: string(), NewProf :: string()) -> any()).
set_profile(Conn, Agent, NewProf) ->
	set_profile(Conn, Agent, NewProf, <<"false">>).

%% @doc {@web} Change the agent's profile.  If `Permanent' is `<<"true">>',
%% the agent will be moved to that profile for each subsequent login.
-spec(set_profile/4 :: (Conn :: pid(), Agent :: string(), NewProf :: string(), Permanent :: string()) -> any()).
set_profile(Conn, Agent, NewProf, Permanent) ->
	gen_server:call(Conn, {supervisor, {set_profile, Agent, NewProf, Permanent}}).

%%====================================================================
%% API
%%====================================================================

-type(login_opt() :: {login, string()}).
-type(endpoint_opt() :: {endpoint, 'sip' | 'sip_registration' | 'pstn' | 'h323' | 'iax2'}).
-type(endpointdata_opt() :: {endpointdata, any()}).
-type(ring_path_opt() :: {ring_path, 'inband' | 'outband'}).
-type(suppress_poll_opt() :: 'suppress_poll').
-type(start_opt() :: 
	login_opt() | 
	endpoint_opt() | 
	endpointdata_opt() | 
	ring_path_opt() |
	suppress_poll_opt()
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

%% @doc {@web} See {@link agent_web_connection:poll/2}.  Basically the
%% same thing.  Supervisors get 2 extra commands:
%% <table>
%% 	<tr>
%% 		<th>Command</th>
%% 		<th>Other Properties</th>
%% 		<th>Description</th>
%% 	</tr>
%% 	<tr>
%% 		<td>supervisorSet</td>
%% 		<td><ul>
%% 			<li>"id":  string()</li>
%% 			<li>"type":  "agent" | "queue" | "media"</li>
%% 			<li>"name":  string()</li>
%% 			<li>"display":  string()</li>
%% 			<li>"details":  Object()</li>
%% 		</ul></td>
%% 		<td>A media, agent, or queue has been changed or updated.</td>
%% 	</tr>
%% 	<tr>
%% 		<td>supervisorDrop</td>
%% 		<td><ul>
%% 			<li>"type":  "agent" | "queue" | "media"</li>
%% 			<li>"id":  string()</li>
%% 			<li>"name":  string()</li>
%% 		</ul></td>
%% 		<td>A media, agent, or queue has stopped/ended it's life cycle.  For
%% 		example, an agent may have logged out, or a media had it's wrapup
%% 		completed.  In the case of queue, usually means it's removed from the
%% 		system.</td>
-spec(poll/2 :: (Pid :: pid(), Frompid :: pid()) -> 'ok').
poll(Pid, Frompid) ->
	gen_server:cast(Pid, {poll, Frompid}).

%% @doc Extract the poll queue from a state record and 0 it out.  Used 
%% when the supervisor is logged in as an agent rather than independent of 
%% routing.  The list is returned in the order the events were recieved.
-spec(nuke_poll_queue/1 :: (State :: #state{}) -> {[any()], #state{}}).
nuke_poll_queue(State) ->
	List = lists:reverse(State#state.poll_queue),
	NewState = State#state{poll_queue = []},
	{List, NewState}.

%%====================================================================
%% Init
%%====================================================================

init(Opts) ->
	SuppressPoll = case proplists:get_value(suppress_poll, Opts) of
		true -> suppress;
		Undef -> Undef
	end,
	State = #state{
		login = proplists:get_value(login, Opts),
		endpointtype = proplists:get_value(endpointtype, Opts),
		endpointdata = proplists:get_value(endpointdata, Opts),
		ring_path = proplists:get_value(ring_path, Opts, inband),
		poll_pid = SuppressPoll
	},
	{ok, State}.

%%====================================================================
%% handle_call
%%====================================================================

handle_call({supervisor, {set_profile, Agent, InNewProf, Permanent}}, _From, State) ->
	Login = binary_to_list(Agent),
	Newprof = binary_to_list(InNewProf),
	Midgood = case agent_manager:query_agent(Login) of
		{true, Apid} ->
			agent:change_profile(Apid, Newprof);
		false ->
			{error, noagent}
	end,
	case {Midgood, Permanent} of
		{{error, Err}, _} ->
			{Errcode, Msg} = case Err of
				noagent ->
					{<<"AGENT_NOEXISTS">>, <<"unknown agent">>};
				unknown_profile ->
					{<<"PROFILE_NOEXISTS">>, <<"unknown profile">>}
			end,
			{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, Msg}, {<<"errcode">>, Errcode}]})}, State};
		{ok, <<"true">>} ->
			case agent_auth:get_agent(login, Login) of
				{atomic, [Arec]} ->
					case agent_auth:set_agent(Arec#agent_auth.id, Login, Arec#agent_auth.skills, Arec#agent_auth.securitylevel, Newprof) of
						{atomic, ok} ->
							{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State};
						{aborted, Err} ->
							Msg = list_to_binary(io_lib:format("Profile changed, but not permanent:  ~p", [Err])),
							{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"errcode">>, <<"UNKNOWN_ERROR">>}, {<<"message">>, Msg}]})}, State}
					end;
				{atomic, [_A, _B | _]} ->
					{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Multiple agent records found, not making a change">>}, {<<"errcode">>, <<"MULTIPLE_AGENTS">>}]})}, State};
				{atomic, []} ->
					{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Agent is not permanent, so not permanent change made">>}, {<<"errcode">>, <<"AGENT_NOEXISTS">>}]})}, State}
			end;
		{ok, _} ->
			{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State}
	end;
handle_call({supervisor, {agent_state, Agent, InStateName, Statedata}}, _From, State) ->
	StateName = binary_to_list(InStateName),
	Json = case agent_manager:query_agent(binary_to_list(Agent)) of
		{true, Apid} ->
			Statechange = case {StateName, Statedata} of
				{"released", <<"default">>} ->
					agent:set_state(Apid, released, default);
				{StateName, undefined} ->
					Astate = agent:list_to_state(StateName),
					agent:set_state(Apid, Astate);
				{StateName, Statedata} ->
					Astate = agent:list_to_state(StateName),
					agent:set_state(Apid, Astate, binary_to_list(Statedata))
			end,
			case Statechange of
				invalid ->
					mochijson2:encode({struct, [{success, false}, {<<"errcode">>, <<"INVALID_STATE_CHANGE">>}, {<<"message">>, <<"invalid state change">>}]});
				ok ->
					mochijson2:encode({struct, [{success, true}]});
				queued ->
					mochijson2:encode({struct, [{success, true}]})
			end;
		_Else ->
			mochijson2:encode({struct, [{success, false}, {<<"errcode">>, <<"AGENT_NOEXISTS">>}, {<<"message">>, <<"Agent not found">>}]})
	end,
	{reply, {200, [], Json}, State};
handle_call({supervisor, {drop_media, Queue, MediaId}}, _From, State) ->
	Json = case queue_manager:get_queue(Queue) of
		undefined ->
			{struct, [{success, false}, {<<"errcode">>, <<"QUEUE_NOEXISTS">>}, {<<"message">>, <<"queue not found">>}]};
		Qpid when is_pid(Qpid) ->
			case call_queue:get_call(Qpid, MediaId) of
				none ->
					{struct, [{success, false}, {<<"errcode">>, <<"MEDIA_NOEXISTS">>}, {<<"message">>, <<"call not found">>}]};
				{_, #queued_call{media = _Mpid}} ->
					%%gen_media:cast(Mpid, email_drop), % only email should respond to this
					% TODO finish this when hangup can take a reason.
					{struct, [{success, false}, {<<"errcode">>, <<"NYI">>}, {<<"message">>, <<"nyi">>}]}
			end
	end,
	{reply, {200, [], mochijson2:encode(Json)}, State};
handle_call({supervisor, {agent_ring, Queue, MediaId, Agent}}, _From, State) ->
	Json = case {agent_manager:query_agent(Agent), queue_manager:get_queue(Queue)} of
		{false, undefined} ->
			mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Neither agent nor queue exist">>}, {<<"errcode">>, <<"AGENT_NOEXISTS">>}]});
		{false, _Pid} ->
			mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"agent doesn't exist">>}, {<<"errcode">>, <<"AGENT_NOEXISTS">>}]});
		{_Worked, undefined} ->
			mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"queue doesn't exist">>}, {<<"errcode">>, <<"QUEUE_NOEXISTS">>}]});
		{{true, Apid}, Qpid} ->
			case call_queue:get_call(Qpid, MediaId) of
				none ->
					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Call is not in the given queue">>}, {<<"errcode">>, <<"MEDIA_NOEXISTS">>}]});
				{_Key, #queued_call{media = Mpid} = Qcall} ->
					case gen_media:ring(Mpid, Apid, Qcall, ?getRingout) of
						deferred ->
							mochijson2:encode({struct, [{success, true}]});
						 _ ->
							mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Could not ring agent">>}, {<<"errcode">>, <<"RING_FAIL">>}]})
					end
			end
	end,
	{reply, {200, [], Json}, State};
%handle_call({supervisor, {spy, Agent}}, _From, State) ->
%	Json  = case agent_manager:query_agent(Agent) of
%		{true, Apid} ->
%			Mepid = State#state.agent_fsm,
%			case agent:spy(Mepid, Apid) of
%				ok ->
%					mochijson2:encode({struct, [{success, true}]});
%				invalid ->
%					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"invalid action">>}, {<<"errcode">>, <<"MEDIA_ACTION_UNSUPPORTED">>}]})
%			end;
%		false ->
%			mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"no such agent">>}, {<<"errcode">>, <<"AGENT_NOEXISTS">>}]})
%	end,
%	{reply, {200, [], Json}, State};
handle_call({supervisor, {kick_agent, Agent}}, _From, State) ->
	Json = case agent_manager:query_agent(Agent) of
		{true, Apid} ->
			case agent:query_state(Apid) of
				{ok, oncall} ->
					mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Agent currently oncall">>}, {<<"errcode">>, <<"INVALID_STATE">>}]});
				{ok, _State} ->
					agent:stop(Apid),
					mochijson2:encode({struct, [{success, true}]})
			end;
		_Else ->
			mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Agent not found">>}, {<<"errcode">>, <<"AGENT_NOEXISTS">>}]})
	end,
	{reply, {200, [], Json}, State};
handle_call({supervisor, {voicemail, Queue, MediaId}}, _From, State) ->
	Json = case queue_manager:get_queue(Queue) of
		Qpid when is_pid(Qpid) ->
			case call_queue:get_call(Qpid, MediaId) of
				{_Key, #queued_call{media = Mpid}} ->
					case gen_media:voicemail(Mpid) of
						invalid ->
							{struct, [{success, false}, {<<"message">>, <<"media doesn't support voicemail">>}, {<<"errcode">>, <<"MEDIA_ACTION_UNSUPPORTED">>}]};
						ok ->
							{struct, [{success, true}]}
					end;
				_ ->
					{struct, [{success, false}, {<<"message">>, <<"call not found">>}, {<<"errcode">>, <<"MEDIA_NOEXISTS">>}]}
			end;
		_ ->
			{struct, [{success, false}, {<<"message">>, <<"queue not found">>}, {<<"errcode">>, <<"QUEUE_NOEXISTS">>}]}
	end,
	{reply, {200, [], mochijson2:encode(Json)}, State};
handle_call({supervisor, {blab, Message, Grouping, Target}}, _From, State) ->
	Toagentmanager = case Grouping of
		<<"agent">> ->
			{agent, binary_to_list(Target)};
		<<"node">> ->
			case Target of
				<<"System">> ->
					all;
				_AtomIsIt ->
					try list_to_existing_atom(binary_to_list(Target)) of
						Atom -> case lists:member(Atom, [node() | nodes()]) of
							true -> {node, Atom};
							false -> {false, false}
						end
					catch _:_ -> {false, false}
					end
			end;
		<<"profile">> ->
			{profile, binary_to_list(Target)};
		<<"all">> ->
			all
	end,
	Json = case Toagentmanager of
		{false, false} ->
			mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"bad type or value">>}, {<<"errcode">>, <<"BADARG">>}]});
		Else ->
			agent_manager:blab(binary_to_list(Message), Else),
			mochijson2:encode({struct, [{success, true}]})
	end,
	{reply, {200, [], Json}, State};
handle_call({supervisor, {remove_problem_recording, Clientid}}, _From, State) ->
	case file:delete("/tmp/"++binary_to_list(Clientid)++"/problem.wav") of
		ok ->
			{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State};
		{error, Reason} ->
			OutReason = list_to_binary(io_lib:format("~p", [Reason])),
			{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, OutReason}, {<<"errcode">>, <<"UNKNOWN_ERROR">>}]})}, State}
	end;
handle_call({supervisor, {start_problem_recording, Clientid}}, _From, State) ->
	Login = State#state.login,
	Endpoint = State#state.endpointtype,
	EndpointData = State#state.endpointdata,
	AgentRec = #agent{
		login = Login,
		id = Login,
		connection = self(),
		endpointtype = Endpoint,
		endpointdata = EndpointData
	},
	case whereis(freeswitch_media_manager) of
		P when is_pid(P) ->
			case freeswitch_media_manager:record_outage(Clientid, self(), AgentRec) of
				ok ->
					{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State};
				{error, Reason} ->
					{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, list_to_binary(io_lib:format("Initializing recording channel failed (~p)", [Reason]))}]})}, State}
			end;
		_ ->
			{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"freeswitch is not available">>}, {<<"errcode">>, <<"FREESWITCH_NOEXISTS">>}]})}, State}
	end;
handle_call({supervisor, get_motd}, _From, State) ->
	Motd = case cpx_supervisor:get_value(motd) of
		none -> false;
		{ok, Text} -> Text
	end,
	{reply, {200, [], mochijson2:encode({struct, [{success, true}, {<<"result">>, Motd}]})}, State};
handle_call({supervisor, {set_motd, Message, Node}}, _From, State) ->
	{ok, Appnodes} = application:get_env('OpenACD', nodes),
	Nodes = case Node of
		<<"system">> -> Appnodes;
		_ -> [list_to_existing_atom(binary_to_list(N)) || N <- Appnodes, atom_to_list(N) == binary_to_list(Node)]
	end,
	case Nodes of
		[] ->
			{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"no known nodes">>}, {<<"errcode">>, <<"NODE_NOEXISTS">>}]})}, State};
		_ ->
			{Func, Args} = case Message of
				<<"">> -> {drop_value, [motd]};
				_ -> {set_value, [motd, Message]}
			end,
			Res = [try rpc:call(N, cpx_supervisor, Func, Args) of
				{atomic, ok} -> ok
			catch
				What:Why ->
					?WARNING("Could not set motd on ~p due to ~p:~p", [N, What,Why]),
					{What, Why}
			end || N <- Nodes],
			case lists:all(fun(E) -> E == ok end, Res) of
				true->
					{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State};
				false ->
					{reply, {200, [], mochijson2:encode({struct, [
						{success, false},
						{<<"message">>, <<"could not set motd on all nodes">>},
						{<<"errcode">>, <<"UNKNOWN_ERR">>}
					]})}, State}
			end
	end;
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



handle_cast(keep_alive, #state{poll_pid = undefined} = State) ->
	%?DEBUG("keep alive", []),
	{noreply, State#state{poll_pid_established = util:now()}};
handle_cast({poll, Frompid}, State) ->
	%?DEBUG("Replacing poll_pid ~w with ~w", [State#state.poll_pid, Frompid]),
	case State#state.poll_pid of
		undefined -> 
			ok;
		Pid when is_pid(Pid) ->
			Pid ! {kill, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Poll pid replaced">>}, {<<"errcode">>, <<"POLL_PID_REPLACED">>}]})}
	end,
	case State#state.poll_queue of
		[] ->
			%?DEBUG("Empty poll queue", []),
			link(Frompid),
			{noreply, State#state{poll_pid = Frompid, poll_pid_established = util:now()}};
		Pollq ->
			%?DEBUG("Poll queue length: ~p", [length(Pollq)]),
			Newstate = State#state{poll_queue=[], poll_pid_established = util:now(), poll_pid = undefined},
			Json2 = {struct, [{success, true}, {<<"result">>, lists:reverse(Pollq)}]},
			Frompid ! {poll, {200, [], mochijson2:encode(Json2)}},
			{noreply, Newstate}
	end;





handle_cast(_Msg, State) ->
	{noreply, State}.

%%====================================================================
%% handle_info
%%====================================================================





handle_info(poll_flush, State) ->
	case {State#state.poll_pid, State#state.poll_queue} of
		{undefined, _} ->
			{noreply, State#state{poll_flush_timer = undefined}};
		{_Pid, []} ->
			{noreply, State#state{poll_flush_timer = undefined}};
		{Pid, _PollQueue} when is_pid(Pid) ->
			Pid ! {poll, {200, [], mochijson2:encode({struct, [
				{success, true},
				{<<"data">>, lists:reverse(State#state.poll_queue)},
				{<<"result">>, lists:reverse(State#state.poll_queue)}
			]})}},
			unlink(Pid),
			{noreply, State#state{poll_queue = [], poll_pid = undefined, poll_pid_established = util:now(), poll_flush_timer = undefined}}
	end;
handle_info(check_live_poll, #state{poll_pid_established = Last, poll_pid = undefined} = State) ->
	Now = util:now(),
	case Now - Last of
		N when N > 20 ->
			?NOTICE("Stopping due to missed_polls; last:  ~w now: ~w difference: ~w", [Last, Now, Now - Last]),
			{stop, normal, State};
		_N ->
			Tref = erlang:send_after(?TICK_LENGTH, self(), check_live_poll),
			{noreply, State#state{ack_timer = Tref}}
	end;
handle_info(check_live_poll, #state{poll_pid_established = Last, poll_pid = Pollpid} = State) when is_pid(Pollpid) ->
	Tref = erlang:send_after(?TICK_LENGTH, self(), check_live_poll),
	case util:now() - Last of
		N when N > 20 ->
			%?DEBUG("sending pong to initiate new poll pid", []),
			Newstate = push_event({struct, [{success, true}, {<<"command">>, <<"pong">>}, {<<"timestamp">>, util:now()}]}, State),
			{noreply, Newstate#state{ack_timer = Tref}};
		_N ->
			{noreply, State#state{ack_timer = Tref}}
	end;







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
				{<<"command">>, <<"supervisorDrop">>},
				{<<"data">>, {struct, [
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
				{<<"command">>, <<"supervisorSet">>},
				{<<"data">>, {struct, [
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



handle_info({'EXIT', Pollpid, Reason}, #state{poll_pid = Pollpid} = State) ->
	case Reason of
		normal ->
			ok;
		_ ->
			?NOTICE("The pollpid died due to ~p", [Reason])
	end,
	{noreply, State#state{poll_pid = undefined}};
handle_info({'EXIT', Pid, Reason}, #state{listener = Pid} = State) ->
	?WARNING("The listener at ~w died due to ~p", [Pid, Reason]),
	{stop, Reason, State};




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
%handle_call({supervisor, Request}, _From, #state{securitylevel = Seclevel} = State) when Seclevel =:= supervisor; Seclevel =:= admin ->
%	?DEBUG("Handing supervisor request ~s", [lists:flatten(Request)]),
%	case Request of
%		["startmonitor"] ->
%			cpx_monitor:subscribe(),
%			{reply, {200, [], mochijson2:encode({struct, [{success, true}, {<<"message">>, <<"subscribed">>}]})}, State};
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
%		[Node | Do] ->
%			Nodes = get_nodes(Node),
%			{Success, Result} = do_action(Nodes, Do, []),
%			{reply, {200, [], mochijson2:encode({struct, [{success, Success}, {<<"result">>, Result}]})}, State}
%	end;
%handle_call({supervisor, _Request}, _From, State) ->
%	?NOTICE("Unauthorized access to a supervisor web call", []),
%	{reply, {403, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"insufficient privledges">>}]})}, State};
