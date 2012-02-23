%% @doc Common functions for the web and tcp-json agent connections.
%% It is up to the connection to handle login and agent launching.  After
%% that, json should be sent into this module, as well as cast messages
%% the connection does not handle internally.
%%
%% == Json API ==
%%
%% The Json API as two sides, client requests and server events.  OpenACD's
%% policy is to assume the client connections are complete idiots, and
%% therefore not worth asking about anything; not event if an event has been
%% handled.  Thus, requests always come from the client, and events always
%% come from the server.
%%
%% === Requests ===
%%
%% A Json request will have the following form:
%% <pre>{"request_id":any(),
%% (optional)module_name() : string(),
%% function_name() : string(),
%% (optional)"args": any() | [any()]}</pre>
%%
%% If the module name is omitted, it is assumed to be this module
%% ({@module}); ie: handled intenrally.  If args is not an array, it is
%% wrapped in an array of length 1.  If args is omitted, it is assumed to
%% be an array of length 0.  This way, requests match up to erlang
%% Module:Function/Arity format.  Erlang functions in this module that have %% {@agent_api} at the beginning of thier documentation conform to the form %% above with one caveat:  The first argument is always the internal state
%% of the connection, and is obviously not sent with the json requests.
%% Thus, a properly documented project will be useful to agent connection
%% and agent client developers.
%%
%% Request_id is an opaque type sent by the client; it is sent back with
%% the reply to enable asynchronous requests.
%%
%% A json response will have 3 major forms.  
%% 
%% A very simple success:
%% <pre> {
%%  "request_id": any(),
%% 	"success":  true
%% }</pre>
%% A success with a result:
%% <pre> {
%%  "request_id": any(),
%% 	"success":  true,
%% 	"result":   any()
%% }</pre>
%% A failure:
%% <pre> {
%%  "request_id":  any(),
%% 	"success":  false,
%% 	"message":  string(),
%% 	"errcode":  string()
%% }</pre>
%%
%% === Events ===
%%
%% A server event is a json object with at least a "command" property.  If
%% the command references a specific agent channel, it will also have a
%% "channel_id" property.  All other properties are specific to the server
%% events.  
%%
%% == Erlang API ==
%%
%% There are two sides to the erlang API, the connection facing side (such
%% as a web or tcp connection), and the api handler side, such as this
%% module or plugins handing agent requests.
%%
%% === Agent Connections ===
%%
%% After the login procedure, init/1 should be called, passing in the agent
%% record (prefereably after the connection is set).  If a reply of
%% `{ok, #state{}}' is returned, stash the state.  It will be used in the
%% encode_cast, and handle_json functions.
%%
%% Both encode_cast and handle_json have the same return types.
%% <dl>
%% <dt>`{ok, json(), state()}'</dt><dd>If json() is undefined, no json is
%% to be sent.  Otherwise the json should be encoded using
%% mochijson2:encode/1 and sent over the wire.</dd>
%% <dt>`{exit, json(), state()}'</dt><dd>the connection should commit
%% hari-kari.  If json() is undefined, that's all that needs to happen, 
%% otherwise json should be sent, then death.</dd>
%% </dl>
%%
%% === Api Handlers ===
%%
%% Modules intended to handle json calls can do so in two ways.  The first
%% is to register a hook to {@link cpx_hooks. agent_api_call}.  This hook
%% is triggered if the module and function with the appropriate arity is
%% not found using the method described below.
%%
%% The alternative is more efficient, preventing a call to cpx_hooks,
%% though there is no custom information passed to the module.  The module
%% has an attribute `agent_api_functions', which is a list of tuples of
%% type `{FunctionAtom, Arity}'.  The arity must be one more than the
%% number of arguments sent with the json request; this is because the
%% state of the agent connection is sent as the first argument.
%%
%% An api handler function (either kind) should return one of the following:
%% <dl>
%% <dt>`ok'</dt><dd>A simple success json is returned</dd>
%% <dt>`{ok, json()}'</dt><dd>A json success is sent with the given json
%% set as the result</dd>
%% <dt>`{error, bin_string(), bin_string()}}'</dt><dd>An error is
%% returned</dd>
%% <dt> `exit'</dt><dd>The connection should exit, likely taking the agent
%% fsm with it.  A simple success is returned.</dd>
%% <dt>`{exit, json()}'</dt><dd>The connection should exit, likely taking
%% the agent with it.  A success is returned, with the json as the result.
%% </dd>
%% </dl>
%%
%% Plugins that want to send server events to agents should send one of the
%% two arbitrary command messages.
%% <ul>
%% <li>`{arbitrary_command, Command, Props}'</li>
%% <li>`{arbitrary_command, ChannelPidOrId, Command, Props}'</li>
%% </ul>
%%
%% In both cases, `Command' should be either an atom or binary string.
%% Props can be either a json object struct, or a property list that can
%% be put into a json object struct.
%%
%% `ChannelPidOrId' is the channel id either in its pid form, string form,
%% or binary string form.  In any form, if the channel does not exist, the
%% message is ignored.
%%
%% When the event is sent, the command property and channelid (if given)
%% properties are automatically pre-pended onto the json struct.  The
%% result is sent to the connection for encoding and being sent over the
%% wire:
%%
%% <pre>{"command": string(),
%% "channelid": string(),
%% "field1": any(),
%% "field2": any(),...
%% "fieldN": any()
%% }</pre>
%%
%% @TODO: Document the server events.

-module(cpx_agent_connection).

-include("log.hrl").
-include("agent.hrl").
-include("call.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(json(Struct), mochijson2:encode(Struct)).
-define(reply_err(Id, Message, Code), ?json({struct, [{request_id, Id},
	{success, false}, {message, Message}, {errcode, Code}]})).
-define(simple_success(Id), ?json({struct, [{request_id, Id},
	{success, true}]})).
-define(reply_success(Id, Struct), ?json({struct, [{request_id, Id},
	{success, true}, {result, Struct}]})).

-type(json() :: {struct, [{binary(), json()}]} | binary() | integer() | float() | [json()]).

-record(state, {
	salt :: any(),
	agent :: #agent{},
	channels = dict:new() :: dict(),
	connection :: pid(),
	supervisor_state :: any()
}).

-record(channel_state, {
	current_call :: #call{} | 'undefined' | 'expect',
	mediaload :: any()
}).

%% public api
-export([
	init/1,
	encode_cast/2,
	handle_json/2
]).
-export([
	%% requests, exported for documentation happy.
	agent_transfer/3,
	end_wrapup/2,
	get_agent_profiles/1,
	get_avail_agents/1,
	get_endpoint/2,
	get_queue_transfer_options/2,
	get_tabs_menu/1,
	% TODO implement
	%load_media/1,
	logout/1,
	media_call/3,
	media_call/4,
	media_cast/3,
	media_cast/4,
	%media_hangup/2,
	%plugin_call/3,
	queue_transfer/4,
	%ring_test/1,
	set_endpoint/3,
	set_release/2,
	set_state/3,
	set_state/4
]).

%% An easier way to do a lookup for api functions.
-agent_api_functions([
	{agent_transfer, 3},
	{end_wrapup, 2},
	{get_agent_profiles, 1},
	{get_avail_agents, 1},
	{get_endpoint, 2},
	{get_queue_transfer_options, 2},
	{get_tabs_menu, 1},
	% TODO implement
	%{load_media, 1},
	{logout, 1},
	{media_call, 3},
	{media_call, 4},
	{media_cast, 3},
	{media_cast, 4},
	{queue_transfer, 4},
	{set_endpoint, 3},
	{set_release, 2},
	{set_state, 3},
	{set_state, 4}
]).

%% =======================================================================
%% Public api
%% =======================================================================

%% @doc After the connection has been started, this should be called to
%% seed the state.
-spec(init/1 :: (Agent :: #agent{}) -> {'ok', #state{}}).
init(Agent) ->
	#agent{used_channels = Channels, connection = Conn} = Agent,
	% TODO used_channels likely lacks what we want
	{ok, #state{agent = Agent, channels = Channels, connection = Conn}}.

%% @doc When the connection gets a cast it cannot handle, this should be
%% called.  It will either return an error, or json to pump out to the
%% client.
-spec(encode_cast/2 :: (State :: #state{}, Cast :: any()) -> 
	{'error', any(), #state{}} | {'ok', json(), #state{}}).
encode_cast(State, Cast) ->
	handle_cast(Cast, State).

%% @doc After unwrapping the binary that will hold json, and connection
%% should call this.
-spec(handle_json/2 :: (State :: #state{}, Json :: json()) ->
	{'ok', json(), #state{}} | {'error', any(), #state{}} |
	{'exit', json(), #state{}}).
handle_json(State, {struct, Json}) ->
	ThisModBin = list_to_binary(atom_to_list(?MODULE)),
	ModBin = proplists:get_value(Json, <<"module">>, ModBin),
	ReqId = proplists:get_value(Json, <<"request_id">>),
	ModRes = try binary_to_existing_atom(ModBin, utf8) of
		ModAtom ->
			{ok, ModAtom}
	catch
		error:badarg ->
			{error, bad_module}
	end,
	FuncBin = proplists:get_value(Json, <<"function">>, <<"undefined">>),
	FuncRes = try binary_to_existing_atom(FunBin, utf) of
		FuncAtom ->
			{ok, FuncAtom};
	catch
		error:badarg ->
			{error, bad_function}
	end,
	Args = case proplists:get_value(Json, <<"args">>, []) of
		ArgsList when is_list(ArgsList) -> ArgsList;
		Term -> [Term]
	end,
	case {ModRes, FuncRes} of
		{{error, bad_module}, _} ->
			{error, bad_module, State};
		{_, {error, bad_function}} ->
			{error, bad_function, State};
		{{ok, Mod}, {ok, Func}} ->
			Attrs = Mod:module_info(attributes),
			AgentApiFuncs = proplists:get_value(agent_api_functions, Attrs, []),
			Arity = length(args) + 1,
			#state{agent = Agent} = State,
			case lists:member({Func, Arity}, [Agent | AgentApiFuncs]) of
				false ->
					{ok, ?reply_err(ReqId, <<"no such function">>, <<"FUNCTION_NOEXISTS">>), State};
				true ->
					try apply(Mod, Func, [State, Args]) of
						{ok, ResultJson} ->
							{ok, ?success(RequestId, ResultJson), State};
						{error, Msg, Code} ->
							{ok, ?reply_err(RequestId, Msg, Code), State};
						Else ->
							ErrMsg = list_to_binary(io_lib:format("~p", [Else])),
							{ok, ?reply_err(ReqId, ErrMsg, <<"UNKNOWN_ERROR">>), State}
					catch
						What:Why ->
							ErrMsg = list_to_binary(io_lib:format("error occured:  ~p:~p", [What, Why])),
							{ok, ?reply_err(ReqId, ErrMsg, <<"UNKNOWN_ERROR">>), State}
					end
			end
	end;

handle_json(State, _InvalidJson) ->
	{error, invalid_json, State}.

%% =======================================================================
%% Agent connection api
%% =======================================================================

%% @doc {@agent_api} Logs the agent out.  The result is a simple success.
-spec(logout/2 :: (State :: #state{}, Id :: any()) -> {'exit', json(), #state{}}).
logout(State, Id) ->
	{exit, ?simple_success(Id), State}.

%% @doc {@agent_api} Sets the release mode of the agent.  To set an agent in
%% a release mode, pass `<<"Default">>', `<<"default">>', or
%% `<<"Id:Name:Bias">>' as the arguement.  Setting the agent idle is done
%% by sending `<<"none">>' or `false'.
-spec(set_release/3 :: (State :: #state{}, Id :: any(),
	Release :: binary() | 'false') ->
	{'ok', json(), #state{}} | {error, any(), #state{}}).
set_release(State, Id, Release) ->
	RelData = case Release of
		<<"none">> ->
			none;
		false ->
			none;
		<<"default">> ->
			default;
		<<"Default">> ->
			default;
		Else ->
			[Id, Name, Bias] = util:string_split(binary_to_list(Else), ":"),
			{Id, Name, list_to_integer(Bias)}
	end,
	#state{agent = Agent} = State,
	case agent:set_release(Agent#agent.source, RelData) of
		ok ->
			{ok, ?simple_success(Id), State};
		Err ->
			ErrBin = list_to_binary(io_lib:format("~p", [Err])),
			{ok, ?reply_err(Id, ErrBin, <<"UNKNOWN_ERR">>), State}
	end.

%% @doc {@agent_api} Set the agent channel `Channel' to the given
%% `Statename' with default state data.  No result property as it either
%% worked or didn't.  There will likely be an event as well to set the agent
%% state, so it is recommended that no actual change occur on the agent UI
%% side until that event is received.
-spec(set_state/4 :: (State :: #state{}, Id :: any(), Channel :: binary(),
	Statename :: binary()) -> {'ok', json(), #state{}}).
set_state(State, Id, Channel, Statename) ->
	ChannelName = binary_to_list(Channel),
	StateName = binary_to_list(Statename),
	case fetch_channel(ChannelName, State) of
		none ->
			{ok, ?reply_err(Id, <<"Channel not found">>, <<"CHANNEL_NOEXISTS">>), State};
		{Chan, _ChanState} ->
			case agent_channel:set_state(Chan, agent_channel:list_to_state(StateName)) of
				ok ->
					{ok, ?simple_success(Id), State};
				{error, invalid} ->
					{ok, ?reply_err(Id, <<"Channel state change invalid">>, <<"INVALID_STATE_CHANGE">>), State}
			end
	end.

%% @doc {@agent_api} Set the agent channel `Channel' to the given
%% `Statename' with the given `Statedata'.  No result property as it either %% worked or it didn't.  State data will vary based on state.  Furthermore,
%% in the case of success, an event is sent later by the agent fsm.  It is
%% recommended that no change to the UI occur until that event is received.
-spec(set_state/5 :: (State :: #state{}, Id :: any(),
	ChannelBin :: binary(), StateBin :: binary(), StateDataBin :: binary()) ->
	{'ok', json(), #state{}}).
set_state(State, Id, ChannelBin, StateBin, StateDataBin) ->
	Channel = binary_to_list(ChannelBin),
	Statename = binary_to_list(StateBin),
	Statedata = binary_to_list(StateDataBin),
	case fetch_channel(Channel, State) of
		none ->
			{ok, ?reply_err(Id, <<"Channel not found">>, <<"CHANNEL_NOEXISTS">>), State};
		{Chan, _ChanState} ->
			case agent_channel:set_state(Chan, agent_channel:list_to_state(Statename), Statedata) of
				ok ->
					{ok, ?simple_success(Id), State};
				invalid ->
					{ok, ?reply_err(Id, <<"Channel state change invalid">>, <<"INVALID_STATE_CHANGE">>), State}
			end
	end.

%% @doc {@agent_api} End wrapup the agent channel 'Channel'.  This also
%% kills the channel, making it available for use again.  No result
%% property as it iether worked or didn't.  There will also be an event
%% later sent by the agent fsm.  It is recommended that no UI changes
%% occur until that event comes in.
-spec(end_wrapup/3 :: (State :: #state{}, Id :: any(), ChanBin :: binary())
	-> {'ok', json(), #state{}}).
end_wrapup(State, Id, ChanBin) ->
	Channel = binary_to_list(ChanBin),
	case fetch_channel(Channel, State) of
		none ->
			{ok, ?reply_err(Id, <<"Channel not found">>, <<"CHANNEL_NOEXISTS">>)};
		{Chan, _ChanState} ->
			case agent_channel:end_wrapup(Chan) of
				ok ->
					{ok, ?simple_success(Id), State};
				invalid ->
					{ok, ?reply_err(Id, <<"Channel not stopped">>, <<"INVALID_STATE_CHANGE">>), State}
			end
	end.

%% @doc {@agent_api} Get a list of the agents that are currently available. 
%% Result is:
%% <pre>[{
%% 	"name":  string(),
%% 	"profile":  string(),
%% 	"state":  "idle" | "released"
%% }]</pre>
-spec(get_avail_agents/2 :: (State :: #state{}, Id :: any()) ->
	{'ok', json(), #state{}}).
get_avail_agents(State, Id) ->
	Agents = [agent:dump_state(Pid)|| {_K, {Pid, _Id, _Time, _Skills}} <-
		agent_manager:list()],
	Noms = [{struct, [{<<"name">>, list_to_binary(Rec#agent.login)}, {<<"profile">>, list_to_binary(Rec#agent.profile)}]} || Rec <- Agents],
	{ok, ?reply_success(Id, Noms), State}.

%% @doc {@agent_api} Get a list of the profiles that are in the system.
%% Result is:
%% <pre>[{
%% 	"name":  string(),
%% 	"order":  number()
%% }]</pre>
-spec(get_agent_profiles/2 :: (State :: #state{}, Id :: any()) ->
	{'ok', json(), #state{}}).
get_agent_profiles(State, Id) ->
	Profiles = agent_auth:get_profiles(),
	Jsons = [
		{struct, [{<<"name">>, list_to_binary(Name)}, {<<"order">>, Order}]} ||
		#agent_profile{name = Name, order = Order} <- Profiles
	],
	{ok, ?reply_success(Id, Jsons), State}.

%% @doc {@agent_api} Transfer the call on the given `Channel' to `Agent'
%% login name.  No result is sent back as it's a simple success or failure.
-spec(agent_transfer/4 :: (State :: #state{}, Id :: any(),
	ChannelBin :: binary(), Agent :: binary()) ->
	{'ok', json(), #state{}}).
agent_transfer(State, Id, ChannelBin, AgentBin) ->
	Channel = binary_to_list(ChannelBin),
	Agent = binary_to_list(AgentBin),
	case fetch_channel(Channel, State) of
		none ->
			{ok, ?reply_err(Id, <<"Channel not found">>, <<"CHANNEL_NOEXISTS">>)};
		{Chan, _ChanState} ->
			case agent_channel:agent_transfer(Chan, Agent) of
				ok ->
					{ok, ?simple_success(Id), State};
				invalid ->
					{ok, ?reply_err(Id, <<"Channel refused">>, <<"INVALID_STATE_CHANGE">>), State}
			end
	end.

%% @doc {@agent_api} @see media_call/5
-spec(media_call/4 :: (State :: #state{}, Id :: any(), Channel :: binary(),
	Command :: binary()) -> {'ok', json(), #state{}}).
media_call(State, Id, Channel, Command) ->
	media_call(State, Id, Channel, Command, []).

%% @doc {@agent_api} Forward a request to the media associated with an
%% oncall agent channel.  `Command' is the name of the request to make.
%% `Args' is a list of arguments to be sent with the `Command'.  Check the
%% documentation of the media modules to see what possible returns there
%% are.
-spec(media_call/5 :: (State :: #state{}, Id :: any(),
	ChannelBin :: binary(), Command :: binary(), Args :: [any()]) ->
	{'ok', json(), #state{}}).
media_call(State, Id, Channel, Command, Args) ->
	case fetch_channel(Channel, State) of
		none ->
			{reply, ?reply_err(Id, <<"Channel doesn't exist">>, <<"CHANNEL_NOEXISTS">>), State};
		{_ChanPid, #channel_state{current_call = #call{source = CallPid} = Call}} ->
			Reply = try gen_media:call(CallPid, {?MODULE, Command, Args}) of
				invalid ->
					?DEBUG("media call returned invalid", []),
					?reply_err(Id, <<"invalid media call">>, <<"INVALID_MEDIA_CALL">>);
				Response ->
					?reply_success(Id, Response)
			catch
				exit:{noproc, _} ->
					?DEBUG("Media no longer exists.", []),
					?reply_err(Id, <<"media no longer exists">>, <<"MEDIA_NOEXISTS">>);
				What:Why ->
					?DEBUG("Media exploded:  ~p:~p", [What,Why]),
					ErrBin = list_to_binary(io_lib:format("~p:~p", [What,Why])),
					?reply_err(Id, ErrBin, <<"UNKNOWN_ERROR">>)
			end,
			{ok, Reply, State}
	end.

%% @doc {@agent_api} @see media_cast/5
-spec(media_cast/4 :: (State :: #state{}, Id :: any(), Channel :: binary(),
	Command :: binary()) -> {'ok', json(), #state{}}).
media_cast(State, Id, Channel, Command) ->
	media_cast(State, Id, Channel, Command, []).

%% @doc {@agent_api} Forward a command to the media associated with an
%% oncall agent channel.  `Command' is the name of the command to send.
%% `Args' is a list of arguments to send with the `Command'.  There is no
%% reply expected, so a simple success is always returned.
-spec(media_cast/5 :: (State :: #state{}, Id :: any(), Channel :: binary(),
	Command :: binary(), Args :: [any()]) -> {'ok', json(), #state{}}).
media_cast(State, Id, Channel, Command, Args) ->
	case fetch_channel(Channel, State) of
		none ->
			{ok, ?reply_err(Id, <<"Channel doesn't exist">>, <<"CHANNEL_NOEXISTS">>), State};
		{_ChanPid, #channel_state{current_call = #call{source = CallPid} = Call}} ->
			gen_media:cast(CallPid, {?MODULE, Command, Args}),
			{ok, ?simple_success(Id), State}
	end.

%% @doc {@agent_api} Get the fields and skills an agent can assign to a
%% media before transfering it back into queue.  Result:
%% <pre>{
%% 	"curentVars":  [{
%% 		string():  string()
%%	}],
%%	"prompts":  [{
%% 		"name":  string(),
%% 		"label":  string(),
%% 		"regex":  regex_string()
%% 	}],
%% 	"skills":[
%%		string() | {"atom":  string(),  "value":  string()}
%% 	]
%% }</pre>
-spec(get_queue_transfer_options/3 :: (State :: #state{}, Id :: any(),
	Channel :: binary()) -> {'ok', json(), #state{}}).
get_queue_transfer_options(State, Id, Channel) ->
	case fetch_channel(Channel, State) of
		none ->
			{ok, ?reply_err(Id, <<"no such channel">>, <<"CHANNEL_NOEXISTS">>), State};
		{_Chan, #channel_state{current_call = Call}} when is_record(Call, call) ->
			{ok, Setvars} = gen_media:get_url_getvars(Call#call.source),
			{ok, {Prompts, Skills}} = cpx:get_env(transferprompt, {[], []}),
			Varslist = [begin
				Newkey = case is_list(Key) of
					true ->
						list_to_binary(Key);
					_ ->
						Key
				end,
				Newval = case is_list(Val) of
					true ->
						list_to_binary(Val);
					_ ->
						Val
				end,
				{Newkey, Newval}
			end || 
			{Key, Val} <- Setvars],
			Encodedprompts = [{struct, [{<<"name">>, Name}, {<<"label">>, Label}, {<<"regex">>, Regex}]} || {Name, Label, Regex} <- Prompts],
			Encodedskills = cpx_web_management:encode_skills(Skills),
			Json = {struct, [
				{<<"currentVars">>, {struct, Varslist}},
				{<<"prompts">>, Encodedprompts},
				{<<"skills">>, Encodedskills}
			]},
			{ok, ?reply_success(Id, Json), State};
		Else ->
			{ok, ?reply_err(Id, <<"channel is not oncall">>, <<"INVALID_STATE_CHANGE">>), State}
	end.

%% @doc {@agent_api} Force the agent channgel to disconnect the media;
%% usually through a brutal %% kill of the media pid.  Best used as an
%% emergency escape hatch, and not under normal call flow.  No result set
%% as it's merely success or failure.
-spec(media_hangup/3 :: (State :: #state{}, Id :: any(),
	Channel :: binary()) -> {'ok', json(), #state{}}).
media_hangup(State, Id, Channel) ->
	case fetch_channel(Channel, State) of
		none ->
			{ok, ?reply_err(Id, <<"no such channel">>, <<"CHANNEL_NOEXISTS">>), State};
		{_ChanPid, #channel_state{current_call = Call}} when is_record(Call, call) ->
			?DEBUG("The agent is committing call murder!", []),
			exit(Call#call.source, agent_connection_request),
			{ok, ?simple_success(Id), State};
		_ ->
			{ok, ?reply_err(Id, <<"channel not oncall">>, <<"INVALID_STATE_CHANGE">>), State}
	end.

%% @doc {@agent_api} Transfer the channel's call into `Queue' with
%% the given `Opts'.  The options is a json object with any number of 
%% properties that are passed to the media.  If there is a property 
%% `"skills"' with a list, the list is interpreted as a set of skills to 
%% apply to the media.  No result is set as it is merely success or 
%% failure.
-spec(queue_transfer/5 :: (State :: #state{}, Id :: any(),
	QueueBin :: binary(), Channel :: binary(), Opts :: json()) ->
	{'ok', json(), #state{}}).
queue_transfer(State, Id, QueueBin, Channel, {struct, Opts}) ->
	Queue = binary_to_list(QueueBin),
	{Skills, Opts0} = case lists:key_take(<<"skills">>, 1, Opts) of
		false -> {[], Opts};
		{value, PostedSkills, Opts1} ->
			FixedSkills = skills_from_post(PostedSkills),
			{FixedSkills, Opts1}
	end,
	case fetch_channel(Channel, State) of
		none ->
			{ok, ?reply_err(Id, <<"no such channel">>, <<"CHANNEL_NOEXISTS">>), State};
		{Chan, #channel_state{current_call = Call}} when is_record(Call, call) ->
			gen_media:set_url_getvars(Call#call.source, Opts0),
			gen_media:add_skills(Call#call.source, Skills),
			case agent_channel:queue_transfer(Chan, Queue) of
				ok ->
					{ok, ?simple_success(Id), State};
				invalid ->
					{ok, ?reply_err(Id, <<"agent channel rejected transfer">>, <<"INVALID_STATE_CHANGE">>), State}
			end;
		_ ->
			{ok, ?reply_err(Id, <<"agent channel not oncall">>, <<"INVALID_STATE_CHANGE">>), State}
	end.

skills_from_post(Skills) ->
	skills_from_post(Skills, []).

skills_from_post([], Acc) ->
	cpx_web_management:parse_posted_skills(Acc);

skills_from_post([{struct, Props} | Tail], Acc) ->
	Atom = binary_to_list(proplists:get_value(<<"atom">>, Props)),
	Expanded = binary_to_list(proplists:get_value(<<"expanded">>, Props)),
	SkillString = "{" ++ Atom ++ "," ++ Expanded ++ "}",
	Acc0 = [SkillString | Acc],
	skills_from_post(Tail, Acc0);

skills_from_post([Atom | Tail], Acc) when is_binary(Atom) ->
	Acc0 = [binary_to_list(Atom) | Acc],
	skills_from_post(Tail, Acc0).

%% @doc {@agent_api} Get the agent's endpoint data for a given module.
-spec(get_endpoint/3 :: (State :: #state{}, Id :: any(),
	TypeBin :: binary()) -> {'ok', json(), #state{}}).
get_endpoint(State, Id, TypeBin) ->
	case catch erlang:binary_to_existing_atom(TypeBin, utf8) of
		{'EXIT', {badarg, _}} ->
			{ok, ?reply_err(Id, <<"invalid endpoint type">>, <<"INVALID_ENDPOINT_TYPE">>), State};
		Type ->
			#state{agent = Agent} = State,
			case agent:get_endpoint(Type, Agent) of
				{error, notfound} ->
					{ok, ?reply_success(Id, null), State};
				{ok, {InitOpts, _}} ->
					Json = endpoint_to_struct(Type, InitOpts),
					{ok, ?reply_success(Id, Json), State}
			end
	end.

endpoint_to_struct(freeswitch_media, Data) ->
	FwType = proplists:get_value(type, Data, null), %% atom()
	FwData = case proplists:get_value(data, Data) of
		undefined -> null;
		Dat -> list_to_binary(Dat)
	end,
	Persistant = proplists:get_value(persistant, Data),
	{struct, [{type, FwType}, {data, FwData}, {persistant, Persistant}]};

endpoint_to_struct(email_media, _Data) ->
	{struct, []};

endpoint_to_struct(dummy_media, Opt) ->
	{struct, [{endpoint, Opt}]}.

%% @doc {@agent_api} Sets the agent's endpoint data to the given, well, data.
%% Particularly useful if the flash phone is used, as all of the connection
%% data will not be available for that until it is started on in the 
%% browser.
% TODO make this not media specific.
-spec(set_endpoint/4 :: (State :: #state{}, Id :: any(),
	Endpoint :: binary(), Data :: binary()) -> any()).
set_endpoint(State, Id, <<"freeswitch_media">>, Struct) ->
	set_endpoint_int(State, Id, freeswitch_media, Struct, fun(Data) ->
		FwType = case proplists:get_value(<<"type">>, Data) of
			%<<"rtmp">> -> rtmp;
			<<"sip_registration">> -> sip_registration;
			<<"sip">> -> sip;
			<<"iax">> -> iax;
			<<"h323">> -> h323;
			<<"pstn">> -> pstn;
			_ -> undefined
		end,

		case FwType of
			undefined ->
				{error, unknown_fw_type};
			_ ->
				FwData = binary_to_list(proplists:get_value(<<"data">>, 
					Data, <<>>)),
				Persistant = case proplists:get_value(<<"persistant">>, 
					Data) of
						true -> true;
						_ -> undefined
				end,

				[{type, FwType}, {data, FwData}, {persistant, Persistant}]
		end
	end);

set_endpoint(State, Id, <<"email_media">>, _Struct) ->
	set_endpoint_int(State, Id, email_media, {struct, []}, fun(_) -> ok end);

set_endpoint(State, Id, <<"dummy_media">>, Struct) ->
	set_endpoint_int(State, Id, dummy_media, Struct, fun(Data) ->
		case proplists:get_value(<<"dummyMediaEndpoint">>, Data) of
			<<"ring_channel">> ->  ring_channel;
			<<"inband">> -> inband;
			<<"outband">> -> outband;
			<<"persistant">> -> persistant;
			_ -> {error, unknown_dummy_endpoint}
		end
	end);

set_endpoint(State, Id, _Type, _Struct) ->
	{ok, ?reply_err(Id, <<"unknwon endpoint">>, <<"INVALID_ENDPOINT">>), State}.

set_endpoint_int(State, Id, Type, {struct, Data}, DataToOptsFun) ->
	case DataToOptsFun(Data) of
		{error, Error} ->
			{ok, ?reply_err(Id, iolist_to_binary(io_lib:format("error with input: ~p", [Error])), <<"INVALID_ENDPOINT">>), State};
		Opts ->
			#state{agent = Agent} = State,
			case agent:set_endpoint(Agent#agent.source, Type, Opts) of
				ok ->
					{ok, ?simple_success(Id), State};
				{error, Error2} ->
					{ok, ?reply_err(Id, iolist_to_binary(io_lib:format("error setting endpoint: ~p", [Error2])), <<"INVALID_ENDPOINT">>), State}
			end
	end.

%% @doc {@agent_api} Gathers the tabs an agent can access, and pushes the result
%% into the command queue.
%% {"command": "set_tabs_menu",
%% "tabs": [
%%     {"label":string(),"href":string()}
%% ]}
% TODO freaking special snowflake.
get_tabs_menu(State, Id) ->
	gen_server:cast(State, get_tabs_menu),
	{ok, ?simple_success(Id), State}.

%% @doc Useful when a plugin needs to send information or results to the
%% agent ui.
% TODO another special snowflake.
-spec(arbitrary_command/3 :: (Conn :: pid(), Command :: binary() | atom(),
	JsonProps :: [{binary() | atom(), any()}]) -> 'ok').
arbitrary_command(Conn, Command, JsonProps) ->
	gen_server:cast(Conn, {arbitrary_command, Command, JsonProps}).

%% =======================================================================
%% Internal Functions
%% =======================================================================

fetch_channel(Channel, Channels) when is_binary(Channel) ->
	fetch_channel(binary_to_list(Channel), Channels);

fetch_channel(Channel, #state{channels = Chans}) ->
	fetch_channel(Channel, Chans);

fetch_channel(Channel, Channels) when is_list(Channel) ->
	?DEBUG("The chan:  ~p, The channels:  ~p", [Channel, Channels]),
	Chans = [C || C <- dict:fetch_keys(Channels), pid_to_list(C) =:= Channel],
	case Chans of
		[] ->	none;
		[Chan] -> {Chan, dict:fetch(Chan, Channels)}
	end;

fetch_channel(Channel, Channels) when is_pid(Channel) ->
	case dict:find(Channel, Channels) of
		error -> none;
		{ok, Chan} -> {Channel, Chan}
	end.

%% -----------------------------------------------------------------------

update_channels(Chanpid, ChanInfo, State) when is_record(State, state) ->
	Channels = State#state.channels,
	Channels0 = update_channels(Chanpid, ChanInfo, Channels),
	{ok, State#state{channels = Channels0}};

update_channels(Chanpid, Call, Channels) when is_record(Call, call) ->
	case fetch_channel(Chanpid, Channels) of
		none ->
			dict:store(Chanpid, #channel_state{current_call = Call}, Channels);
		{Chanpid, ChanState} ->
			dict:store(Chanpid, ChanState#channel_state{current_call = Call}, Channels)
	end;

update_channels(Chanpid, {wrapup, Call}, Channels) when is_record(Call, call) ->
	Store = #channel_state{mediaload = undefined, current_call = Call},
	dict:store(Chanpid, Store, Channels);

update_channels(Chanpid, {_, Call}, Channels) when is_record(Call, call) ->
	Store = case fetch_channel(Chanpid, Channels) of
		none ->
			#channel_state{current_call = Call, mediaload = Call};
		{_, Cache} ->
			Cache#channel_state{current_call = Call, mediaload = Call}
	end,
	dict:store(Chanpid, Store, Channels);

update_channels(_Chanpid, _Call, State) ->
	{ok, State}.

%% -----------------------------------------------------------------------

handle_cast({arbitrary_command, Command, {struct, Props}}, State) ->
	handle_cast({arbitrary_command, Command, Props}, State);

handle_cast({arbitrary_command, Command, Props}, State) when is_atom(Command); is_binary(Command) ->
	Json = {struct, [{<<"command">>, Command} | Props]},
	{ok, Json, State};

handle_cast({arbitrary_command, Channel, Command, {struct, Props}}, State) ->
	handle_cast({arbitrary_command, Channel, Command, Props}, State);

handle_cast({arbitrary_command, Channel, Command, Props}, State) when is_binary(Command); is_atom(Command) ->
	case fetch_channel(Channel, State) of
		none ->
			{ok, undefined, State};
		{ChanPid, _ChanData} ->
			Props0 = [{<<"command">>, Command}, {<<"channelid">>, binary_to_list(pid_to_list(ChanPid))} | Props],
			{ok, {struct, Props0}, State}
	end;

handle_cast({mediapush, Chanpid, Call, Data}, State) ->
	{ok, State0} = update_channels(Chanpid, Call, State),
	case Data of
		% because freeswitch, legacy format
		EventName when is_atom(EventName) ->
			Props = [{<<"event">>, EventName}, {<<"media">>, Call#call.type}],
			handle_cast({arbitrary_command, Chanpid, <<"mediaevent">>, Props}, State0);
		% email uses this
		{mediaload, Call} ->
			Props = [{<<"media">>, Call#call.source_module}],
			handle_cast({arbitrary_command, Chanpid, <<"mediaload">>, Props}, State0);
		% freeswitch uses this
		{mediaload, Call, _Data} ->
			Props = [{<<"media">>, Call#call.source_module}],
			handle_cast({arbitrary_commadn, Chanpid, <<"mediaload">>, Props}, State0);
		% not sure what uses this.  It's still pretty messy.
		{Command, Call, EventData} ->
			Props = [{<<"event">>, EventData}, {<<"media">>, Call#call.type}],
			handle_cast({arbitrary_command, Chanpid, Command, Props}, State0);
		% one of two versions I'd like to see in the future
		{struct, Props} when is_list(Props) ->
			handle_cast({arbitrary_command, Chanpid, <<"mediaevent">>, Props}, State0);
		% and the second of the prefered versions
		Props when is_list(Props) ->
			handle_cast({arbitrary_command, Chanpid, <<"mediaevent">>, Props}, State0)
	end;

handle_cast({set_release, Release, Time}, State) ->
	ReleaseData = case Release of
		none ->
			false;
		{Id, Label, Bias} ->
			{struct, [
				{<<"id">>, list_to_binary(Id)},
				{<<"label">>, if is_atom(Label) -> Label; true -> list_to_binary(Label) end},
				{<<"bias">>, Bias}
			]}
	end,
	Json = {struct, [
		{<<"command">>, <<"arelease">>},
		{<<"releaseData">>, ReleaseData},
		{<<"changeTime">>, Time * 1000}
	]},
	{ok, Json, State};

handle_cast({set_channel, Pid, StateName, Statedata}, State) ->
	Headjson = {struct, [
		{<<"command">>, <<"setchannel">>},
		{<<"state">>, StateName},
		{<<"statedata">>, encode_statedata(Statedata)},
		{<<"channelid">>, list_to_binary(pid_to_list(Pid))}
	]},
	{ok, State0} = update_channels(Pid, {StateName, Statedata}, State),
	{ok, Headjson, State0};

handle_cast({channel_died, Pid, NewAvail}, State) ->
	#state{channels = Channels} = State,
	Json = {struct, [
		{<<"command">>, <<"endchannel">>},
		{<<"channelid">>, list_to_binary(pid_to_list(Pid))},
		{<<"availableChannels">>, NewAvail}
	]},
	Channels0 = dict:erase(Pid, Channels),
	State0 = State#state{channels = Channels0},
	{ok, Json, State0};

handle_cast({change_profile, Profile}, State) ->
	Headjson = {struct, [
		{<<"command">>, <<"aprofile">>},
		{<<"profile">>, list_to_binary(Profile)}
	]},
	{ok, Headjson, State};

handle_cast({url_pop, URL, Name}, State) ->
	Headjson = {struct, [
		{<<"command">>, <<"urlpop">>},
		{<<"url">>, list_to_binary(URL)},
		{<<"name">>, list_to_binary(Name)}
	]},
	{ok, Headjson, State};

handle_cast({blab, Text}, State) when is_list(Text) ->
	handle_cast({blab, list_to_binary(Text)}, State);

handle_cast({blab, Text}, State) when is_binary(Text) ->
	Headjson = {struct, [
		{<<"command">>, <<"blab">>},
		{<<"text">>, Text}
	]},
	{ok, Headjson, State};

handle_cast({new_endpoint, _Module, _Endpoint}, State) ->
	%% TODO should likely actually tell the agent.  Maybe.
	{ok, undefined, State};

%% this should actually be implemented as a pure call.  Ah well.
handle_cast(get_tabs_menu, State) ->
	#state{agent = #agent{source = Apid}, connection = Conn} = State,
	spawn_get_tabs_menu(Conn, Apid),
	{ok, undefined, State};

handle_cast(_, State) ->
	{ok, undefined, State}.

spawn_get_tabs_menu(Conn, Apid) ->
	Agent = agent:dump_state(Apid),
	Admin = {<<"Dashboard">>, <<"tabs/dashboard.html">>},
	Endpoints = {<<"Endpoints">>, <<"tabs/endpoints.html">>},
	{ok, HookRes} = cpx_hooks:trigger_hooks(agent_web_tabs, [Agent], all),
	Filtered = [Endpoints | [X || {B1, B2} = X <- HookRes, is_binary(B1), is_binary(B2)]],
	TabsList = case Agent#agent.security_level of
		agent -> Filtered;
		Level when Level =:= admin; Level =:= supervisor ->
			[Admin | Filtered]
	end,
	Tabs = [{struct, [{<<"label">>, Label}, {<<"href">>, Href}]} ||
		{Label, Href} <- TabsList],
	gen_server:cast(Conn, {arbitrary_command, set_tabs_menu, [{<<"tabs">>, Tabs}]}).

%% @doc Encode the given data into a structure suitable for mochijson2:encode
-spec(encode_statedata/1 :: 
	(Callrec :: #call{}) -> json();
	(Clientrec :: #client{}) -> json();
	({'onhold', Holdcall :: #call{}, 'calling', any()}) -> json();
	({Relcode :: string(), Bias :: non_neg_integer()}) -> json();
	('default') -> {'struct', [{binary(), 'default'}]};
	(List :: string()) -> binary();
	({}) -> 'false').
encode_statedata(Callrec) when is_record(Callrec, call) ->
%	case Callrec#call.client of
%		Clientrec when is_record(Clientrec, client) ->
%			Brand = Clientrec#client.label;
%		_ ->
%			Brand = "unknown client"
%	end,
	Clientrec = Callrec#call.client,
	Client = case Clientrec#client.label of
		undefined ->
			<<"unknown client">>;
		Else ->
			list_to_binary(Else)
	end,
	{struct, [
		{<<"callerid">>, list_to_binary(element(1, Callrec#call.callerid) ++ " " ++ element(2, Callrec#call.callerid))},
		{<<"brandname">>, Client},
		{<<"ringpath">>, Callrec#call.ring_path},
		{<<"mediapath">>, Callrec#call.media_path},
		{<<"callid">>, list_to_binary(Callrec#call.id)},
		{<<"source_module">>, Callrec#call.source_module},
		{<<"type">>, Callrec#call.type}]};
encode_statedata(Clientrec) when is_record(Clientrec, client) ->
	Label = case Clientrec#client.label of
		undefined ->
			undefined;
		Else ->
			list_to_binary(Else)
	end,
	{struct, [
		{<<"brandname">>, Label}]};
encode_statedata({onhold, Holdcall, calling, Calling}) ->
	Holdjson = encode_statedata(Holdcall),
	Callingjson = encode_statedata(Calling),
	{struct, [
		{<<"onhold">>, Holdjson},
		{<<"calling">>, Callingjson}]};
encode_statedata({_, default, _}) ->
	{struct, [{<<"reason">>, default}]};
encode_statedata({_, ring_fail, _}) ->
	{struct, [{<<"reason">>, ring_fail}]};
encode_statedata({_, Reason, _}) ->
	{struct, [{<<"reason">>, list_to_binary(Reason)}]};
encode_statedata(List) when is_list(List) ->
	list_to_binary(List);
encode_statedata({}) ->
	false.
