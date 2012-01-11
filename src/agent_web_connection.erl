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

%% @doc Handles the internal (cpx interaction) part of an
%%  agent web connection.
%% 
%% {@web}
%%
%% The listener and connection are designed to be able to function with
%% any ui that adheres to the api.  The api is broken up between the two
%% modules.  {@module} holds the functions that require communication with
%% a specific agent.  For login and utility functions,
%% {@link agent_web_listener}.  
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
%% To make a web api call, make a post request to path "/api" with one
%% field named `"request"'.  The value of the request field should be a 
%% a json object:
%% <pre> {
%% 	"function":  string(),
%% 	"args":      [any()]
%% }</pre>
%% See a functions documentation for what `"args"' should be.
%% 
%% A response will have 3 major forms.  Note that due to legacy reasons 
%% there may be more properties then listed.  They should be ignored, as
%% they will be phased out in favor of the more refined api.
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
%% @see cpx_web_management
-module(agent_web_connection).
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
	start_link/2,
	start/2,
	stop/1,
	api/2,
	dump_agent/1,
	encode_statedata/1,
	set_salt/2,
	poll/2,
	keep_alive/1,
	mediaload/1,
	dump_state/1,
	format_status/2,
	is_web_api/2
]).

%% Web api exports.
%% to make documenting a web api easier, the listener will directly
%% calls these functions.
-export([
	set_state/2,
	set_state/3,
	dial/2,
	get_avail_agents/1,
	agent_transfer/2,
	agent_transfer/3,
	media_command/3,
	media_command/4,
	media_hangup/1,
	load_media/1,
	ring_test/1,
	get_agent_profiles/1,
	get_queue_transfer_options/1,
	warm_transfer/2,
	warm_transfer_complete/1,
	warm_transfer_cancel/1,
	queue_transfer/3,
	init_outbound/3,
	set_endpoint/4,
	logout/1
]).

-web_api_functions([
	{set_state, 2},
	{set_state, 3},
	{dial, 2},
	{get_avail_agents, 1},
	{agent_transfer, 2},
	{agent_transfer, 3},
	{media_command, 3},
	{media_command, 4},
	{media_hangup, 1},
	{load_media, 1},
	{ring_test, 1},
	{get_agent_profiles, 1},
	{get_queue_transfer_options, 1},
	{warm_transfer, 2},
	{warm_transfer_complete, 1},
	{warm_transfer_cancel, 1},
	{queue_transfer, 3},
	{init_outbound, 3},
	{set_endpoint, 4},
	{poll, 2},
	{logout, 1}
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-type(tref() :: any()).

-record(state, {
	salt :: any(),
	agent_fsm :: pid() | 'undefined',
	current_call :: #call{} | 'undefined' | 'expect',
	mediaload :: any(),
	poll_queue = [] :: [{struct, [{binary(), any()}]}],
		% list of json structs to be sent to the client on poll.
	poll_flush_timer :: any(),
	poll_pid :: 'undefined' | pid(),
	poll_pid_established = 1 :: pos_integer(),
	ack_timer :: tref() | 'undefined',
	securitylevel = agent :: 'agent' | 'supervisor' | 'admin',
	listener :: 'undefined' | pid(),
	supervisor_state :: any()
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

-type(json_simple() :: {'struct', [{binary(), binary()}]}).

%%====================================================================
%% WEB API
%%====================================================================

-type(bin_string() :: binary()). % defined to indicate a string in binary

%% @doc {@web} Logs the agent out.  The result is a simple success.
-spec(logout/1 :: (Conn :: pid()) -> any()).
logout(Conn) ->
	gen_server:call(Conn, logout).

%% @doc {@web} Set the agent to the given `Statename' with default state 
%% data.  No result property as it either worked or didn't.
-spec(set_state/2 :: (Conn :: pid(), Statename :: bin_string()) -> any()).
set_state(Conn, Statename) ->
	gen_server:call(Conn, {set_state, binary_to_list(Statename)}).

%% @doc {@web} Set the agent to the given `Statename' with the given 
%% `Statedata'.  No result property as it either worked or it didn't.  
%% State data will vary based on state.  For released, it can be either 
%% the string `"Default"' or a string of `"Id:Name:Bias"'.
-spec(set_state/3 :: (Conn :: pid(), Statename :: bin_string(), Statedata :: any()) -> any()).
set_state(Conn, Statename, Statedata) ->
	gen_server:call(Conn, {set_state, binary_to_list(Statename), binary_to_list(Statedata)}).

%% @doc {@web} Attempt to dial the passed number.  Implicitly sets the 
%% agent from precall to outbound.  No results property as it either 
%% worked or didn't.
-spec(dial/2 :: (Conn :: pid(), Number :: bin_string()) -> any()).
dial(Conn, Number) ->
	gen_server:call(Conn, {dial, binary_to_list(Number)}).

%% @doc {@web} Get a list of the agents that are currently available.  
%% Result is:
%% <pre>[{
%% 	"name":  string(),
%% 	"profile":  string(),
%% 	"state":  "idle" | "released"
%% }]</pre>
-spec(get_avail_agents/1 :: (Conn :: pid()) -> any()).
get_avail_agents(Conn) ->
	gen_server:call(Conn, get_avail_agents).

%% @doc {@web} Get a list of the profiles that are in the system.  Result 
%% is:
%% <pre>[{
%% 	"name":  string(),
%% 	"order":  number()
%% }]</pre>
-spec(get_agent_profiles/1 :: (Conn :: pid()) -> any()).
get_agent_profiles(Conn) ->
	gen_server:call(Conn, {undefined, "/profilelist"}).

%% @doc {@web} Transfer the call to the given `Agent' login name.  No 
%% result is sent back as it's a simple success or failure.
-spec(agent_transfer/2 :: (Conn :: pid(), Agent :: bin_string()) -> any()).
agent_transfer(Conn, Agent) ->
	gen_server:call(Conn, {agent_transfer, binary_to_list(Agent)}).

%% @doc {@web} Transfer the call to the given `Agent' and associate the 
%% media with the given `Caseid'.  No result is sent back as it's a simple 
%% success or failure.
-spec(agent_transfer/3 :: (Conn :: pid(), Agent :: bin_string(), Caseid :: bin_string()) -> any()).
agent_transfer(Conn, Agent, Caseid) ->
	gen_server:call(Conn, {agent_transfer, binary_to_list(Agent), binary_to_list(Caseid)}).

%% @doc {@web} Forward a command or request to the media associated with 
%% an oncall agent.  `Command' is the name of the request to make.  `Mode' 
%% is either `"call"' or `"cast"'.  `"call"' indicates an indepth reply is 
%% expected from the media.  `"cast"' indicates no meaningful reply is 
%% expected, so as long as the command was sent, success is returned.  
%% `Args' is any arguments to sent with the `Command'.  In the case of 
%% `"cast"' mode, there is no result as it's a simple succcess.  Check the 
%% documentation of the media modules to see what possible returns there 
%% are.
-spec(media_command/4 :: (Conn :: pid(), Command :: bin_string(), Mode :: bin_string(), Args :: [any()]) -> any()).
media_command(Conn, Command, Mode, Args) ->
	Post = [
		{"command", Command},
		{"mode", binary_to_list(Mode)},
		{"args", Args}
	],
	gen_server:call(Conn, {media, Post}).

%% @doc {@web} media_command with an empty argument list.
-spec(media_command/3 :: (Conn :: pid(), Command :: bin_string(), Mode :: bin_string()) -> any()).
media_command(Conn, Command, Mode) ->
	media_command(Conn, Command, Mode, []).

%% @doc {@web} Start a warmtransfer of the media associated with the 
%% oncall agent to `Number'.  No result is sent back as it's simply 
%% success or failure.
-spec(warm_transfer/2 :: (Conn :: pid(), Number :: bin_string()) -> any()).
warm_transfer(Conn, Number) ->
	gen_server:call(Conn, {warm_transfer, binary_to_list(Number)}).

%% @doc {@web} Complete a started transfer; implicitly moves the agent to 
%% wrapup.  No result is set as it's a simple success or failure.
-spec(warm_transfer_complete/1 :: (Conn :: pid()) -> any()).
warm_transfer_complete(Conn) ->
	gen_server:call(Conn, warm_transfer_complete).

%% @doc {@web} Cancel a started transfer, implicitly putting the agent 
%% oncall.  No result is set as it's a simple success or failure.
-spec(warm_transfer_cancel/1 :: (Conn :: pid()) -> any()).
warm_transfer_cancel(Conn) ->
	gen_server:call(Conn, warm_transfer_cancel).

%% @doc {@web} Get the fields and skills an agent can assign to a media 
%% before transfering it back into queue.  Result:
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
-spec(get_queue_transfer_options/1 :: (Conn :: pid()) -> any()).
get_queue_transfer_options(Conn) ->
	gen_server:call(Conn, {undefined, "/get_queue_transfer_options"}).

%% @doc {@web} Force the agent to disconnect the media; usually through a 
%% brutal %% kill of the media pid.  Best used as an emergency escape 
%% hatch, and not under normal call flow.  No result set as it's merely 
%% success or failure.
-spec(media_hangup/1 :: (Conn :: pid()) -> any()).
media_hangup(Conn) ->
	gen_server:call(Conn, {undefined, "/call_hangup"}).

%% @doc {@web} Test if freeswitch can ring an agent's softphone.  No 
%% result set as it's either a success or false.  The true success is if 
%% the agent's phone rings.
-spec(ring_test/1 :: (Conn :: pid()) -> any()).
ring_test(Conn) ->
	gen_server:call(Conn, {undefined, "/ringtest"}).

%% @doc {@web} Transfer the call the agent is in into `Queue' with the 
%% given `Opts'.  The options is a json object with any number of 
%% properties that are passed to the media.  If there is a property 
%% `"skills"' with a list, the list is interpreted as a set of skills to 
%% apply to the media.  No result is set as it is merely success or 
%% failure.
-spec(queue_transfer/3 :: (Conn :: pid(), Queue :: bin_string(), {struct, Opts :: [{bin_string(), any()}]}) -> any()).
queue_transfer(Conn, Queue, {struct, Opts}) ->
	FixedOpts1 = [case Key of
		<<"skills">> ->
			{"skills", Val};
		_ ->
			{binary_to_list(Key), binary_to_list(Val)}
	end || {Key, Val} <- Opts],
	RawSkills = proplists:get_value("skills", FixedOpts1),
	FixedOpts2 = proplists:delete("skills", FixedOpts1),
	FixedSkills = [case S of
		{struct, ExpandProp} ->
			Atom = binary_to_list(proplists:get_value(<<"atom">>, ExpandProp)),
			Expanded = binary_to_list(proplists:get_value(<<"expanded">>,ExpandProp)),
			{"skills", "{" ++ Atom ++ "," ++ Expanded ++ "}"};
		Atom when is_binary(Atom) ->
			{"skills", binary_to_list(Atom)}
	end || S <- RawSkills],
	FixedOpts = FixedOpts2 ++ FixedSkills ++ [{"queue", binary_to_list(Queue)}],
	gen_server:call(Conn, {undefined, "/queue_transfer", FixedOpts}).

%% @doc {@web} Set and agent to precall for a new media for `Client' 
%% calling to `Type'.  Currently only `Type' of `"freeswitch"' is allowed.
%% There is no result set as it's only a success or failure message.
-spec(init_outbound/3 :: (Conn :: pid(), Client :: bin_string(), Type :: bin_string()) -> any()).
init_outbound(Conn, Client, Type) ->
	gen_server:call(Conn, {init_outbound, binary_to_list(Client), binary_to_list(Type)}).

%% @doc {@web} Sets the agent's endpoint data to the given, well, data.
%% Particularly useful if the flash phone is used, as all of the connection
%% data will not be available for that until it is started on in the 
%% browser.
-spec(set_endpoint/4 :: (Conn :: pid(), Endpoint :: bin_string(), Data :: bin_string(), Persist :: 'true' | 'false') -> any()).
set_endpoint(Conn, Endpoint, Data, Persist) ->
	case catch list_to_existing_atom(binary_to_list(Endpoint)) of
		{'EXIT', {badarg, _}} ->
			{200, [], mochijson2:encode({struct, [{success, false},{<<"message">>, <<"invalid endpoint type">>}, {<<"errcode">>, <<"INVALID_ENDPOINT_TYPE">>}]})};
		EndpointType ->
			EndpointData = binary_to_list(Data),
			Persisty = case Persist of
				true -> persistent;
				false -> transient
			end,
			case gen_server:call(Conn, {set_endpoint, EndpointType, EndpointData, Persisty}) of
				ok -> {200, [], mochijson2:encode({struct, [{success, true}]})};
				{ok, _Pid} -> {200, [], mochijson2:encode({stuct, [{success, true}]})};
				{error, Error} ->
					{200, [], mochijson2:encode({struct, [
						{success, false},
						{<<"message">>, list_to_binary(io_lib:format("Error setting endpoint:  ~p", [Error]))},
						{<<"errcode">>, <<"INVALID_ENDPOINT">>}
					]})}
			end
	end.

%% @doc {@web} If the media set anything to be loaded at call start, 
%% retreive it.  This is useful if the client (such as web browser) needs 
%% to refresh the page or crashes, but is able to recover before the 
%% automatic logout occurs.  Results will vary from media to media.
-spec(load_media/1 :: (Conn :: pid()) -> any()).
load_media(Conn) ->
	gen_server:call(Conn, mediaload).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Description: Starts the server
%%--------------------------------------------------------------------

%% @doc Get the list of funcions/arities exposed for web use.
-spec(get_web_api/0 :: () -> [{atom(), non_neg_integer()}]).
get_web_api() ->
	Attrs = ?MODULE:module_info(attributes),
	proplists:get_value(web_api_functions, Attrs).

-spec(is_web_api/2 :: (Func :: atom(), Arity :: non_neg_integer()) -> boolean()).
is_web_api(Func, Arity) ->
	Api = get_web_api(),
	lists:member({Func, Arity}, Api).

%% @doc Starts the passed agent at the given security level.
-spec(start_link/2 :: (Agent :: #agent{}, Security :: security_level()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start_link(Agent, Security) ->
	gen_server:start_link(?MODULE, [Agent, Security], [{timeout, 10000}]).

%% @doc Starts the passed agent at the given security level.
-spec(start/2 :: (Agent :: #agent{}, Security :: security_level()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start(Agent, Security) ->
	gen_server:start(?MODULE, [Agent, Security], [{timeout, 10000}]).

%% @doc Stops the passed Web connection process.
-spec(stop/1 :: (Pid :: pid()) -> 'ok').
stop(Pid) ->
	gen_server:call(Pid, stop).

%% @doc {@web} Register Frompid as the poll_pid; or in the case of the web
%% api, establish a connection to await events.  As soon as a 
%% {@link agent_web_connection:login/4} completes successfully, the client
%% should make a call to the path `"/poll"', or a standard api call with
%% a function of `"poll"'.  No args needed.  If the status code is 408, it
%% means a new poll was started without finishing the current one.
%%
%% A result is a list of json object with at least one property `"command"'.
%% Common commands are:
%% <table>
%% 	<tr>
%% 		<th>Command</th>
%% 		<th>Other Properties</th>
%% 		<th>Description</th>
%% 	</tr>
%% 	<tr>
%% 		<td>pong</td>
%% 		<td>"timestamp":  integer()</td>
%% 		<td>Nothing has happened in the last 20 seconds, but we don't want
%% 		the conneciton to just die.  Sync up clocks based on what that
%% 		timestamp from the server said if needed.  Start a new poll.</td>
%% 	</tr>
%% 	<tr>
%% 		<td>astate</td>
%% 		<td><ul>
%% 			<li>"state":  string()</li>
%% 			<li>"statedata":  object(); optional</li>
%% 		</ul></td>
%% 		<td>The agent fsm has changed state, and so should the agent ui.
%% 		The statedata will vary wildly based on the state.  Released will
%% 		have the reason and bias; oncall, wrapup, and ringing will have
%% 		a mass of call data; idle will have no state data.</td>
%%	</tr>
%% 	<tr>
%% 		<td>aprofile</td>
%% 		<td><ul>
%% 			<li>"profile":  string()</li>
%% 		</ul></td>
%% 		<td>The agent has been moved to a new profile.</td>
%% 	</tr>
%% 	<tr>
%% 		<td>urlpop</td>
%% 		<td><ul>
%% 			<li>"url":  string()</li>
%% 			<li>"name":  string()</li>
%% 		</ul></td>
%% 		<td>Open the url in the named view.  If the view exists, re-use 
%% 		ditching what is there.</td>
%% 	</tr>
%% 	<tr>
%% 		<td>blab</td>
%% 		<td><ul>
%% 			<li>"text":  string()</li>
%% 		</ul></td>
%% 		<td>A supervisor has sent a message to the agent.  Text is the 
%% 		message.  A simple dialog box will suffice.</td>
%% 	</tr>
%% 	<tr>
%% 		<td>mediaload</td>
%%		<td><ul>
%% 			<li>"media":  string()</li>
%% 			<li>"fullpane":  boolean()</li>
%% 		</ul></td>
%% 		<td>Allows the media to have the client attempt to load extra 
%% 		Information from the media.  `"fullpane"' indicates if the window
%% 		Or information panel the media is requesting be opened is to be
%% 		as large as possible or simply a smaller window.  Defaults to 
%% 		`true'.</td>
%% 	</tr>
%% 	<tr>
%% 		<td>mediaevent</td>
%% 		<td><ul>
%% 			<li>"media":  string()</li>
%% 			<li>"event":  string()</li>
%% 		</ul></td>
%% 		<td>A media is able to send generic events to an agent interface,
%% 		and this is the final result.  The media will likely add more 
%% 		properties.  No response is expected from the client.</td>
%% 	</tr>
%% </table>
-spec(poll/2 :: (Pid :: pid(), Frompid :: pid()) -> 'ok').
poll(Pid, Frompid) ->
	gen_server:cast(Pid, {poll, Frompid}).

%% @doc Do a web api call.
-spec(api/2 :: (Pid :: pid(), Apicall :: any()) -> any()).
api(Pid, Apicall) ->
	gen_server:call(Pid, Apicall).

%% @doc Dump the state of agent associated with the passed connection.
-spec(dump_agent/1 :: (Pid :: pid()) -> {#agent{}, 'agent' | 'supervisor' | 'admin'}).
dump_agent(Pid) ->
	gen_server:call(Pid, dump_agent).

%% @doc Sets the salt.  Hmmm, salt....
-spec(set_salt/2 :: (Pid :: pid(), Salt :: any()) -> 'ok').
set_salt(Pid, Salt) ->
	gen_server:cast(Pid, {set_salt, Salt}).

%% @doc keep alive, keep alive.
-spec(keep_alive/1 :: (Pid :: pid()) -> 'ok').
keep_alive(Pid) ->
	gen_server:cast(Pid, keep_alive).

%% @doc Get the settings used for a media load.  Only useful for the web
%% listener, and then only useful in the checkcookie clause.
-spec(mediaload/1 :: (Conn :: pid()) -> [{any(), any()}] | 'undefined').
mediaload(Conn) ->
	gen_server:call(Conn, mediaload).

-spec(dump_state/1 :: (Conn :: pid) -> #agent{}).
dump_state(Conn) ->
	gen_server:call(Conn, dump_state).

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

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Agent, Security]) ->
	?DEBUG("web_connection init ~p with security ~w", [Agent, Security]),
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
			Tref = erlang:send_after(?TICK_LENGTH, self(), check_live_poll),
			agent_web_listener:linkto(self()),
			State = agent:dump_state(Apid),
			CurrentCall = case State#agent.statedata of
				Call when is_record(Call, call) ->
					Call;
				{on_hold, Call, calling, _Number} ->
					Call;
				_ ->
					undefined
			end,

%			case Security of
%				agent ->
%					ok;
%				supervisor ->
%					cpx_monitor:subscribe();
%				admin ->
%					cpx_monitor:subscribe()
%			end,
			{ok, #state{agent_fsm = Apid, current_call = CurrentCall, ack_timer = Tref, securitylevel = Security, listener = whereis(agent_web_listener)}}
	end.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
	{stop, shutdown, ok, State};
handle_call(logout, _From, State) ->
	{stop, normal, {200, [{"Set-Cookie", "cpx_id=dead"}], mochijson2:encode({struct, [{success, true}]})}, State};
handle_call(get_avail_agents, _From, State) ->
	Agents = [AgState || {_K, {Pid, _Id, _Time, _Skills}} <-
		agent_manager:list(),
		AgState <- [agent:dump_state(Pid)],
		AgState#agent.state == idle orelse AgState#agent.state == released],

	Noms = [{struct, [{<<"name">>, list_to_binary(Rec#agent.login)}, {<<"profile">>, list_to_binary(Rec#agent.profile)}, {<<"state">>, Rec#agent.state}]} || Rec <- Agents],
	{reply, {200, [], mochijson2:encode({struct, [{success, true}, {<<"agents">>, Noms}, {<<"result">>, Noms}]})}, State};
handle_call({set_state, Statename}, _From, #state{agent_fsm = Apid} = State) ->
	case agent:set_state(Apid, agent:list_to_state(Statename)) of
		ok ->
			{reply, {200, [], mochijson2:encode({struct, [{success, true}, {<<"status">>, ok}]})}, State};
		invalid ->
			{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"status">>, invalid}, {<<"message">>, <<"invalid state change">>}, {<<"errcode">>, <<"INVALID_STATE_CHANGE">>}]})}, State}
	end;
handle_call({set_state, Statename, InStatedata}, _From, #state{agent_fsm = Apid} = State) ->
	Statedata = case Statename of
		"released" ->
			case InStatedata of
				"Default" ->
					default;
				_ ->
					[Id, Name, Bias] = util:string_split(InStatedata, ":"),
					{Id, Name, list_to_integer(Bias)}
			end;
		_ ->
			InStatedata
	end,
	case agent:set_state(Apid, agent:list_to_state(Statename), Statedata) of
		invalid ->
			{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"status">>, invalid}, {<<"message">>, <<"invalid state change">>}, {<<"errcode">>, <<"INVALID_STATE_CHANGE">>}]})}, State}; 
		Status -> 
			{reply, {200, [], mochijson2:encode({struct, [{success, true}, {<<"status">>, Status}]})}, State} 
	end; 
handle_call({set_endpoint, Endpoint, Data, Persist}, _From, #state{agent_fsm = Apid} = State) -> 
	{reply, agent:set_endpoint(Apid, Endpoint, Data, Persist), State};
handle_call({dial, Number}, _From, #state{agent_fsm = AgentPid} = State) ->
	AgentRec = agent:dump_state(AgentPid),
	case AgentRec#agent.state of
		precall ->
			#agent{statedata = Call} = AgentRec,
			case Call#call.direction of
				outbound ->
					case gen_media:call(Call#call.source, {dial, Number}) of
						ok ->
							{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State};
						{error, Error} ->
							?NOTICE("Outbound call error ~p", [Error]),
							{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, list_to_binary(lists:flatten(io_lib:format("~p, Check your phone configuration", [Error])))}, {<<"errcode">>, <<"UNKNOWN_ERROR">>}]})}, State}
					end;
				_ ->
					{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"This is not an outbound call">>}, {<<"errcode">>, <<"INVALID_STATE_CHANGE">>}]})}, State}
			end;
		_ ->
			{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Agent is not in pre-call">>}, {<<"errcode">>, <<"INVALID_STATE_CHANGE">>}]})}, State}
	end;
handle_call(dump_agent, _From, #state{agent_fsm = Apid} = State) ->
	Astate = agent:dump_state(Apid),
	{reply, {Astate, State#state.securitylevel}, State};
handle_call({agent_transfer, Agentname, CaseID}, From, #state{current_call = Call} = State) when is_record(Call, call) ->
	gen_media:cast(Call#call.source, {set_caseid, CaseID}),
	handle_call({agent_transfer, Agentname}, From, State);
handle_call({agent_transfer, Agentname}, _From, #state{agent_fsm = Apid} = State) ->
	case agent_manager:query_agent(Agentname) of
		{true, Target} ->
			Reply = case agent:agent_transfer(Apid, Target) of
				ok ->
					{200, [], mochijson2:encode({struct, [{success, true}]})};
				invalid ->
					{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Could not start transfer">>}, {<<"errcode">>, <<"INVALID_STATE_CHANGE">>}]})}
			end,
			{reply, Reply, State};
		false ->
			{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Agent not found">>}, {<<"errcode">>, <<"AGENT_NOEXISTS">>}]})}, State}
	 end;
handle_call({warm_transfer, Number}, _From, #state{current_call = Call} = State) when is_record(Call, call) ->
	?NOTICE("warm transfer to ~p", [Number]),
	Reply = case gen_media:warm_transfer_begin(Call#call.source, Number) of
		ok ->
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		invalid ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Could not start transfer">>}, {<<"errcode">>, <<"INVALID_MEDIA_CALL">>}]})}
	end,
	{reply, Reply, State};
handle_call(warm_transfer_cancel, _From, #state{current_call = Call} = State) when is_record(Call, call) ->
	?NOTICE("warm transfer cancel", []),
	Reply = case gen_media:warm_transfer_cancel(Call#call.source) of
		ok ->
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		invalid ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Could not cancel transfer">>}, {<<"errcode">>, <<"INVALID_MEDIA_CALL">>}]})}
	end,
	{reply, Reply, State};
handle_call(warm_transfer_complete, _From, #state{current_call = Call} = State) when is_record(Call, call) ->
	?NOTICE("warm transfer complete", []),
	Reply = case gen_media:warm_transfer_complete(Call#call.source) of
		ok ->
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		invalid ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Could not complete transfer">>}, {<<"errcode">>, <<"INVALID_MEDIA_CALL">>}]})}
	end,
	{reply, Reply, State};
handle_call({init_outbound, Client, Type}, _From, #state{agent_fsm = Apid} = State) ->
	?NOTICE("Request to initiate outbound call of type ~p to ~p", [Type, Client]),
	AgentRec = agent:dump_state(Apid), % TODO - avoid
	Reply = case AgentRec#agent.state of
		Agentstate when Agentstate =:= released; Agentstate =:= idle ->
			try list_to_existing_atom(Type) of
				freeswitch ->
					case whereis(freeswitch_media_manager) of
						P when is_pid(P) ->
							case freeswitch_media_manager:make_outbound_call(Client, Apid, AgentRec#agent.login) of
								{ok, Pid} ->
									Call = gen_media:get_call(Pid),
									agent:set_state(Apid, precall, Call),
									{200, [], mochijson2:encode({struct, [{success, true}]})};
								{error, Reason} ->
									{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, list_to_binary(io_lib:format("Initializing outbound call failed (~p)", [Reason]))}, {<<"errcode">>, <<"UNKNOWN_ERROR">>}]})} 
							end;
						 _ ->
							{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"freeswitch is not available">>}, {<<"errcode">>, <<"MEDIA_NOEXISTS">>}]})}
					end;
				% TODO - more outbound types go here :)
				_ ->
					{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Unknown call type">>}, {<<"errcode">>, <<"MEDIA_NOEXISTS">>}]})}
			catch
				_:_ ->
					{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Unknown call type">>}, {<<"errcode">>, <<"MEDIA_NOEXISTS">>}]})}
			end;
		_ ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Agent must be released or idle">>}, {<<"errcode">>, <<"INVALID_STATE_CHANGE">>}]})}
	end,
	{reply, Reply, State};

% TODO supervisor handling was here.  Forward requests to 
% supervisor_web_connection module for teh happy.

handle_call({media, Post}, _From, #state{current_call = Call} = State) when is_record(Call, call) ->
	Commande = proplists:get_value("command", Post),
	?DEBUG("Media Command:  ~p", [Commande]),
	case proplists:get_value("mode", Post) of
		"call" ->
			{Heads, Data} = try gen_media:call(Call#call.source, {Commande, Post}) of
				invalid ->
					?DEBUG("agent:media_call returned invalid", []),
					{[], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"invalid media call">>}, {<<"errcode">>, <<"INVALID_MEDIA_CALL">>}]})}; 
				Response -> 
					parse_media_call(Call, {Commande, Post}, Response)
			catch
				exit:{noproc, _} ->
					?DEBUG("Media no longer exists.", []),
					{[], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"media no longer exists">>}, {<<"errcode">>, <<"MEDIA_NOEXISTS">>}]})}
			end,
			{reply, {200, Heads, Data}, State};
		"cast" ->
			gen_media:cast(Call#call.source, {Commande, Post}),
			{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State};
		undefined ->
			{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"no mode defined">>}, {<<"errcode">>, <<"BAD_REQUEST">>}]})}, State}
	end;
handle_call({undefined, "/get_queue_transfer_options"}, _From, #state{current_call = Call} = State) when is_record(Call, call) ->
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
		{<<"success">>, true},
		{<<"currentVars">>, {struct, Varslist}},
		{<<"prompts">>, Encodedprompts},
		{<<"skills">>, Encodedskills},
		{<<"result">>, {struct, [
			{<<"currentVars">>, {struct, Varslist}},
			{<<"prompts">>, Encodedprompts},
			{<<"skills">>, Encodedskills}
		]}}
	]},
	{reply, {200, [], mochijson2:encode(Json)}, State};
handle_call({undefined, "/get_queue_transfer_options"}, _From, State) ->
	{reply, ?reply_err(<<"Not in a call">>, <<"INVALID_STATE_CHANGE">>), State};
handle_call({undefined, "/call_hangup"}, _From, #state{current_call = Call} = State) when is_record(Call, call) ->
	Call#call.source ! call_hangup,
	?DEBUG("The agent is committing call murder!", []),
	Json = case agent:set_state(State#state.agent_fsm, {wrapup, State#state.current_call}) of
		invalid ->
			{struct, [{success, false}, {<<"message">>, <<"agent refused statechange">>}, {<<"errcode">>, <<"INVALID_STATE_CHANGE">>}]};
		ok ->
			{struct, [{success, true}, {<<"message">>, <<"agent accepted statechange">>}]}
	end,
	{reply, {200, [], mochijson2:encode(Json)}, State};
handle_call({undefined, "/ringtest"}, _From, #state{current_call = undefined, agent_fsm = Apid} = State) ->
	AgentRec = agent:dump_state(Apid), % TODO - avoid
	Json = case cpx:get_env(ring_manager) of
		{ok, Module} when AgentRec#agent.state == released ->
			case Module:ring_agent_echo(Apid, AgentRec, undefined, 60000) of
				{ok, _} ->
					{struct, [{success, true}]};
				{error, Error} ->
					{struct, [{success, false}, {<<"message">>, iolist_to_binary(io_lib:format("ring test failed: ~p", [Error]))}, {<<"errcode">>, <<"UNKNOWN_ERROR">>}]}
			end;
		undefined ->
			{struct, [{success, false}, {<<"message">>, <<"no ring manager available">>}, {<<"errcode">>, <<"MEDIA_NOEXISTS">>}]};
		_ ->
			{struct, [{success, false}, {<<"message">>, <<"you must be released to perform a ring test">>}, {<<"errcode">>, <<"INVALID_STATE">>}]} 
	end, 
	{reply, {200, [], mochijson2:encode(Json)}, State};
handle_call({undefined, "/queue_transfer", Opts}, _From, #state{current_call = Call, agent_fsm = Apid} = State) when is_record(Call, call) ->
	Queue = proplists:get_value("queue", Opts),
	MidSkills = proplists:get_all_values("skills", Opts),
	Midopts = proplists:delete("skills", proplists:delete("queue", Opts)),
	gen_media:set_url_getvars(Call#call.source, Midopts),
	Skills = cpx_web_management:parse_posted_skills(MidSkills),
	gen_media:add_skills(Call#call.source, Skills),
	?NOTICE("queue transfer to ~p", [Queue]),
	?DEBUG("options:  ~p", [Opts]),
	Reply = case agent:queue_transfer(Apid, Queue) of
		ok ->
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		invalid ->
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Could not start transfer">>}, {<<"errcode">>, <<"INVALID_STATE_CHANGE">>}]})}
	end,
	{reply, Reply, State};
handle_call({undefined, "/report_issue", Post}, _From, State) ->
	Summary = proplists:get_value("reportIssueSummary", Post),
	Description = proplists:get_value("reportIssueError", Post),
	Reproduce = proplists:get_value("reportIssueReproduce", Post),
	Humandetails = proplists:get_value("reportIssueDetails", Post),
	Uidetails = proplists:get_value("uistate", Post),
	Details = list_to_binary([Humandetails, <<"\n==== automatically gathered ====\n">>, Uidetails]),
	case cpx_supervisor:submit_bug_report(list_to_binary(Summary), list_to_binary(Description), list_to_binary(Reproduce), Details) of
		ok ->
			{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State};
		{error, Err} ->
			Json = {struct, [
				{success, false},
				{<<"message">>, list_to_binary(io_lib:format("~p", [Err]))}
			]},
			{reply, {200, [], mochijson2:encode(Json)}, State}
	end;
handle_call({undefined, "/profilelist"}, _From, State) ->
	Profiles = agent_auth:get_profiles(),
	% TODO finish off the filtering.
%	#agent{profile = Myprof} = agent:dump_state(State#state.agent_fsm);
%	Filter = fun(#agent_profile{options = Opts} = Prof) ->
%		if
%			proplists:get_value(hidden, Opts) ->
%				false;
%			proplists:get_value(isolated, Opts, false) andalso (Prof#agent_profile.name =/= Myprof) ->
%				false;
	
	Jsons = [
		{struct, [{<<"name">>, list_to_binary(Name)}, {<<"order">>, Order}]} ||
		#agent_profile{name = Name, order = Order} <- Profiles
	],
	R = {200, [], mochijson2:encode({struct, [{success, true}, {<<"profiles">>, Jsons}, {<<"result">>, Jsons}]})},
	{reply, R, State};	
handle_call({undefined, [$/ | Path]}, From, State) ->
	handle_call({undefined, [$/ | Path], []}, From, State);
handle_call({undefined, [$/ | Path], Post}, _From, #state{current_call = Call} = State) when is_record(Call, call) ->
	%% considering how things have gone, the best guess is this is a media call.
	%% Note that the results below are only for email, so this will need
	%% to be refactored when we support more medias.
	?DEBUG("forwarding request to media.  Path: ~p; Post: ~p", [Path, Post]),
	try gen_media:call(Call#call.source, {get_blind, Path}) of
		{ok, Mime} ->
			{Heads, Data} = parse_media_call(Call, {"get_path", Path}, {ok, Mime}),
%			Body = element(5, Mime),
%			{reply, {200, [], list_to_binary(Body)}, State};
			{reply, {200, Heads, Data}, State};
		none ->
			{reply, {404, [], <<"path not found">>}, State};
		{message, Mime} ->
			Filename = case email_media:get_disposition(Mime) of
				inline ->
					util:bin_to_hexstr(erlang:md5(erlang:ref_to_list(make_ref())));
				{_, Nom} ->
					binary_to_list(Nom)
			end,
			Heads = [
				{"Content-Disposition", lists:flatten(io_lib:format("attachment; filename=\"~s\"", [Filename]))},
				{"Content-Type", lists:append([binary_to_list(element(1, Mime)), "/", binary_to_list(element(2, Mime))])}
			],
			Encoded = mimemail:encode(Mime),
			{reply, {200, Heads, Encoded}, State};
		Else ->
			?INFO("Not a mime tuple ~p", [Else]),
			{reply, {404, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"unparsable reply">>}, {<<"errcode">>, <<"BAD_MEDIA_RESPONSE">>}]})}, State}
	catch
		exit:{noproc, _} ->
			?ERROR("request to fetch ~p from ~p ~p by ~p", [Path, Call#call.id, Call#call.source, State#state.agent_fsm]),
			{reply, {404, [], <<"path not found">>}, State}
	end;
handle_call(mediaload, _From, State) ->
	{reply, State#state.mediaload, State};
handle_call(dump_state, _From, State) ->
	{reply, State, State};
handle_call({supervisor, _Request}, _From, #state{securitylevel = agent} = State) ->
	{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"insuffienct privledges">>}, {<<"errcode">>, <<"ACCESS_DENIED">>}]})}, State};
handle_call({supervisor, Request}, From, #state{supervisor_state = undefined} = State) ->
	Agent = agent:dump_state(State#state.agent_fsm),
	{ok, SupState} = supervisor_web_connection:init([
		{login, Agent#agent.login},
		{endpointtype, Agent#agent.endpointtype},
		{endpointdata, Agent#agent.endpointdata},
		{ring_path, Agent#agent.defaultringpath}
	]),
	NewState = State#state{supervisor_state = SupState},
	handle_call({supervisor, Request}, From, NewState);
handle_call({supervisor, Request}, From, #state{supervisor_state = SupState} = State) ->
	{reply, Out, NewSupState} = supervisor_web_connection:handle_call({supervisor, Request}, From, SupState),
	{reply, Out, State#state{supervisor_state = NewSupState}};
handle_call(Allothers, _From, State) ->
	?DEBUG("unknown call ~p", [Allothers]),
	{reply, {404, [], <<"unknown_call">>}, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
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
			Json2 = {struct, [{success, true}, {message, <<"Poll successful">>}, {data, lists:reverse(Pollq)}, {<<"result">>, lists:reverse(Pollq)}]},
			Frompid ! {poll, {200, [], mochijson2:encode(Json2)}},
			{noreply, Newstate}
	end;
handle_cast({mediaload, #call{type = email} = Call}, State) ->
	Midstate = case State#state.current_call of
		expect ->
			State#state{current_call = Call};
		_ ->
			State
	end,
	Json = {struct, [
		{<<"command">>, <<"mediaload">>},
		{<<"media">>, <<"email">>}
	]},
	Newstate = push_event(Json, Midstate),
	{noreply, Newstate#state{mediaload = []}};
handle_cast({mediaload, #call{type = voice}}, State) ->
	Json = {struct, [
		{<<"command">>, <<"mediaload">>},
		{<<"media">>, <<"voice">>},
		{<<"fullpane">>, false}
	]},
	Newstate = push_event(Json, State),
	{noreply, Newstate#state{mediaload = [{<<"fullpane">>, false}]}};
handle_cast({mediaload, #call{type = voice}, Options}, State) ->
	Base = [
		{<<"command">>, <<"mediaload">>},
		{<<"media">>, <<"voice">>},
		{<<"fullpane">>, false}
	],
	Json = {struct, lists:append(Base, Options)},
	Newstate = push_event(Json, State),
	{noreply, Newstate#state{mediaload = [{<<"fullpane">>, false} | Options]}};
handle_cast({mediaload, #call{type = voicemail}, Options}, State) ->
	Base = [
		{<<"command">>, <<"mediaload">>},
		{<<"media">>, <<"voicemail">>},
		{<<"fullpane">>, false}
	],
	Json = {struct, lists:append(Base, Options)},
	Newstate = push_event(Json, State),
	{noreply, Newstate#state{mediaload = [{<<"fullpane">>, false} | Options]}};
handle_cast({mediapush, #call{type = Mediatype}, Data}, State) ->
	?DEBUG("mediapush type:  ~p;  Data:  ~p", [Mediatype, Data]),
	case Mediatype of
		email ->
			case Data of
				send_done ->
					Json = {struct, [
						{<<"command">>, <<"mediaevent">>},
						{<<"media">>, email},
						{<<"event">>, <<"send_complete">>},
						{<<"success">>, true}
					]},
					Newstate = push_event(Json, State),
					{noreply, Newstate};
				{send_fail, Error} ->
					Json = {struct, [
						{<<"command">>, <<"mediaevent">>},
						{<<"media">>, email},
						{<<"event">>, <<"send_complete">>},
						{<<"message">>, list_to_binary(io_lib:format("~p", [Error]))},
						{<<"success">>, false}
					]},
					Newstate = push_event(Json, State),
					{noreply, Newstate};
				_Else ->
					?INFO("No other data's supported:  ~p", [Data]),
					{noreply, State}
			end;
		voice ->
			case Data of
				SimpleCommand when is_atom(SimpleCommand) ->
					Json = {struct, [
						{<<"command">>, <<"mediaevent">>},
						{<<"media">>, voice},
						{<<"event">>, SimpleCommand}
					]},
					Newstate = push_event(Json, State),
					{noreply, Newstate}
			end;
		Else ->
			?INFO("Currently no for media pushings: ~p", [Else]),
			{noreply, State}
	end;
handle_cast({set_salt, Salt}, State) ->
	{noreply, State#state{salt = Salt}};
handle_cast({change_state, AgState, Data}, State) ->
	%?DEBUG("State:  ~p; Data:  ~p", [AgState, Data]),
	Headjson = {struct, [
		{<<"command">>, <<"astate">>},
		{<<"state">>, AgState},
		{<<"statedata">>, encode_statedata(Data)}
	]},
	Newstate = push_event(Headjson, State),
	{noreply, Midstate} = case Data of
		Call when is_record(Call, call) ->
			{noreply, Newstate#state{current_call = Call}};
		{onhold, Call, calling, _Number} ->
			{noreply, Newstate#state{current_call = Call}};
		_ ->
			{noreply, Newstate#state{current_call = undefined}}
	end,
	Fullstate = case AgState of
		wrapup ->
			Midstate#state{mediaload = undefined};
		_ ->
			Midstate
	end,
	{noreply, Fullstate};
handle_cast({change_state, AgState}, State) ->
	Headjson = {struct, [
			{<<"command">>, <<"astate">>},
			{<<"state">>, AgState}
		]},
	Midstate = push_event(Headjson, State),
	Newstate = case AgState of
		wrapup ->
			Midstate#state{mediaload = undefined};
		_ ->
			Midstate
	end,
	{noreply, Newstate#state{current_call = undefined}};
handle_cast({change_profile, Profile}, State) ->
	Headjson = {struct, [
			{<<"command">>, <<"aprofile">>},
			{<<"profile">>, list_to_binary(Profile)}
		]},
	Newstate = push_event(Headjson, State),
	{noreply, Newstate};
handle_cast({url_pop, URL, Name}, State) ->
	Headjson = {struct, [
			{<<"command">>, <<"urlpop">>},
			{<<"url">>, list_to_binary(URL)},
			{<<"name">>, list_to_binary(Name)}
		]},
	Newstate = push_event(Headjson, State),
	{noreply, Newstate};
handle_cast({blab, Text}, State) when is_list(Text) ->
	handle_cast({blab, list_to_binary(Text)}, State);
handle_cast({blab, Text}, State) when is_binary(Text) ->
	Headjson = {struct, [
		{<<"command">>, <<"blab">>},
		{<<"text">>, Text}
	]},
	Newstate = push_event(Headjson, State),
	{noreply, Newstate};
handle_cast(Msg, State) ->
	?DEBUG("Other case ~p", [Msg]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(poll_flush, State) ->
	{SupPollQueue, NewSupState} = case State#state.supervisor_state of
		undefined -> {[], undefined};
		SupState -> supervisor_web_connection:nuke_poll_queue(SupState)
	end,
	FullQueue = lists:append(lists:reverse(State#state.poll_queue), SupPollQueue),
	case {State#state.poll_pid, FullQueue} of
		{undefined, _} ->
			{noreply, State#state{poll_flush_timer = undefined}};
		{_Pid, []} ->
			{noreply, State#state{poll_flush_timer = undefined}};
		{Pid, _PollQueue} when is_pid(Pid) ->
			Pid ! {poll, {200, [], mochijson2:encode({struct, [
				{success, true},
				{<<"result">>, FullQueue}
			]})}},
			unlink(Pid),
			{noreply, State#state{poll_queue = [], poll_pid = undefined, poll_pid_established = util:now(), poll_flush_timer = undefined, supervisor_state = NewSupState}}
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
handle_info({cpx_monitor_event, _Message}, #state{securitylevel = agent} = State) ->
	?WARNING("Not eligible for supervisor view, so shouldn't be getting events.  Unsubbing", []),
	cpx_monitor:unsubscribe(),
	{noreply, State};
handle_info({cpx_monitor_event, _} = Msg, #state{supervisor_state = SupState} = State) ->
	{noreply, NewSupState} = supervisor_web_connection:handle_info(Msg, SupState),
	MidState = case State#state.poll_flush_timer of
		undefined ->
			Self = self(),
			State#state{poll_flush_timer = erlang:send_after(?POLL_FLUSH_INTERVAL, Self, poll_flush)};
		_ ->
			State
	end,
	{noreply, MidState#state{supervisor_state = NewSupState}};
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
handle_info({'EXIT', Agent, Reason}, #state{agent_fsm = Agent} = State) ->
	case State#state.poll_pid of
		undefined ->
			ok;
		Pid when is_pid(Pid) ->
			Pid ! {poll, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"forced logout by fsm death">>}, {<<"errcode">>, <<"FSM_DEATH">>}]})}},
			ok
	end,
	{stop, Reason, State};
handle_info(Info, State) ->
	?DEBUG("info I can't handle:  ~p", [Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(Reason, State) ->
	?NOTICE("terminated ~p", [Reason]),
	timer:cancel(State#state.ack_timer),
	case State#state.poll_pid of
		undefined ->
			ok;
		Pid when is_pid(Pid) ->
			Pid ! {kill, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"forced logout">>}, {<<"errcode">>, <<"FORCED_LOGOUT">>}]})},
			ok
	end.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-spec(format_status/2 :: (Cause :: any(), Data :: [any()]) -> #state{}).
format_status(normal, [PDict, State]) ->
	[{data, [{"State", format_status(terminate, [PDict, State])}]}];
format_status(terminate, [_PDict, State]) ->
	case State#state.current_call of
		#call{id = ID} ->
			State#state{current_call = ID};
		_ -> State
	end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

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

email_props_to_json(Proplist) ->
	email_props_to_json(Proplist, []).

email_props_to_json([], Acc) ->
	{struct, lists:reverse(Acc)};
email_props_to_json([{Key, Value} | Tail], Acc) ->
	{Dokey, Newkey} = case {is_binary(Key), is_list(Key)} of
		{true, _} -> {ok, Key};
		{_, true} -> {ok, list_to_binary(Key)};
		{_, _} -> {false, false}
	end,
	{Doval, Newval} = case {is_binary(Value), is_list(Value)} of
		{true, _} -> {ok, Value};
		{_, true} -> {ok, email_props_to_json(Value)};
		{_, _} -> {false, false}
	end,
	case {Dokey, Doval} of
		{ok, ok} ->
			email_props_to_json(Tail, [{Newkey, Newval} | Acc]);
		_Else ->
			email_props_to_json(Tail, Acc)
	end.

-type(headers() :: [{string(), string()}]).
-type(mochi_out() :: binary()).
-spec(parse_media_call/3 :: (Mediarec :: #call{}, Command :: {string(), any()}, Response :: any()) -> {headers(), mochi_out()}).
parse_media_call(#call{type = email}, {<<"attach">>, _Args}, {ok, Filenames}) ->
	Binnames = lists:map(fun(N) -> list_to_binary(N) end, Filenames),
	Json = {struct, [
		{success, true},
		{<<"result">>, Binnames}
	]},
	Html = mochiweb_html:to_html({
		<<"html">>, [], [
			{<<"head">>, [], []},
			{<<"body">>, [], [
				{<<"textarea">>, [], [mochijson2:encode(Json)]}
			]}
		]}),
	%?DEBUG("html:  ~p", [Html]),
	{[], Html};
parse_media_call(#call{type = email}, {<<"attach">>, _Args}, {error, Error}) ->
	Json = {struct, [
		{success, false},
		{<<"message">>, Error},
		{<<"errcode">>, Error}
	]},
	Html = mochiweb_html:to_html({
		<<"html">>, [], [
			{<<"head">>, [], []},
			{<<"body">>, [], [
				{<<"textarea">>, [], [mochijson2:encode(Json)]}
			]}
		]}),
	{[], Html};
parse_media_call(#call{type = email}, {<<"detach">>, _Args}, {ok, Keys}) ->
	Binnames = lists:map(fun(N) -> list_to_binary(N) end, Keys),
	Json = {struct, [
		{success, true},
		{<<"result">>, Binnames}
	]},
	{[], mochijson2:encode(Json)};
parse_media_call(#call{type = email}, {<<"get_skeleton">>, _Args}, {Type, Subtype, Heads, Props}) ->
	Json = {struct, [
		{<<"type">>, Type}, 
		{<<"subtype">>, Subtype},
		{<<"headers">>, email_props_to_json(Heads)},
		{<<"properties">>, email_props_to_json(Props)}
	]},
	{[], mochijson2:encode({struct, [{success, true}, {<<"result">>, Json}]})};
parse_media_call(#call{type = email}, {<<"get_skeleton">>, _Args}, {TopType, TopSubType, Tophead, Topprop, Parts}) ->
	Fun = fun
		({Type, Subtype, Heads, Props}, {F, Acc}) ->
			Head = {struct, [
				{<<"type">>, Type},
				{<<"subtype">>, Subtype},
				{<<"headers">>, email_props_to_json(Heads)},
				{<<"properties">>, email_props_to_json(Props)}
			]},
			{F, [Head | Acc]};
		({Type, Subtype, Heads, Props, List}, {F, Acc}) ->
			{_, Revlist} = lists:foldl(F, {F, []}, List),
			Newlist = lists:reverse(Revlist),
			Head = {struct, [
				{<<"type">>, Type},
				{<<"subtype">>, Subtype},
				{<<"headers">>, email_props_to_json(Heads)},
				{<<"properties">>, email_props_to_json(Props)},
				{<<"parts">>, Newlist}
			]},
			{F, [Head | Acc]}
	end,
	{_, Jsonlist} = lists:foldl(Fun, {Fun, []}, Parts),
	Json = {struct, [
		{<<"type">>, TopType}, 
		{<<"subtype">>, TopSubType}, 
		{<<"headers">>, email_props_to_json(Tophead)},
		{<<"properties">>, email_props_to_json(Topprop)},
		{<<"parts">>, lists:reverse(Jsonlist)}]},
	%?DEBUG("json:  ~p", [Json]),
	{[], mochijson2:encode({struct, [{success, true}, {<<"result">>, Json}]})};
parse_media_call(#call{type = email}, {<<"get_path">>, _Path}, {ok, {Type, Subtype, _Headers, _Properties, Body} = Mime}) ->
	Emaildispo = email_media:get_disposition(Mime),
	%?DEBUG("Type:  ~p; Subtype:  ~p;  Dispo:  ~p", [Type, Subtype, Emaildispo]),
	case {Type, Subtype, Emaildispo} of
		{Type, Subtype, {attachment, Name}} ->
			%?DEBUG("Trying to some ~p/~p (~p) as attachment", [Type, Subtype, Name]),
			{[
				{"Content-Disposition", lists:flatten(io_lib:format("attachment; filename=\"~s\"", [binary_to_list(Name)]))},
				{"Content-Type", lists:append([binary_to_list(Type), "/", binary_to_list(Subtype)])}
			], Body};
		{<<"text">>, <<"rtf">>, {inline, Name}} ->
			{[
				{"Content-Disposition", lists:flatten(io_lib:format("attachment; filename=\"~s\"", [binary_to_list(Name)]))},
				{"Content-Type", lists:append([binary_to_list(Type), "/", binary_to_list(Subtype)])}
			], Body};
		{<<"text">>, <<"html">>, _} ->
			Listbody = binary_to_list(Body),
			Parsed = try mochiweb_html:parse(lists:append(["<html>", Listbody, "</html>"])) of
				Islist when is_list(Islist) ->
					Islist;
				Isntlist ->
					[Isntlist]
			catch
				error:function_clause ->
					% most likely there's a doc type, so this would parse out correctly anyway.
					case mochiweb_html:parse(Listbody) of
						Islist when is_list(Islist) ->
							Islist;
						Isntlist ->
							[Isntlist]
					end
			end,
			Lowertag = fun(E) -> string:to_lower(binary_to_list(E)) end,
			Stripper = fun
				(_F, [], Acc) ->
					lists:reverse(Acc);
				(F, [Head | Rest], Acc) when is_tuple(Head) ->
					case Head of
						{comment, _} ->
							F(F, Rest, [Head | Acc]);
						{Tag, _Attr, Kids} ->
							case Lowertag(Tag) of
								"html" ->
									F(F, Kids, []);
								"head" ->
									F(F, Rest, Acc);
								"body" ->
									F(F, Kids, Acc);
								_Else ->
									F(F, Rest, [Head | Acc])
							end
					end;
				(F, [Bin | Rest], Acc) when is_binary(Bin) ->
					F(F, Rest, [Bin | Acc])
			end,
			Newhtml = Stripper(Stripper, Parsed, []),
			{[], mochiweb_html:to_html({<<"span">>, [], Newhtml})};
		{Type, Subtype, _Disposition} ->
			{[{"Content-Type", lists:append([binary_to_list(Type), "/", binary_to_list(Subtype)])}], Body}
%
%		{"text", _, _} ->
%			{[], list_to_binary(Body)};
%		{"image", Subtype, {Linedness, Name}} ->
%			Html = case Linedness of
%				inline ->
%					{<<"img">>, [{<<"src">>, list_to_binary(Name)}], []};
%				attachment ->
%					{<<"a">>, [{<<"href">>, list_to_binary(Name)}], list_to_binary(Name)}
%			end,
%			{[], mochiweb_html:to_html(Html)};
%		{Type, Subtype, Disposition} ->
%			?WARNING("unsure how to handle ~p/~p disposed to ~p", [Type, Subtype, Disposition]),
%			{[], <<"404">>}
	end;
parse_media_call(#call{type = email}, {<<"get_path">>, _Path}, {message, Bin}) when is_binary(Bin) ->
	%?DEBUG("Path is a message/Subtype with binary body", []),
	{[], Bin};
parse_media_call(#call{type = email}, {<<"get_from">>, _}, undefined) ->
	{[], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"no reply info">>}, {<<"errcode">>, <<"REPLYINFO_NOEXISTS">>}]})};
parse_media_call(#call{type = email}, {<<"get_from">>, _}, {Label, Address}) ->
	Json = {struct, [
		{<<"label">>, Label},
		{<<"address">>, Address}
	]},
	{[], mochijson2:encode({struct, [{success, true}, {<<"result">>, Json}]})};
parse_media_call(Mediarec, Command, Response) ->
	?WARNING("Unparsable result for ~p:~p.  ~p", [Mediarec#call.type, element(1, Command), Response]),
	{[], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"unparsable result for command">>}, {<<"errcode">>, <<"BAD_RETURN">>}]})}.

-spec(do_action/3 :: (Nodes :: [atom()], Do :: any(), Acc :: [any()]) -> {'true' | 'false', any()}).
do_action([], _Do, Acc) ->
	{true, Acc};
%% get a list of the agent profiles and how many agents are logged into each
do_action([Node | Tail], ["agent_profiles"] = Do, Acc) ->
	Profiles = agent_auth:get_profiles(),
	Makeprops = fun(#agent_profile{name = Name}) ->
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
	%{Mega, Sec, _Micro} = Agent#agent.lastchange,
	%Now = (Mega * 1000000) + Sec,
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
		{<<"lastchanged">>, Agent#agent.lastchange}
		%{<<"remotenumber">>, Remnum}
	],
	Statedata = case Agent#agent.statedata of
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
		{<<"callerid">>, list_to_binary(element(1, Call#call.callerid) ++ " " ++ element(2, Call#call.callerid))},
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

encode_client(Client) when is_record(Client, client) ->
	{struct, [
		{<<"label">>, list_to_binary(Client#client.label)},
		{<<"id">>, list_to_binary(Client#client.id)}
	]};
encode_client(_) ->
	undefined.

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


		

-ifdef(TEST).

poll_flushing_test_() ->
	{foreach,
	fun() ->
		Agent = #agent{login = "testagent"},
		{ok, Apid} = agent:start(Agent),
		{ok, WebListener} = gen_server_mock:named({local, agent_web_listener}),
		{ok, AgentManMock} = gen_leader_mock:start(agent_manager),
		?DEBUG("query agent", []),
		gen_leader_mock:expect_leader_call(AgentManMock, fun(_, _, State, _) ->
			{ok, false, State}
		end),
		?DEBUG("start agent", []),
		gen_leader_mock:expect_call(AgentManMock, fun(_, _, State, _) ->
			%{ok, Apid} = Out = agent:start(Agent),
			{ok, {ok, Apid}, State}
		end),
%		?DEBUG("update skill list", []),
%		gen_leader_mock:expect_cast(AgentManMock, fun(_, State, _) ->
%			ok
%		end),
		gen_server_mock:expect_cast(WebListener, fun({linkto, _P}, _) ->
			ok
		end),
		{ok, Seedstate} = init([Agent, agent]),
		AssertMocks = fun() ->
			gen_server_mock:assert_expectations(WebListener),
			gen_leader_mock:assert_expectations(AgentManMock)
		end,
		{Agent, Apid, WebListener, AgentManMock, Seedstate, AssertMocks}
	end,
	fun({_Agent, Apid, WebListener, AgentManMock, _Seedstate, _AssertMocks}) ->
		agent:stop(Apid),
		gen_server_mock:stop(WebListener),
		gen_leader_mock:stop(AgentManMock),
		timer:sleep(100) % giving the named mocks time to dereg names
	end,
	[fun({_Agent, _Apid, WebListener, _AgentManMock, Seedstate, AssertMocks}) ->
		{"A single item shoved into queue",
		fun() ->
			State = push_event(<<"string">>, Seedstate),
			Res = receive
				Any ->
					 Any
			after 550 ->
				timeout
			end,
			?assertEqual(poll_flush, Res),
			AssertMocks()
		end}
	end,
	fun({_Agent, _Apid, _WebListener, _AgentManMock, Seedstate, AssertMocks}) ->
		{"Two items shoved in quickly",
		fun() ->
			State = push_event(<<"string">>, Seedstate),
			State2 = push_event(<<"string2">>, State),
			{{noreply, State3}, Res1} = receive
				Any ->
					?assertEqual(poll_flush, Any),
					{handle_info(Any, State2), Any}
			after 550 ->
				{{noreply, State2}, timeout}
			end,
			Res2 = receive
				Any2 ->
					 Any2
			after 550 ->
				timeout
			end,
			?assertEqual({poll_flush, timeout}, {Res1, Res2}),
			AssertMocks()
		end}
	end,
	fun({_Agent, _Apid, WebListener, _AgentManMock, Seedstate, AssertMocks}) ->
		{"Two fast, one slow, then 2 more fast",
		fun() ->
			State1 = push_event(<<"string1">>, Seedstate),
			State2 = push_event(<<"string2">>, State1),
			gen_server_mock:expect_info(WebListener, fun({poll, {200, [], Json}}, _) ->
				{struct, [{<<"success">>, true}, {<<"data">>, [<<"string1">>, <<"string2">>]}, {<<"result">>, [<<"string1">>, <<"string2">>]}]} = mochijson2:decode(Json),
				ok
			end),
			gen_server_mock:expect_info(WebListener, fun({poll, {200, [], Json}}, _) ->
				{struct, [{<<"success">>, true}, {<<"data">>, [<<"string3">>]}, {<<"result">>, [<<"string3">>]}]} = mochijson2:decode(Json),
				ok
			end),
			gen_server_mock:expect_info(WebListener, fun({poll, {200, [], Json}}, _) ->
				{struct, [{<<"success">>, true}, {<<"data">>, [<<"string4">>, <<"string5">>]}, {<<"result">>, [<<"string4">>, <<"string5">>]}]} = mochijson2:decode(Json),
				ok
			end),
			HandleInfoState1 = State2#state{poll_pid = WebListener},
			{{noreply, State3}, Res1} = receive
				Any ->
					?assertEqual(poll_flush, Any),
					{handle_info(Any, HandleInfoState1), Any}
			after 550 ->
				{{noreply, State2}, timeout}
			end,
			?DEBUG("first fast pair complete", []),
			?assertEqual(poll_flush, Res1),
			?assertEqual([], State3#state.poll_queue),
			State4 = push_event(<<"string3">>, State3),
			HandleInfoState2 = State4#state{poll_pid = WebListener},
			{{noreply, State5}, Res2} = receive
				Any2 ->
					?assertEqual(poll_flush, Any2),
					{handle_info(Any2, HandleInfoState2), Any2}
			after 550 ->
				{{noreply, State4}, timeout}
			end,
			?DEBUG("single pass", []),
			?assertEqual(poll_flush, Res2),
			?assertEqual([], State5#state.poll_queue),
			State6 = push_event(<<"string4">>, State5),
			State7 = push_event(<<"string5">>, State6),
			HandleInfoState3 = State7#state{poll_pid = WebListener},
			{{noreply, State8}, Res3} = receive
				Any3 ->
					?assertEqual(poll_flush, Any3),
					{handle_info(Any3, HandleInfoState3), Any3}
			after 550 ->
				{{noreply, State7}, timeout}
			end,
			?DEBUG("2nd pair complete", []),
			?assertEqual(poll_flush, Res3),
			?assertEqual([], State8#state.poll_queue),
			AssertMocks()
		end}
	end]}.




check_live_poll_test_() ->
	{timeout, ?TICK_LENGTH * 5, fun() -> [
	{"When poll pid is undefined, and less than 10 seconds have passed",
	fun() ->
		?DEBUG("timeout:  ~p", [?TICK_LENGTH * 5]),
		State = #state{poll_pid_established = util:now(), poll_pid = undefined},
		?assertMatch({noreply, NewState}, handle_info(check_live_poll, State)),
		?DEBUG("Starting recieve", []),
		Ok = receive
			check_live_poll ->
				true
		after ?TICK_LENGTH + 1 ->
			false
		end,
		?assert(Ok)
	end},
	{"When poll pid is undefined, and more than 10 seconds have passed",
	fun() ->
		State = #state{poll_pid_established = util:now() - 12, poll_pid = undefined},
		?assertEqual({stop, normal, State}, handle_info(check_live_poll, State)),
		Ok = receive
			check_live_poll	->
				 false
		after ?TICK_LENGTH + 1 ->
			true
		end,
		?assert(Ok)
	end},
	{"When poll pid exists, and less than 20 seconds have passed",
	fun() ->
		State = #state{poll_pid_established = util:now() - 5, poll_pid = self()},
		{noreply, Newstate} = handle_info(check_live_poll, State),
		?assertEqual([], Newstate#state.poll_queue),
		Ok = receive
			check_live_poll ->
				 true
		after ?TICK_LENGTH + 1 ->
			false
		end,
		?assert(Ok)
	end},
	{"When poll pid exists, and more than 20 seconds have passed",
	fun() ->
		State = #state{poll_pid_established = util:now() - 25, poll_pid = self()},
		{noreply, Newstate} = handle_info(check_live_poll, State),
		?assertEqual([], Newstate#state.poll_queue),
		Ok = receive
			check_live_poll	->
				 true
		after ?TICK_LENGTH + 1 ->
			false
		end,
		?assert(Ok)
	end}] end}.

set_state_test_() ->
	{
		foreach,
		fun() ->
			%agent_manager:start([node()]),
			gen_leader_mock:start(agent_manager),
			gen_leader_mock:expect_leader_call(agent_manager, 
				fun({exists, "testagent"}, _From, State, _Elec) -> 
					{ok, Apid} = agent:start(#agent{login = "testagent"}),
					{ok, {true, Apid}, State} 
				end),
			{ok, Connpid} = agent_web_connection:start(#agent{login = "testagent", skills = [english]}, agent),
			{Connpid}
		end,
		fun({Connpid}) ->
			stop(Connpid)
			%agent_auth:stop(),
			%agent_manager:stop()
		end,
		[
			fun({Connpid}) ->
				{"Set state valid",
				fun() ->
					Reply = gen_server:call(Connpid, {set_state, "idle"}),
					?assertEqual({200, [], [123, [34,"success", 34], 58,<<"true">>, 44, [34,<<"status">>, 34], 58, [34,"ok", 34], 125]}, Reply),
					Reply2 = gen_server:call(Connpid, {set_state, "released", "Default"}),
					?assertEqual({200, [], [123, [34,"success", 34], 58,<<"true">>, 44, [34,<<"status">>, 34], 58, [34,"ok", 34], 125]}, Reply2)
				end}
			end,
			fun({Connpid}) ->
				{"Set state invalid",
				fun() ->
					Reply = gen_server:call(Connpid, {set_state, "wrapup"}),
					ExpectedJson = mochijson2:encode({struct, [
						{success, false},
						{<<"status">>, invalid},
						{<<"message">>, <<"invalid state change">>},
						{<<"errcode">>, <<"INVALID_STATE_CHANGE">>}
					]}),
					?CONSOLE("Reply:  ~p;~nExpected:  ~p", [Reply, ExpectedJson]),
					?assertEqual({200, [], ExpectedJson}, Reply),
					Reply2 = gen_server:call(Connpid, {set_state, "wrapup", "garbage"}),
					?CONSOLE("~p", [Reply2]),
					?assertEqual({200, [], ExpectedJson}, Reply2)
				end}
			end
		]
	}.

%extract_groups_test() ->
%	Rawlist = [
%		{{queue, "queue1"}, [{group, "group1"}], node(), os:timestamp(), none, undefined},
%		{{queue, "queue2"}, [{group, "Default"}], node(), os:timestamp(), none, undefined},
%		{{agent, "agent1"}, [{profile, "profile1"}], node(), os:timestamp(), none, undefined},
%		{{media, "media1"}, [], node(), os:timestamp(), none, undefined},
%		{{queue, "queue3"}, [{group, "Default"}], node(), os:timestamp(), none, undefined},
%		{{agent, "agent2"}, [{profile, "Default"}], node(), os:timestamp(), none, undefined},
%		{{agent, "agent3"}, [{profile, "profile1"}], node(), os:timestamp(), none, undefined}
%	],
%	Expected = [
%		{"agentprofile", "Default"},
%		{"agentprofile", "profile1"},
%		{"queuegroup", "Default"},
%		{"queuegroup", "group1"}
%	],
%	Out = extract_groups(Rawlist),
%	?assertEqual(Expected, Out).

		
-define(MYSERVERFUNC, 
	fun() ->
		["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		mnesia:create_schema([node()]),
		%mnesia:start(),
		%agent_auth:start(),
		Agent = #agent{login = "agent", skills = [english]},
		{ok, Fsmpid} = agent:start(Agent),
		gen_leader_mock:start(agent_manager),
		gen_leader_mock:expect_leader_call(agent_manager, fun({exists, "agent"}, _From, State, _Elec) -> {ok, {true, Fsmpid}, State} end),
		%agent_manager:start([node()]),
		%agent_auth:start(),
		{ok, Pid} = start_link(Agent, agent),
		unlink(Pid),
		Stopfun = fun() ->
			?CONSOLE("stopping agent_auth", []),
			%agent_auth:stop(),
			?CONSOLE("stopping agent_manager", []),
			%agent_manager:stop(),
			gen_leader_mock:stop(agent_manager),
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

%-include("gen_server_test.hrl").


-endif.
