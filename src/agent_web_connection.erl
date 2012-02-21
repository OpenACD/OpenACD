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

%% @doc Handles the internal (cpx interaction) part of an agent web
%% connection.
%% 
%% == Hooks ==
%%
%% This module can trigger the following {@link cpx_hooks. hooks}:
%%
%% === agent_web_path ===
%%
%%	When the agent_web_connection gets an http request for a path that
%% is not handled internally or by the media (if the agent is oncall).
%% 
%% ==== Arguments ====
%% 
%% 	<ul>
%%		<li>Path :: string() - Path portion of URL requested</li>
%%		<li>Post :: proplist() | undefined - Posted data</li>
%%		<li>Call :: #call{} | undefined - Current media if available</li>
%%	</ul>
%% 
%% ==== Returns ====
%% 
%% Returns other than the ones listed are wrapped in an error with an
%% UNKNOWN_ERROR code.
%%
%%	<dl>
%%		<dt>Binary :: binary()</dt>
%% 			<dd>The body to return for the request.  
%% status code of 200 is used.</dd>
%%
%%		<dt>{Status :: http_status(), Headers :: proplist(), 
%% 			Binary :: binary()}</dt>
%%			<dd>Allows control of the status code and headers</dd>
%%
%%		<dt>ok</dt>
%% 			<dd>Returnes a simple json success message back to the client</dd>
%%
%%		<dt>{json, Json :: json()}</dt>
%% 			<dd>Returns wrapped in a success message.</dd>
%%
%%		<dt>{Errcode :: binary(), ErrMessage :: binary()}</dt>
%% 			<dd>Return gets wrapped in an error json return.</dd>
%%	</dl>
%%
%% === agent_web_tabs ===
%%
%% When the agent logs in, this is triggered.  Each valid response is
%% added to the list of tabs an agent may open.
%%
%% ==== Arguments ====
%%
%% <ul>
%%     <li>Agent :: agent{} - The agent that has logged in.</li>
%% </ul>
%%
%% ==== Returns ====
%%
%% Invalid returns are ignored.
%%
%% <dl>
%%     <dt>{TranslationTag :: binary(), Href :: binary()}</dt>
%%     <dd>Translation tag is used to look up what to display in the tabs
%% menu for the user.  Href is the url path to load.  When the user
%% selects the tabe, an agent_web_path will be triggered.</dd>
%% </dl>
%%
%% == Web API ==
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
%% @see cpx_hooks

-module(agent_web_connection).
-author("Micah").

-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([dummy_plugin/3]).
-endif.

-include("log.hrl").
-include("call.hrl").
-include("agent.hrl").
-include("queue.hrl").
-include("web.hrl").

-include_lib("stdlib/include/qlc.hrl").

%% API
-export([
	start_link/1,
	start/1,
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
%% handy encode functions
-export([encode_call/1, encode_client/1]).

%% Web api exports.
%% to make documenting a web api easier, the listener will directly
%% calls these functions.
-export([
	set_release/2,
	set_state/3,
	set_state/4,
	end_wrapup/2,
	dial/2,
	get_avail_agents/1,
	agent_transfer/2,
	agent_transfer/3,
	%media_command/3,
	%media_command/4,
	media_hangup/1,
	media_call/3,
	media_call/4,
	media_cast/3,
	media_cast/4,
	load_media/1,
	ring_test/1,
	get_agent_profiles/1,
	get_queue_transfer_options/1,
	warm_transfer/2,
	warm_transfer_complete/1,
	warm_transfer_cancel/1,
	queue_transfer/3,
	init_outbound/3,
	set_endpoint/3,
	logout/1,
	plugin_call/3,
	arbitrary_command/3,
	get_tabs_menu/1
]).

-web_api_functions([
	{set_release, 2},
	{set_state, 3},
	{set_state, 4},
	{end_wrapup, 2},
	{dial, 2},
	{get_avail_agents, 1},
	{agent_transfer, 2},
	{agent_transfer, 3},
	%{media_command, 3},
	%{media_command, 4},
	{media_hangup, 1},
	{media_call, 3},
	{media_call, 4},
	{media_cast, 3},
	{media_cast, 4},
	{load_media, 1},
	{ring_test, 1},
	{get_agent_profiles, 1},
	{get_queue_transfer_options, 1},
	{warm_transfer, 2},
	{warm_transfer_complete, 1},
	{warm_transfer_cancel, 1},
	{queue_transfer, 3},
	{init_outbound, 3},
	{get_endpoint, 2},
	{set_endpoint, 3},
	{plugin_call, 3},
	{poll, 2},
	{logout, 1},
	{get_tabs_menu, 1}
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-type(tref() :: any()).

-record(channel_state, {
	current_call :: #call{} | 'undefined' | 'expect',
	mediaload :: any()
}).

-record(state, {
	salt :: any(),
	agent_fsm :: pid() | 'undefined',
	agent_channels = dict:new() :: dict(),
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

%% @doc {@web} Sets the release mode of the agent.  If `Release' is
%% `none', the agent will be set idle, otherwise set to the release mode 
%% given.
-spec(set_release/2 :: (Conn :: pid(), Release :: bin_string()) -> any()).
set_release(Conn, Release) ->
	gen_server:call(Conn, {set_release, Release}).

%% @doc {@web} Set the agent channel `Channel' to the given `Statename' 
%% with default state data.  No result property as it either worked or 
%% didn't.
-spec(set_state/3 :: (Conn :: pid(), Channel :: bin_string(), Statename :: bin_string()) -> any()).
set_state(Conn, Channel, Statename) ->
	gen_server:call(Conn, {set_state, binary_to_list(Channel), binary_to_list(Statename)}).

%% @doc {@web} Set the agent channel `Channel' to the given `Statename' 
%% with the given `Statedata'.  No result property as it either worked or 
%% it didn't.  State data will vary based on state.
-spec(set_state/4 :: (Conn :: pid(), Channel :: bin_string(), Statename :: bin_string(), Statedata :: any()) -> any()).
set_state(Conn, Channel, Statename, Statedata) ->
	gen_server:call(Conn, {set_state, binary_to_list(Channel), binary_to_list(Statename), binary_to_list(Statedata)}).

%% @doc {@web} End wrapup the agent channel 'Channel'.  This also kills 
%% the channel, making it available for use again.  No result property as 
%% it iether worked or didn't.
-spec(end_wrapup/2 :: (Conn :: pid(), Channel :: bin_string()) -> any()).
end_wrapup(Conn, Channel) ->
	gen_server:call(Conn, {end_wrapup, binary_to_list(Channel)}).

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

%% @doc {@web} @see media_call/4
-spec(media_call/3 :: (Conn :: pid(), Channel :: bin_string(), Command :: bin_string()) -> any()).
media_call(Conn, Channel, Command) ->
	media_call(Conn, Channel, Command, []).

%% @doc {@web} Forward a request to the media associated with an oncall
%% agent channel.  `Command' is the name of the request to make.  `Args'
%% is a list of arguments to be sent with the `Command'.  Check the
%% documentation of the media modules to see what possible returns there
%% are.
-spec(media_call/4 :: (Conn :: pid(), Channel :: bin_string(), Command :: bin_string(), Args :: [any()]) -> any()).
media_call(Conn, Channel, Command, Args) ->
	gen_server:call(Conn, {media_call, Channel, Command, Args}).

%% @doc {@web} @see media_cast/4
-spec(media_cast/3 :: (Conn :: pid(), Channel :: bin_string(), Command :: bin_string()) -> any()).
media_cast(Conn, Channel, Command) ->
	media_cast(Conn, Channel, Command, []).

%% @doc {@web} Forward a command to the media associated with an oncall
%% agent channel.  `Command' is the name of the command to send.  `Args'
%% is a list of arguments to send with the `Command'.  There is no reply
%% expected, so a simple success is always returned.
-spec(media_cast/4 :: (Conn :: pid(), Channel :: bin_string(), Command :: bin_string(), Args :: [any()]) -> any()).
media_cast(Conn, Channel, Command, Args) ->
	gen_server:call(Conn, {media_cast, Channel, Command, Args}).

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

%% @doc {@web} Get the agent's endpoint data.
-spec(get_endpoint/2 :: (Conn :: pid(), Type :: bin_string()) -> any()).
get_endpoint(Conn, TypeBin) ->
	case catch erlang:binary_to_existing_atom(TypeBin, utf8) of
		{'EXIT', {badarg, _}} ->
			?reply_err(<<"invalid endpoint type">>, <<"INVALID_ENDPOINT_TYPE">>);
		Type ->
			case gen_server:call(Conn, {get_endpoint, Type}) of
				{ok, Data} ->
					?reply_success(endpoint_to_struct(Type, Data));
				{error, notfound} ->
					?reply_success(null)
			end
	end.

%% @doc {@web} Sets the agent's endpoint data to the given, well, data.
%% Particularly useful if the flash phone is used, as all of the connection
%% data will not be available for that until it is started on in the 
%% browser.
-spec(set_endpoint/3 :: (Conn :: pid(), Endpoint :: bin_string(), Data :: bin_string()) -> any()).
set_endpoint(Conn, <<"freeswitch_media">>, Struct) ->
	set_endpoint_int(Conn, freeswitch_media, Struct, fun(Data) ->
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
set_endpoint(Conn, <<"email_media">>, _Struct) ->
	set_endpoint_int(Conn, email_media, {struct, []}, fun(_) -> ok end);

set_endpoint(Conn, <<"dummy_media">>, Struct) ->
	set_endpoint_int(Conn, dummy_media, Struct, fun(Data) ->
		case proplists:get_value(<<"dummyMediaEndpoint">>, Data) of
			<<"ring_channel">> ->  ring_channel;
			<<"inband">> -> inband;
			<<"outband">> -> outband;
			<<"persistant">> -> persistant;
			_ -> {error, unknown_dummy_endpoint}
		end
	end);
set_endpoint(_Conn, _Type, _Struct) ->
	?reply_err(<<"unknwon endpoint">>, <<"INVALID_ENDPOINT">>).

%% @doc {@web} Gathers the tabs an agent can access, and pushes the result
%% into the command queue.
%% {"command": "set_tabs_menu",
%% "tabs": [
%%     {"label":string(),"href":string()}
%% ]}
get_tabs_menu(Conn) ->
	gen_server:cast(Conn, get_tabs_menu).

% set_endpoint(Conn, Endpoint, Data, Persist) ->
% 			EndpointData = binary_to_list(Data),
% 			Z = gen_server:call(Conn, {set_endpoint, EndpointType, EndpointData, Persisty}),
% 			case Z of
% 				ok -> {200, [], mochijson2:encode({struct, [{success, true}]})};
% 				{ok, _Pid} -> {200, [], mochijson2:encode({stuct, [{success, true}]})};
% 				{error, Error} ->
% 					{200, [], mochijson2:encode({struct, [
% 						{success, false},
% 						{<<"message">>, list_to_binary(io_lib:format("Error setting endpoint:  ~p", [Error]))},
% 						{<<"errcode">>, <<"INVALID_ENDPOINT">>}
% 					]})}
% 			end
% 	end.

%% @doc {@web} If the media set anything to be loaded at call start, 
%% retreive it.  This is useful if the client (such as web browser) needs 
%% to refresh the page or crashes, but is able to recover before the 
%% automatic logout occurs.  Results will vary from media to media.
-spec(load_media/1 :: (Conn :: pid()) -> any()).
load_media(Conn) ->
	gen_server:call(Conn, mediaload).

%% @doc {@web} Forward the request to the given plugin_app.  If the app
%% is missing, or the call fails, expect an error.  Results will vary
%% from plugin to plugin.
-spec(plugin_call/3 :: (Conn :: pid(), Plugin :: string(), Args :: [any()]) -> any()).
plugin_call(Conn, Plugin, Args) ->
	gen_server:call(Conn, {plugin_call, Plugin, Args}).

%% @doc Useful when a plugin needs to send information or results to the
%% agent ui.
-spec(arbitrary_command/3 :: (Conn :: pid(), Command :: binary() | atom(),
	JsonProps :: [{binary() | atom(), any()}]) -> 'ok').
arbitrary_command(Conn, Command, JsonProps) ->
	gen_server:cast(Conn, {arbitrary_command, Command, JsonProps}).

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
-spec(start_link/1 :: (Agent :: #agent{}) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start_link(Agent) ->
	gen_server:start_link(?MODULE, [Agent], [{timeout, 10000}]).

%% @doc Starts the passed agent at the given security level.
-spec(start/1 :: (Agent :: #agent{}) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start(Agent) ->
	gen_server:start(?MODULE, [Agent], [{timeout, 10000}]).

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
%%  <tr>
%%  	<td>pluginevent</td>
%%		<td><ul>
%%  		<li>"plugin_app": string()</li>
%%  		<li>"event":  any()</li>
%%  	</ul></td>
%%  	<td>A Plugin can send events to specific agents.  Very plugin
%%  	Specific.  Check the plugin documentation for details.</td>
%%  </tr>
%% </table>
-spec(poll/2 :: (Pid :: pid(), Frompid :: pid()) -> 'ok').
poll(Pid, Frompid) ->
	gen_server:cast(Pid, {poll, Frompid}).

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

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Agent]) ->
	#agent{security_level = Security} = Agent,
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
			%State = agent:dump_state(Apid),

%			case Security of
%				agent ->
%					ok;
%				supervisor ->
%					cpx_monitor:subscribe();
%				admin ->
%					cpx_monitor:subscribe()
%			end,
			spawn_get_tabs_menu(Apid),
			{ok, #state{agent_fsm = Apid, ack_timer = Tref, securitylevel = Security, listener = whereis(agent_web_listener)}}
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
		AgState <- [agent:dump_state(Pid)]],

	Noms = [{struct, [{<<"name">>, list_to_binary(Rec#agent.login)}, {<<"profile">>, list_to_binary(Rec#agent.profile)}]} || Rec <- Agents],
	{reply, {200, [], mochijson2:encode({struct, [{success, true}, {<<"agents">>, Noms}, {<<"result">>, Noms}]})}, State};

handle_call({set_release, Release}, _From, #state{agent_fsm = Apid} = State) ->
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
	case agent:set_release(Apid, RelData) of
		ok ->
			{reply, ?simple_success(), State};
		_ ->
			{reply, ?reply_err(<<"unknown error">>, <<"UNKNOWN_ERR">>), State}
	end;

handle_call({set_state, Channel, Statename}, _From, #state{agent_channels = Channels} = State) ->
	Chans = [C || C <- dict:fetch_keys(Channels), pid_to_list(C) =:= Channel],
	case Chans of
		[] ->
			{reply, ?reply_err(<<"Channel not found">>, <<"CHANNEL_NOEXISTS">>), State};
		[Chan] ->
			case agent_channel:set_state(Chan, agent_channel:list_to_state(Statename)) of
				ok ->
					{reply, ?simple_success(), State};
				{error, invalid} ->
					{reply, ?reply_err(<<"Channel state change invalid">>, <<"INVALID_STATE_CHANGE">>), State}
			end
	end;

handle_call({set_state, Channel, Statename, Statedata}, _From, #state{agent_channels = Channels} = State) ->
	Chans = [C || C <- dict:fetch_keys(Channels), pid_to_list(C) =:= Channel],
	case Chans of
		[] ->
			{reply, ?reply_err(<<"Channel not found">>, <<"CHANNEL_NOEXISTS">>), State};
		[Chan] ->
			case agent_channel:set_state(Chan, agent_channel:list_to_state(Statename), Statedata) of
				ok ->
					{reply, ?simple_success(), State};
				invalid ->
					{reply, ?reply_err(<<"Channel state change invalid">>, <<"INVALID_STATE_CHANGE">>), State}
			end
	end;

handle_call({end_wrapup, Channel}, _From, #state{agent_channels = Channels} = State) ->
	Chans = [C || C <- dict:fetch_keys(Channels), pid_to_list(C) =:= Channel],
	case Chans of
		[] ->
			{reply, ?reply_err(<<"Channel not found">>, <<"CHANNEL_NOEXISTS">>)};
		[Chan] ->
			case agent_channel:end_wrapup(Chan) of
				ok ->
					{reply, ?simple_success(), State};
				invalid ->
					{reply, ?reply_err(<<"Channel not stopped">>, <<"INVALID_STATE_CHANGE">>), State}
			end
	end;

handle_call({get_endpoint, Type}, _From, #state{agent_fsm = Apid} = State) ->
	Agent = agent:dump_state(Apid),
	Reply = case agent:get_endpoint(Type, Agent) of
		R = {error, notfound} -> R;
		{ok, {InitOpts, _}} -> {ok, InitOpts}
	end,
	{reply, Reply, State};
handle_call({set_endpoint, Type, Data}, _From, #state{agent_fsm = Apid} = State) ->
	{reply, agent:set_endpoint(Apid, Type, Data), State};
handle_call({dial, Number}, _From, #state{agent_fsm = AgentPid} = State) ->
	{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"not yet implemented">>}, {<<"errcode">>, <<"NYI">>}]})}, State};
%	AgentRec = agent:dump_state(AgentPid),
%	case AgentRec#agent.state of
%		precall ->
%			#agent{statedata = Call} = AgentRec,
%			case Call#call.direction of
%				outbound ->
%					case gen_media:call(Call#call.source, {dial, Number}) of
%						ok ->
%							{reply, {200, [], mochijson2:encode({struct, [{success, true}]})}, State};
%						{error, Error} ->
%							?NOTICE("Outbound call error ~p", [Error]),
%							{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, list_to_binary(lists:flatten(io_lib:format("~p, Check your phone configuration", [Error])))}, {<<"errcode">>, <<"UNKNOWN_ERROR">>}]})}, State}
%					end;
%				_ ->
%					{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"This is not an outbound call">>}, {<<"errcode">>, <<"INVALID_STATE_CHANGE">>}]})}, State}
%			end;
%		_ ->
%			{reply, {200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"Agent is not in pre-call">>}, {<<"errcode">>, <<"INVALID_STATE_CHANGE">>}]})}, State}
%	end;
handle_call(dump_agent, _From, #state{agent_fsm = Apid} = State) ->
	Astate = agent:dump_state(Apid),
	{reply, Astate, State};
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
handle_call({init_outbound, Client, Type}, _From, #state{agent_fsm = Apid} = State) ->
	?NOTICE("Request to initiate outbound call of type ~p to ~p", [Type, Client]),
	%AgentRec = agent:dump_state(Apid), % TODO - avoid
	% TODO depricated; staring precall is a media specific thing, so this 
	% should just be reactive.
	Reply = case agent:precall(Apid, {precall, Client, Type})of
		{ok, ChanPid} ->
			{200, [], mochijson2:encode({struct, [{success, true}]})};
		{error, Else} ->
			?INFO("Could not start precall for ~p of ~p due to ~p", [Client, Type, Else]),
			{200, [], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"unknown error">>}, {<<"errcode">>, <<"UNKNOWN_ERROR">>}]})}
	end, 
	{reply, Reply, State};

% TODO supervisor handling was here.  Forward requests to 
% supervisor_web_connection module for teh happy.

handle_call({media_call, Channel, Command, Args}, _From, #state{agent_channels = Channels} = State) ->
	case fetch_channel(Channel, Channels) of
		none ->
			{reply, ?reply_err(<<"Channel doesn't exist">>, <<"CHANNEL_NOEXISTS">>), State};
		{_ChanPid, #channel_state{current_call = #call{source = CallPid} = Call}} ->
			Reply = try gen_media:call(CallPid, {?MODULE, Command, Args}) of
				invalid ->
					?DEBUG("media call returned invalid", []),
					?reply_err(<<"invalid media call">>, <<"INVALID_MEDIA_CALL">>);
				Response ->
					{H, D} = parse_media_call(Call, {?MODULE, Command, Args}, Response),
					{200, H, D}
			catch
				exit:{noproc, _} ->
					?DEBUG("Media no longer exists.", []),
					?reply_err(<<"media no longer exists">>, <<"MEDIA_NOEXISTS">>)
			end,
			{reply, Reply, State}
	end;

handle_call({media_cast, Channel, Command, Args}, _From, #state{agent_channels = Channels} = State) ->
	case fetch_channel(Channel, Channels) of
		none ->
			{reply, ?reply_err(<<"Channel doesn't exist">>, <<"CHANNEL_NOEXISTS">>), State};
		{_ChanPid, #channel_state{current_call = #call{source = CallPid} = Call}} ->
			gen_media:cast(CallPid, {?MODULE, Command, Args}),
			{reply, ?simple_success(), State}
	end;

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
handle_call({plugin_call, Plugin, Args}, _From, State) ->
	Apps = [Appname || {Appname, _, _} <- application:which_applications(),
		atom_to_list(Appname) =:= Plugin],
	case Apps of
		[] ->
			{reply, ?reply_err(<<"No such plugin">>, <<"PLUGIN_NOEXISTS">>), State};
		[App | _] ->
			case application:get_env(App, agent_web_handler) of
				undefined ->
					{reply, ?reply_err(<<"Plugin doesn't handle web">>, <<"PLUGIN_NON_WEB">>), State};
				{ok, {Mod, Func}} ->
					Reply = erlang:apply(Mod, Func, [State#state.agent_fsm, {struct, []}, Args]),
					case Reply of
						{error, {Msg, Code}} when is_binary(Msg), is_binary(Code) ->
							{reply, ?reply_err(Msg, Code), State};
						{error, Else} ->
							Msg = io_lib:format("~p", [Else]),
							Msg0 = list_to_binary(Msg),
							{reply, ?reply_err(Msg0, <<"UNKNOWN_ERROR">>), State};
						{ok, Json} ->
							{reply, ?reply_success(Json), State}
					end
			end
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
	Json = {struct, [{success, false}, {<<"message">>, <<"not yet implemented">>}, {<<"errcode">>, <<"NYI">>}]},
%	AgentRec = agent:dump_state(Apid), % TODO - avoid
%	Json = case cpx:get_env(ring_manager) of
%		{ok, Module} when AgentRec#agent.state == released ->
%			HandleEvent = fun(EventName, _Data, {FsNode, UUID}, FunState) ->
%				case EventName of
%					"CHANNEL_ANSWER" ->
%						freeswitch:sendmsg(FsNode, UUID, [
%							{"call-command", "execute"},
%							{"execute-app-name", "delay_echo"},
%							{"execute-app-arg", "1000"}
%						]),
%						{noreply, FunState};
%					_ ->
%						{noreply, FunState}
%				end
%			end,
%			case Module:ring(AgentRec, [{handle_event, HandleEvent}], [no_oncall_on_bridge]) of
%				{ok, _} ->
%					{struct, [{success, true}]};
%				{error, Error} ->
%					{struct, [{success, false}, {<<"message">>, iolist_to_binary(io_lib:format("ring test failed: ~p", [Error]))}, {<<"errcode">>, <<"UNKNOWN_ERROR">>}]}
%			end;
%		undefined ->
%			{struct, [{success, false}, {<<"message">>, <<"no ring manager available">>}, {<<"errcode">>, <<"MEDIA_NOEXISTS">>}]};
%		_ ->
%			{struct, [{success, false}, {<<"message">>, <<"you must be released to perform a ring test">>}, {<<"errcode">>, <<"INVALID_STATE">>}]} 
%	end, 
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
handle_call({undefined, Path, Post}, _From, State) ->
	?DEBUG("Forwarding to hooks for handling:  ~p", [Path]),
	case cpx_hooks:trigger_hooks(agent_web_path, [Path, Post, State#state.current_call]) of
		{ok, Binary} when is_binary(Binary) ->
			{reply, {200, [], Binary}, State};
		{ok, {Status, Headers, Binary} = Out} when is_integer(Status), is_list(Headers), is_binary(Binary) ->
			{reply, Out, State};
		{ok, ok} ->
			{reply, ?simple_success(), State};
		{ok, {json, Json}} ->
			{reply, ?reply_success(Json), State};
		{ok, {Errcode, ErrMsg}} ->
			{reply, ?reply_err(ErrMsg, Errcode), State};
		{error, unhandled} ->
			{reply, {404, [], <<"not_found">>}, State};
		Err ->
			Msg = list_to_binary(io_lib:format("~p", [Err])),
			{reply, ?reply_err(Msg, <<"UNKNOWN_ERROR">>), State}
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
		{agent, Agent}
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
handle_cast({mediapush, ChanPid, _Callrec, {mediaload, #call{source_module = email_media} = Call}}, State) ->
	?WARNING("Media specific mediapush handling!", []),
	Midstate = case State#state.current_call of
		expect ->
			State#state{current_call = Call};
		_ ->
			State
	end,
	Json = {struct, [
		{<<"command">>, <<"mediaload">>},
		{<<"channelid">>, list_to_binary(pid_to_list(ChanPid))},
		{<<"media">>, Call#call.source_module}
	]},
	Newstate = push_event(Json, Midstate),
	{noreply, Newstate#state{mediaload = []}};
handle_cast({mediapush, ChanPid, _Callrec, {mediaload, #call{source_module = freeswitch_media} = Call, _}}, State) ->
	?WARNING("Media specific mediapush handling!", []),
	Json = {struct, [
		{<<"command">>, <<"mediaload">>},
		{<<"channelid">>, list_to_binary(pid_to_list(ChanPid))},
		{<<"media">>, Call#call.source_module}
	]},
	Newstate = push_event(Json, State),
	{noreply, Newstate};
%handle_cast({mediaload, #call{type = voice}}, State) ->
%	Json = {struct, [
%		{<<"command">>, <<"mediaload">>},
%		{<<"media">>, <<"voice">>},
%		{<<"fullpane">>, false}
%	]},
%	Newstate = push_event(Json, State),
%	{noreply, Newstate#state{mediaload = [{<<"fullpane">>, false}]}};
%handle_cast({mediaload, #call{type = voice}, Options}, State) ->
%	Base = [
%		{<<"command">>, <<"mediaload">>},
%		{<<"media">>, <<"voice">>},
%		{<<"fullpane">>, false}
%	],
%	Json = {struct, lists:append(Base, Options)},
%	Newstate = push_event(Json, State),
%	{noreply, Newstate#state{mediaload = [{<<"fullpane">>, false} | Options]}};
%handle_cast({mediaload, #call{type = voicemail}, Options}, State) ->
%	Base = [
%		{<<"command">>, <<"mediaload">>},
%		{<<"media">>, <<"voicemail">>},
%		{<<"fullpane">>, false}
%	],
%	Json = {struct, lists:append(Base, Options)},
%	Newstate = push_event(Json, State),
%	{noreply, Newstate#state{mediaload = [{<<"fullpane">>, false} | Options]}};
% TODO agent_web_connection should not interfere
handle_cast({mediapush, Chanpid, #call{source_module = email_media}, Data}, State) ->
	?WARNING("Media specific mediapush handling!", []),
	Chanid = list_to_binary(pid_to_list(Chanpid)),
			case Data of
				send_done ->
					Json = {struct, [
						{<<"channelid">>, Chanid},
						{<<"command">>, <<"mediaevent">>},
						{<<"media">>, email},
						{<<"event">>, <<"send_complete">>},
						{<<"success">>, true}
					]},
					Newstate = push_event(Json, State),
					{noreply, Newstate};
				{send_fail, Error} ->
					Json = {struct, [
						{<<"channelid">>, Chanid},
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
handle_cast({mediapush, Chanpid, #call{source_module = freeswitch_media} = Call, Data}, State) ->
	?WARNING("Media specific mediapush handling!", []),
	Mediatype = Call#call.type,
	?DEBUG("mediapush type:  ~p;  Data:  ~p", [Mediatype, Data]),
	Chanid = list_to_binary(pid_to_list(Chanpid)),
	case Data of
		SimpleCommand when is_atom(SimpleCommand) ->
			Json = {struct, [
				{<<"channelid">>, Chanid},
				{<<"command">>, <<"mediaevent">>},
				{<<"media">>, voice},
				{<<"event">>, SimpleCommand}
			]},
			Newstate = push_event(Json, State),
			{noreply, Newstate}
	end;

handle_cast({mediapush, Chanpid, Call, {Command, Call, Data} = Fd}, State) ->
	?DEBUG("mediapush unmolested by agent web connection: ~p", [Fd]),
	Chanid = list_to_binary(pid_to_list(Chanpid)),
	Json = {struct, [
		{<<"channelid">>, Chanid},
		{<<"command">>, Command},
		{<<"media">>, Call#call.type},
		{<<"event">>, Data}
	]},
	Newstate = push_event(Json, State),
	{noreply, Newstate};

handle_cast({set_salt, Salt}, State) ->
	{noreply, State#state{salt = Salt}};

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
	NewState = push_event(Json, State),
	{noreply, NewState};

handle_cast({set_channel, Pid, StateName, Statedata}, #state{agent_channels = AChannels} = State) ->
	Headjson = {struct, [
		{<<"command">>, <<"setchannel">>},
		{<<"state">>, StateName},
		{<<"statedata">>, encode_statedata(Statedata)},
		{<<"channelid">>, list_to_binary(pid_to_list(Pid))}
	]},
	NewAChannels = case {Statedata, dict:find(Pid, AChannels)} of
		{Call, error} when is_record(Call, call), StateName =:= wrapup ->
			Store = #channel_state{mediaload = undefined, current_call = Call},
			dict:store(Pid, Store, AChannels);
		{Call, error} when is_record(Call, call) ->
			Store = #channel_state{current_call = Call, mediaload = Call},
			dict:store(Pid, Store, AChannels);
		{Call, {ok, Cache}} when StateName =:= wrapup, is_record(Call, call) ->
			Store = Cache#channel_state{mediaload = undefined, current_call = Call},
			dict:store(Pid, Store, AChannels);
		{Call, {ok, Cache}} ->
			Store = Cache#channel_state{current_call = Call},
			dict:store(Pid, Store, AChannels);
		{{Call, Number}, error} ->
			Store = #channel_state{mediaload = Call, current_call = Call},
			dict:store(Pid, Store, AChannels);
		{{Call, Number}, {ok, Cache}} ->
			Store = Cache#channel_state{mediaload = Call, current_call = Call},
			dict:store(Pid, Store, AChannels)
	end,
	NewState = push_event(Headjson, State#state{agent_channels = NewAChannels}),
	{noreply, NewState};

handle_cast({channel_died, Pid, NewAvail}, #state{agent_channels = AChannels} = State) ->
	Json = {struct, [
		{<<"command">>, <<"endchannel">>},
		{<<"channelid">>, list_to_binary(pid_to_list(Pid))},
		{<<"availableChannels">>, NewAvail}
	]},
	NewDict = dict:erase(Pid, AChannels),
	NewState = push_event(Json, State#state{agent_channels = NewDict}),
	{noreply, NewState};

%handle_cast({change_state, AgState, Data}, State) ->
%	%?DEBUG("State:  ~p; Data:  ~p", [AgState, Data]),
%	Headjson = {struct, [
%		{<<"command">>, <<"astate">>},
%		{<<"state">>, AgState},
%		{<<"statedata">>, encode_statedata(Data)}
%	]},
%	Newstate = push_event(Headjson, State),
%	{noreply, Midstate} = case Data of
%		Call when is_record(Call, call) ->
%			{noreply, Newstate#state{current_call = Call}};
%		{onhold, Call, calling, _Number} ->
%			{noreply, Newstate#state{current_call = Call}};
%		_ ->
%			{noreply, Newstate#state{current_call = undefined}}
%	end,
%	Fullstate = case AgState of
%		wrapup ->
%			Midstate#state{mediaload = undefined};
%		_ ->
%			Midstate
%	end,
%	{noreply, Fullstate};
%handle_cast({change_state, AgState}, State) ->
%	Headjson = {struct, [
%			{<<"command">>, <<"astate">>},
%			{<<"state">>, AgState}
%		]},
%	Midstate = push_event(Headjson, State),
%	Newstate = case AgState of
%		wrapup ->
%			Midstate#state{mediaload = undefined};
%		_ ->
%			Midstate
%	end,
%	{noreply, Newstate#state{current_call = undefined}};
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
handle_cast({new_endpoint, _Module, _Endpoint}, State) ->
	%% TODO update media in poll
	{noreply, State};
handle_cast({arbitrary_command, Command, JsonProps}, State) ->
	Headjson = {struct, [{<<"command">>, Command} | JsonProps]},
	Newstate = push_event(Headjson, State),
	{noreply, Newstate};
handle_cast(get_tabs_menu, State) ->
	#state{agent_fsm = Apid} = State,
	spawn_get_tabs_menu(Apid),
	{noreply, State};
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

spawn_get_tabs_menu(Apid) ->
	Conn = self(),
	spawn(fun() -> spawn_get_tabs_menu(Conn, Apid) end).

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
	?MODULE:arbitrary_command(Conn, set_tabs_menu, [{<<"tabs">>, Tabs}]).

fetch_channel(Channel, Channels) when is_binary(Channel) ->
	fetch_channel(binary_to_list(Channel), Channels);
fetch_channel(Channel, Channels) ->
	?DEBUG("The chan:  ~p, The channels:  ~p", [Channel, Channels]),
	Chans = [C || C <- dict:fetch_keys(Channels), pid_to_list(C) =:= Channel],
	case Chans of
		[] ->	none;
		[Chan] -> {Chan, dict:fetch(Chan, Channels)}
	end.

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
parse_media_call(#call{source_module = email_media}, {agent_web_connection, <<"attach">>, _Args}, {ok, Filenames}) ->
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
parse_media_call(#call{source_module = email_media}, {agent_web_connection, <<"attach">>, _Args}, {error, Error}) ->
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
parse_media_call(#call{source_module = email_media}, {agent_web_connection, <<"detach">>, _Args}, {ok, Keys}) ->
	Binnames = lists:map(fun(N) -> list_to_binary(N) end, Keys),
	Json = {struct, [
		{success, true},
		{<<"result">>, Binnames}
	]},
	{[], mochijson2:encode(Json)};
parse_media_call(#call{source_module = email_media}, {agent_web_connection, <<"get_skeleton">>, _Args}, {Type, Subtype, Heads, Props}) ->
	Json = {struct, [
		{<<"type">>, Type}, 
		{<<"subtype">>, Subtype},
		{<<"headers">>, email_props_to_json(Heads)},
		{<<"properties">>, email_props_to_json(Props)}
	]},
	{[], mochijson2:encode({struct, [{success, true}, {<<"result">>, Json}]})};
parse_media_call(#call{source_module = email_media}, {agent_web_connection, <<"get_skeleton">>, _Args}, {TopType, TopSubType, Tophead, Topprop, Parts}) ->
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
parse_media_call(#call{source_module = email_media}, {agent_web_connection, <<"get_path">>, _Path}, {ok, {Type, Subtype, _Headers, _Properties, Body} = Mime}) ->
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
			Outjson = {struct, [
				{success, true},
				{result, mochiweb_html:to_html({<<"span">>, [], Newhtml})}
			]},
			{[], mochijson2:encode(Outjson)};
		{Type, Subtype, _Disposition} ->
			%% well, here's hoping it doesn't explode later.
			{[{"Content-Type", lists:append([binary_to_list(Type), "/", binary_to_list(Subtype)])}], mochijson2:encode({struct, [{success, true},{<<"result">>, Body}]})}
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
parse_media_call(#call{source_module = email_media}, {agent_web_connection, <<"get_path">>, _Path}, {message, Bin}) when is_binary(Bin) ->
	%?DEBUG("Path is a message/Subtype with binary body", []),
	{[], Bin};
parse_media_call(#call{source_module = email_media}, {agent_web_connection, <<"get_from">>, _}, undefined) ->
	{[], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"no reply info">>}, {<<"errcode">>, <<"REPLYINFO_NOEXISTS">>}]})};
parse_media_call(#call{source_module = email_media}, {agent_web_connection, <<"get_from">>, _}, {Label, Address}) ->
	Json = {struct, [
		{<<"label">>, Label},
		{<<"address">>, Address}
	]},
	{[], mochijson2:encode({struct, [{success, true}, {<<"result">>, Json}]})};
parse_media_call(Mediarec, Command, Response) ->
	?WARNING("Unparsable result for ~p:~p.  ~p", [Mediarec#call.source_module, element(2, Command), Response]),
	{[], mochijson2:encode({struct, [{success, false}, {<<"message">>, <<"unparsable result for command">>}, {<<"errcode">>, <<"BAD_RETURN">>}]})}.

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
		{<<"profile">>, list_to_binary(Agent#agent.profile)}
		%{<<"remotenumber">>, Remnum}
	],
	Proplist = Prestatedata,
	{struct, Proplist}.

encode_agents([], Acc) -> 
	lists:reverse(Acc);
encode_agents([Head | Tail], Acc) ->
	encode_agents(Tail, [encode_agent(Head) | Acc]).

encode_call(Call) when is_record(Call, call) ->
	{struct, [
		{<<"id">>, list_to_binary(Call#call.id)},
		{<<"type">>, Call#call.type},
		{<<"source_module">>, Call#call.source_module},
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
	Label = if
		Client#client.label == undefined -> <<"Default">>;
		is_atom(Client#client.label) -> Client#client.label;
		true -> list_to_binary(Client#client.label)
	end,
	Id = if
		is_atom(Client#client.id) -> Client#client.id;
		true -> list_to_binary(Client#client.id)
	end,
	{struct, [
		{<<"label">>, Label},
		{<<"id">>, Id}
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

set_endpoint_int(Conn, Type, {struct, Data}, DataToOptsFun) ->
	case DataToOptsFun(Data) of
		{error, Error} ->
			?reply_err(iolist_to_binary(io_lib:format("error with input: ~p", [Error])), <<"INVALID_ENDPOINT">>);
		Opts ->
			case gen_server:call(Conn, {set_endpoint, Type, Opts}) of
				ok ->
					?simple_success();
				{error, Error2} ->
					?reply_err(iolist_to_binary(io_lib:format("error setting endpoint: ~p", [Error2])), <<"INVALID_ENDPOINT">>)
			end
	end.


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
		{ok, Seedstate} = init([Agent]),
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
				{struct, [{<<"success">>, true}, {<<"result">>, [<<"string1">>, <<"string2">>]}]} = mochijson2:decode(Json),
				ok
			end),
			gen_server_mock:expect_info(WebListener, fun({poll, {200, [], Json}}, _) ->
				{struct, [{<<"success">>, true}, {<<"result">>, [<<"string3">>]}]} = mochijson2:decode(Json),
				ok
			end),
			gen_server_mock:expect_info(WebListener, fun({poll, {200, [], Json}}, _) ->
				{struct, [{<<"success">>, true}, {<<"result">>, [<<"string4">>, <<"string5">>]}]} = mochijson2:decode(Json),
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
			gen_event:start({local, cpx_agent_event}),
			gen_leader_mock:start(agent_manager),
			gen_leader_mock:expect_leader_call(agent_manager, 
				fun({exists, "testagent"}, _From, State, _Elec) -> 
					{ok, Apid} = agent:start(#agent{login = "testagent"}),
					{ok, {true, Apid}, State} 
				end),
			gen_leader_mock:expect_cast(agent_manager, fun(_,_,_) -> ok end),
			{ok, Connpid} = agent_web_connection:start(#agent{login = "testagent", skills = [english]}),
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
					gen_leader_mock:expect_cast(agent_manager, fun(_, _, _) -> ok end),
					{200, [], Reply} = gen_server:call(Connpid, {set_release, <<"none">>}),
					?assertEqual({struct, [{<<"success">>, true}]}, mochijson2:decode(Reply)),
					gen_leader_mock:expect_cast(agent_manager, fun(_,_,_) -> ok end),
					{200, [], Reply2} = gen_server:call(Connpid, {set_release, <<"default">>}),
					?assertEqual({struct, [{<<"success">>, true}]}, mochijson2:decode(Reply2))
				end}
			end
		]
	}.

dummy_plugin(_AgentPid, _ReplyBase, <<"success">>) ->
	{ok, <<"success">>};
dummy_plugin(_AgentPid, _ReplyBase, [Msg, Code]) ->
	{error, {Msg, Code}};
dummy_plugin(_AgentPid, _ReplyBase, Error) ->
	{error, Error}.

simplify_web_response({reply, HttpData, _State}) ->
	simplify_web_response(HttpData);

simplify_web_response({200, _Headers, Json}) ->
	simplify_web_response(Json);

simplify_web_response({StatusCode, _Headers, _Content}) ->
	{error, {bad_status, StatusCode}};

simplify_web_response({struct, Props}) ->
	case proplists:get_value(<<"success">>, Props, false) of
		false ->
			Msg = proplists:get_value(<<"message">>, Props, <<"missing_msg">>),
			Code = proplists:get_value(<<"errcode">>, Props, <<"missing_code">>),
			{error, {Code, Msg}};
		true ->
			Res = proplists:get_value(<<"result">>, Props),
			{ok, Res}
	end;

simplify_web_response(Json) ->
	Decoded = mochijson2:decode(Json),
	simplify_web_response(Decoded).

plugin_call_test_() ->
	{setup,
	fun() ->
		meck:new(application, [unstick, passthrough]),
		meck:expect(application, which_applications, fun() ->
			[{'OpenACD', "OpenACD", "1.0.0"},
			{kernel, "ERTS  CXC 138 10", "2.14.5"}]
		end),
		application:set_env('OpenACD', agent_web_handler, {agent_web_connection, dummy_plugin}),
		#state{}
	end,
	fun(_) ->
		application:unset_env('OpenACD', agent_web_handler),
		meck:unload(application)
	end,
	fun(State) -> [
		{"no plugin", fun() ->
			GRes = handle_call({plugin_call, "missing_app", "any"}, "from", State),
			SimpleRes = simplify_web_response(GRes),
			Expected = {error, {<<"PLUGIN_NOEXISTS">>, <<"No such plugin">>}},
			?assertEqual(Expected, SimpleRes)
		end},
		{"Plugin isn't responding to web", fun() ->
			GRes = handle_call({plugin_call, "kernel", "any"}, "from", State),
			SimpleRes = simplify_web_response(GRes),
			Expected = {error,
				{<<"PLUGIN_NON_WEB">>, <<"Plugin doesn't handle web">>}
			},
			?assertEqual(Expected, SimpleRes)
		end},
		{"Plugin simple success", fun() ->
			GRes = handle_call({plugin_call, "OpenACD", <<"success">>}, "from", State),
			SimpleRes = simplify_web_response(GRes),
			Expected = {ok, <<"success">>},
			?assertEqual(Expected, SimpleRes)
		end}
	] end}.

get_endpoint_test_() ->
	{
		foreach,
		fun() ->
			gen_event:start({local, cpx_agent_event}),
			gen_server_mock:named({local, freeswitch_media_manager}),

			gen_leader_mock:start(agent_manager),
			gen_leader_mock:expect_leader_call(agent_manager, 
				fun({exists, "testagent"}, _From, State, _Elec) -> 
					{ok, Apid} = agent:start(#agent{login = "testagent"}),
					{ok, {true, Apid}, State} 
				end),
			gen_leader_mock:expect_cast(agent_manager, fun(_,_,_) -> ok end),

			{ok, Connpid} = agent_web_connection:start(#agent{login = "testagent", skills = [english]}),


			Connpid
		end,
		fun(Connpid) ->
			%stop(Connpid),
			%gen_server_mock:stop(FMMPid)
			ok
		end,
		[
			fun(Connpid) ->
				{"Get freeswitch endpoint",

				fun() ->
					meck:new(freeswitch_ring),
					meck:expect(freeswitch_ring, start, fun(_Node, _, _) -> {ok, zoo} end),

					gen_server_mock:expect_call(freeswitch_media_manager, fun(_, _From, State) -> {ok, 
						{'freeswitch@127.0.0.1', "dialstring", "dest"}, State} end),

					gen_leader_mock:expect_cast(agent_manager, fun(_,_,_) -> ok end),

					AgentPid = (dump_state(Connpid))#state.agent_fsm,
					agent:set_endpoint(AgentPid, freeswitch_media, [{type, sip}, {data, "foobar"}, {persistant, true}]),
					
					{ok, {struct, Props}} = simplify_web_response(get_endpoint(Connpid, <<"freeswitch_media">>)),

					?debugVal(Props),

					?assertEqual(<<"sip">>, proplists:get_value(<<"type">>, Props)),
					?assertEqual(<<"foobar">>, proplists:get_value(<<"data">>, Props)),
					?assertEqual(true, proplists:get_value(<<"persistant">>, Props)),

					?assert(meck:validate(freeswitch_ring)),
					meck:unload(freeswitch_ring)
				end}
			end,
			fun(Connpid) ->
				{"Get email endpoint",
				fun() ->
					AgentPid = (dump_state(Connpid))#state.agent_fsm,
					agent:set_endpoint(AgentPid, email_media, null),
					?assertEqual({ok, null}, simplify_web_response(
						get_endpoint(Connpid, <<"email_media">>)))
				end}
			end
		]
	}.

set_endpoint_test_() ->
	{
		foreach,
		fun() ->
			gen_event:start({local, cpx_agent_event}),
			gen_server_mock:named({local, freeswitch_media_manager}),

			gen_leader_mock:start(agent_manager),
			gen_leader_mock:expect_leader_call(agent_manager, 
				fun({exists, "testagent"}, _From, State, _Elec) -> 
					{ok, Apid} = agent:start(#agent{login = "testagent"}),
					{ok, {true, Apid}, State} 
				end),
			gen_leader_mock:expect_cast(agent_manager, fun(_,_,_) -> ok end),

			{ok, Connpid} = agent_web_connection:start(#agent{login = "testagent", skills = [english]}),


			Connpid
		end,
		fun(Connpid) ->
			%stop(Connpid),
			%gen_server_mock:stop(FMMPid)
			ok
		end,
		[
			fun(Connpid) ->
				{"Set unknown endpoint",
				fun() ->
					?assertEqual(
						{error,{<<"INVALID_ENDPOINT">>,
                                    <<"unknwon endpoint">>}},
						simplify_web_response(
						set_endpoint(Connpid, <<"blabla">>, {struct, []})))
				end}
			end,
			fun(Connpid) ->
				{"Set freeswitch endpoint",
				fun() ->
					meck:new(freeswitch_ring),
					meck:expect(freeswitch_ring, start, fun(_Node, _, _) -> {ok, zoo} end),

					[test_valid_set_fw_endpoint(Connpid, TypeIn, <<"somedata">>, PersistantIn, Type, "somedata", Persistant) ||
						{TypeIn, Type} <- 
							[{<<"sip_registration">>, sip_registration},
							{<<"sip">>, sip},
							{<<"iax">>, iax},
							{<<"h323">>, h323},
							{<<"pstn">>, pstn}],
						{PersistantIn, Persistant} <-
							[{true, true}, {false, undefined}, {undefined, undefined}]],
					
					?assert(meck:validate(freeswitch_ring)),
					meck:unload(freeswitch_ring)
				end}
			end,
			fun(Connpid) ->
				{"Set email endpoint",
				fun() ->
					gen_leader_mock:expect_cast(agent_manager, fun(_,_,_) -> ok end),

					?assertEqual(
						{ok, undefined},
						simplify_web_response(
						set_endpoint(Connpid, <<"email_media">>, null))),
					?assertNotMatch({error, _}, agent:get_endpoint(	
						email_media, dump_agent(Connpid)))
				end}
			end,
			fun(Connpid) ->
				{"Set dummy endpoint",
				fun() ->
					gen_leader_mock:expect_cast(agent_manager, fun(_,_,_) -> ok end),

					?assertEqual(
						{ok, undefined},
						simplify_web_response(
						set_endpoint(Connpid, <<"email_media">>, null)))
				end}
			end
		]
	}.



test_valid_set_fw_endpoint(Connpid, TypeIn, DataIn, PersistantIn, Type, Data, Persistant) ->

	gen_server_mock:expect_call(freeswitch_media_manager, fun(_, _From, State) -> {ok, 
		{'freeswitch@127.0.0.1', "dialstring", "dest"}, State} end),

	gen_leader_mock:expect_cast(agent_manager, fun(_,_,_) -> ok end),

	?assertEqual(
		{ok, undefined},
		simplify_web_response(
		set_endpoint(Connpid, <<"freeswitch_media">>,
			{struct, [
				{<<"type">>, TypeIn},
				{<<"data">>, DataIn},
				{<<"persistant">>, PersistantIn}
			]}))),
	Agent = dump_agent(Connpid),
	{ok, {Opts, _}} = agent:get_endpoint(freeswitch_media,
	 	Agent),
	
	?assertEqual(Type,
	 	proplists:get_value(type, Opts)),
	?assertEqual(Data,
		proplists:get_value(data, Opts)),
	?assertEqual(Persistant,
		proplists:get_value(persistant, Opts)).

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
