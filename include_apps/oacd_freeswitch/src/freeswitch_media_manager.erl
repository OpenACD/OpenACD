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

%% @doc The controlling module for connection OpenACD to a freeswitch 
%% installation.  There are 2 primary requirements for this work:  
%% the freeswitch installation must have mod_erlang installed and active,
%% and the freeswitch dialplan must add the following variables added
%% to the call data:
%% <dl>
%% <dt>queue</dt><dd>The name of the queue as configured in OpenACD</dd>
%% <dt>brand</dt><dd>The Id of the client to associate the call with</dd>
%% </dl>
%% Primary job of this module is to listen to freeswitch for events, and 
%% shove those events to the appriate child process.
%%
%% This also acts as the reference implementation of a ring_manager.  A ring
%% manager is responsible for handling requests from agents to create a ring
%% channel.
%%
%% A proper ring channel will respond to `{ring, AgentRec, Callrec}' with 
%% either `{error, What}' or `{ok, Pid, Paths}' where pid is the process id the
%% passed in agent will use as it's ring channel.
%%
%% The important parts of the agent record are: <ul>
%% <li>`Agent#agent.endpointtype'</li>
%% <li>`Agent#agent.endpointdata'</li>
%% <li>`Agent#agent.login'</li>
%% </ul>
%%
%% The endpoint type is a 3 element tuple.  The first element is the ring
%% channel pid, if there is one.  This can usually be ignored as the agent
%% will requrest a ring only if it does not have a ring pid.  The second 
%% element is either `persistent' or `transient'.  `persistent' means the 
%% will use the returned pid for any future ringing.  `transient' means the
%% returned pid will be used only for the given ring duration and then 
%% promply dropped like a bad habit.  The final bit is an end point type
%% the agent process feels may be helpful to ring itself.  The values passed
%% by default are useful to freeswitch_media_manager, though this may not be
%% true for other ring_managers.
%%
%% Endpointdata is a string the agent supplies which may help ring itself.
%% In the case of freeswitch_media_manager, the endpointdata or login are 
%% combined with the sip, iax2, h323, or dialstring options (depending on
%% the endpointtype).
%% 
%% Finally, the Callrec will be either `persistent' when setting up a 
%% persistent ring channel, `test' when checking to see if an agent is 
%% ringable, or the actual call record the ring channel will be used to 
%% ring for.  This allows the ring_manager to tailor the ring channel used 
%% to the media that is needed, without the agent needed to know much.  In 
%% freeswith_media_manager's case, this allows it to choose between 
%% freeswitch_transient that will die when answered (non freeswitch media) 
%% or hang around until the call ends (freeswitch medias).  For persistent 
%% ring channels, they will get the call record when they need to start 
%% ringing, allowing them to make the choice.
%%
%% That just leaves that `Paths' varaible in the success return.  `Paths' 
%% indicates which interactions can be requrested by the agent.  In the case
%% of a persisitant channel with is ignored since the channel cannot be 
%% answered or hungup on outside of OpenACD.  However, for transient 
%% channels, the answer can happen outside the agent interface.  Furthermore
%% some medias cannot be answered or hungup on within OpenACD.  Thus, there
%% are 4 possible responses for `Paths':<ul>
%% <li>`both'</li>
%% <li>`answer'</li>
%% <li>`hangup'</li>
%% <li>`neither'</li>
%% </ul>
%% `both' indicates an agent can answer and hangup the media from the agent
%% interface.  `answer' means only answering the media can be done.
%% `hangup' means only hanging up the media.  Finally, `neither' means 
%% answering and hanging up must be done through different means, such as 
%% the softphone.
%% channel can be answered 
%% @see freeswitch_media

-module(freeswitch_media_manager).
-author("Micah").

-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("OpenACD/include/log.hrl").
-include_lib("OpenACD/include/queue.hrl").
-include_lib("OpenACD/include/call.hrl").
-include_lib("OpenACD/include/agent.hrl").

-define(TIMEOUT, 10000).
-define(default_dial_string(Type), case Type of
		sip_registration -> "sofia/internal/$1%";
		sip -> "sofia/internal/sip:$1";
		iax2 -> "iax2/$1";
		h323 -> "opal/h3232:$1";
		pstn -> "";
		rtmp -> "rtmp/$1"
end).
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
				<variables>
					<variable name=\"user_context\" value=\"default\"/>
				</variables>
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
	start_link/2, 
	start/2, 
	stop/0,
	get_handler/1,
	notify/2,
	make_outbound_call/3,
	record_outage/3,
	fetch_domain_user/2,
	new_voicemail/5,
	ring_agent/3,
	ring_agent/4,
	ring_agent_echo/4,
	%ring/3,
	ring/4,
	get_media/1,
	get_ring_data/2,
	get_default_dial_string/0,
	do_dial_string/3,
	get_agent_dial_string/2,
	monitor_agent/2,
	monitor_client/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	nodename :: atom(),
	freeswitch_up = false :: boolean(),
	call_dict = dict:new() :: dict(),
	dialstring = "" :: string(),
	eventserver :: pid() | 'undefined',
	xmlserver :: pid() | 'undefined',
	fetch_domain_user :: []
	}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include_lib("OpenACD/include/gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================

-type(domain_opt() :: {domain, string()}).
-type(dialstring_opt() :: {dialstring, string()}). % dialing externals
-type(sip_opt() :: {sip, string()}).
-type(iax2_opt() :: {iax2, string()}).
-type(h323_opt() :: {h323, string()}).
-type(rtmp_opt() :: {rtmp, string()}).
-type(not_ring_manager_opt() :: 'not_ring_manager').
-type(start_opt() :: 
	domain_opt() |
	dialstring_opt() |
	sip_opt() |
	iax2_opt() |
	h323_opt() |
	rtmp_opt() |
	not_ring_manager_opt()
).
-type(start_opts() :: [start_opt()]).
%% @doc Start the media manager unlinked to the parant process with C node `node() Nodemane' 
%% and `[{atom(), term()] Options'.
%% <ul>
%% <li>`domain :: string()'</li>
%% <li>`dialstring :: string()'</li>
%% </ul>
-spec(start/2 :: (Nodename :: atom(), Options :: start_opts()) -> {'ok', pid()}).
start(Nodename, [Head | _Tail] = Options) when is_tuple(Head) ->
	gen_server:start({local, ?MODULE}, ?MODULE, [Nodename, Options], []).
	
%% @doc Start the media manager linked to the parant process with C node `node() Nodemane' 
%% and `[{atom(), term()] Options'.
%% <ul>
%% <li>`domain :: string()'</li>
%% <li>`dialstring :: string()'</li>
%% <li>`sip :: string()'</li>
%% <li>`iax2 :: string()'</li>
%% <li>`h323 :: string()'</li>
%% <li>`rtmp :: string()'</li>
%% </ul>
-spec(start_link/2 :: (Nodename :: atom(), Options :: start_opts()) -> {'ok', pid()}).
start_link(Nodename, [Head | _Tail] = Options) when is_tuple(Head) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Nodename, Options], []).

%% @doc returns {`ok', pid()} if there is a freeswitch media process handling the given `UUID'.
-spec(get_handler/1 :: (UUID :: string()) -> {'ok', pid()} | 'noexists').
get_handler(UUID) -> 
	gen_server:call(?MODULE, {get_handler, UUID}).

-spec(notify/2 :: (UUID :: string(), Pid :: pid()) -> 'ok').
notify(UUID, Pid) ->
	gen_server:cast(?MODULE, {notify, UUID, Pid}).

-spec(make_outbound_call/3 :: (Client :: any(), AgentPid :: pid(), Agent :: string()) -> {'ok', pid()} | {'error', any()}).
make_outbound_call(Client, AgentPid, Agent) ->
	gen_server:call(?MODULE, {make_outbound_call, Client, AgentPid, Agent}).

-spec(record_outage/3 :: (Client :: any(), AgentPid :: pid(), Agent :: string()) -> 'ok' | {'error', any()}).
record_outage(Client, AgentPid, Agent) ->
	gen_server:call(?MODULE, {record_outage, Client, AgentPid, Agent}).

-spec(new_voicemail/5 :: (UUID :: string(), File :: string(), Queue :: string(), Priority :: pos_integer(), Client :: #client{} | string()) -> 'ok').
new_voicemail(UUID, File, Queue, Priority, Client) ->
	gen_server:cast(?MODULE, {new_voicemail, UUID, File, Queue, Priority, Client}).

-spec(stop/0 :: () -> 'ok').
stop() ->
	gen_server:call(?MODULE, stop).

%ring(#agent{endpointdata = undefined} = Agent, RingCbs, Options) ->
%	ring(Agent#agent.endpointtype, Agent#agent.login, RingCbs, Options);
%ring(Agent, RingCbs, Options) when is_record(Agent, agent) ->
%	ring(Agent#agent.endpointtype, Agent#agent.endpointdata, RingCbs, Options).

ring(EndpointType, Dialstring, RingCbs, Options) ->
	gen_server:call(?MODULE, {ring, EndpointType, Dialstring, RingCbs, Options}).

% @doc Just blindly start an agent's phone ringing, set to hangup on pickup.
% Yes, this is a prank call function.
-spec(ring_agent/4 :: (AgentPid :: pid(), Agent :: string(), Call :: #call{}, Timeout :: pos_integer()) -> {'ok', pid()} | {'error', any()}).
ring_agent(AgentPid, Agent, Call, Timeout) ->
	gen_server:call(?MODULE, {ring_agent, AgentPid, Agent, Call, Timeout}).

% @doc This functions primary use is to create a persistent ring channel to
% the agent; ie:  off hook agent.  `Opts' is the same as what 
% {@link freeswitch_ring:start/5} takes.
% @see freeswitch_ring:start/5
-spec(ring_agent/3 :: (Apid :: pid(), Agent :: #agent{}, Opts :: [any()]) -> {'ok', pid()} | {'error', any()}).
ring_agent(Apid, Agent, Opts) ->
	gen_server:call(?MODULE, {ring_agent, Apid, Agent, Opts}).

% @doc Just blindly start an agent's phone ringing, set to echo test on pickup.
% Yes, this is a prank call function.
-spec(ring_agent_echo/4 :: (AgentPid :: pid(), Agent :: string(), Call :: #call{}, Timeout :: pos_integer()) -> {'ok', pid()} | {'error', any()}).
ring_agent_echo(AgentPid, Agent, Call, Timeout) ->
	gen_server:call(?MODULE, {ring_agent_echo, AgentPid, Agent, Call, Timeout}).

-spec(get_media/1 :: (MediaKey :: pid() | string()) -> {string(), pid()} | 'none').
get_media(MediaKey) ->
	gen_server:call(?MODULE, {get_media, MediaKey}).

get_ring_data(Agent, Options) ->
	gen_server:call(?MODULE, {get_ring_data, Agent, Options}).

-spec(get_default_dial_string/0 :: () -> string()).
get_default_dial_string() ->
	gen_server:call(?MODULE, get_default_dial_string).

-spec(get_agent_dial_string/2 :: (AgentRec :: #agent{}, Options :: [string()]) -> string()).
get_agent_dial_string(AgentRec, Options) ->
	gen_server:call(?MODULE, {get_agent_dial_string, AgentRec, Options}).

% TODO - need better support for forked dial strings
-spec(do_dial_string/3 :: (DialString :: string(), Destination :: string(), Options :: [string()]) -> string()).
do_dial_string(DialString, Destination, []) ->
	re:replace(DialString, "\\$1", Destination, [{return, list}]);
do_dial_string([${ | _] = DialString, Destination, Options) ->
	% Dialstring begins with a {} block.
	% escape any commas in the string, unless they're already escaped
	EscapedOptions = [re:replace(Option, "([^\\\\]),", "\\1\\\\,", [{return, list}, global]) ||
		Option <- Options],
	D1 = re:replace(DialString, "\\$1", Destination, [{return, list}]),
	re:replace(D1, "^{", "{" ++ string:join(EscapedOptions, ",") ++ ",", [{return, list}]);
do_dial_string(DialString, Destination, Options) ->
	% escape any commas in the string, unless they're already escaped
	EscapedOptions = [re:replace(Option, "([^\\\\]),", "\\1\\\\,", [{return, list}, global]) ||
		Option <- Options],
	D1 = re:replace(DialString, "\\$1", Destination, [{return, list}]),
	"{" ++ string:join(EscapedOptions, ",") ++ "}" ++ D1.

-spec(monitor_agent/2 :: (Agent :: string(), Watcher :: string()) -> {'ok', pid()}).
monitor_agent(Agent, Watcher) ->
	gen_server:call(?MODULE, {monitor, agent, Agent, Watcher}).

-spec(monitor_client/2 :: (Client :: string(), Watcher :: string()) -> {'ok', pid()}).
monitor_client(Client, Watcher) ->
	gen_server:call(?MODULE, {monitor, client, Client, Watcher}).

-spec(get_node/0 :: () -> atom()).
get_node() ->
	gen_server:call(?MODULE, get_node).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%% @private
init([Nodename, Options]) -> 
	?DEBUG("starting...", []),
	process_flag(trap_exit, true),
	DialString = proplists:get_value(dialstring, Options, ""),
	monitor_node(Nodename, true),
	{Listenpid, DomainPid, FetchUserOpts} = case net_adm:ping(Nodename) of
		pong ->
			Lpid = start_listener(Nodename),
			freeswitch:event(Nodename, ['CHANNEL_DESTROY']),
			StrippedOpts = [ X || {Key, _} = X <- Options, Key /= domain],
			%{ok, Pid} = freeswitch:start_fetch_handler(Nodename, directory, ?MODULE, fetch_domain_user, StrippedOpts),
			%link(Pid),
			{Lpid, undefined, StrippedOpts};
		_ ->
			StrippedOpts = [ X || {Key, _} = X <- Options, Key /= domain],
			{undefined, undefined, StrippedOpts}
	end,
	?INFO("Started for node ~p", [Nodename]),
	{ok, #state{nodename=Nodename, dialstring = DialString, eventserver = Listenpid, xmlserver = DomainPid, freeswitch_up = true, fetch_domain_user = FetchUserOpts}}.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private

handle_call({get_ring_data, Agent, Options}, _From, #state{fetch_domain_user = UserOpts} = State) ->
	Node = State#state.nodename,
	Type = proplists:get_value(type, Options, sip_registration),
	Data = proplists:get_value(data, Options),
	{Base, Destination} = case {Type, Data} of
		{sip_registration, undefined} ->
			{"sofia/internal/$1%", re:replace(Agent#agent.login, "@", "_", [{return, list}])};
		{sip_registration, Data} ->
			{"sofia/internal/$1%", re:replace(Data, "@", "_", [{return, list}])};
		{_, undefined} ->
			{error, undefined};
		{sip, Data} ->
			{proplists:get_value(sip, UserOpts, "sofia/internal/sip:$1"), Data};
		{iax2, Data} ->
			{proplists:get_value(iax2, UserOpts, "iax2/$1"), Data};
		{h323, Data} ->
			{proplists:get_value(h323, UserOpts, "opal/h323:$1"), Data};
		{pstn, Data} ->
			{proplists:get_value(dialstring, UserOpts, State#state.dialstring), Data}
	end,
	case Base of
		error -> {reply, {error, bad_destination}, State};
		_ -> {reply, {Node, Base, Destination}, State}
	end;

handle_call({make_outbound_call, Client, AgentPid, Agent}, _From, #state{nodename = Node, dialstring = DS, freeswitch_up = FS} = State) when FS == true ->
	{ok, Pid} = freeswitch_outbound:start(Node, Agent, AgentPid, Client, DS, 30),
	link(Pid),
	{reply, {ok, Pid}, State};
handle_call({make_outbound_call, _Client, _AgentPid, _Agent}, _From, State) -> % freeswitch is down
	{reply, {error, noconnection}, State};
%handle_call({record_outage, Client, AgentPid, Agent}, _From, #state{nodename = Node, freeswitch_up = FS} = State) when FS == true ->
%	Recording = "/tmp/"++Client++"/problem.wav",
%	case filelib:ensure_dir(Recording) of
%		ok ->
%			F = fun(UUID) ->
%					fun(ok, _Reply) ->
%							freeswitch:api(State#state.nodename, uuid_transfer, UUID ++ " 'gentones:%(500\\,0\\,500),sleep:600,record:/tmp/"++Client++"/problem.wav' inline");
%						(error, Reply) ->
%							?WARNING("originate failed: ~p", [Reply]),
%							ok
%					end
%			end,
%			DialString = get_agent_dial_string(Agent, [], State),
%			case freeswitch_ring:start(Node, Agent, AgentPid, #call{id=none, source=none}, ?getRingout, F, [no_oncall_on_bridge, {dialstring, DialString}]) of
%				{ok, _Pid} ->
%					{reply, ok, State};
%				{error, Reason} ->
%					{reply, {error, Reason}, State}
%			end;
%		{error, Reason} ->
%			{reply, {error, Reason}, State}
%	end;
handle_call({record_outage, _Client, _AgentPid, _AgentRec}, _From, State) -> % freeswitch is down
	{reply, {error, noconnection}, State};
handle_call({get_handler, UUID}, _From, #state{call_dict = Dict} = State) ->
	case dict:find(UUID, Dict) of
		error -> 
			{reply, noexists, State};
		{ok, Pid} -> 
			{reply, Pid, State}
	end;
handle_call(stop, _From, State) ->
	?NOTICE("Normal termination", []),
	{stop, normal, ok, State};
handle_call({ring, _Agent, _Call}, _From, #state{freeswitch_up = false} = State) ->
	{reply, {error, noconnection}, State};
handle_call({ring, EndPointData, Callrec}, _From, #state{dialstring = BaseDialstring} = State) ->
	NewOptions = [{dialstring, BaseDialstring}, {destination, EndPointData}, {call, Callrec}],
	case freeswitch_ring:start(State#state.nodename, freeswitch_ring_transient, NewOptions) of
		{ok, Pid} ->
			{reply, {ok, Pid, neither}, State};
		Error ->
			{reply, Error, State}
	end;
handle_call({ring, {Type, Data}, Callrec}, _From, #state{fetch_domain_user = BaseDialOpts} = State) ->
	Default = case Type of
		pstn -> State#state.dialstring;
		_ -> ?default_dial_string(Type)
	end,
	BaseDialString = proplists:get_value(Type, BaseDialOpts, Default),
	Destination = case Data of
		Agent when is_record(Agent, agent) ->
			Agent#agent.login;
		_ -> Data
	end,
	NewOptions = [{dialstring, BaseDialString}, {destination, Destination}, {call, Callrec}],
	case freeswitch_ring:start(State#state.nodename, freeswitch_ring_transient, NewOptions) of
		{ok, Pid} ->
			{reply, {ok, Pid, neither}, State};
		Error ->
			{reply, Error, State}
	end;
handle_call({ring, EndPointData, _Callrec}, _From, #state{dialstring = BaseDailstring} = State) ->
	NewOptions = [{dialstring, BaseDailstring}, {destination, EndPointData}],
	case freeswitch_ring:start(State#state.nodename, freeswitch_ring_persistant, NewOptions) of
		{ok, Pid} ->
			{reply, {ok, Pid, both}, State};
		Error ->
			{reply, Error, State}
	end;
handle_call({ring, {Type, Data}, _Callrec}, _From, #state{fetch_domain_user = BaseDialOpts} = State) ->
	Default = case Type of
		sip -> "sofia/internal/sip:$1";
		iax2 -> "iax2/$1";
		h323 -> "opal/h323:$1";
		pstn -> ""
	end,
	BaseDialString = proplists:get_value(Type, BaseDialOpts, ?default_dial_string(Type)),
	NewOptions = [{dialstring, BaseDialString}],
	case freeswitch_ring:start(State#state.nodename, freeswitch_ring_persistent, NewOptions) of
		{ok, Pid} ->
			{reply, {ok, Pid, both}, State};
		Error ->
			{reply, Error, State}
	end;
handle_call({ring, _EndPointType, _EndPointData, _Callback, _Options}, _From, #state{freeswitch_up = false} = State) ->
	{reply, {error, noconnection}, State};
handle_call({ring, {undefined, transient, sip_registration}, EndPointData, Callback, Options}, _From, #state{dialstring = BaseDialstring} = State) ->
	NewOptions = [{dialstring, BaseDialstring}, {destination, EndPointData} | Options],
	Out = freeswitch_ring:start(State#state.nodename, Callback, NewOptions),
	{reply, Out, State};
handle_call({ring, {undefined, transient, Type}, EndPointData, Callback, Options}, _From, #state{fetch_domain_user = BaseDialOpts} = State) ->
	BaseDialString = proplists:get_value(Type, BaseDialOpts, ?default_dial_string(Type)),
	NewOptions = [{dialstring, BaseDialString}, {destination, EndPointData} | Options],
	Out = freeswitch_ring:start(State#state.nodename, Callback, NewOptions),
	{reply, Out, State};
handle_call({ring_agent, _Apid, _Agent, _Opts}, _From, #state{freeswitch_up = false} = State) ->
	{reply, {error, noconnection}, State};
%handle_call({ring_agent, Apid, Agent, Opts}, _From, #state{nodename = Node} = State) ->
%	case Agent#agent.endpointtype of
%		{Rpid, persitent, _} when is_pid(Rpid) ->
%			spawn(fun() ->
%				freeswitch_ring:call(Rpid, {agent_state, ringing, "doesn't matter"})
%			end),
%			{reply, {ok, Rpid}, State};
%		{_, transient, Eptype} ->
%			Default = ?default_dial_string(Eptype),
%			BaseDialString = proplists:get_value(Eptype, State#state.fetch_domain_user, Default),
%			EndPointData = case Agent#agent.endpointdata of
%				undefined -> Agent#agent.login;
%				_ -> Agent#agent.endpointdata
%			end,
%			Options = [{dialstring, BaseDialString}, {destination, EndPointData} | Opts],
%			Out = freeswitch_ring:start(State#state.nodename, freeswitch_ring_transient, Options),
%			{reply, Out, State};
%		Else ->
%			{reply, {error, {badendpointtype, Else}}, State}
%	end;
%handle_call({ring_agent, _Apid, #agent{endpointtype = {Pid, _, _}} = Agent, Call, _Timout}, _From, #state{freeswitch_up = true} = State) when is_pid(Pid) ->
%	?INFO("~s already has a ring channel in ~p", [Agent#agent.login, Pid]),
%	{reply, {ok, Pid}, State};
%handle_call({ring_agent, AgentPid, Agent, Call, Timeout}, _From, #state{nodename = Node} = State) ->
%	case State#state.freeswitch_up of
%		true ->
%			Fun = fun(_) ->
%				fun(_, _) -> ok end
%			end,
%			DialString = get_agent_dial_string(Agent, [], State),
%			Out = freeswitch_ring:start(Node, Agent, AgentPid, Call, Timeout, Fun, [single_leg, {dialstring, DialString}]),
%			{reply, Out, State};
%		false ->
%			{reply, {error, noconnection}, State}
%	end;
%handle_call({ring_agent_echo, AgentPid, Agent, Call, Timeout}, _From, #state{nodename = Node} = State) ->
%	case State#state.freeswitch_up of
%		true ->
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
%			DialString = get_agent_dial_string(Agent, [], State),
%			Out = freeswitch_ring:start(Node, [{handle_event, HandleEvent}], [{agent, Agent},{dialstring,DialString},{destination,Agent#agent.endpointdata}]),
%			{reply, Out, State};
%		false ->
%			{reply, {error, noconnection}, State}
%	end;
handle_call({get_media, MediaPid}, _From, #state{call_dict = Dict} = State) when is_pid(MediaPid) ->
	Folder = fun(Id, Pid, _Acc) ->
		case Pid of
			MediaPid ->
				Id;
			_ ->
				none
		end
	end,
	case dict:fold(Folder, none, Dict) of
		none ->
			{reply, none, State};
		Id ->
			{reply, {Id, MediaPid}, State}
	end;
handle_call({get_media, MediaKey}, _From, #state{call_dict = Dict} = State) ->
	case dict:find(MediaKey, Dict) of
		{ok, Pid} ->
			{reply, {MediaKey, Pid}, State};
		error ->
			{reply, none, State}
	end;
%handle_call({get_agent_dial_string, AgentRec, Options}, _From, State) ->
%	DialString = get_agent_dial_string(AgentRec, Options, State),
%	{reply, DialString, State};
handle_call(get_default_dial_string, _From, State) ->
	{reply, State#state.dialstring, State};
%handle_call({monitor, Type, Watched, Watcher}, _From, State) ->
%	DialString = case Watcher of
%		X when is_record(X, agent) ->
%			get_agent_dial_string(Watcher, [], State);
%		_ ->
%			Watcher
%	end,
%	Res = case Type of
%		agent ->
%			freeswitch_monitor:monitor_agent(Watched, DialString, State#state.nodename);
%		client ->
%			freeswitch_monitor:monitor_client(Watched, DialString, State#state.nodename)
%	end,
%	{reply, Res, State};
handle_call(Request, _From, State) ->
	?INFO("Unexpected call:  ~p", [Request]),
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast({channel_destroy, UUID}, #state{call_dict = Dict} = State) ->
	case dict:find(UUID, Dict) of
		{ok, Pid} ->
			Pid ! channel_destroy,
			{noreply, State};
		error ->
			{noreply, State}
	end;
handle_cast({notify, Callid, Pid}, #state{call_dict = Dict} = State) ->
	NewDict = dict:store(Callid, Pid, Dict),
	{noreply, State#state{call_dict = NewDict}};
handle_cast({new_voicemail, UUID, File, Queue, Priority, Client}, #state{nodename = Node} = State) ->
	{ok, Pid} = freeswitch_voicemail:start(Node, UUID, File, Queue, Priority, Client),
	link(Pid),
	{noreply, State};
handle_cast(_Msg, State) ->
	%?CONSOLE("Cast:  ~p", [Msg]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info({freeswitch_sendmsg, "inivr "++UUID}, #state{call_dict = Dict} = State) ->
	?NOTICE("call ~s entered IVR", [UUID]),
	case dict:find(UUID, Dict) of
		{ok, Pid} ->
			Pid;
		error ->
			case freeswitch_media:start(State#state.nodename, State#state.dialstring, UUID) of
				{ok, Pid} ->
					link(Pid),
					{noreply, State#state{call_dict = dict:store(UUID, Pid, Dict)}};
				Else ->
					?WARNING("Error setting up new call ~s:  ~p", [UUID, Else]),
					{noreply, State}
			end
	end;
handle_info({get_pid, UUID, Ref, From}, #state{call_dict = Dict} = State) ->
	case dict:find(UUID, Dict) of
		{ok, Pid} ->
			From ! {Ref, Pid},
			?NOTICE("pid for ~s already allocated", [UUID]),
			{noreply, State};
		error ->
			case freeswitch_media:start(State#state.nodename, State#state.dialstring, UUID) of
				{ok, Pid} -> 
					From ! {Ref, Pid},
					link(Pid),
					{noreply, State#state{call_dict = dict:store(UUID, Pid, Dict)}};
				Else ->
					From ! {Ref, Else},
					?WARNING("Could not create pid for ~s:  ~p", [UUID, Else]),
					{noreply, State}
			end
	end;
handle_info({'EXIT', Pid, Reason}, #state{eventserver = Pid, nodename = Nodename, freeswitch_up = true} = State) ->
	?NOTICE("listener pid exited unexpectedly: ~p", [Reason]),
	Lpid = start_listener(Nodename),
	freeswitch:event(Nodename, ['CHANNEL_DESTROY']),
	{noreply, State#state{eventserver = Lpid}};
handle_info({'EXIT', Pid, Reason}, #state{xmlserver = Pid, nodename = Nodename, dialstring = _DialString, freeswitch_up = true, fetch_domain_user = Fopts} = State) ->
	?NOTICE("XML pid exited unexpectedly: ~p", [Reason]),
	{ok, NPid} = freeswitch:start_fetch_handler(Nodename, directory, ?MODULE, fetch_domain_user, Fopts),
	link(NPid),
	{noreply, State#state{xmlserver = NPid}};
handle_info({'EXIT', Pid, _Reason}, #state{eventserver = Pid} = State) ->
	{noreply, State#state{eventserver = undefined}};
handle_info({'EXIT', Pid, _Reason}, #state{xmlserver = Pid} = State) ->
	{noreply, State#state{xmlserver = undefined}};
handle_info({'EXIT', Pid, Reason}, #state{call_dict = Dict} = State) ->
	?NOTICE("trapped exit of ~p, doing clean up for ~p", [Reason, Pid]),
	F = fun(Key, Value, Acc) -> 
		case Value of
			Pid ->
				% TODO - We might be able to do more than just terminate the cdr
				case Reason of
					normal ->
						% all is well.
						Acc;
					shutdown ->
						% also good.
						Acc;
					_ ->
						cdr:truncate(Key),
						cpx_monitor:drop({media, Key}),
						Acc
				end;
			_Else ->
				dict:store(Key, Value, Acc)
		end
	end,
	NewDict = dict:fold(F, dict:new(), Dict),
	{noreply, State#state{call_dict = NewDict}};
handle_info({nodedown, Nodename}, #state{nodename = Nodename, xmlserver = Pid, eventserver = Lpid} = State) ->
	case cpx:get_env(ring_manager) of
		?MODULE -> application:unset_env('OpenACD', ring_manager);
		_ -> ok
	end,
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
handle_info(freeswitch_ping, #state{nodename = Nodename, fetch_domain_user = Fopts} = State) ->
	case net_adm:ping(Nodename) of
		pong ->
			?NOTICE("Freeswitch node ~p is back up", [Nodename]),
			monitor_node(Nodename, true),
			Lpid = start_listener(Nodename),
			{ok, Pid} = freeswitch:start_fetch_handler(Nodename, directory, ?MODULE, fetch_domain_user, Fopts),
			link(Pid),
			freeswitch:event(Nodename, ['CHANNEL_DESTROY']),
			case proplists:get_value(not_ring_manager, Fopts) of
				true -> ok;
				_ -> application:set_env('OpenACD', ring_manager, ?MODULE)
			end,
			{noreply, State#state{eventserver = Lpid, xmlserver = Pid, freeswitch_up = true}};
		pang ->
			timer:send_after(1000, freeswitch_ping),
			{noreply, State}
	end;
handle_info(Info, State) ->
	?DEBUG("Unexpected info:  ~p", [Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @private
terminate(_Reason, _State) ->
	application:unset_env('OpenACD', ring_manager),
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
			Ename = proplists:get_value("Event-Name", Event),
			case Ename of
				"CHANNEL_DESTROY" ->
					gen_server:cast(?MODULE, {channel_destroy, UUID});
				_ ->
					ok
			end,
			listener(Node);
		{nodedown, Node} -> 
			gen_server:cast(?MODULE, nodedown);
		 Otherwise -> 
			 ?INFO("Uncertain reply received by the fmm listener:  ~p", [Otherwise]),
			 listener(Node)
	end.

-spec(fetch_domain_user/2 :: (Node :: atom(), State :: #state{}) -> 'ok').
fetch_domain_user(Node, State) ->
	?DEBUG("entering fetch loop with state ~p", [State]),
	receive
		{fetch, directory, "domain", "name", _Value, ID, [undefined | Data]} ->
			case proplists:get_value("as_channel", Data) of
				"true" ->
					User = proplists:get_value("user", Data),
					Domain = proplists:get_value("domain", Data),
					case agent_manager:query_agent(User) of
						{true, Pid} ->
							try agent:dump_state(Pid) of
								Agent ->
									{ok, {Opts, _}} = agent:get_endpoint(freeswitch_media, Agent),
									Type = proplists:get_value(type, Opts),
									Dat = proplists:get_value(data, Opts),

									DialString = case {Type, Dat} of
										{sip_registration, undefined} ->
											"${sofia_contact("++User++"@"++Domain++")}";
										{sip_registration, Data} ->
											"${sofia_contact("++re:replace(Data, "@", "_", [{return, list}])++"@"++Domain++")}";
										{sip, Data} ->
											freeswitch_media_manager:do_dial_string(proplists:get_value(sip, State, "sofia/internal/sip:"), Data, []);
										{iax2, Data} ->
											freeswitch_media_manager:do_dial_string(proplists:get_value(iax2, State, "iax2/"), Data, []);
										{h323, Data} ->
											freeswitch_media_manager:do_dial_string(proplists:get_value(h323, State, "opal/h323:"), Data, []);
										{pstn, Data} ->
											freeswitch_media_manager:do_dial_string(proplists:get_value(dialstring, State, ""), Data, []);
										rtmp ->
											freeswitch_media_manager:do_dial_string(proplists:get_value(rtmp, State, "rtmp/$1"), Data, [])
									end,
									?NOTICE("returning ~s for user directory entry ~s", [DialString, User]),
									freeswitch:fetch_reply(Node, ID, lists:flatten(io_lib:format(?DIALUSERRESPONSE, [Domain, User, DialString])))
							catch
								_:_ -> % agent pid is toast?
									freeswitch:fetch_reply(Node, ID, ?NOTFOUNDRESPONSE)
							end;
						false ->
							freeswitch:fetch_reply(Node, ID, ?NOTFOUNDRESPONSE)
					end;
				_Else ->
					case proplists:get_value("action", Data) of
						"sip_auth" -> % authing a SIP device
							case proplists:get_value(sipauth, State) of
								undefined ->
									%% not doing sip auth, return nothing
									?DEBUG("Not doing SIP auth", []),
									freeswitch:fetch_reply(Node, ID, ""),
									ok;
								_ ->
									User = proplists:get_value("user", Data),
									Domain = proplists:get_value("domain", Data),
									Realm = proplists:get_value("sip_auth_realm", Data),
									% TODO Can this be done w/o dealing w/ a plain text pw?
									?DEBUG("Sip auth\n\tUser:  ~p\n\tdomain:  ~p\n\tRelam:  ~p",[User,Domain,Realm]),
									freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE)
%									case agent_manager:query_agent(User) of
%										{true, Pid} ->
%											try agent:dump_state(Pid) of
%												Agent when Agent#agent.password == 'undefined' ->
%													return_a1_hash(Domain, User, Node, ID);
%												Agent ->
%													Password=Agent#agent.password,
%													Hash = util:bin_to_hexstr(erlang:md5(User++":"++Realm++":"++Password)),
%													freeswitch:fetch_reply(Node, ID, lists:flatten(io_lib:format(?REGISTERRESPONSE, [Domain, User, Hash]))),
%													agent_auth:set_extended_prop({login, Agent#agent.login}, a1_hash, Hash)
%												catch
%													_:_ -> % agent pid is toast?
%														freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE)
%												end;
%											false ->
%												return_a1_hash(Domain, User, Node, ID)
%										end
								end;
						undefined -> % I guess we're just looking up a user?
							% Looking up for first part of an auth most likely.
							% only auth we support is sip (which is above) so we'll
							% depend on that.
							case proplists:get_value(sipauth, State) of
								undefined -> freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE);
								_ ->
									User = proplists:get_value("user", Data),
									Domain = proplists:get_value("domain", Data),
									case agent_manager:query_agent(User) of
										{true, _Pid} ->
											freeswitch:fetch_reply(Node, ID, lists:flatten(io_lib:format(?USERRESPONSE, [Domain, User])));
										false ->
											freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE)
									end
							end;
						_ ->
							freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE)
					end
			end,
			?MODULE:fetch_domain_user(Node, State);
		{fetch, _Section, _Something, _Key, _Value, ID, [undefined | _Data]} ->
			freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE),
			?MODULE:fetch_domain_user(Node, State);
		{nodedown, Node} ->
			?DEBUG("Node we were serving XML search requests to exited", []),
			ok;
		Other ->
			?DEBUG("got other response: ~p", [Other]),
			?MODULE:fetch_domain_user(Node, State)
	end.

return_a1_hash(Domain, User, Node, FetchID) ->
	case agent_auth:get_extended_prop({login, User}, a1_hash) of
		{ok, Hash} ->
			freeswitch:fetch_reply(Node, FetchID, lists:flatten(io_lib:format(?REGISTERRESPONSE, [Domain, User, Hash])));
		undefined ->
			freeswitch:fetch_reply(Node, FetchID, ?EMPTYRESPONSE);
		{error, noagent} ->
			case agent_auth:get_extended_prop({login, re:replace(User, "_", "@", [{return, list}])}, a1_hash) of
				{ok, Hash} ->
					freeswitch:fetch_reply(Node, FetchID, lists:flatten(io_lib:format(?REGISTERRESPONSE, [Domain, User, Hash])));
				undefined ->
					freeswitch:fetch_reply(Node, FetchID, ?EMPTYRESPONSE);
				{error, noagent} ->
					freeswitch:fetch_reply(Node, FetchID, ?EMPTYRESPONSE)
			end
	end.

%get_agent_dial_string(#agent{endpointtype = {_, _, Endpointtype}} = Agent, Opts, State) ->
%	get_agent_dial_string(Agent#agent{endpointtype = Endpointtype}, Opts, State);
%ret_agent_dial_string(AgentRec, Options, State) ->
%	case AgentRec#agent.endpointtype of
%		sip_registration ->
%			case AgentRec#agent.endpointdata of
%				undefined ->
%					freeswitch_media_manager:do_dial_string("sofia/internal/$1%", re:replace(AgentRec#agent.login, "@", "_", [{return, list}]), Options);
%				_ ->
%					%"sofia/internal/"++re:replace(Agent#agent.endpointdata, "@", "_", [{return, list}])++"%"
%					freeswitch_media_manager:do_dial_string("sofia/internal/$1%", re:replace(AgentRec#agent.endpointdata, "@", "_", [{return, list}]), Options)
%			end;
%		sip ->
%			freeswitch_media_manager:do_dial_string(proplists:get_value(sip, State#state.fetch_domain_user, "sofia/internal/sip:$1"), AgentRec#agent.endpointdata, Options);
%		iax2 ->
%			freeswitch_media_manager:do_dial_string(proplists:get_value(iax2, State#state.fetch_domain_user, "iax2/$1"), AgentRec#agent.endpointdata, Options);
%		h323 ->
%			freeswitch_media_manager:do_dial_string(proplists:get_value(h323, State#state.fetch_domain_user, "opal/h323:$1"), AgentRec#agent.endpointdata, Options);
%		pstn ->
%			freeswitch_media_manager:do_dial_string(proplists:get_value(dialstring, State#state.fetch_domain_user, ""), AgentRec#agent.endpointdata, Options);
%		rtmp ->
%			freeswitch_media_manager:do_dial_string(proplists:get_value(rtmp, State#state.fetch_domain_user, "rtmp/$1"), AgentRec#agent.endpointdata, Options)
%	end.
