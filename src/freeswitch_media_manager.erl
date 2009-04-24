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

%% @doc The controlling module for connection CPX to a freeswitch installation.  There are 2 primary requirements for this work:  
%% the freeswitch installaction must have mod_erlang installed and active, and the freeswitch dialplan must add the following
%% variables to the call data:
%% <dl>
%% <dt>queue</dt><dd>The name of the queue as entered into the queue_manager</dd>
%% <dt>brand</dt><dd>As the combined brand id</dd>
%% </dl>
%% Primary job of this module is to listen to freeswitch for events, and shove those events to the appriate child process.
%% @see freeswitch_media

-module(freeswitch_media_manager).
-author("Micah").

-behaviour(gen_server).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").
-include("queue.hrl").
-include("call.hrl").
-include("agent.hrl").

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

%% API
-export([
	start_link/2, 
	start/2, 
	stop/0,
	ring_agent/2,
	get_handler/1,
	notify/2,
	make_outbound_call/3,
	fetch_domain_user/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	nodename :: atom(),
	freeswitch_c_pid :: pid(),
	call_dict = dict:new(),
	domain,
	voicegateway = "" :: string()
	}).
	
% watched calls structure:
% callid, pid of gen_server handling it

% notes for tomorrow:  set up a process per call id to watch/process that call id

%%====================================================================
%% API
%%====================================================================

%% @doc Start the media manager unlinked to the parant process with C node `node() Nodemane' 
%% and `[{atom(), term()] Options'.
%% <ul>
%% <li>`domain :: string()'</li>
%% <li>`voicegateway :: string()</li>
%% </ul>
start(Nodename, [Head | _Tail] = Options) when is_tuple(Head) ->
	gen_server:start({local, ?MODULE}, ?MODULE, [Nodename, Options], []);
	
%% @doc Start the media manager unlinked to the parent process.  `Nodename' is the name of the C node for mod_erlang in freeswitch; 
%% `Domain' is the domain to ring to sip agents.
%% @clear
% Domain is there to help ring agents.
start(Nodename, Domain) -> 
	gen_server:start({local, ?MODULE}, ?MODULE, [Nodename, [{domain, Domain}]], []).
	
%% @doc Start the media manager linked to the parant process with C node `node() Nodemane' 
%% and `[{atom(), term()] Options'.
%% <ul>
%% <li>`domain :: string()'</li>
%% <li>`voicegateway :: string()</li>
%% </ul>
start_link(Nodename, [Head | _Tail] = Options) when is_tuple(Head) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Nodename, Options], []);

%% @doc Start the media manager linked to the parent process.  `Nodename' is the name of the C node for mod_erlang in freeswitch; 
%% `Domain' is the domain to ring to sip agents.
%% @clear
start_link(Nodename, Domain) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Nodename, [{domain, Domain}]], []).

%% @doc returns {`ok', pid()} if there is a freeswitch media process handling the given `UUID'.
-spec(get_handler/1 :: (UUID :: string()) -> {'ok', pid()} | 'noexists').
get_handler(UUID) -> 
	gen_server:call(?MODULE, {get_handler, UUID}).
	
notify(UUID, Pid) ->
	gen_server:cast(?MODULE, {notify, UUID, Pid}).

make_outbound_call(Number, AgentPid, AgentRec) ->
	gen_server:call(?MODULE, {make_outbound_call, Number, AgentPid, AgentRec}).

stop() ->
	gen_server:call(?MODULE, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%% @private
init([Nodename, Options]) -> 
	?DEBUG("starting...", []),
	process_flag(trap_exit, true),
	Self = self(),
	_Lpid = spawn(fun() -> 
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
	end),
	Voicegateway = proplists:get_value(voicegateway, Options, ""),
	freeswitch:start_fetch_handler(Nodename, directory, ?MODULE, fetch_domain_user, [{voicegateway, Voicegateway}]),
	%T = freeswitch:event(Nodename, [channel_create, channel_answer, channel_destroy, channel_hangup, custom, 'fifo::info']),
	%?CONSOLE("Attempted to start events:  ~p", [T]),
	Domain = proplists:get_value(domain, Options, "localhost"),
	{ok, #state{nodename=Nodename, domain=Domain, voicegateway = Voicegateway}}.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
%handle_call({ring_agent, AgentPid, Call}, _From, State) ->
%	?CONSOLE("ring_agent to ~p for call ~p", [AgentPid, Call#queued_call.id]),
%	AgentRec = agent:dump_state(AgentPid),
%	Args = "{dstchan=" ++ Call#queued_call.id ++ ",agent="++ AgentRec#agent.login ++"}sofia/default/" ++ AgentRec#agent.login ++ "%" ++ State#state.domain ++ " '&erlang("++atom_to_list(?MODULE)++":! "++atom_to_list(node())++")'",
%	X = freeswitch:api(State#state.nodename, originate, Args),
%	?CONSOLE("Bgapi call res:  ~p;  With args: ~p", [X, Args]),
%	{reply, agent:set_state(AgentPid, ringing, Call), State};

handle_call({make_outbound_call, Number, AgentPid, AgentRec}, _From, #state{nodename = Node, domain = Domain} = State) ->
	freeswitch_outbound:start(Node, AgentRec, AgentPid, Number, 30, Domain),
	{reply, ok, State};
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
handle_call(Request, _From, State) ->
	?INFO("Unexpected call:  ~p", [Request]),
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast({notify, Callid, Pid}, #state{call_dict = Dict} = State) ->
	NewDict = dict:store(Callid, Pid, Dict),
	{noreply, State#state{call_dict = NewDict}};
handle_cast(_Msg, State) ->
	%?CONSOLE("Cast:  ~p", [Msg]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info({new_pid, Ref, From}, State) ->
	{ok, Pid} = freeswitch_media:start_link(State#state.nodename, State#state.domain),
	From ! {Ref, Pid},
	% even the media won't know the proper data for the call until later.
	%Callrec = freeswitch_media:get_call(Pid),
	%?CONSOLE("Callrec:  ~p", [Callrec]),
	%NewDict = dict:store(Callrec#call.id, Pid, Dict),
	{noreply, State};
handle_info({'EXIT', Pid, Reason}, #state{call_dict = Dict} = State) -> 
	?NOTICE("trapped exit of ~p, doing clean up for ~p", [Reason, Pid]),
	F = fun(Key, Value, Acc) -> 
		case Value of
			Pid -> 
				Acc;
			_Else -> 
				dict:store(Key, Value, Acc)
		end
	end,
	NewDict = dict:fold(F, dict:new(), Dict),
	{noreply, State#state{call_dict = NewDict}};
handle_info(Info, State) ->
	?DEBUG("Unexpected info:  ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @private
terminate(_Reason, _State) ->
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

% TODO add supporting code so fmm can get the right media to ring it.
%% @doc Ring `AgentPid' with `Call'.
ring_agent(AgentPid, Call) -> 
	gen_server:call(?MODULE, {ring_agent, AgentPid, Call}).

%% @private
% listens for info from the freeswitch c node.
listener(Node) ->
	receive
		{event, [UUID | _Event]} ->
			?DEBUG("recieved event '~p' from c node.", [UUID]),
			%gen_server:cast(?MODULE, Event), 
			listener(Node);
		{nodedown, Node} -> 
			gen_server:cast(?MODULE, nodedown);
		 Otherwise -> 
			 ?INFO("Uncertain reply received by the fmm listener:  ~p", [Otherwise]),
			 listener(Node)
	end.

fetch_domain_user(Node, State) ->
	?DEBUG("entering fetch loop with state ~p", [State]),
	receive
		{fetch, directory, "domain", "name", _Value, ID, [undefined | Data]} ->
			case proplists:get_value("as_channel", Data) of
				"true" ->
					User = proplists:get_value("user", Data),
					Domain = proplists:get_value("domain", Data),
					% XXX hardcoded for now
					case agent_manager:query_agent(User) of
						{true, Pid} ->
							try agent:dump_state(Pid) of
								#agent{remotenumber = Number} when is_list(Number) ->
									%GW = "{ignore_early_media=true}sofia/gateway/cpxvgw.fusedsolutions.com/"++Number,
									GW = "{ignore_early_media=true}sofia/gateway/"++ proplists:get_value(voicegateway, State, "") ++"/" ++ Number,
									freeswitch:send(Node, {fetch_reply, ID, lists:flatten(io_lib:format(?DIALUSERRESPONSE, [Domain, User, GW]))});
								Else ->
									?DEBUG("state: ~p", [Else]),
									freeswitch:send(Node, {fetch_reply, ID, lists:flatten(io_lib:format(?DIALUSERRESPONSE, [Domain, User, "sofia/default/"++User++"%"++Domain]))})
							catch
								_:_ -> % agent pid is toast?
									freeswitch:send(Node, {fetch_reply, ID, ?NOTFOUNDRESPONSE})
							end;
						false ->
							freeswitch:send(Node, {fetch_reply, ID, ?NOTFOUNDRESPONSE})
					end;
				_Else ->
					freeswitch:send(Node, {fetch_reply, ID, ?EMPTYRESPONSE})
			end,
			fetch_domain_user(Node, State);
		{fetch, _Section, _Something, _Key, _Value, ID, [undefined | _Data]} ->
			freeswitch:send(Node, {fetch_reply, ID, ?EMPTYRESPONSE}),
			fetch_domain_user(Node, State);
		{nodedown, Node} ->
			?DEBUG("Node we were serving XML search requests to exited", []),
			ok;
		Other ->
			?DEBUG("got other response: ~p", [Other]),
			fetch_domain_user(Node, State)
	end.
