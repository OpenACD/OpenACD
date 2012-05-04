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

%% @doc Helper behaviour for having freeswitch ring endpoints, be they 
%% agent or arbitrary data.  Much like gen_media, this wraps a gen_server
%% giving the callback module some extra info for the callback functions, as
%% well as adding another:  handle_event.
%%
%% Callback functions:
%%
%% <b>init(FsReference, Args) -> Response</b>
%% 		types:	FsReference = {FsNode, UUID}
%% 					FsNode = node()
%% 					UUID = string()
%% 				Args = [any()]
%% 				Response = {'ok', Opts, State} | 'ignore' | {'stop', Reason}
%%					Opts = [string()]
%%					State = Reason = any()
%%
%% 		Before the originate is done this function is called.  Args is the 
%%		list of options sent to freeswitch_ring unaltered.  Freeswitch Ring 
%%		does not remove options it doesn't understand.  The `Opts' returned
%% 		is a list of strings used for the dial options in the originate.
%%
%% 	<b>handle_call(Msg, From, FsRef, State) -> Result</b>
%%		types:	Msg = State = any()
%%				From = {pid(), reference()}
%%				FsRef = {node(), string()}
%%				Results = gen_server:handle_call results
%%
%%		Any call freeswitch_ring can't handle (which would be nearly all) is
%%		forwarded to here.
%%
%%	<b>handle_cast(Msg, FsRef, State) -> Result</b>
%%		types:	Msg = State = any()
%%				FsRef = {node(), string()}
%%
%%		gen_server:cast.
%%
%%	<b>handle_info(Msg, FsRef, State) -> Result</b>
%%		types:  Msg = State = any()
%%				FsRef = {node(), string()}
%%
%%		ServerPid ! Msg.
%%
%%	<b>handle_event(EventName, EventData, FsRef, State) -> Result</b>
%%		types:	EventName = string()
%%				EventData = [{string(), string()}, ...]
%%				FsRef = {node(), string()}
%%				State = any()
%%				Result = {noreply, State} | {stop, Reason, State}
%%				Reason = any()
%%
%%		freeswitch_ring subscribes to channel_answer, channel_bridge, 
%%		channel_unbridge, and channel_hangup automatically.  Other events
%%		can be subscribed to using the 'events' option.  Returns are the 
%%		same as gen_server:handle_info.

-module(freeswitch_ring).
-author("Micah").

-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("OpenACD/include/log.hrl").
-include_lib("OpenACD/include/queue.hrl").
-include_lib("OpenACD/include/call.hrl").
-include_lib("OpenACD/include/agent.hrl").

%% API
-export([
	start/3,
	start/6,
	start_link/3,
	start_link/6,
	hangup/1,
	get_uuid/1,
	ring/3
	]).

-export([
	behaviour_info/1
]).

%% gen_server callbacks
-export([
	init/1, 
	handle_call/3, 
	handle_cast/2, 
	handle_info/2,
	terminate/2, 
	code_change/3 %,
	%format_status/2
]).

-record(callbacks, {
	state :: any(),
	init :: fun(),
	handle_event :: fun(),
	handle_call :: fun(),
	handle_cast :: fun(),
	handle_info :: fun(),
	terminate :: fun(),
	code_change :: fun()
}).

-record(state, {
	cnode :: atom(),
	uuid :: string(),
	options = [] :: [any()],
	callbacks = #callbacks{}
	}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include_lib("OpenACD/include/gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================

-type(call_opt() :: {'call', #call{}}).
-type(caller_name() :: string()).
-type(caller_id() :: string()).
-type(caller_id_opt() :: {'caller_id', {caller_name(), caller_id()}}).
-type(ringout_opt() :: {'ringout', pos_integer()}).
-type(persistent_opt() :: 'persistent').
-type(dial_vars_opt() :: {'dial_vars', [{string(), string()}]}).
-type(dialstring_opt() :: {'dialstring', string()}).
-type(destination_opt() :: {'destination', string()}).
-type(dnis_opt() :: {'dnis', string()}).
-type(no_oncall_on_bridge_opt() :: 'no_oncall_on_bridge').
-type(start_opt() :: 
	call_opt() |
	caller_id_opt() |
	ringout_opt() |
	persistent_opt() |
	dial_vars_opt() |
	dialstring_opt() |
	destination_opt() |
	dnis_opt() |
	no_oncall_on_bridge_opt()
).
-type(start_opts() :: [start_opt()]).

-type(uuid() :: string()).
-type(fs_refs() :: {atom(), uuid()}).
-type(callback_fun() :: 
	{'init', fun((fs_refs(), [any()]) -> {'ok', any()})} |
	{'handle_event', fun((string(), [{string(), string()}], fs_refs(), any()) -> any())} |
	{'handle_call', fun((any(), {pid(), reference()}, fs_refs(), any()) -> any())} |
	{'handle_cast', fun((any(), fs_refs(), any()) -> any())} |
	{'handle_info', fun((any(), fs_refs(), any()) -> any())} |
	{'terminate', fun((any(), any()) -> any())} |
	{'change_code', fun((any(), any(), any()) -> any())}
).
-type(callback_funs() :: [callback_fun()]).
-type(callbacks() :: atom() | callback_funs()).

-spec(start/3 :: (Fsnode :: atom(), Callbacks :: callbacks(), Options :: start_opts()) -> {'ok', pid()}).
start(Fsnode, Callbacks, Options) ->
	gen_server:start(?MODULE, [Fsnode, Callbacks, Options], []).

start(Agent, Chan, Call, Fsnode, Callbacks, Options) when is_record(Call, call) ->
	NewOptions = [{agent, Agent}, {agent_channel, Chan}, {call, Call} | Options],
	gen_server:start(?MODULE, [Fsnode, Callbacks, NewOptions], []).

-spec(start_link/3 :: (Fsnode :: atom(), Callbacks :: atom() | callbacks(), Options :: start_opts()) -> {'ok', pid()}).
start_link(Fsnode, Callbacks, Options) ->
	gen_server:start_link(?MODULE, [Fsnode, Callbacks, Options], []).

start_link(Agent, _Chan, Call, Fsnode, Callbacks, Options) when is_record(Call, call) ->
	NewOptions = [{call, Call} | Options],
	gen_server:start_link(?MODULE, [Fsnode, Callbacks, NewOptions], []).

%% @doc Used by erlang to detmine if a module implementing this behavor is 
%% valid.
-spec(behaviour_info/1 :: 
	(Info :: 'callbacks' | any()) -> [{atom(), non_neg_integer()}] | 'undefined').
behaviour_info(callbacks) ->
	[{init, 2},
	{handle_event, 4},
	{handle_call, 4},
	{handle_cast, 3},
	{handle_info, 3},
	{terminate, 3},
	{code_change, 3}];
behaviour_info(_Other) ->
    undefined.

-spec(hangup/1 :: (Pid :: pid()) -> 'ok').
hangup(Pid) ->
	gen_server:cast(Pid, hangup).

-spec(get_uuid/1 :: (Pid :: pid()) -> string()).
get_uuid(Pid) ->
	gen_server:call(Pid, {'$freeswitch_ring', get_uuid}).

%% @doc In the case of a persistent ring channel, send tones to the agent
%% to indicate the phone is ringing.
-spec(ring/3 :: (RingPid :: pid(), CallId :: string(), Ringout :: pos_integer()) -> 'ok').
ring(RingPid, CallId, Ringout) ->
	gen_server:call(RingPid, {ring, CallId, Ringout}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%init([Agent, Call, Fsnode, Callbacks, Options]) ->
%	NewOpts = case is_record(Call, call) of
%		true -> [{call, Call} | Options];
%		_ -> Options
%	end,
%	init([Fsnode, Callbacks, NewOpts]);
init([Fsnode, Module, Options]) when is_atom(Module) ->
	Callbacks = #callbacks{
		init = fun(FsRefs, Args) -> Module:init(FsRefs, Args) end,
		handle_event = fun(Event, Data, FsRefs, InState) -> Module:handle_event(Event, Data, FsRefs, InState) end,
		handle_call = fun(Msg, From, FsRefs, InState) -> Module:handle_call(Msg, From, FsRefs, InState) end,
		handle_cast = fun(Msg, FsRefs, InState) -> Module:handle_cast(Msg, FsRefs, InState) end,
		handle_info = fun(Msg, FsRefs, InState) -> Module:handle_info(Msg, FsRefs, InState) end,
		terminate = fun(Reason, FsRefs, InState) -> Module:terminate(Reason, FsRefs, InState) end,
		code_change = fun(OldVsn, InState, Extra) -> Module:change_code(OldVsn, InState, Extra) end
	},
	init([Fsnode, Callbacks, Options]);
init([Fsnode, CallbackList, Options]) when is_list(CallbackList) ->
	Callbacks = #callbacks{
		init = proplists:get_value(init, CallbackList, fun(_, _) -> {ok, [], <<"dummy_state">>} end),
		handle_event = proplists:get_value(handle_event, CallbackList, fun(_, _, _, InState) -> {noreply, InState} end),
		handle_call = proplists:get_value(handle_call, CallbackList, fun(_, _, _, InState) -> {reply, ok, InState} end),
		handle_cast = proplists:get_value(handle_cast, CallbackList, fun(_, _, InState) -> {noreply, InState} end),
		handle_info = proplists:get_value(handle_info, CallbackList, fun(_, _, InState) -> {noreply, InState} end),
		terminate = proplists:get_value(handle_info, CallbackList, fun(_, _, _) -> ok end),
		code_change = proplists:get_value(code_change, CallbackList, fun(_, InState, _) -> {ok, InState} end)
	},
	init([Fsnode, Callbacks, Options]);
init([Fsnode, #callbacks{init = InitFun} = Callbacks, Options]) ->
%init([Fnode, AgentRec, Apid, Fun, Options]) ->
%init([Fnode, AgentRec, Apid, Call, Ringout, Fun, Options]) when is_record(Call, call) ->
	case freeswitch:api(Fsnode, create_uuid) of
		{ok, UUID} ->
			Callrec = proplists:get_value(call, Options),
			{CallerName, CallerNumber, Dnis, Ringout, DefaultDailOpts} = case Callrec of
				#call{callerid = {Cname, Cnumber}, dnis = TheDnis, client = Client} ->
					{RingoutTime, DOpts} = case Client of
						undefined -> {60,[]};
						_ -> {proplists:get_value("ringout",Client#client.options,60),
							proplists:get_value("dial_vars", Client#client.options, [])}
					end,
					{Cname, Cnumber, TheDnis, RingoutTime, DOpts};
				undefined ->
					TheDnis = proplists:get_value(dnis, Options, "0000000"),
					{Cname, Cnumber} = proplists:get_value(caller_id, Options, {"noname", "nonumber"}),
					{Cname, Cnumber, TheDnis, 60, []}
			end,
			HangupAfterBridge = case proplists:get_value(persistent, Options) of
				true -> "false";
				_ -> "true"
			end,
			ParkAfterBridge = case proplists:get_value(persistent, Options) of
				true -> "park_after_bridge=true";
				_ -> ""
			end,
			PreInitDialStringOpts = [
					"origination_caller_id_name='"++CallerName++"'",
					"origination_caller_id_number="++CallerNumber,
					"hangup_after_bridge="++HangupAfterBridge,
					ParkAfterBridge,
					"origination_uuid="++UUID,
					"originate_timeout="++integer_to_list(round(Ringout)),
					"sip_h_X-DNIS='"++Dnis++"'"
					| proplists:get_value(dial_vars, Options, DefaultDailOpts)],
			case InitFun({Fsnode, UUID}, Options) of
				{ok, NewDialStringOpts, CallbackState} ->
					DialStringOpts = NewDialStringOpts ++ PreInitDialStringOpts,
					DialString = case {proplists:get_value(dialstring, Options), proplists:get_value(destination, Options)} of
						{undefined, _} ->
							exit(bad_dialstring);
						{_BaseDialString, undefined} ->
							exit(bad_destination);
						{BaseDialstring, Destination} ->
							% safe because it doesn't dive into fs manager pid
							%?ERROR("ds:  ~s;  dest:  ~s", [BaseDialstring, Destination]),
							freeswitch_media_manager:do_dial_string(BaseDialstring, Destination, DialStringOpts)
					end,
					case proplists:get_value(call, Options) of
						undefined ->
							?INFO("originating ring channel with args:  ~s", [DialString ++ " &park()"]);
						#call{id = CallId} ->
							?INFO("originating ring channel for ~p with args: ~p", [CallId, DialString ++ " &park()"])
					end,
					case freeswitch:bgapi(Fsnode, originate, DialString ++ " &park()") of
						{ok, _BgApiId} ->
							Gethandle = fun(Recusef, Count) ->
								?DEBUG("Counted ~p", [Count]),
								case freeswitch:handlecall(Fsnode, UUID) of
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
									?ERROR("bad uuid ~p when calling ~p", [UUID, proplists:get_value(agent, Options)]),
									{stop, normal};
								{error, Other} ->
									?ERROR("other error starting; ~p for ~p", [Other, proplist:get_value(agent, Options)]),
									{stop, normal};
								_Else ->
									?DEBUG("starting for ~p", [UUID]),
										{ok, #state{
											cnode = Fsnode, 
											uuid = UUID,
											options = Options,
											callbacks = Callbacks#callbacks{state = CallbackState}
										}}
							end;
						Else ->
							?ERROR("originate failed with ~p  when calling ~p", [Else, proplists:get_value(agent, Options)]),
							{stop, normal}
					end;
				Else ->
					?ERROR("create_uuid failed with ~p when trying to call ~p", [Else, proplists:get_value(agent, Options)]),
					{stop, normal}
			end;
		SomeFaile ->
			{stop, SomeFaile}
	end.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({'$freeswitch_ring', get_uuid}, _From, #state{uuid = UUID} = State) ->
	{reply, UUID, State};
%handle_call({ring, _OtherLeg, Ringout}, _From, #state{uuid = UUID, persistent = true} = State) ->
%	TrueRing = case round(Ringout / 100) of
%		0 ->
%			"1";
%		R ->
%			integer_to_list(R)
%	end,
%	Callback = fun bgapi_handler/2,
%	freeswitch:bgapi(State#state.cnode, uuid_transfer, UUID ++ " 'playback:tone_stream://%(2000\\,4000\\,440\\,480);loops="++TrueRing++",park' inline", Callback),
%	{reply, ok, State};
handle_call(Request, From, #state{callbacks = #callbacks{handle_call = CbCall} = Callbacks} = State) ->
	case CbCall(Request, From, {State#state.cnode, State#state.uuid}, Callbacks#callbacks.state) of
		{noreply, NewCbState} ->
			{noreply, State#state{callbacks = Callbacks#callbacks{state = NewCbState}}};
		{reply, Reply, NewCbState} ->
			{reply, Reply, State#state{callbacks = Callbacks#callbacks{state = NewCbState}}};
		{stop, Reason, Reply, NewCbState} ->
			{stop, Reason, Reply, State#state{callbacks = Callbacks#callbacks{state = NewCbState}}};
		{stop, Reason, NewCbState} ->
			{stop, Reason, State#state{callbacks = Callbacks#callbacks{state= NewCbState}}}
	end.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%handle_cast(hangup, #state{uuid = UUID, persistent = true} = State) ->
%	Callback = fun(OkErr, Res) ->
%		?DEBUG("hungup persistent callback fun res:  ~p:~p", [OkErr,Res])
%	end,
%	freeswitch:bgapi(State#state.cnode, uuid_transfer, UUID ++ " -both 'park' inline", Callback),
%	{noreply, State};
%handle_cast(hangup, #state{uuid = UUID} = State) ->
%	freeswitch:sendmsg(State#state.cnode, UUID,
%		[{"call-command", "hangup"},
%			{"hangup-cause", "NORMAL_CLEARING"}]),
%	{noreply, State};
handle_cast(Msg, #state{callbacks = #callbacks{handle_cast = CastFun, state = CbState} = Callbacks} = State) ->
	case CastFun(Msg, {State#state.cnode, State#state.uuid}, CbState) of
		{noreply, NewCbState} ->
			{noreply, State#state{callbacks = Callbacks#callbacks{state = NewCbState}}};
		{stop, Reason, NewCbState} ->
			{stop, Reason, State#state{callbacks = Callbacks#callbacks{state = NewCbState}}}
	end.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({call, {event, [UUID | _Rest]}}, #state{cnode = Cnode, options = Options, uuid = UUID} = State) ->
	?DEBUG("call", []),
	OptEvents = lists:sort(proplists:get_value(events, Options, [])),
	BaseEvents = lists:sort(['CHANNEL_ANSWER', 'CHANNEL_BRIDGE', 'CHANNEL_UNBRIDGE', 'CHANNEL_HANGUP']),
	Events = lists:umerge(OptEvents, BaseEvents),
	freeswitch:session_setevent(Cnode, Events),
	{noreply, State};
handle_info({call_event, {event, [UUID | Rest]}}, #state{options = _Options, uuid = UUID, callbacks = #callbacks{handle_event = CbHandleEvent} = Callbacks} = State) ->
	Event = proplists:get_value("Event-Name", Rest),
	case CbHandleEvent(Event, Rest, {State#state.cnode, State#state.uuid}, Callbacks#callbacks.state) of
		{stop, Reason, NewCbState} ->
			{stop, Reason, State#state{callbacks = Callbacks#callbacks{state = NewCbState}}};
		{noreply, NewCbState} ->
			{noreply, State#state{callbacks = Callbacks#callbacks{state = NewCbState}}}
	end;

%	Continue = case lists:keysearch(eventfun, 1, Options) of
%		{value, {eventfun, Fun}} when is_function(Fun) ->
%			case Fun(UUID, Event, Rest) of
%				Fun2 when is_function(Fun2) ->
%					Fun2();
%				_ ->
%					true
%			end;
%		_ ->
%			true
%	end,
%	case Continue of
%		true ->
%			case Event of
%				"CHANNEL_ANSWER" ->
%					case proplists:get_value(single_leg, State#state.options) of
%						true ->
%							?INFO("Call with single leg answered", []),
%							Call = State#state.callrec,
%							try gen_media:oncall(Call#call.source) of
%								invalid ->
%									freeswitch:api(State#state.cnode, uuid_park, Call#call.id),
%									{stop, normal, State};
%								ok ->
%									{noreply, State}
%							catch
%								exit:{noproc, _} ->
%									?WARNING("~p died before I could complete the bridge", [Call#call.source]),
%									% prolly get no such channel, but just in case it still lives.
%									freeswitch:api(State#state.cnode, uuid_park, Call#call.id),
%									{stop, normal, State}
%							end;
%						_ ->
%							{noreply, State}
%					end;
%				"CHANNEL_BRIDGE" ->
%					case proplists:get_value(no_oncall_on_bridge, State#state.options) of
%						true ->
%							{noreply, State};
%						_ ->
%							?INFO("Call bridged", []),
%							Call = State#state.callrec,
%							try gen_media:oncall(Call#call.source) of
%								invalid ->
%									freeswitch:api(State#state.cnode, uuid_park, Call#call.id),
%									{stop, normal, State};
%								ok ->
%									{noreply, State}
%							catch
%								exit:{noproc, _} ->
%									?WARNING("~p died before I could complete the bridge", [Call#call.source]),
%									% prolly get no such channel, but just in case it still lives.
%									freeswitch:api(State#state.cnode, uuid_park, Call#call.id),
%									{stop, normal, State}
%							end
%					end;
%				"CHANNEL_UNBRIDGE" ->
%					%cdr:hangup(State#state.callrec, agent),
%					{noreply, State};
%				"CHANNEL_HANGUP" ->
%					%AState = agent:dump_state(State#state.agent_pid),
%%					case AState#agent.state of
%%						oncall ->
%%							?NOTICE("Agent ~s still oncall when ring channel hungup", [AState#agent.login]),
%%							ok;
%%						_ ->
%%							ok
%%					end,
%					{noreply, State};
%				_Else ->
%					%?DEBUG("call_event ~p", [Event]),
%					{noreply, State}
%			end;
%		ReturnVal ->
%			ReturnVal
%	end;
%handle_info(call_hangup, #state{persistent = undefined} = State) ->
%	?DEBUG("Call hangup info", []),
%	{stop, normal, State};
%handle_info(call_hangup, State) ->
%	?WARNING("Call hangup when this is supposed to be persistent; ending messily", []),
%	{stop, call_hangup, State};
handle_info(Info, #state{callbacks = #callbacks{handle_info = CbInfoFun} = Callbacks} = State) ->
	case CbInfoFun(Info, {State#state.cnode, State#state.uuid}, Callbacks#callbacks.state) of
		{noreply, NewCbState} ->
			{noreply, State#state{callbacks = Callbacks#callbacks{state = NewCbState}}};
		{stop, Reason, NewCbState} ->
			{stop, Reason, State#state{callbacks = Callbacks#callbacks{state = NewCbState}}}
	end.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(Reason, #state{callbacks = #callbacks{terminate = Fun} = Callbacks} = State) ->
	?NOTICE("FreeSWITCH ring channel teminating ~p", [Reason]),
	Fun(Reason, {State#state.cnode, State#state.uuid}, Callbacks#callbacks.state).

%%--------------------------------------------------------------------
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(OldVsn, #state{callbacks = #callbacks{code_change = Fun} = Callbacks} = State, Extra) ->
	{ok, NewCbState} = Fun(OldVsn, Callbacks#callbacks.state, Extra),
	{ok, State#state{callbacks = Callbacks#callbacks{state = NewCbState}}}.

%-spec(format_status/2 :: (Cause :: any(), Props :: [any()]) -> #state{} | [any()]).
%format_status(normal, [PDict, State]) ->
%	[{data, [{"State", format_status(terminate, [PDict, State])}]}];
%format_status(terminate, [_PDict, State]) ->
%	State#state{}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%bgapi_handler(ok, Res) ->
%	?DEBUG("Default bgapi handler:  ~p", [Res]),
%	ok;
%bgapi_handler(error, Res) ->
%	?WARNING("bgapi errors:  ~p", [Res]),
%	ok.
