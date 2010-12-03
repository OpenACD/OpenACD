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

%% @doc Companion to sip_bots.  When a new call comes into the sip_bot,
%% this is supposed to act like the phone for a dummy agent.  
%% @see sip_bot_manager
-module(sip_bot).
-author(micahw).

-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("log.hrl").

%% API
-export([
	start/1,
	start/2,
	start_link/1,
	start_link/2
]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {
	nodename :: atom(),
	uuid = "" :: string(),
	answered = false :: boolean(),
	parked = false :: boolean(),
	playback_file = "" :: string()
}).

-type(state() :: #state{}).
-define(GEN_SERVER, true).
-include("gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================

-type(uuid_option() :: {'uuid', string()}).
-type(playback_file() :: {'playback_file', string()}).
-type(start_option() :: 
	uuid_option() |
	playback_file()
).
-type(start_options() :: [start_option()]).

-spec(start/1 :: (Node :: atom()) -> {'ok', pid()}).
start(Node) ->
	start(Node, []).

-spec(start/2 :: (Node :: atom(), Opts :: start_options()) -> {'ok', pid()}).
start(Node, Opts) ->
	gen_server:start(?MODULE, [Node, Opts], []).

-spec(start_link/1 :: (Node :: atom()) -> {'ok', pid()}).
start_link(Node) ->
	start_link(Node, []).

-spec(start_link/2 :: (Node :: atom(), Opts :: start_options()) -> {'ok', pid()}).
start_link(Node, Opts) ->
	gen_server:start_link(?MODULE, [Node, Opts], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Node, Opts]) ->
	UUID = proplists:get_value(uuid, Opts, ""),
	Playback = proplists:get_value(playback_file, Opts, "sounds/sip_bot_message.aiff"),
	{ok, #state{uuid = UUID, nodename = Node, playback_file = Playback}}.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Request, From, State) ->
	?NOTICE("unexpected call ~p from ~p", [Request, From]),
    {reply, invalid, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
	?NOTICE("unexpted cast ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info({call, {event, [UUID | Rest]}}, State) when is_list(UUID) ->
	?DEBUG("reporting new call ~p.", [UUID]),
	case_event_name([UUID | Rest], State);
handle_info({call_event, {event, [UUID | Rest]}}, State) when is_list(UUID) ->
	%?DEBUG("reporting existing call progess ~p.", [UUID]),
	case_event_name([ UUID | Rest], State);
handle_info({bgok, Reply}, State) ->
	?DEBUG("bgok:  ~p for ~p", [Reply, State]),
	{noreply, State};
handle_info({bgerror, Err}, State) ->
	?INFO("bg error ~p", [Err]),
	%% the apid is known by gen_media, let it handle if it is not not.
	{noreply, State};
handle_info(channel_destroy, State) ->
	?NOTICE("Hangup in IVR for ~p", [State]),
	{stop, hangup, State};
handle_info(call_hangup, State) ->
	?NOTICE("Call hangup info, terminating ~p", [State]),
	{stop, normal, State};
handle_info(Info, State) ->
	?NOTICE("unexpected info ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
	?NOTICE("Terminated due to ~p", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
case_event_name([UUID | Rawcall], State) ->
	Ename = proplists:get_value("Event-Name", Rawcall),
	?DEBUG("Event:  ~p;  UUID:  ~p", [Ename, UUID]),
	case Ename of
		"CHANNEL_PARK" when State#state.answered == false ->
			% answer the mofo!
			freeswitch:sendmsg(State#state.nodename, UUID,[{"call-command", "execute"},{"execute-app-name", "answer"}]),
			{noreply, State#state{answered = true}};
		"CHANNEL_ANSWER" when State#state.parked == false ->
			freeswitch:api(State#state.nodename, uuid_park, UUID),
			{noreply, State#state{parked = false}};
		"CHANNEL_PARK" ->
			?DEBUG("Trying to playback a file (~s)", [State#state.playback_file]),
			?DEBUG("signal bond:  ~p", [proplists:get_value("variable_signal_bond", Rawcall)]),
			freeswitch:sendmsg(State#state.nodename, UUID, [
				{"call-command", "execute"},
				{"event-lock", "true"},
				{"execute-app-name", "playback"},
				{"execute-app-arg", State#state.playback_file}
			]),
			%spew_vars(Rawcall),
			{noreply, State};
		_ ->
			% not much else I can think of.
			{noreply, State}
	end.

%spew_vars(Rawcall) ->
%	spawn(fun() -> spew_vars_loop(Rawcall) end).
%
%spew_vars_loop([]) ->
%	ok;
%spew_vars_loop([{Key, Val} | Tail]) ->
%	io:format("	~s:	~s~n", [Key, Val]),
%	spew_vars_loop(Tail).

%12:24:01 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_DATA";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"
%12:24:01 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_PARK";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"
%12:25:00 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_HANGUP";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"
%12:25:00 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_UNPARK";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"
%12:25:00 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_EXECUTE_COMPLETE";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"
%12:25:00 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_STATE";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"
%12:25:00 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_HANGUP_COMPLETE";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"
%12:25:00 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_STATE";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"
%12:25:00 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_DESTROY";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"
%12:25:00 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_STATE";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"

	
	
	
	
	
	
	
