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

%% @doc A transient ring channel.

-module(freeswitch_ring_persistent).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("log.hrl").
-include("call.hrl").

-export([
	init/2,
	handle_event/4,
	handle_call/4,
	handle_cast/3,
	handle_info/3,
	terminate/3,
	code_change/3
]).

-record(state, {
	current_state = idle :: 'idle' | 'ringing' | 'oncall'
}).

%% ======
%% API
%% ======

%% =====
%% freeswitch_ring callbacks
%% =====

%% =====
%% init
%% =====
init(_Fsref, _Options) ->
	{ok, ["park_after_bridge=true","hangup_after_bridge=false"], #state{}}.

%% =====
%% handle_call
%% =====

handle_call({agent_state, ringing, _RingCall}, _From, {Fsnode, UUID}, State) ->
	Callback = fun(_, _) -> ok end,
	freeswitch:bgapi(Fsnode, uuid_transfer, UUID ++ " 'playback:tone_stream://%(2000\\,4000\\,440\\,480);loops=20,park' inline", Callback),
	{reply, ok, State};
handle_call(Msg, _From, FsRef, State) ->
	?WARNING("Unrecognized message ~p (fs:  ~p)", [Msg, FsRef]),
	{reply, invalid, State}.

%% =====
%% handle_cast
%% =====
handle_cast({agent_state, oncall, #call{type = voice}}, _FsRef, State) ->
	% the ring sound is automatically cut off in this case.
	{noreply, State};
% TODO next 2 clauses could stand to be less transfer happy.
handle_cast({agent_state, AState, _Data}, FsInfo, State) ->
	handle_cast({agent_state, AState}, FsInfo, State);
handle_cast({agent_state, _AState}, {Fsnode, UUID}, State) ->
	freeswitch:bgapi(Fsnode, uuid_transfer, UUID ++ " 'park' inline", fun(_, _) -> 
		?INFO("machine goes bing!", []),
	ok end),
	{noreply, State};
handle_cast(Msg, _FsRef, State) ->
	?INFO("unhandled cast ~p", [Msg]),
	{noreply, State}.

%% =====
%% handle_info
%% =====
handle_info(_Msg, _FsRef, State) ->
	{noreply, State}.

%% =====
%% handle_event
%% =====
handle_event("CHANNEL_HANGUP", _Data, _FsRef, State) ->
	{stop, "CHANNEL_HANGUP", State};
handle_event(_Event, _, _, State) ->
	%?WARNING("Cannot handle event ~s.", [Event]),
	{noreply, State}.

%% =====
%% terminate
%% =====

terminate(_Reason, _Fsref, _State) ->
	ok.

%% =====
%% code_change
%% =====

code_change(_oldVsn, State, _Extra) ->
	{ok, State}.

%% =====
%% internal api
%% =====
