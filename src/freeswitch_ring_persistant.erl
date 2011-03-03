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

-module(freeswitch_ring_persistant).

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

handle_call({ring, _OtherLeg, Ringout}, _From, {Fsnode, UUID}, #state{current_state = idle} = State) ->
	Callback = fun(_, _) -> ok end,
	freeswitch:bgapi(Fsnode, uuid_transfer, UUID ++ " 'playback:tone_stream://%(2000\\,4000\\,440\\,480);loops="++integer_to_list(Ringout)++",park' inline", Callback),
	{reply, ok, State#state{current_state = ringing}};
handle_call(Msg, _From, _FsRef, State) ->
	?WARNING("Unrecognized message ~p", [Msg]),
	{reply, invalid, State}.

%% =====
%% handle_cast
%% =====
handle_cast(_Msg, _FsRef, State) ->
	{noreply, State}.

%% =====
%% handle_info
%% =====
handle_info(_Msg, _FsRef, State) ->
	{noreply, State}.

%% =====
%% handle_event
%% =====
handle_event("CHANNEL_ANSWER", _Data, _FsRef, State) ->
	{noreply, State};
handle_event("CHANNEL_BRIDGE", _Data, _FsRef, #state{current_state = ringing} = State) ->
	{noreply, State#state{current_state = oncall}};
handle_event("CHANNEL_UNBRIDGE", _Data, _FsRef, #state{current_state = oncall} = State) ->
	{noreply, State#state{current_state = idle}};
handle_event(_, _, _, State) ->
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
