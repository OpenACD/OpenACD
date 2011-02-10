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

-module(freeswitch_ring_transient).

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
	call :: 'undefined' | #call{},
	no_oncall_on_bridge :: 'undefined' | 'true'
}).

%% ======
%% API
%% ======

%  Yes it's blank for now.

%% =====
%% freeswitch_ring callbacks
%% =====

%% =====
%% init
%% =====
init(_Fsref, Options) ->
	{ok, [], #state{
		call = proplists:get_value(Options, call),
		no_oncall_on_bridge = proplists:get_value(no_oncall_on_bridge, Options)
	}}.

%% =====
%% handle_call
%% =====
handle_call(_Msg, _From, _FsRef, State) ->
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
handle_event("CHANNEL_ANSWER", _Data, _FsRef, #state{call = undefined} = State) ->
	{noreply, State};
handle_event("CHANNEL_ANSWER", _Data, {FSNode, _UUID}, #state{call = Call} = State) ->
	try gen_media:oncall(Call#call.source) of
		invalid ->
			freeswitch:api(FSNode, uuid_park, Call#call.id),
			{stop, normal, State};
		ok ->
			{noreply, State}
	catch
		exit:{noproc, _} ->
			?WARNING("~p died before I could complete the bridge", [Call#call.source]),
			freeswitch:api(FSNode, uuid_park, Call#call.id),
			{stop, normal, State}
	end;
handle_event("CHANNEL_BRIDGE", _Data, _FsRef, #state{no_oncall_on_bridge = true} = State) ->
	{noreply, State};
handle_event("CHANNEL_BRIDGE", _Data, {Fsnode, _UUID}, #state{call = Call} = State) when is_record(Call, call) ->
	try gen_media:oncall(Call#call.source) of
		invalid ->
			freeswitch:api(Fsnode, uuid_park, Call#call.id),
			{stop, normal, State};
		ok ->
			{noreply, State}
	catch
		exit:{noproc, _} ->
			?WARNING("~p died before I could complete the bridge", [Call#call.source]),
			freeswitch:api(Fsnode, uuid_park, Call#call.id),
			{stop, normal, State}
	end;
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
