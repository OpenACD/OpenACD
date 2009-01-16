%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Original Code is Spice Telphony.
%% 
%% The Initial Developer of the Original Code is 
%% Andrew Thompson and Micah Warren.
%% Portions created by the Initial Developers are Copyright (C) 
%% SpiceCSM. All Rights Reserved.

%% Contributor(s): 

%% Andrew Thompson <athompson at spicecsm dot com>
%% Micah Warren <mwarren at spicecsm dot com>
%% 

%% @doc A fake media manager, you probably don't want to use this for anything
%% other than testing and reference.
-module(stub_media_manager).

-behaviour(gen_server).

-include("call.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-export([create_and_queue_call/8, start/0]).

-record(state, {
	calls = [] :: list()
}).

init([]) ->
	{ok, #state{}}.

%% @private
handle_call({create_and_queue_call, ID, Type, CallerID, Client, Skills, Queue, Priority}, _From, State) ->
	case queue_manager:get_queue(Queue) of
		undefined ->
			{reply, unknownqueue, State};
		Qpid ->
			Call = #call{id=ID, type=Type, callerid=CallerID, client=Client, skills=Skills, source=self(), media_path=inband, ring_path=inband},
			call_queue:add(Qpid, Priority, Call),
			{reply, ok, State#state{calls = lists:append(State#state.calls, [Call])}}
		end;

handle_call({ring_agent, AgentPid, Call}, _From, State) ->
	%% @todo A real media manager would do something substantial here, like try to
	%% send SIP to an agent's phone or whatever. We should do something about
	%% ringout here though...
	{reply, agent:set_state(AgentPid, ringing, Call), State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%% @private
handle_cast(_Request, State) ->
	{noreply, State}.

%% @private
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-spec(start/0 :: () -> {ok, pid()}).
start() ->
	gen_server:start(?MODULE, [], []).

-spec(create_and_queue_call/8 :: (Pid :: pid(), ID :: string(), Type :: atom(), CallerID :: string(), Client :: #client{}, Skills :: [atom()], Queue :: atom(), Priority :: non_neg_integer()) -> 'unknownqueue' | 'ok').
create_and_queue_call(Pid, ID, Type, CallerID, Client, Skills, Queue, Priority) ->
	gen_server:call(Pid, {create_and_queue_call, ID, Type, CallerID, Client, Skills, Queue, Priority}).

