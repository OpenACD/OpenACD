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

%% @doc A simple one for one supervisor to act as a middle man.  This 
%% is to allow a process to fail without bringing down the rest of the
%% system.
-module(cpx_middle_supervisor).
-author("Micah").

-include("cpx.hrl").
-include("call.hrl").

-ifdef(EUNIT).
	-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(supervisor).

%% API
-export([
	start_link/3
	]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link(Maxr, Maxt, Spec) ->
	?CONSOLE("Starting a middleman", []),
	{ok, Pid} = supervisor:start_link(?MODULE, [Maxr, Maxt, Spec]),
	supervisor:start_child(Pid, Spec),
	{ok, Pid}.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([Maxr, Maxt, Spec]) ->
    {ok,{{one_for_one, Maxr, Maxt}, [Spec]}}.

%%====================================================================
%% Internal functions
%%====================================================================

-ifdef(EUNIT).

startup_test_() ->
	[{"start with a spec",
	fun() ->
		Out = start_link(3, 5, {dummy_media, {dummy_media, start_link, ["test"]}, temporary, brutal_kill, worker, [?MODULE]}),
		?assertMatch({ok, _Pid}, Out)
	end}].
	
	

-endif.