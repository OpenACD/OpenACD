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
	start_direct/3,
	start_middleman/3,
	add_to_middleman/2,
	dump_middleman/1
	]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc a Direct supervisor is a `one_for_one' supervisor. 
start_direct(Maxr, Maxt, Spec) when is_tuple(Spec) ->
	?CONSOLE("Starting a middleman", []),
	supervisor:start_link(?MODULE, [Maxr, Maxt, [Spec], one_for_one]);
start_direct(Maxr, Maxt, Spec) when is_list(Spec) ->
	?CONSOLE("Starting a middleman", []),
	supervisor:start_link(?MODULE, [Maxr, Maxt, Spec, one_for_one]);
start_direct(Maxr, Maxt, Name) when is_atom(Name) ->
	?CONSOLE("Staring named middleman ~p", [Name]),
	supervisor:start_link({local, Name}, ?MODULE, [Maxr, Maxt, [], one_for_one]).

%% @doc a middle man supervisor is a `simple_one_for_one' supervisor that starts a direct supervisor as a temporary child.
%% The started supervisor then starts a child on a permanent basis.
start_middleman(Maxr, Maxt, Name) ->
	?CONSOLE("Starting a middle man monitor", []),
	%{routing, {cpx_middle_supervisor, start_link, [routing_sup]}, temporary, 2000, supervisor, [?MODULE]}
	Spec = {?MODULE, {?MODULE, start_direct, [Maxr, Maxt]}, temporary, brutal_kill, supervisor, [?MODULE]},
	supervisor:start_link({local, Name}, ?MODULE, [Maxr, Maxt, [], one_for_one]).

%% @doc Start up a process with a middle supervisor between the spec and the supervisor `Name.'
add_to_middleman(Name, Maxr, Maxt, Spec) when is_record(Spec, cpx_conf) ->
	Childspec = {Spec#cpx_conf.id, {Spec#cpx_conf.module_name, Spec#cpx_conf.start_function, Spec#cpx_conf.start_args}
	supervisor:start_child(Name, {

drop_from_middleman(Name, Spec) when is_record(Spec, cpx_conf) ->
	drop_from_middleman(Name, Spec#cpx_conf.id);
drop_from_middleman(Name, Id) ->
	supervisor:terminate_child(Name, Id),
	supervisor:delete_child(Name, Id).

dump_middleman(Name) ->
	supervisor:which_children(Name).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([Maxr, Maxt, Spec, Type]) ->
	?CONSOLE("Checking spec: ~p", [supervisor:check_childspecs(Spec)]),
    {ok,{{Type, Maxr, Maxt}, Spec}}.

%%====================================================================
%% Internal functions
%%====================================================================

-ifdef(EUNIT).

startup_test_() ->
	[{"start as an anonymous direct supervisor with one spec.",
	fun() ->
		Dummyspec = {dummy_media, {dummy_media, start_link, ["test"]}, temporary, brutal_kill, worker, [?MODULE]},
		?CONSOLE("spec ~p is ~p", [Dummyspec, supervisor:check_childspecs([Dummyspec])]),
		Out = start_direct(3, 5, {dummy_media, {dummy_media, start_link, ["test"]}, temporary, brutal_kill, worker, [?MODULE]}),
		?CONSOLE("~p", [Out]),
		?assertMatch({ok, _P}, Out),
		{ok, Pid} = Out,
		exit(Pid, shutdown)
	end},
	{"Start as an anonymouse direct supervisor with a list of specs.",
	fun() ->
		Dummyspec1 = {dummy_media1, {dummy_media, start_link, ["test"]}, temporary, brutal_kill, worker, [?MODULE]},
		Dummyspec2 = {dummy_media2, {dummy_media, start_link, ["test"]}, temporary, brutal_kill, worker, [?MODULE]},
		?CONSOLE("specs: ~p", [supervisor:check_childspecs([Dummyspec1, Dummyspec2])]),
		Out = start_direct(3, 5, [Dummyspec1, Dummyspec2]),
		?CONSOLE("~p", [Out]),
		?assertMatch({ok, _P}, Out),
		{ok, Pid} = Out,
		exit(Pid, shutdown)
	end},
	{"start as a neamed direct supervisor.",
	fun() ->
		Out = start_direct(3, 5, testsup),
		?CONSOLE("~p", [Out]),
		?assertMatch({ok, _P}, Out),
		{ok, Pid} = Out,
		exit(Pid, shutdown)
	end},
	{"Start a middle man",
	fun() ->
		Out = start_middleman(3, 5, testsup),
		?CONSOLE("~p", [Out]),
		?assertMatch({ok, _P}, Out),
		{ok, Pid} = Out,
		exit(Pid, shutdown)
	end},
	{"Start a middleman, then have it start directs.",
	fun() ->
		Dummyspec = {dummy_media, {dummy_media, start_link, ["test"]}, temporary, brutal_kill, worker, [?MODULE]},
		{ok, Top} = start_middleman(3, 5, testsup),
		?CONSOLE("middleman ~p", [Top]),
		Middle = add_to_middleman(testsup, Dummyspec),
		?CONSOLE("direct ~p", [Middle]),
		?assertMatch({ok, _P}, Middle),
		exit(Top, shutdown)
	end},
	{"Stop a child of a direct that is a child of a middleman.",
	fun() ->
		Dummyspec = {"dummy_media_manager", {dummy_media_manager, start_link, ["test"]}, temporary, brutal_kill, worker, [?MODULE]},
		{ok, Top} = start_middleman(3, 5, testsup),
		Middle = add_to_middleman(3, 5, testsup, Dummyspec),
		drop_from_middleman(testsup, "dummy_media_manager"),
		?assertEqual(undefined, whereis(dummy_media_manager))
	end}].
	
	

-endif.