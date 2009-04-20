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
	start_anon/3,
	start_named/3,
	add_with_middleman/4,
	add_directly/2,
	drop_child/2
	]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_anon(Maxr, Maxt, Spec) when is_record(Spec, cpx_conf) ->
	Childspec = {Spec#cpx_conf.id, {Spec#cpx_conf.module_name, Spec#cpx_conf.start_function, Spec#cpx_conf.start_args}, permanent, 2000, worker, [?MODULE]},
	supervisor:start_link(?MODULE, [Maxr, Maxt, Childspec]).
	
start_named(Maxr, Maxt, Name) when is_atom(Name) ->
	supervisor:start_link({local, Name}, ?MODULE, [Maxr, Maxt]).

add_with_middleman(Name, Maxr, Maxt, Spec) when is_record(Spec, cpx_conf) ->
	?CONSOLE("~p adding ~p", [Name, Spec]),
	supervisor:start_child(Name, {Spec#cpx_conf.id, {?MODULE, start_anon, [Maxr, Maxt, Spec]}, temporary, 2000, supervisor, [?MODULE]}).

add_directly(Name, Spec) when is_record(Spec, cpx_conf) ->
	Childspec = {Spec#cpx_conf.id, {Spec#cpx_conf.module_name, Spec#cpx_conf.start_function, Spec#cpx_conf.start_args}, permanent, 2000, worker, [?MODULE]},
	supervisor:start_child(Name, Childspec).

drop_child(Name, Spec) when is_record(Spec, cpx_conf) ->
	drop_child(Name, Spec#cpx_conf.id);
drop_child(Name, Childid) ->
	supervisor:terminate_child(Name, Childid),
	supervisor:delete_child(Name, Childid).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([Maxr, Maxt]) ->
    {ok,{{one_for_one, Maxr, Maxt}, []}};
init([Maxr, Maxt, Spec]) ->
	?CONSOLE("Checking spec: ~p", [supervisor:check_childspecs([Spec])]),
    {ok,{{one_for_one, Maxr, Maxt}, [Spec]}}.

%%====================================================================
%% Internal functions
%%====================================================================

-ifdef(EUNIT).

startup_test_() ->
	[{"start as an anonymous direct supervisor with one spec.",
	fun() ->
		Dummyspec = #cpx_conf{
			id = dummy_media_manager, 
			module_name = dummy_media_manager, 
			start_function = start_link,
			start_args = []
		},
		Out = start_anon(3, 5, Dummyspec),
		?CONSOLE("~p", [Out]),
		?assertMatch({ok, _P}, Out),
		{ok, Pid} = Out,
		exit(Pid, shutdown)
	end},
	{"start as a neamed supervisor.",
	fun() ->
		Out = start_named(3, 5, testsup),
		?CONSOLE("~p", [Out]),
		?assertMatch({ok, _P}, Out),
		{ok, Pid} = Out,
		exit(whereis(testsup), shutdown),
		?CONSOLE("~p", [whereis(testsup)])
	end},
	{"Start a middle man",
	fun() ->
		Out = start_named(3, 5, testsup),
		?CONSOLE("~p", [Out]),
		?assertMatch({ok, _P}, Out),
		{ok, Pid} = Out,
		Dummyspec = #cpx_conf{
			id = dummy_media_manager, 
			module_name = dummy_media_manager, 
			start_function = start_link,
			start_args = []
		},
		Out2 = add_with_middleman(testsup, 3, 5, Dummyspec),
		?assertMatch({ok, _P2}, Out2),
		{ok, Pid2} = Out2,
		?assertNot(Pid2 =:= Pid),
		Dpid = whereis(dummy_media_manager),
		?assertNot(Pid2 =:= Dpid),
		exit(whereis(testsup), shutdown),
		?CONSOLE("~p", [whereis(testsup)])
	end},
	{"Stop a child that has a middleman.",
	fun() ->
		Dummyspec = #cpx_conf{
			id = dummy_media_manager, 
			module_name = dummy_media_manager, 
			start_function = start_link,
			start_args = []
		},
		{ok, Top} = start_named(3, 5, testsup),
		Middle = add_with_middleman(testsup, 3, 5, Dummyspec),
		drop_child(testsup, dummy_media_manager),
		?assertEqual(undefined, whereis(dummy_media_manager))
	end}].
	
	

-endif.