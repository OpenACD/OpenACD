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

%% @hidden
-module(test_coverage).
-export([start/1]).

start(?MODULE) -> 
	ok;
start([Module|T]) ->
	start(Module),
	start(T);
start([]) ->
	ok;
start(Module) ->
	cover:start(),
	cover:compile_beam(string:concat("debug_ebin/", atom_to_list(Module))),
	apply(Module, test, []),
	cover:analyse_to_file(Module, string:concat(string:concat("coverage/", atom_to_list(Module)), ".txt")),
	cover:analyse_to_file(Module, string:concat(string:concat("coverage/", atom_to_list(Module)), ".html"), [html]).
