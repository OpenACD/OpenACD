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

-module(scenarios).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-compile(export_all).

scenario1() ->
	queue_manager:add_queue(queue1, [], 5),
	queue_manager:add_queue(queue2, [], 1),
	{ok, Pid} = stub_media_manager:start(),
	stub_media_manager:create_and_queue_call(Pid, "Call1", voice, "Hello world", {client, 1, 1, "somebrand"}, [german], queue1, 0),
	stub_media_manager:create_and_queue_call(Pid, "Call2", voice, "Goodbye world", {client, 1, 1, "somebrand"}, [english], queue2, 0),
	Pid.

s1() -> 
	scenario1().
	
s2() -> 
	queue_manager:add_queue("L3-00170001", [], 1).
	
-ifdef(EUNIT).


-endif.
