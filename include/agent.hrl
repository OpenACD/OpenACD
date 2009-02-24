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

-record(agent, {
	login = erlang:error({undefined, login}) :: string(),
	skills = [english, '_agent', '_node'] :: [atom(), ...],
	securitylevel = agent :: 'agent' | 'supervisor' | 'admin',
	connection :: pid(),
	state = released :: 'idle' | 'ringing' | 'precall' | 'oncall' | 'outgoing' | 'released' | 'warmtransfer' | 'wrapup',	
	statedata = default ::	{} |		% when state is released
						#call{} |	% when state is ringing, oncall, outgoing, or wrapup
						any() |	% state = precall
						{integer(), -1} | {integer(), 0} | {integer(), 1} | default |	% released
						{onhold, #call{}, calling, #call{}},	% warmtransfer
	queuedrelease :: any(),	% is the current state is to go to released, what is the released type
	lastchangetimestamp = now() :: any(),	% at what time did the last state change occur
	defaultringpath = inband :: 'inband' | 'outband'
}).
	
%% 10/10/2008 Micah
%% statedata's structure is dependant on the state atom.
%% proposed structures:
%% idle :: {}
%% ringing :: #call{}
%% precall :: #client{}
%% oncall :: #call{}
%% outgoing :: #call{}
%% released :: {int(), -1 } | {int(), 0} | {int(), 1} | default
%% warmtransfer :: {onhold, #call{}, calling, #call{}},
%% wrapup :: #call{}

-record(agent_auth, {
	login :: string(),
	password :: string(),
	skills = [english, '_agent', '_node'] :: [atom(), ...]
}).
