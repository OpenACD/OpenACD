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

%% @hidden
-type(voice_type() :: 'sip_registration' | 'sip' | 'iax2' | 'h323' | 'pstn').
%% first is before it's establish, second is once it's established.
-type(endpointringpid() :: 'undefined' | pid()).
-type(endpointringpersistance() :: 'transient' | 'persistant').
-type(endpointtype() :: {endpointringpid(), endpointringpersistance(), voice_type()}).
-type(release_bias() :: -1 | 0 | 1).
-type(release_label() :: 'default' | string()).
-type(release_id() :: string()).
-type(release_code() :: {release_id(), release_label(), release_bias()}).
-type(skill() :: atom() | {atom(), any()}).
-type(skill_list() :: [skill()]).

-record(agent, {
	login = erlang:error({undefined, login}) :: string(),
	id :: 'undefined' | string(),
	skills = [english, '_agent', '_node'] :: [atom(), ...],
	connection :: pid(),
	profile = "Default" :: string() | 'error',
	source :: pid(),
	release_data :: release_code() | 'undefined',
	available_channels = [dummy, voice, visual, slow_text, fast_text, fast_text, fast_text],
	used_channels = dict:new(),
	endpoints = dict:new(),
	ring_channel = none :: 'none' | any(),
	%state = released :: 'idle' | 'ringing' | 'precall' | 'oncall' | 'outgoing' | 'released' | 'warmtransfer' | 'wrapup',	
	%oldstate = released :: 'idle' | 'ringing' | 'precall' | 'oncall' | 'outgoing' | 'released' | 'warmtransfer' | 'wrapup',	
	%statedata = {"default", default, -1} ::	{} |		% when state is idle
						%#call{} |	% when state is ringing, oncall, outgoing, or wrapup
						%any() |	% state = precall
						%{release_id(), release_label(), release_bias()} |	% released
						%{onhold, #call{}, calling, string()},	% warmtransfer
	last_change = util:now() :: pos_integer(),	% at what time did the last state change occur
	%defaultringpath = inband :: 'inband' | 'outband',
	%endpointtype = {undefined, transient, sip_registration} :: endpointtype(),
	% data used either to dial the agent on demand or start a perisistant
	% connection
	%endpointdata = undefined :: 'undefined' | string(),
	start_opts = [] :: [any()],
	log_pid :: 'undefined' | pid()
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
%% warmtransfer :: {onhold, #call{}, calling, string()},
%% wrapup :: #call{}

-type(security_level() :: 'agent' | 'supervisor' | 'admin').
-type(statename() :: 'idle' | 'ringing' | 'precall' | 'oncall' | 'outgoing' | 'released' | 'warmtransfer' | 'wrapup').

-record(agent_auth, {
	id :: string(),
	login :: string(),
	password :: string(),
	skills = [english, '_agent', '_node'] :: [atom()],
	securitylevel = agent :: security_level(),
	integrated :: 'undefined' | pos_integer(),
	profile = "Default" :: string(),
	firstname = "" :: string(),
	lastname = "" :: string(),
	extended_props = [] :: [{atom(), any()}],
	timestamp = util:now() :: pos_integer()
}).

-type(profile_option() :: 
	'hidden' | 
	'isolated' |
	{'queues', [string()]} |
	{'profiles', [string()]} |
	{'clients', [string()]} |
	{'graphed', boolean()}).

-record(agent_profile, {
	name = erlang:error({undefined, name}) :: string() | 'error',
	id :: 'undefined' | string(), %erlang:error({undefined, id}) :: string(),
	order = 1 :: pos_integer(),
	options = [] :: [profile_option()],
	skills = [] :: [atom()],
	timestamp = util:now() :: pos_integer()
}).

-record(agent_cache, {
	pid,
	id,
	time_avail = os:timestamp(),
	skills,
	channels,
	endpoints
}).

-record(agent_key, {
	rotations = 0,
	has_all,
	skill_count,
	idle_time = os:timestamp()
}).

-define(DEFAULT_PROFILE, #agent_profile{name = "Default", id = "0", timestamp = util:now()}).

-define(DEFAULT_RELEASE, {"default", default, -1}).

-record(release_opt, {
	id :: pos_integer(),
	label :: string(),
	bias = 0 :: -1 | 0 | 1,
	timestamp = util:now() :: pos_integer()
	}).

-record(agent_state, {
	id :: string(),
	agent :: string(),
	state :: statename() | 'login' | 'logout',
	oldstate :: statename() | 'login' | 'logout',
	statedata :: any(),
	start :: integer(),
	ended :: 'undefined' | integer(),
	profile :: string(),
	timestamp = util:now() :: integer(),
	nodes :: [atom()]
}).

-record(agent_profile_change, {
	id :: string(),
	agent :: string(),
	old_profile :: string(),
	new_profile :: string(),
	skills :: skill_list(),
	dropped_skills :: skill_list(),
	gained_skills :: skill_list()
}).
