-record(agent, {
	login :: string(),
	skills = [english, '_agent', '_node'] :: [atom(), ...],
	securitylevel = agent :: 'agent' | 'supervisor' | 'admin',
	connection :: pid(),
	state = released :: 'idle' | 'ringing' | 'precall' | 'oncall' | 'outgoing' | 'released' | 'warmtransfer' | 'wrapup',	
	statedata = default ::	{} |		% when state is released
						#call{} |	% when state is ringing, oncall, outgoing, or wrapup
						any() |	% state = precall
						{integer(), -1} | {integer(), 0} | {integer(), 1} | default |	% released
						{onhold, #call{}, calling, #call{}},	% warmtransfer
	queuedrelease = undefined :: any(),	% is the current state is to go to released, what is the released type
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
