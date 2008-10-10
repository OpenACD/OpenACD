-record(agent, {
	login :: string(),
	skills = [english] :: [atom(), ...],
	securitylevel = agent :: 'agent' | 'supervisor' | 'admin',
	socket :: port(),	% is port() appropriate?
	state = idle :: 'idle' | 'ringing' | 'precall' | 'onclall' | 'outgoing' | 'released' | 'warmtransfer' | 'wrapup',	
	statedata = {} ::	{} |		% when state is idle
						#call{} |	% when state is ringing, oncall, outgoing, or wrapup
						#client{} |	% state = precall
						{int(), -1} | {int(), 0} | {int(), 1} | default |	% released
						{onhold, #call{}, calling, #call{}},	% warmtransfer
	queuedrelease = undef :: any(),	% is the current state is to go to released, what is the released type
	lastchangetimestamp :: time(),	% at what time did the last state change occur
	endpoints :: [{'voice', string()}, {'chat', string()}]  % the strings here are the uri's the media type is sent to to reach the agent.
	
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