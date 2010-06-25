% An example file to demostrate how to set up sip_bots for testing.
% to use this file, start sip_bot_manager passing 
% `{file, "tests/sip_bot_config_example.hrl"}' as the second argument.

% === Required entries ===

% The node OpenACD is running on.  Best if on different hardware.
{acd_node, openacd@example}.
% The domain or IP of OpenACD's freeswitch instance.  The agents
% sip_bot_manager starts will attempt to register to it.
{gateway, "openacd.example.com"}.

% === Optional entries ===

% A list of agents to start, and register.  When sip_bot_manager starts,
% it has OpenACD at acd_node start these agents.  If you don't provide 
% this, you won't be testing much.
{agents, [
	{"username1", "password1"},
	{"username2", "password2"}
]}.
% The realm agents us to authenticate.  Defaults to gateway.
{realm, "realm.example.com"}.

