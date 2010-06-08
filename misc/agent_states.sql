DROP TABLE IF EXISTS `agent_states`;
CREATE TABLE agent_states (
	agent varchar(64) not null default '',
	oldstate int(11) not null,
	newstate int(11) not null,
	start int(11) not null default '0',
	end int(11) not null default '0',
	profile int(11) not null default '1',
	data varchar(256)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

CREATE INDEX state_idx ON agent_states (agent, oldstate, start);
