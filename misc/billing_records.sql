DROP TABLE IF EXISTS `billing_transactions`;
CREATE TABLE `billing_transactions` (
  `UniqueID` varchar(20) NOT NULL,
  `Transaction` tinyint(4) NOT NULL,
  `Start` int(11) NOT NULL,
  `End` int(11) NOT NULL,
  `Data` varchar(100) default NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;


DROP TABLE IF EXISTS `billing_summaries`;
CREATE TABLE `billing_summaries` (
  `UniqueID` varchar(20) NOT NULL,
  `DNIS` varchar(32) NOT NULL,
  `TenantID` smallint(6) NOT NULL,
  `BrandID` smallint(6) NOT NULL,
  `Start` int(11) NOT NULL,
  `End` int(11) NOT NULL,
  `InQueue` int(11) NOT NULL default '0',
  `InCall` int(11) NOT NULL,
  `WrapUp` int(11) NOT NULL,
  `AgentID` smallint(6) default NULL,
  `LastState` tinyint(4) NOT NULL,
  `LastQueue` varchar(32) NOT NULL,
  `CallType` varchar(10) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `call_info`;
CREATE TABLE `call_info` (
	`UniqueID` varchar(20) NOT NULL,
	`TenantID` smallint(6) NOT NULL,
	`BrandID` smallint(6) NOT NULL,
	`CallType` varchar(10) NOT NULL,
	`DNIS` varchar(50),
	`CallerIDNum` varchar(20),
	`CallerIDName` varchar(50),
	`EmailID` int(11),
	`VoicemailID` varchar(20),
	`DialedNumber` varchar(20),
	`CaseID` int(11)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

CREATE INDEX ts_idx ON billing_transactions (Transaction, Start);
CREATE INDEX uts_idx ON billing_transactions (UniqueID, Transaction, Start);
CREATE INDEX bsqtimes ON billing_transactions (Transaction, Start, Data);
CREATE UNIQUE INDEX summary_idx ON billing_summaries (UniqueID);
CREATE INDEX summary_calls_since ON billing_summaries (Start,LastQueue,LastState);
