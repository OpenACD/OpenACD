-record(client, {
		tenant :: pos_integer(),
		brand :: pos_integer(),
		label :: string()
}).

-record(call, {	
		id :: string(),
		type = voice :: 'voice' | 'email' | 'chat',
		callerid = "Unknown Unknown" :: string(),
		% source is the Pid of the media manager this is from
		source :: pid(),
		bound = [] :: [pid()],
		% client record
		client :: #client{},
		skills = [english] :: [atom(), ...],
		cook :: pid(),
		ring_path = outband :: 'inband' | 'outband' | 'any',
		media_path = outband :: 'inband' | 'outband'
}).


