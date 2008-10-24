-record(client, {
		tenant :: pos_integer(),
		brand :: pos_integer(),
		label :: string()
}).

-record(call, {	
		id :: string(),
		type = voice :: 'voice' | 'email',
		callerid = "Unknown Unknown" :: string(),
		% source is the Pid of the media manager this is from
		source :: pid(),
		bound = [] :: [pid()],
		% client record
		client :: #client{},
		skills = [english] :: [atom(), ...],
		cook :: pid()
}).


