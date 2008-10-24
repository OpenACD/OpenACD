-record(call, {	
		id :: string(),
		type = voice :: 'voice' | 'email',
		callerid = "Unknown Unknown" :: string(),
		% source is the Pid of the media manager this is from
		source :: pid(),
		bound = [] :: [pid()],
		% client record
		client :: any(),
		skills = [english] :: [atom(), ...],
		cook :: pid()
		}).


