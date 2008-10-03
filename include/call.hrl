-record(call, {	
		% idnum is a String
		idnum,
		% type is an atom, starting w/ voice, email
		type = voice,
		% callerid is a String
		callerid,
		% source is the Pid of the media manager this is from
		source,
		% bound is a list of the dispatcher Pids this calls is 
		% bound to.
		bound = [],
		% client record
		client,
		% skills is  list of atomic skills
		skills = [english]
		}).
