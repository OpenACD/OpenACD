-record(call, {	
		id :: string(),
		type = voice :: 'voice' | 'email',
		callerid :: string(),
		% source is the Pid of the media manager this is from
		source :: pid(),
		bound = [] :: [pid()],
		% client record
		client :: any(),
		skills = [english] :: [atom(), ...]
		}).

-type(recipe_step() :: {non_neg_integer(), 
	'new_queue' | 'add_skills' | 'remove_skils' | 'change_priority' | 'voicemail' | 'announce' | 'add_recipe', 
	[any()],
	'run_once' | 'run_many'}).
	
-type(recipe() :: [recipe_step()]).