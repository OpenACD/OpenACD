
-define(DEFAULT_WEIGHT, 5).
-define(DEFAULT_RECIPE, []). % TODO - flesh this out?

-type(recipe_step() :: {non_neg_integer(), 
	'new_queue' | 'add_skills' | 'remove_skils' | 'change_priority' | 'voicemail' | 'announce' | 'add_recipe', 
	[any()],
	'run_once' | 'run_many'}).
	
-type(recipe() :: [recipe_step()]).
