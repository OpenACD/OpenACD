
-define(DEFAULT_WEIGHT, 5).
-define(DEFAULT_RECIPE, []). % TODO - flesh this out?

-type(recipe_step() :: {non_neg_integer(), 
	'new_queue' | 'add_skills' | 'remove_skills' | 'set_priority' | 'prioritize' | 'deprioritize' | 'voicemail' | 'announce' | 'add_recipe', 
	[any()],
	'run_once' | 'run_many'}).
	
-type(recipe() :: [recipe_step()]).

-record(queue, {
	name = "Unknown Queue" :: string(),
	wieght = 1 :: non_neg_integer(),
	skills = [english, '_node'] :: [atom()],
	recipe = ?DEFAULT_RECIPE :: recipe()
}).