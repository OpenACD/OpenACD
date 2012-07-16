% All calls that come into the center will get a skill that no agent has
% thus forcing the dispatchers to have no-one to grab to initially.
% the queues added will remove that skill eventually.
{queues, ["slowQ1","slowQ2","slowQ3"]}.
{additional_queues, [
	{"slowQ1", [
		{recipe, [
			{[{ticks, 15}], [{remove_skills, [block]}], run_once, <<"Remove the blocking skill">>}
		]},
		{skills, [block]}
	]},
	{"slowQ2", [
		{recipe, [
			{[{ticks, 15}], [{remove_skills, [block]}], run_once, <<"Remove the blocking skill">>}
		]},
		{skills, [block]}
	]},
	{"slowQ3", [
		{recipe, [
			{[{ticks, 15}], [{remove_skills, [block]}], run_once, <<"Remove the blocking skill">>}
		]},
		{skills, [block]}
	]}
]}.
{start_count, 20}.
