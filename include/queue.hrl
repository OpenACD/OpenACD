%%	The contents of this file are subject to the Common Public Attribution
%%	License Version 1.0 (the “License”); you may not use this file except
%%	in compliance with the License. You may obtain a copy of the License at
%%	http://opensource.org/licenses/cpal_1.0. The License is based on the
%%	Mozilla Public License Version 1.1 but Sections 14 and 15 have been
%%	added to cover use of software over a computer network and provide for
%%	limited attribution for the Original Developer. In addition, Exhibit A
%%	has been modified to be consistent with Exhibit B.
%%
%%	Software distributed under the License is distributed on an “AS IS”
%%	basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%	License for the specific language governing rights and limitations
%%	under the License.
%%
%%	The Original Code is Spice Telephony.
%%
%%	The Initial Developers of the Original Code is 
%%	Andrew Thompson and Micah Warren.
%%
%%	All portions of the code written by the Initial Developers are Copyright
%%	(c) 2008-2009 SpiceCSM.
%%	All Rights Reserved.
%%
%%	Contributor(s):
%%
%%	Andrew Thompson <athompson at spicecsm dot com>
%%	Micah Warren <mwarren at spicecsm dot com>
%%

%% @hidden
-define(DEFAULT_WEIGHT, 5).
-define(DEFAULT_RECIPE, [{3, remove_skills, ['_node'], run_once}]). % TODO - flesh this out?

%-type(recipe_step() :: {non_neg_integer(),
	%'new_queue' | 'add_skills' | 'remove_skills' | 'set_priority' | 'prioritize' | 'deprioritize' | 'voicemail' | 'announce' | 'add_recipe',
	%[any()],
	%'run_once' | 'run_many'}).

-type(recipe_runs() :: 'run_once' | 'run_many').

-type(recipe_step() ::
	{non_neg_integer(), 'add_skills', [atom(),...], recipe_runs()} |
	{non_neg_integer(), 'remove_skills', [atom(),...], recipe_runs()} |
	{non_neg_integer(), 'set_priority', integer(), recipe_runs()} |
	{non_neg_integer(), 'prioritize', [], recipe_runs()} |
	{non_neg_integer(), 'deprioritize', [], recipe_runs()} |
	{non_neg_integer(), 'voicemail', [], recipe_runs()} |
	{non_neg_integer(), 'announce', string(), recipe_runs()} |
	{non_neg_integer(), 'add_recipe', tuple(), recipe_runs()}). % no recursive types, so you can't use recipe_runs here.

-type(recipe() :: [recipe_step()]).

-record(call_queue, {
	name = erlang:error({undefined, name}) :: string(),
	weight = 1 :: non_neg_integer(),
	skills = [english, '_node'] :: [atom()],
	recipe = ?DEFAULT_RECIPE :: recipe(),
	hold_music :: string(),
	group = "Default" :: string()
}).

-record(queue_group, {
	name = erlang:error({undefined, name}) :: string(),
	recipe = [] :: recipe(),
	sort = 10 :: non_neg_integer(),
	protected = false :: 'true' | 'false'
}).

-record(skill_rec, {
	atom :: atom(),
	name = "New Skill" :: string(),
	protected = false :: 'true' | 'false',
	description = "Default description" :: string(),
	group = "Misc" :: string() 
}).
