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
%%	The Original Code is OpenACD.
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
%%	Andrew Thompson <andrew at hijacked dot us>
%%	Micah Warren <micahw at lordnull dot com>
%%

%% @doc The helper module to config the call_queues, skills, clients, and
%% queue groups.
%%
%% <h3>Call Queue</h3>
%% A Call Queue has a name, weight, skills, hold_music, group, and recipe. 
%% The name is a unique identifier of the queue.  There can be only one 
%% queue with a given name in a cluster.
%%
%% The wieght is how important media in the queue are relative to other 
%% queues.  A higher wieght indicates more importance.
%%
%% Skills is a list of skills to add to media that is placed in the queue.
%% Note that media automatically get the magic '_node' skill assigned to 
%% them.
%%
%% Hold music really only matters to freeswitch or voice media.  It is a 
%% string that defines the file the hold music is located in.
%%
%% Group is which Queue Group the queue is a member of.  The skills and 
%% recipe of a queue is combined with it's group on start up.
%%
%% Finally, there is the recipe.  A reciepe is a list of recipe steps.
%% A recipe step is a list of contions, what actions to take when those
%% conditions are met, whether to run multiple times or only once, and
%% finally a comment describing the step.
%%
%% The full description of a recipe:
%% 	recipe_runs() :: run_once | run_many
%% 	recipe_comparison() :: &lt; | &gt; | =
%% 	recipe_condition() ::
%%		{ticks, pos_integer()} |
%%		{eligible_agents, recipe_comparison(), non_neg_integer()} |
%%		{available_agents, recipe_comparison(), non_neg_integer()} |
%%		{queue_position, recipe_comparison(), non_neg_integer()} |
%%		{calls_queued, recipe_comparison(), non_neg_integer()}).
%%
%%	recipe_operation() ::
%%		{add_skills, [atom(), ...]} |
%%		{remove_skills, [atom(), ...]} |
%%		{set_priority, integer()} |
%%		{prioritize, []} |
%%		{deprioritize, []} |
%%		{voicemail, []} |
%%		{announce, string()} |
%%		{add_recipe, recipe_step()}
%%
%%	recipe_comment() :: binary()
%%
%%	recipe_step() ::
%%		{[recipe_condition(), ...], [recipe_operation(), ...], 
%%			recipe_runs(), recipe_comment()}
%%
%%	recipe() :: [recipe_step()]
%%
%% <h3>Skills</h3>
%%
%% A skill configuration has an atom, name, protected, description, and 
%% group.
%%
%% The atom is the key, and is used when routing.  Certain skills are 
%% 'magic' in that they expand to {atom(), string()} values under the
%% correct conditions.  A magic skill is denoted by an atom starting with
%% an underscore by convetion, such as '_queue'.  It is an error to assign
%% an unexpanded magic skill under conditions when it cannot expand.  The 
%% magic skills are:
%% <table style="border:black solid 1px">
%% <tr><th>Atom</th><th>Expands When</th><th>Other Notes</th></tr>
%% <tr><td>_agent</td><td>Assigned to an agent</td>
%% 	<td>Expands to the agent's login</td></tr>
%% <tr><td>_profile</td><td>Assigned to an agent</td><td>Expands to the 
%% 	name of the agent's profile.</td></tr>
%% <tr><td>_queue></td><td>Assigned to a media that is in a queue</td>
%% 	<td>Expands to the name of the queue media is in</td></tr>
%% <tr><td>_brand</td><td>Assigned to a media</td><td>Expands to the 
%% 	client's label.</td></tr>
%% <tr><td>_node</td><td>Assigned to an agent or media</td><td>Expands to 
%% 	the node the agent fsm or gen_media process is running on.</td></tr>
%% <tr><td>_all</td><td>Always</td><td>Does not actually expand, but 
%% 	overrides other skills, making that agent able to take any media, or
%% 	the media answerable by any agent.</td></tr>
%% </table>
%%
%% The remaining configuration options only matter to the configuration for
%% human's sake.  The only one that's not obvious is protected.  If 
%% protected is set to true, the skill cannot be edited or deleted.  The
%% magic skills are protected.
%%
%% <h3>Queue Group</h3>
%% 
%% A queue group is a recipe and list of skills that each queue in the
%% group shares.
%%
%% <h3>Clients</h3>
%%
%% A client is an id, label, and list of options.  Currently the only two
%% options available are url_pop and autoend_wrapup.

-module(call_queue_config).
-author("Micah").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-define(QUEUE_TABLE(Nodes), 
	[
		{attributes, record_info(fields, call_queue)},
		{disc_copies, Nodes}
	]
).
-define(SKILL_TABLE(Nodes), 
	[
		{attributes, record_info(fields, skill_rec)},
		{disc_copies, Nodes}
	]
).
-define(CLIENT_TABLE(Nodes),
	[
		{attributes, record_info(fields, client)},
		{disc_copies, Nodes}
	]
).
-define(QUEUE_GROUP_TABLE(Nodes),
	[
		{attributes, record_info(fields, queue_group)},
		{disc_copies, Nodes}
	]
).
-define(DEFAULT_QUEUE_GROUP, #queue_group{name = "Default", sort = 0, protected = true}).

-define(DEFAULTABLE_CLIENT_OPTIONS, [url_pop, autoend_wrapup]).

-define(DEFAULT_STORAGE, call_queue_config_mnesia).

-include("log.hrl").
-include("queue.hrl").
-include("call.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API

%%
-export([
	start/0,
	build_tables/0
]).
-export([
	new_queue/1, 
	new_queue/5,
	destroy_queue/1,
	get_queue/1,
	get_merged_queue/1,
	get_queues/0,
	get_queues/1,
	set_queue/2,
	set_queue/6
	]).
-export([
	new_queue_group/1,
	new_queue_group/3,
	get_queue_groups/0,
	get_queue_group/1,
	set_queue_group/2,
	set_queue_group/4,
	destroy_queue_group/1
	]).
-export([
	new_skill/1,
	new_skill/4,
	skill_exists/1,
	get_skill/1,
	get_skills/0,
	get_skills/1,
	set_skill/2,
	set_skill/4,
	rename_skill_group/2,
	destroy_skill/1
	]).
-export([
	new_client/1,
	new_client/3,
	destroy_client/1,
	destroy_client/2,
	set_client/2,
	set_client/3,
	get_client/1,
	get_client/2,
	get_default_client/0,
	get_clients/0]).

%% =====
%% All configs
%% =====

-spec(start/0 :: () -> any()).
start() ->
	Store = case application:get_env('OpenACD', call_queue_config_storage) of
		{ok, St} -> St;
		undefined -> ?DEFAULT_STORAGE
	end,
	Store:start().

%% @doc Attempts to set-up and create the required mnesia table `call_queue' on 
%% the local node if it does not exist.
-spec(build_tables/0 :: () ->'ok').
build_tables() -> 
	?DEFAULT_STORAGE:build_tables().

%% =====
%% Call queue
%% =====

%% @doc Attempt to remove the queue `#call_queue{}' or `string()' `Queue' from the configuration database.
-spec(destroy_queue/1 :: (Queue :: #call_queue{}) -> {atom(), any()};
					(Queue :: string()) -> {atom(), any()}).
destroy_queue(Queue) when is_record(Queue, call_queue) -> 
	?DEFAULT_STORAGE:destroy_queue(Queue).

%% @doc Get the configuration for the passed `string()' `Queue' name.	
-spec(get_queue/1 :: (Queue :: string()) -> #call_queue{} | {'noexists', any()} | 'noexists').
get_queue(Name) ->
	case cpx_hooks:trigger_hooks(get_queue, [Name], first) of
		{ok, Entry} -> Entry;
		_ -> noexists
	end.

%% @doc Get the configureation for the passed `string()' `Queue' name and
%% merge it with the queue group's skills and recipe.
-spec(get_merged_queue/1 :: (Queue :: string()) -> #call_queue{} | {'noexists', any()} | 'noexists').
get_merged_queue(Queue) ->
	case get_queue(Queue) of
		QueueRec when is_record(QueueRec, call_queue) ->
			case get_queue_group(QueueRec#call_queue.group) of
				{atomic, [GroupRec]} when is_record(GroupRec, queue_group) ->
					Recipe = correct_recipe(lists:append(QueueRec#call_queue.recipe, GroupRec#queue_group.recipe)),
					Skills = util:merge_skill_lists(GroupRec#queue_group.skills, QueueRec#call_queue.skills),
					QueueRec#call_queue{skills = Skills, recipe = Recipe};
				_NoGroup ->
					?WARNING("no group ~s, returning raw queue", [QueueRec#call_queue.group]),
					QueueRec
			end;
		NoQueue ->
			NoQueue
	end.

%% @doc Get all the queues that are members of the specified Group (`string()').
-spec(get_queues/1 :: (Group :: string()) -> [#call_queue{}]).
get_queues(Group) ->
	[X || X <- get_queues(), X#call_queue.group =:= Group].
	
%% @doc Get all the queue configurations (`[#call_queue{}]').
-spec(get_queues/0 :: () -> [#call_queue{}]).
get_queues() ->
	case cpx_hooks:trigger_hooks(get_queues, [], all) of
		{ok, Lists} ->
			IsUniqueFun = fun(#call_queue{name=Name}, NameSet) ->
				not gb_sets:is_member(Name, NameSet)
			end,

			AddToChecksFun = fun(#call_queue{name=Name}, NameSet) ->
				gb_sets:add(Name, NameSet)
			end,

			Init = gb_sets:new(),
			get_uniques(IsUniqueFun, AddToChecksFun, Init, Lists);
		_ -> []
	end.

%% @doc Create a new default queue configuraiton from `#call_queue{} Queue'.
%% @see new_queue/2
-spec(new_queue/1 :: (QueueName :: #call_queue{} ) -> {'aborted', any()} | {'atomic', #call_queue{}}).
new_queue(Queue) when is_record(Queue, call_queue) ->
	?DEFAULT_STORAGE:new_queue(Queue).

%% @doc Create a new queue from the given `string() Name, pos_integer() Weight, [atom()] Skills, recipe() Recipe, string() Group'.
-spec(new_queue/5 :: (Name :: string(), Weight :: pos_integer(), Skills :: [atom() | {atom(), any()}], Recipe :: recipe(), Group :: string()) -> {'aborted', any()} | {'atomic', #call_queue{}}).
new_queue(Name, Weight, Skills, Recipe, Group) when Weight > 0, is_integer(Weight) ->
	?DEFAULT_STORAGE:new_queue(Name, Weight, Skills, Recipe, Group).

-spec(set_queue/6 :: (OldName :: string(), Name :: string(), Weight :: pos_integer(), Skills :: [atom() | {atom(), any()}], Recipe :: recipe(), Group :: string()) -> {'aborted', any()} | {'atomic', #call_queue{}}).
set_queue(OldName, Name, Weight, Skills, Recipe, Group) when Weight > 0, is_integer(Weight) ->
	?DEFAULT_STORAGE:set_queue(OldName, Name, Weight, Skills, Recipe, Group).

%% @doc Sets the queue name `Queue' to the passed `#call_queue{}'.
-spec(set_queue/2 :: (Queue :: string(), Rec :: #call_queue{}) -> {'atomic', 'ok'} | {'aborted', any()}).
set_queue(Queue, Rec) ->
	?DEFAULT_STORAGE:set_queue(Queue, Rec).

%% =====
%% call queue groups Configs
%% =====

%% @doc Add a new group to the configuation database
-spec(new_queue_group/1 :: (Rec :: #queue_group{}) -> {'atomic', 'ok'}).
new_queue_group(Rec) when is_record(Rec, queue_group) ->
	?DEFAULT_STORAGE:new_queue_group(Rec).

%% @doc Add a new group with name, sort order and recipe
-spec(new_queue_group/3 :: (Name :: string(), Sort :: non_neg_integer(), Recipe :: recipe()) -> {'atomic', 'ok'}).
new_queue_group(Name, Sort, Recipe) when is_integer(Sort) ->
	?DEFAULT_STORAGE:new_queue_group(Name, Sort, Recipe).


%% @doc get a `#queue_group{}' named `Name'
-spec(get_queue_group/1 :: (Name :: string()) -> {'atomic', [#queue_group{}]}).
get_queue_group(Name) ->
	case cpx_hooks:trigger_hooks(get_queue_group, [Name], first) of
		{ok, Entry} -> {atomic, [Entry]};
		_ -> {atomic, []}
	end.

%% @doc Gets all `#queue_group{}' in a list sorted by group.
-spec(get_queue_groups/0 :: () -> [#queue_group{}]).
get_queue_groups() ->
	case cpx_hooks:trigger_hooks(get_queue_groups, [], all) of
		{ok, Lists} ->
			IsUniqueFun = fun(#queue_group{name=Name}, NameSet) ->
				not gb_sets:is_member(Name, NameSet)
			end,

			AddToChecksFun = fun(#queue_group{name=Name}, NameSet) ->
				gb_sets:add(Name, NameSet)
			end,

			Init = gb_sets:new(),
			Groups = get_uniques(IsUniqueFun, AddToChecksFun, Init, Lists),
			Sort = fun(Group1, Group2) ->
				Group1#queue_group.sort =< Group2#queue_group.sort
			end,
			lists:sort(Sort, Groups);
		_ -> []
	end.

%% @doc Set the `#queue_group{}' named `Oldname' to the passed `#queue_group{}' `Rec'.
-spec(set_queue_group/2 :: (Oldname :: string(), Rec :: #queue_group{}) -> {'atomic', atom()} | {'aborted', atom()}).
set_queue_group(Oldname, Rec) when is_record(Rec, queue_group) ->
	?DEFAULT_STORAGE:set_queue_group(Oldname, Rec).

%% @doc Set the Name, Sort, and recipe of the `#queue_group{}' named `Oldname' to `Newname', `Newsort', and `Newrecipe'.
-spec(set_queue_group/4 :: (Oldname :: string(), Newname :: string(), Newsort :: non_neg_integer(), Newrecipe :: recipe()) -> {'atomic', 'ok'}).
set_queue_group(Oldname, Newname, Newsort, Newrecipe) when is_integer(Newsort) ->
	?DEFAULT_STORAGE:set_queue_group(Oldname, Newname, Newsort, Newrecipe).

%% @doc remove the queue_group named `Groupname' from the database.
-spec(destroy_queue_group/1 :: (Groupname :: string()) -> {'atomic', 'ok'} | {'atomic', {'error', 'protected'}}).
destroy_queue_group(Groupname) ->
	?DEFAULT_STORAGE:destroy_queue_group(Groupname).

%% =====
%% Skill Configs
%% =====

%% @doc Add a new skill to the configuration database.  `atom()' `Skillatom', `string()' `Skillname', 
%% `string()' `Skilldesc', `string()' `Creator'.
%% @see new_skill
-spec(new_skill/4 :: (Skillatom :: atom(), Skillname :: string(), Skilldesc :: string(), Group :: string()) -> {'atomic', 'ok'}).
new_skill(Skillatom, Skillname, Skilldesc, Group) when is_atom(Skillatom), is_list(Group) ->
	?DEFAULT_STORAGE:new_skill(Skillatom, Skillname, Skilldesc, Group).

%% @doc Add `#skill_rec{}' `Rec' to the configuration database.
-spec(new_skill/1 :: (Rec :: #skill_rec{}) -> {'atomic', 'ok'}).
new_skill(Rec) when is_record(Rec, skill_rec) ->
	?DEFAULT_STORAGE:new_skill(Rec).
	
%% @doc Check if the given `string()' `Skillname' exists.
%% Returns the `atom()' of `Skillname' or `undefined'
-spec(skill_exists/1 :: (Skillname :: string()) -> atom()).
skill_exists(Skillname) when is_list(Skillname) ->
	case get_skill(Skillname) of
		undefined -> undefined;
		Skillrec -> Skillrec#skill_rec.atom
	end.

%% @doc get a single `#skill_rec{}'


-spec(get_skill/1 :: (Skill :: atom() | string()) -> #skill_rec{} | 'undefined').
get_skill(Skill) when is_list(Skill) ->
	try list_to_existing_atom(Skill) of
		Anything ->
			get_skill(Anything)
	catch
		error:_Anyerror ->
			undefined
	end;
get_skill(Skill) when is_atom(Skill) ->
	case cpx_hooks:trigger_hooks(get_skill, [Skill], first) of
		{ok, Entry} -> Entry;
		_ -> undefined
	end.

-spec(set_skill/4 :: (Skillatom :: atom(), Skillname :: string(), Skilldesc :: string(), Group :: string()) -> {'atomic', 'ok'}).
set_skill(Skillatom, Skillname, Skilldesc, Group) when is_atom(Skillatom), is_list(Group) ->
	?DEFAULT_STORAGE:set_skill(Skillatom, Skillname, Skilldesc, Group).

%% @doc updates the skill Old skill with the data in the Newrec; the atom remains unchanged
-spec(set_skill/2 :: (Oldskill :: atom(), Newrec :: #skill_rec{}) -> {'atomic', 'ok'} | {'atomic', 'undefined'}).
set_skill(Oldskill, Newrec) when is_atom(Oldskill), is_record(Newrec, skill_rec) ->
	?DEFAULT_STORAGE:set_skill(Oldskill, Newrec).

%% @doc Return `[#skill_rec{}]' in the system sorted by group
-spec(get_skills/0 :: () -> [#skill_rec{}]).
get_skills() ->
	case cpx_hooks:trigger_hooks(get_skills, [], all) of
		{ok, Lists} ->
			IsUniqueFun = fun(#skill_rec{atom=Atom}, AtomSet) ->
				not gb_sets:is_member(Atom, AtomSet)
			end,

			AddToChecksFun = fun(#skill_rec{atom=Atom}, AtomSet) ->
				gb_sets:add(Atom, AtomSet)
			end,

			Init = gb_sets:new(),
			Skills = get_uniques(IsUniqueFun, AddToChecksFun, Init, Lists),
			Compare = fun(Skill1, Skill2) ->
				Skill1#skill_rec.group =< Skill2#skill_rec.group
			end,
			lists:sort(Compare, Skills);
		_ -> []
	end.

%% @doc Returns `[#skill_rec{}]' in the system which have a group of `string()' `Group'.
-spec(get_skills/1 :: (Group :: string()) -> [#skill_rec{}]).
get_skills(Group) when is_list(Group) ->
	[X || X <- get_skills(), X#skill_rec.group =:= Group].

%% @doc Removes the skill named `string()' `Skillname' from the database.  The 
%% atom is still in the system, so this is just for looks.
-spec(destroy_skill/1 :: (Skillname :: string()) -> {'atomic', 'ok'} | {'error', 'protected'}).
destroy_skill(Skillname) ->
	?DEFAULT_STORAGE:destroy_skill(Skillname).

%% @doc Move every skill in Oldgroup to Newgroup.
-spec(rename_skill_group/2 :: (Oldgroup :: string(), Newgroup :: string()) -> {'atomic', 'ok'} | {'error', {'exists', string()}}).
rename_skill_group(Oldgroup, Newgroup) when is_list(Newgroup) ->
	?DEFAULT_STORAGE:rename_skill_group(Oldgroup, Newgroup).

%% =====
%% Client configs
%% =====

%% @doc Add a new client with `string()' `Label', `integer()' `Tenantid', and `integer()' `Brandid'.
%% @see new_client/1
-spec(new_client/3 :: (Label :: string(), Id :: string(), Options :: client_opts()) -> {'atomic', 'ok'} | {'aborted', any()}).
new_client(Label, ID, Options) ->
	?DEFAULT_STORAGE:new_client(Label, ID, Options).

-spec(new_client/1 :: (Rec :: #client{}) -> {'atomic', 'ok'} | {'aborted', any()}).
%% @doc Add a new client based on `#client{}' `Rec'.
new_client(Rec) when is_record(Rec, client) ->
	?DEFAULT_STORAGE:new_client(Rec).

%% @doc Update the client `string() Id' to `string()' `Newlabel', 'client_opts() Options'.
%% @see set_client/2
-spec(set_client/3 :: (Id :: string() | 'undefined', Newlabel :: string(), Options :: client_opts() ) -> {'atomic', 'ok'} | {'aborted', any()}).
set_client(Id, Newlabel, Options) ->
	?DEFAULT_STORAGE:set_client(Id, Newlabel, Options).

%% @doc Update the client `string() Id' to the `#client{}' `Client'.
-spec(set_client/2 :: (Id :: string() | 'undefined', Client :: #client{}) -> {'atomic', 'ok'} | {'aborted', any()}).
set_client(Id, Client) when is_record(Client, client) ->
	?DEFAULT_STORAGE:set_client(Id, Client).

%% @doc Removed the client id `Id' from the client database.
-spec(destroy_client/1 :: (Id :: string()) -> {'atomic', 'ok'} | {'aborted', 'protected'}).
destroy_client(Id) ->
	?DEFAULT_STORAGE:destroy_client(Id).

-spec(destroy_client/2 :: (Key :: 'id' | 'label', Value :: string()) -> {'atomic', 'ok'} | {'aborted', 'protected'}).
destroy_client(Key, Val) ->
	?DEFAULT_STORAGE:destroy_client(Key, Val).

%% @doc Get the `#client{}' associated with the id `Id' using integration
%% if possible.
-spec(get_client/1 :: (Label :: string() | 'undefined') -> #client{} | 'none').
get_client(Label) ->
	get_client(label, Label).

%% @doc Get the `#client{}' associated with the give `Key Value'.
-spec(get_client/2 :: (Key :: 'label' | 'id', Value :: string() | 'undefined') -> #client{} | 'none').
% get_client(_Key, undefined) ->
% 	get_default_client();
get_client(Key, Value) ->
	case cpx_hooks:trigger_hooks(get_client, [Key, Value], first) of
		{ok, Entry} -> Entry;
		_ -> none
	end.

%% @doc Gets the default client.
-spec(get_default_client/0 :: () -> #client{}).
get_default_client() ->
	get_client(id, undefined).

%% @doc Gets `[#client{}]' sorted by `#client.label'.
-spec(get_clients/0 :: () -> [#client{}]).
get_clients() ->
	case cpx_hooks:trigger_hooks(get_clients, [], all) of
		{ok, Lists} ->
			IsUniqueFun = fun(#client{id=Id, label=Label}, {IdSet, LabelSet}) ->
				not (gb_sets:is_member(Id, IdSet) or gb_sets:is_member(Label, LabelSet))
			end,

			AddToChecksFun = fun(#client{id=Id, label=Label}, {IdSet, LabelSet}) ->
				{gb_sets:add(Id, IdSet), gb_sets:add(Label, LabelSet)}
			end,

			Init = {gb_sets:new(), gb_sets:new()},
			Clients = get_uniques(IsUniqueFun, AddToChecksFun, Init, Lists),
			Sort = fun(Client1, Client2) ->
				Client1#client.label =< Client2#client.label
			end,
			lists:sort(Sort, Clients);
		_ -> []
	end.

%% =====
%% Internal / helper functions
%% =====

correct_recipe(Recipe) ->
	correct_recipe(Recipe, []).

correct_recipe([], Acc) ->
	lists:reverse(Acc);
correct_recipe([{Cond, Op, Args, Runs} | Tail], Acc) when is_atom(Runs) ->
	correct_recipe(Tail, [{Cond, [{Op, Args}], Runs, <<"No Comment">>} | Acc]);
correct_recipe([H | T], Acc) ->
	correct_recipe(T, [H | Acc]).


get_uniques(IsUniqueFun, AddToChecksFun, Init, EntriesLists) ->
	GetUniquesFun = fun(Entry, {UniqueCheckAcc, UniqueEntries}) ->
		case IsUniqueFun(Entry, UniqueCheckAcc) of
			true -> {AddToChecksFun(Entry, UniqueCheckAcc), [Entry|UniqueEntries]};
			false -> {UniqueCheckAcc, UniqueEntries}
		end
	end,

	{_, Uniques} = lists:foldl(fun(Entries, Acc) ->
		lists:foldl(GetUniquesFun, Acc, Entries)
	end, {Init, []}, EntriesLists),

	lists:reverse(Uniques).

%% =====
%% Tests
%% =====

-ifdef(TEST).

start_test_() ->
	[{"storage set", fun() ->
		application:set_env('OpenACD', call_queue_config_storage, somestore),

		meck:new(somestore),
		meck:expect(somestore, start, fun() -> ok end),
		?assert(meck:validate(somestore)),
		meck:unload(somestore)
	end},
	{"default mnesia store", fun() ->
		application:unset_env('OpenACD', call_queue_config_storage),
		meck:new(call_queue_config_mnesia),
		meck:expect(call_queue_config_mnesia, start, fun() -> ok end),

		call_queue_config:start(),
		?assert(meck:called(call_queue_config_mnesia, start, [])),
		?assert(meck:validate(call_queue_config_mnesia)),
		meck:unload(call_queue_config_mnesia)				
	end}].

get_queues_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(get_queues),
		cpx_hooks:set_hook(a, get_queues, somestore, get_queues, [], 10),

		meck:new(somestore)
	end,
	fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, get_queues, fun() -> none end),
		?assertEqual([], get_queues())
	end},
	{"normal", fun() ->
		meck:expect(somestore, get_queues, 0, 
			{ok, [#call_queue{name="ali"},
			#call_queue{name="baba"}]}),

		?assertEqual([#call_queue{name="ali"},
			#call_queue{name="baba"}], get_queues())
	end},
	{"multiple sources", fun() ->
		Queue1ali = #call_queue{name="ali"},
		Queue2baba = #call_queue{name="baba"},
		Queue3kazam = #call_queue{name="kazam"},

		meck:expect(somestore, get_queues, 0,
			{ok, [Queue1ali, Queue2baba]}),

		meck:expect(somestore, get_queues2, 0, 
			{ok, [Queue3kazam]}),

		cpx_hooks:set_hook(b, get_queues, somestore, get_queues2, [], 5),

		?assertEqual([Queue1ali, Queue2baba, Queue3kazam], get_queues())
	end},
	{"name conflict", fun() ->
		Queue1ali = #call_queue{name="ali"},
		Queue2baba = #call_queue{name="baba"},
		Queue3ali = #call_queue{name="ali", skills=[jumping]},

		meck:expect(somestore, get_queues, 0, 
			{ok, [Queue1ali, Queue2baba]}),

		meck:expect(somestore, get_queues2, 0, 
			{ok, [Queue3ali]}),

		cpx_hooks:set_hook(b, get_queues, somestore, get_queues2, [], 5),

		?assertEqual([Queue1ali, Queue2baba], get_queues())
	end}]}.

% get_queues_by_group_test_() ->
% 	{foreach, fun() ->
% 		cpx_hooks:start_link(),
% 		cpx_hooks:drop_hooks(get_queues_by_group),
% 		cpx_hooks:set_hook(a, get_queues_by_group, somestore, get_queues_by_group, [], 10),

% 		meck:new(somestore)
% 	end,
% 	fun(_) ->
% 		meck:unload(somestore)
% 	end,
% 	[{"unhandled", fun() ->
% 		meck:expect(somestore, get_queues_by_group, fun(_) -> none end),
% 		?assertEqual([], get_queues("nogroup"))
% 	end},
% 	{"normal", fun() ->
% 		Entry1 = #call_queue{name="ali", group="persia"},
% 		Entry2 = #call_queue{name="baba", group="persia"},
		
% 		meck:expect(somestore, get_queues_by_group, 1, 
% 			{ok, [Entry1, Entry2]}),

% 		?assertEqual([Entry1, Entry2], get_queues("persia"))
% 	end}]}. %% No handling of duplicates

get_queue_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(get_queue),
		cpx_hooks:set_hook(a, get_queue, somestore, get_queue, [], 10),

		meck:new(somestore)
	end, fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, get_queue, fun(_) -> none end),
		?assertEqual(noexists, get_queue("nothing")),
		?assert(meck:validate(somestore))
	end},
	{"by name", fun() ->
		cpx_hooks:set_hook(b, get_queue, somestore, get_queue2, [], 5),

		Entry = #call_queue{name="kebab"},

		meck:expect(somestore, get_queue, fun(_) -> none end),
		meck:expect(somestore, get_queue2, fun("kebab") -> {ok, Entry} end),

		?assertEqual(Entry, get_queue("kebab")),

		?assert(meck:validate(somestore))
	end}]}.

get_queue_groups_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(get_queue_groups),
		cpx_hooks:set_hook(a, get_queue_groups, somestore, get_queue_groups, [], 10),

		meck:new(somestore)
	end,
	fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, get_queue_groups, fun() -> none end),
		?assertEqual([], get_queue_groups())
	end},
	{"normal sorted", fun() ->
		meck:expect(somestore, get_queue_groups, 0, 
			{ok, [#queue_group{name="ali", sort=5},
			#queue_group{name="baba", sort=1},
			#queue_group{name="zeta", sort=5}]}),

		?assertMatch([#queue_group{name="baba"},
			#queue_group{name="ali"},
			#queue_group{name="zeta"}], get_queue_groups())
	end},
	{"multiple sources", fun() ->
		Group1ali = #queue_group{name="ali"},
		Group2baba = #queue_group{name="baba"},
		Group3kazam = #queue_group{name="kazam"},

		meck:expect(somestore, get_queue_groups, 0,
			{ok, [Group1ali, Group2baba]}),

		meck:expect(somestore, get_queue_groups2, 0, 
			{ok, [Group3kazam]}),

		cpx_hooks:set_hook(b, get_queue_groups, somestore, get_queue_groups2, [], 5),

		?assertEqual([Group1ali, Group2baba, Group3kazam], get_queue_groups())
	end},
	{"name conflict", fun() ->
		Group1ali = #queue_group{name="ali"},
		Group2baba = #queue_group{name="baba"},
		Group3ali = #queue_group{name="ali", skills=[jumping]},

		meck:expect(somestore, get_queue_groups, 0, 
			{ok, [Group1ali, Group2baba]}),

		meck:expect(somestore, get_queue_groups2, 0, 
			{ok, [Group3ali]}),

		cpx_hooks:set_hook(b, get_queue_groups, somestore, get_queue_groups2, [], 5),

		?assertEqual([Group1ali, Group2baba], get_queue_groups())
	end}]}.

get_queue_group_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(get_queue_group),
		cpx_hooks:set_hook(a, get_queue_group, somestore, get_queue_group, [], 10),

		meck:new(somestore)
	end, fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, get_queue_group, fun(_) -> none end),
		?assertEqual({atomic, []}, get_queue_group("nothing")),
		?assert(meck:validate(somestore))
	end},
	{"by name", fun() ->
		cpx_hooks:set_hook(b, get_queue_group, somestore, get_queue_group2, [], 5),

		Entry = #queue_group{name="kebab"},

		meck:expect(somestore, get_queue_group, fun(_) -> none end),
		meck:expect(somestore, get_queue_group2, fun("food") -> {ok, Entry} end),

		?assertEqual({atomic, [Entry]}, get_queue_group("food")),

		?assert(meck:validate(somestore))
	end}]}.

get_skills_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(get_skills),
		cpx_hooks:set_hook(a, get_skills, somestore, get_skills, [], 10),

		meck:new(somestore)
	end,
	fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, get_skills, fun() -> none end),
		?assertEqual([], get_skills())
	end},
	{"normal sorted", fun() ->
		meck:expect(somestore, get_skills, 0, 
			{ok, [#skill_rec{atom=ali, group="b"},
			#skill_rec{atom=baba, group="a"},
			#skill_rec{atom=zeta, group="b"}]}),

		?assertMatch([#skill_rec{atom=baba},
			#skill_rec{atom=ali},
			#skill_rec{atom=zeta}], get_skills())
	end},
	{"multiple sources", fun() ->
		Entry1ali = #skill_rec{atom=ali},
		Entry2baba = #skill_rec{atom=baba},
		Entry3kazam = #skill_rec{atom=kazam},

		meck:expect(somestore, get_skills, 0,
			{ok, [Entry1ali, Entry2baba]}),

		meck:expect(somestore, get_skills2, 0, 
			{ok, [Entry3kazam]}),

		cpx_hooks:set_hook(b, get_skills, somestore, get_skills2, [], 5),

		?assertEqual([Entry1ali, Entry2baba, Entry3kazam], get_skills())
	end},
	{"name conflict", fun() ->
		Entry1ali = #skill_rec{atom=ali},
		Entry2baba = #skill_rec{atom=baba},
		Entry3ali = #skill_rec{atom=ali, group="a"},

		meck:expect(somestore, get_skills, 0, 
			{ok, [Entry1ali, Entry2baba]}),

		meck:expect(somestore, get_skills2, 0, 
			{ok, [Entry3ali]}),

		cpx_hooks:set_hook(b, get_skills, somestore, get_skills2, [], 5),

		?assertEqual([Entry1ali, Entry2baba], get_skills())
	end}]}.

get_skills_by_group_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(get_skills_by_group),
		cpx_hooks:set_hook(a, get_skills_by_group, somestore, get_skills_by_group, [], 10),

		meck:new(somestore)
	end,
	fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, get_skills_by_group, fun(_) -> none end),
		?assertEqual([], get_skills("nogroup"))
	end},
	{"normal", fun() ->
		Entry1 = #skill_rec{atom=dance, group="talent"},
		Entry2 = #skill_rec{atom=sing, group="talent"},
		
		meck:expect(somestore, get_skills_by_group, 1, 
			{ok, [Entry1, Entry2]}),

		?assertEqual([Entry1, Entry2], get_skills("talent"))
	end}]}. %% No handling of duplicates


get_skill_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(get_skill),
		cpx_hooks:set_hook(a, get_skill, somestore, get_skill, [], 10),

		meck:new(somestore)
	end, fun(_) ->
		meck:unload(somestore)
	end,
	[{"no atom", fun() ->
		?assertEqual(undefined, get_skill("nosuchatom"))
	end},
	{"unhandled", fun() ->
		meck:expect(somestore, get_skill, fun(_) -> none end),
		?assertEqual(undefined, get_skill(nothing)),
		?assertEqual(undefined, get_skill("nothing")),
		?assert(meck:validate(somestore))
	end},
	{"normal", fun() ->
		cpx_hooks:set_hook(b, get_skill, somestore, get_skill2, [], 5),

		Entry = #skill_rec{atom=cook},

		meck:expect(somestore, get_skill, fun(_) -> none end),
		meck:expect(somestore, get_skill2, fun(_) -> {ok, Entry} end),

		?assertEqual(Entry, get_skill(cook)),
		?assertEqual(Entry, get_skill("cook")),

		?assert(meck:validate(somestore))
	end}]}.

get_clients_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(get_clients),
		cpx_hooks:set_hook(a, get_clients, somestore, get_clients, [], 10),

		meck:new(somestore)
	end,
	fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, get_clients, fun() -> none end),
		?assertEqual([], get_clients())
	end},
	{"normal sorted", fun() ->
		meck:expect(somestore, get_clients, 0, 
			{ok, [#client{id="1", label="ali"},
			#client{id="2", label="baba"},
			#client{id="3", label="zeta"}]}),

		?assertMatch([#client{label="ali"},
			#client{label="baba"},
			#client{label="zeta"}], get_clients())
	end},
	{"multiple sources", fun() ->
		Entry1ali = #client{id="1", label="ali"},
		Entry2baba = #client{id="2", label="baba"},
		Entry3kazam = #client{id="3", label="kazam"},

		meck:expect(somestore, get_clients, 0,
			{ok, [Entry1ali, Entry2baba]}),

		meck:expect(somestore, get_clients2, 0, 
			{ok, [Entry3kazam]}),

		cpx_hooks:set_hook(b, get_clients, somestore, get_clients2, [], 5),

		?assertEqual([Entry1ali, Entry2baba, Entry3kazam], get_clients())
	end},
	{"name conflict", fun() ->
		Entry1ali = #client{id="1", label="ali"},
		Entry2baba = #client{id="2", label="baba"},
		Entry3ali = #client{id="3", label="ali"},

		meck:expect(somestore, get_clients, 0, 
			{ok, [Entry1ali, Entry2baba]}),

		meck:expect(somestore, get_clients2, 0, 
			{ok, [Entry3ali]}),

		cpx_hooks:set_hook(b, get_clients, somestore, get_clients2, [], 5),

		?assertEqual([Entry1ali, Entry2baba], get_clients())
	end}]}.

get_client_test_() ->
	{foreach, fun() ->
		cpx_hooks:start_link(),
		cpx_hooks:drop_hooks(get_client),
		cpx_hooks:set_hook(a, get_client, somestore, get_client, [], 10),

		meck:new(somestore)
	end, fun(_) ->
		meck:unload(somestore)
	end,
	[{"unhandled", fun() ->
		meck:expect(somestore, get_client, fun(_, _) -> none end),
		?assertEqual(none, get_client(id, "noid")),
		?assertEqual(none, get_client(label, "nolabel")),
		?assert(meck:validate(somestore))
	end},
	{"by name", fun() ->
		cpx_hooks:set_hook(b, get_client, somestore, get_client2, [], 5),

		Entry = #client{id="1", label="foodie"},

		meck:expect(somestore, get_client, fun(_, _) -> none end),
		meck:expect(somestore, get_client2, fun(_, _) -> {ok, Entry} end),

		?assertEqual(Entry, get_client(id, "1")),
		?assertEqual(Entry, get_client(label, "foodie")),

		?assert(meck:validate(somestore))
	end}]}.

-endif.
