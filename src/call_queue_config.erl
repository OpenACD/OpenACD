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

%% @doc The helper module to config the call_queues.
%% Uses the mnesia table 'call_queue.'  Queues are not started until a call requires it.
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

-include("log.hrl").
-include("queue.hrl").
-include("call.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API

%%
-export([
	build_tables/0,
	merge/3
]).
-export([
	new_queue/1, 
	new_queue/5,
	destroy_queue/1,
	get_queue/1,
	get_merged_queue/1,
	get_queues/0,
	get_queues/1,
	set_queue/2
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

%% @doc Attempts to set-up and create the required mnesia table `call_queue' on 
%% the local node if it does not exist.
-spec(build_tables/0 :: () ->'ok').
build_tables() -> 
	?DEBUG("~p building tables...", [?MODULE]),
	A = util:build_table(call_queue, ?QUEUE_TABLE([node()])),
	case A of
		{atomic, ok} -> 
			% since the table didn't already exist, build up the default queue
			new_queue(#call_queue{name = "default_queue"}),
			ok;
		_Else -> 
			ok
	end,
	B = util:build_table(skill_rec, ?SKILL_TABLE([node()])),
	case B of
		{atomic, ok} ->
			% since the table didn't already exist, build up some default skills
			F = fun() -> 
				mnesia:write(#skill_rec{name="English", atom=english, description="English", group = "Language"}),
				mnesia:write(#skill_rec{name="German", atom=german, description="German", group = "Language"}),
				mnesia:write(#skill_rec{name="Agent Name", atom='_agent', description="Magic skill that is replaced by the agent's name.", group = "Magic", protected = true}),
				mnesia:write(#skill_rec{name="Agent Profile", atom='_profile', description="Magic skill that is replaced by the agent's profile name", group = "Magic", protected = true}),
				mnesia:write(#skill_rec{name="Node", atom='_node', description="Magic skill that is replaced by the node identifier.", group = "Magic", protected = true}),
				mnesia:write(#skill_rec{name="Queue", atom='_queue', description="Magic skill replaced by a queue's name", group = "Magic", protected = true}),
				mnesia:write(#skill_rec{name="All", atom='_all', description="Magic skill to denote an agent that can answer any call regardless of other skills.", group = "Magic", protected = true}),
				mnesia:write(#skill_rec{name="Brand", atom='_brand', description="Magic skill to expand to a client's label (brand)", group="Magic", protected=true})
			end,
			case mnesia:transaction(F) of
				{atomic, ok} -> 
					ok;
				Otherwise -> 
					Otherwise
			end;
		_Or -> 
			ok
	end,
	C = util:build_table(client, ?CLIENT_TABLE([node()])),
	case C of
		{atomic, ok} ->
			Addc = fun() ->
				mnesia:write(#client{label = "Demo Client", id="00990099"}),
				mnesia:write(#client{label = undefined, id = undefined})
			end,
			mnesia:transaction(Addc);
		_Orelse ->
			ok
	end,
	D = util:build_table(queue_group, ?QUEUE_GROUP_TABLE([node()])),
	case D of
		{atomic, ok} ->
			case new_queue_group(?DEFAULT_QUEUE_GROUP) of
				{atomic, ok} ->
					ok
			end;
		_Or3 ->
			ok
	end.

%% @doc utiltiy function to help recover from a netsplit.  Since a merge can 
%% take a while, it is recommended to put this in a spawn.  When the merge is
%% complete, the resulting merged records are sent to `pid Replyto'.  
-spec(merge/3 :: (Nodes :: [atom()], Time :: pos_integer(), Replyto :: pid()) -> 'ok').
merge(Nodes, Time, Replyto) ->
	Queues = merge_queues(Nodes, Time),
	Groups = merge_queue_groups(Nodes, Time),
	Skills = merge_skills(Nodes, Time),
	Clients = merge_clients(Nodes, Time),
	Recs = lists:append([Queues, Groups, Skills, Clients]),
	Replyto ! {merge_complete, call_queue_config, Recs},
	ok.

%% =====
%% Call queue
%% =====

%% @doc Attempt to remove the queue `#call_queue{}' or `string()' `Queue' from the configuration database.
-spec(destroy_queue/1 :: (Queue :: #call_queue{}) -> {atom(), any()};
					(Queue :: string()) -> {atom(), any()}).
destroy_queue(Queue) when is_record(Queue, call_queue) -> 
	destroy_queue(Queue#call_queue.name);
destroy_queue(Queue) -> 
	F = fun() -> 
		mnesia:delete({call_queue, Queue})
	end,
	mnesia:transaction(F).
	
%% @doc Get the configuration for the passed `string()' `Queue' name.
-spec(get_queue/1 :: (Queue :: string()) -> #call_queue{} | {'noexists', any()} | 'noexists').
get_queue(Queue) ->
	F = fun() -> 
		mnesia:read({call_queue, Queue})
	end,
	case mnesia:transaction(F) of
		{atomic, []} -> 
			noexists;
		{atomic, [Rec]} when is_record(Rec, call_queue) ->
			Oldrecipe = Rec#call_queue.recipe,
			case correct_recipe(Rec#call_queue.recipe) of
				Oldrecipe ->
					Rec;
				NewRecipe ->
					Outrec = Rec#call_queue{recipe = NewRecipe},
					set_queue(Queue, Outrec),
					Outrec
			end;
		Else -> 
			{noexists, Else}
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
	QH = qlc:q([X || X <- mnesia:table(call_queue), X#call_queue.group =:= Group]),
	F = fun() ->
		qlc:e(QH)
	end,
	{atomic, MidQueues} = mnesia:transaction(F),
	Queues = [begin 
		Oldrecipe = Rec#call_queue.recipe,
		case correct_recipe(Rec#call_queue.recipe) of
		Oldrecipe ->
			Rec;
		NewRecipe ->
			Outrec = Rec#call_queue{recipe = NewRecipe},
			set_queue(Rec#call_queue.name, Outrec),
			Outrec
		end
	end || Rec <- MidQueues ],
	lists:sort(fun(Qreca, Qrecb) -> Qreca#call_queue.name =< Qrecb#call_queue.name end, Queues).
	
%% @doc Get all the queue configurations (`[#call_queue{}]').
-spec(get_queues/0 :: () -> [#call_queue{}]).
get_queues() -> 
	QH = qlc:q([{X, G#queue_group.recipe, G#queue_group.skills} || 
		X <- mnesia:table(call_queue), 
		G <- mnesia:table(queue_group), 
		G#queue_group.name =:= X#call_queue.group
	]),
	F = fun() -> 
		qlc:e(QH)
	end,
	{atomic, Reply} = mnesia:transaction(F),
	Mergereci = fun({#call_queue{recipe = Qreci} = Queue, Groupreci, Gskills}) ->
		Newreci = lists:append([Qreci, Groupreci]),
		Queue#call_queue{
			recipe = correct_recipe(Newreci),
			skills = lists:umerge(lists:sort(Queue#call_queue.skills), lists:sort(Gskills))
		}
	end,
	lists:map(Mergereci, Reply).

%% @doc Create a new default queue configuraiton from `#call_queue{} Queue'.
%% @see new_queue/2
-spec(new_queue/1 :: (QueueName :: #call_queue{} ) -> {'aborted', any()} | {'atomic', #call_queue{}}).
new_queue(Queue) when is_record(Queue, call_queue) ->
	F = fun() ->
		mnesia:write(Queue#call_queue{timestamp = util:now()})
	end,
	Trans = mnesia:transaction(F),
	case whereis(queue_manager) of
		undefined ->
			Trans;
		Qmpid when is_pid(Qmpid) ->
			queue_manager:add_queue(Queue#call_queue.name, [
				{recipe, Queue#call_queue.recipe}, 
				{weight, Queue#call_queue.weight},
				{skills, Queue#call_queue.skills}
			]),
			Trans
	end.

%% @doc Create a new queue from the given `string() Name, pos_integer() Weight, [atom()] Skills, recipe() Recipe, string() Group'.
-spec(new_queue/5 :: (Name :: string(), Weight :: pos_integer(), Skills :: [atom() | {atom(), any()}], Recipe :: recipe(), Group :: string()) -> {'aborted', any()} | {'atomic', #call_queue{}}).
new_queue(Name, Weight, Skills, Recipe, Group) when Weight > 0, is_integer(Weight) ->
	Rec = #call_queue{
		name = Name,
		weight = Weight,
		skills = Skills,
		recipe = Recipe,
		group = Group},
	new_queue(Rec).

%% @doc Sets the queue name `Queue' to the passed `#call_queue{}'.
-spec(set_queue/2 :: (Queue :: string(), Rec :: #call_queue{}) -> {'atomic', 'ok'} | {'aborted', any()}).
set_queue(Queue, Rec) ->
	F = fun() ->
		case mnesia:read({call_queue, Queue}) of
			[_OldRec] ->
				mnesia:delete({call_queue, Queue}),
				mnesia:write(Rec#call_queue{timestamp = util:now()})
		end
	end,
	mnesia:transaction(F).
	
%% @hidden
-spec(merge_queues/2 :: (Nodes :: [atom()], Time :: pos_integer()) -> [#call_queue{}]).
merge_queues(Nodes, Time) ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(call_queue), X#call_queue.timestamp > Time]),
		qlc:e(QH)
	end,
	merge_results(query_nodes(Nodes, F)).
		
%% =====
%% call queue groups Configs
%% =====

%% @doc Add a new group to the configuation database
-spec(new_queue_group/1 :: (Rec :: #queue_group{}) -> {'atomic', 'ok'}).
new_queue_group(Rec) when is_record(Rec, queue_group) ->
	F = fun() ->
		mnesia:write(Rec)
	end,
	mnesia:transaction(F).

%% @doc Add a new group with name, sort order and recipe
-spec(new_queue_group/3 :: (Name :: string(), Sort :: non_neg_integer(), Recipe :: recipe()) -> {'atomic', 'ok'}).
new_queue_group(Name, Sort, Recipe) when is_integer(Sort) ->
	Qgroup = #queue_group{name = Name, sort = Sort, recipe = Recipe},
	new_queue_group(Qgroup).

%% @doc get a `#queue_group{}' named `Name'
-spec(get_queue_group/1 :: (Name :: string()) -> {'atomic', [#queue_group{}]}).
get_queue_group(Name) ->
	F = fun() ->
		QH = qlc:q([begin
			Oldrecipe =X#queue_group.recipe,
			case correct_recipe(X#queue_group.recipe) of
				Oldrecipe ->
					X;
				NewRecipe ->
					Outrec = X#queue_group{recipe = NewRecipe},
					mnesia:write(Outrec#queue_group{timestamp = util:now()}),
					Outrec
			end
		end || X <- mnesia:table(queue_group), X#queue_group.name =:= Name]),
		qlc:e(QH)
	end,
	mnesia:transaction(F).

%% @doc Gets all `#queue_group{}' in a list sorted by group.
-spec(get_queue_groups/0 :: () -> [#queue_group{}]).
get_queue_groups() ->
	F = fun() ->
		QH = qlc:q([begin
			Oldrecipe =X#queue_group.recipe,
			case correct_recipe(X#queue_group.recipe) of
				Oldrecipe ->
					X;
				NewRecipe ->
					Outrec = X#queue_group{timestamp = util:now(), recipe = NewRecipe},
					mnesia:write(Outrec),
					Outrec
			end
		end || X <- mnesia:table(queue_group)]),
		qlc:e(QH)
	end,
	{atomic, Groups} = mnesia:transaction(F),
	Sort = fun(Group1, Group2) ->
		Group1#queue_group.sort < Group2#queue_group.sort
	end,
	lists:sort(Sort, Groups).

%% @doc Set the `#queue_group{}' named `Oldname' to the passed `#queue_group{}' `Rec'.
-spec(set_queue_group/2 :: (Oldname :: string(), Rec :: #queue_group{}) -> {'atomic', atom()} | {'aborted', atom()}).
set_queue_group(Oldname, Rec) when is_record(Rec, queue_group) ->
	F = fun() ->
		case mnesia:read({queue_group, Oldname}) of
			[] ->
				{error, {noexists, Oldname}};
			[#queue_group{protected = Prot}] ->
				Newrec = Rec#queue_group{protected = Prot},
				mnesia:delete({queue_group, Oldname}),
				mnesia:write(Newrec#queue_group{timestamp = util:now()}),
				QH = qlc:q([Queue || Queue <- mnesia:table(call_queue), Queue#call_queue.group =:= Oldname]),
				Queues = qlc:e(QH),
				Regroupqs = fun(Queue) ->
					Newqrec = Queue#call_queue{group = Rec#queue_group.name},
					mnesia:delete({call_queue, Queue#call_queue.name}),
					mnesia:write(Newqrec#call_queue{timestamp = util:now()})
				end,
				lists:map(Regroupqs, Queues),
				ok
		end
	end,
	mnesia:transaction(F).

%% @doc Set the Name, Sort, and recipe of the `#queue_group{}' named `Oldname' to `Newname', `Newsort', and `Newrecipe'.
-spec(set_queue_group/4 :: (Oldname :: string(), Newname :: string(), Newsort :: non_neg_integer(), Newrecipe :: recipe()) -> {'atomic', 'ok'}).
set_queue_group(Oldname, Newname, Newsort, Newrecipe) when is_integer(Newsort) ->
	Rec = #queue_group{name = Newname, sort = Newsort, recipe = Newrecipe},
	set_queue_group(Oldname, Rec).

%% @doc remove the queue_group named `Groupname' from the database.
-spec(destroy_queue_group/1 :: (Groupname :: string()) -> {'atomic', 'ok'} | {'atomic', {'error', 'protected'}}).
destroy_queue_group(Groupname) ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(queue_group), X#queue_group.name =:= Groupname]),
		[Queue] = qlc:e(QH),
		case Queue#queue_group.protected of
			true ->
				{error, protected};
			false ->
				mnesia:delete({queue_group, Groupname}),
				ok
		end
	end,
	mnesia:transaction(F).

%% @hidden
-spec(merge_queue_groups/2 :: (Nodes :: [atom()], Time :: pos_integer()) -> [#queue_group{}]).
merge_queue_groups(Nodes, Time) ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(skill_rec), X#skill_rec.timestamp > Time]),
		qlc:e(QH)
	end,
	merge_results(query_nodes(Nodes, F)).

%% =====
%% Skill Configs
%% =====

%% @doc Add a new skill to the configuration database.  `atom()' `Skillatom', `string()' `Skillname', 
%% `string()' `Skilldesc', `string()' `Creator'.
%% @see new_skill
-spec(new_skill/4 :: (Skillatom :: atom(), Skillname :: string(), Skilldesc :: string(), Group :: string()) -> {'atomic', 'ok'}).
new_skill(Skillatom, Skillname, Skilldesc, Group) when is_atom(Skillatom), is_list(Group) ->
	Rec = #skill_rec{atom = Skillatom, name = Skillname, description = Skilldesc, group = Group},
	new_skill(Rec).

%% @doc Add `#skill_rec{}' `Rec' to the configuration database.
-spec(new_skill/1 :: (Rec :: #skill_rec{}) -> {'atomic', 'ok'}).
new_skill(Rec) when is_record(Rec, skill_rec) ->
	F = fun() -> 
		mnesia:write(Rec)
	end,
	mnesia:transaction(F).
	
%% @doc Check if the given `string()' `Skillname' exists.
%% Returns the `atom()' of `Skillname' or `undefined'
-spec(skill_exists/1 :: (Skillname :: string()) -> atom()).
skill_exists(Skillname) when is_list(Skillname) ->
	try list_to_existing_atom(Skillname) of
		Anything -> 
			F = fun() -> 
				mnesia:read({skill_rec, Anything})
			end,
			case mnesia:transaction(F) of
				{atomic, [Rec|_Tail]} ->
					Rec#skill_rec.atom;
				_Else -> % failure or empty response
					undefined
			end
	catch
		error:_Anyerror -> 
			undefined
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
	F = fun() ->
		mnesia:read({skill_rec, Skill})
	end,
	case mnesia:transaction(F) of
		{atomic, []} ->
			undefined;
		{atomic, [Skillrec]} ->
			Skillrec
	end.

%% @doc updates the skill Old skill with the data in the Newrec; the atom remains unchanged
-spec(set_skill/2 :: (Oldskill :: atom(), Newrec :: #skill_rec{}) -> {'atomic', 'ok'} | {'atomic', 'undefined'}).
set_skill(Oldskill, Newrec) when is_atom(Oldskill), is_record(Newrec, skill_rec) ->
	F = fun() ->
		case mnesia:read({skill_rec, Oldskill}) of
			[] ->
				undefined;
			[_Oldrec] ->
				Forwrite = Newrec#skill_rec{atom = Oldskill, timestamp = util:now()},
				mnesia:delete({skill_rec, Oldskill}),
				mnesia:write(Forwrite),
				ok
		end
	end,
	mnesia:transaction(F).

%% @doc Return `[#skill_rec{}]' in the system sorted by group
-spec(get_skills/0 :: () -> [#skill_rec{}]).
get_skills() ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(skill_rec)]),
		qlc:e(QH)
	end,
	{atomic, Skills} = mnesia:transaction(F),
	Compare = fun(Skill1, Skill2) ->
		Skill1#skill_rec.group =< Skill2#skill_rec.group
	end,
	lists:sort(Compare, Skills).

%% @doc Returns `[#skill_rec{}]' in the system which have a group of `string()' `Group'.
-spec(get_skills/1 :: (Group :: string()) -> [#skill_rec{}]).
get_skills(Group) when is_list(Group) ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(skill_rec), X#skill_rec.group =:= Group]),
		qlc:e(QH)
	end,
	{atomic, Skills} = mnesia:transaction(F),
	Compare = fun(Skill1, Skill2) ->
		Skill1#skill_rec.name =< Skill2#skill_rec.name
	end,
	lists:sort(Compare, Skills).

%% @doc Removes the skill named `string()' `Skillname' from the database.  The 
%% atom is still in the system, so this is just for looks.
-spec(destroy_skill/1 :: (Skillname :: string()) -> {'atomic', 'ok'} | {'error', 'protected'}).
destroy_skill(Skillname) ->
	Get = fun() ->
		QH = qlc:q([X || X <- mnesia:table(skill_rec), X#skill_rec.name =:= Skillname]),
		qlc:e(QH)
	end,
	case mnesia:transaction(Get) of
		{atomic, []} ->
			{atomic, ok};
		{atomic, [#skill_rec{protected = true}]} ->
			{error, protected};
		{atomic, [Skillrec]} ->
			Del = fun() ->
				mnesia:delete({skill_rec, Skillrec#skill_rec.atom})
			end,
			mnesia:transaction(Del)
	end.

%% @doc Move every skill in Oldgroup to Newgroup.
-spec(rename_skill_group/2 :: (Oldgroup :: string(), Newgroup :: string()) -> {'atomic', 'ok'} | {'error', {'exists', string()}}).
rename_skill_group(Oldgroup, Newgroup) when is_list(Newgroup) ->
	Testng = fun() ->
		QH = qlc:q([X || X <- mnesia:table(skill_rec), X#skill_rec.group =:= Newgroup]),
		qlc:e(QH)
	end,
	case mnesia:transaction(Testng) of
		{atomic, []} ->
			Doupdate = fun() ->
				QH = qlc:q([X || X <- mnesia:table(skill_rec), X#skill_rec.group =:= Oldgroup]),
				Skills = qlc:e(QH),
				Update = fun(Oldskill) ->
					Newskill = Oldskill#skill_rec{group = Newgroup},
					mnesia:delete({skill_rec, Oldskill#skill_rec.atom}),
					mnesia:write(Newskill#skill_rec{timestamp = util:now()})
				end,
				lists:map(Update, Skills),
				ok
			end,
			mnesia:transaction(Doupdate);
		{atomic, List} when length(List) >= 1 ->
			?ERROR("error, target name ~p exists", [Newgroup]),
			{error, {exists, Newgroup}}
	end.

%% @hidden
-spec(merge_skills/2 :: (Nodes :: [atom()], Time :: pos_integer()) -> [#skill_rec{}]).
merge_skills(Nodes, Time) ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(skill_rec), X#skill_rec.timestamp > Time]),
		qlc:e(QH)
	end,
	merge_results(query_nodes(Nodes, F)).

%% =====
%% Client configs
%% =====

%% @doc Add a new client with `string()' `Label', `integer()' `Tenantid', and `integer()' `Brandid'.
%% @see new_client/1
-spec(new_client/3 :: (Label :: string(), Id :: string(), Options :: client_opts()) -> {'atomic', 'ok'} | {'aborted', any()}).
new_client(Label, ID, Options) ->
	Rec = #client{label = Label, id = ID, options = Options},
	new_client(Rec).

-spec(new_client/1 :: (Rec :: #client{}) -> {'atomic', 'ok'} | {'aborted', any()}).
%% @doc Add a new client based on `#client{}' `Rec'.
new_client(Rec) when is_record(Rec, client) ->
	F = fun() ->
		NewOptions = merge_client_options(Rec#client.options),
		mnesia:write(Rec#client{options = NewOptions})
	end,
	mnesia:transaction(F).

%% @doc Update the client `string() Id' to `string()' `Newlabel', 'client_opts() Options'.
%% @see set_client/2
-spec(set_client/3 :: (Id :: string() | 'undefined', Newlabel :: string(), Options :: client_opts() ) -> {'atomic', 'ok'} | {'aborted', any()}).
set_client(Id, Newlabel, Options) ->
	Client = #client{label = Newlabel, id = Id, options = Options},
	set_client(Id, Client).

%% @doc Update the client `string() Id' to the `#client{}' `Client'.
-spec(set_client/2 :: (Id :: string() | 'undefined', Client :: #client{}) -> {'atomic', 'ok'} | {'aborted', any()}).
set_client(Id, Client) when is_record(Client, client) ->
	F = fun() ->
		mnesia:delete({client, Id}),
%		Newoptions = case Id of
%			undefined ->
%				Client#client.options;
%			_ ->
%				merge_client_options(Client#client.options)
%		end,
		mnesia:write(Client#client{timestamp = util:now()})
	end,
	mnesia:transaction(F).

%% @doc Removed the client id `Id' from the client database.
-spec(destroy_client/1 :: (Id :: string()) -> {'atomic', 'ok'} | {'aborted', 'protected'}).
destroy_client(Id) ->
	destroy_client(id, Id).

-spec(destroy_client/2 :: (Key :: 'id' | 'label', Value :: string()) -> {'atomic', 'ok'} | {'aborted', 'protected'}).
destroy_client(_Key, undefined) ->
	{aborted, protected};
destroy_client(id, Id) ->
	F = fun() -> 
		mnesia:delete({client, Id})
	end,
	mnesia:transaction(F);
destroy_client(label, Label) ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(client), X#client.label =:= Label]),
		[Rec] = qlc:e(QH),
		mnesia:delete({client, Rec#client.id})
	end,
	mnesia:transaction(F).

%% @doc Get the `#client{}' associated with the id `Id' using integration
%% if possible.
-spec(get_client/1 :: (Label :: string() | 'undefined') -> #client{} | 'none').
get_client(Label) ->
	get_client(label, Label).

%% @doc Get the `#client{}' associated with the give `Key Value'.
-spec(get_client/2 :: (Key :: 'label' | 'id', Value :: string() | 'undefined') -> #client{} | 'none').
get_client(_Key, undefined) ->
	get_default_client();
get_client(Key, Value) ->
	try integration:get_client(Key, Value) of
		none ->
			?DEBUG("integration has no such client ~p", [Value]),
			destroy_client(Key, Value),
			none;
		{ok, Id, Label, Options} ->
			?DEBUG("integration found client ~p", [Label]),
			Client = #client{label = Label, id = Id, options = Options, last_integrated = util:now()},
			set_client(Id, Client),
			local_get_client(Id);
		{error, nointegration} ->
			?DEBUG("No integration, falling back for ~p", [Value]),
			local_get_client(Key, Value)
	catch
		throw:{badreturn, Err} ->
			?WARNING("Integration failed with message:  ~p", [Err]),
			local_get_client(Key, Value)
	end.

%% @doc Gets the default client.
-spec(get_default_client/0 :: () -> #client{}).
get_default_client() ->
	local_get_client(undefined).

%% @doc Skips integration, goes right for the local cache using key as id.
-spec(local_get_client/1 :: (Id :: string() | 'undefined') -> #client{} | 'none').
local_get_client(Id) ->
	local_get_client(id, Id).

-spec(local_get_client/2 :: (Key :: 'id' | 'label', Value :: string() | 'undefined') -> #client{} | 'none').
local_get_client(Key, Value) ->
	F = fun() ->
		QH = case Key of
			id ->
				qlc:q([X || X <- mnesia:table(client), X#client.id =:= Value]);
			label ->
				qlc:q([X || X <- mnesia:table(client), X#client.label =:= Value])
		end,
		qlc:e(QH)
	end,
	case mnesia:transaction(F) of
		{atomic, []} ->
			none;
		{atomic, [#client{id = undefined} = Client]} when is_record(Client, client) ->
			Client;
		{atomic, [Client]} ->
			Options = merge_client_options(Client#client.options),
			Client#client{options = Options}
	end.

%% @doc Gets `[#client{}]' sorted by `#client.label'.
-spec(get_clients/0 :: () -> [#client{}]).
get_clients() ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(client)]),
		qlc:e(QH)
	end,
	{atomic, Clients} = mnesia:transaction(F),
	lists:sort(Clients).

%% @hidden
-spec(merge_clients/2 :: (Nodes :: [atom()], Time :: pos_integer()) -> [#client{}]).
merge_clients(Nodes, Time) ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(client), X#client.timestamp > Time]),
		qlc:e(QH)
	end,
	merge_results(query_nodes(Nodes, F)).

%% =====
%% Internal / helper functions
%% =====

merge_client_options(Given) ->
	#client{options = Defaults} = get_default_client(),
	merge_client_options(Given, Defaults, ?DEFAULTABLE_CLIENT_OPTIONS).

merge_client_options(Given, _Defaults, []) ->
	Given;
merge_client_options(Given, Defaults, [Key | Tail]) ->
	case {proplists:get_value(Key, Given), proplists:get_value(Key, Defaults)} of
		{undefined, undefined} ->
			merge_client_options(Given, Defaults, Tail);
		{undefined, Val} ->
			merge_client_options([{Key, Val} | Given], Defaults, Tail);
		_ ->
			merge_client_options(Given, Defaults, Tail)
	end.

query_nodes(Nodes, F) ->
	query_nodes_loop(Nodes, F, []).

query_nodes_loop([], _F, Acc) ->
	Acc;
query_nodes_loop([N | Nodes], F, Acc) ->
	case mnesia:transaction(F) of
		{atomic, _} = Res ->
			query_nodes_loop(Nodes, F, [Res | Acc]);
		Else ->
			?WARNING("Error querying node ~w:  ~p", [N, Else]),
			query_nodes_loop(Nodes, F, Acc)
	end.

merge_results(Results) ->
	merge_results_loop(Results, []).

merge_results_loop([], Acc) ->
	Acc;
merge_results_loop([{atomic, Recs} | Tail], Acc) ->
	Newacc = diff_recs(Recs, Acc),
	merge_results_loop(Tail, Newacc).

diff_recs(Left, Right) ->
	Sort = fun(A, B) when is_record(A, call_queue) ->
			A#call_queue.name < B#call_queue.name;
		(A, B) when is_record(A, queue_group) ->
			A#queue_group.name < B#queue_group.name;
		(A, B) when is_record(A, skill_rec) ->
			A#skill_rec.name < B#skill_rec.name;
		(A, B) when is_record(A, client) ->
			A#client.label < B#client.label
	end,
	Sleft = lists:sort(Sort, Left),
	Sright = lists:sort(Sort, Right),
	diff_recs_loop(Sleft, Sright, []).

diff_recs_loop([], Right, Acc) ->
	lists:append(lists:reverse(Acc), Right);
diff_recs_loop(Left, [], Acc) ->
	lists:append(lists:reverse(Acc), Left);
diff_recs_loop([Lhead | Ltail] = Left, [Rhead | Rtail] = Right, Acc) ->
	case nom_equal(Lhead, Rhead) of
		true ->
			case timestamp_comp(Lhead, Rhead) of
				false ->
					diff_recs_loop(Ltail, Rtail, [Lhead | Acc]);
				true ->
					diff_recs_loop(Ltail, Rtail, [Rhead | Acc])
			end;
		false ->
			case nom_comp(Lhead, Rhead) of
				true ->
					diff_recs_loop(Ltail, Right, [Lhead | Acc]);
				false ->
					diff_recs_loop(Left, Rtail, [Rhead | Acc])
			end
	end.
	
nom_equal(A, B) when is_record(A, call_queue) ->
	A#call_queue.name =:= B#call_queue.name;
nom_equal(A, B) when is_record(A, queue_group) ->
	A#queue_group.name =:= B#queue_group.name;
nom_equal(A, B) when is_record(A, skill_rec) ->
	A#skill_rec.name =:= B#skill_rec.name;
nom_equal(A, B) when is_record(A, client) ->
	A#client.label =:= B#client.label.

nom_comp(A, B) when is_record(A, call_queue) ->
	A#call_queue.name < B#call_queue.name;
nom_comp(A, B) when is_record(A, queue_group) ->
	A#queue_group.name <  B#queue_group.name;
nom_comp(A, B) when is_record(A, skill_rec) ->
	A#skill_rec.name < B#skill_rec.name;
nom_comp(A, B) when is_record(A, client) ->
	A#client.label < B#client.label.

timestamp_comp(A, B) when is_record(A, call_queue) ->
	A#call_queue.timestamp < B#call_queue.timestamp;
timestamp_comp(A, B) when is_record(A, queue_group) ->
	A#queue_group.timestamp < B#queue_group.timestamp;
timestamp_comp(A, B) when is_record(A, skill_rec) ->
	A#skill_rec.timestamp < B#skill_rec.timestamp;
timestamp_comp(A, B) when is_record(A, client) ->
	A#client.timestamp < B#client.timestamp.

correct_recipe(Recipe) ->
	correct_recipe(Recipe, []).

correct_recipe([], Acc) ->
	lists:reverse(Acc);
correct_recipe([{Cond, Op, Args, Runs} | Tail], Acc) when is_atom(Runs) ->
	correct_recipe(Tail, [{Cond, [{Op, Args}], Runs, <<"No Comment">>} | Acc]);
correct_recipe([H | T], Acc) ->
	correct_recipe(T, [H | Acc]).
%% =====
%% Tests
%% =====

-ifdef(TEST).

test_queue() -> 
	Recipe = [{[{ticks, 2}], [{add_skills, [true]}], run_once}],
	new_queue("test queue", 3, [testskill], Recipe, "Default"),
	%Default = #call_queue{name = "goober"},
	#call_queue{name = "test queue", weight = 3, skills = [testskill], recipe = Recipe, group = "Default"}.
	
call_queue_test_() ->
	{
		foreach,
		fun() -> 
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			mnesia:create_schema([node()]),
			mnesia:start(),
			build_tables(),
			F = fun() -> 
				mnesia:delete({call_queue, "default_queue"})
			end,
			mnesia:transaction(F),
			Dumptestq = fun() ->
				mnesia:transaction(fun() -> mnesia:delete({call_queue, "test queue"}) end)
			end,
			{Dumptestq}
		end,
		fun(_Whatever) -> 
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			ok
		end,
		[
			{
				"New Queue with Weight",
				fun() ->
					?assertEqual({atomic, ok}, new_queue("test queue", 3, [], [], "Default")),
					F = fun() ->
						mnesia:read({call_queue, "test queue"})
					end,
					Q = #call_queue{
						name = "test queue",
						weight = 3,
						recipe = [],
						skills = []},
					?CONSOLE("trans:  ~p", [mnesia:transaction(F)]),
					?CONSOLE("rec:  ~p", [Q]),
					?assertEqual({atomic, [Q]}, mnesia:transaction(F))
				end
			},
			{
				"New Queue with Invalid Weight",
				fun() -> 
					?assertError(function_clause, new_queue("name", "not a number", [], [], "Default")),
					?assertError(function_clause, new_queue("name", -1, [], [], "Default")),
					?assertError(function_clause, new_queue("name", 0, [], [], "Default"))
				end
			},
			{
				"New Queue with Skills",
				fun() ->
					TestQueue = #call_queue{skills = [testskill], name = "test queue", weight = 1, recipe = []},
					?assertEqual({atomic, ok}, new_queue("test queue", 1, [testskill], [], "Default")),
					F = fun() ->
						mnesia:read({call_queue, "test queue"})
					end,
					?CONSOLE("trans:  ~p", [mnesia:transaction(F)]),
					?CONSOLE("test:  ~p", [TestQueue]),
					?assertEqual({atomic, [TestQueue]}, mnesia:transaction(F))
				end
			},
			{
				"New Queue with Recipe",
				fun() -> 
					Recipe = [{[{ticks, 2}], [{add_skills, [true]}], run_once, <<"recipe">>}],
					Queue = #call_queue{name="test queue", recipe=Recipe, skills = []},
					?assertEqual({atomic, ok}, new_queue("test queue", 1, [], Recipe, "Default")),
					F = fun() ->
						mnesia:read({call_queue, "test queue"})
					end,
					?CONSOLE("Queue:  ~p", [Queue]),
					?CONSOLE("Trans:  ~p", [mnesia:transaction(F)]),
					?assertEqual({atomic, [Queue]}, mnesia:transaction(F))
				end
			},
			{
				"New Queue with active Queue Manager",
				fun() ->
					queue_manager:start([node()]),
					_Queue = new_queue(#call_queue{name = "test queue"}),
					?assertMatch(true, queue_manager:query_queue("test queue")),
					queue_manager:stop()
				end
			},
			{
				"Set queue",
				fun() ->
					Queue = test_queue(),
					new_queue(Queue),
					Newqueue = #call_queue{
						name = "new name",
						skills = [],
						recipe = [],
						group = "New Group"
					},
					set_queue("test queue", Newqueue),
					?assertEqual(noexists, get_queue("test queue")),
					?assertEqual(Newqueue, get_queue("new name"))
				end

			},
			{
				"Destroy",
				fun() -> 
					Queue = test_queue(),
					new_queue(Queue),
					destroy_queue(Queue),
					Select = qlc:q([X || X <- mnesia:table(call_queue), X#call_queue.name =:= Queue#call_queue.name]),
					F = fun() -> 
						qlc:e(Select)
					end,
					?assertEqual({atomic, []}, mnesia:transaction(F))
				end
			},
			{
				"Get All",
				fun() -> 
					Queue = test_queue(),
					Queue2 = Queue#call_queue{name="test queue 2"},
					new_queue(Queue),
					new_queue(Queue2),
					?assertEqual([Queue, Queue2], get_queues()),
					destroy_queue(Queue),
					destroy_queue(Queue2)
				end
			},
			{
				"Get One Queue",
				fun() -> 
					Queue = test_queue(),
					Queue2 = Queue#call_queue{name="test queue 2"},
					new_queue(Queue),
					new_queue(Queue2),
					?assertEqual(Queue, get_queue(Queue#call_queue.name)),
					?assertEqual(Queue2, get_queue(Queue2#call_queue.name)),
					destroy_queue(Queue),
					destroy_queue(Queue2)
				end
			},
			{
				"Get All of None",
				fun() -> 
					Queue = test_queue(),
					destroy_queue(Queue),
					?assertMatch(noexists, get_queue(Queue#call_queue.name))
				end
			},
			{
				"Get queue with old style recipe",
				fun() ->
					BaseQueue = test_queue(),
					Queue = BaseQueue#call_queue{recipe = [{[{ticks, 3}], add_skills, [true], run_once}]},
					new_queue(Queue),
					?assertMatch(#call_queue{recipe = [{[{ticks, 3}], [{add_skills, [true]}], run_once, <<"No Comment">>}]}, get_queue("test queue")),
					F = fun() ->
						QH = qlc:q([ X#call_queue.recipe || X <- mnesia:table(call_queue), X#call_queue.name == "test queue"]),
						qlc:e(QH)
					end,
					{atomic, [Out | _]} = mnesia:transaction(F),
					?assertEqual([{[{ticks, 3}], [{add_skills, [true]}], run_once, <<"No Comment">>}], Out)
				end
			},
			{
				"Get a queue merged with it's group settings",
				fun() ->
					BaseQueue = test_queue(),
					Group = #queue_group{
						name = "Default",
						skills = [testskill, uberskill],
						recipe = [{[{ticks, 4}], [{add_skills, [superskill]}], run_once}]
					},
					mnesia:transaction(fun() -> mnesia:write(Group) end),
					?DEBUG("Did it get there?  ~p", [get_queue_group("Default")]),
					Queue = get_merged_queue("test queue"),
					?DEBUG("Queue:  ~p", [Queue]),
					?assertEqual([testskill, uberskill], Queue#call_queue.skills),
					?assertEqual([
						{[{ticks, 2}], [{add_skills, [true]}], run_once},
						{[{ticks, 4}], [{add_skills, [superskill]}], run_once}
					], Queue#call_queue.recipe)
				end
			}
		]
	}.

queue_group_test_() ->
	{
		foreach,
		fun() -> 
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			mnesia:create_schema([node()]),
			mnesia:start(),
			build_tables()
		end,
		fun(_Whatever) -> 
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			ok
		end,
		[
			{
				"New call group by record",
				fun() ->
					Qgroup = #queue_group{name = "test group"},
					new_queue_group(Qgroup),
					F = fun() ->
						mnesia:read({queue_group, "test group"})
					end,
					?assertEqual({atomic, [Qgroup]}, mnesia:transaction(F))
				end
			},
			{
				"New call group explicit",
				fun() ->
					Recipe = [{[{ticks, 3}], [{prioritize, []}], run_once, <<"recipe">>}],
					Qgroup = #queue_group{name = "test group", sort = 15, recipe = Recipe},
					new_queue_group("test group", 15, Recipe),
					F = fun() ->
						mnesia:read({queue_group, "test group"})
					end,
					?assertEqual({atomic, [Qgroup]}, mnesia:transaction(F))
				end
			},
			{
				"destroy unprotected call group",
				fun() ->
					Qgroup = #queue_group{name = "test group"},
					new_queue_group(Qgroup),
					F = fun() ->
						mnesia:read({queue_group, "test group"})
					end,
					?assertEqual({atomic, [Qgroup]}, mnesia:transaction(F)),
					?assertEqual({atomic, ok}, destroy_queue_group("test group")),
					?assertEqual({atomic, []}, mnesia:transaction(F))
				end
			},
			{
				"destroy protected call group",
				fun() ->
					?assertEqual({atomic, {error, protected}}, destroy_queue_group("Default")),
					F = fun() ->
						mnesia:read({queue_group, "Default"})
					end,
					{atomic, [Res]} = mnesia:transaction(F),
					?assertEqual("Default", Res#queue_group.name)
				end
			},
			{
				"get a call group",
				fun() ->
					Default = ?DEFAULT_QUEUE_GROUP,
					{atomic, [Res]} = get_queue_group("Default"),
					?assertEqual(Default#queue_group.name, Res#queue_group.name),
					?assertEqual(Default#queue_group.sort, Res#queue_group.sort),
					?assertEqual(Default#queue_group.recipe, Res#queue_group.recipe),
					?assertEqual(Default#queue_group.protected, Res#queue_group.protected)
				end
			},
			{
				"get all call groups (order matters)",
				fun() ->
					Sort10 = #queue_group{name = "Added 1st", sort = 10},
					Sort5 = #queue_group{name = "Added 2nd", sort = 5},
					Sort7 = #queue_group{name = "Added 3rd", sort = 7},
					new_queue_group(Sort10),
					new_queue_group(Sort5),
					new_queue_group(Sort7),
					Test = [#queue_group{name = "Default", sort = 0, protected = true}, Sort5, Sort7, Sort10],
					[G1, G2, G3, G4] = Gotten = get_queue_groups(),
					?assertEqual(length(Test), length(Gotten)),
					?assertMatch(#queue_group{name = "Default", sort = 0, protected = true}, G1),
					?assertMatch(#queue_group{name = "Added 2nd", sort = 5, protected = false}, G2),
					?assertMatch(#queue_group{name = "Added 3rd", sort = 7, protected = false}, G3),
					?assertMatch(#queue_group{name = "Added 1st", sort = 10, protected = false}, G4)
				end
			},
			{
				"update a protected call group by record",
				fun() ->
					Recipe = [{[{ticks, 3}], [{prioritize, []}], run_once, <<"recipe">>}],
					Updateto = #queue_group{name = "newname", sort = 5, protected = false, recipe = Recipe},
					Default = ?DEFAULT_QUEUE_GROUP,
					Test = Default#queue_group{name = "newname", sort = 5, recipe = Recipe},
					Setres = set_queue_group(Default#queue_group.name, Updateto),
					?CONSOLE("res:  ~p", [Setres]),
					?assertEqual({atomic, ok}, Setres),
					Got = get_queue_group("newname"),
					?CONSOLE("also res:  ~p", [Got]),
					?assertMatch({atomic, [#queue_group{name = "newname", sort = 5, recipe = Recipe}]}, Got)
				end
			},
			{
				"update a protected call group explicitly",
				fun() ->
					Recipe = [{[{ticks, 3}], [{prioritize, []}], run_once, <<"recipe">>}],
					Default = ?DEFAULT_QUEUE_GROUP,
					Test = Default#queue_group{name = "newname", sort = 5, recipe = Recipe},
					set_queue_group(Default#queue_group.name, "newname", 5, Recipe),
					Got = get_queue_group("newname"),
					?CONSOLE("Got:  ~p", [Got]),
					?assertMatch({atomic, [#queue_group{name = "newname", sort = 5, recipe = Recipe}]}, Got)
				end
			},
			{
				"update a group that doesn't exist",
				fun() ->
					?assertEqual({atomic, {error, {noexists, "testname"}}}, set_queue_group("testname", ?DEFAULT_QUEUE_GROUP))
				end
			},
			{
				"Get a queue group with old style recipe",
				fun() ->
					new_queue_group("name", 5, [{[{ticks, 3}], add_skills, [true], run_once}]),
					?assertMatch({atomic, [#queue_group{recipe = [{[{ticks, 3}], [{add_skills, [true]}], run_once, <<"No Comment">>}]}]}, get_queue_group("name")),
					F = fun() ->
						qlc:e(qlc:q([ X#queue_group.recipe || X <- mnesia:table(queue_group), X#queue_group.name == "name"]))
					end,
					{atomic, [R | _]} = mnesia:transaction(F),
					?assertEqual([{[{ticks, 3}], [{add_skills, [true]}], run_once, <<"No Comment">>}], R)
				end
			}
		]
	}.

skill_rec_test_() -> 
	{
		foreach,
		fun() -> 
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			mnesia:create_schema([node()]),
			mnesia:start(),
			build_tables(),
			Test = fun(_F, [], []) ->
					ok;
				(F, [H1 | T1], [H2 | T2]) ->
					?assertEqual(H1#skill_rec.name, H2#skill_rec.name),
					?assertEqual(H1#skill_rec.atom, H2#skill_rec.atom),
					?assertEqual(H1#skill_rec.description, H2#skill_rec.description),
					?assertEqual(H1#skill_rec.group, H2#skill_rec.group),
					F(F, T1, T2)
			end,
			Test
		end,
		fun(_Whatever) -> 
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			ok
		end,
		[
			{
				"Test for a known skill atom",
				fun() -> 
					Skillrec = skill_exists("_node"),
					?assertMatch('_node', Skillrec)
				end
			},
			{
				"Test for an unknown skill atom",
				fun() -> 
					Skillrec = skill_exists("Not a valid skill"),
					?assertMatch(undefined, Skillrec)
				end
			},
			fun(Test) -> 
				{
					"Set a skill by record",
					fun() ->
						Skillrec = #skill_rec{name = "testskill", atom = testskill, description = "test skill", group = "Misc", protected = false},
						new_skill(Skillrec),
						F = fun() ->
							QH = qlc:q([X || X <- mnesia:table(skill_rec), X#skill_rec.atom =:= testskill]),
							qlc:e(QH)
						end,
						{atomic, [Res]} = mnesia:transaction(F),
						Test(Test, [Skillrec], [Res])
					end
				}
			end,
			fun(Test) ->
				{
					"Set a skill explicit",
					fun() ->
						Skillrec = #skill_rec{name = "testskill", atom = testskill, description = "test skill", group = "Misc", protected = false},
						new_skill(testskill, "testskill", "test skill", "Misc"),
						F = fun() ->
							QH = qlc:q([X || X <- mnesia:table(skill_rec), X#skill_rec.atom =:= testskill]),
							qlc:e(QH)
						end,
						{atomic, [Res]} = mnesia:transaction(F),
						Test(Test, [Skillrec], [Res])
					end
				}
			end,
			fun(Testfun) ->
				{
					"get all skills",
					fun() ->
						Skills = lists:sort([
							#skill_rec{name="English", atom=english, description="English", group = "Language"},
							#skill_rec{name="German", atom=german, description="German", group = "Language"},
							#skill_rec{name="Queue", atom='_queue', description="Magic skill replaced by a queue's name", group = "Magic", protected = true},
							#skill_rec{name="Node", atom='_node', description="Magic skill that is replaced by the node identifier.", group = "Magic", protected = true},
							#skill_rec{name="Agent Name", atom='_agent', description="Magic skill that is replaced by the agent's name.", group = "Magic", protected = true},
							#skill_rec{name="Brand", atom='_brand', description="Magic skill to expand to a client's label (brand)", group="Magic", protected=true},
							#skill_rec{name="All", atom='_all', description="Magic skill to denote an agent that can answer any call regardless of other skills.", group = "Magic", protected = true},
							#skill_rec{name="Agent Profile", atom='_profile', description="Magic skill that is replaced by the agent's profile name", group = "Magic", protected = true}
						]),
						Gotten = lists:sort(get_skills()),
						lists:foreach(fun(X) -> ?debugFmt("~p", [X]) end, Gotten),
						?assertEqual(length(Skills), length(Gotten)),
						Testfun(Testfun, Skills, Gotten)
					end
				}
			end,
			fun(Testfun) ->
				{
					"get all skills in a group",
					fun() ->
						Skills = lists:sort([
							#skill_rec{name="English", atom=english, description="English", group = "Language"},
							#skill_rec{name="German", atom=german, description="German", group = "Language"}
						]),
						Gotten = lists:sort(get_skills("Language")),
						?assertEqual(length(Skills), length(Gotten)),
						Testfun(Testfun, Skills, Gotten)
					end
				}
			end,
			{
				"destroy a non-protected skill",
				fun() ->
					destroy_skill("English"),
					F = fun() ->
						mnesia:read({skill_rec, english})
					end,
					?assertEqual({atomic, []}, mnesia:transaction(F))
				end
			},
			{
				"destroy a protected skill",
				fun() ->
					destroy_skill("All"),
					F = fun() ->
						mnesia:read({skill_rec, '_all'})
					end,
					?assertMatch({atomic, [#skill_rec{}]}, mnesia:transaction(F))
				end
			},
			fun(Test) ->
				{
					"change group on skills",
					fun() ->
						Skills = [
							#skill_rec{name="English", atom=english, description="English", group = "Talky"},
							#skill_rec{name="German", atom=german, description="German", group = "Talky"}
						],
						Rgres = rename_skill_group("Language", "Talky"),
						Gottennew = get_skills("Talky"),
						Gottenold = get_skills("Language"),
						?assertEqual({atomic, ok}, Rgres),
						?assertEqual([], Gottenold),
						Test(Test, Skills, Gottennew)
					end
				}
			end,
			{
				"Change group, but group exists",
				fun() ->
					?assertEqual({error, {exists, "Magic"}}, rename_skill_group("Language", "Magic"))
				end
			},
			fun(Testfun) ->
				{
					"get a single skill",
					fun() ->
						Test = #skill_rec{name="English", atom=english, description="English", group = "Language"},
						Testfun(Testfun, [Test], [get_skill(english)])
					end
				}
			end,
			fun(Test) ->
				{
					"update a skill",
					fun() ->
						New = #skill_rec{name="Newname", atom=testskill, description="Newdesc", group = "Newgroup"},
						Result = #skill_rec{name="Newname", atom=english, description="Newdesc", group = "Newgroup"},
						?assertEqual({atomic, ok}, set_skill(english, New)),
						Test(Test, [Result], [get_skill(english)]),
						?assertEqual(undefined, get_skill(testskill))
					end
				}
			end
		]
	}.

merge_client_options_test_() ->
	Given = [{givenkey, "given"}, {sharedkey, "shared"}],
	Default = [{defaultkey, "default"}, {sharedkey, "default_shared"}],
	[{"neither has mergaeble key",
	fun() ->
		Expected = Given,
		?assertEqual(Expected, merge_client_options(Given, Default, [neither]))
	end},
	{"Given lacks a key",
	fun() ->
		Expected = [{defaultkey, "default"} | Given],
		?assertEqual(Expected, merge_client_options(Given, Default, [defaultkey]))
	end},
	{"Default doesn't overwrite existing values",
	fun() ->
		Expected = Given,
		?assertEqual(Expected, merge_client_options(Given, Default, [sharedkey]))
	end},
	{"Default lacks a key",
	fun() ->
		Expected = Given,
		?assertEqual(Expected, merge_client_options(Given, Default, [givenkey]))
	end}].

client_rec_test_() ->
	{foreach,
	fun() -> 
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		mnesia:create_schema([node()]),
		mnesia:start(),
		build_tables()
	end,
	fun(_Whatever) -> 
		mnesia:stop(),
		mnesia:delete_schema([node()]),
		ok
	end,
	[{"Create a client by record",
	fun() ->
		Client = #client{label = "testclient", id = "00230001"},
		new_client(Client),
		F = fun() ->
			Select = qlc:q([X || X <- mnesia:table(client), X#client.label =:= "testclient"]),
			qlc:e(Select)
		end,
		?assertMatch({atomic, [#client{label = "testclient", id = "00230001"}]}, mnesia:transaction(F))
	end},
	{"Create a client by explicit",
	fun() ->
		new_client("testclient", "00230001", []),
		F = fun() ->
			Select = qlc:q([X || X <- mnesia:table(client), X#client.label =:= "testclient"]),
			qlc:e(Select)
		end,
		?assertMatch({atomic, [#client{label = "testclient", id = "00230001"}]}, mnesia:transaction(F))
	end},
	{"Destroy a client",
	fun() ->
		new_client("testclient", "00230001", []),
		destroy_client("00230001"),
		F = fun() ->
			Select = qlc:q([X || X <- mnesia:table(client), X#client.label =:= "testclient"]),
			qlc:e(Select)
		end,
		?assertEqual({atomic, []}, mnesia:transaction(F))
	end},
	{"Update a client explicit",
	fun() ->
		new_client("oldname", "00230001", []),
		set_client("00230001", "newname", []),
		Findold = fun() ->
			Select = qlc:q([X || X <- mnesia:table(client), X#client.label =:= "oldname"]),
			qlc:e(Select)
		end,
		Findnew = fun() ->
			Select = qlc:q([X || X <- mnesia:table(client), X#client.label =:= "newname"]),
			qlc:e(Select)
		end,
		?assertEqual({atomic, []}, mnesia:transaction(Findold)),
		?assertMatch({atomic, [#client{label = "newname", id = "00230001"}]}, mnesia:transaction(Findnew))
	end},
	{"Update a client by record",
	fun() ->
		Client1 = #client{label = "Client1", id = "00230001"},
		Client2 = #client{label = "Client2", id = "00230001"},
		new_client(Client1),
		set_client("00230001", Client2),
		Findold = fun() ->
			Select = qlc:q([X || X <- mnesia:table(client), X#client.label =:= "Client1"]),
			qlc:e(Select)
		end,
		Findnew = fun() ->
			Select = qlc:q([X || X <- mnesia:table(client), X#client.label =:= "Client2"]),
			qlc:e(Select)
		end,
		?assertEqual({atomic, []}, mnesia:transaction(Findold)),
		?assertMatch({atomic, [#client{label = "Client2"}]}, mnesia:transaction(Findnew))
	end},
	{"Get a client list",
	fun() ->
		Client1 = #client{label = "Client1", id = "00230001"},
		Client2 = #client{label = "Client2", id = "00470002"},
		Client3 = #client{label = "Aclient", id = "00560001"},
		DemoClient = #client{label="Demo Client", id = "00990099"},
		new_client(Client1),
		new_client(Client2),
		new_client(Client3),
		Expected = lists:sort([
			#client{label = undefined}, 
			#client{label = "Aclient", id = "00560001"}, 
			#client{label = "Client1", id = "00230001"}, 
			#client{label = "Client2", id = "00470002"}, 
			#client{label = "Demo Client", id = "00990099"}]),
		Got = lists:sort(get_clients()),
		?assertEqual(Expected, Got)
	end},
	{"Get a single client without integration",
	fun() ->
		Client1 = #client{label = "Client1", id = "00230001"},
		Client2 = #client{label = "Client2", id = "00470002"},
		Client3 = #client{label = "Aclient", id = "00560001"},
		new_client(Client1),
		new_client(Client2),
		new_client(Client3),
		Res = get_client("Client2"),
		?assertEqual(Client2#client.label, Res#client.label)
	end},
	{"Get a client that's not there without integration",
	fun() ->
		?assertEqual(none, get_client("Noexists"))
	end},
	{"Get a client when the default has options",
	fun() ->
		set_client(undefined, undefined, [{autoend_wrapup, 10}]),
		set_client("test1", "test1", []),
		Find = fun() ->
			qlc:e(qlc:q([X || X <- mnesia:table(client), X#client.label =:= "test1"]))
		end,
		{atomic, [#client{options = Rawopts}]} = mnesia:transaction(Find),
		#client{options = Dasopts} = get_client("test1"),
		?assertEqual([{autoend_wrapup, 10}], Dasopts),
		?assertEqual([], Rawopts)
	end},
	{"integration mocking",
		{foreach,
		fun() ->
			{ok, Mock} = gen_server_mock:named({local, integration}),
			Mock
		end,
		fun(Mock) ->
			unregister(integration),
			gen_server_mock:stop(Mock)
		end,
		[fun(Mock) ->
			{"client from integration",
			fun() ->
				gen_server_mock:expect_call(Mock, fun({get_client, label, "client"}, _, State) ->
					{ok, {ok, "0020004", "client", []}, State}
				end),
				?assertMatch(#client{label = "client", id = "0020004", options = []}, get_client(label, "client")),
				?assertMatch(#client{label = "client", id = "0020004", options = []}, local_get_client(label, "client"))
			end}
		end,
		fun(Mock) ->
			{"client not found in integration",
			fun() ->
				gen_server_mock:expect_call(Mock, fun({get_client, label, "client"}, _, State) ->
					{ok, none, State}
				end),
				new_client(#client{label = "client", id = "0020004"}),
				?assertMatch(#client{label = "client", id = "0020004"}, local_get_client(label, "client")),
				?assertEqual(none, get_client(label, "client")),
				?assertEqual(none, local_get_client(label, "client"))
			end}
		end,
		fun(Mock) ->
			{"integration fail",
			fun() ->
				gen_server_mock:expect_call(Mock, fun({get_client, label, "client"}, _, State) ->
					{ok, gooberpants, State}
				end),
				new_client(#client{label = "client", id = "0020004"}),
				?assertMatch(#client{label = "client", id = "0020004"}, get_client("client"))
			end}
		end]}
	}]}.

timestamp_test_() ->
	{
		foreach,
		fun() -> 
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			mnesia:create_schema([node()]),
			mnesia:start(),
			build_tables()
		end,
		fun(_Whatever) -> 
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			ok
		end,
		[%"Adding" Tests defunct because just creating the recored creates a valid timestamp; 
		%there is no need to initialize the timestamp field.
%		{"Adding a queue updates timestamp",
%		fun() ->
%			Inrec = #call_queue{name = "test", timestamp = 1},
%			new_queue(Inrec),
%			F = fun() ->
%				QH = qlc:q([X || X <- mnesia:table(call_queue), X#call_queue.name =:= "test"]),
%				qlc:e(QH)
%			end,
%			{atomic, [Rec]} = mnesia:transaction(F),
%			?assertNot(1 =:= Rec#call_queue.timestamp)
%		end},
		{"Updating a queue updates the timestamp",
		fun() ->
			Inrec = #call_queue{name = "test", timestamp = 1},
			new_queue(Inrec),
			F = fun() ->
				QH = qlc:q([X || X <- mnesia:table(call_queue), X#call_queue.name =:= "test"]),
				qlc:e(QH)
			end,
			{atomic, [#call_queue{timestamp = Oldtime}]} = mnesia:transaction(F),
			timer:sleep(2000),
			set_queue("test", #call_queue{name = "test", timestamp = 1}),
			{atomic, [#call_queue{timestamp = Newtime}]} = mnesia:transaction(F),
			?assert(Oldtime < Newtime)
		end},
%		{"Adding a queue group updates the timestamp",
%		fun() ->
%			new_queue_group(#queue_group{name = "test", timestamp = 1}),
%			F = fun() ->
%				QH = qlc:q([X || X <- mnesia:table(queue_group), X#queue_group.name =:= "test"]),
%				qlc:e(QH)
%			end,
%			{atomic, [#queue_group{timestamp = T}]} = mnesia:transaction(F),
%			?assert(1 < T)
%		end},
		{"Updating a queue group updates the timestamp",
		fun() ->
			new_queue_group(#queue_group{name = "test", timestamp = 1}),
			F = fun() ->
				QH = qlc:q([X || X <- mnesia:table(queue_group), X#queue_group.name =:= "test"]),
				qlc:e(QH)
			end,
			{atomic, [#queue_group{timestamp = Oldt}]} = mnesia:transaction(F),
			timer:sleep(2000),	
			set_queue_group("test", #queue_group{name = "test", timestamp = 1}),
			{atomic, [#queue_group{timestamp = Newt}]} = mnesia:transaction(F),
			?assert(Oldt < Newt)
		end},
%		{"Adding a skill updates the timestamp",
%		fun() ->
%			new_skill(#skill_rec{atom = testskill, name = "test", timestamp = 1}),
%			F = fun() ->
%				QH = qlc:q([X || X <- mnesia:table(skill_rec), X#skill_rec.name =:= "test"]),
%				qlc:e(QH)
%			end,
%			{atomic, [#skill_rec{timestamp = T}]} = mnesia:transaction(F),
%			?assert(1 < T)
%		end},
		{"Updating a skill updates the timestamp",
		fun() ->
			new_skill(#skill_rec{atom = testskill, name = "test", timestamp = 1}),
			F = fun() ->
				QH = qlc:q([X || X <- mnesia:table(skill_rec), X#skill_rec.name =:= "test"]),
				qlc:e(QH)
			end,
			{atomic, [#skill_rec{timestamp = Oldt}]} = mnesia:transaction(F),
			timer:sleep(2000),
			set_skill(testskill, #skill_rec{atom = testskill, name = "test", timestamp = 1}),
			{atomic, [#skill_rec{timestamp = Newt}]} = mnesia:transaction(F),
			?assert(Oldt < Newt)
		end},
%		{"Adding a client",
%		fun() ->
%			new_client(#client{label = "test", timestamp = 1}),
%			F = fun() ->
%				QH = qlc:q([X || X <- mnesia:table(client), X#client.label =:= "test"]),
%				qlc:e(QH)
%			end,
%			{atomic, [#client{timestamp = T}]} = mnesia:transaction(F),
%			?assert(1 < T)
%		end},
		{"Updating a client",
		fun() ->
			new_client(#client{label = "test", id = "test", timestamp = 1}),
			F = fun() ->
				QH = qlc:q([X || X <- mnesia:table(client), X#client.label =:= "test"]),
				qlc:e(QH)
			end,
			{atomic, [#client{timestamp = Oldt}]} = mnesia:transaction(F),
			timer:sleep(2000),
			set_client("test", #client{label = "test", timestamp = 1}),
			{atomic, [#client{timestamp = Newt}]} = mnesia:transaction(F),
			?assert(Oldt < Newt)
		end}]
	}.

merge_util_test_() ->
	[?_assertEqual(true, nom_equal(#call_queue{name = "a", timestamp = 1}, #call_queue{name = "a", timestamp = 2})),
	?_assertEqual(false, nom_equal(#call_queue{name = "a", timestamp = 1}, #call_queue{name = "b", timestamp = 2})),
	?_assertEqual(true, nom_comp(#call_queue{name = "a", timestamp = 1}, #call_queue{name = "b", timestamp = 2})),
	?_assertEqual(false, nom_comp(#call_queue{name = "b", timestamp = 1}, #call_queue{name = "a", timestamp = 2})),
	?_assertEqual(true, timestamp_comp(#call_queue{name = "a", timestamp = 1}, #call_queue{name = "a", timestamp = 2})),
	?_assertEqual(false, timestamp_comp(#call_queue{name = "a", timestamp = 2}, #call_queue{name = "a", timestamp = 1}))].
	
-endif.
