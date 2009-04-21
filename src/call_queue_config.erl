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

%% @doc The helper module to config the call_queues.
%% Uses the mnesia table 'call_queue.'  Queues are not started until a call requires it.
-module(call_queue_config).
-author("Micah").

-ifdef(EUNIT).
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
	
-include("queue.hrl").
-include("call.hrl").
-include_lib("stdlib/include/qlc.hrl").

% TODO roll these into call_queue?
%% API

%%
-export([
	build_tables/0,
	build_tables/1
]).
-export([
	new_queue/1, 
	new_queue/5,
	destroy_queue/1,
	get_queue/1,
	get_queues/0,
	get_queues/1,
	set_queue/1,
	set_queue/2,
	set_queue_name/2,
	set_queue_recipe/2,
	set_queue_skills/2,
	set_queue_weight/2
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
	set_client/2,
	set_client/4,
	get_client/1,
	get_clients/0]).

%% =====
%% All configs
%% =====
%% @doc Attempts to set-up and create the required mnesia table `call_queue' on all visible nodes.
%% Errors caused by the table already existing are ignored.
%% @see build_tables/1
build_tables() -> 
	build_tables(lists:append(nodes(), [node()])).

%% @doc Attempts to set-up and create the required mnesia table `call_queue' on the specified nodes
%% Errors caused by the table already existing are ignored.
build_tables(Nodes) -> 
	?CONSOLE("~p building tables...", [?MODULE]),
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
				mnesia:write(#client{label = "Demo Client", tenant = 99, brand = 99})
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
			Rec;
		Else -> 
			{noexists, Else}
	end.

%% @doc Get all the queues that are members of the specified Group (`string()').
-spec(get_queues/1 :: (Group :: string()) -> [#call_queue{}]).
get_queues(Group) ->
	QH = qlc:q([X || X <- mnesia:table(call_queue), X#call_queue.group =:= Group]),
	F = fun() ->
		qlc:e(QH)
	end,
	{atomic, Queues} = mnesia:transaction(F),
	lists:sort(fun(Qreca, Qrecb) -> Qreca#call_queue.name =< Qrecb#call_queue.name end, Queues).
	
%% @doc Get all the queue configurations (`[#call_queue{}]').
-spec(get_queues/0 :: () -> [#call_queue{}]).
get_queues() -> 
	QH = qlc:q([{X, G#queue_group.recipe} || 
		X <- mnesia:table(call_queue), 
		G <- mnesia:table(queue_group), 
		G#queue_group.name =:= X#call_queue.group
	]),
	F = fun() -> 
		qlc:e(QH)
	end,
	{atomic, Reply} = mnesia:transaction(F),
	Mergereci = fun({#call_queue{recipe = Qreci} = Queue, Groupreci}) ->
		Newreci = lists:append([Qreci, Groupreci]),
		Queue#call_queue{recipe = Newreci}
	end,
	lists:map(Mergereci, Reply).

%% @doc Create a new default queue configuraiton with the name `string()' `QueueName'.
%% @see new_queue/2
-spec(new_queue/1 :: (QueueName :: #call_queue{} ) -> #call_queue{}).
new_queue(Queue) when is_record(Queue, call_queue) ->
	
	F = fun() ->
		mnesia:write(Queue)
	end,
	Trans = mnesia:transaction(F),
	case whereis(queue_manager) of
		undefined ->
			Trans;
		Qmpid when is_pid(Qmpid) ->
			queue_manager:add_queue(Queue#call_queue.name, Queue#call_queue.recipe, Queue#call_queue.weight),
			Trans
	end.

new_queue(Name, Weight, Skills, Recipe, Group) when Weight > 0, is_integer(Weight) ->
	Rec = #call_queue{
		name = Name,
		weight = Weight,
		skills = Skills,
		recipe = Recipe,
		group = Group},
	new_queue(Rec).
		
%% @doc Using with a single `{Key, Value}', or `[{Key, Value}]', create a new queue called `string()' `QueueName' and add it to the database.
%% Valid keys/value combos are:
%% <dl>
%% <dt>weight</dt><dd>An integer.</dd>
%% <dt>skills</dt><dd>A list of atoms for the skills a call will be initially assigned.</dd>
%% <dt>recipe</dt><dd>A recipe config for this queue for use by {@link cook. cooks}.</dd>
%% </dl>
%-spec(new_queue/2 :: (QueueName :: string(), {'weight' | 'skills' | 'recipe' | 'queue_group', any()}) -> #call_queue{};
%					(QueueName :: string(), [{'weight' | 'skills' | 'recipe' | 'queue_group', any()}]) -> #call_queue{}).
%new_queue(QueueName, {Key, Value}) when is_atom(Key) -> 
%	new_queue(QueueName, [{Key, Value}]);
%new_queue(QueueName, Options) when is_list(Options) -> 
%	Q = #call_queue{name=QueueName},
%	Fullqrec = set_options(Q, Options),
%	F = fun() ->
%		mnesia:write(Fullqrec)
%	end,
%	{atomic, ok} = mnesia:transaction(F),
%	case whereis(queue_manager) of
%		undefined ->
%			Fullqrec;
%		QMPid when is_pid(QMPid) ->
%			queue_manager:add_queue(Fullqrec#call_queue.name, Fullqrec#call_queue.recipe, Fullqrec#call_queue.weight),
%			Fullqrec
%	end.
%% @doc Set all the params for a config based on the `#call_queue{}' `Queue'.  Returns the results of the mnesia transaction.
-spec(set_queue/1 :: (Queue :: #call_queue{}) -> {'aborted', any()} | {'atomic', any()}).
set_queue(Queue) when is_record(Queue, call_queue) -> 
	F = fun() -> 
		mnesia:write(Queue)
	end,
	mnesia:transaction(F).

%% @doc Sets the queue name `Queue' to the passed `#call_queue{}'.
-spec(set_queue/2 :: (Queue :: string(), Rec :: #call_queue{}) -> {'atomic', 'ok'} | {'aborted', any()}).
set_queue(Queue, Rec) ->
	F = fun() ->
		case mnesia:read({call_queue, Queue}) of
			[_OldRec] ->
				mnesia:delete({call_queue, Queue}),
				mnesia:write(Rec)
		end
	end,
	mnesia:transaction(F).

%% @doc Rename a queue from `string()' `OldName' to `string()' `NewName'.  Returns the results of the mnesia transaction.
-spec(set_queue_name/2 :: (OldName :: string(), NewName :: string()) -> any()).
set_queue_name(OldName, NewName) ->
	F = fun() -> 
		[OldRec] = mnesia:read({call_queue, OldName}),
		NewRec = OldRec#call_queue{name=NewName},
		mnesia:write(NewRec),
		mnesia:delete({call_queue, OldName})
	end,
	mnesia:transaction(F).

% TODO notify queue?  Kill the functions?
%% @doc Update `string()' `Queue' with a new `recipe()' `Recipe'.
-spec(set_queue_recipe/2 :: (Queue :: string(), Recipe :: recipe()) -> any()).
set_queue_recipe(Queue, Recipe) -> 
	F = fun() -> 
		[OldRec] = mnesia:read({call_queue, Queue}),
		NewRec = OldRec#call_queue{recipe=Recipe},
		mnesia:write(NewRec)
	end,
	mnesia:transaction(F).
	
%% @doc Update the `string()' `Queue' replacing the skills with `[atom()]' `Skills'.
%% Returns the result of the mnesia transaction.
-spec(set_queue_skills/2 :: (Queue :: string(), Skills :: [atom()]) -> any()).
set_queue_skills(Queue, Skills) -> 
	F = fun() -> 
		[OldRec] = mnesia:read({call_queue, Queue}),
		NewRec = OldRec#call_queue{skills=Skills},
		mnesia:write(NewRec)
	end,
	mnesia:transaction(F).
	
%% @doc Update `string()' `Queue' with a new wight of `pos_integer()' `Weight'.
%% Returns the result of the mnesia transaction.
-spec(set_queue_weight/2 :: (Queue :: string(), Weight :: pos_integer()) -> any()).
set_queue_weight(Queue, Weight) when is_integer(Weight) andalso Weight >= 1-> 
	F = fun() ->
		[OldRec] = mnesia:read({call_queue, Queue}),
		NewRec = OldRec#call_queue{weight = Weight},
		mnesia:write(NewRec)
	end,
	mnesia:transaction(F).
	
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
		QH = qlc:q([X || X <- mnesia:table(queue_group), X#queue_group.name =:= Name]),
		qlc:e(QH)
	end,
	mnesia:transaction(F).

%% @doc Gets all `#queue_group{}' in a list sorted by group.
-spec(get_queue_groups/0 :: () -> [#queue_group{}]).
get_queue_groups() ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(queue_group)]),
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
				mnesia:write(Newrec),
				QH = qlc:q([Queue || Queue <- mnesia:table(call_queue), Queue#call_queue.group =:= Oldname]),
				Queues = qlc:e(QH),
				Regroupqs = fun(Queue) ->
					Newqrec = Queue#call_queue{group = Rec#queue_group.name},
					mnesia:delete({call_queue, Queue#call_queue.name}),
					mnesia:write(Newqrec)
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
-spec(destroy_queue_group/1 :: (Groupname :: string()) -> {'atomic', 'ok'}).
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
				Forwrite = Newrec#skill_rec{atom = Oldskill},
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

%% @doc Removes the skill named `string()' `Skillname' from the database.
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
					mnesia:write(Newskill)
				end,
				lists:map(Update, Skills),
				ok
			end,
			mnesia:transaction(Doupdate);
		{atomic, List} when length(List) >= 1 ->
			?CONSOLE("error, target name exists", []),
			{error, {exists, Newgroup}}
	end.
				
%% =====
%% Client configs
%% =====

%% @doc Add a new client with `string()' `Label', `integer()' `Tenantid', and `integer()' `Brandid'.
%% @see new_client/1
-spec(new_client/3 :: (Label :: string(), Tenantid :: pos_integer(), Brandid :: pos_integer()) -> {'atomic', 'ok'}).
new_client(Label, Tenantid, Brandid) when is_integer(Tenantid), is_integer(Brandid) ->
	Rec = #client{label = Label, tenant = Tenantid, brand = Brandid},
	new_client(Rec).

-spec(new_client/1 :: (Rec :: #client{}) -> {'atomic', 'ok'}).
%% @doc Add a new client based on `#client{}' `Rec'.
new_client(Rec) when is_record(Rec, client) ->
	F = fun() ->
		mnesia:write(Rec)
	end,
	mnesia:transaction(F).

%% @doc Update the client `string()' `Label' to `string()' `Newlabel', `integer()' `Tenantid', `integer()' `Brandid'.
%% @see set_client/2
-spec(set_client/4 :: (Label :: string(), Newlabel :: string(), Tenantid :: pos_integer(), Brandid :: pos_integer()) -> {'atomic', 'ok'}).
set_client(Label, Newlabel, Tenantid, Brandid) when is_integer(Tenantid), is_integer(Brandid) ->
	Client = #client{label = Newlabel, tenant = Tenantid, brand = Brandid},
	set_client(Label, Client).

%% @doc Update the client `string()' `Label' to the `#client{}' `Client'.
-spec(set_client/2 :: (Label :: string(), Client :: #client{}) -> {'atomic', 'ok'}).
set_client(Label, Client) when is_record(Client, client) ->
	F = fun() ->
		mnesia:delete({client, Label}),
		mnesia:write(Client)
	end,
	mnesia:transaction(F).

%% @doc Removed the client labeled `Label' from the client database.
-spec(destroy_client/1 :: (Label :: string()) -> {'atomic', 'ok'}).
destroy_client(Label) ->
	F = fun() -> 
		mnesia:delete({client, Label})
	end,
	mnesia:transaction(F).

%% @doc Get the `#client{}' associated with the label `Label'.
-spec(get_client/1 :: (Label :: string()) -> #client{} | 'none').
get_client(Label) ->
	F = fun() ->
		QH = qlc:q([X || X <- mnesia:table(client), X#client.label =:= Label]),
		qlc:e(QH)
	end,
	case mnesia:transaction(F) of
		{atomic, []} ->
			none;
		{atomic, [Client]} when is_record(Client, client) ->
			Client
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

%% =====
%% Internal / helper functions
%% =====

%% @private
% helper function to create a call_queue record from a list of {Key, Value}.
set_options(QueueRec, []) -> 
	QueueRec;
set_options(QueueRec, [{Key, Value} | Tail]) -> 
	case Key of
		weight when is_integer(Value) -> 
			set_options(QueueRec#call_queue{weight=Value}, Tail);
		skills -> 
			Skills = lists:append([QueueRec#call_queue.skills, Value]),
			% TODO it would be nice if we could check if those skills exist.
			set_options(QueueRec#call_queue{skills=Skills}, Tail);
		recipe -> 
			set_options(QueueRec#call_queue{recipe = Value}, Tail);
		queue_group ->
			set_options(QueueRec#call_queue{group = Value}, Tail)
	end.

%% =====
%% Tests
%% =====

-ifdef(EUNIT).

test_queue() -> 
	Recipe = [{2, add_skills, [true], run_once}],
	new_queue("test queue", 3, [testskill], Recipe, "Default"),
	Default = #call_queue{name = "goober"},
	#call_queue{name = "test queue", weight = 3, skills = [testskill], recipe = Recipe, group = "Default"}.
	
call_queue_test_() ->
	["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
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
					Recipe = [{[{ticks, 2}], add_skills, [true], run_once}],
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
				"Set Name", 
				fun() ->
					Queue = test_queue(),
					set_queue(Queue),
					TestQueue = Queue#call_queue{name="new name"},
					set_queue_name(Queue#call_queue.name, TestQueue#call_queue.name),
					Selectnew = qlc:q([X || X <- mnesia:table(call_queue), X#call_queue.name =:= "new name"]),
					SelectnewF = fun() -> 
						qlc:e(Selectnew)
					end,
					Selectold = qlc:q([X || X <- mnesia:table(call_queue), X#call_queue.name =:= "test queue"]),
					SelectoldF = fun() -> 
						qlc:e(Selectold)
					end,
					?assertEqual({atomic, [TestQueue]}, mnesia:transaction(SelectnewF)),
					?assertEqual({atomic, []}, mnesia:transaction(SelectoldF)),
					destroy_queue(Queue),
					destroy_queue(TestQueue)
				end
			},
			{
				"Set Recipe",
				fun() -> 
					Queue = test_queue(),
					set_queue(Queue),
					Recipe = [{[{ticks, 3}], announce, "announcement", run_once}],
					TestQueue = Queue#call_queue{recipe = Recipe},
					set_queue_recipe(Queue#call_queue.name, Recipe),
					Select = qlc:q([X || X <- mnesia:table(call_queue), X#call_queue.name =:= Queue#call_queue.name]),
					F = fun() -> 
						qlc:e(Select)
					end,
					?assertEqual({atomic, [TestQueue]}, mnesia:transaction(F)),
					destroy_queue(Queue)
				end
			},
			{
				"Set Skills",
				fun() -> 
					Queue = test_queue(), 
					set_queue(Queue),
					TestQueue = Queue#call_queue{skills = [german]},
					set_queue_skills(Queue#call_queue.name, [german]),
					Select = qlc:q([X || X <- mnesia:table(call_queue), X#call_queue.name =:= Queue#call_queue.name]),
					F = fun() ->
						qlc:e(Select)
					end,
					?assertEqual({atomic, [TestQueue]}, mnesia:transaction(F)),
					destroy_queue(Queue)
				end
			},
			{
				"Set Weight",
				fun() -> 
					Queue = test_queue(),
					set_queue(Queue),
					TestQueue = Queue#call_queue{weight = 7},
					set_queue_weight(Queue#call_queue.name, 7),
					Select = qlc:q([X || X <- mnesia:table(call_queue), X#call_queue.name =:= Queue#call_queue.name]),
					F = fun() -> 
						qlc:e(Select)
					end,
					?assertEqual({atomic, [TestQueue]}, mnesia:transaction(F)),
					destroy_queue(Queue)
				end
			},
			{
				"Destroy",
				fun() -> 
					Queue = test_queue(),
					set_queue(Queue),
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
					set_queue(Queue),
					set_queue(Queue2),
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
					set_queue(Queue),
					set_queue(Queue2),
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
			}
		]
	}.

queue_group_test_() ->
	["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
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
					Recipe = [{[{ticks, 3}], prioritize, [], run_once}],
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
					?assertEqual({atomic, [?DEFAULT_QUEUE_GROUP]}, mnesia:transaction(F))
				end
			},
			{
				"get a call group",
				fun() ->
					?assertEqual({atomic, [?DEFAULT_QUEUE_GROUP]}, get_queue_group("Default"))
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
					Gotten = get_queue_groups(),
					?assertEqual(Test, Gotten)
				end
			},
			{
				"update a protected call group by record",
				fun() ->
					Recipe = [{[{ticks, 3}], prioritize, [], run_once}],
					Updateto = #queue_group{name = "newname", sort = 5, protected = false, recipe = Recipe},
					Default = ?DEFAULT_QUEUE_GROUP,
					Test = Default#queue_group{name = "newname", sort = 5, recipe = Recipe},
					Setres = set_queue_group(Default#queue_group.name, Updateto),
					?CONSOLE("res:  ~p", [Setres]),
					?assertEqual({atomic, ok}, Setres),
					?assertEqual({atomic, [Test]}, get_queue_group("newname"))
				end
			},
			{
				"update a protected call group explicitly",
				fun() ->
					Recipe = [{[{ticks, 3}], prioritize, [], run_once}],
					Default = ?DEFAULT_QUEUE_GROUP,
					Test = Default#queue_group{name = "newname", sort = 5, recipe = Recipe},
					set_queue_group(Default#queue_group.name, "newname", 5, Recipe),
					?assertEqual({atomic, [Test]}, get_queue_group("newname"))
				end
			},
			{
				"update a group that doesn't exist",
				fun() ->
					?assertEqual({atomic, {error, {noexists, "testname"}}}, set_queue_group("testname", ?DEFAULT_QUEUE_GROUP))
				end
			}
		]
	}.

skill_rec_test_() -> 
	["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
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
			{
				"Set a skill by record",
				fun() ->
					Skillrec = #skill_rec{name = "testskill", atom = testskill, description = "test skill", group = "Misc", protected = false},
					new_skill(Skillrec),
					F = fun() ->
						QH = qlc:q([X || X <- mnesia:table(skill_rec), X#skill_rec.atom =:= testskill]),
						qlc:e(QH)
					end,
					?assertEqual({atomic, [Skillrec]}, mnesia:transaction(F))
				end
			},
			{
				"Set a skill explicit",
				fun() ->
					Skillrec = #skill_rec{name = "testskill", atom = testskill, description = "test skill", group = "Misc", protected = false},
					new_skill(testskill, "testskill", "test skill", "Misc"),
					F = fun() ->
						QH = qlc:q([X || X <- mnesia:table(skill_rec), X#skill_rec.atom =:= testskill]),
						qlc:e(QH)
					end,
					?assertEqual({atomic, [Skillrec]}, mnesia:transaction(F))
				end
			},
			{
				"get all skills",
				fun() ->
					Skills = [
						#skill_rec{name="English", atom=english, description="English", group = "Language"},
						#skill_rec{name="German", atom=german, description="German", group = "Language"},
						#skill_rec{name="Queue", atom='_queue', description="Magic skill replaced by a queue's name", group = "Magic", protected = true},
						#skill_rec{name="Node", atom='_node', description="Magic skill that is replaced by the node identifier.", group = "Magic", protected = true},
						#skill_rec{name="Agent Name", atom='_agent', description="Magic skill that is replaced by the agent's name.", group = "Magic", protected = true},
						#skill_rec{name="Brand", atom='_brand', description="Magic skill to expand to a client's label (brand)", group="Magic", protected=true},
						#skill_rec{name="All", atom='_all', description="Magic skill to denote an agent that can answer any call regardless of other skills.", group = "Magic", protected = true}
					],
					Gotten = get_skills(),
					lists:foreach(fun(X) -> ?debugFmt("~p", [X]) end, Gotten),
					?assertEqual(Skills, Gotten)
				end
			},
			{
				"get all skills in a group",
				fun() ->
					Skills = [
						#skill_rec{name="English", atom=english, description="English", group = "Language"},
						#skill_rec{name="German", atom=german, description="German", group = "Language"}
					],
					Gotten = get_skills("Language"),
					?assertEqual(Skills, Gotten)
				end
			},
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
					?assertEqual(Skills, Gottennew)
				end
			},
			{
				"Change group, but group exists",
				fun() ->
					?assertEqual({error, {exists, "Magic"}}, rename_skill_group("Language", "Magic"))
				end
			},
			{
				"get a single skill",
				fun() ->
					Test = #skill_rec{name="English", atom=english, description="English", group = "Language"},
					?assertEqual(Test, get_skill(english))
				end
			},
			{
				"update a skill",
				fun() ->
					New = #skill_rec{name="Newname", atom=testskill, description="Newdesc", group = "Newgroup"},
					Result = #skill_rec{name="Newname", atom=english, description="Newdesc", group = "Newgroup"},
					?assertEqual({atomic, ok}, set_skill(english, New)),
					?assertEqual(Result, get_skill(english)),
					?assertEqual(undefined, get_skill(testskill))
				end
			}
		]
	}.

client_rec_test_() ->
	["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
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
				"Create a client by record",
				fun() ->
					Client = #client{label = "testclient", tenant = 23, brand = 1},
					new_client(Client),
					F = fun() ->
						Select = qlc:q([X || X <- mnesia:table(client), X#client.label =:= "testclient"]),
						qlc:e(Select)
					end,
					?assertEqual({atomic, [Client]}, mnesia:transaction(F))
				end
			},
			{
				"Create a client by explicit",
				fun() ->
					new_client("testclient", 23, 1),
					F = fun() ->
						Select = qlc:q([X || X <- mnesia:table(client), X#client.label =:= "testclient"]),
						qlc:e(Select)
					end,
					?assertEqual({atomic, [#client{label = "testclient", tenant = 23, brand = 1}]}, mnesia:transaction(F))
				end
			},
			{
				"Destroy a client",
				fun() ->
					new_client("testclient", 23, 1),
					destroy_client("testclient"),
					F = fun() ->
						Select = qlc:q([X || X <- mnesia:table(client), X#client.label =:= "testclient"]),
						qlc:e(Select)
					end,
					?assertEqual({atomic, []}, mnesia:transaction(F))
				end
			},
			{
				"Update a client explicit",
				fun() ->
					new_client("oldname", 23, 1),
					set_client("oldname", "newname", 47, 2),
					Findold = fun() ->
						Select = qlc:q([X || X <- mnesia:table(client), X#client.label =:= "oldname"]),
						qlc:e(Select)
					end,
					Findnew = fun() ->
						Select = qlc:q([X || X <- mnesia:table(client), X#client.label =:= "newname"]),
						qlc:e(Select)
					end,
					?assertEqual({atomic, []}, mnesia:transaction(Findold)),
					?assertEqual({atomic, [#client{label = "newname", tenant = 47, brand = 2}]}, mnesia:transaction(Findnew))
				end
			},
			{
				"Update a client by record",
				fun() ->
					Client1 = #client{label = "Client1", tenant = 23, brand = 1},
					Client2 = #client{label = "Client2", tenant = 47, brand = 2},
					new_client(Client1),
					set_client("Client1", Client2),
					Findold = fun() ->
						Select = qlc:q([X || X <- mnesia:table(client), X#client.label =:= "Client1"]),
						qlc:e(Select)
					end,
					Findnew = fun() ->
						Select = qlc:q([X || X <- mnesia:table(client), X#client.label =:= "Client2"]),
						qlc:e(Select)
					end,
					?assertEqual({atomic, []}, mnesia:transaction(Findold)),
					?assertEqual({atomic, [Client2]}, mnesia:transaction(Findnew))
				end
			},
			{
				"Get a client list",
				fun() ->
					Client1 = #client{label = "Client1", tenant = 23, brand = 1},
					Client2 = #client{label = "Client2", tenant = 47, brand = 2},
					Client3 = #client{label = "Aclient", tenant = 56, brand = 1},
					DemoClient = #client{label="Demo Client", tenant=99, brand=99},
					new_client(Client1),
					new_client(Client2),
					new_client(Client3),
					?assertEqual([Client3, Client1, Client2, DemoClient], get_clients())
				end
			},
			{
				"Get a single client",
				fun() ->
					Client1 = #client{label = "Client1", tenant = 23, brand = 1},
					Client2 = #client{label = "Client2", tenant = 47, brand = 2},
					Client3 = #client{label = "Aclient", tenant = 56, brand = 1},
					new_client(Client1),
					new_client(Client2),
					new_client(Client3),
					?assertEqual(Client2, get_client("Client2"))
				end
			},
			{
				"Get a client that's not there",
				fun() ->
					?assertEqual(none, get_client("Noexists"))
				end
			}
		]
	}.


-endif.
