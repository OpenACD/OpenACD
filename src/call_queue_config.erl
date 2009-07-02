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
-define(DEFAULT_QUEUE_GROUP, #queue_group{name = "Default", sort = 0, protected = true, timestamp = 1}).

-include("log.hrl").
-include("queue.hrl").
-include("call.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API

%%
-export([
	build_tables/0,
	build_tables/1,
	merge/3
]).
-export([
	new_queue/1, 
	new_queue/5,
	destroy_queue/1,
	get_queue/1,
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
	set_client/2,
	set_client/4,
	get_client/1,
	get_clients/0]).

%% =====
%% All configs
%% =====

%% @doc Attempts to set-up and create the required mnesia table `call_queue' on all visible nodes.
%% @see build_tables/1
-spec(build_tables/0 :: () ->'ok').
build_tables() -> 
	build_tables(lists:append(nodes(), [node()])).

%TODO So why aren't we using Nodes here?
%% @doc Attempts to set-up and create the required mnesia table `call_queue' on the specified nodes
-spec(build_tables/1 :: (Nodes :: [atom()]) -> 'ok').
build_tables(_Nodes) -> 
	?DEBUG("~p building tables...", [?MODULE]),
	A = util:build_table(call_queue, ?QUEUE_TABLE([node()])),
	case A of
		{atomic, ok} -> 
			% since the table didn't already exist, build up the default queue
			new_queue(#call_queue{name = "default_queue", timestamp = util:now()}),
			ok;
		_Else -> 
			ok
	end,
	B = util:build_table(skill_rec, ?SKILL_TABLE([node()])),
	case B of
		{atomic, ok} ->
			% since the table didn't already exist, build up some default skills
			F = fun() -> 
				mnesia:write(#skill_rec{name="English", atom=english, description="English", group = "Language", timestamp = util:now()}),
				mnesia:write(#skill_rec{name="German", atom=german, description="German", group = "Language", timestamp = util:now()}),
				mnesia:write(#skill_rec{name="Agent Name", atom='_agent', description="Magic skill that is replaced by the agent's name.", group = "Magic", protected = true, timestamp = util:now()}),
				mnesia:write(#skill_rec{name="Node", atom='_node', description="Magic skill that is replaced by the node identifier.", group = "Magic", protected = true, timestamp = util:now()}),
				mnesia:write(#skill_rec{name="Queue", atom='_queue', description="Magic skill replaced by a queue's name", group = "Magic", protected = true, timestamp = util:now()}),
				mnesia:write(#skill_rec{name="All", atom='_all', description="Magic skill to denote an agent that can answer any call regardless of other skills.", group = "Magic", protected = true, timestamp = util:now()}),
				mnesia:write(#skill_rec{name="Brand", atom='_brand', description="Magic skill to expand to a client's label (brand)", group="Magic", protected=true, timestamp = util:now()})
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
				mnesia:write(#client{label = "Demo Client", tenant = 99, brand = 99, timestamp = util:now()})
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
		group = Group,
		timestamp = 1},
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
		mnesia:write(Rec#queue_group{timestamp = util:now()})
	end,
	mnesia:transaction(F).

%% @doc Add a new group with name, sort order and recipe
-spec(new_queue_group/3 :: (Name :: string(), Sort :: non_neg_integer(), Recipe :: recipe()) -> {'atomic', 'ok'}).
new_queue_group(Name, Sort, Recipe) when is_integer(Sort) ->
	Qgroup = #queue_group{name = Name, sort = Sort, recipe = Recipe, timestamp=1},
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
	Rec = #queue_group{name = Newname, sort = Newsort, recipe = Newrecipe, timestamp=util:now()},
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
	Rec = #skill_rec{atom = Skillatom, name = Skillname, description = Skilldesc, group = Group, timestamp = 1},
	new_skill(Rec).

%% @doc Add `#skill_rec{}' `Rec' to the configuration database.
-spec(new_skill/1 :: (Rec :: #skill_rec{}) -> {'atomic', 'ok'}).
new_skill(Rec) when is_record(Rec, skill_rec) ->
	F = fun() -> 
		mnesia:write(Rec#skill_rec{timestamp = util:now()})
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
-spec(new_client/3 :: (Label :: string(), Tenantid :: pos_integer(), Brandid :: pos_integer()) -> {'atomic', 'ok'}).
new_client(Label, Tenantid, Brandid) when is_integer(Tenantid), is_integer(Brandid) ->
	Rec = #client{label = Label, tenant = Tenantid, brand = Brandid, timestamp = 0},
	new_client(Rec).

-spec(new_client/1 :: (Rec :: #client{}) -> {'atomic', 'ok'}).
%% @doc Add a new client based on `#client{}' `Rec'.
new_client(Rec) when is_record(Rec, client) ->
	F = fun() ->
		mnesia:write(Rec#client{timestamp = util:now()})
	end,
	mnesia:transaction(F).

%% @doc Update the client `string()' `Label' to `string()' `Newlabel', `integer()' `Tenantid', `integer()' `Brandid'.
%% @see set_client/2
-spec(set_client/4 :: (Label :: string(), Newlabel :: string(), Tenantid :: pos_integer(), Brandid :: pos_integer()) -> {'atomic', 'ok'}).
set_client(Label, Newlabel, Tenantid, Brandid) when is_integer(Tenantid), is_integer(Brandid) ->
	Client = #client{label = Newlabel, tenant = Tenantid, brand = Brandid, timestamp = 1},
	set_client(Label, Client).

%% @doc Update the client `string()' `Label' to the `#client{}' `Client'.
-spec(set_client/2 :: (Label :: string(), Client :: #client{}) -> {'atomic', 'ok'}).
set_client(Label, Client) when is_record(Client, client) ->
	F = fun() ->
		mnesia:delete({client, Label}),
		mnesia:write(Client#client{timestamp = util:now()})
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
	diff_recs_loop(Left, Right, []).

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
	
%% =====
%% Tests
%% =====

-ifdef(EUNIT).

test_queue() -> 
	Recipe = [{2, add_skills, [true], run_once}],
	new_queue("test queue", 3, [testskill], Recipe, "Default"),
	%Default = #call_queue{name = "goober"},
	#call_queue{name = "test queue", weight = 3, skills = [testskill], recipe = Recipe, group = "Default", timestamp = util:now()}.
	
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
						skills = [], 
						timestamp = util:now()},
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
					TestQueue = #call_queue{skills = [testskill], name = "test queue", weight = 1, recipe = [], timestamp = util:now()},
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
					Queue = #call_queue{name="test queue", recipe=Recipe, skills = [], timestamp = util:now()},
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
					_Queue = new_queue(#call_queue{name = "test queue", timestamp = util:now()}),
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
						group = "New Group", 
						timestamp = util:now()
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
					Queue2 = Queue#call_queue{name="test queue 2", timestamp = util:now()},
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
					Queue2 = Queue#call_queue{name="test queue 2", timestamp = util:now()},
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
					Qgroup = #queue_group{name = "test group", timestamp = util:now()},
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
					Qgroup = #queue_group{name = "test group", sort = 15, recipe = Recipe, timestamp = util:now()},
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
					Qgroup = #queue_group{name = "test group", timestamp = util:now()},
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
					Sort10 = #queue_group{name = "Added 1st", sort = 10, timestamp = util:now()},
					Sort5 = #queue_group{name = "Added 2nd", sort = 5, timestamp = util:now()},
					Sort7 = #queue_group{name = "Added 3rd", sort = 7, timestamp = util:now()},
					new_queue_group(Sort10),
					new_queue_group(Sort5),
					new_queue_group(Sort7),
					Test = [#queue_group{name = "Default", sort = 0, protected = true, timestamp = util:now()}, Sort5, Sort7, Sort10],
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
					Recipe = [{[{ticks, 3}], prioritize, [], run_once}],
					Updateto = #queue_group{name = "newname", sort = 5, protected = false, recipe = Recipe, timestamp = util:now()},
					Default = ?DEFAULT_QUEUE_GROUP,
					Test = Default#queue_group{name = "newname", sort = 5, recipe = Recipe, timestamp = util:now()},
					Setres = set_queue_group(Default#queue_group.name, Updateto),
					?CONSOLE("res:  ~p", [Setres]),
					?assertEqual({atomic, ok}, Setres),
					?assertMatch({atomic, [#queue_group{name = "newname", sort = 5, recipe = Recipe}]}, get_queue_group("newname"))
				end
			},
			{
				"update a protected call group explicitly",
				fun() ->
					Recipe = [{[{ticks, 3}], prioritize, [], run_once}],
					Default = ?DEFAULT_QUEUE_GROUP,
					Test = Default#queue_group{name = "newname", sort = 5, recipe = Recipe, timestamp = util:now()},
					set_queue_group(Default#queue_group.name, "newname", 5, Recipe),
					?assertMatch({atomic, [#queue_group{name = "newname", sort = 5, recipe = Recipe}]}, get_queue_group("newname"))
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
						Skillrec = #skill_rec{name = "testskill", atom = testskill, description = "test skill", group = "Misc", protected = false, timestamp = util:now()},
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
						Skillrec = #skill_rec{name = "testskill", atom = testskill, description = "test skill", group = "Misc", protected = false, timestamp = util:now()},
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
							#skill_rec{name="English", atom=english, description="English", group = "Language", timestamp = util:now()},
							#skill_rec{name="German", atom=german, description="German", group = "Language", timestamp = util:now()},
							#skill_rec{name="Queue", atom='_queue', description="Magic skill replaced by a queue's name", group = "Magic", protected = true, timestamp = util:now()},
							#skill_rec{name="Node", atom='_node', description="Magic skill that is replaced by the node identifier.", group = "Magic", protected = true, timestamp = util:now()},
							#skill_rec{name="Agent Name", atom='_agent', description="Magic skill that is replaced by the agent's name.", group = "Magic", protected = true, timestamp = util:now()},
							#skill_rec{name="Brand", atom='_brand', description="Magic skill to expand to a client's label (brand)", group="Magic", protected=true, timestamp = util:now()},
							#skill_rec{name="All", atom='_all', description="Magic skill to denote an agent that can answer any call regardless of other skills.", group = "Magic", protected = true, timestamp = util:now()}
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
							#skill_rec{name="English", atom=english, description="English", group = "Language", timestamp = util:now()},
							#skill_rec{name="German", atom=german, description="German", group = "Language", timestamp = util:now()}
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
							#skill_rec{name="English", atom=english, description="English", group = "Talky", timestamp = util:now()},
							#skill_rec{name="German", atom=german, description="German", group = "Talky", timestamp = util:now()}
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
						Test = #skill_rec{name="English", atom=english, description="English", group = "Language", timestamp = util:now()},
						Testfun(Testfun, [Test], [get_skill(english)])
					end
				}
			end,
			fun(Test) ->
				{
					"update a skill",
					fun() ->
						New = #skill_rec{name="Newname", atom=testskill, description="Newdesc", group = "Newgroup", timestamp = util:now()},
						Result = #skill_rec{name="Newname", atom=english, description="Newdesc", group = "Newgroup", timestamp = util:now()},
						?assertEqual({atomic, ok}, set_skill(english, New)),
						Test(Test, [Result], [get_skill(english)]),
						?assertEqual(undefined, get_skill(testskill))
					end
				}
			end
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
					Client = #client{label = "testclient", tenant = 23, brand = 1, timestamp=util:now()},
					new_client(Client),
					F = fun() ->
						Select = qlc:q([X || X <- mnesia:table(client), X#client.label =:= "testclient"]),
						qlc:e(Select)
					end,
					?assertMatch({atomic, [#client{label = "testclient", tenant = 23, brand = 1}]}, mnesia:transaction(F))
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
					?assertMatch({atomic, [#client{label = "testclient", tenant = 23, brand = 1}]}, mnesia:transaction(F))
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
					?assertMatch({atomic, [#client{label = "newname", tenant = 47, brand = 2}]}, mnesia:transaction(Findnew))
				end
			},
			{
				"Update a client by record",
				fun() ->
					Client1 = #client{label = "Client1", tenant = 23, brand = 1, timestamp = util:now()},
					Client2 = #client{label = "Client2", tenant = 47, brand = 2, timestamp = util:now()},
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
					?assertMatch({atomic, [#client{label = "Client2"}]}, mnesia:transaction(Findnew))
				end
			},
			{
				"Get a client list",
				fun() ->
					Client1 = #client{label = "Client1", tenant = 23, brand = 1, timestamp = util:now()},
					Client2 = #client{label = "Client2", tenant = 47, brand = 2, timestamp = util:now()},
					Client3 = #client{label = "Aclient", tenant = 56, brand = 1, timestamp = util:now()},
					DemoClient = #client{label="Demo Client", tenant=99, brand=99, timestamp = util:now()},
					new_client(Client1),
					new_client(Client2),
					new_client(Client3),
					?assertMatch([#client{label = "Aclient"}, #client{label = "Client1"}, #client{label = "Client2"}, #client{label = "Demo Client"}], get_clients())
				end
			},
			{
				"Get a single client",
				fun() ->
					Client1 = #client{label = "Client1", tenant = 23, brand = 1, timestamp = util:now()},
					Client2 = #client{label = "Client2", tenant = 47, brand = 2, timestamp = util:now()},
					Client3 = #client{label = "Aclient", tenant = 56, brand = 1, timestamp = util:now()},
					new_client(Client1),
					new_client(Client2),
					new_client(Client3),
					Res = get_client("Client2"),
					?assertEqual(Client2#client.label, Res#client.label)
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

timestamp_test_() ->
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
		[{"Adding a queue updates timestamp",
		fun() ->
			Inrec = #call_queue{name = "test", timestamp = 1},
			new_queue(Inrec),
			F = fun() ->
				QH = qlc:q([X || X <- mnesia:table(call_queue), X#call_queue.name =:= "test"]),
				qlc:e(QH)
			end,
			{atomic, [Rec]} = mnesia:transaction(F),
			?assertNot(1 =:= Rec#call_queue.timestamp)
		end},
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
		{"Adding a queue group updates the timestamp",
		fun() ->
			new_queue_group(#queue_group{name = "test", timestamp = 1}),
			F = fun() ->
				QH = qlc:q([X || X <- mnesia:table(queue_group), X#queue_group.name =:= "test"]),
				qlc:e(QH)
			end,
			{atomic, [#queue_group{timestamp = T}]} = mnesia:transaction(F),
			?assert(1 < T)
		end},
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
		{"Adding a skill updates the timestamp",
		fun() ->
			new_skill(#skill_rec{atom = testskill, name = "test", timestamp = 1}),
			F = fun() ->
				QH = qlc:q([X || X <- mnesia:table(skill_rec), X#skill_rec.name =:= "test"]),
				qlc:e(QH)
			end,
			{atomic, [#skill_rec{timestamp = T}]} = mnesia:transaction(F),
			?assert(1 < T)
		end},
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
		{"Adding a client",
		fun() ->
			new_client(#client{label = "test", timestamp = 1}),
			F = fun() ->
				QH = qlc:q([X || X <- mnesia:table(client), X#client.label =:= "test"]),
				qlc:e(QH)
			end,
			{atomic, [#client{timestamp = T}]} = mnesia:transaction(F),
			?assert(1 < T)
		end},
		{"Updating a client",
		fun() ->
			new_client(#client{label = "test", timestamp = 1}),
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
