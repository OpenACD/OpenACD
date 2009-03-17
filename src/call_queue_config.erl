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

-include("queue.hrl").
-include("call.hrl").
-include_lib("stdlib/include/qlc.hrl").

% TODO roll these into call_queue?
%% API
-export([
	new_queue/1, 
	new_queue/2,
	new_skill/1,
	new_skill/4,
	new_client/1,
	new_client/3,
	destroy_client/1,
	set_client/2,
	set_client/4,
	get_client/1,
	get_clients/0,
	destroy/1,
	get_queue/1,
	get_all/0,
	skill_exists/1,
	set_all/1,
	set_name/2,
	set_recipe/2,
	set_skills/2,
	set_weight/2,
	build_tables/0,
	build_tables/1
]).

%% @doc Attempts to set-up and create the required mnesia table `call_queue' on all visible nodes.
%% Errors caused by the table already existing are ignored.
%% @see build_tables/1
build_tables() -> 
	build_tables(lists:append(nodes(), [node()])).

%% @doc Attempts to set-up and create the required mnesia table `call_queue' on the specified nodes
%% Errors caused by the table already existing are ignored.
build_tables(Nodes) -> 
	?CONSOLE("~p building tables...", [?MODULE]),
	A = util:build_table(call_queue, ?QUEUE_TABLE(Nodes)),
	case A of
		{atomic, ok} -> 
			% since the table didn't already exist, build up the default queue
			new_queue("default_queue"),
			ok;
		_Else -> 
			ok
	end,
	B = util:build_table(skill_rec, ?SKILL_TABLE(Nodes)),
	case B of
		{atomic, ok} ->
			% since the table didn't already exist, build up some default skills
			F = fun() -> 
				mnesia:write(#skill_rec{name="English", atom=english, description="English Language"}),
				mnesia:write(#skill_rec{name="German", atom=german, description="German Language"}),
				mnesia:write(#skill_rec{name="Agent Name", atom='_agent', description="Magic skill that is replaced by the agent's name."}),
				mnesia:write(#skill_rec{name="Node", atom='_node', description="Magic skill that is replaced by the node identifier."}),
				mnesia:write(#skill_rec{name="Queue", atom='_queue', description="Magic skill replaced by a queue's name"}),
				mnesia:write(#skill_rec{name="All", atom='_all', description="Magic skill to denote an agent that can answer any call regardless of other skills."})
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
	C = util:build_table(client, ?CLIENT_TABLE(Nodes)),
	case C of
		{atomic, ok} ->
			ok;
		_Orelse ->
			ok
	end.

%% @doc Attempt to remove the queue `#call_queue{}' or `string()' `Queue' from the configuration database.
-spec(destroy/1 :: (Queue :: #call_queue{}) -> {atom(), any()};
					(Queue :: string()) -> {atom(), any()}).
destroy(Queue) when is_record(Queue, call_queue) -> 
	destroy(Queue#call_queue.name);
destroy(Queue) -> 
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
	
%% @doc Get all the queue configurations (`[#call_queue{}]').
-spec(get_all/0 :: () -> [#call_queue{}]).
get_all() -> 
	QH = qlc:q([X || X <- mnesia:table(call_queue)]),
	F = fun() -> 
		qlc:e(QH)
	end,
	{atomic, Reply} = mnesia:transaction(F),
	Reply.

%% @doc Create a new default queue configuraiton with the name `string()' `QueueName'.
%% @see new_queue/2
-spec(new_queue/1 :: (QueueName :: string()) ->#call_queue{}).
new_queue(QueueName) -> 
	new_queue(QueueName, []).

%% @doc Using with a single `{Key, Value}', or `[{Key, Value}]', create a new queue called `string()' `QueueName' and add it to the database.
%% Valid keys/value combos are:
%% <dl>
%% <dt>weight</dt><dd>An integer.</dd>
%% <dt>skills</dt><dd>A list of atoms for the skills a call will be initially assigned.</dd>
%% <dt>recipe</dt><dd>A recipe config for this queue for use by {@link cook. cooks}.</dd>
%% </dl>
-spec(new_queue/2 :: (QueueName :: string(), {'weight' | 'skills' | 'recipe', any()}) -> #call_queue{};
					(QueueName :: string(), [{'weight' | 'skills' | 'recipe', any()}]) -> #call_queue{}).
new_queue(QueueName, {Key, Value}) when is_atom(Key) -> 
	new_queue(QueueName, [{Key, Value}]);
new_queue(QueueName, Options) when is_list(Options) -> 
	Q = #call_queue{name=QueueName},
	Fullqrec = set_options(Q, Options),
	F = fun() ->
		mnesia:write(Fullqrec)
	end,
	{atomic, ok} = mnesia:transaction(F),
	case whereis(queue_manager) of
		undefined ->
			Fullqrec;
		QMPid when is_pid(QMPid) ->
			queue_manager:add_queue(Fullqrec#call_queue.name, Fullqrec#call_queue.recipe, Fullqrec#call_queue.weight),
			Fullqrec
	end.

%% @doc Add a new skill to the configuration database.  `atom()' `Skillatom', `string()' `Skillname', 
%% `string()' `Skilldesc', `string()' `Creator'.
%% @see new_skill
-spec(new_skill/4 :: (Skillatom :: atom(), Skillname :: string(), Skilldesc :: string(), Creator :: string()) -> {'atomic', 'ok'}).
new_skill(Skillatom, Skillname, Skilldesc, Creator) when is_atom(Skillatom) ->
	Rec = #skill_rec{atom = Skillatom, name = Skillname, description = Skilldesc, creator = Creator},
	new_skill(Rec).

%% @doc Add `#skill_rec{}' `Rec' to the configuration database.
-spec(new_skill/1 :: (Rec :: #skill_rec{}) -> {'atomic', 'ok'}).
new_skill(Rec) when is_record(Rec, skill_rec) ->
	F = fun() -> 
		mnesia:write(Rec)
	end,
	mnesia:transaction(F).

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

%% @doc Set all the params for a config based on the `#call_queue{}' `Queue'.  Returns the results of the mnesia transaction.
-spec(set_all/1 :: (Queue :: #call_queue{}) -> {'aborted', any()} | {'atomic', any()}).
set_all(Queue) when is_record(Queue, call_queue) -> 
	F = fun() -> 
		mnesia:write(Queue)
	end,
	mnesia:transaction(F).
	
%% @doc Rename a queue from `string()' `OldName' to `string()' `NewName'.  Returns the results of the mnesia transaction.
-spec(set_name/2 :: (OldName :: string(), NewName :: string()) -> any()).
set_name(OldName, NewName) ->
	F = fun() -> 
		[OldRec] = mnesia:read({call_queue, OldName}),
		NewRec = OldRec#call_queue{name=NewName},
		mnesia:write(NewRec),
		mnesia:delete({call_queue, OldName})
	end,
	mnesia:transaction(F).

% TODO notify queue?  Kill the functions?
%% @doc Update `string()' `Queue' with a new `recipe()' `Recipe'.
-spec(set_recipe/2 :: (Queue :: string(), Recipe :: recipe()) -> any()).
set_recipe(Queue, Recipe) -> 
	F = fun() -> 
		[OldRec] = mnesia:read({call_queue, Queue}),
		NewRec = OldRec#call_queue{recipe=Recipe},
		mnesia:write(NewRec)
	end,
	mnesia:transaction(F).
	
%% @doc Update the `string()' `Queue' replacing the skills with `[atom()]' `Skills'.
%% Returns the result of the mnesia transaction.
-spec(set_skills/2 :: (Queue :: string(), Skills :: [atom()]) -> any()).
set_skills(Queue, Skills) -> 
	F = fun() -> 
		[OldRec] = mnesia:read({call_queue, Queue}),
		NewRec = OldRec#call_queue{skills=Skills},
		mnesia:write(NewRec)
	end,
	mnesia:transaction(F).
	
%% @doc Update `string()' `Queue' with a new wight of `pos_integer()' `Weight'.
%% Returns the result of the mnesia transaction.
-spec(set_weight/2 :: (Queue :: string(), Weight :: pos_integer()) -> any()).
set_weight(Queue, Weight) when is_integer(Weight) andalso Weight >= 1-> 
	F = fun() ->
		[OldRec] = mnesia:read({call_queue, Queue}),
		NewRec = OldRec#call_queue{weight = Weight},
		mnesia:write(NewRec)
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
			{atomic, [Rec|_Tail]} = mnesia:transaction(F),
			Rec#skill_rec.atom
	catch
		error:_Anyerror -> 
			undefined
	end.

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
			set_options(QueueRec#call_queue{recipe = Value}, Tail)
	end.

-ifdef(EUNIT).

test_queue() -> 
	Recipe = [{2, add_skills, [true], run_once}],
	Options = [{skills, [testskill]}, {weight, 3}, {recipe, Recipe}],
	new_queue("test queue", Options).
	
call_queue_test_() ->
	["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
	{
		setup,
		fun() -> 
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			mnesia:create_schema([node()]),
			mnesia:start(),
			build_tables(),
			F = fun() -> 
				mnesia:delete({call_queue, "default_queue"})
			end,
			mnesia:transaction(F)
		end,
		fun(_Whatever) -> 
			mnesia:stop(),
			mnesia:delete_schema([node()]),
			ok
		end,
		[
			{
				"New Default Queue",
				fun() -> 
					Queue = #call_queue{name="test queue"},
					?assertEqual(Queue, new_queue("test queue"))
				end
			},
			{
				"New Queue with Weight",
				fun() -> 
					Queue = #call_queue{name="test queue", weight=3},
					?assertEqual(Queue, new_queue("test queue", {weight, 3}))
				end
			},
			{
				"New Queue with Invalid Weight",
				fun() -> 
					?assertError({case_clause, weight}, new_queue("name", {weight, "not a number"}))
				end
			},
			{
				"New Queue with Skills",
				fun() ->
					Queue = #call_queue{name="test queue"},
					TestQueue = Queue#call_queue{skills = lists:append([Queue#call_queue.skills, [testskill]])},
					?assertEqual(TestQueue, new_queue("test queue", {skills, [testskill]}))
				end
			},
			{
				"New Queue with Recipe",
				fun() -> 
					Recipe = [{2, add_skills, [true], run_once}],
					Queue = #call_queue{name="test queue", recipe=Recipe},
					?assertEqual(Queue, new_queue("test queue", {recipe, Recipe}))
				end
			},
			{
				"New Queue with Options List",
				fun() -> 
					Recipe = [{2, add_skills, [true], run_once}],
					Queue = #call_queue{name="test queue", recipe=Recipe, weight=3},
					TestQueue = Queue#call_queue{skills= lists:append([Queue#call_queue.skills, [testskill]])},
					Options = [{skills, [testskill]}, {weight, 3}, {recipe, Recipe}],
					?assertEqual(TestQueue, new_queue("test queue", Options))
				end
			},
			{
				"Set All",
				fun() -> 
					Recipe = [{2, add_skills, [true], run_once}],
					Options = [{skills, [testskill]}, {weight, 3}, {recipe, Recipe}],
					Queue = new_queue("test queue", Options),
					set_all(Queue),
					QH = qlc:q([X || X <- mnesia:table(call_queue), X#call_queue.name =:= "test queue"]),
					F = fun() -> 
						qlc:e(QH)
					end,
					?assertEqual({atomic, [Queue]}, mnesia:transaction(F)),
					destroy(Queue)
				end
			},
			{
				"New Queue with active Queue Manager",
				fun() ->
					queue_manager:start([node()]),
					_Queue = new_queue("test queue"),
					?assertMatch(true, queue_manager:query_queue("test queue")),
					queue_manager:stop()
				end
			},
			{
				"Set Name", 
				fun() ->
					Queue = test_queue(),
					set_all(Queue),
					TestQueue = Queue#call_queue{name="new name"},
					set_name(Queue#call_queue.name, TestQueue#call_queue.name),
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
					destroy(Queue),
					destroy(TestQueue)
				end
			},
			{
				"Set Recipe",
				fun() -> 
					Queue = test_queue(),
					set_all(Queue),
					Recipe = [{3, announce, "announcement", run_once}],
					TestQueue = Queue#call_queue{recipe = Recipe},
					set_recipe(Queue#call_queue.name, Recipe),
					Select = qlc:q([X || X <- mnesia:table(call_queue), X#call_queue.name =:= Queue#call_queue.name]),
					F = fun() -> 
						qlc:e(Select)
					end,
					?assertEqual({atomic, [TestQueue]}, mnesia:transaction(F)),
					destroy(Queue)
				end
			},
			{
				"Set Skills",
				fun() -> 
					Queue = test_queue(), 
					set_all(Queue),
					TestQueue = Queue#call_queue{skills = [german]},
					set_skills(Queue#call_queue.name, [german]),
					Select = qlc:q([X || X <- mnesia:table(call_queue), X#call_queue.name =:= Queue#call_queue.name]),
					F = fun() ->
						qlc:e(Select)
					end,
					?assertEqual({atomic, [TestQueue]}, mnesia:transaction(F)),
					destroy(Queue)
				end
			},
			{
				"Set Weight",
				fun() -> 
					Queue = test_queue(),
					set_all(Queue),
					TestQueue = Queue#call_queue{weight = 7},
					set_weight(Queue#call_queue.name, 7),
					Select = qlc:q([X || X <- mnesia:table(call_queue), X#call_queue.name =:= Queue#call_queue.name]),
					F = fun() -> 
						qlc:e(Select)
					end,
					?assertEqual({atomic, [TestQueue]}, mnesia:transaction(F)),
					destroy(Queue)
				end
			},
			{
				"Destroy",
				fun() -> 
					Queue = test_queue(),
					set_all(Queue),
					destroy(Queue),
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
					set_all(Queue),
					set_all(Queue2),
					?assertEqual([Queue, Queue2], get_all()),
					destroy(Queue),
					destroy(Queue2)
				end
			},
			{
				"Get One Queue",
				fun() -> 
					Queue = test_queue(),
					Queue2 = Queue#call_queue{name="test queue 2"},
					set_all(Queue),
					set_all(Queue2),
					?assertEqual(Queue, get_queue(Queue#call_queue.name)),
					?assertEqual(Queue2, get_queue(Queue2#call_queue.name)),
					destroy(Queue),
					destroy(Queue2)
				end
			},
			{
				"Get All of None",
				fun() -> 
					Queue = test_queue(),
					destroy(Queue),
					?assertMatch(noexists, get_queue(Queue#call_queue.name))
				end
			}
		]
	}.

skill_rec_test_() -> 
	["testpx", _Host] = string:tokens(atom_to_list(node()), "@"),
	{
		setup,
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
					new_client(Client1),
					new_client(Client2),
					new_client(Client3),
					?assertEqual([Client3, Client1, Client2], get_clients())
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
