%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Original Code is Spice Telphony.
%% 
%% The Initial Developer of the Original Code is 
%% Andrew Thompson and Micah Warren.
%% Portions created by the Initial Developers are Copyright (C) 
%% SpiceCSM. All Rights Reserved.

%% Contributor(s): 

%% Andrew Thompson <athompson at spicecsm dot com>
%% Micah Warren <mwarren at spicecsm dot com>
%% 

%% @doc The helper module to config the call_queues.
%% Uses the mnesia table 'call_queue.'  Queues are not started until a call requires it.
-module(call_queue_config).
-author("Micah").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
% TODO remove the ram_copies config param, disc_copies implies ram.
-define(QUEUE_TABLE, 
	[
		{attributes, record_info(fields, call_queue)},
		{disc_copies, lists:append([nodes(), [node()]])},
		{ram_copies, nodes()}
	]
).
-define(SKILL_TABLE, 
	[
		{attributes, record_info(fields, skill_rec)},
		{disc_copies, lists:append([nodes(), [node()]])},
		{ram_copies, nodes()}
	]
).

-include("queue.hrl").
-include_lib("stdlib/include/qlc.hrl").

% TODO roll these into call_queue?
%% API
-export([
	new_queue/1, 
	new_queue/2,
	new_skill/1,
	new_skill/4,
	destroy/1,
	get_all/1,
	get_all/0,
	skill_exists/1,
	set_all/1,
	set_name/2,
	set_recipe/2,
	set_skills/2,
	set_weight/2,
	build_tables/0
]).

%% @doc Attempts to set-up and create the required mnesia table 'call_queue.'
%% Errors caused by the table already existing are ignored.
build_tables() -> 
	% TODO use util:build_table for both call_queue and skill_rec
	io:format("~p building tables...~n", [?MODULE]),
	A = util:build_table(call_queue, ?QUEUE_TABLE),
	case A of
		{atomic, ok} -> 
			% since the table didn't already exist, build up the default skills
			mnesia:create_table(skill_rec, ?SKILL_TABLE),
			F = fun() -> 
				mnesia:write(#skill_rec{name="English", atom=english, description="English Language"}),
				mnesia:write(#skill_rec{name="German", atom=german, description="German Language"}),
				mnesia:write(#skill_rec{name="Agent Name", atom='_agent', description="Magic skill that is replaced by the agent's name."}),
				mnesia:write(#skill_rec{name="Node", atom='_node', description="Magic skill that is replaced by the node identifier."})
			end,
			case mnesia:transaction(F) of
				{atomic, ok} -> 
					ok;
				Otherwise -> 
					Otherwise
			end;
		{aborted, {already_exists, _Table}} ->
			ok;
		Else -> 
			Else
	end.

%% @doc Attempt to remove the queue `Queue' (or `Queue#call_queue.name') from the configuration database.
destroy(Queue) when is_record(Queue, call_queue) -> 
	destroy(Queue#call_queue.name);
destroy(Queue) -> 
	F = fun() -> 
		mnesia:delete({call_queue, Queue})
	end,
	mnesia:transaction(F).
	
%% @doc Get the configuration for the passed `Queue' name.
get_all(Queue) ->
	% TODO this isn't a get_all, rename it.
	F = fun() -> 
		mnesia:read({call_queue, Queue})
	end,
	case mnesia:transaction(F) of
		{atomic, []} -> 
			[];
		{atomic, [Rec]} when is_record(Rec, call_queue) -> 
			[Rec];
		Else -> 
			{noexists, Else}
	end.
	
%% @doc Get all the queue configurations.
get_all() -> 
	QH = qlc:q([X || X <- mnesia:table(call_queue)]),
	F = fun() -> 
		qlc:e(QH)
	end,
	{atomic, Reply} = mnesia:transaction(F),
	Reply.

%% @doc Create a new default queue configuraiton with the name `QueueName'.
new_queue(QueueName) -> 
	new_queue(QueueName, []).

%% @doc Using with a single {Key, Value} tuple, or a list of same, create a new queue called QueueName and add it to the database.
%% Valid keys/value combos are:
%% <dl>
%% <dt>weight</dt><dd>An integer.</dd>
%% <dt>skills</dt><dd>A list of atoms for the skills a call will be initially assigned.</dd>
%% <dt>recipe</dt><dd>A recipe config for this queue for use by {@link cook. cooks}.</dd>
%% </dl>
new_queue(QueueName, {Key, Value}) when is_atom(Key) -> 
	new_queue(QueueName, [{Key, Value}]);
new_queue(QueueName, Options) when is_list(Options) -> 
	Q = #call_queue{name=QueueName},
	Fullqrec = set_options(Q, Options),
	F = fun() ->
		mnesia:write(Fullqrec)
	end,
	mnesia:transaction(F).

new_skill(Skillatom, Skillname, Skilldesc, Creator) when is_atom(Skillatom) ->
	Rec = #skill_rec{atom = Skillatom, name = Skillname, description = Skilldesc, creator = Creator},
	new_skill(Rec).

new_skill(Rec) when is_record(Rec, skill_rec) ->
	F = fun() -> 
		mnesia:write(Rec)
	end,
	mnesia:transaction(F).
		
%% @doc Set all the params for a config based on the record Queue.
set_all(Queue) when is_record(Queue, call_queue) -> 
	F = fun() -> 
		mnesia:write(Queue)
	end,
	mnesia:transaction(F).
	
%% @doc Rename a queue from OldName to NewName.
set_name(OldName, NewName) ->
	F = fun() -> 
		[OldRec] = mnesia:read({call_queue, OldName}),
		NewRec = OldRec#call_queue{name=NewName},
		mnesia:write(NewRec),
		mnesia:delete({call_queue, OldName})
	end,
	mnesia:transaction(F).

% TODO notify queue?  Kill the functions?
%% @doc Update the recipe for a queue named Queue.
set_recipe(Queue, Recipe) -> 
	F = fun() -> 
		[OldRec] = mnesia:read({call_queue, Queue}),
		NewRec = OldRec#call_queue{recipe=Recipe},
		mnesia:write(NewRec)
	end,
	mnesia:transaction(F).
	
%% @doc Update the skill list for a queue named Queue.
set_skills(Queue, Skills) -> 
	F = fun() -> 
		[OldRec] = mnesia:read({call_queue, Queue}),
		NewRec = OldRec#call_queue{skills=Skills},
		mnesia:write(NewRec)
	end,
	mnesia:transaction(F).
	
%% @doc Set the weight for a queue named Queue.
set_weight(Queue, Weight) when is_integer(Weight) andalso Weight >= 1-> 
	F = fun() ->
		[OldRec] = mnesia:read({call_queue, Queue}),
		NewRec = OldRec#call_queue{weight = Weight},
		mnesia:write(NewRec)
	end,
	mnesia:transaction(F).

%% @doc Check if the given `Skillname' exists.
skill_exists(Skillname) when is_list(Skillname) -> 
	try list_to_existing_atom(Skillname) of
		Anything -> 
			F = fun() -> 
				mnesia:read({skill_rec, Anything})
			end,
			{atomic, [Rec|_Tail]} = mnesia:transaction(F)
			% TODO what the hell is this returning?  Should be the skill atom
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
	mnesia:start(),
	build_tables(),
	[
		{
			"New Default Queue",
			fun() -> 
				Queue = #call_queue{name="test queue"},
				?assertMatch(Queue, new_queue("test queue"))
			end
		},
		{
			"New Queue with Weight",
			fun() -> 
				Queue = #call_queue{name="test queue", weight=3},
				?assertMatch(Queue, new_queue("test queue", {weight, 3}))
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
				?assertMatch(TestQueue, new_queue("test queue", {skills, [testskill]}))
			end
		},
		{
			"New Queue with Recipe",
			fun() -> 
				Recipe = [{2, add_skills, [true], run_once}],
				Queue = #call_queue{name="test queue", recipe=Recipe},
				?assertMatch(Queue, new_queue("test queue", {recipe, Recipe}))
			end
		},
		{
			"New Queue with Options List",
			fun() -> 
				Recipe = [{2, add_skills, [true], run_once}],
				Queue = #call_queue{name="test queue", recipe=Recipe, weight=3},
				TestQueue = Queue#call_queue{skills= lists:append([Queue#call_queue.skills, [testskill]])},
				Options = [{skills, [testskill]}, {weight, 3}, {recipe, Recipe}],
				?assertMatch(TestQueue, new_queue("test queue", Options))
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
				?assertMatch({atomic, [Queue]}, mnesia:transaction(F)),
				destroy(Queue)
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
				?assertMatch({atomic, [TestQueue]}, mnesia:transaction(SelectnewF)),
				?assertMatch({atomic, []}, mnesia:transaction(SelectoldF)),
				destroy(Queue),
				destroy(TestQueue)
			end
		},
		{
			"Set Recipe",
			fun() -> 
				Queue = test_queue(),
				set_all(Queue),
				Recipe = [{3, new_queue, ["queue name"], run_once}],
				TestQueue = Queue#call_queue{recipe = Recipe},
				set_recipe(Queue#call_queue.name, Recipe),
				Select = qlc:q([X || X <- mnesia:table(call_queue), X#call_queue.name =:= Queue#call_queue.name]),
				F = fun() -> 
					qlc:e(Select)
				end,
				?assertMatch({atomic, [TestQueue]}, mnesia:transaction(F)),
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
				?assertMatch({atomic, [TestQueue]}, mnesia:transaction(F)),
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
				?assertMatch({atomic, [TestQueue]}, mnesia:transaction(F)),
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
				?assertMatch({atomic, []}, mnesia:transaction(F))
			end
		},
		{
			"Get All",
			fun() -> 
				Queue = test_queue(),
				Queue2 = Queue#call_queue{name="test queue 2"},
				set_all(Queue),
				set_all(Queue2),
				?assertMatch([Queue, Queue2], get_all()),
				destroy(Queue),
				destroy(Queue2)
			end
		},
		{
			"Get All of One",
			fun() -> 
				Queue = test_queue(),
				Queue2 = Queue#call_queue{name="test queue 2"},
				set_all(Queue),
				set_all(Queue2),
				?assertMatch(Queue, get_all(Queue#call_queue.name)),
				?assertMatch(Queue2, get_all(Queue2#call_queue.name)),
				destroy(Queue),
				destroy(Queue2)
			end
		},
		{
			"Get All of None",
			fun() -> 
				Queue = test_queue(),
				destroy(Queue),
				?assertMatch(noexists, get_all(Queue#call_queue.name))
			end
		}
	 ].

skill_rec_test_() -> 
	mnesia:start(),
	[
		{
			"Test for a known skill atom",
			fun() -> 
				Skillrec = skill_exists("_node"),
				?assertMatch('_node', Skillrec#skill_rec.atom)
			end
		},
		{
			"Test for an unknown skill atom",
			fun() -> 
				Skillrec = skill_exists("Not a valid skill"),
				?assertMatch(undefined, Skillrec)
			end
		}
	].

-endif.
