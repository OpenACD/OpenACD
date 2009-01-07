%%%-------------------------------------------------------------------
%%% File          : call_queue_config.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  12/3/08
%%%-------------------------------------------------------------------

%% @doc The helper module to config the call_queues for cpx.
%% Uses the mnesia table 'call_queue.'  Queues are not started until a call requires it.
-module(call_queue_config).
-author(null).

-behaviour(gen_server).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-define(QUEUE_TABLE, [{attributes, record_info(fields, call_queue)},{ram_copies, nodes()}]).
-define(SKILL_TABLE, [{attributes, record_info(fields, skill_rec)}, {ram_copies, nodes()}]).
-else.
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
-endif.

-include("queue.hrl").
-include_lib("stdlib/include/qlc.hrl").


%% API
-export([
	start_link/0, 
	start/0, 
	new/1, 
	new/2, 
	destroy/1,
	get_all/1,
	get_all/0,
	skill_exists/1,
	set_all/1,
	set_name/2,
	set_recipe/2,
	set_skills/2,
	set_weight/2,
	stop/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() -> 
	gen_server:start({global, ?MODULE}, ?MODULE, [], []).
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
	A = build_tables(),
	io:format("~p~n", [A]),
	case A of
		ok -> 
			{ok, #state{}};
		Else -> 
			{stop, {build_tables, Else}}
	end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({destroy, Queue}, _From, State) -> 
	F = fun() -> 
		mnesia:delete({call_queue, Queue})
	end,
	mnesia:transaction(F),
	{reply, ok, State};
handle_call({set_all, Queue}, _From, State) ->
	F = fun() -> 
		mnesia:write(Queue)
	end,
	mnesia:transaction(F),
	{reply, ok, State};
handle_call({set_name, OldName, NewName}, _From, State) -> 
	F = fun() -> 
		[OldRec] = mnesia:read({call_queue, OldName}),
		NewRec = OldRec#call_queue{name=NewName},
		mnesia:write(NewRec),
		mnesia:delete({call_queue, OldName})
	end,
	mnesia:transaction(F),
	{reply, ok, State};
handle_call({set_recipe, Queue, Recipe}, _From, State) -> 
	F = fun() -> 
		[OldRec] = mnesia:read({call_queue, Queue}),
		NewRec = OldRec#call_queue{recipe=Recipe},
		mnesia:write(NewRec)
	end,
	mnesia:transaction(F),
	{reply, ok, State};
handle_call({set_skills, Queue, Skills}, _From, State) ->
	F = fun() -> 
		[OldRec] = mnesia:read({call_queue, Queue}),
		NewRec = OldRec#call_queue{skills=Skills},
		mnesia:write(NewRec)
	end,
	mnesia:transaction(F),
	{reply, ok, State};
handle_call({set_weight, Queue, Weight}, _From, State) -> 
	F = fun() ->
		[OldRec] = mnesia:read({call_queue, Queue}),
		NewRec = OldRec#call_queue{weight = Weight},
		mnesia:write(NewRec)
	end,
	mnesia:transaction(F),
	{reply, ok, State};
handle_call({get, Queue}, _From, State) -> 
	F = fun() -> 
		mnesia:read({call_queue, Queue})
	end,
	case mnesia:transaction(F) of
		{atomic, []} -> 
			{reply, noexists, State};
		{atomic, [Rec]} when is_record(Rec, call_queue) -> 
			{reply, Rec, State};
		Else -> 
			{reply, {noexists, Else}, State}
	end;
handle_call(get_all, _From, State) -> 
	QH = qlc:q([X || X <- mnesia:table(call_queue)]),
	F = fun() -> 
		qlc:e(QH)
	end,
	{atomic, Reply} = mnesia:transaction(F),
	{reply, Reply, State};
handle_call({get_skill_rec, Skillname}, _From, State) -> 
	F = fun() -> 
		mnesia:read({skill_rec, Skillname})
	end,
	{atomic, [Rec|_Tail]} = mnesia:transaction(F),
	{reply, Rec, State};
handle_call(stop, _From, State) -> 
	{stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @doc Attempts to set-up and create the required mnesia table 'call_queue.'
%% Errors caused by the table already existing are ignored.
build_tables() -> 
	io:format("~p building tables...~n", [?MODULE]),
	Nodes = lists:append([nodes(), [node()]]),
	io:format("disc_copies: ~p~n", [Nodes]),
	mnesia:create_schema(Nodes),
	mnesia:start(),
	A = mnesia:create_table(call_queue, ?QUEUE_TABLE),
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

%% @doc Attempt to remove the queue Queue (or Queue#call_queue.name) from the configuration database.
destroy(Queue) when is_record(Queue, call_queue) -> 
	destroy(Queue#call_queue.name);
destroy(Queue) -> 
	gen_server:call({global, ?MODULE}, {destroy, Queue}).

%% @doc Get the configuration for the passed Queue name.
get_all(Queue) ->
	gen_server:call({global, ?MODULE}, {get, Queue}).

%% @doc Get all the queue configurations.
get_all() -> 
	gen_server:call({global, ?MODULE}, get_all).

%% @doc Create a new default queue configuraiton with the name QueueName.
new(QueueName) -> 
	new(QueueName, []).

%% @doc Using with a single {Key, Value} tuple, or a list of same, create a new queue called QueueName and add it to the database.
%% Valid keys/value combos are:
%% <dl>
%% <dt>wieght</dt><dd>An integer.</dd>
%% <dt>skills</dt><dd>A list of atoms for the skills a call will be initially assigned.</dd>
%% <dt>recipe</dt><dd>A recipe config for this queue for use by {@link cook. cooks}.</dd>
%% </dl>
new(QueueName, {Key, Value}) when is_atom(Key) -> 
	new(QueueName, [{Key, Value}]);
new(QueueName, Options) when is_list(Options) -> 
	Q = #call_queue{name=QueueName},
	set_options(Q, Options).

%% @doc Set all the params for a config based on the record Queue.
set_all(Queue) when is_record(Queue, call_queue) -> 
	gen_server:call({global, ?MODULE}, {set_all, Queue}).

%% @doc Rename a queue from OldName to NewName.
set_name(OldName, NewName) ->
	gen_server:call({global, ?MODULE}, {set_name, OldName, NewName}).

%% @doc Update the recipe for a queue named Queue.
set_recipe(Queue, Recipe) -> 
	gen_server:call({global, ?MODULE}, {set_recipe, Queue, Recipe}).

%% @doc Update the skill list for a queue named Queue.
set_skills(Queue, Skills) -> 
	gen_server:call({global, ?MODULE}, {set_skills, Queue, Skills}). 

%% @doc Set the weight for a queue named Queue.
set_weight(Queue, Weight) when is_integer(Weight) andalso Weight >= 1-> 
	gen_server:call({global, ?MODULE}, {set_weight, Queue, Weight}).

skill_exists(Skillname) when is_list(Skillname) -> 
	try list_to_existing_atom(Skillname) of
		Anything -> 
			gen_server:call({global, ?MODULE}, {get_skill_rec, Anything})
	catch
		error:_Anyerror -> 
			undefined
	end.

stop() -> 
	gen_server:call({global, ?MODULE}, stop).

set_options(QueueRec, []) -> 
	QueueRec;
set_options(QueueRec, [{Key, Value} | Tail]) -> 
	case Key of
		weight when is_integer(Value) -> 
			set_options(QueueRec#call_queue{weight=Value}, Tail);
		skills -> 
			Skills = lists:append([QueueRec#call_queue.skills, Value]),
			set_options(QueueRec#call_queue{skills=Skills}, Tail);
		recipe -> 
			set_options(QueueRec#call_queue{recipe = Value}, Tail)
	end.

-ifdef(EUNIT).

test_queue() -> 
	Recipe = [{2, add_skills, [true], run_once}],
	Options = [{skills, [testskill]}, {weight, 3}, {recipe, Recipe}],
	new("test queue", Options).
	
call_queue_test_() -> 
	mnesia:start(),
	{
		setup,
		local,
		fun() -> 
			start(),
			ok
		end,
		fun(_) -> 
			stop()
		end,
		[
			{
				"New Default Queue",
				fun() -> 
					Queue = #call_queue{name="test queue"},
					?assertMatch(Queue, new("test queue"))
				end
			},
			{
				"New Queue with Weight",
				fun() -> 
					Queue = #call_queue{name="test queue", weight=3},
					?assertMatch(Queue, new("test queue", {weight, 3}))
				end
			},
			{
				"New Queue with Invalid Weight",
				fun() -> 
					?assertError({case_clause, weight}, new("name", {weight, "not a number"}))
				end
			},
			{
				"New Queue with Skills",
				fun() ->
					Queue = #call_queue{name="test queue"},
					TestQueue = Queue#call_queue{skills = lists:append([Queue#call_queue.skills, [testskill]])},
					?assertMatch(TestQueue, new("test queue", {skills, [testskill]}))
				end
			},
			{
				"New Queue with Recipe",
				fun() -> 
					Recipe = [{2, add_skills, [true], run_once}],
					Queue = #call_queue{name="test queue", recipe=Recipe},
					?assertMatch(Queue, new("test queue", {recipe, Recipe}))
				end
			},
			{
				"New Queue with Options List",
				fun() -> 
					Recipe = [{2, add_skills, [true], run_once}],
					Queue = #call_queue{name="test queue", recipe=Recipe, weight=3},
					TestQueue = Queue#call_queue{skills= lists:append([Queue#call_queue.skills, [testskill]])},
					Options = [{skills, [testskill]}, {weight, 3}, {recipe, Recipe}],
					?assertMatch(TestQueue, new("test queue", Options))
				end
			},
			{
				"Set All",
				fun() -> 
					Recipe = [{2, add_skills, [true], run_once}],
					Options = [{skills, [testskill]}, {weight, 3}, {recipe, Recipe}],
					Queue = new("test queue", Options),
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
		 ]
	}.

skill_rec_test_() -> 
	mnesia:start(),
	{
		setup,
		local,
		fun() -> 
			start(),
			ok
		end,
		fun(_) -> 
			stop()
		end,
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
		]
	}.

-endif.
