%%%-------------------------------------------------------------------
%%% File          : agent_auth.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  12/2/08
%%%-------------------------------------------------------------------
-module(agent_auth).
-author(null).

-behaviour(gen_server).

-include("call.hrl").
-include("agent.hrl").
-include_lib("stdlib/include/qlc.hrl").

-ifdef(EUNIT).
	-include_lib("eunit/include/eunit.hrl").
-endif.


%% API
-export([start_link/5, start/5, start_link/0, start/0, stop/0, auth/3]).

-ifdef(EUNIT).
	-export([cache/3, destroy/1]).
-endif.

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	integration,
	mod,
	start_func,
	start_args,
	check_func,
	check_args
}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

% @doc The Mod, Func, and Args are the module, funciton, and args needed to start the integration (if any).
start() -> 
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start_link() -> 
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start(Mod, StartFunc, StartArgs, CheckFunc, CheckArgs) -> 
	gen_server:start({local, ?MODULE}, ?MODULE, [Mod, StartFunc, StartArgs, CheckFunc, CheckArgs], []).
start_link(Mod, StartFunc, StartArgs, CheckFunc, CheckArgs) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Mod, StartFunc, StartArgs, CheckFunc, CheckArgs], []).

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
init([Mod, StartFunc, StartArgs, CheckFunc, CheckArgs]) ->
	case build_tables() of
		ok -> 
			apply(Mod, StartFunc, StartArgs),
			{ok, #state{mod=Mod, start_func=StartFunc, start_args=StartArgs, check_func = CheckFunc, check_args = CheckArgs}};
		Else -> 
			{stop, {build_tables, Else}}
	end;
init([]) -> 
	case build_tables() of
		ok -> 
			{ok, #state{integration = false}};
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
handle_call({authentication, Username, Password, Salt}, _From, State) -> 
	% start w/ the remote try.  If that fails, try the local.
	case State#state.integration of
		false -> 
			io:format("local authentication only~n"),
			Reply = local_auth(Username, Password, Salt),
			{reply, Reply, State};
		_Else -> 
			io:format("remote authenitcaitona attempt first~n"),
			Args = lists:append([[Username, Password, Salt], State#state.check_args]),
			case apply(State#state.mod, State#state.check_func, Args) of
				{allow, Skills} -> 
					cache(Username, Password, Skills),
					{reply, {allow, lists:appned([Skills, ['_agent', '_node']])}, State};
				deny -> 
					destroy(Username),
					{reply, deny, State};
				_Else -> 
					Reply = local_auth(Username, Password, Salt),
					{reply, Reply, State}
			end
	end;
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call(Request, _From, State) ->
	io:format("agent_auth does not understand request:  ~p~n", [Request]),
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

auth(Username, Password, Salt) -> 
	gen_server:call(?MODULE, {authentication, Username, Password, Salt}).

build_tables() ->
	io:format("building tables...~n"),
	Nodes = lists:append([nodes(), [node()]]),
	io:format("disc_copies: ~p~n", [Nodes]),
	mnesia:create_schema(Nodes),
	mnesia:start(),
	A = mnesia:create_table(agent_auth, [
		{attributes, record_info(fields, agent_auth)},
		{disc_copies, Nodes},
		{ram_copies, nodes()}
	]),
	io:format("~p~n", [A]),
	case A of
		{atomic, ok} -> 
			ok;
		{aborted, {already_exists, _Table}} ->
			ok;
		Else -> 
			Else
	end.
	
cache(Username, Password, Skills) -> 
	% md5 the password before it gets here!
	Agent = #agent_auth{
		login=Username,
		password = Password,
		skills = Skills},
	F = fun() -> 
		mnesia:write(Agent)
	end,
	mnesia:transaction(F).

destroy(Username) -> 
	F = fun() -> 
		mnesia:delete({agent_auth, Username})
	end,
	mnesia:transaction(F).

clean_salt(Salt) when is_integer(Salt) -> 
	clean_salt(integer_to_list(Salt));
clean_salt(Salt) when is_binary(Salt) -> 
	Salt;
clean_salt(Salt) when is_list(Salt) -> 
	list_to_binary(Salt).

clean_password(Password) when is_binary(Password) -> 
	Password;
clean_password(Password) when is_list(Password) -> 
	iolist_to_binary(Password).

local_auth(Username, TPassword, TSalt) -> 
	Password = clean_password(TPassword),
	Salt = clean_salt(TSalt),
	QH = qlc:q([X || X <- mnesia:table(agent_auth), X#agent_auth.login =:= Username]),
	% salt the password	
	F = fun() -> qlc:e(QH) end,
	case mnesia:transaction(F) of
		%{error, mnesia, {aborted, {no_exists, agent_auth}}} -> 
		%	mnesia:create_table(agent_auth, record_info(fields, agent_auth)),
		%	deny;
		{atomic, []} -> 
			deny;
		{atomic, [Agent]} when is_record(Agent, agent_auth) -> 
			%check the password after salt
			case erlang:md5(Salt ++ Agent#agent_auth.password) of
				Password -> 
					{allow, lists:append([Agent#agent_auth.skills, ['_agent', '_node']])};
				Else -> 
					io:format("Password: ~p~nSalting: ~p~n", [Password, Else]),
					deny
			end;
		Else ->
			io:format("Unusual respnse from local auth query: ~p~n", [Else]),
			deny
	end.

stop() -> 
	gen_server:call(?MODULE, stop).

-ifdef(EUNIT).

local_auth_test_() -> 
	mnesia:start(),
	{
		setup,
		fun() -> 
			start(),
			ok
		end,
		fun(_) -> 
			stop()
		end,
		[
			{
				"Cache a user 'A'",
				fun() -> 
					?assertMatch({atomic, ok}, cache("A", erlang:md5("B"), [])),
					destroy("A")
				end
			},
			{
				"Auth a user 'A'",
				fun() -> 
					cache("A", erlang:md5("B"), [testskill]),
					?assertMatch({allow, [testskill, '_agent', '_node']}, local_auth("A", erlang:md5(integer_to_list(5) ++ erlang:md5("B")), 5)),
					destroy("A"),
					?assertMatch(deny, local_auth("A", erlang:md5(integer_to_list(5) ++ erlang:md5("B")), 5))
				end
			},
			{
				"Destroy a user 'A'",
				fun() -> 
					cache("A", erlang:md5("B"), [testskill]),
					destroy("A"),
					?assertMatch(deny, local_auth("A", erlang:md5("Salt" ++ erlang:md5("B")), 5))
				end
			},
			{
				"Raw Cache Test",
				fun() -> 
					cache("A", erlang:md5("B"), [testskill]),
					Agent = #agent_auth{
						login="A",
						password=erlang:md5("B"),
						skills=[testskill]},
					F = fun() -> 
						Pass = erlang:md5("B"),
						QH = qlc:q([X || X <- mnesia:table(agent_auth), X#agent_auth.login =:= "A", X#agent_auth.password =:= Pass]),
						qlc:e(QH)
					end,
					?assertMatch({atomic, [Agent]}, mnesia:transaction(F)),
					destroy("A")
				end
			},
			{
				"Raw Destroy Test",
				fun() -> 
					cache("A", "B", [testskill]),
					destroy("A"),
					F = fun() -> 
						Pass = erlang:md5("B"),
						QH = qlc:q([X || X <- mnesia:table(agent_auth), X#agent_auth.login =:= "A", X#agent_auth.password =:= Pass]),
						qlc:e(QH)
					end,
					?assertMatch({atomic, []}, mnesia:transaction(F))
				end
			}
		]
	}.

-endif.