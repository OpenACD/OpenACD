%%%-------------------------------------------------------------------
%%% File          : agent_auth.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  12/2/08
%%%-------------------------------------------------------------------

%% @doc Connection to the local authenication cache and integration to another module.
%% Authentication is first checked by the integration module (if any).  If that fails, 
%% this module will fall back to it's local cache in the mnesica 'agent_auth' table.
%% the cache table is both ram and disc copies on all nodes.

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

%% @doc Starts with no integration, meaning all authentication requests are done against the mnesia cache.
start() -> 
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).
%% @doc Starts with no integration, meaning all authentication requests are done against the mnesia cache.
start_link() -> 
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).
%% @doc Starts with integration, meaning all authentication requests are done first against the mnesia cache only if the integration fails.
%% Mod is the integration module.  When this startes, Mod:StartFunc is called as the last step of this modules startup.
%% When an authenticaiton request is made, Mod:CheckFunc is called with the Username, Password, and Salt prepended to the CheckArgs list.
%% See also @see auth/3
-spec(start/5 :: (Mod :: atom(), StartFunc :: atom(), StartArgs :: [any()], CheckFunc :: atom(), CheckArgs :: [any()]) -> 'ok' | any()).
start(Mod, StartFunc, StartArgs, CheckFunc, CheckArgs) -> 
	gen_server:start({local, ?MODULE}, ?MODULE, [Mod, StartFunc, StartArgs, CheckFunc, CheckArgs], []).
	
%% @doc Starts with integration, meaning all authentication requests are done first against the mnesia cache only if the integration fails.
%% Mod is the integration module.  When this startes, Mod:StartFunc is called as the last step of this modules startup.
%% When an authenticaiton request is made, Mod:CheckFunc is called with the Username, Password, and Salt prepended to the CheckArgs list.
%% @see auth/3
-spec(start_link/5 :: (Mod :: atom(), StartFunc :: atom(), StartArgs :: [any()], CheckFunc :: atom(), CheckArgs :: [any()]) -> 'ok' | any()).
start_link(Mod, StartFunc, StartArgs, CheckFunc, CheckArgs) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Mod, StartFunc, StartArgs, CheckFunc, CheckArgs], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

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
				{allow, CachePassword, Skills} -> 
					cache(Username, CachePassword, Skills),
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
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @doc Base authentication function.  Does the appropariate gen_server:call.  Username, Password, and Salt are all expected to be lists.
%% Password is an md5 hash of the salt and the md5 hash of the plain text password.  In pseudo code, password is
%%
%% <pre>md5(Salt + md5(password))</pre>
%%
%% When integration is enabled, the integration module should return {allow, CachePassword, [Skill]} so that the local authentication
%% cache can store a propperly hashed password.  Local passwords are stored in a lower cased md5 hash.
%% 
%% If integration returns anything other than {allow, CachePassword, [Skill]} or 'deny', agent_auth falls back on it's mnesia cache.
%% If that fails, deny is returned in all cases.
%% 
%% The magic skills of '_agent' and '_node' are automcatically appeneded to the skill list, and therefore do not need to be stored on the 
%% integration side.
%% @see local_auth/3.
-spec(auth/3 :: (Username :: string(), Password :: string(), Salt :: string()) -> {'allow', [atom()]} | 'deny').
auth(Username, Password, Salt) -> 
	gen_server:call(?MODULE, {authentication, Username, Password, Salt}).

%% @doc Starts mnesia and creates the tables.  If the tables already exist, returns ok.
-spec(build_tables/0 :: () -> 'ok').
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

%% @doc Caches the passed Username, Password, and Skills to the mnesia database.  Username is the plaintext name and used as the key. 
%% Password is assumed to be prehashed either from erlang:md5 or as a string (list).  Skills do not need to (and in fact should not)
%% include the magic skills of '_agent' or '_node'.
-spec(cache/3 ::	(Username :: string(), Password :: string(), Skills :: [atom()]) -> {'atomic', 'ok'} | {'aborted', any()};
					(Username :: string(), Password :: binary(), Skills :: [atom()]) -> {'atomic', 'ok'} | {'aborted', any()}).
cache(Username, Password, Skills) when is_binary(Password) -> 
	cache(Username, util:bin_to_hexstr(Password), Skills);
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

%% @doc Removes the passed user from the local cache.  Called when integration returns a deny.
-spec(destroy/1 :: (Username :: string()) -> {'atomic', 'ok'} | {'aborted', any()}).
destroy(Username) -> 
	F = fun() -> 
		mnesia:delete({agent_auth, Username})
	end,
	mnesia:transaction(F).

%% @doc Checks the Username and prehashed Password using the given Salt for the chached password.
%% internally called by the auth callback; there should be no need to call this directly.
-spec(local_auth/3 :: (Username :: string(), Password :: string(), Salt :: string()) -> {'allow', [atom()]} | 'deny').
local_auth(Username, Password, Salt) -> 
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
			case salt(Agent#agent_auth.password, Salt) of
				Password -> 
					{allow, lists:append([Agent#agent_auth.skills, ['_agent', '_node']])};
				Else -> 
					io:format("Password: ~p~nSalting: ~p~nPresalting: ~p~nAgentname: ~p~n", [Password, Else, util:bin_to_hexstr(Agent#agent_auth.password), Agent#agent_auth.login]),
					deny
			end;
		Else ->
			io:format("Unusual respnse from local auth query: ~p~n", [Else]),
			deny
	end. 

%% @doc Apply the passed Salt to the passed Hast.  The Hash is convered from a bin if need be, lowercased, then appended to the Salt.
%% all this is the erlang:md5'ed.  That result is then turned into a list, and lowercased.
-spec(salt/2 :: (Hash :: binary(), Salt :: string()) -> string();
				(Hash :: string(), Salt :: string()) -> string()).
salt(Hash, Salt) when is_binary(Hash) ->
	io:format("agent_auth hash conversion...~n"),
	salt(util:bin_to_hexstr(Hash), Salt);
salt(Hash, Salt) when is_list(Hash) -> 
	io:format("agent_auth salting~n     hash: ~p~n     salt: ~p~n", [Hash, Salt]),
	Lower = string:to_lower(Hash),
	string:to_lower(util:bin_to_hexstr(erlang:md5(Salt ++ Lower))).
	
%% @doc Duh.
stop() -> 
	gen_server:call(?MODULE, stop).

-ifdef(EUNIT).

%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------

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
				"Salting a Binary",
				fun() -> 
					Salt = "salt",
					Hash = erlang:md5("test"),
					?assertMatch("fa148970883f467111816d22cf840613", salt(Hash, Salt))
				end
			},
			{
				"Salting a Hex",
				fun() ->
					Salt = "salt",
					Hash = util:bin_to_hexstr(erlang:md5("test")),
					?assertMatch("fa148970883f467111816d22cf840613", salt(Hash, Salt))
				end
			},
			{
				"Cache a user with bin encoded password",
				fun() -> 
					cache("A", erlang:md5("B"), [testskill]),
					F = fun() -> 
						QH = qlc:q([X || X <- mnesia:table(agent_auth), X#agent_auth.login =:= "A"]),
						qlc:e(QH)
					end,
					{atomic, [Agent]} = mnesia:transaction(F),
					?assertMatch("9d5ed678fe57bcca610140957afab571", Agent#agent_auth.password),
					destroy("A")
				end
			},
			{
				"Cache a user with a hex encoded password in caps",
				fun() ->
					cache("A", util:bin_to_hexstr(erlang:md5("B")), [testskill]),
					F = fun() -> 
						QH = qlc:q([X || X <- mnesia:table(agent_auth), X#agent_auth.login =:= "A"]),
						qlc:e(QH)
					end,
					{atomic, [Agent]} = mnesia:transaction(F),
					?assertMatch("9d5ed678fe57bcca610140957afab571", Agent#agent_auth.password),
					destroy("A")
				end
			},
			{
				"Auth a user 'A'",
				fun() -> 
					cache("A", erlang:md5("B"), [testskill]),
					Salt = "12345",
					SaltedPassword = salt(erlang:md5("B"), Salt),
					?assertMatch({allow, [testskill, '_agent', '_node']}, local_auth("A", SaltedPassword, Salt)),
					destroy("A"),
					?assertMatch(deny, local_auth("A", erlang:md5(integer_to_list(5) ++ erlang:md5("B")), 5))
				end
			},
			{
				"Auth a user 'A' with integer_to_list(salt)",
				fun() -> 
					cache("A", erlang:md5("B"), [testskill]),
					Salt = 123,
					SaltedPassword = salt(erlang:md5("B"), integer_to_list(Salt)),
					?assertMatch({allow, [testskill, '_agent', '_node']}, local_auth("A", SaltedPassword, integer_to_list(Salt))),
					destroy("A"),
					?assertMatch(deny, local_auth("A", SaltedPassword, integer_to_list(Salt)))
				end
			},
			{
				"Destory a user 'A'",
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