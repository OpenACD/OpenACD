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
-export([start_link/5, start/5, start_link/0, start/0, auth/2]).

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
	build_tables(),
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start_link() -> 
	build_tables(),
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start(Mod, StartFunc, StartArgs, CheckFunc, CheckArgs) -> 
	build_tables(),
	gen_server:start({local, ?MODULE}, ?MODULE, [Mod, StartFunc, StartArgs, CheckFunc, CheckArgs], []).
start_link(Mod, StartFunc, StartArgs, CheckFunc, CheckArgs) ->
    build_tables(),
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
	apply(Mod, StartFunc, StartArgs),
    {ok, #state{mod=Mod, start_func=StartFunc, start_args=StartArgs, check_func = CheckFunc, check_args = CheckArgs}};
init([]) -> 
	{ok, #state{integration = false}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({authentication, Username, Password}, _From, State) -> 
	% start w/ the remote try.  If that fails, try the local.
	case State#state.integration of
		false -> 
			io:format("local authentication only~n"),
			Reply = local_auth(Username, Password),
			{reply, Reply, State};
		_Else -> 
			io:format("remote authenitcaitona attempt first~n"),
			Args = lists:append([[Username, Password], State#state.check_args]),
			case apply(State#state.mod, State#state.check_func, Args) of
				{allow, Skills} -> 
					cache(Username, Password, Skills),
					{reply, {allow, Skills}, State};
				deny -> 
					{reply, deny, State};
				_Else -> 
					Reply = local_auth(Username, Password),
					{reply, Reply, State}
			end
	end;
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

auth(Username, Password) -> 
	gen_server:call(?MODULE, {authentication, Username, Password}).

build_tables() ->
	io:format("building tables...~n"),
	mnesia:start(),
	A = mnesia:create_table(agent_auth, [{attributes, record_info(fields, agent_auth)}]),
	io:format("~p~n", [A]),
	ok.
	
cache(Username, Password, Skills) -> 
	ok.
	
local_auth(Username, Password) -> 
	P = erlang:md5(Password),
	QH = qlc:q([X || X <- mnesia:table(agent_auth), X#agent_auth.login =:= Username, X#agent_auth.password =:= P]),
	io:format("QH: ~p~n", [QH]),
	F = fun() -> qlc:e(QH) end,
	case mnesia:transaction(F) of
		%{error, mnesia, {aborted, {no_exists, agent_auth}}} -> 
		%	mnesia:create_table(agent_auth, record_info(fields, agent_auth)),
		%	deny;
		{atomic, []} -> 
			deny;
		{atomic, [Agent]} when is_record(Agent, agent_auth) -> 
			{allow, Agent#agent_auth.skills};
		Else ->
			io:format("Unusual respnse from lcal auth query: ~p~n", [Else]),
			deny
	end.
			