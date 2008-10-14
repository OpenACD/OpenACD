-module(agent_manager).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-export([start_link/0, start/0, stop/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("call.hrl").
-include("agent.hrl").

-spec(start_link/0 :: () -> 'ok').
start_link() -> 
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	
-spec(start/0 :: () -> 'ok').
start() -> 
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

-spec(stop/0 :: () -> 'ok').
stop() ->
	gen_server:call(?MODULE, stop).

init([]) -> 
	process_flag(trap_exit, true),
	case global:whereis_name(?MODULE) of
		undefined ->
				global:register_name(?MODULE, self(), {global, random_notify_name});
		GID -> 
			link(GID)
		end,
	{ok, dict:new()}.

handle_call({start_agent, #agent{login=Login} = Agent}, _From, State) ->
	% starts a new agent and returns the state of that agent.
	case dict:is_key(Login, State) of 
		true -> 
			{reply, {exists, dict:find(Login, State)}, State};
		false -> 
			Self = self(),
			{ok, Pid} = agent:start(Agent),
			case global:whereis_name(?MODULE) of
				Self -> 
					{reply, {ok, Pid}, dict:store(Login, Pid, State)};
				undefined -> 
					global:register_name(?MODULE, self(), {global, random_notify_name}),
					{replay, {ok, Pid}, dict:store(Login, Pid, State)};
				_ -> 
					try gen_server:call({global, ?MODULE}, {exists, Login}) of 
						true ->
							{ok, Val} = dict:find(Login, State),
							{reply, {ok, Val}, State};
						false -> 
							gen_server:call({global, ?MODULE}, {notify, Login, Pid}), % like the queue manager, handle a timeout.
							{reply, {ok, Pid}, dict:store(Login, Pid, State)}
					catch
						exit:{timeout, _} -> 
							global:register_name(?MODULE, self(), {global, random_notify_name}),
							{reply, timeout, dict:append(Login, Pid, State)}
					end
			end
	end;

handle_call({exists, Login}, _From, State) ->
	dict:is_key(Login, State);

handle_call({notify, Login, Pid}, _From, State) -> 
	{reply, ok, dict:append(Login, Pid, State)};

handle_call({get_process, Login}, _From, State) -> 
	case dict:is_key(Login, State) of 
		true -> 
			{reply, dict:find(Login, State), State};
		false -> 
			{reply, no_process, State}
	end;

handle_call(Request, _From, State) ->
	{reply, {unknown_call, Request}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-ifdef('EUNIT').

-endif.

