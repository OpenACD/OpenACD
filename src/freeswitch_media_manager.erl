%%%-------------------------------------------------------------------
%%% File          : freeswitch_media_manager.erl
%%% Author        : Micah Warren
%%% Organization  : __MyCompanyName__
%%% Project       : cpxerl
%%% Description   : 
%%%
%%% Created       :  11/18/08
%%%-------------------------------------------------------------------
-module(freeswitch_media_manager).
-author("Micah").

-behaviour(gen_server).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("queue.hrl").
-include("call.hrl").

-define(TIMEOUT, 10000).

%% API
-export([start_link/1, start/1, listener/1, new_call/2, existing_call/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	nodename :: atom(),
	freeswitch_c_pid :: pid(),
	watched_calls % ets table of the call id's already queued.
	}).
	
% watched calls structure:
% callid, status

% notes for tomorrow:  set up a process per call id to watch/process that call id

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Nodename) -> 
	gen_server:start({local, ?MODULE}, ?MODULE, [Nodename], []).
start_link(Nodename) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Nodename], []).

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
init([Nodename]) -> 
	process_flag(trap_exit, true),
	Self = self(),
	_Lpid = spawn(fun() -> 
		{freeswitchnode, Nodename} ! register_event_handler,
		receive
			ok ->
				Self ! {register_event_handler, {ok, self()}},
				listener(Nodename);
			{error, Reason} -> 
				Self ! {register_event_handler, {error, Reason}}
		after ?TIMEOUT -> 
			Self ! {register_event_handler, timeout}
		end
	end),
	T = freeswitch:event(Nodename, all),
	io:format("ATtempted to start events in ffm's init:  ~p~n", [T]),
    {ok, #state{nodename=Nodename, watched_calls = ets:new(watched_calls, [named_table, public])}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({stop_watching, Callid}, _From, State) -> 
	ets:delete(watched_calls, Callid),
	{reply, ok, State};
handle_call({queue_call, Call, Queue, Priority}, _From, State) -> 
	io:format("queue call called."),
	case queue_manager:get_queue(Queue) of
		undefined ->
			io:format("unknownqueue~n"),
			{reply, unknownqueue, State};
		Qpid ->
			call_queue:add(Qpid, Priority, Call),
			io:format("found queue, updating fmm ets~n"),
			ets:update_element(watched_calls, Call#call.id, {2, queued}),
			{reply, ok, State}
	end;
handle_call({ring_agent, AgentPid, Call}, _From, State) ->
	%% @todo A real media manager would do something substantial here, like try to
	%% send SIP to an agent's phone or whatever. We should do something about
	%% ringout here though...
	{reply, agent:set_state(AgentPid, ringing, Call), State};
handle_call(Request, _From, State) ->
	io:format("Sudden call at fmm:  ~p~n", [Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------


handle_cast(Rawcall, State) when is_list(Rawcall) -> 
	% is this a call we need to worry about?
	io:format("raw call in~n"),
	case freeswitch:get_event_header(Rawcall, "Core-UUID") of
		{error, notfound} -> 
			% Hmmm, ignore it as there's no way to track it.
			io:format("fmm:  ignoring a call with no Core-UUID~n");
		Callid when is_list(Callid) -> 
			% look it up
			case ets:insert_new(watched_calls, {Callid, new}) of
				false -> 
					io:format("Call exists in table~n"),
					spawn(?MODULE, existing_call, [Rawcall, self()]),
					{noreply, State};
				_Otherwise -> 
					io:format("Attempting to parse call~n"),
					% because this takes a while, spawn it off to another process,
					% that process will rip the data appart, and then have this queue it up.
					spawn(?MODULE, new_call, [Rawcall, self()]),
					{noreply, State}
			end;
		_Otherwise -> 
			% another ignore error
			{noreply, State}
	end;
handle_cast(Msg, State) ->
	io:format("freeswitch media manager got cast:  ~p~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({register_event_handler, {ok, Pid}}, State) -> 
	{noreply, State#state{freeswitch_c_pid = Pid}};
handle_info({'EXIT', Pid, Reason}, State) -> 
	io:format("Trapping exit from ~p because of ~p.~n", [Pid, Reason]);
handle_info(Info, State) ->
	io:format("Sudden info at the fmm:  ~p~n", [Info]),
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

queue_call(Callrec, Queue, Priority) -> 
	gen_server:call(?MODULE, {queue_call, Callrec, Queue, Priority}).

existing_call(Rawcall, Spid) -> 
	io:format("handling a call we are already watching"),
	% rip out the call id and the message type (we'll need both)
	Callid = freeswitch:get_event_header(Rawcall, "Core_UUID"),
	ok.

new_call(Rawcall, Spid) -> 
	io:format("parsing...~n"),
	% rip out the core-uuid, variable_queue, variable_client
	% set the source, type, ring_path, and media_path
	Queue = freeswitch:get_event_header(Rawcall, "variable_queue"),
	Client = freeswitch:get_event_header(Rawcall, "variable-client"),
	Callid = freeswitch:get_event_header(Rawcall, "Core-UUID"),
	% properly set the client for actual implementation
	% for now, just spit back some bogus data
	% priority also needs to be held in that client data.
	% again, for now faking it.
	Priority = 1,
	Clientrec = #client{tenant=1, brand=2, label=Client},
	Callrec = #call{id = Callid, 
		type = voice,
		source = Spid,
		client = Clientrec,
		ring_path = outband,
		media_path = outband},
	queue_call(Callrec, Queue, Priority).
	
listener(Node) ->
	receive
		{event, Event} ->
			gen_server:cast(?MODULE, Event), 
			listener(Node);
		{nodedown, Node} -> 
			gen_server:cast(?MODULE, nodedown);
		 Otherwise -> 
			 io:format("Uncertain reply received by the fmm listener:  ~p~n", [Otherwise]),
			 listener(Node)
	end.