-module(cpxlog_terminal).
-behaviour(gen_event).
-export([
	init/1,
	handle_event/2,
	handle_call/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-define(LOGLEVELS, [
	debug,
	info,
	notice,
	warning,
	error,
	critical,
	alert,
	emergency
]).

-record(state, {
	level = info,
	lasttime
}).

init(_Args) ->
	{ok, #state{lasttime = erlang:localtime()}}.

handle_event({Level, Time, Module, Line, Pid, Message, Args}, State) ->
	case (element(3, element(1, Time)) =/= element(3, element(1, State#state.lasttime))) of
		true ->
			io:format("Day changed from ~p to ~p~n", [element(1, State#state.lasttime), element(1, Time)]);
		false ->
			ok
	end,
	case (lists:member(Level, ?LOGLEVELS) andalso (util:list_index(Level, ?LOGLEVELS) >= util:list_index(State#state.level, ?LOGLEVELS))) of
		true ->
			io:format("~w:~w:~w [~s] ~w@~s:~w ~s~n", [
					element(1, element(2, Time)),
					element(2, element(2, Time)),
					element(3, element(2, Time)),
					string:to_upper(atom_to_list(Level)),
					Pid, Module, Line,
					io_lib:format(Message, Args)]);
		false ->
			ok
	end,
	{ok, State#state{lasttime = Time}};
handle_event({set_log_level, Level}, State) ->
	case lists:member(Level, ?LOGLEVELS) of
		true ->
			io:format("Loglevel changed to: ~s~n", [string:to_upper(atom_to_list(Level))]),
			{ok, State#state{level = Level}};
		false ->
			io:format("Invalid loglevel: ~s~n", [string:to_upper(atom_to_list(Level))]),
			{ok, State}
	end;
handle_event(_Event, State) ->
	{ok, State}.

handle_call(_Request, State) ->
	{ok, ok, State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(_Args, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
