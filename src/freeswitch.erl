%% @doc A module for interfacing with FreeSWITCH using mod_erlang_event.
-module(freeswitch).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([send/2, start_log_handler/1, start_event_handler/1, api/3, bgapi/3, event/2, nixevent/2, noevents/1, close/1, get_event_header/2, get_event_body/1, get_event_name/1, log_receiver_loop/1, event_receiver_loop/1, start_search/1, search_loop/1]).
-define(TIMEOUT, 10000).

%% @doc Return the value for a specific header in an event or `{error,notfound}'.
get_event_header([], _Needle) ->
	{error, notfound};
get_event_header({event, Headers}, Needle) when is_list(Headers) ->
	get_event_header(Headers, Needle);
get_event_header([{Key,Value} | Headers], Needle) ->
	case Key of
		Needle ->
			Value;
		_ ->
			get_event_header(Headers, Needle)
	end.

%% @doc Return the name of the event.
get_event_name(Event) ->
	get_event_header(Event, "Event-Name").

%% @doc Return the body of the event or `{error, notfound}' if no event body.
get_event_body(Event) ->
	get_event_header(Event, "body").


%% @doc send some raw crap to FreeSWITCH. Returns the reply or `timeout' on a
%% timeout.
send(Node, Term) ->
	{send, Node} ! Term,
	receive
		Response ->
			Response
	after ?TIMEOUT ->
		timeout
	end.

%% @doc Make a blocking API call to FreeSWITCH. The result of the API is
%returned or `timeout' if FreeSWITCH fails to respond.
api(Node, Cmd, Args) ->
	{api, Node} ! {api, Cmd, Args},
	receive
		X -> X
	after ?TIMEOUT ->
		timeout
	end.

%% @doc Make a backgrounded API call to FreeSWITCH. The asynchronous reply is
%% sent to calling process after it is received. This function
%% returns the result of the initial bgapi call or `timeout' if FreeSWITCH fails
%% to respond.
bgapi(Node, Cmd, Args) ->
	Self = self(),
	% spawn a new process so that both responses go here instead of directly to
	% the calling process.
	spawn(fun() ->
		{bgapi, Node} ! {bgapi, Cmd, Args},
		receive
			{error, Reason} ->
				% send the error condition to the calling process
				Self ! {api, {error, Reason}};
			{ok, JobID} ->
				% send the reply to the calling process
				Self ! {api, ok},
				receive % wait for the job's reply
					{bgok, JobID, Reply} ->
						% send the actual command output back to the calling process
						Self ! {bgok, Reply};
					{bgerror, JobID, Reply} ->
						Self ! {bgerror, Reply}
				end
		after ?TIMEOUT ->
			% send a timeout to the calling process
			Self ! {api, timeout}
		end
	end),

	% get the initial result of the command, NOT the asynchronous response, and
	% return it
	receive
		{api, X} -> X
	end.

%% @doc Request to receive any events in the list `List'.
event(Node, Events) when is_list(Events) ->
	{event, Node} ! list_to_tuple(lists:append([event], Events)),
	receive
		X -> X
	after ?TIMEOUT ->
		timeout
	end;
event(Node, Event) when is_atom(Event) ->
	event(Node, [Event]).

%% @doc Stop receiving any events in the list `Events'.
nixevent(Node, Events) when is_list(Events) ->
	{nixevent, Node} ! list_to_tuple(lists:append([nixevent], Events)),
	receive
		X -> X
	after ?TIMEOUT ->
		timeout
	end;
nixevent(Node, Event) when is_atom(Event) ->
	nixevent(Node, [Event]).

%% @doc Stop receiving any events from `Node'.
noevents(Node) ->
	{noevents, Node} ! noevents,
	receive
		X -> X
	after ?TIMEOUT ->
		timeout
	end.

%% @doc Close the connection to `Node'.
close(Node) ->
	{close, Node} ! exit,
	receive
		X -> X
	after ?TIMEOUT ->
		timeout
	end.

start_handler(Node, Type, Module, Function) ->
	Self = self(),
	spawn(fun() ->
		monitor_node(Node, true),
		{foo, Node} ! Type,
		receive
			ok ->
				Self ! {Type, {ok, self()}},
				apply(Module, Function, [Node]);
			{error,Reason} ->
				Self ! {Type, {error, Reason}}
		after ?TIMEOUT ->
				Self ! {Type, timeout}
		end
		end),
	
	receive
		{Type, X} -> X
	end.

start_log_handler(Node) ->
	start_handler(Node, register_log_handler, ?MODULE, log_receiver_loop).

start_event_handler(Node) ->
	start_handler(Node, register_event_handler, ?MODULE, event_receiver_loop).

start_search(Node) ->
	start_handler(Node, {bind, dialplan}, ?MODULE, search_loop).

log_receiver_loop(Node) ->
	receive
		{log, Log} ->
			io:format("got log message ~p~n", [Log]),
			log_receiver_loop(Node);
		{nodedown, Node} ->
			io:format("FreeSWITCH node is ~p down, log receiver process exiting~n", [Node]);
		Any ->
			io:format("log handler got unexpected message: ~p~n", [Any]),
			log_receiver_loop(Node)
	end.

event_receiver_loop(Node) ->
	receive
		{event, Event} ->
			io:format("got event ~p~n", [Event]),
			event_receiver_loop(Node);
		{nodedown, Node} ->
			io:format("FreeSWITCH node ~p is down, event receiver process exiting~n", [Node]);
		Any ->
			io:format("log handler got unexpected message: ~p~n", [Any]),
			event_receiver_loop(Node)
	end.

search_loop(Node) ->
	receive
		{fetch, dialplan, Tag, Key, Value, ID, _Data} ->
			io:format("got dialplan fetch Tag ~p and Key ~p = Value ~p with id ~p~n", [Tag, Key, Value, ID]),
			{wtf, Node} ! {fetch_reply, ID, "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><document type=\"freeswitch/xml\"><section name=\"dialplan\" description=\"Dynamic test\"><context name=\"public\"><extension name=\"erlang test\"><condition><action application=\"sleep\" data=\"5\"/><action application=\"hangup\" data=\"\"/></condition></extension></context></section></document>"},
			search_loop(Node);
		{fetch, Section, Tag, Key, Value, ID, _Data} ->
			io:format("got fetch for Section ~p, Tag ~p and Key ~p = Value ~p with id ~p~n", [Section, Tag, Key, Value, ID]),
			{wtf, Node} ! {fetch_reply, ID, "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n<document type=\"freeswitch/xml\">\n<section name=\"result\">\n<result status=\"not found\" />\n</section>\n</document>"},
			search_loop(Node);
		{nodedown, Node} ->
			io:format("FreeSWITCH node ~p is down, event receiver process exiting~n", [Node]);
		Any ->
			io:format("search handler got unexpected message: ~p~n", [Any]),
			search_loop(Node)
	end.


-ifdef(EUNIT).


-endif.
