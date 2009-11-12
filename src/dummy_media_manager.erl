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

-module(dummy_media_manager).
-author(micahw).

-behaviour(gen_server).

-include("log.hrl").
-include("cpx.hrl").
-include("call.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
	start_link/1,
	start_supervised/1,
	stop/0,
	stop_supped/0,
	set_option/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(conf, {
	call_frequency = {distribution, 110} :: any(),
	call_max_life = {distribution, 900} :: any(),
	queues = any :: any()
}).
-record(state, {
	conf = #conf{} :: #conf{},
	calls = [] :: [{pid(), string()}],
	timer :: any()
}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

start_supervised(Options) ->
	Conf = #cpx_conf{
		id = ?MODULE,
		module_name = ?MODULE,
		start_function = start_link,
		start_args = [Options]
	},
	cpx_middle_supervisor:add_with_middleman(management_sup, 3, 5, Conf).

stop() ->
	gen_server:cast(?MODULE, {stop, normal}).

stop_supped() ->
	cpx_middle_supervisor:drop_child(management_sup, dummy_media_manager).

set_option(Key, Valu) ->
	gen_server:cast(?MODULE, {set_option, Key, Valu}).
	
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%--------------------------------------------------------------------
init(Options) ->
	process_flag(trap_exit, true),
	Conf = #conf{
		call_frequency = proplists:get_value(call_frequency, Options, {distribution, 110}),
		call_max_life = proplists:get_value(call_max_life, Options, {distribution, 900}),
		queues = proplists:get_value(queues, Options, any)
	},
	Seeds = proplists:get_value(start_count, Options, 10),
	Fun = fun(_, Acc) ->
		Pid = queue_media(Conf),
		#call{id = Id} = gen_media:get_call(Pid),
		[{Pid, Id} | Acc]
	end,
	Pidlist = lists:foldl(Fun, [], lists:seq(1, Seeds)),
	Time = util:get_number(Conf#conf.call_frequency) * 1000,
	?INFO("Spawning new call after ~p", [Time]),
	Self = self(),
    {ok, #state{
		calls = Pidlist,
		conf = Conf,
		timer = erlang:send_after(Time, ?MODULE, spawn_call)
	}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------
handle_call(get_count, _, #state{calls = Pidlist} = State) ->
	{reply, {ok, length(Pidlist)}, State};
handle_call({get_call, Needle}, _, #state{calls = Pidlist} = State) ->
	Out = case is_pid(Needle) of
		true ->
			case proplists:get_value(Needle, Pidlist) of
				undefined ->
					undefined;
				Else ->
					{ok, {Needle, Else}}
			end;
		false ->
			case find_key(Needle, Pidlist) of
				{ok, Key} ->
					{ok, {Key, Needle}};
				undefined ->
					undefined
			end
	end,
	{reply, Out, State};
handle_call(Request, _From, State) ->
    {reply, {invalid, Request}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_cast({set_option, Option, Arg}, #state{conf = Conf} = State) ->
	Newconf = case Option of
		call_frequency ->
			Conf#conf{call_frequency = Arg};
		call_max_life ->
			Conf#conf{call_max_life = Arg};
		queues ->
			Conf#conf{queues = Arg};
		_Else ->
			?WARNING("unknown option ~p", [Option]),
			Conf
	end,
	{noreply, State#state{conf = Newconf}};
handle_cast({stop, Why}, State) when Why =:= normal orelse Why =:= shutdown ->
	{stop, Why, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_info(spawn_call, #state{calls = Pidlist, conf = Conf} = State) ->
	Newpid = queue_media(State#state.conf),
	#call{id = Newid} = gen_media:get_call(Newpid),
	Time = util:get_number(Conf#conf.call_frequency) * 1000,
	?INFO("Spawning new call after ~p", [Time]),
	Timer = erlang:send_after(Time, ?MODULE, spawn_call),
	{noreply, State#state{calls = [{Newpid, Newid} | Pidlist], timer = Timer}};
handle_info({'EXIT', Reason, Pid}, #state{calls = Pidlist} = State) ->
	case proplists:get_value(Pid, Pidlist) of
		undefined ->
			{noreply, State};
		Id ->
			cpx_monitor:drop({media, Id}),
			Newlist = proplists:delete(Pid, Pidlist),
			{noreply, State#state{calls = Newlist}}
	end;
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(_Reason, #state{calls = Pidlist}) ->
	Fun = fun({_Pid, Id}) ->
		cpx_monitor:drop({media, Id})
	end,
	lists:foreach(Fun, Pidlist),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

queue_media(Conf) ->
	[Pid] = dummy_media:q_x(1, [{queues, Conf#conf.queues}, {max_life, util:get_number(Conf#conf.call_max_life)}]),
	Pid.

find_key(Needle, []) ->
	undefined;
find_key(Needle, [{Key, Needle} | _]) ->
	{ok, Key};
find_key(Needle, [_ | Tail]) ->
	find_key(Needle, Tail).