%%      The contents of this file are subject to the Common Public Attribution
%%      License Version 1.0 (the “License”); you may not use this file except
%%      in compliance with the License. You may obtain a copy of the License at
%%      http://opensource.org/licenses/cpal_1.0. The License is based on the
%%      Mozilla Public License Version 1.1 but Sections 14 and 15 have been
%%      added to cover use of software over a computer network and provide for
%%      limited attribution for the Original Developer. In addition, Exhibit A
%%      has been modified to be consistent with Exhibit B.
%%
%%      Software distributed under the License is distributed on an “AS IS”
%%      basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%      License for the specific language governing rights and limitations
%%      under the License.
%%
%%      The Original Code is OpenACD.
%%
%%      The Initial Developers of the Original Code is
%%      Andrew Thompson and Micah Warren.
%%
%%      All portions of the code written by the Initial Developers are Copyright
%%      (c) 2008-2009 SpiceCSM.
%%      All Rights Reserved.
%%
%%      Contributor(s):
%%
%%      Andrew Thompson <andrew at hijacked dot us>
%%      Micah Warren <micahw at lordnull dot com>
%%

-module(sipxplugin_poller).
-author("eZuce").

-import(mongoapi).
-import(application).
-import(agent_auth).
-import(call_queue_config).
-import(queue_manager).
-import(cpx_supervisor).
-export([start/0, stop/0, init/1, loop/2]).

-include("../../../include/log.hrl").
-include("../../../include/cpx.hrl").

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	{ok, start_poller()}.

start_poller() ->
	mongodb:singleServer(def),
	mongodb:connect(def),
	init_poller(10000).

% @doc Spawn a poller process
%
% @spec init(PollInterval::integer())

init_poller(PollInterval) ->
    spawn_link(
		sipxplugin_poller, loop, [PollInterval, 0]
	).

loop(PollInterval, LastPollTime) ->
	receive
		exit -> ok
	after
		PollInterval ->
			{NewPollTime} =
				get_new_config(LastPollTime),
			loop(PollInterval, NewPollTime)
    end.

get_new_config(LastPollTime) ->
	NewPollTime = calendar:datetime_to_gregorian_seconds(
		{ date(), time() }
	),
	%connect to openacd db and count objects in commands collection
	Mong = mongoapi:new(def,<<"openacd">>),
	CommandCount = Mong:count("commands"),
	if CommandCount =:= 0 -> ?WARNING("No Command to execute", []);
		true ->
			%if command count > 0 retrieve all commands and process them
			?WARNING("No of Commands to execute ~p", [CommandCount]),
			{Status, Commands} = Mong:find("commands", [], undefined, 0, CommandCount),
			lists:foreach(fun(Cmd) ->
				get_command_values(Cmd, Mong)
			end, Commands)
	end,
    { NewPollTime }.

get_command_values(Data, Mong) ->
	if Data =:= [] -> ?DEBUG("No Data", []);
		true ->
			% command format { "_id" : ObjectId("4ce62e892957ca4fc97387a1"), "command" : "ADD", "count" : 2, "objects" : []}
			[{_, Id}, {_, CmdValue}, {_, Count}, {_, {_, Objects}}] = Data,
			lists:foreach(fun(Object) ->
				% objects to process starts with type e.g. "type" : "agent", "name" : "bond", "pin" : "1234"
				{_, Type} = lists:nth(1, Object),
				if Type =:= <<"agent">> ->
					process_agent(Object, erlang:binary_to_list(CmdValue));
				Type =:= <<"profile">> ->
					process_profile(Object, erlang:binary_to_list(CmdValue));
				Type =:= <<"skill">> ->
					process_skill(Object, erlang:binary_to_list(CmdValue));
				Type =:= <<"client">> ->
					process_client(Object, erlang:binary_to_list(CmdValue));
				Type =:= <<"queueGroup">> ->
					process_queue_group(Object, erlang:binary_to_list(CmdValue));
				Type =:= <<"queue">> ->
					process_queue(Object, erlang:binary_to_list(CmdValue));
				Type =:= <<"freeswitch_media_manager">> ->
					process_fs_media_manager(Object, erlang:binary_to_list(CmdValue));
				Type =:= <<"agent_configuration">> ->
					process_agent_configuration(Object, erlang:binary_to_list(CmdValue));
				true -> ?WARNING("Unrecognized type", [])
				end
			end, Objects),
			Mong:runCmd([{"findandmodify", "commands"},{"query", [{"_id",Id}]},{"remove",1}])
	end.

process_agent(Agent, Command) ->
	{_, Name} = lists:nth(2, Agent),
	{_, Pin} = lists:nth(3, Agent),
	{_, Group} = lists:nth(4, Agent),
	{_, Skills} = lists:nth(5, Agent),
	{_, Queues} = lists:nth(6, Agent),
	{_, Clients} = lists:nth(7, Agent),
	{_, Firstname} = lists:nth(8, Agent),
	{_, Lastname} = lists:nth(9, Agent),
	{_, Security} = lists:nth(11, Agent),
	SkillsList = lists:flatmap(fun(X)->[list_to_atom(X)] end, string:tokens((erlang:binary_to_list(Skills)), ", ")),
	QueuesList = lists:flatmap(fun(X)->[{'_queue',X}] end, string:tokens((erlang:binary_to_list(Queues)), ", ")),
        ClientsList = lists:flatmap(fun(X)->[{'_brand',X}] end, string:tokens((erlang:binary_to_list(Clients)), ", ")),
        AllSkills = lists:merge(lists:merge(QueuesList, ClientsList), SkillsList),
	if Security =:= <<"SUPERVISOR">> ->
		SecurityAtom = supervisor;
	Security =:= <<"ADMIN">> ->
		SecurityAtom = admin;
	true -> SecurityAtom = agent
	end,
	if Command =:= "ADD" ->
		agent_auth:add_agent(erlang:binary_to_list(Name), erlang:binary_to_list(Firstname), erlang:binary_to_list(Lastname), erlang:binary_to_list(Pin), AllSkills, SecurityAtom, erlang:binary_to_list(Group));
	Command =:= "DELETE" ->
		agent_auth:destroy(erlang:binary_to_list(Name));
	Command =:= "UPDATE" ->
		{_, Oldname} = lists:nth(10, Agent),
		{_, [Old]} = agent_auth:get_agent(erlang:binary_to_list(Oldname)),
		agent_auth:set_agent(element(2, Old), erlang:binary_to_list(Name), erlang:binary_to_list(Pin), AllSkills, SecurityAtom, erlang:binary_to_list(Group), erlang:binary_to_list(Firstname), erlang:binary_to_list(Lastname));
	true -> ?WARNING("Unrecognized command", [])
	end.

process_profile(Profile, Command) ->
	{_, Name} = lists:nth(2, Profile),
	{_, Skills} = lists:nth(3, Profile),
        {_, Queues} = lists:nth(4, Profile),
        {_, Clients} = lists:nth(5, Profile),
        SkillsList = lists:flatmap(fun(X)->[list_to_atom(X)] end, string:tokens((erlang:binary_to_list(Skills)), ", ")),
	QueuesList = lists:flatmap(fun(X)->[{'_queue',X}] end, string:tokens((erlang:binary_to_list(Queues)), ", ")),
	ClientsList = lists:flatmap(fun(X)->[{'_brand',X}] end, string:tokens((erlang:binary_to_list(Clients)), ", ")),
	AllSkills = lists:merge(lists:merge(QueuesList, ClientsList), SkillsList),
	if Command =:= "ADD" ->
		agent_auth:new_profile(erlang:binary_to_list(Name), AllSkills);
	Command =:= "DELETE" ->
		agent_auth:destroy_profile(erlang:binary_to_list(Name));
	Command =:= "UPDATE" ->
		{_, Oldname} = lists:nth(6, Profile),
		Old = agent_auth:get_profile(erlang:binary_to_list(Oldname)),
		agent_auth:set_profile(erlang:binary_to_list(Oldname), erlang:binary_to_list(Name), AllSkills);
	true -> ?WARNING("Unrecognized command", [])
	end.

process_skill(Skill, Command) ->
	{_, Name} = lists:nth(2, Skill),
	{_, Atom} = lists:nth(3, Skill),
	{_, Group} = lists:nth(4, Skill),
	{_, Description} = lists:nth(5, Skill),
	if Description =:= null ->
		Descr = "";
	true -> Descr = erlang:binary_to_list(Description)
	end,
	if Command =:= "ADD" ->
		call_queue_config:new_skill(list_to_atom(erlang:binary_to_list(Atom)), erlang:binary_to_list(Name), Descr, erlang:binary_to_list(Group));
	Command =:= "DELETE" ->
		call_queue_config:destroy_skill(erlang:binary_to_list(Name));
	Command =:= "UPDATE" ->
		call_queue_config:set_skill(list_to_atom(erlang:binary_to_list(Atom)), erlang:binary_to_list(Name), Descr, erlang:binary_to_list(Group));
	true -> ?WARNING("Unrecognized command", [])
	end.

process_client(Client, Command) ->
	{_, Name} = lists:nth(2, Client),
	{_, Identity} = lists:nth(3, Client),
	if Command =:= "ADD" ->
		call_queue_config:new_client(erlang:binary_to_list(Name), erlang:binary_to_list(Identity), []);
	Command =:= "DELETE" ->
		call_queue_config:destroy_client(erlang:binary_to_list(Identity));
	Command =:= "UPDATE" ->
		call_queue_config:set_client(erlang:binary_to_list(Identity), erlang:binary_to_list(Name), []);
	true -> ?WARNING("Unrecognized command", [])
	end.

process_queue_group(QueueGroup, Command) ->
	{_, Name} = lists:nth(2, QueueGroup),
	{_, Skills} = lists:nth(3, QueueGroup),
	{_, Profiles} = lists:nth(4, QueueGroup),
	{_, Sort} = lists:nth(5, QueueGroup),
	SkillsList = lists:flatmap(fun(X)->[list_to_atom(X)] end, string:tokens((erlang:binary_to_list(Skills)), ", ")),
	if Command =:= "ADD" ->
		call_queue_config:new_queue_group(erlang:binary_to_list(Name), binary_to_number(Sort), []);
	Command =:= "DELETE" ->
		call_queue_config:destroy_queue_group(erlang:binary_to_list(Name));
	Command =:= "UPDATE" ->
		{_, Oldname} = lists:nth(6, QueueGroup),
		{_, [{_, _, OldRecipe, _, _, _}]} = call_queue_config:get_queue_group(erlang:binary_to_list(Oldname)),
		call_queue_config:set_queue_group(erlang:binary_to_list(Oldname), erlang:binary_to_list(Name), binary_to_number(Sort), OldRecipe);
	true -> ?WARNING("Unrecognized command", [])
	end.

process_queue(Queue, Command) ->
	{_, Name} = lists:nth(2, Queue),
	{_, QueueGroup} = lists:nth(3, Queue),
	{_, Skills} = lists:nth(4, Queue),
	{_, Profiles} = lists:nth(5, Queue),
	SkillsList = lists:flatmap(fun(X)->[list_to_atom(X)] end, string:tokens((erlang:binary_to_list(Skills)), ", ")),
	ProfilesList = lists:flatmap(fun(X)->[{'_profile',X}] end, string:tokens((erlang:binary_to_list(Profiles)), ", ")),
	AllSkills = lists:merge(SkillsList, ProfilesList),
	{_, Weight} = lists:nth(6, Queue),
	if Command =:= "ADD" ->
		call_queue_config:new_queue(erlang:binary_to_list(Name), binary_to_number(Weight), AllSkills, [], erlang:binary_to_list(QueueGroup)),
		queue_manager:load_queue(erlang:binary_to_list(Name));
	Command =:= "DELETE" ->
		call_queue_config:destroy_queue(erlang:binary_to_list(Name));
	Command =:= "UPDATE" ->
		{_, Oldname} = lists:nth(7, Queue),
		OldRecipe = element(5, call_queue_config:get_queue(erlang:binary_to_list(Oldname))),
		call_queue_config:set_queue(erlang:binary_to_list(Oldname), erlang:binary_to_list(Name), binary_to_number(Weight), AllSkills, OldRecipe, erlang:binary_to_list(QueueGroup));
	true -> ?WARNING("Unrecognized command", [])
	end.

process_fs_media_manager(Config, Command) ->
        {_, Enabled} = lists:nth(2, Config),
        {_, CNode} = lists:nth(3, Config),
        {_, DialString} = lists:nth(4, Config),
        if Enabled =:= <<"true">> ->
		Conf = #cpx_conf{id = freeswitch_media_manager, module_name = freeswitch_media_manager, start_function = start_link, start_args = [list_to_atom(erlang:binary_to_list(CNode)), [{h323,[]}, {iax2,[]}, {sip,[]}, {dialstring,erlang:binary_to_list(DialString)}]], supervisor = mediamanager_sup},
                cpx_supervisor:update_conf(freeswitch_media_manager, Conf);
        Enabled =:= <<"false">> ->
                cpx_supervisor:destroy(freeswitch_media_manager);
        true -> ?WARNING("Unrecognized command", [])
        end.

process_agent_configuration(Config, Command) ->
        {_, ListenerEnabled} = lists:nth(2, Config),
        if ListenerEnabled =:= <<"true">> ->
		Conf = #cpx_conf{id = agent_dialplan_listener, module_name = agent_dialplan_listener, start_function = start_link, start_args = [], supervisor = agent_connection_sup},
                cpx_supervisor:update_conf(agent_dialplan_listener, Conf);
        ListenerEnabled =:= <<"false">> ->
                cpx_supervisor:destroy(agent_dialplan_listener);
        true -> ?WARNING("Unrecognized command", [])
        end.


binary_to_number(B) ->
    list_to_number(binary_to_list(B)).

list_to_number(L) ->
    try list_to_float(L)
    catch
        error:badarg ->
            list_to_integer(L)
    end.
