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
-export([start/0, stop/0, init/1, loop/2]).

-include("../../../include/log.hrl").

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
	{_, Firstname} = lists:nth(7, Agent),
	{_, Lastname} = lists:nth(8, Agent),
	{_, Security} = lists:nth(10, Agent),
	SkillsList = lists:flatmap(fun(X)->[list_to_atom(X)] end, string:tokens((erlang:binary_to_list(Skills)), ", ")),
	if Security =:= <<"SUPERVISOR">> ->
		SecurityAtom = supervisor;
	Security =:= <<"ADMIN">> ->
		SecurityAtom = admin;
	true -> SecurityAtom = agent
	end,
	if Command =:= "ADD" ->
		agent_auth:add_agent(erlang:binary_to_list(Name), erlang:binary_to_list(Firstname), erlang:binary_to_list(Lastname), erlang:binary_to_list(Pin), SkillsList, SecurityAtom, erlang:binary_to_list(Group));
	Command =:= "DELETE" ->
		agent_auth:destroy(erlang:binary_to_list(Name));
	Command =:= "UPDATE" ->
		{_, Oldname} = lists:nth(9, Agent),
		{_, [Old]} = agent_auth:get_agent(erlang:binary_to_list(Oldname)),
		agent_auth:set_agent(element(2, Old), erlang:binary_to_list(Name), erlang:binary_to_list(Pin), SkillsList, SecurityAtom, erlang:binary_to_list(Group), erlang:binary_to_list(Firstname), erlang:binary_to_list(Lastname));
	true -> ?WARNING("Unrecognized command", [])
	end.

process_profile(Profile, Command) ->
	{_, Name} = lists:nth(2, Profile),
	{_, Skills} = lists:nth(3, Profile),
	SkillsList = lists:flatmap(fun(X)->[list_to_atom(X)] end, string:tokens((erlang:binary_to_list(Skills)), ", ")),
	if Command =:= "ADD" ->
		agent_auth:new_profile(erlang:binary_to_list(Name), SkillsList);
	Command =:= "DELETE" ->
		agent_auth:destroy_profile(erlang:binary_to_list(Name));
	Command =:= "UPDATE" ->
		{_, Oldname} = lists:nth(4, Profile),
		Old = agent_auth:get_profile(erlang:binary_to_list(Oldname)),
		agent_auth:set_profile(erlang:binary_to_list(Oldname), erlang:binary_to_list(Name), SkillsList);
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
