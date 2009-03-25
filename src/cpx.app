{application, cpx, [
	{description, "Call queuing and routing system"},
	{vsn, "0.1"},
	{modules, [
		cpx,
		agent,
		agent_auth,
		agent_tcp_connection,
		agent_tcp_listener,
		call_queue,
		cook,
		cpx_supervisor,
		dispatcher,
		dispatch_manager,
		queue_manager,
		util,
		cpx_web_management,
		agent_web_connection,
		agent_web_listener,
		freeswitch_media_manager,
		freeswitch_media
	]},
	{registered, [
		agent_manager,
		cpx_supervisor,
		dispatch_manager,
		queue_manager,
		freeswitch_media_manager
	]},
	{mod, {cpx, []}},
	{env, []},
	{applications, [kernel, stdlib, mnesia]},
	{start_phases, []}
]}.
