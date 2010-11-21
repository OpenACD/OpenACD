{application, sipxplugin, [{description, "sipXecs config plugin"},
						{vsn, "0.1"},
						{modules, [sipxplugin_app, sipxplugin_poller, sipxplugin_supervisor]},
						{registered, [sipxplugin_poller, sipxplugin_supervisor]},
						{applications, [kernel, stdlib, erlmongo]},
						{mod, {sipxplugin_app, []}},
						{start_phases, []}
						]}.
