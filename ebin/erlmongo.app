{application, erlmongo, [{description, "Erlang driver for mongodb"},
						{vsn, "0.1"},
						{modules, [erlmongo_app, mongodb, mongoapi, mongodb_supervisor]},
						{registered, [mongodb, mongodb_supervisor]},
						{applications, [kernel, stdlib]},
						{mod, {erlmongo_app, []}},
						{start_phases, []}
						]}.