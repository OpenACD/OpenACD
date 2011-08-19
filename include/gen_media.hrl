-record(inivr_state, {}).

-record(inqueue_state, {
	queue_mon :: 'undefined' | reference(),
	queue_pid :: {string(), pid()},
	cook :: 'undefined' | reference(),
	cook_mon :: reference()
}).

-record(inqueue_ringing_state, {
	queue_mon :: 'undefined' | reference(),
	queue_pid :: {string(), pid()},
	cook :: 'undefined' | reference(),
	cook_mon :: reference(),
	ring_mon :: 'undefined' | reference(),
	ring_pid :: {string, pid()},
	ringout :: reference(),
	outband_ring_pid :: 'undefined' | pid()
}).

-record(oncall_state, {
	oncall_mon :: 'undefined' | reference(),
	oncall_pid :: {string(), pid()}
}).

-record(oncall_ringing_state, {
	oncall_mon :: 'undefined' | reference(),
	oncall_pid :: {string(), pid()},
	ring_mon :: reference(),
	ring_pid :: {string(), pid()},
	ringout :: reference
}).

-record(wrapup_state, {
	transfer_state :: 'undefined' | #warm_transfer_merged_state{}
}).

%% it is up to the media to maintain a list of held medias.
-record(warm_transfer_hold_state, {
	oncall_mon :: reference(),
	oncall_pid :: {string(), pid()},
	held_refs [] :: [{any(), any()}],
	merged_refs [] :: [{any(), any()}],
	caller_ref :: any()
}).

%% it is up to the media to maintain a list of held medias
-record(warm_transfer_3rd_party_state, {
	oncall_mon :: reference(),
	oncall_pid :: {string(), pid()},
	held_refs [] :: [{any(), any()}],
	merged_refs [] :: [{any(), any()}],
	active_ref :: {any(), any()},
	caller_ref :: any()
}).

%% it is up tot he media to maintain a list of help medias.
-record(warm_transfer_merged_state, {
	oncall_mon :: reference(),
	oncall_pid :: {string(), pid()},
	merged_refs [] :: [{any(), any()}],
	caller_ref :: any()
}).
