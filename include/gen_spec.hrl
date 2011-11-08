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
%%	The Original Code is OpenACD.
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
%%	Andrew Thompson <andrew at hijacked dot us>
%%	Micah Warren <micahw at lordnull dot com>
%%

%% define the type state() before including this file otherwise it'll explode.

-type(gen_timeout() :: pos_integer() | 'infinity' | 'hibernate').

-ifdef(GEN_SERVER).

-spec(init/1 :: (Args :: [any()]) -> {'ok', state()} | {'ok', state(), gen_timeout()} | {'stop', any()} | 'ignore').
-spec(handle_call/3 :: (Event :: any(), From :: pid(), State :: state()) -> 
	{'reply', any(), state()} | 
	{'reply', any(), state(), gen_timeout()} | 
	{'noreply', state()} | 
	{'noreply', state(), gen_timeout()} | 
	{'stop', any(), any(), state()} | 
	{'stop', any(), state()}).
-spec(handle_cast/2 :: (Request :: any(), State :: state()) -> 
	{'noreply', state()} | 
	{'noreply', state(),  gen_timeout()} | {
	'stop', any(), state()}).
-spec(handle_info/2 :: (Request :: any(), State :: state()) ->  
	{'noreply', state()} | 
	{'noreply', state(),  gen_timeout()} | 
	{'stop', any(), state()}).
-spec(terminate/2 :: (Reason :: any(), State :: state()) -> any()).
-spec(code_change/3 :: (Vsn :: {'down', any()} | any(), State :: state(), Extra :: any()) -> {'ok', state()}).

-endif.

-ifdef(GEN_FSM).

-spec(init/1 :: (Args :: [any()]) -> 
	{'ok', statename(),  state()} | 
	{'ok',  statename(), state(), gen_timeout()} | 
	{'stop', any()} | 'ignore').
-spec(handle_event/3 :: (Event :: any(), StateName :: statename(), State :: state()) -> 
	{'next_state', statename(), state()} | 
	{'next_state', statename(), state(), gen_timeout()} | 
	{'stop', any(), state()}).
-spec(handle_sync_event/4 :: (Event :: any(), From :: pid(), StateName :: statename(), State :: state()) -> 
	{'reply', any(), statename(), state()} | 
	{'reply', any(), statename(), state(), gen_timeout()} | 
	{'next_state', statename(), state()} | 
	{'next_state', statename(), state(), gen_timeout()} | 
	{'stop', any(), any(), state()} | 
	{'stop', any(), state()}).
-spec(handle_info/3 :: (Event :: any(), StateName :: statename(), State :: state()) ->
	{'next_state', statename(), state()} | 
	{'next_state', statename(), state(), gen_timeout()} | 
	{'stop', any(), state()}).
-spec(terminate/3 :: (Reason :: any(), StateName :: statename(), State :: state()) -> any()).
-spec(code_change/4 :: (OldVsn :: {'down', any()} | any(), StateName :: statename(), State :: state(), Extra :: any()) -> 
	{'ok', statename(), state()}).

-endif.

-ifdef(GEN_EVENT).

-spec(init/1 :: (Args :: [any()]) -> {'ok', state()} | {'ok', state(), 'hibernate'}).
-spec(handle_event/2 :: (Event :: any(), State :: state()) -> 
	{'ok', state()} | 
	{'ok', state(), 'hibernate'} |
	{'swap_handler', any(), state(), atom() | {atom(), any()}, any()} |
	'remove_handler').
-spec(handle_call/2 :: (Request :: any(), State :: state()) -> 
	{'ok', any(), state()} | 
	{'ok', any(), state(), 'hibernate'} |
	{'swap_handler', any(), any(), state(), atom() | {atom(), any()}, any()} |
	{'remove_handler', any()}).
-spec(handle_info/2 :: (Info :: any(), State :: state()) ->
	{'ok', state()} | 
	{'ok', state(), 'hibernate'} |
	{'swap_handler', any(), state(), atom() | {atom(), any()}, any()} |
	'remove_handler').
-spec(terminate/2 :: (Reason :: any(), State :: state()) -> any()).
-spec(code_change/3 :: (OldVsn :: {'down', any()} | any(), State :: state(), Extra :: any()) -> {'ok', state()}).

-endif.

-ifdef(GEN_LEADER).

-type(election() :: tuple()).

-spec(init/1 :: (Args :: any()) -> {'ok', state()}).
-spec(elected/3 :: (State :: state(), Election :: election(), Node :: atom()) -> {'ok', any(), state()}).
-spec(surrendered/3 :: (State :: state(), Elected :: any(), Election :: election()) -> {'ok', state()}).
-spec(handle_DOWN/3 :: (Node :: atom(), State :: state(), Election :: election()) -> {'ok', state()}).
-spec(handle_leader_call/4 :: (Request :: any(), From :: pid(), State :: state(), Election :: election()) -> {'reply', any(), state()}).
-spec(handle_call/4 :: (Request :: any(), From :: pid(), State :: state(), Election :: election()) -> {'reply', any(), state()} | {'stop', any(), any(), state()} | {'noreply', any(), state()}).
-spec(handle_leader_cast/3 :: (Request :: any(), State :: state(), Election :: election()) -> {'noreply', state()}).
-spec(handle_cast/3 :: (Msg :: any(), State :: state(), Election :: election()) -> {'noreply', state()}).
-spec(handle_info/2 :: (Info :: any(), State :: state()) -> {'noreply', state()}).
-spec(from_leader/3 :: (Msg :: any(), State :: state(), Election :: election()) -> {'ok', state()}).
-spec(terminate/2 :: (Reason :: any(), State :: state()) -> 'ok').
-spec(code_change/4 :: (OldVsn :: {'down', any} | any(), State :: state(), Election :: election(), Extra :: any()) -> {'ok', state()}).

-endif.

-ifdef(SUPERVISOR).

-type(childid() :: any()).
-type(startfunc() :: {atom, atom, [any()]}).
-type(childrestart() :: 'permanent' | 'transient' | 'temporary').
-type(childkill() :: 'brutal_kill' | non_neg_integer() | 'infinity').
-type(childtype() :: 'worker' | 'supervisor').
-type(childmodules() :: [atom()] | 'dynamic').
-type(child_spec() :: {childid(), startfunc(), childrestart(), childkill(), childtype(), childmodules()}).

-type(restart_strat() :: 'one_for_all' | 'one_for_one' | 'rest_for_one' | 'simple_one_for_one').
-spec(init/1 :: (Args :: [any]) -> 'ignore' | {'ok', {{restart_strat(), non_neg_integer(), non_neg_integer()}, [child_spec()]}}).

-endif.

-ifdef(GEN_MEDIA).

-include("gen_media.hrl").

-type(route_hint() :: {string(), #call{}} | 'undefined' | #call{}).
-type(gen_media_statename() :: 'iniver' | 'inqueue' | 'inqueue_ringing' |
	'oncall' | 'oncall_ringing' | 'wrapup').
-type(gen_media_state() :: #inivr_state{} | #inqueue_state{} |
	#inqueue_ringing_state{} | #oncall_state{} | #oncall_ringing_state{} |
	#wrapup_state{}).

-spec(init/1 :: (Args :: any()) ->
		{'ok', {state(), route_hint()}} |
		{'ok', {state(), #call{}, {transaction_type(), any()}}} |
		{'stop', any()} | 'ignore').

-spec(handle_ring/4 :: (Agent :: pid(), RingData :: any(), Call :: #call{}, 
	State :: state()) ->
		{'ok', state()} | {'invalid', state()}).

-spec(handle_ring_stop/4 :: (Statename :: gen_media_statename(),
	Call :: #call{}, GenMediaState :: gen_media_state(), State :: state()) ->
		{'ok', state()}).

-spec(handle_answer/5 :: (Agent :: pid(),
	Statename :: gen_media_statename(), Call :: #call{},
	GenMediaState :: gen_media_state(), State :: state()) ->
		{'ok', state()} | {'error', any(), state()}).

-spec(handle_agent_transfer/4 :: (Agent :: pid(), Timeout :: pos_integer(),
	Call :: #call{}, State :: state()) ->
		{'ok', state()} | {'error', any(), state()}).

-spec(handle_queue_transfer/5 :: (Queue :: string(),
	Statename :: gen_media_statename(), Call :: #call{},
	GenMediaState :: gen_media_state(), State :: state()) ->
		{'ok', state()}).

-spec(handle_wrapup/5 :: (From :: {pid(), reference()}, 
	Statename :: gen_media_statename(), Call :: #call{},
	GenMediaState :: gen_media_state(), State :: state()) ->
		{'ok', state()} | {'hangup', state()}).

-spec(handle_call/6 :: (Event :: any(), From :: {pid(), any()},
	Statename :: gen_media_statename(), Call :: #call{},
	GenMediaState :: gen_media_state(), State :: state()) ->
		{'reply', any(), state()} | 
		{'reply', any(), state(), gen_timeout()} | 
		{'noreply', state()} | 
		{'noreply', state(), gen_timeout()} | 
		{'stop', any(), any(), state()} | 
		{'stop', any(), state()} |
		{'stop_ring', any(), state()} |
		{'answer', any(), state()} |
		{'wrapup', any(), state()}).

-spec(handle_cast/3 :: (Request :: any(), Call :: #call{},
	State :: state()) ->
		{'noreply', state()} | 
		{'noreply', state(),  gen_timeout()} | 
		{'stop', any(), state()} | 
		{'stop_ring', state()} | 
		{'answer', state()} | 
		{'wrapup', state()}).

-spec(handle_info/5 :: (Request :: any(),
	Statename :: gen_media_statename(), Call :: #call{},
	GenMediaState :: gen_media_state(), State :: state()) ->
		{'noreply', state()} | 
		{'noreply', state(),  gen_timeout()} | 
		{'stop', any(), state()} | 
		{'stop_ring', state()} | 
		{'answer', state()} | 
		{'wrapup', state()}).

-spec(terminate/5 :: (Reason :: any(), Statename :: gen_media_statename(),
	Call :: #call{}, GenMediaState :: gen_media_state(), State :: state()) -> 
		any()).

-spec(code_change/4 :: (Vsn :: {'down', any()} | any(), Call :: #call{},
	State :: state(), Extra :: any()) ->
		{'ok', state()}).

-endif.

-ifdef(GEN_CDR_DUMPER).

-spec(init/1 :: (Args :: any()) -> {'ok', state()}).
-spec(dump/2 :: (MnesiaRow :: #agent_state{} | #cdr_rec{}, State :: state()) -> {'ok', state()}).
-spec(commit/1 :: (State :: state()) -> {'ok', state()}).
-spec(rollback/1 :: (State :: state()) -> any()).
-spec(terminate/2 :: (Reason :: any(), State :: state()) -> any()).
-spec(code_change/3 :: (Oldvsn :: any(), State :: state(), Extra :: any()) -> {'ok', state()}).

-endif.
