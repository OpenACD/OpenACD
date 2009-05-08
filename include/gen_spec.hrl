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
-spec(elected/2 :: (State :: state(), Election :: election()) -> {'ok', any(), state()}).
-spec(surrendered/3 :: (State :: state(), Elected :: any(), Election :: election()) -> {'ok', state()}).
-spec(handle_DOWN/3 :: (Node :: node(), State :: state(), Election :: election()) -> {'ok', state()}).
-spec(handle_leader_call/4 :: (Request :: any(), From :: pid(), State :: state(), Election :: election()) -> {'reply', any(), state()}).
-spec(handle_call/3 :: (Request :: any(), From :: pid(), State :: state()) -> {'ok', any(), state()}).
-spec(handle_leader_cast/3 :: (Request :: any(), State :: state(), Election :: election()) -> {'noreply', state()}).
-spec(handle_cast/2 :: (Msg :: any(), State :: state()) -> {'noreply', state()}).
-spec(handle_info/2 :: (Info :: any(), State :: state()) -> {'noreply', state()}).
-spec(from_leader/3 :: (Msg :: any(), State :: state(), Election :: election()) -> {'ok', state()}).
-spec(terminate/2 :: (Reason :: any(), State :: state()) -> 'ok').
-spec(code_change/4 :: (OldVsn :: {'down', any} | any(), State :: state(), Election :: election(), Extra :: any()) -> {'ok', state()}).

-endif.