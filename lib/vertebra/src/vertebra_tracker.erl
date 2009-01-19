% Copyright 2008, Engine Yard, Inc.
%
% This file is part of Vertebra.
%
% Vertebra is free software: you can redistribute it and/or modify it under the
% terms of the GNU Lesser General Public License as published by the Free
% Software Foundation, either version 3 of the License, or (at your option) any
% later version.
%
% Vertebra is distributed in the hope that it will be useful, but WITHOUT ANY
% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
% A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
% details.
%
% You should have received a copy of the GNU Lesser General Public License
% along with Vertebra.  If not, see <http://www.gnu.org/licenses/>.

-module(vertebra_tracker).

-author("ksmith@engineyard.com").

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, get_status/2, set_status/3, buffer_stanza/3]).
-export([reset/1, get_buffered_stanza/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {cn,
         targets=dict:new()}).

-record(target,
        {jid,
         status,
         pending}).

reset(TrackerPid) ->
  gen_server:call(TrackerPid, reset).

get_status(TrackerPid, TargetJid) ->
  gen_server:call(TrackerPid, {get_status, TargetJid}).

set_status(TrackerPid, TargetJid, Status) when Status =:= offline;
                                               Status =:= online ->
  gen_server:call(TrackerPid, {set_status, TargetJid, Status}).

buffer_stanza(TrackerPid, TargetJid, Stanza) ->
  gen_server:call(TrackerPid, {buffer_stanza, TargetJid, Stanza}).

%% Meant for unit testing only
get_buffered_stanza(TrackerPid, TargetJid) ->
  gen_server:call(TrackerPid, {get_stanza, TargetJid}).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

start_link(Connection) ->
  gen_server:start_link(?MODULE, [Connection], []).

init([]) ->
  {ok, #state{}};

init([Connection]) ->
  {ok, #state{cn=Connection}}.

handle_call({get_stanza, TargetJid}, _From, State) ->
  Reply = case find_target(TargetJid, false, State) of
            not_found ->
              not_found;
            Target ->
              case Target#target.pending =:= undefined of
                true ->
                  not_buffered;
                false ->
                  Target#target.pending
              end
          end,
  {reply, Reply, State};

handle_call({get_status, TargetJid}, _From, State) ->
  case find_target(TargetJid, false, State) of
    not_found ->
      {reply, online, State};
    Target ->
      {reply, Target#target.status, State}
  end;

handle_call(reset, _From, State) ->
  {reply, ok, #state{cn=State#state.cn}};

handle_call({set_status, TargetJid, online}, _From, State) ->
  NewState = case find_target(TargetJid, false, State) of
               not_found ->
                 State;
               Target ->
                 if
                   Target#target.pending =:= undefined->
                     ok;
                   true ->
                     send_pending(State#state.cn, Target#target.pending)
                 end,
                 State#state{targets=dict:erase(TargetJid, State#state.targets)}
             end,
  {reply, ok, NewState};

handle_call({set_status, TargetJid, Status}, _From, State) ->
  Target = (find_target(TargetJid, State))#target{status=Status},
  {reply, ok, State#state{targets=dict:store(TargetJid, Target, State#state.targets)}};

handle_call({buffer_stanza, TargetJid, Stanza}, _From, State) ->
  Target = find_target(TargetJid, State),
  if
    Target#target.pending =:= undefined ->
      FinalTarget = Target#target{pending=Stanza, status=offline},
      {reply, ok, State#state{targets=dict:store(TargetJid, FinalTarget, State#state.targets)}};
    true ->
      {reply, {error, already_buffered}, State}
  end;

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
find_target(TargetJid, State) ->
  find_target(TargetJid, true, State).
find_target(TargetJid, CreateNew, State) ->
  case dict:find(TargetJid, State#state.targets) of
    error ->
      if
        CreateNew =:= true ->
          #target{jid=TargetJid};
        true ->
          not_found
      end;
    {ok, Target} ->
      Target
  end.

send_pending(undefined, _) ->
  ok;
send_pending(Cn, Pending) ->
  natter_connection:raw_send(Cn, Pending).
