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
-export([start_link/1, target_status/2, target_status/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {cn,
         targets=dict:new()}).

-record(target,
        {jid,
         status,
         messages=[]}).

target_status(TrackerPid, TargetJid) ->
  gen_server:call(TrackerPid, {get_status, TargetJid}).

target_status(TrackerPid, TargetJid, Status) ->
  gen_server:call(TrackerPid, {set_status, TargetJid, Status}).

buffer_stanza(TrackerPid, TargetJid, Stanza) ->
  gen_server:call(TrackerPid, {buffer_stanza, Stanza}).

start_link(Connection) ->
  gen_server:start_link(?MODULE, [Connection], []).

init([Connection]) ->
  {ok, #state{cn=Connection}}.


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get_status, TargetJid}, _From, State) ->
  case dict:find(TargetJid, State#state.targets) of
    error ->
      {reply, online, State};
    {ok, Target} ->
      {reply, Target#target.status}
  end;

handle_call({set_status, TargetJid, online}, _From, State) ->
  {reply, {error, not_implemented}, State};

handle_call({set_status, TargetJid, Status}, _From, State) ->
  Target = (find_target(TargetJid, State))#target{status=Status},
  {reply, ok, State#state{targets=dict:store(TargetJid, Target, State#state.targets)}};

handle_call({buffer_stanza, TargetJid, Stanza}, _From, State) ->
  Target = find_target(TargetJid, State),
  FinalTarget = Target#target{messages=lists:reverse([Stanza|Target#target.messages])},
  {reply, ok, State#state{targets=dict:store(TargetJid, FinalTarget, State#state.targets)}};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
find_target(TargetJid, State) ->
  case dict:find(TargetJid, State#state.targets) of
    error ->
      #target{jid=TargetJid};
    {ok, Target} ->
      Target
  end.
