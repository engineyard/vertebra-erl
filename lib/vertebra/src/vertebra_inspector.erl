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

-module(vertebra_inspector).

-behaviour(natter_stanza_inspector).
-behaviour(gen_server).

%% API
-export([start_link/2, inspect_inbound_stanza/2, annotate_outbound_stanza/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {owner_jid,
         config}).

start_link(ConfigFile, OwnerJid) when is_list(OwnerJid) ->
  gen_server:start_link(?MODULE, [ConfigFile, OwnerJid], []).

inspect_inbound_stanza(InspectorPid, Stanza) ->
  gen_server:call(InspectorPid, {inspect_inbound, Stanza}).

annotate_outbound_stanza(InspectorPid, Stanza) ->
  gen_server:call(InspectorPid, {annotate_outbound, Stanza}).

init([ConfigFile, OwnerJid]) ->
  {ok, Config} = vertebra_inspector_config:read(ConfigFile),
  {ok, #state{config=Config,
              owner_jid=OwnerJid}}.

handle_call({inspect_inbound, {xmlelement, "iq", _Attrs, _SubEls}}, _From, State) ->
  %% Depending on what we do with annotations we can do this lookup once
  %% when the inspector is started.
  case vertebra_inspector_config:find_rule(State#state.owner_jid, State#state.config) of
    {delay, Min, Max} ->
      {reply, {delay, random_interval(Min, Max)}, State};
    _ ->
      {reply, route, State}
  end;

handle_call({inspect_inbound, _Stanza}, _From, State) ->
  {reply, route, State};

handle_call({annotate_outbound, Stanza}, _From, State) ->
  {reply, Stanza, State};

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
random_interval(Min, Max) ->
  {T1, T2, T3} = erlang:now(),
  random:seed(T1, T2, T3),
  erlang:round(((Max - Min + 1) * random:uniform()) + Min).
