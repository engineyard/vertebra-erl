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

%%%%%%
%%% The client follows a simple progression through a set of states.
%%%
%%%    1. New
%%%    2. Ready
%%%    3. Consume
%%%    4. Commit
%%%
%%% These states correspond to various network communications. In the new
%%% state, the client sends the initial "op" stanza and receives a token.
%%%
%%% In the ready state, the client waits for and responds to an Acknowledgement
%%% stanza. It can also receive a Negative Acknowledgement, which causes it to
%%% enter a "Auth Fail" state.
%%%
%%% When the "data" stanzas start coming in, it enters the Consume state, and
%%% responds for each of them.
%%%
%%% When the "final" stanza comes in, it enters the Commit state, in which it
%%% signals the code that all of the data has been received.
%%%%%%

-module(vertebra_client).

-behaviour(gen_fsm).

%% API
-export([execute/1, sync_execute/1]).
-export([start_link/5]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% Private callback
-export([return/2]).

-record(state, {callback}).

%%====================================================================
%% API
%%====================================================================
execute(Args) ->
  Session = proplists:get_value(session, Args),
  Callback = proplists:get_value(callback, Args),
  Jid = proplists:get_value(jid, Args),
  Type = proplists:get_value(op_type, Args),
  SubEls = proplists:get_value(sub_els, Args),
  start_link(Session, Callback, Jid, Type, SubEls).

sync_execute(Args) ->
  {ok, Pid} = execute([{callback, {?MODULE, return, [self()]}} | Args]),
  consume(Pid, []).

start_link(Session, {M,F,A} = Callback, Jid, Name, SubEls) when is_atom(M),
                                                                is_atom(F),
                                                                is_list(A) ->
  gen_fsm:start_link(?MODULE, [Session, Callback, Jid, Name, SubEls], []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
init([Session, Callback, Jid, Name, SubEls]) ->
  vertebra_session:start_op(Session, Jid, Name, SubEls),
  {ok, new, #state{callback=Callback}}.

%%% state_name(_Event, State) ->
%%%   {next_state, state_name, State}.

%%% state_name(_Event, _From, State) ->
%%%   Reply = ok,
%%%   {reply, Reply, state_name, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

handle_info(ack, ready, State) ->
  {next_state, consume, State};

handle_info(final, consume, State) ->
  callback(State, final),
  {stop, normal, State};

handle_info(nack, ready, State) ->
  callback(State, nack),
  {stop, normal, State};

handle_info({reply, _Id, result, _Elt}, new, State) ->
  {next_state, ready, State};

handle_info({result, Result}, consume, State) ->
  callback(State, {result, Result}),
  {next_state, consume, State};

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

callback(#state{callback={M,F,A}}, Arg) ->
  apply(M, F, [Arg | A]).

consume(Pid, Results) ->
  receive
    {return, Pid, final} ->
      {ok, Results};
    {return, Pid, nack} ->
      {error, nack};
    {return, Pid, {result, Result}} ->
      consume(Pid, [Result | Results])
  end.

return(Message, Pid) ->
  Pid ! {return, self(), Message}.
