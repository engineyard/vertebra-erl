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

-module(cavalcade_command).

-author("ksmith@engineyard.com").

-behaviour(gen_xmpp_client).
-behaviour(gen_server).

%% Public API
%% You should ONLY use this function
%% unless you are familiar with how this module works
-export([run/5]).
-export([run/6]).

%% Private API
-export([complete_ack/5, receive_op_results/5, receive_final/5]).
-export([start_link/5, start_link/6, execute/2, test/0]).

%% gen_xmpp_client callbacks
-export([xmpp_init/1, presence/6, message/8, iq/8, recv_cast/2]).

%% gen_server callbacks
-export([init/1, code_change/3, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-record(state,
        {xmpp,
         is_test=false,
         command,
         inputs,
         targetjid,
         token,
         results=undefined,
         client_info}).

-record(xmpp_state,
        {command_pid=undefined,
         next_action=complete_ack}).

run(Config, Op, Inputs, Token, Target) ->
  run(Config, Op, Inputs, Token, Target, {pid, self()}),
  receive
    {xmpp_command_result, From, Data} ->
      {ok, From, Data}
  after 10000 ->
      {error, timeout}
  end.

run(Config, Op, Inputs, Token, Target, ClientRef) when is_list(Inputs),
                                                       is_tuple(ClientRef) ->
  {ok, R1} = xml_util:convert(to, Inputs),
  {ok, CP} = cavalcade_command:start_link(Config, Op, R1, Target, Token),
  cavalcade_command:execute(CP, ClientRef);

run(Op, Inputs, Token, Target, ClientRef, XMPP) when is_list(Inputs),
                                                     is_tuple(ClientRef),
                                                     is_pid(XMPP) ->
  {ok, R1} = xml_util:convert(to, Inputs),
  {ok, CP} = cavalcade_command:start_link([], Op, R1, Target, Token, XMPP),
  cavalcade_command:execute(CP, ClientRef).

%% Private API Functions
execute(CommandPid, {ClientType, ClientRef}) ->
  gen_server:cast(CommandPid, {execute, {ClientType, ClientRef}}).

complete_ack(CommandPid, From, PacketId, Attrs, Elts) ->
  gen_server:call(CommandPid, {complete_ack, From, PacketId, Attrs, Elts}),
  receive_op_results.

receive_op_results(CommandPid, From, PacketId, Attrs, Elts) ->
  gen_server:call(CommandPid, {receive_op_results, From, PacketId, Attrs, Elts}),
  receive_final.

receive_final(CommandPid, From, PacketId, _Attrs, _Elts) ->
  gen_server:call(CommandPid, {receive_final, From, PacketId}),
  undefined.

%% gen_xmpp_client functions
start_link(Config, Command, Inputs, TargetJid, Token) when is_list(Config) ->
  {ok, XMPP} = gen_xmpp_client:start_link(?MODULE, Config, []),
  gen_server:start_link(?MODULE, [XMPP, Command, Inputs, TargetJid, Token, false], []).

start_link(Config, Command, Inputs, TargetJid, Token, XMPP) when is_list(Config) ->
  gen_server:start_link(?MODULE, [XMPP, Command, Inputs, TargetJid, Token, true], []).

xmpp_init(_Args) ->
  {ok, #xmpp_state{}}.

presence(_XMPP, _Type, _From, _Attrs, _Elts, State) ->
  State.

%% Ignore chat message packets
message(_XMPP, _Type, _From, _Subject, _Body, _Attrs, _Elts, State) ->
  State.

iq(_XMPP, _Type, From, _QueryNS, PacketId, Attrs, Elts, State) ->
  case State#xmpp_state.command_pid of
    undefined ->
      State;
    CommandPid ->
      #xmpp_state{next_action=FnName} = State,
      case FnName of
        undefined ->
          State;
        _ ->
          NextPendingAction= cavalcade_command:FnName(CommandPid, From, PacketId, Attrs, Elts),
          State#xmpp_state{next_action=NextPendingAction}
      end
  end.

recv_cast({cmd_pid, CommandPid}, State) ->
  State#xmpp_state{command_pid=CommandPid};

recv_cast(_Msg, State) ->
  State.

%% gen_server functions
init([XMPP, Command, Inputs, TargetJid, Token, IsTest]) ->
  {ok, #state{xmpp=XMPP, command=Command,
              inputs=Inputs, targetjid=TargetJid,
              token=Token,
              is_test=IsTest}}.

code_change(_OldVsn, State, _Extras) ->
  {ok, State}.

terminate(_Reason, State) ->
  case State#state.is_test of
    true ->
      xmpp:stop(State#state.xmpp);
    false ->
      ok
  end.

handle_cast({execute, ClientInfo}, State) ->
  gen_xmpp_client:cast(State#state.xmpp, {cmd_pid, self()}),
  Op = ops_builder:generic_op({State#state.command,
                               State#state.token,
                               State#state.inputs}),
  cavalcade_xmpp:send_set(State#state.xmpp, State#state.targetjid, Op),
  {noreply, State#state{client_info=ClientInfo}};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call({complete_ack, From, PacketId, _Attrs, Elts}, _From, State) ->
  Token = get_token(Elts),
  Ack = ops_builder:ack_op(Token),
  cavalcade_xmpp:send_result(State#state.xmpp, From, PacketId, Ack),
  {reply, ok, State};

handle_call({receive_op_results, From, PacketId, _Attrs, Elts}, _From, State) ->
  [H|_T] = Elts,
  cavalcade_xmpp:send_result(State#state.xmpp, From, PacketId, ops_builder:final_op(State#state.token)),
  {reply, ok, State#state{results=H}};

handle_call({receive_final, From, PacketId}, _From, State) ->
  cavalcade_xmpp:send_result(State#state.xmpp, From, PacketId, ops_builder:final_op(State#state.token)),
  #state{results={xmlelement, "result", _, Data},
         client_info={_RefType, ClientRef}} = State,
  FinalResults = prep_results(Data),
  ClientRef ! {xmpp_command_result, From, FinalResults},
  {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
  {reply, ignored, State}.


handle_info(_Msg, State) ->
  {noreply, State}.

test() ->
  Token = uuid_server:generate_uuid(),
  Op = "/security/discover",
  Target = "herault@localhost/herault",
  Resources = [{resource, [], "/cluster/rd00"}],
  Config = [{host, "localhost"}, {username, "cavalcade"},
            {authentication, {password, "testing"}},
            {resource, Token}],

  cavalcade_command:run(Config, Op, Resources, Token, Target).

%% Private functions
get_token([{xmlelement, _, Attrs, _}|_T]) ->
  proplists:get_value("token", Attrs).

prep_results(Results) ->
  {ok, Data} = xml_util:convert(from, Results),
  Data.
