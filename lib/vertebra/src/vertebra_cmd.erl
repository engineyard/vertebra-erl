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

-module(vertebra_cmd).

-behaviour(gen_server).

%% API
-export([test/0, start_link/5, start_link/6, run/5, run/6, execute/2]).

-define(ERROR_TRACKING_DISABLED, {}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {inputs,
         op,
         target,
         start_token,
         connection,
         own_connection=true,
         client,
         results=[]}).

test() ->
  Token = uuid_server:generate_uuid(),
  Op = "/security/discover",
  Target = "herault@localhost/herault",
  Resources = [{resource, [], "/cluster/rd00"}],
  Config = [{user, "cavalcade"},
            {password, "testing"},
            {resource, Token}],
  vertebra_cmd:run(Config, Op, Resources, Token, Target).

start_link(Config, Op, Resources, Target, Token) ->
  gen_server:start_link(?MODULE, [Config, Op, Resources, Target, Token], []).

start_link([], Op, Resources, Target, Token, Connection) ->
  gen_server:start_link(?MODULE, [[], Op, Resources, Target, Token, Connection], []).

run(Config, Op, Inputs, Token, Target) ->
  run(Config, Op, Inputs, Token, Target, {pid, self()}),
  receive
    {xmpp_command_result, From, Data} ->
      {ok, CD} = xml_util:convert(from, Data),
      {ok, From, CD}
  after 10000 ->
      {error, timeout}
  end.

run(Config, Op, Inputs, Token, Target, ClientRef) when is_list(Inputs),
						       is_tuple(ClientRef) ->
  {ok, R1} = xml_util:convert(to, Inputs),
  {ok, CP} = vertebra_cmd:start_link(Config, Op, R1, Target, Token),
  unlink(CP),
  vertebra_cmd:execute(CP, ClientRef);

run(Op, Inputs, Token, Target, ClientRef, XMPP) when is_list(Inputs),
						     is_tuple(ClientRef),
						     is_pid(XMPP) ->
  {ok, R1} = xml_util:convert(to, Inputs),
  {ok, CP} = vertebra_cmd:start_link([], Op, R1, Target, Token, XMPP),
  unlink(CP),
  vertebra_cmd:execute(CP, ClientRef).

execute(CommandPid, ClientRef) ->
  gen_server:cast(CommandPid, {execute, ClientRef}).

init([Config, Op, Inputs, Target, Token]) ->
  {ok, Connection} = natter_connection:start_link(Config),
  natter_connection:register_exchange(Connection, "iq", Target, self()),
  {ok, #state{inputs=Inputs,
              op=Op,
              target=Target,
              start_token=string:tokens(Token, ":"),
              connection=Connection}};

init([[], Op, Inputs, Target, Token, Connection]) ->
  natter_connection:register_exchange(Connection, "iq", Target, self()),
  {ok, #state{inputs=Inputs,
              op=Op,
              target=Target,
              start_token=string:tokens(Token, ":"),
              connection=Connection,
              own_connection=false}}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({execute, Client}, State) ->
  Me = self(),
  Op = ops_builder:generic_op({State#state.op,
                               string:join(State#state.start_token, ":"),
                               State#state.inputs}),
  case vertebra_xmpp:send_wait_set(State#state.connection, ?ERROR_TRACKING_DISABLED, State#state.target, Op) of
    {ok, _UpdatedToken, Reply} ->
      case handle_reply(Reply) of
        retry ->
          %% Restart execute process since we got a wait error
          spawn(fun() -> vertebra_cmd:execute(Me, Client) end);
        ok ->
          Me ! {packet, Reply}
      end,
      {noreply, State#state{client=Client}};
    Error ->
      {stop, Error, State}
  end;

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({packet, {xmlelement, "iq", Attrs, [{xmlelement, "final", _, _}=Stanza]}}, State) ->
  From = proplists:get_value("from", Attrs),
  vertebra_xmpp:send_result(State#state.connection,
                                ?ERROR_TRACKING_DISABLED,
                                proplists:get_value("id", Attrs),
                                From,
                                increment_sequence(Stanza)),
  #state{client={pid, ClientRef}} = State,
  ClientRef ! {xmpp_command_result, From, lists:flatten(State#state.results)},
  {stop, normal, State};

handle_info({packet, {xmlelement, "iq", Attrs, [{xmlelement, "error", _, [Reason]}=Stanza]}}, State) ->
  From = proplists:get_value("from", Attrs),
  vertebra_xmpp:send_result(State#state.connection,
                                ?ERROR_TRACKING_DISABLED,
                                proplists:get_value("id", Attrs),
                                From,
                                increment_sequence(Stanza)),
  #state{client={pid, ClientRef}} = State,
  {ok, {string, ReasonDesc}} = xml_util:convert(from, Reason),
  ClientRef ! {xmpp_command_result, From, {error, ReasonDesc}},
  {stop, normal, State};


handle_info({packet, {xmlelement, "iq", Attrs, [{xmlelement, "result", _, Results}=Result]}}, State) ->
    vertebra_xmpp:send_result(State#state.connection,
                                ?ERROR_TRACKING_DISABLED,
                                proplists:get_value("id", Attrs),
                                proplists:get_value("from", Attrs),
                                increment_sequence(Result)),
  {noreply, State#state{results=[Results|State#state.results]}};

handle_info({packet, {xmlelement, "iq", Attrs, [{xmlelement, "ack", _, _}=Ack]}}, State) ->
  vertebra_xmpp:send_result(State#state.connection,
                                ?ERROR_TRACKING_DISABLED,
                                proplists:get_value("id", Attrs),
                                proplists:get_value("from", Attrs),
                                increment_sequence(Ack)),
  {noreply, State};

handle_info({packet, _}, State) ->
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  case State#state.own_connection of
    true ->
      natter_connection:close(State#state.connection);
    false ->
      natter_connection:unregister_exchange(State#state.connection, "iq", State#state.target)
  end,
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
handle_reply(Reply) ->
  case vertebra_error_policy:analyze(Reply) of
    wait ->
      timer:sleep(random:uniform(5000)),
      retry;
    Result ->
      Result
  end.

increment_sequence({xmlelement, Name, Attrs, Subels}) ->
  NewToken = case proplists:get_value("token", Attrs)  of
               undefined ->
                 exit({error, missing_token});
               Token ->
                 vertebra_util:increment_token_sequence(Token)
             end,
  {xmlelement, Name, [{"token", NewToken} | proplists:delete("token", Attrs)], Subels}.
