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

-module(workflow_runner).

-author("ksmith@engineyard.com").

-behaviour(gen_server).

-define(STARTING_INPUT, starting_input).
-include("cavalcade.hrl").

-define(SKIP_WARN, fun(E) -> io:format("Warning! Skipping (~p: ~p): ~p~n", [?FILE, ?LINE, E]) end).
-define(MISSING_WARN, fun(E) -> io:format("Warning! Missing (~p: ~p): ~p~n", [?FILE, ?LINE, E]) end).
%% API
-export([start_link/3, run/1, get_results/1, get_token/1, test/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {config,
         token=uuid_server:generate_uuid(),
         context,
         owner}).

start_link(Config, Workflow, Owner) when is_pid(Owner) ->
  gen_server:start_link(?MODULE, [Config, Workflow, Owner], []).

run(WorkflowPid) ->
  gen_server:cast(WorkflowPid, start).

get_results(WorkflowPid) ->
  gen_server:call(WorkflowPid, get_results).

get_token(WorkflowPid) ->
  gen_server:call(WorkflowPid, get_token).

test(WorkflowFile) ->
  {ok, [Workflow]} = workflow_parser:parse_file(WorkflowFile),
  Config = [{host, "localhost"}, {username, "cavalcade"},
            {authentication, {password, "testing"}}],
  {ok, P} = workflow_runner:start_link(Config, Workflow, self()),
  workflow_runner:run(P),
  receive
    {workflow_results, Results} ->
      Results
  after 60000 ->
      timeout
  end.

%% gen_server callbacks
init([Config, Workflow, Owner]) ->
  {ok, #state{config=Config, context=workflow_manager:new(Workflow), owner=Owner}}.

handle_call(get_token, _From, State) ->
  {reply, State#state.token, State};

handle_call(get_results, _From, State) ->
  {reply, {ok, workflow_manager:global_state(State#state.context)}, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(start, State) ->
  {noreply, run_next_op(State)};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({xmpp_command_result, From, Data}, State) ->
  OutputName = workflow_manager:current_output(State#state.context),
  NC = workflow_manager:global_state(update_global_state(OutputName,
                                                         From,
                                                         Data,
                                                         workflow_manager:global_state(State#state.context)), State#state.context),
  NewContext = workflow_manager:decr_response_count(NC),
  NewState = State#state{context=NewContext},
  Reply = case workflow_manager:response_count(NewContext) == 0 of
            true ->
              UpdatedState = run_next_op(NewState),
              case  workflow_manager:current_step(UpdatedState#state.context) =:= complete of
                true ->
                  State#state.owner ! {workflow_results, workflow_manager:marshal_global_state(UpdatedState#state.context)},
                  {stop, normal, NewState};
                false ->
                  {noreply, UpdatedState}
              end;
            false ->
              {noreply, NewState}
          end,
  Reply;

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("Workflow complete. Terminating...~n"),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

prepare_inputs(Op, GlobalState) ->
  prepare_inputs(Op#workflow_op.inputs, GlobalState, [], []).

prepare_inputs([{import, Attrs}=H|T], GlobalState, Targets, Accum) ->
  [NewTargets,
   NewAccum] = case proplists:get_value("type", Attrs) of
                 undefined ->
                   ?SKIP_WARN(H),
                   [Targets, Accum];
                 Role ->
                   case proplists:get_value("name", Attrs) of
                     undefined ->
                       ?SKIP_WARN(H),
                       [Targets, Accum];
                     Value ->
                       case dict:find(Value, GlobalState) of
                         error ->
                           ?MISSING_WARN(H),
                           [Targets, Accum];
                         {ok, Data} ->
                           case Role of
                             "target" ->
                               [[D || {_, D} <- Data], Accum];
                             _ ->
                               [Targets, [Data|Accum]]
                           end
                       end
                   end
               end,
  prepare_inputs(T, GlobalState, NewTargets, NewAccum);
prepare_inputs([{string, Attrs, Value}=H|T], GlobalState, Targets, Accum) ->
  case proplists:get_value("name", Attrs) of
    "target" ->
      CleanedTarget = case is_binary(Value) of
                        false ->
                          {string, Attrs, list_to_binary(Value)};
                        true ->
                          H
                      end,
      prepare_inputs(T, GlobalState, [CleanedTarget|Targets], Accum);
    _ ->
      prepare_inputs(T, GlobalState, Targets, [H|Accum])
  end;
prepare_inputs([H|T], GlobalState, Targets, Accum) ->
  prepare_inputs(T, GlobalState, Targets, [H|Accum]);
prepare_inputs([], _GlobalState, Targets, Accum) ->
  {Targets, lists:reverse(Accum)}.

run_next_op(State) ->
  NewContext = workflow_manager:advance(State#state.context),
  case workflow_manager:current_step(NewContext) =:= complete of
    true ->
      io:format("Workflow is complete~n"),
      State#state{context=NewContext};
    false ->
      CurrentOp = workflow_manager:current_op(NewContext),
      {Targets, Params} = prepare_inputs(CurrentOp,
                                         workflow_manager:global_state(NewContext)),
      #state{token=Token, config=Config} = State,
      MyPid = self(),
      NewState = State#state{context=workflow_manager:response_count(length(Targets), NewContext)},
      spawn(fun() ->
                send_commands([{resource, Token}|Config],
                              CurrentOp#workflow_op.op,
                              Params,
                              Token,
                              Targets,
                              {gen_server, MyPid}) end),
      NewState
  end.

send_commands(Config, Op, Params, Token, [Target|T], NotifyInfo) ->
  case is_list(Target) of
    true ->
      send_commands(Config, Op, Params, Token, Target, NotifyInfo);
    false ->
      io:format("Sending ~p to ~p~n", [Op, vert_string_to_list(Target)]),
      vertebra_cmd:run(Config, Op, Params, Token, vert_string_to_list(Target), NotifyInfo)
  end,
  send_commands(Config, Op, Params, Token, T, NotifyInfo);
send_commands(_Config, _Op, _Params, _Token, [], _NotifyInfo) ->
  io:format("No more targets. Done sending commands~n"),
  ok.

vert_string_to_list({string, _, Text}) when is_binary(Text) ->
  binary_to_list(Text);
vert_string_to_list(Text) ->
  Text.

update_global_state([{all, Attrs}|T], From, Data, GlobalState) ->
  Key = case proplists:get_value("name", Attrs) of
          undefined ->
            throw(missing_global_export);
          V ->
            V
        end,
  NewGlobalState = case dict:find(Key, GlobalState) of
                     error ->
                       dict:store(Key, [{From, Data}], GlobalState);
                     Value ->
                       dict:store(Key, [{From, Data}|Value], GlobalState)
                   end,
  update_global_state(T, From, Data, NewGlobalState);
update_global_state([], _From, _Data, GlobalState) ->
  GlobalState.
