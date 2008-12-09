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

-module(cavalcade_worker).

-author("ksmith@engineyard.com").

-behaviour(gen_server).

%% API
-export([start_link/4]).

-define(OK, {string, [{"name", "result"}], <<"ok">>}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([kickstart/1, get_results/1]).

-record(state,
        {packetid,
         from,
         op,
         token}).

start_link(From, PacketId, Token, Op) ->
  gen_server:start_link(?MODULE, [From, PacketId, Token, Op], []).

kickstart(Worker) ->
  gen_server:cast(Worker, start).

get_results(Worker) ->
  gen_server:call(Worker, get_results).

init([From, PacketId, Token, Op]) ->
  process_flag(trap_exit, true),
  {ok, #state{from=From,
              op=Op,
              packetid=PacketId,
              token=Token}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(start, State) ->
  #state{op=Op} = State,
  {xmlelement, "op", Attrs, _} = Op,
  handle_request(proplists:get_value("type", Attrs), State);

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(stop, State) ->
  {stop, normal, State};

handle_info({workflow_results, Results}, State) ->
  {ok, R} = xml_util:convert(to, Results),
  cavalcade_srv:send_set(State#state.from, ops_builder:result_op(R, State#state.token)),
  cavalcade_srv:send_set(State#state.from, ops_builder:final_op(State#state.token)),
  {stop, normal, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
handle_request("/workflow/store"=Action, State) ->
  case is_authorized(State#state.from, Action) of
    false ->
      cavalcade_srv:confirm_op(State#state.from, State#state.op, State#state.token, State#state.packetid, false);
    true ->
      case cavalcade_xmpp:get_named_arg("workflow", State#state.op) of
        not_found ->
          cavalcade_srv:confirm_op(State#state.from, State#state.op, State#state.token, State#state.packetid, false);
        {string, _Attrs, Workflow} ->
          cavalcade_srv:confirm_op(State#state.from, State#state.op, State#state.token, State#state.packetid, true),
          case workflow_parser:parse_workflow(xml_util:unescape(Workflow)) of
            {ok, Workflows} ->
              ok = store_workflows(Workflows),
              {ok, Res} = xml_util:convert(to, ?OK),
              cavalcade_srv:send_set(State#state.from, ops_builder:result_op(Res, State#state.token)),
              cavalcade_srv:send_set(State#state.from, ops_builder:final_op(State#state.token));
            {error, Reason} ->
              Result = xml_util:convert(to, {string, [{"name", "result"}], list_to_binary(lists:flatten(["error: ", Reason]))}),
              cavalcade_srv:send_set(State#state.from, ops_builder:result_op(Result, State#state.token)),
              cavalcade_srv:send_set(State#state.from, ops_builder:final_op(State#state.token))
          end;
        _InvalidWorkflow ->
          cavalcade_srv:confirm_op(State#state.from, State#state.op, State#state.token, State#state.packetid, false)
      end
  end,
  {stop, normal, State};

handle_request("/workflow/execute"=Action, State) ->
  case is_authorized(State#state.from, Action) of
    false ->
      cavalcade_srv:confirm_op(State#state.from, State#state.op, State#state.token, State#state.packetid, false),
      {stop, normal, State};
    true ->
      case cavalcade_xmpp:get_named_arg("workflow", State#state.op) of
        not_found ->
          cavalcade_srv:confirm_op(State#state.from, State#state.op, State#state.token, State#state.packetid, false),
          {stop, normal, State};
        {string, _Attrs, WorkflowName} when is_binary(WorkflowName) ->
          cavalcade_srv:confirm_op(State#state.from, State#state.op, State#state.token, State#state.packetid, true),
          WN = binary_to_list(WorkflowName),
          case invoke_workflow(WN) of
            {ok, _} ->
              {noreply, State};
            {error, Err} ->
              cavalcade_srv:send_set(State#state.from, ops_builder:result_op(Err, State#state.token)),
              cavalcade_srv:send_set(State#state.from, ops_builder:final_op(State#state.token)),
              {stop, normal, State}
          end
      end
  end;

handle_request("/workflow/status", State) ->
  case cavalcade_xmpp:get_named_arg("workflow_handle", State#state.op) of
    not_found ->
      io:format("Missing workflow_handle. nack'd~n"),
      cavalcade_srv:confirm_op(State#state.from, State#state.op, State#state.token, State#state.packetid, false);
    {string, [{"name", "workflow_handle"}], WH} ->
      WorkflowHandle = binary_to_list(WH),
      case cavalcade_srv:find_worker(WorkflowHandle) of
        not_found ->
          io:format("Missing worker. nack'd~n"),
          cavalcade_srv:confirm_op(State#state.from, State#state.op, State#state.token, State#state.packetid, false);
        {ok, Worker} ->
          cavalcade_srv:confirm_op(State#state.from, State#state.op, State#state.token, State#state.packetid, true),
          case cavalcade_worker:get_results(Worker) of
            still_running ->
              {ok, R} = xml_util:convert(to, {string, [{"name", "results"}], <<"still_running">>}),
              io:format("Sending result: ~p~n", [R]),
              cavalcade_srv:send_set(State#state.from, ops_builder:result_op(R, State#state.token));
            Results ->
              {ok, R} = xml_util:convert(to, Results),
              cavalcade_srv:send_set(State#state.from, ops_builder:result_op(R, State#state.token))
          end,
          cavalcade_srv:send_set(State#state.from, ops_builder:final_op(State#state.token))
      end
  end,
  {stop, normal, State};

handle_request("/workflow/delete", State) ->
  {stop, normal, State};
handle_request("/workflow/list", State) ->
  {stop, normal, State};
handle_request(_Ignored, State) ->
  cavalcade_srv:confirm_op(State#state.from, State#state.op, State#state.token, State#state.packetid, false),
  {stop, normal, State}.

is_authorized(From, Action) ->
  Config = cavalcade_srv:get_config(),
  case authz:verify(Config, proplists:get_value(herault, Config), From, [Action]) of
    {ok, Resp} ->
      Resp;
    Error ->
      Error
  end.

store_workflows([H|T]) ->
  ok = workflow_store:store_workflow(H),
  store_workflows(T);
store_workflows([]) ->
  ok.

invoke_workflow(WorkflowName) ->
  case workflow_store:find_workflow(WorkflowName) of
    {ok, Workflow} ->
      {ok, P} = workflow_runner:start_link(cavalcade_srv:get_config(), Workflow, self()),
      WorkflowToken = workflow_runner:get_token(P),
      workflow_runner:run(P),
      xml_util:convert(to, {string, [{"name", "result"}], list_to_binary(WorkflowToken)});
    _Error ->
      {ok, Result} = xml_util:convert(to, {string, [{"name", "error"}], <<"error locating workflow">>}),
      {error, Result}
  end.
