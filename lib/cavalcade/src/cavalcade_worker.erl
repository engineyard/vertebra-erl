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

-behaviour(gen_server).

-author("ksmith@engineyard.com").

-define(OK, {string, [{"name", "result"}], <<"ok">>}).

-export([start_link/4, kickstart/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-record(state,
        {owner,
         from,
         op,
         token}).

start_link(Owner, From, Token, Op) ->
  gen_server:start_link(?MODULE, [Owner, From, Token, Op], []).

kickstart(Worker) ->
  gen_server:cast(Worker, start).

init([Owner, From, Token, Op]) ->
  process_flag(trap_exit, true),
  {ok, #state{owner=Owner,
              from=From,
              token=Token,
              op=Op}}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(start, State) ->
  #state{op=Op} = State,
  {xmlelement, "op", Attrs, _} = Op,
  handle_request(proplists:get_value("type", Attrs), State);

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(stop, State) ->
  {stop, normal, State};

handle_info({workflow_results, Results}, State) ->
  gen_actor:send_result(State#state.owner, State#state.from, State#state.token, Results),
  gen_actor:end_results(State#state.owner, State#state.from, State#state.token),
  {stop, normal, State};

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
handle_request("/workflow/store", State) ->
  {string, _, Workflow} = vertebra_xmpp:get_named_arg("workflow", State#state.op),
  case workflow_parser:parse_workflow(xml_util:unescape(Workflow)) of
    {ok, Workflows} ->
      ok = store_workflows(Workflows),
      {ok, OK} = xml_util:convert(to, ?OK),
      gen_actor:send_result(State#state.owner, State#state.from, State#state.token, OK);
    {error, Reason} ->
      gen_actor:send_error(State#state.owner, State#state.from, State#state.token, Reason)
  end,
  gen_actor:end_result(State#state.owner, State#state.from, State#state.token),
  {stop, normal, State};

handle_request("/workflow/execute", State) ->
  {string, _Attrs, WorkflowName} = vertebra_xmpp:get_named_arg("workflow", State#state.op),
  case invoke_workflow(WorkflowName) of
    ok ->
      {noreply, State};
    {error, Err} ->
      gen_actor:send_error(State#state.owner, State#state.from, State#state.token, Err),
      gen_actor:end_result(State#state.owner, State#state.from, State#state.token),
      {stop, normal, State}
  end.

store_workflows([H|T]) ->
  ok = workflow_store:store_workflow(H),
  store_workflows(T);
store_workflows([]) ->
  ok.

invoke_workflow(WorkflowName) when is_binary(WorkflowName)->
  invoke_workflow(binary_to_list(WorkflowName));
invoke_workflow(WorkflowName) when is_list(WorkflowName)->
  case workflow_store:find_workflow(WorkflowName) of
    {ok, Workflow} ->
      {ok, _} = workflow_runner:start_link(cavalcade_srv:get_config(), Workflow, self()),
      ok;
    _Error ->
      {error, "error locating workflow"}
  end.
