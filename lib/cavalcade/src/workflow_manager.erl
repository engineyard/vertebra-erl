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

-module(workflow_manager).

-author("ksmith@engineyard.com").

-record(workflow_context,
        {workflow,
         current_step,
         current_op,
         response_count,
         outputs,
         state=dict:new()}).

-include("cavalcade.hrl").

-export([advance/1, current_step/1, current_op/1, current_output/1, global_state/1, global_state/2, marshal_global_state/1]).
-export([new/1, response_count/1, response_count/2, decr_response_count/1]).

current_step(Context) ->
  Context#workflow_context.current_step.

current_op(Context) ->
  Context#workflow_context.current_op.

current_output(Context) ->
  Context#workflow_context.outputs.

marshal_global_state(Context) ->
  #workflow_context{state=GlobalState} = Context,
  Data = dict:fold(fun(Name, Value, AccIn) ->
                       lists:merge([convert_result_pair(Name, V) || V <- Value], AccIn) end,
                   [], GlobalState),
  {list, [{"name", "workflow_results"}], Data}.

global_state(Context) ->
  Context#workflow_context.state.

global_state(State, Context) ->
  Context#workflow_context{state=State}.

new(Workflow) when is_tuple(Workflow) ->
  #workflow_context{workflow=Workflow}.

response_count(Context) ->
  Context#workflow_context.response_count.

response_count(Count, Context) when Count > -1 ->
  Context#workflow_context{response_count=Count}.

decr_response_count(Context) ->
  if
    Context#workflow_context.response_count == 0 ->
      Context;
    true ->
      Context#workflow_context{response_count = Context#workflow_context.response_count - 1}
  end.


advance(Context) ->
  case Context#workflow_context.current_step of
    %% Handle new workflow
    undefined ->
      C1 = next_step(Context),
      next_op(C1);
    %% Workflow is complete, return unmodified context
    complete ->
      Context;
    _Step ->
      if
        Context#workflow_context.response_count > 0 ->
          Context;
        true ->
          %% Got a step, find the next op
          C1 = next_op(Context),
          if
            %% No more ops for this step
            C1 =:= Context ->
              advance(next_step(C1));
            true ->
              C1
          end
      end
  end.


%% Internal functions
next_step(Context) ->
  #workflow_context{current_step=CurrentStep, workflow=Workflow} = Context,
  file:write_file("/tmp/dump.txt", io_lib:format("~p~n", [Workflow])),
  case Context#workflow_context.current_step of
    %% Handle workflow start - No current step selected
    undefined ->
      case find_step(Workflow#workflow.start, Workflow#workflow.steps) of
        undefined ->
          exit({missing_step, Workflow#workflow.start});
        %% Handle case where workflow is done
        complete ->
          Context;
        Step ->
          Context#workflow_context{current_step=Step}
      end;
    %% Look up next step based on where we're at in the workflow
    _ ->
      case proplists:get_value("default", CurrentStep#workflow_step.transitions) of
        undefined ->
          case proplists:get_value("end", CurrentStep#workflow_step.transitions) of
            undefined ->
              %% If the step doesn't specify a default or end transition, toss an exception
              throw({missing_step, "default or end"});
            _ ->
              %% If we're at the end step, set the current step to complete
              Context#workflow_context{current_step=complete}
          end;
        %% We found a step to select -- let's look it up
        StepName ->
          case find_step(StepName, Workflow#workflow.steps) of
            %% The next step wasn't found, toss an exception
            undefined ->
              throw({missing_step, StepName});
            %% Found the next step, update the context
            Step ->
              Context#workflow_context{current_step=Step}
          end
      end
  end.

next_op(Context) ->
  case Context#workflow_context.current_step of
    %% Workflow not started yet
    undefined ->
      Context;
    %% Workflow is done
    complete ->
      Context;
    Step ->
      case length(Step#workflow_step.ops) of
        %% No more ops, return unmodified context
        0 ->
          Context;
        %% We have more ops
        %% Dequeue the next one and set it as the current op
        _ ->
          #workflow_step{ops=[Op|T]} = Step,
          NewStep = Step#workflow_step{ops=T},
          Context#workflow_context{current_op=Op, current_step=NewStep, outputs=Op#workflow_op.outputs}
      end
  end.

find_step(Name, [H|T]) ->
  case H#workflow_step.name =:= Name of
    true ->
      H;
    false ->
      find_step(Name, T)
  end;
find_step(_Name, []) ->
  undefined.

convert_result_pair(Name, {From, Data}) ->
  {struct, [{"name", Name},
            {"from", From}], Data}.
