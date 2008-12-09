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

-module(workflow_parser).

-author("ksmith@engineyard.com").

-export([parse_file/1, parse_workflow/1, test/0]).

-include("cavalcade.hrl").

test() ->
  workflow_parser:parse_file("examples/workflow.xml").

parse_file(FileName) ->
  {ok, Contents} = file:read_file(FileName),
  parse_workflow(Contents).

parse_workflow(Workflow) ->
  case erlsom:parse_sax(Workflow, [[], []], fun(Event, Acc) -> handle_event(Event, Acc) end) of
    {ok, Workflows, _} ->
      {ok, Workflows};
    Error ->
      Error
  end.

handle_event({startElement, _, "workflow", _, Attrs}, [Current, Stack]) ->
  [Current, [new_workflow(Attrs)|Stack]];

handle_event({startElement, _, "op", _, Attrs}, [Current, Stack]) ->
  [[new_op(Attrs)|Current], Stack];

handle_event({endElement, _, "op", _}, [[Op|Steps], Stack]) ->
  [SH|ST] = Steps,
  [[SH#workflow_step{ops=[Op|SH#workflow_step.ops]}|ST], Stack];

handle_event({startElement, _, "string", _, Attrs}, [Current, Stack]) ->
  [[[convert_attrs(Attrs), string]|Current], Stack];

handle_event({startElement, _, "res", _, Attrs}, [Current, Stack]) ->
  [[[convert_attrs(Attrs), resource]|Current], Stack];

handle_event({endElement, _, Name, _}, [[CH|CT], Stack]) when Name =:= "string" orelse
                                                              Name =:= "res" orelse
                                                              Name =:= "import" ->
  [NH|NT] = CT,
  FinalCH = list_to_tuple(lists:reverse(CH)),
  NewNH = case NH of
            {list, Attrs, Children} ->
              {list, Attrs, [FinalCH|Children]};
            {workflow_op, _, _, _} ->
              NH#workflow_op{inputs=[FinalCH|NH#workflow_op.inputs]}
          end,
  [[NewNH|NT], Stack];

handle_event({startElement, _, "list", _, Attrs}, [Current, Stack]) ->
  [[{list, convert_attrs(Attrs), []}|Current], Stack];

handle_event({endElement, _, "list", _}, [[{list, Attrs, Children}|Current], Stack]) ->
  [Op|Steps] = Current,
  UpdatedOp = Op#workflow_op{inputs=[{list, Attrs, Children}|Op#workflow_op.inputs]},
  [[UpdatedOp|Steps], Stack];

handle_event({characters, Text}, [[CH|CT], Stack]=State) ->
  case string:strip(Text) of
    "\n" ->
      State;
    "" ->
      State;
    Value ->
      [[[Value|CH]|CT], Stack]
  end;

handle_event({startElement, _, "state", _, Attrs}, [Current, Stack]) ->
  [[new_step(Attrs)|Current], Stack];

handle_event({endElement, _, "state", _}, [[CH|CT], [WH|WT]]) ->
  UpdatedStep = CH#workflow_step{ops=lists:reverse(CH#workflow_step.ops)},
  UpdatedWorkflow = WH#workflow{steps=[UpdatedStep|WH#workflow.steps]},
  [CT, [UpdatedWorkflow|WT]];

handle_event({startElement, _, "output", _, _Attrs}, [Current, Stack]) ->
  [[[]|Current], Stack];

handle_event({endElement, _, "output", _}, [[Outputs|T], Stack]) ->
  [Op|Steps] = T,
  UpdatedOp = Op#workflow_op{outputs=lists:flatten([Outputs|Op#workflow_op.outputs])},
  [[UpdatedOp|Steps], Stack];

handle_event({startElement, _, Name, _, Attrs}, [[Outputs|T], Stack]) when Name =:= "all" orelse
                                                                           Name =:= "first" orelse
                                                                           Name =:= "last" orelse
                                                                           Name =:= "avg" ->
  NewOutputs = [{list_to_atom(Name), convert_attrs(Attrs)}|Outputs],
  [[NewOutputs|T], Stack];

handle_event({startElement, _, "transition", _, Attrs}, [Current, Stack]) ->
  [[[get_attribute("type", Attrs)]|Current], Stack];

handle_event({endElement, _, "transition", _}, [Current, Stack]) ->
  [CH|CT] = Current,
  [SH|ST] = CT,
  UpdatedStep = case CH of
                  [Target, Name] ->
                    SH#workflow_step{transitions=[{Name, Target}|SH#workflow_step.transitions]};
                  ["end"] ->
                    SH#workflow_step{transitions=[{"end", []}]}
                end,
  [[UpdatedStep|ST], Stack];

handle_event({endElement, _, "workflow"}, [Current, [WH|WT]]) ->
  UpdatedWorkflow = WH#workflow{steps=lists:reverse(WH#workflow.steps)},
  [Current, [UpdatedWorkflow|WT]];

handle_event({startElement, _, "import", _, Attrs}, [Current, Stack]) ->
  [[[convert_attrs(Attrs), import]|Current], Stack];

handle_event(endDocument, [_Current, Stack]) ->
  Stack;

handle_event(_Event, State) ->
  State.

new_workflow(Attrs) ->
  #workflow{name=get_attribute("name", Attrs),
            start=get_attribute("start", Attrs)}.

new_step(Attrs) ->
  #workflow_step{name=get_attribute("name", Attrs)}.

new_op(Attrs) ->
  #workflow_op{op=get_attribute("type", Attrs)}.

convert_attrs(Attrs) ->
  lists:map(fun({attribute, Name, _, _, Value}) ->
                {Name, Value} end, Attrs).

get_attribute(Name, [{attribute, Name, _, _, Value}|_T]) ->
  Value;
get_attribute(Name, [_H|T]) ->
  get_attribute(Name, T);
get_attribute(_Name, []) ->
  undefined.
