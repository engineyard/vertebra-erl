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

-module(test_workflow_parser).

-author("ksmith@engineyard.com").

-include_lib("eunit/include/eunit.hrl").
-include("tests.hrl").
-include("cavalcade.hrl").

easy_parse_test_() ->
  test_util:new_bundle({test_util, setup},
		       ?TEARDOWN,
		       [fun() ->
			    {ok, [WF]} = workflow_parser:parse_workflow(read_file("workflow.xml", false)),
			    ?assertEqual(WF#workflow.name, "list_gems") end]).

escape_parse_test_() ->
  test_util:new_bundle({test_util, setup},
		       ?TEARDOWN,
		       [fun() ->
			    {ok, [WF]} = workflow_parser:parse_workflow(xml_util:unescape(read_file("escaped_workflow.txt", false))),
			    ?assertEqual(WF#workflow.name, "list_gems") end,
			fun() ->
			   {ok, [WF]} = workflow_parser:parse_workflow(xml_util:unescape(read_file("workflow.xml", true))),
			   ?assertEqual(WF#workflow.name, "list_gems") end]).

empty_parse_test_() ->
  test_util:new_bundle({test_util, setup},
		       ?TEARDOWN,
		       [fun() ->
			    {ok, [WF]} = workflow_parser:parse_workflow(read_file("empty_workflow.xml", false)),
			    ?assertEqual(length(WF#workflow.steps), 0),
			    ?assertEqual(WF#workflow.name, "list_gems") end]).

easy_validate_test_() ->
  test_util:new_bundle({test_util, setup},
		       ?TEARDOWN,
		       [fun() ->
			    ValidationInfo = [{step_count, 2},
					      {start_step, "find_slices"},
					      {workflow_name, "list_gems"},
					      {"find_slices",
					       [{op_count, 1},
						{"find_slices-/security/discover",
						 [{op, "/security/discover"}]}]},
					      {"get_gems",
					       [{op_count, 1},
						{"get_gems-/list",
						 [{op, "/list"}]}]}],
			    ?assertEqual(ok, validate_workflow(ValidationInfo, build_test_workflow("workflow.xml"))) end]).

build_test_workflow(Filename) ->
  F = read_file(Filename, true),
  {ok, [WF]} = workflow_parser:parse_workflow(xml_util:unescape(F)),
  WF.

read_file(Name, Escape) ->
  {ok, F} = file:read_file("./examples/" ++ Name),
  case Escape of
    true ->
      xml_util:escape(F);
    false ->
      F
  end.

validate_workflow(Info, Workflow) ->
  ?assertEqual(proplists:get_value(workflow_name, Info), Workflow#workflow.name),
  ?assertEqual(proplists:get_value(step_count, Info),
		 length(Workflow#workflow.steps)),
   lists:foreach(fun(Step) ->
		     validate_step(proplists:get_value(Step#workflow_step.name, Info),
				   Step) end,
		 Workflow#workflow.steps).

validate_step(undefined, Step) ->
  throw({untested_step, Step#workflow_step.name});
validate_step(StepInfo, Step) ->
  ?assertEqual(proplists:get_value(op_count, StepInfo),
	       length(Step#workflow_step.ops)),
  lists:foreach(fun(Op) ->
		    validate_op(proplists:get_value(Step#workflow_step.name ++ "-" ++ Op#workflow_op.op, StepInfo), Op) end,
		Step#workflow_step.ops).

validate_op(OpInfo, Op) ->
  ?assertEqual(proplists:get_value(op, OpInfo), Op#workflow_op.op).
