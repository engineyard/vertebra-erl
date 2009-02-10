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

-module(test_workflow_manager).

-author("ksmith@engineyard.com").

-include_lib("eunit/include/eunit.hrl").

-include("tests.hrl").

-define(MARSHALLED_STRING,
	{list,[{"name","workflow_results"}],
	 [{struct,[{"name","agent"},
		   {"from","rd00-s0000@localhost/agent"}],
	   {string,["name","response"],<<"hello">>}}]}).

-export([empty_startup/0, empty_teardown/1]).

empty_startup() ->
  ok.

empty_teardown(_) ->
  ok.

response_count_test_() ->
  test_util:new_bundle({?MODULE, empty_startup},
		       {?MODULE, empty_teardown},
		       [fun() ->
			    {ok, F} = file:read_file("../examples/workflow.xml"),
			    {ok, [WF]} = workflow_parser:parse_workflow(F),
			    C = workflow_manager:new(WF),
			    C1 = workflow_manager:response_count(1, C),
			    C2 = workflow_manager:decr_response_count(C1),
			    ?assertEqual(0, workflow_manager:response_count(C2)) end,
			%% Test response count never drops below 0
		       fun() ->
			   {ok, F} = file:read_file("../examples/workflow.xml"),
			   {ok, [WF]} = workflow_parser:parse_workflow(F),
			   C = workflow_manager:new(WF),
			   C1 = workflow_manager:response_count(1, C),
			   C2 = workflow_manager:decr_response_count(C1),
			   C3 = workflow_manager:decr_response_count(C2),
			   ?assertEqual(0, workflow_manager:response_count(C3)) end,
		       fun() ->
			   {ok, F} = file:read_file("../examples/workflow.xml"),
			   {ok, [WF]} = workflow_parser:parse_workflow(F),
			   C = workflow_manager:new(WF),
			   ?assertException(error, _, workflow_manager:response_count(-1, C)) end ]).
global_state_test_() ->
  test_util:new_bundle({?MODULE, empty_startup},
		       {?MODULE, empty_teardown},
		       [fun() ->
			    {ok, F} = file:read_file("../examples/workflow.xml"),
			    {ok, [WF]} = workflow_parser:parse_workflow(F),
			    String = {string, ["name", "response"], <<"hello">>},
			    C = workflow_manager:new(WF),
			    C1 = workflow_manager:global_state(String, C),
			    ?assertMatch(String, workflow_manager:global_state(C1)) end,
			 fun() ->
 			    {ok, F} = file:read_file("../examples/workflow.xml"),
 			    {ok, [WF]} = workflow_parser:parse_workflow(F),
 			    C = workflow_manager:new(WF),
 			    GS = workflow_manager:global_state(C),
 			    GS1 = dict:store("agent", {string, [{"name", "rd00-s0000@localhost/agent"}],
 						     <<"rd00-s0000@localhost/agent">>}, GS),
 			    C1 = workflow_manager:global_state(GS1, C),
 			   ?assertMatch(GS1, workflow_manager:global_state(C1)) end]).

marshalling_state_test_() ->
  test_util:new_bundle({?MODULE, empty_startup},
		       {?MODULE, empty_teardown},
		       [fun() ->
			    {ok, F} = file:read_file("../examples/workflow.xml"),
			    {ok, [WF]} = workflow_parser:parse_workflow(F),
			    String = {string, ["name", "response"], <<"hello">>},
			    C = workflow_manager:new(WF),
			    GS = workflow_manager:global_state(C),
			    GS1 = dict:store("agent", [{"rd00-s0000@localhost/agent", String}], GS),
			    C1 = workflow_manager:global_state(GS1, C),
			    ?assertMatch(?MARSHALLED_STRING, workflow_manager:marshal_global_state(C1)) end]).

advance_workflow_test_() ->
  test_util:new_bundle({?MODULE, empty_startup},
		       {?MODULE, empty_teardown},
		       [fun() ->
			    {ok, F} = file:read_file("../examples/workflow.xml"),
			    {ok, [WF]} = workflow_parser:parse_workflow(F),
			    C = workflow_manager:new(WF),
			    C1 = workflow_manager:response_count(1, C),
			    %% Should be true because we're advance to the first step/op
			    ?assertNot(C1 == workflow_manager:advance(C1)) end,
			fun() ->
			    {ok, F} = file:read_file("../examples/workflow.xml"),
			    {ok, [WF]} = workflow_parser:parse_workflow(F),
			    C = workflow_manager:new(WF),
			    C1 = workflow_manager:response_count(1, C),
			    C2 = workflow_manager:advance(C1),
			    %% try advancing again even though we didn't get a response
			    C3 = workflow_manager:advance(C2),
			    ?assertMatch(C2, C3) end,
			fun() ->
			    {ok, F} = file:read_file("../examples/workflow.xml"),
			    {ok, [WF]} = workflow_parser:parse_workflow(F),
			    C = workflow_manager:new(WF),
			    C1 = workflow_manager:response_count(1, C),
			    C2 = workflow_manager:advance(C1),
			    C3 = workflow_manager:decr_response_count(C2),
			    %% try advancing again even though we didn't get a response
			    C4 = workflow_manager:advance(C3),
			    ?assertNot(C3 =:= C4) end]).
