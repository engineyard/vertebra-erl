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

-module(test_workflow_store).

-author("ksmith@engineyard.com").

-include_lib("eunit/include/eunit.hrl").
-include("tests.hrl").
-include("cavalcade.hrl").

add_workflow_test_() ->
  test_util:new_bundle(?SETUP_ENV([{workflow_store, init, []}]),
		       ?TEARDOWN,
		       [fun() ->
			    WF = #workflow{name="foo"},
			    ok = workflow_store:store_workflow(WF) end]).

find_workflow_test_() ->
  test_util:new_bundle(?SETUP_ENV([{workflow_store, init, []}]),
		       ?TEARDOWN,
			[fun() ->
			     WF = #workflow{name="bar"},
			     ok = workflow_store:store_workflow(WF),
			     {ok, WF1} = workflow_store:find_workflow("bar"),
			     ?assertEqual(WF#workflow.name, WF1#workflow.name) end]).
