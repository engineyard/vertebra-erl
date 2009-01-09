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

-module(test_vertebra_inspector_config).

-include_lib("eunit/include/eunit.hrl").

good_parse_test_() ->
  [?_assertMatch({ok,[{rules,[{"slownet",
                               {rule,"slownet","delay","5","10",0.0}}]},
                      {inspections,[{"authstutter",
                                      {inspection,"authstutter","all",undefined,
                                       "herault@localhost/herault","repeat","1","1",
                                       0.0}},
                                    {"catchall",
                                     {inspection,"catchall","all","slownet",undefined,undefined,
                                      0,0,0.0}}]}]}, vertebra_inspector_config:read("data/good_inspector.xml")),
  ?_assertThrow({error, bad_rule_def, _}, vertebra_inspector_config:read("data/rule_missing_id.xml")),
  ?_assertThrow({error, bad_rule_def, _}, vertebra_inspector_config:read("data/rule_missing_min_max_percent.xml")),
  ?_assertThrow({error, bad_rule_def, _}, vertebra_inspector_config:read("data/rule_missing_behavior.xml")),
  ?_assertThrow({error, bad_inspection_def, _}, vertebra_inspector_config:read("data/inspect_missing_id.xml")),
  ?_assertThrow({error, bad_inspection_def, _}, vertebra_inspector_config:read("data/inspect_missing_behavior.xml")),
  ?_assertThrow({error, bad_inspection_def, _}, vertebra_inspector_config:read("data/inspect_missing_rule.xml")),
  ?_assertThrow({error, bad_inspection_def, _}, vertebra_inspector_config:read("data/inspect_missing_min_max_percent.xml"))].
