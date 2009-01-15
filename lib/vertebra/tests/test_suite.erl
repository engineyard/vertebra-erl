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

-module(test_suite).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  [{module, test_xml_util},
   {module, test_uuid},
   {module, test_ops_builder},
   {module, test_vertebra_xmpp},
   {module, test_vertebra_inspector_config},
   {module, test_vertebra_util},
   {module, test_error_builder}].
