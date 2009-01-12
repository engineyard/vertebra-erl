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

-module(test_vertebra_util).

-include_lib("eunit/include/eunit.hrl").

-define(COMPLETE_CONFIG, [{host, "localhost"}, {user, "test"}, {resource, "test"}]).
-define(MISSING_RESOURCE_CONFIG, [{host, "localhost"}, {user, "test"}]).
-define(MISSING_USER_CONFIG, [{host, "localhost"}, {resource, "test"}]).
-define(MISSING_HOST_CONFIG, [{user, "test"}]).

jid_builder_test_() ->
  [?_assertMatch("test@localhost/test", vertebra_util:jid_from_config(?COMPLETE_CONFIG)),
   ?_assertMatch("test@localhost", vertebra_util:jid_from_config(?MISSING_RESOURCE_CONFIG)),
   ?_assertMatch(error, vertebra_util:jid_from_config(?MISSING_USER_CONFIG)),
   ?_assertMatch(error, vertebra_util:jid_from_config(?MISSING_HOST_CONFIG))].
