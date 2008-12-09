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

-define(ENTITY_1, "foo@rd00.engineyard.com/user").
-define(ENTITY_2, "rd00-s0001@rd00.engineyard.com/agent").
-define(ENTITY_3, "rd00-s0000@rd00.engineyard.com/agent").

-define(AGENT_1_JID, "rd00-s0000@rd00.engineyard.com/agent").
-define(AGENT_2_JID, "rd00-s0001@rd00.engineyard.com/agent").
-define(ALL_AGENTS, [?AGENT_1_JID, ?AGENT_2_JID]).

-define(AGENT_1_RESOURCES, ["/gem", "/node", "/cluster/rd00"]).
-define(AGENT_1_RESOURCES_BAD, ["/gem", "/node", "/cluster/rd01"]).

-define(AGENT_2_RESOURCES, ["/cpu", "/node", "/cluster/rd00"]).
-define(AGENT_RESOURCE, "/xen").

-define(TEARDOWN, {test_util, teardown}).
-define(SETUP_ENV, fun(Env) -> fun() -> test_util:setup_env(Env) end end).
