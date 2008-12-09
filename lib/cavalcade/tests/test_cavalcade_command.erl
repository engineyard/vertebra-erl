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

-module(test_cavalcade_command).

-author("ksmith@engineyard.com").

-include_lib("eunit/include/eunit.hrl").

-include("tests.hrl").

run_command_test_() ->
  test_util:new_bundle({test_util, setup},
		       ?TEARDOWN,
		       [fun() ->
			    {ok, XMPP} = fake_xmpp:start_link(),
			    cavalcade_command:run("/security/discover",
						  [],
						  uuid_server:generate_uuid(),
						  "herault@localhost/herault",
						  {pid, self()},
						  XMPP),
			    {ok, []} = fake_xmpp:extract(),
			    fake_xmpp:stop() end]).
