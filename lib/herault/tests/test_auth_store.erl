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

-module(test_auth_store).

-author("ksmith@engineyard.com").

-include_lib("eunit/include/eunit.hrl").
-include("tests.hrl").

-define(TEST_ENV, [{auth_store, init, ["./example/herault.cfg"]},
		   {auth_store, add_entry, [?ENTITY_1]},
		   {auth_store, add_entry, [?ENTITY_2]}]).

add_entry_test_() ->
  test_util:new_bundle(?SETUP_ENV([{auth_store, init, ["./example/herault.cfg"]}]),
		       ?TEARDOWN,
		       [fun() ->
			    ok = auth_store:add_entry(?ENTITY_1) end,
			fun() ->
			   mnesia:delete_table(hauth_entry),
			   {aborted, _Reason} = auth_store:add_entry(?ENTITY_1) end]).

has_access_test_() ->
  test_util:new_bundle(?SETUP_ENV(?TEST_ENV),
		       ?TEARDOWN,
		       [fun() ->
			    true = auth_store:has_access(?ENTITY_1) end,
			fun() ->
			    false = auth_store:has_access(?ENTITY_3) end,
			fun() ->
			    mnesia:delete_table(hauth_entry),
			   {aborted, _Reason} = auth_store:has_access(?ENTITY_1) end]).

delete_entry_test_() ->
  test_util:new_bundle(?SETUP_ENV(?TEST_ENV),
		       ?TEARDOWN,
		       [fun() ->
			    ok = auth_store:delete_entry(?ENTITY_1),
			    ok = auth_store:delete_entry(?ENTITY_3) end,
			fun() ->
			    mnesia:delete_table(hauth_entry),
			    {aborted, _Reason} = auth_store:delete_entry(?ENTITY_2) end]).
