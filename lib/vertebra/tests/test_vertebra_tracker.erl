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

-module(test_vertebra_tracker).

-include_lib("eunit/include/eunit.hrl").

-define(TARGET_JID, "rd00-s0000@localhost/agent").

-define(TEST_STANZA, {xmlelement, "iq", [{"from", "rd00-s0000@localhost/agent"},
                                          {"to", "foo@localhost"},
                                          {"id", "100"},
                                          {"type", "set"}], [{xmlelement, "op", [{"token", "abc:def"},
                                                                               {"type", "/security/verify"}], []}]}).


happy_path_test_() ->
  [{setup, fun start_tracker/0,
    fun stop_tracker/1,
    [?_assertMatch(online, vertebra_tracker:get_status(vert_tracker, ?TARGET_JID)),
     ?_assertMatch(ok, vertebra_tracker:set_status(vert_tracker, ?TARGET_JID, offline)),
     fun() ->
         vertebra_tracker:reset(vert_tracker),
         ?assertMatch(online, vertebra_tracker:get_status(vert_tracker, ?TARGET_JID)),
         ?assertMatch(ok, vertebra_tracker:set_status(vert_tracker, ?TARGET_JID, offline)),
         ?assertMatch(offline, vertebra_tracker:get_status(vert_tracker, ?TARGET_JID)) end,
     fun() ->
         vertebra_tracker:reset(vert_tracker),
         ?assertMatch(online, vertebra_tracker:get_status(vert_tracker, ?TARGET_JID)),
         ?assertMatch(ok, vertebra_tracker:set_status(vert_tracker, ?TARGET_JID, offline)),
         ?assertMatch(ok, vertebra_tracker:buffer_stanza(vert_tracker, ?TARGET_JID, ?TEST_STANZA)) end,
     fun() ->
         vertebra_tracker:reset(vert_tracker),
         ?assertMatch(online, vertebra_tracker:get_status(vert_tracker, ?TARGET_JID)),
         ?assertMatch(ok, vertebra_tracker:set_status(vert_tracker, ?TARGET_JID, offline)),
         ?assertMatch(ok, vertebra_tracker:buffer_stanza(vert_tracker, ?TARGET_JID, ?TEST_STANZA)),
         ?assertMatch(?TEST_STANZA, vertebra_tracker:get_buffered_stanza(vert_tracker, ?TARGET_JID)) end,
     fun() ->
         vertebra_tracker:reset(vert_tracker),
         ?assertMatch(online, vertebra_tracker:get_status(vert_tracker, ?TARGET_JID)),
         ?assertMatch(ok, vertebra_tracker:set_status(vert_tracker, ?TARGET_JID, offline)),
         ?assertMatch(ok, vertebra_tracker:buffer_stanza(vert_tracker, ?TARGET_JID, ?TEST_STANZA)),
         ?assertMatch(?TEST_STANZA, vertebra_tracker:get_buffered_stanza(vert_tracker, ?TARGET_JID)),
         ?assertMatch(ok, vertebra_tracker:set_status(vert_tracker, ?TARGET_JID, online)),
         ?assertMatch(not_found, vertebra_tracker:get_buffered_stanza(vert_tracker, ?TARGET_JID)) end]}].

sad_path_test_() ->
  [{setup, fun start_tracker/0,
    fun stop_tracker/1,
    [?_assertMatch(ok, vertebra_tracker:buffer_stanza(vert_tracker, ?TARGET_JID, ?TEST_STANZA)),
     ?_assertMatch({error, already_buffered}, vertebra_tracker:buffer_stanza(vert_tracker, ?TARGET_JID, ?TEST_STANZA)),
     fun() ->
         vertebra_tracker:reset(vert_tracker),
         ?assertMatch(ok, vertebra_tracker:set_status(vert_tracker, ?TARGET_JID, online)) end]}].


start_tracker() ->
  {ok, Pid} = vertebra_tracker:start_link(),
  erlang:register(vert_tracker, Pid),
  Pid.

stop_tracker(Pid) ->
  exit(Pid, shutdown).
