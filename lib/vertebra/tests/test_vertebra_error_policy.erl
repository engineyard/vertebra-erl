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

-module(test_vertebra_error_policy).

-include_lib("eunit/include/eunit.hrl").

-define(ERR_503, {xmlelement,"iq",
                  [{"from","not_found@localhost"},
                   {"to","herault@localhost/herault"},
                   {"type","error"},
                   {"xml:lang","en"},
                   {"id","100"}],
                  [{xmlelement,"ping",[{"xmlns","urn:xmpp:ping"}],[]},
                   {xmlelement,"error",
                    [{"code","503"},{"type","cancel"}],
                    [{xmlelement,"service-unavailable",
                      [{"xmlns","urn:ietf:params:xml:ns:xmpp-stanzas"}]}]}]}).

-define(ERR_404, {xmlelement,"iq",
                  [{"from","not_found@localhost"},
                   {"to","herault@localhost/herault"},
                   {"type","error"},
                   {"xml:lang","en"},
                   {"id","100"}],
                  [{xmlelement,"ping",[{"xmlns","urn:xmpp:ping"}],[]},
                   {xmlelement,"error",
                    [{"code","404"},{"type","wait"}],
                    [{xmlelement,"service-unavailable",
                      [{"xmlns","urn:ietf:params:xml:ns:xmpp-stanzas"}]}]}]}).

-define(ERR_500, {xmlelement,"iq",
                  [{"from","not_found@localhost"},
                   {"to","herault@localhost/herault"},
                   {"type","error"},
                   {"xml:lang","en"},
                   {"id","100"}],
                  [{xmlelement,"ping",[{"xmlns","urn:xmpp:ping"}],[]},
                   {xmlelement,"error",
                    [{"code","500"},{"type","cancel"}],
                    [{xmlelement,"internal-server-error",
                      [{"xmlns","urn:ietf:params:xml:ns:xmpp-stanzas"}]}]}]}).

-define(ERR_400, {xmlelement,"iq",
                  [{"from","not_found@localhost"},
                   {"to","herault@localhost/herault"},
                   {"type","error"},
                   {"xml:lang","en"},
                   {"id","100"}],
                  [{xmlelement,"ping",[{"xmlns","urn:xmpp:ping"}],[]},
                   {xmlelement,"error",
                    [{"code","400"},{"type","modify"}],
                    [{xmlelement,"bad-request",
                      [{"xmlns","urn:ietf:params:xml:ns:xmpp-stanzas"}]}]}]}).

-define(ERR_INCOMPLETE, {xmlelement,"iq",
                         [{"from","not_found@localhost"},
                          {"to","herault@localhost/herault"},
                          {"type","error"},
                          {"xml:lang","en"},
                          {"id","100"}],
                         [{xmlelement,"ping",[{"xmlns","urn:xmpp:ping"}],[]}]}).

-define(TEST_STANZA, {xmlelement, "iq", [{"from", "rd00-s0000@localhost/agent"},
                                          {"to", "foo@localhost"},
                                          {"id", "100"},
                                          {"type", "set"}], [{xmlelement, "op", [{"token", "abc:def"},
                                                                               {"type", "/security/verify"}], []}]}).




iq_errors_test_() ->
  [?_assertMatch(wait, vertebra_error_policy:analyze(?ERR_503)),
   ?_assertMatch(wait, vertebra_error_policy:analyze(?ERR_404)),
   ?_assertMatch(cancel, vertebra_error_policy:analyze(?ERR_500)),
   ?_assertMatch(cancel, vertebra_error_policy:analyze(?ERR_400)),
   ?_assertMatch(cancel, vertebra_error_policy:analyze(?ERR_INCOMPLETE))].

inspector_integration_test_() ->
  [fun() ->
       {ok, P} = vertebra_inspector:start_link("data/integration_inspector.xml", "herault@localhost/herault"),
       {replace, Replacement} = vertebra_inspector:inspect_inbound_stanza(P, ?TEST_STANZA),
       file:write_file("/tmp/test.txt", io_lib:format("~p~n", [Replacement])),
       ?assertMatch(wait, vertebra_error_policy:analyze(Replacement)) end].
