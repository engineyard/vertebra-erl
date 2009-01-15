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

-module(test_error_builder).

-include_lib("eunit/include/eunit.hrl").

-export([generate_tests/0]).

-define(FROM_JID, "rd00-s0000@localhost/agent").
-define(TO_JID, "herault@localhost/herault").
-define(TEST_VALUES, [{400, "modify", "bad-request"},
                      {403, "auth", "forbidden"},
                      {404, "wait", "recipient-unavailable"},
                      {500, "cancel", "undefined-condition"},
                      {503, "cancel", "service-unavailable"}]).

iq_error_test_() ->
  [{generator, fun generate_tests/0},
   [fun() -> ?assertMatch(no_err_template, error_builder:iq_error(302, ?FROM_JID, ?TO_JID)) end]].

vert_error_test_() ->
  [fun() -> test_vert_error({"Testing",
                             undefined,
                             ?TO_JID,
                             123,
                             "result",
                             "123456789:123456789"},
                            error_builder:vert_error("Testing", {undefined, 123, "result"}, "123456789:123456789", "herault@localhost/herault")) end].

test_vert_error({Message, undefined, To, Id, StanzaType, Token}, VertError) when is_list(Message) ->
  ErrMessage = list_to_binary(Message),
  Target = {xmlelement,"iq",
            [{"to", To},
             {"type", StanzaType},
             {"xml:lang", "en"},
             {"id", Id}],
            [{xmlelement,"error",
              [{"token", Token},
               {"xmlns", "http://xmlschema.engineyard.com/agent/api"},
               {"type", "error"}],
              [{xmlelement, "string",
                [{"name", "reason"}],
                [{xmlcdata, ErrMessage}]}]}]},
  ?assertMatch(Target, VertError).


test_iq_error({Code, ErrorType, ErrorTagName, undefined, To}, IQError) ->
  ErrorCode = integer_to_list(Code),
  Target = {xmlelement,
            "iq",
            [{"to", To},
             {"type", "error"}],
            [{xmlelement,
              "error",
              [{"type", ErrorType},
               {"code", ErrorCode}],
               [{xmlelement, ErrorTagName, [{"xmlns","urn:ietf:params:xml:ns:xmpp-stanzas"}], []}]}]},
  ?assertMatch(Target, IQError);
test_iq_error({Code, ErrorType, ErrorTagName, From, To}, IQError) ->
  ErrorCode = integer_to_list(Code),
  Target = {xmlelement,
            "iq",
            [{"from", From},
             {"to", To},
             {"type", "error"}],
            [{xmlelement,
              "error",
              [{"type", ErrorType},
               {"code", ErrorCode}],
               [{xmlelement, ErrorTagName, [{"xmlns","urn:ietf:params:xml:ns:xmpp-stanzas"}], []}]}]},
  ?assertMatch(Target, IQError).

generate_tests() ->
  generate_tests(?TEST_VALUES, []).

generate_tests([{Code, ErrorType, TagName}|T], Accum) ->
  TestFun = fun() -> test_iq_error({Code, ErrorType, TagName, undefined, ?TO_JID}, error_builder:iq_error(Code, undefined, ?TO_JID)),
                     test_iq_error({Code, ErrorType, TagName, ?FROM_JID, ?TO_JID}, error_builder:iq_error(Code, ?FROM_JID, ?TO_JID)) end,
  generate_tests(T, [TestFun|Accum]);
generate_tests([], Accum) ->
  lists:reverse(Accum).
