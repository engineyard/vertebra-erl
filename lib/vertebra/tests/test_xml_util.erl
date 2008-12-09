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

-module(test_xml_util).

-author("stesla@engineyard.com").

-include_lib("eunit/include/eunit.hrl").

-export([suite/0]).

-define(_fromScalar(Expected, Type, Value),
        ?_assertEqual({ok, Expected},
                      xml_util:convert(from,
                                       {xmlelement, Type, [], [{xmlcdata, Value}]}))).

-define(_toScalar(From, XmlType, XmlValue),
        ?_assertEqual({ok, {xmlelement, XmlType, [], [{xmlcdata, XmlValue}]}},
                      xml_util:convert(to, From))).

-define(EMPTY, []).

suite() ->
  [from_test_(),
   to_test_(),
   escape_test_()].

escape_test_() ->
  generate([?_assertEqual("&lt;foo&gt;", xml_util:escape("<foo>")),
	    ?_assertEqual("<foo>", xml_util:unescape("&lt;foo&gt;")),
	    ?_assertEqual("<foo>", xml_util:unescape(xml_util:escape("<foo>"))),
	    ?_assertEqual("<foo><test /></foo>", xml_util:unescape("&lt;foo&gt;&lt;test /&gt;&lt;/foo&gt;")),
	    ?_assertEqual("<foo id=\"wibble\">", xml_util:unescape("&lt;foo id=\"wibble\"&gt;")),
	    ?_assertEqual("<foo id=\"wibble\"><test /></foo>", xml_util:unescape(xml_util:escape("<foo id=\"wibble\"><test /></foo>")))]).

from_test_() ->
  generate([?_assertEqual({ok, []}, xml_util:convert(from, [])),
            ?_fromScalar({string, ?EMPTY, <<"42">>}, "string", <<"42">>),
            ?_fromScalar({integer, ?EMPTY, 42}, "int", <<"42">>),
            ?_fromScalar({integer, ?EMPTY, 42}, "i4", <<"42">>),
            ?_fromScalar({boolean, ?EMPTY, true}, "boolean", <<"1">>),
            ?_fromScalar({boolean, ?EMPTY, false}, "boolean", <<"0">>),
            ?_fromScalar({double, ?EMPTY, -42.42}, "double", <<"-42.42">>),
            ?_fromScalar({base64, ?EMPTY, <<"foo">>}, "base64", <<"foo">>),
            ?_fromScalar({dateTime.iso8601, ?EMPTY, <<"bar">>},
                         "dateTime.iso8601", <<"bar">>),
            ?_fromScalar({resource, ?EMPTY, <<"/foo/bar">>}, "res", <<"/foo/bar">>),
            ?_assertEqual({ok, {nil, ?EMPTY, nil}},
                          xml_util:convert(from, {xmlelement, "nil", [], []})),
            ?_assertEqual({ok, {list, ?EMPTY, []}},
                          xml_util:convert(from, {xmlelement, "list", [], []})),
            ?_assertEqual({ok, {struct, ?EMPTY, []}},
                          xml_util:convert(from, {xmlelement, "struct", [], []})),
            fun() ->
                Attrs = [{"foo", "bar"},
                         {"baz", "quux"}],
                ?assertEqual({ok, {nil, Attrs, nil}},
                             xml_util:convert(from, {xmlelement, "nil", Attrs, []}))
            end
           ]).

to_test_() ->
  generate([?_assertEqual({ok, []}, xml_util:convert(to, [])),
            ?_toScalar({string, ?EMPTY, <<"42">>}, "string", <<"42">>),
            ?_toScalar({integer, ?EMPTY, 42}, "i4", <<"42">>),
            ?_toScalar({boolean, ?EMPTY, true}, "boolean", <<"1">>),
            ?_toScalar({boolean, ?EMPTY, false}, "boolean", <<"0">>),
            ?_toScalar({double, ?EMPTY, -42.42}, "double", <<"-42.420000">>),
            ?_toScalar({base64, ?EMPTY, <<"foo">>}, "base64", <<"foo">>),
            ?_toScalar({dateTime.iso8601, ?EMPTY, <<"bar">>},
                        "dateTime.iso8601", <<"bar">>),
            ?_toScalar({resource, ?EMPTY, <<"/foo/bar">>}, "res", <<"/foo/bar">>),
            ?_assertEqual({ok, {xmlelement, "list", [], []}},
                          xml_util:convert(to, {list, ?EMPTY, []})),
            ?_assertEqual({ok, {xmlelement, "nil", [], []}},
                          xml_util:convert(to, {nil, ?EMPTY, nil})),
            ?_assertEqual({ok, {xmlelement, "struct", [], []}},
                          xml_util:convert(to, {struct, ?EMPTY, []})),
            fun() ->
                Attrs = [{"foo", "bar"},
                         {"baz", "quux"}],
                ?assertEqual({ok, {xmlelement, "nil", Attrs, []}},
                             xml_util:convert(to, {nil, Attrs, nil}))
            end
           ]).

%% Private functions

generate(Tests) -> {setup,
                    fun() -> ok end,
                    fun(_) -> ok end,
                    Tests}.
