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

-module(test_entrepot_util).

-author("stesla@engineyard.com").

-include_lib("eunit/include/eunit.hrl").

ancestors_test_() ->
  generate([?_assertEqual(["/"], entrepot_util:ancestors("/")),
            ?_assertEqual(["/", "/foo"], entrepot_util:ancestors("/foo")),
            ?_assertEqual(["/", "/foo", "/foo/bar", "/foo/bar/baz"], entrepot_util:ancestors("/foo/bar/baz"))]).

is_ancestor_test_() ->
  generate([?_assertEqual(false, entrepot_util:is_ancestor("/foo", "/bar")),
            ?_assertEqual(true, entrepot_util:is_ancestor("/", "/foo")),
            ?_assertEqual(true, entrepot_util:is_ancestor("/foo", "/foo/bar/baz/quux")),
            ?_assertEqual(false, entrepot_util:is_ancestor("/foo", "/bar/foo/baz"))]).

get_child_test_() ->
  generate([fun() ->
                Xml = {xmlelement, "foo", [], []},
                ?assertEqual(undefined, entrepot_util:get_child("key", Xml))
            end]).

%% Private functions

generate(Tests) -> {setup,
                    fun() -> ok end,
                    fun(_) -> ok end,
                    Tests}.
