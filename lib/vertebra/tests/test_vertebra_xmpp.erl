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

-module(test_vertebra_xmpp).

-include_lib("eunit/include/eunit.hrl").

-define(ARG, {xmlelement, "string", [{"name", "target"}], [{xmlcdata, <<"testing">>}]}).
-define(ARG_DECODED, xml_util:convert(from, ?ARG)).

-define(ARG_OP, {xmlelement, "op", [], [?ARG]}).
-define(NOARG_OP, {xmlelement, "op", [], []}).

build_resources_test_() ->
  generate([
	    ?_assertEqual([{resource, [{"name", "test"}], <<"test">>}], vertebra_xmpp:build_resources(["test"])),
	    ?_assertEqual([], vertebra_xmpp:build_resources([])),
	    ?_assertEqual("abc", vertebra_xmpp:get_token([{xmlelement, "test", [{"token", "abc"}], []}])),
	    ?_assertEqual(undefined, vertebra_xmpp:get_token([{xmlelement, "test", [{"not_token", "abc"}], []}])),
	    ?_assertEqual(?ARG_DECODED, {ok, vertebra_xmpp:get_named_arg("target", ?ARG_OP)}),
	    ?_assertEqual(not_found, vertebra_xmpp:get_named_arg("target", ?NOARG_OP))]).

%% Inernal functions
generate(Tests) -> {setup,
                    fun() -> ok end,
                    fun(_) -> ok end,
                    Tests}.
