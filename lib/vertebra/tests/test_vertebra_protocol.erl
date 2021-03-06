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

-module(test_vertebra_protocol).

-include_lib("xml.hrl").
-include_lib("eunit/include/eunit.hrl").

valid_cancel_op_test_() ->
  [{setup, fun() -> vertebra_protocol:error("cancel", "must retry", "foo:bar") end,
    fun(ErrOp) ->
	generate_valid_cancel_op_tests(ErrOp) end}].


valid_error_test_() ->
  [{setup, fun() -> vertebra_protocol:error("something bad", "foo:bar") end,
    fun(ErrOp) ->
	generate_valid_error_tests(ErrOp) end}].

valid_ack_test_() ->
  [{setup, fun() -> vertebra_protocol:ack("foo:bar") end,
    fun generate_valid_ack_tests/1}].

valid_partial_data_test_() ->
  [{setup, fun() -> vertebra_protocol:data([], "foo:bar") end,
    fun(Op) ->
	generate_valid_result_tests("partial", Op, 0) end}].

valid_final_data_test_() ->
  [{setup, fun() -> vertebra_protocol:data([], "foo:bar") end,
    fun(Op) ->
	generate_valid_result_tests("partial", Op, 0) end}].

valid_list_data_test_() ->
  [{setup, fun() -> vertebra_protocol:data({xmlelement, "list", [{"name", "resources"}],
					   [{xmlelement, "res", [], [{xmlcdata, <<"/cluster/rd00">>}]}]}, "foo:bar") end,
    fun(Op) ->
	generate_valid_result_tests("partial", Op, 1) end}].

non_list_result_test_() ->
  [{setup,
    fun() -> vertebra_protocol:data({xmlelement, "string", [], [{xmlcdata, <<"/cluster/rd00">>}]}, "foo:bar") end,
    fun(Op) ->
	generate_valid_result_tests("partial", Op, 1) end}].

generate_valid_result_tests(_Type, Result, ExpectedSubEls) ->
  [?_assertEqual("data", Result#xmlelement.name),
   ?_assertEqual("foo:bar", proplists:get_value("token", Result#xmlelement.attrs)),
   ?_assertEqual(ExpectedSubEls, length(Result#xmlelement.sub_el)),
   ?_assertEqual({xmlelement, "iq", [{"type", "result"}, {"xml:lang", "en"}, {"id", "42"}],
                  [Result]}, vertebra_protocol:finalize(Result, "result", "42"))].

generate_valid_ack_tests(Ack) ->
  [?_assertEqual("ack", Ack#xmlelement.name),
   ?_assertEqual("foo:bar", proplists:get_value("token", Ack#xmlelement.attrs)),
   ?_assertEqual(0, length(Ack#xmlelement.sub_el)),
   fun() ->
      FAck = vertebra_protocol:finalize(Ack, "result", "42"),
      ?assertEqual({xmlelement, "iq", [{"type", "result"}, {"xml:lang", "en"}, {"id", "42"}], [Ack]}, FAck) end].

generate_valid_cancel_op_tests(Error) ->
  [?_assertEqual("error", Error#xmlelement.name),
   ?_assertEqual("foo:bar", proplists:get_value("token", Error#xmlelement.attrs)),
   ?_assertEqual("cancel", proplists:get_value("type", Error#xmlelement.attrs)),
   ?_assertMatch([{xmlelement, "string",
		  [{"name", "reason"}],
		  [{xmlcdata, <<"must retry">>}]}], Error#xmlelement.sub_el)].


generate_valid_error_tests(Error) ->
  [?_assertEqual("error", Error#xmlelement.name),
   ?_assertEqual("foo:bar", proplists:get_value("token", Error#xmlelement.attrs)),
   ?_assertEqual("error", proplists:get_value("type", Error#xmlelement.attrs)),
   ?_assertMatch([{xmlelement, "string",
		  [{"name", "reason"}],
		  [{xmlcdata, <<"something bad">>}]}], Error#xmlelement.sub_el)].
