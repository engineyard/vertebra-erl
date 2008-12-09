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

-module(test_ops_builder).

-include_lib("xml.hrl").
-include_lib("eunit/include/eunit.hrl").

valid_cancel_op_test_() ->
  [{setup, fun() -> ops_builder:error_op("cancel", "must retry", "foo:bar") end,
    fun(ErrOp) ->
	generate_valid_cancel_op_tests(ErrOp) end}].


valid_error_op_test_() ->
  [{setup, fun() -> ops_builder:error_op("something bad", "foo:bar") end,
    fun(ErrOp) ->
	generate_valid_error_op_tests(ErrOp) end}].

valid_ack_op_test_() ->
  [{setup, fun() -> ops_builder:ack_op("foo:bar") end,
    fun generate_valid_ack_op_tests/1}].

valid_partial_result_op_test_() ->
  [{setup, fun() -> ops_builder:result_op([], "foo:bar") end,
    fun(Op) ->
	generate_valid_result_tests("partial", Op, 0) end}].

valid_final_result_op_test_() ->
  [{setup, fun() -> ops_builder:result_op([], "foo:bar") end,
    fun(Op) ->
	generate_valid_result_tests("partial", Op, 0) end}].

valid_list_result_op_test_() ->
  [{setup, fun() -> ops_builder:result_op({xmlelement, "list", [{"name", "resources"}],
					   [{xmlelement, "res", [], [{xmlcdata, <<"/cluster/rd00">>}]}]}, "foo:bar") end,
    fun(Op) ->
	generate_valid_result_tests("partial", Op, 1) end}].

non_list_result_test_() ->
  [{setup,
    fun() -> ops_builder:result_op({xmlelement, "string", [], [{xmlcdata, <<"/cluster/rd00">>}]}, "foo:bar") end,
    fun(Op) ->
	generate_valid_result_tests("partial", Op, 1) end}].

generate_valid_result_tests(_Type, Result, ExpectedSubEls) ->
  [?_assertEqual("result", Result#xmlelement.name),
   ?_assertEqual("foo:bar", proplists:get_value("token", Result#xmlelement.attrs)),
   ?_assertEqual(ExpectedSubEls, length(Result#xmlelement.sub_el)),
   ?_assertEqual({xmlelement, "iq", [{"type", "result"}, {"xml:lang", "en"}, {"id", "42"}],
                  [Result]}, ops_builder:finalize(Result, "result", "42"))].

generate_valid_ack_op_tests(Ack) ->
  [?_assertEqual("ack", Ack#xmlelement.name),
   ?_assertEqual("foo:bar", proplists:get_value("token", Ack#xmlelement.attrs)),
   ?_assertEqual(0, length(Ack#xmlelement.sub_el)),
   fun() ->
      FAck = ops_builder:finalize(Ack, "result", "42"),
      ?assertEqual({xmlelement, "iq", [{"type", "result"}, {"xml:lang", "en"}, {"id", "42"}], [Ack]}, FAck) end].

generate_valid_cancel_op_tests(Error) ->
  [?_assertEqual("error", Error#xmlelement.name),
   ?_assertEqual("foo:bar", proplists:get_value("token", Error#xmlelement.attrs)),
   ?_assertEqual("cancel", proplists:get_value("type", Error#xmlelement.attrs)),
   ?_assertMatch([{xmlelement, "string",
		  [{"name", "reason"}],
		  [{xmlcdata, <<"must retry">>}]}], Error#xmlelement.sub_el)].


generate_valid_error_op_tests(Error) ->
  [?_assertEqual("error", Error#xmlelement.name),
   ?_assertEqual("foo:bar", proplists:get_value("token", Error#xmlelement.attrs)),
   ?_assertEqual("error", proplists:get_value("type", Error#xmlelement.attrs)),
   ?_assertMatch([{xmlelement, "string",
		  [{"name", "reason"}],
		  [{xmlcdata, <<"something bad">>}]}], Error#xmlelement.sub_el)].
