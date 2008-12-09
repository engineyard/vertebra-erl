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

-module(test_entrepot_store).

-author("stesla@engineyard.com").

-include_lib("eunit/include/eunit.hrl").
-include("models.hrl").
-include("test.hrl").

-define(_KEY1, [{<<"cluster">>, <<"/cluster/42">>},
                {<<"slices">>, <<"/slices">>}]).
-define(KEY1, sets:from_list(?_KEY1)).
-define(KEY2, sets:from_list([{<<"cluster">>, <<"/cluster/42">>}])).
-define(KEY3, sets:from_list([{<<"slices">>, <<"/slices">>}])).

setup() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  ok = entrepot_store:init(),
  entrepot_store:store(?KEY1, <<"<foo/>">>),
  entrepot_store:store(?KEY2, <<"<bar/>">>),
  entrepot_store:store(?KEY3, <<"<quux/>">>),
  ok.

teardown() ->
  mnesia:stop(),
  mnesia:delete_schema([node()]),
  ok.

init_test_() ->
  generate([?_assertEqual(set, mnesia:table_info(item, type))]).

create_test_() ->
  Key = sets:from_list([{<<"foo">>,<<"/foo">>},
                        {<<"bar">>,<<"/bar">>}]),
  generate([?_assertEqual({atomic, ok}, entrepot_store:store(Key, <<"<some-xml/>">>))]).

find_test_() ->
  generate([?_assertFetch([], [{<<"not">>, <<"/not">>}, {<<"there">>, <<"/there">>}]),
            ?_assertFetch([], [{<<"cluster">>, <<"/cluster/42">>},
                               {<<"whocares">>, <<"/notthere">>}]),
            ?_assertFetch([#item{key = ?KEY1, value = <<"<foo/>">>}],
                          [{<<"cluster">>, <<"/cluster/42">>},
                           {<<"slices">>, <<"/slices">>}]),
            ?_assertFetch([#item{key = ?KEY1, value = <<"<foo/>">>}],
                          [{<<"slices">>, <<"/slices">>},
                           {<<"cluster">>, <<"/cluster/42">>}]),
            ?_assertFetch([#item{key = ?KEY1, value = <<"<foo/>">>},
                           #item{key = ?KEY2, value = <<"<bar/>">>}],
                          [{<<"cluster">>, <<"/cluster/42">>}]),
            ?_assertFetch([#item{key = ?KEY1, value = <<"<foo/>">>},
                           #item{key = ?KEY2, value = <<"<bar/>">>}],
                          [{<<"cluster">>, <<"/cluster">>}])]).

delete_test_() ->
  generate([fun() ->
                {ok, Results} = entrepot_store:delete(?KEY1),
                ?assertEqual([#item{key = ?KEY1, value = <<"<foo/>">>}], Results),
                ?assertFetch([], ?_KEY1)
            end]).

%% Private functions

generate(Tests) -> {setup,
                    fun() -> setup() end,
                    fun(_Opts) -> teardown() end,
                    Tests}.
