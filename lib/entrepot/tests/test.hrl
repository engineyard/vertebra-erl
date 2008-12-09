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

%%% Test Assertions

-define(assertFetch(Value, Key),
        ?assertEqual({ok, Value},
                     entrepot_store:fetch(sets:from_list(Key)))).

-define(_assertFetch(Value, Key),
        fun() -> ?assertFetch(Value, Key) end).

%%% Ops and various values that go with them to keep the tests cleaner

-define(DELETE_OP, {xmlelement, "op", [{"type", "/entrepot/delete"}],
                   [{xmlelement, "struct", [{"name", "key"}],
                     [{xmlelement, "res", [{"name", "cluster"}],
                       [{xmlcdata, <<"/cluster/42">>}]},
                      {xmlelement, "res", [{"name", "slice"}],
                       [{xmlcdata, <<"/slice/15">>}]}]}]}).

-define(DELETE_KEY, [{<<"cluster">>,<<"/cluster/42">>},
                    {<<"slice">>,<<"/slice/15">>}]).

-define(DELETE_RESULT, [{xmlelement, "struct", [{"name","key"}],
                        [{xmlelement, "res", [{"name","slice"}],
                          [{xmlcdata, <<"/slice/15">>}]},
                         {xmlelement, "res", [{"name","cluster"}],
                          [{xmlcdata, <<"/cluster/42">>}]}]},
                       {xmlelement, "struct", [{"name","value"}],
                        [{xmlelement, "i4",[{"name","count"}],[{xmlcdata,<<"42">>}]}]}]).

-define(DELETE_VALUE, {struct, [{"name","value"}],
                      [{integer, [{"name","count"}], 42}]}).

-define(FETCH_OP, {xmlelement, "op", [{"type", "/entrepot/fetch"}],
                   [{xmlelement, "struct", [{"name", "key"}],
                     [{xmlelement, "res", [{"name", "cluster"}],
                       [{xmlcdata, <<"/cluster/42">>}]},
                      {xmlelement, "res", [{"name", "slice"}],
                       [{xmlcdata, <<"/slice/15">>}]}]}]}).

-define(FETCH_KEY, [{<<"cluster">>,<<"/cluster/42">>},
                    {<<"slice">>,<<"/slice/15">>}]).

-define(FETCH_RESULT, [{xmlelement, "struct", [{"name","key"}],
                        [{xmlelement, "res", [{"name","slice"}],
                          [{xmlcdata, <<"/slice/15">>}]},
                         {xmlelement, "res", [{"name","cluster"}],
                          [{xmlcdata, <<"/cluster/42">>}]}]},
                       {xmlelement, "struct", [{"name","value"}],
                        [{xmlelement, "i4",[{"name","count"}],[{xmlcdata,<<"42">>}]}]}]).

-define(FETCH_VALUE, {struct, [{"name","value"}],
                      [{integer, [{"name","count"}], 42}]}).

-define(STORE_OP, {xmlelement, "op", [{"type", "/entrepot/store"}],
                      [{xmlelement, "struct", [{"name", "key"}],
                        [{xmlelement, "res", [{"name", "cluster"}],
                          [{xmlcdata, <<"/cluster/42">>}]},
                         {xmlelement, "res", [{"name", "slice"}],
                          [{xmlcdata, <<"/foo">>}]}]},
                       {xmlelement, "struct", [{"name", "value"}],
                        [{xmlelement, "i4", [{"name", "count"}],
                         [{xmlcdata, <<"42">>}]}]}]}).

-define(STORE_RESULT, [{xmlelement,"struct", [{"name","key"}],
                        [{xmlelement,"res", [{"name","slice"}],
                          [{xmlcdata,<<"/foo">>}]},
                         {xmlelement,"res", [{"name","cluster"}],
                          [{xmlcdata,<<"/cluster/42">>}]}]},
                       {xmlelement,"struct", [{"name","value"}],
                        [{xmlelement,"i4", [{"name","count"}],
                          [{xmlcdata,<<"42">>}]}]}]).

-define(STORE_KEY, [{<<"slice">>,<<"/foo">>},
                    {<<"cluster">>,<<"/cluster/42">>}]).

-define(STORE_VALUE, {struct, [{"name","value"}], [{integer, [{"name","count"}], 42}]}).
