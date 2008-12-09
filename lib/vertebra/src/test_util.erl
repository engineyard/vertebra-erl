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

-module(test_util).

-author("ksmith@engineyard.com").

-export([new_bundle/2, new_bundle/3, setup/0, teardown/1, setup_env/1]).

setup() ->
  case application:get_application(sasl) of
    undefined ->
      application:start(sasl);
    {ok, sasl} ->
      ok
  end,
  mnesia:create_schema([node()]),
  mnesia:start(),
  ok.

setup_env(Calls) ->
  setup(),
  lists:foreach(fun({M, F, A}) -> apply(M, F, A) end, Calls).

teardown(_Token) ->
  mnesia:stop(),
  mnesia:delete_schema([node()]).

new_bundle({_, _}=SetupMFA, Tests) ->
  new_bundle(SetupMFA, {}, Tests);
new_bundle(SetupFun, Tests) when is_function(SetupFun) ->
  {setup,
   SetupFun,
   build_teardown_fun({}),
   Tests}.

new_bundle(SetupFun, TeardownMFA, Tests) when is_function(SetupFun) ->
  {setup,
   SetupFun,
   build_teardown_fun(TeardownMFA),
   Tests};
new_bundle({SetupMod, SetupFun}, TeardownMFA, Tests) ->
  {setup,
   fun() -> SetupMod:SetupFun() end,
   build_teardown_fun(TeardownMFA),
   Tests}.
%% Internal Functions
build_teardown_fun({}) ->
  fun(_Token) -> ok end;
build_teardown_fun({TeardownMod, TeardownFun}) ->
  fun(Token) -> TeardownMod:TeardownFun(Token) end.
