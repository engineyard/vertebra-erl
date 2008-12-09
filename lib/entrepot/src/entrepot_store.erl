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

-module(entrepot_store).

-author("stesla@engineyard.com").

-behavior(gen_store).

-include("models.hrl").

-export([init/0]).
-export([delete/1, delete/2, fetch/1, fetch/2, store/2]).

%% gen_store callbacks
-export([table_info/0, table_name/0]).

%% Private API
-export([collect_results/1, collect_callback/2]).

-define(TABLE, item).

init() ->
  gen_store:init(?MODULE).

delete(Key) ->
  synchronize(fun delete/2, Key).

delete(Key, Callback) ->
  CallbackFun = fun(Item) ->
                    mnesia:delete_object(Item),
                    callback_with_record(Item, Callback)
                end,
  proper_subset_query(Key, CallbackFun).

fetch(Key) ->
  synchronize(fun fetch/2, Key).

fetch(Key, {_M, _F, _Pid} = Callback) ->
  proper_subset_query(Key, Callback).

store(Key, Value) ->
  Item = #item{key=Key, value=Value},
  mnesia:transaction(fun() -> mnesia:write(Item) end).

table_name() ->
  ?TABLE.

table_info() ->
  [{table_attrs, record_info(fields, ?TABLE)}].

%% Private Functions

ancestor_set({Name, Resource}) ->
  Ancestors = entrepot_util:ancestors(binary_to_list(Resource)),
  NamedAncestors = lists:map(fun(X) -> {Name, list_to_binary(X)} end, Ancestors),
  sets:from_list(NamedAncestors).

callback_with_record(Record, Callback) when is_function(Callback) ->
  Callback(Record);
callback_with_record(Record, {M, F, Args}) ->
  apply(M, F, [Record | Args]).

collect_callback(Result, Pid) ->
  Pid ! {result, Result}.

collect_results(Results) ->
  receive
    {result, Result} ->
      collect_results([Result | Results]);
    {return, From} ->
      From ! {self(), Results}
  end.

is_proper_subset(Term, Key) ->
  AncestorsList = lists:map(fun ancestor_set/1, sets:to_list(Key)),
  Ancestors = sets:union(AncestorsList),
  case sets:intersection(Term, Ancestors) of
    Term -> true;
    _ -> false
  end.

proper_subset_query(Key, Callback) ->
  mnesia:transaction(fun() ->
                         First = mnesia:first(?TABLE),
                         proper_subset_query(Key, First, Callback)
                     end).

proper_subset_query(_Key, '$end_of_table', _Callback) ->
  ok;
proper_subset_query(Key, Current, Callback) ->
  case catch is_proper_subset(Key, Current) of
    true ->
      [Record] = mnesia:read({?TABLE, Current}),
      callback_with_record(Record, Callback);
    _ ->
      ignore
  end,
  Next = mnesia:next(?TABLE, Current),
  proper_subset_query(Key, Next, Callback).

return_results(Pid) ->
  Pid ! {return, self()},
  receive
    {Pid, Result} ->
      {ok, Result}
  after 1000 ->
      {error, timeout}
  end.

synchronize(Function, Key) ->
  Pid = proc_lib:spawn_link(?MODULE, collect_results, [[]]),
  {atomic, ok} = Function(Key, {?MODULE, collect_callback, [Pid]}),
  return_results(Pid).
