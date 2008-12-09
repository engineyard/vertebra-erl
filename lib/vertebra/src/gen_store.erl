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

-module(gen_store).

-include_lib("stdlib/include/qlc.hrl").

-export([behaviour_info/1, init/1, sq/2, sq/3, w/1, dk/2, do/1]).

-define(TQ, fun(Q) -> mnesia:transaction(fun() -> qlc:e(Q) end) end).
-define(TW, fun(R) -> mnesia:transaction(fun() -> mnesia:write(R) end) end).
-define(TDO, fun(R) -> mnesia:transaction(fun() -> mnesia:delete_object(R) end) end).
-define(TDK, fun(Tab, Key) -> mnesia:transaction(fun() -> mnesia:delete({Tab, Key}) end) end).

behaviour_info(callbacks) ->
  [{table_info, 0},
   {table_name, 0}].

init(StoreMod) ->
  TableInfo = StoreMod:table_info(),
  TableName = StoreMod:table_name(),
  TableAttrs = proplists:get_value(table_attrs, TableInfo),
  TableType = proplists:get_value(table_type, TableInfo),
  case TableName =:= undefined of
    true ->
      throw({missing_value, table_name});
    false ->
      case TableAttrs =:= undefined of
        true ->
          throw({missing_value, table_attrs});
        false ->
          try
            mnesia:table_info(TableName, cookie),
            ok
          catch
            exit: {aborted, _} ->
              MnesiaProps = [{disc_copies, [node()]},
                             {attributes, TableAttrs}],
              FinalMnesiaProps = case TableType of
                                   undefined ->
                                     MnesiaProps;
                                   V ->
                                     [{type, V}|MnesiaProps]
                                 end,
              case mnesia:create_table(TableName, FinalMnesiaProps) of
                {atomic, ok} ->
                  ok;
                Error ->
                  throw(Error)
              end
          end
      end
  end.

sq(StoreMod, Conditions) when is_list(Conditions) ->
  ?TQ(qlc:q([X || X <- mnesia:table(StoreMod:table_name()),
                  evaluate_conditions(Conditions, X)])).

sq(StoreMod, Transformer, Conditions) when is_list(Conditions) ->
  ?TQ(qlc:q([Transformer(X) || X <- mnesia:table(StoreMod:table_name()),
                               evaluate_conditions(Conditions, X)])).
w(Record) ->
  ?TW(Record).

do(Record) ->
  ?TDO(Record).

dk(StoreMod, EntryKey) ->
  ?TDK(StoreMod:table_name(), EntryKey).

%% Internal Functions
evaluate_conditions([H|T], X) when is_function(H) ->
  case H(X) of
    true ->
      evaluate_conditions(T, X);
    false ->
      false
  end;
evaluate_conditions([], _X) ->
  true.
