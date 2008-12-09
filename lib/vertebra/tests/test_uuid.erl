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

-module(test_uuid).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

suite() ->
  [uniqueness_test_()].

uniqueness_test_() ->
  Children = 50,
  generate({timeout, 240, [fun() ->
			       uuid_server:start_link(),
			       start_children(Children, Children),
			       R = gather_results(Children, Children),
			       R1 = lists:sort(R),
			       find_dups(R1, 1),
			       start_children(Children, Children),
			       R2 = gather_results(Children, Children),
			       R21 = lists:sort(R2),
			       find_dups(R21, 1),
			       case length(sets:to_list(sets:intersection(sets:from_list(R1), sets:from_list(R21)))) > 0 of
 				 true ->
 				   exit("Duplicates found across multiple runs");
 				 false ->
 				   ok
 			       end,
			       ok end]}).
find_dups(Results, Position) when Position > length(Results) ->
  ok;

find_dups(Results, Position)  ->
  F = lists:nth(Position, Results),
  case Position + 1 > length(Results) of
    true ->
      ok;
    false ->
      S = lists:nth(Position + 1, Results),
      case F =:= S of
	true ->
	  exit("Duplicate UUID found!");
	false ->
	  find_dups(Results, Position + 1)
      end
  end.

gather_results(ChildCount, IdCount) when is_integer(IdCount) ->
  gather_results(ChildCount * IdCount, []);

gather_results(0, Accum) when is_list(Accum) ->
  Accum;

gather_results(ItemCount, Accum) when is_list(Accum) ->
  receive
    {uuid, U} ->
      gather_results(ItemCount - 1, lists:append(Accum, [U]))
  end.

start_children(0, _IdCount) ->
  ok;

start_children(Count, IdCount) ->
  ParentPid = self(),
  spawn(fun() ->
	    generate_uuid(IdCount, ParentPid) end),
  start_children(Count - 1, IdCount).

generate_uuid(0, _Parent) ->
  ok;

generate_uuid(Count, Parent) ->
  U = uuid_server:generate_uuid(),
  Parent ! {uuid, U},
  generate_uuid(Count - 1, Parent).

generate(Tests) -> {setup,
                    fun() -> ok end,
                    fun(_) -> ok end,
                    Tests}.
