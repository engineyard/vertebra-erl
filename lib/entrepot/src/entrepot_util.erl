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

-module(entrepot_util).

-author("stesla@engineyard.com").

-export([ancestors/1, get_child/2, get_element/2, is_ancestor/2]).

-define(PATH_SEPARATOR, "/").

ancestors(Resource) when is_list(Resource) ->
  Tokens = string:tokens(Resource, ?PATH_SEPARATOR),
  ancestors(lists:reverse(Tokens), []).
ancestors([], Acc) ->
  ["/" | Acc];
ancestors([H|T], Acc) ->
  Ancestor = string:join([""|lists:reverse(T)] ++ [H], "/"),
  ancestors(T, [Ancestor | Acc]).

get_element(Name, Els) ->
  F = fun({xmlelement, _, Attrs, _}) ->
          proplists:get_value("name", Attrs) =:= Name
      end,
  case lists:filter(F, Els) of
    [Elt] -> Elt;
    _ -> undefined
  end.

get_child(Name, {xmlelement, _Name, _Attrs, Subels}) ->
  get_element(Name, Subels).

is_ancestor(Ancestor, Resource) when is_list(Ancestor), is_list(Resource) ->
  string:str(Resource, Ancestor) =:= 1.
