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

-module(vertebra_error_policy).

-include("typespecs.hrl").

%% This module analyzes XMPP and Vertebra errors and determines what action
%% should be taken.

-export([analyze/1]).

% Analyze an incoming stanza and determine what action to take
-spec(analyze/1 :: ({_, _, Attrs :: proplist(), Subels :: list()}) -> ok | wait | cancel | atom()).
analyze({xmlelement, "iq", Attrs, Subels}) ->
  case proplists:get_value("type", Attrs) of
    "error" ->
      analyze_iq_error(Subels);
    _ ->
      analyze_vertebra_stanza(Subels)
  end.

%% Internal functions
analyze_iq_error(Subels) ->
  case find_error_element(Subels) of
    not_found ->
      cancel;
    Error ->
      {Code, Severity, Type} = extract_iq_error_data(Error),
      if
        Code =:= "503" andalso Type =:= "service-unavailable" ->
          wait;
        true ->
          case Severity of
            "modify" ->
              cancel;
            _ ->
              list_to_atom(Severity)
          end
      end
  end.

analyze_vertebra_stanza(Subels) ->
  case find_error_element(Subels) of
    not_found ->
      ok;
    _ ->
      cancel
  end.

extract_iq_error_data({xmlelement, "error", Attrs, Subels}) when length(Subels) > 0 ->
  Code = proplists:get_value("code", Attrs),
  Severity = proplists:get_value("type", Attrs, "cancel"),
  ErrorType = case Subels of
                [{xmlelement, Type, _}] ->
                  Type;
                [{xmlelement, Type, _, _}] ->
                  Type
              end,
  {Code, Severity, ErrorType}.

find_error_element([{xmlelement, "error", _, _}=H|_T]) ->
  H;
find_error_element([_|T]) ->
  find_error_element(T);
find_error_element([]) ->
  not_found.
