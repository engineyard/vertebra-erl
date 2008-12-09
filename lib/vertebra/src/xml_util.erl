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

-module(xml_util).

-author("stesla@engineyard.com").

-export([convert/2, get_type/1, escape/1, unescape/1]).

-define(XML_TOKENS, [{"&lt;", "<"}, {"&gt;", ">"}]).

convert(Direction, Elt) when is_list(Elt) ->
  {ok, lists:map(fun(X) ->
                     {ok, Result} = convert(Direction, X),
                     Result end, Elt)};
convert(from, {xmlelement, Name, Attrs, Children}) when is_list(Name) ->
  {ok, convert_element(from, {list_to_atom(Name), Attrs, Children})};
convert(to, {Name, Attrs, Children}) ->
  {ok, convert_element(to, {Name, Attrs, Children})}.

escape(Text) ->
  xmerl_lib:export_text(xmerl_ucs:from_utf8(Text)).

unescape(Text) when is_binary(Text) ->
  unescape(binary_to_list(Text));

unescape(Text) ->
  unescape(Text, ?XML_TOKENS).

unescape(Text, [H|T]) ->
  unescape(replace_token(Text, H), T);
unescape(Text, []) ->
  Text.

get_type({base64, _, _}) ->
  base64;
get_type({boolean, _, _}) ->
  boolean;
get_type({dateTime.iso8601, _, _}) ->
  dateTime.iso8601;
get_type({double, _, _}) ->
  double;
get_type({i4, _, _}) ->
  integer;
get_type({list, _, _}) ->
  list;
get_type({string, _, _}) ->
  string;
get_type({struct, _, _}) ->
  struct;
get_type({nil, _, _}) ->
  nil;
get_type({resource, _, _}) ->
  resource;
get_type(_Other) ->
  undefined.

%% Base64
convert_element(from, {base64, Attrs, [{xmlcdata, Value}]}) ->
  {base64, Attrs, Value};
convert_element(to, {base64, Attrs,  Value}) when is_list(Value) ->
  {xmlelement, "base64", Attrs, [{xmlcdata, list_to_binary(Value)}]};
convert_element(to, {base64, Attrs,  Value}) ->
  {xmlelement, "base64", Attrs, [{xmlcdata, Value}]};

%% Booleans
convert_element(from, {boolean, Attrs, [{xmlcdata, <<"0">>}]}) ->
  {boolean, Attrs, false};
convert_element(from, {boolean, Attrs, [{xmlcdata, <<"1">>}]}) ->
  {boolean, Attrs, true};
convert_element(to, {boolean, Attrs, false}) ->
  {xmlelement, "boolean", Attrs, [{xmlcdata, <<"0">>}]};
convert_element(to, {boolean, Attrs, true}) ->
  {xmlelement, "boolean", Attrs, [{xmlcdata, <<"1">>}]};

%% Date/Time
                                                % This is just getting stored so that it can be spit back out on the wire. So,
                                                % aside from the type annotation, it doesn't need any more conversion.
convert_element(from, {dateTime.iso8601, Attrs, [{xmlcdata, Value}]}) ->
  {dateTime.iso8601, Attrs, Value};
convert_element(to, {dateTime.iso8601, Attrs, Value}) when is_list(Value)->
  {xmlelement, "dateTime.iso8601", Attrs, [{xmlcdata, list_to_binary(Value)}]};
convert_element(to, {dateTime.iso8601, Attrs, Value}) ->
  {xmlelement, "dateTime.iso8601", Attrs, [{xmlcdata, Value}]};

                                                %Doubles
convert_element(from, {double, Attrs, [{xmlcdata, Value}]}) ->
  {Double,[]} = string:to_float(binary_to_list(Value)),
  {double, Attrs, Double};
convert_element(to, {double, Attrs, Value}) ->
  Result = lists:flatten(io_lib:format("~f", [Value])),
  {xmlelement, "double", Attrs, [{xmlcdata, list_to_binary(Result)}]};

%% Integers
%% This is the only conversion which calls out to other
%% private helper methods. This is necessary since we support
%% multiple tag names to denote integers ('i4', 'int', and 'integer')
convert_element(from, {i4, AttrsList, Children}) ->
  convert_integer(from, AttrsList, Children);
convert_element(to, {i4, AttrsList, Children}) ->
  convert_integer(to, AttrsList, Children);
convert_element(from, {int, AttrsList, Children}) ->
  convert_integer(from, AttrsList, Children);
convert_element(to, {int, AttrsList, Children}) ->
  convert_integer(to, AttrsList, Children);
convert_element(from, {integer, AttrsList, Children}) ->
  convert_integer(from, AttrsList, Children);
convert_element(to, {integer, AttrsList, Children}) ->
  convert_integer(to, AttrsList, Children);

%% Strings
convert_element(from, {string, Attrs, [{xmlcdata, Value}]}) ->
  {string, Attrs, Value};
convert_element(from, {string, Attrs, []}) ->
  {string, Attrs, <<"">>};
convert_element(to, {string, Attrs, Value}) when is_list(Value) ->
  {xmlelement, "string", Attrs, [{xmlcdata, list_to_binary(Value)}]};
convert_element(to, {string, Attrs, Value}) ->
  {xmlelement, "string", Attrs, [{xmlcdata, Value}]};

%% Lists
convert_element(from, {list, Attrs, Values}) ->
  {ok, Result} = convert(from, Values),
  {list, Attrs, Result};
convert_element(to, {list, Attrs, Values}) ->
  {ok, Result} = convert(to, Values),
  {xmlelement, "list", Attrs, Result};

%% Structs
convert_element(from, {struct, Attrs, Values}) ->
  {ok, Result} = convert(from, Values),
  {struct, Attrs, Result};
convert_element(to, {struct, Attrs, Values}) ->
  {ok, Result} = convert(to, Values),
  {xmlelement, "struct", Attrs, Result};

%% Nils (Null values)
convert_element(from, {nil, Attrs, _Values}) ->
  {nil, Attrs, nil};
convert_element(to, {nil, Attrs, _Values}) ->
  {xmlelement, "nil", Attrs, []};

%% Resources
convert_element(from, {res, Attrs, [{xmlcdata, Value}]}) ->
  {resource, Attrs, Value};
convert_element(to, {resource, Attrs, Value}) ->
  {xmlelement, "res", Attrs, [{xmlcdata, Value}]}.

%% Helper functions
convert_integer(from, Attrs, [{xmlcdata, Value}]) ->
  {Int,[]} = string:to_integer(binary_to_list(Value)),
  {integer, Attrs, Int};
convert_integer(to, Attrs, Value) ->
  Result = lists:flatten(io_lib:format("~B", [Value])),
  {xmlelement, "i4", Attrs, [{xmlcdata, list_to_binary(Result)}]}.

replace_token(Text, {Match, Replacement}) ->
  EndOfText = length(Text) - length(Match),
  case string:str(Text, Match) of
    0 ->
      Text;
    EndOfText ->
      Rest = string:substr(Text, 1, (length(Text) - length(Match) - 1)),
      lists:flatten([Rest, Replacement]);
    1 ->
      Rest = string:substr(Text, 1  + length(Match)),
      replace_token(lists:flatten([Replacement, Rest]), {Match, Replacement});
    Start ->
      Front = string:substr(Text, 1, Start - 1),
      Back = string:substr(Text, Start + length(Match)),
      replace_token(lists:flatten([Front, Replacement, Back]), {Match, Replacement})
  end.
