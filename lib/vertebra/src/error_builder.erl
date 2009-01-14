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

-module(error_builder).

-author("ksmith@engineyard.com").

-export([iq_error/3]).

-include("xml.hrl").

-define(DEFAULT_ATTRS, [{"xmlns", "urn:ietf:params:xml:ns:xmpp-stanzas"}]).
-define(ERROR_TYPES, [{400, [#xmlelement{name="bad-request",
                                         attrs=?DEFAULT_ATTRS},
                             "modify"]},
                      {403, [#xmlelement{name="forbidden",
                                         attrs=?DEFAULT_ATTRS},
                             "auth"]},
                      {404, [#xmlelement{name="recipient-unavailable",
                                         attrs=?DEFAULT_ATTRS},
                             "wait"]},
                      {500, [#xmlelement{name="undefined-condition",
                                         attrs=?DEFAULT_ATTRS},
                             "cancel"]},
                      {503, [#xmlelement{name="service-unavailable",
                                         attrs=?DEFAULT_ATTRS},
                             "cancel"]}].

iq_error(Code, From, To) ->
  case proplists:get_value(Code, ?ERROR_TYPES) of
    undefined ->
      no_err_template;
    [ErrorElement, ErrorType] ->
      Attrs = case From =:= undefined of
                true ->
                  [{"to", To},
                   {"type", "error"}];
                false ->
                  [{"from", From},
                   {"to", To},
                   {"type", "error"}]
              end,
      #xmlelement{name="iq",
                  attrs=Attrs,
                  sub_el=[#xmlelement{name="error",
                                      attrs=[{"type", ErrorType},
                                             {"code", integer_to_list(Code)}],
                                      sub_el=[ErrorElement]}]}
  end.
