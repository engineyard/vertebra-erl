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

-module(vertebra_protocol).

-export([op/1, nack/1, ack/1, final/1, data/2, error/2, error/3, finalize/3]).

-export([make_resource_list/1]).

-include("xml.hrl").

finalize(Op, OpType, Id) when OpType == "get" orelse
                              OpType == "set" orelse
                              OpType == "error" orelse
                              OpType == "result" ->
  Attrs = [{"type", OpType},
           {"xml:lang", "en"},
           {"id", Id}],
  #xmlelement{name="iq", attrs=Attrs, sub_el=[Op]}.

error(Reason, Token) when is_list(Reason) ->
  error("error", Reason, Token).

error(Type, Reason, Token) when is_list(Reason) ->
  {ok, ReasonEl} = xml_util:convert(to, {string, [{"name", "reason"}], list_to_binary(Reason)}),
  #xmlelement{name="error", attrs=[{"token", Token},
                                   {"xmlns", ?AGENT_NS},
                                   {"type", Type}], sub_el=[ReasonEl]}.

ack(Token) ->
  #xmlelement{name="ack", attrs=[{"token",Token},
                                 {"xmlns", ?AGENT_NS}], sub_el=[]}.

nack(Token) ->
  #xmlelement{name="nack", attrs=[{"token",Token},
                                  {"xmlns", ?AGENT_NS}], sub_el=[]}.

final(Token) ->
  #xmlelement{name="final", attrs=[{"token",Token},
                                   {"xmlns", ?AGENT_NS}], sub_el=[]}.

data({xmlelement, "list", _, _} = Results, Token) ->
  Attrs = [{"token", Token}, {"xmlns", ?AGENT_NS}],
  {xmlelement, "data", Attrs, [Results]};
data(Results, Token) when is_list(Results) ->
  Attrs = [{"token", Token}, {"xmlns", ?AGENT_NS}],
  {xmlelement, "data", Attrs, build_subels(Results, [])};
data(Results, Token) ->
  data([Results], Token).

%%
%% op({OpName, SubEls}) -> #xmlelement
%% OpName -> string
%% SubEls -> [SubEl]
%% SubEl -> {Name, Attrs, [SubEl] | []} | {text, Text}
%%
op({OpName, Token, SubEls}) ->
  Attrs = [{"type", OpName}, {"token", Token}, {"xmlns", ?AGENT_NS}],
  #xmlelement{name="op", attrs=Attrs, sub_el=build_subels(SubEls, [])}.

build_subels([{text, Text}|T], Accum) ->
  build_subels(T, lists:append(Accum, [#xmlcdata{data=Text}]));
build_subels([{xmlelement, _, _, _}=H|T], Accum) ->
  build_subels(T, lists:append(Accum, [H]));
build_subels([{ElName, ElAttrs, ElSubEls}|T], Accum) ->
  SubEls = build_subels(ElSubEls, []),
  build_subels(T, lists:append(Accum, [#xmlelement{name=ElName, attrs=ElAttrs, sub_el=SubEls}]));
build_subels([], Accum) ->
  Accum.

make_resource_list(Resources) ->
  lists:map(fun(R) -> {xmlelement, "res", [], [{xmlcdata, R}]} end, Resources).
