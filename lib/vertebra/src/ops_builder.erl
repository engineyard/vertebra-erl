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

-module(ops_builder).

-export([generic_op/1, nack_op/1, ack_op/1, final_op/1, result_op/2, error_op/2, error_op/3, finalize/3]).

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

error_op(Reason, Token) when is_list(Reason) ->
  error_op("error", Reason, Token).

error_op(Type, Reason, Token) when is_list(Reason) ->
  {ok, ReasonEl} = xml_util:convert(to, {string, [{"name", "reason"}], list_to_binary(Reason)}),
  #xmlelement{name="error", attrs=[{"token", Token},
                                   {"xmlns", ?AGENT_NS},
                                   {"type", Type}], sub_el=[ReasonEl]}.

ack_op(Token) ->
  #xmlelement{name="ack", attrs=[{"token",Token},
                                 {"xmlns", ?AGENT_NS}], sub_el=[]}.

nack_op(Token) ->
  #xmlelement{name="nack", attrs=[{"token",Token},
                                  {"xmlns", ?AGENT_NS}], sub_el=[]}.

final_op(Token) ->
  #xmlelement{name="final", attrs=[{"token",Token},
                                   {"xmlns", ?AGENT_NS}], sub_el=[]}.

result_op({xmlelement, "list", _, _} = Results, Token) ->
  Attrs = [{"token", Token}, {"xmlns", ?AGENT_NS}],
  {xmlelement, "data", Attrs, [Results]};
result_op(Results, Token) when is_list(Results) ->
  Attrs = [{"token", Token}, {"xmlns", ?AGENT_NS}],
  {xmlelement, "data", Attrs, build_subels(Results, [])};
result_op(Results, Token) ->
  result_op([Results], Token).

%%
%% generic_op({OpName, SubEls}) -> #xmlelement
%% OpName -> string
%% SubEls -> [SubEl]
%% SubEl -> {Name, Attrs, [SubEl] | []} | {text, Text}
%%
generic_op({OpName, Token, SubEls}) ->
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
