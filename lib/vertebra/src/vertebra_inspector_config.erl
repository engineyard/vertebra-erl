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

-module(vertebra_inspector_config).

-author("ksmith@engineyard.com").

-include("vertebra_inspector.hrl").

-export([read/1, find_rule/2]).

read(ConfigFileName) ->
  {ok, FileContents} = file:read_file(ConfigFileName),
  {ok, Config, _} = erlsom:parse_sax(FileContents, [{rules, []}, {inspections, []}], fun(Event, Acc) ->
                                                                                         handle_event(Event, Acc) end),
  {ok, Config}.

find_rule(Jid, [{rules, Rules}, {inspections, Inspections}]=Config) ->
  case find_applicable_rule(Jid, Inspections, Rules) of
    not_found ->
      if
        Jid =:= "all" ->
          not_found;
        true ->
          find_rule("all", Config)
      end;
    Rule ->
      Rule
  end.

%% Internal functions
string_to_integer(undefined) ->
  undefined;
string_to_integer(Value) when is_list(Value) ->
  list_to_integer(Value).

string_to_float(undefined) ->
  undefined;
string_to_float(Value) when is_list(Value) ->
  list_to_float(Value).

handle_event({startElement, _, "rule", _, Attrs}, [{rules, Rules}, Inspections]) ->
  [{rules, [new_rule(Attrs)|Rules]}, Inspections];

handle_event({startElement, _, "inspect", _, Attrs}, [Rules, {inspections, Inspections}]) ->
  [Rules, {inspections, [new_inspection(Attrs)|Inspections]}];

handle_event(_, Acc) ->
  Acc.

find_attr(Name, Attrs) ->
  find_attr(Name, Attrs, not_found).

find_attr(Name, [{attribute, Name, _, _, Value}|_], _) ->
  Value;
find_attr(Name, [_|T], Default) ->
  find_attr(Name, T, Default);
find_attr(_, [], Default) ->
  Default.

new_rule(Attrs) ->
  Id = find_attr("id", Attrs),
  Min = string_to_integer(find_attr("min", Attrs, "0")),
  Max = string_to_integer(find_attr("max", Attrs, "0")),
  Percent = string_to_float(find_attr("percent", Attrs, "0.0")),
  Behavior = find_attr("behavior", Attrs),
  Type = find_attr("type", Attrs, ""),
  new_rule(Id, Min, Max, Percent, Type, Behavior).

new_rule(Id, Min, Max, _Percent, _Type, "delay") when length(Id) > 0,
                                                      Min > 0,
                                                      Max > 0 ->
  {Id, #rule{id=Id,
             min=Min,
             max=Max,
             behavior=delay}};

new_rule(Id, _Min, _Max, Percent, _Type, "drop") when length(Id) > 0,
                                                      Percent > 0.0 ->
  {Id, #rule{id=Id,
             behavior=drop,
             percent=Percent}};

new_rule(Id, _Min, _Max, Percent, Type, "iq_error") when length(Id) > 0,
                                                         Percent > 0.0,
                                                         length(Type) > 0 ->
  {Id, #rule{id=Id,
             behavior=iq_error,
             type=string_to_integer(Type),
             percent=Percent}};

new_rule(Id, _Min, _Max, Percent, Type, "vert_error") when length(Id) > 0,
                                                           Percent > 0.0,
                                                           length(Type) > 0 ->
  {Id, #rule{id=Id,
             behavior=vert_error,
             type=Type,
             percent=Percent}};

new_rule(Id, _, _, _, _, _) ->
  throw({error, bad_rule_def, Id}).

new_inspection(Attrs) ->
  Id = find_attr("id", Attrs),
  To = find_attr("to", Attrs, undefined),
  Min = string_to_integer(find_attr("min", Attrs, "0")),
  Max = string_to_integer(find_attr("max", Attrs, "0")),
  Percent = string_to_float(find_attr("percent", Attrs, "0.0")),
  Behavior = find_attr("behavior", Attrs),
  Type = find_attr("type", Attrs, ""),
  Rule = find_attr("rule", Attrs),
  new_inspection(Id, To, Min, Max, Percent, Type, Behavior, Rule).

new_inspection(Id, To, _, _, _, _, _, Rule) when length(Id) > 0,
                                                 length(Rule) > 0 ->
  {Id, #inspection{id=Id, to=To, rule=Rule}};

new_inspection(Id, To, Min, Max, _Percent, _Type, "delay", _Rule) when length(Id) > 0,
                                                                       length(To) > 0,
                                                                       Min > 0,
                                                                       Max > 0 ->
  {Id, #inspection{id=Id,
                   to=To,
                   behavior=delay,
                   min=Min,
                   max=Max}};

new_inspection(Id, To, _Min, _Max, Percent, _Type, "drop", _Rule) when length(Id) > 0,
                                                                       length(To) > 0,
                                                                       Percent > 0.0 ->
  {Id, #inspection{id=Id,
                   to=To,
                   behavior=drop,
                   percent=Percent}};

new_inspection(Id, To, _Min, _Max, Percent, _Type, "repeat", _Rule) when length(Id) > 0,
                                                                       length(To) > 0,
                                                                       Percent > 0.0 ->
  {Id, #inspection{id=Id,
                   to=To,
                   behavior=repeat,
                   percent=Percent}};


new_inspection(Id, To, _Min, _Max, Percent, Type, "iq_error", _Rule) when length(Id) > 0,
                                                                          length(To) > 0,
                                                                          length(Type) > 0,
                                                                          Percent > 0.0 ->
  {Id, #inspection{id=Id,
                   to=To,
                   behavior=iq_error,
                   type=string_to_integer(Type),
                   percent=Percent}};

new_inspection(Id, To, _Min, _Max, Percent, Type, "vert_error", _Rule) when length(Id) > 0,
                                                                            length(To) > 0,
                                                                            length(Type) > 0,
                                                                            Percent > 0.0 ->
  {Id, #inspection{id=Id,
                   to=To,
                   behavior=vert_error,
                   type=Type,
                   percent=Percent}};

new_inspection(Id, _, _, _, _, _, _, _) ->
  throw({error, bad_inspection_def, Id}).

find_applicable_rule(Jid, [{_, Inspection}|T], Rules) ->
  case Inspection#inspection.to =:= Jid of
    true ->
      case Inspection#inspection.rule =:= undefined of
        true ->
          build_behavior_decl(Inspection);
        false ->
          find_named_rule(Inspection#inspection.rule, Rules)
      end;
    false ->
      find_applicable_rule(Jid, T, Rules)
  end;
find_applicable_rule(_Jid, [], _Rules) ->
  not_found.

build_behavior_decl(Inspection) ->
  case Inspection#inspection.min == 0 of
    true ->
      {Inspection#inspection.behavior, Inspection#inspection.percent};
    false ->
      {Inspection#inspection.behavior, Inspection#inspection.min, Inspection#inspection.max}
  end.

find_named_rule(RuleName, [{RuleName, Rule}|_]) ->
  build_rule_decl(Rule);
find_named_rule(RuleName, [_|T]) ->
  find_named_rule(RuleName, T);
find_named_rule(_RuleName, []) ->
  not_found.

build_rule_decl(Rule) ->
  case Rule#rule.min == 0 of
    true ->
      {Rule#rule.behavior, Rule#rule.percent};
    false ->
      {Rule#rule.behavior, Rule#rule.min, Rule#rule.max}
  end.
