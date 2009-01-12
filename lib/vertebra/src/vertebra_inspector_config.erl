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
  Min = find_attr("min", Attrs, 0),
  Max = find_attr("max", Attrs, 0),
  Percent = find_attr("percent", Attrs, 0.0),
  Behavior = find_attr("behavior", Attrs),
  new_rule(Id, Min, Max, Percent, Behavior).

new_rule(Id, Min, Max, Percent, Behavior) when length(Id) > 0,
                                               length(Behavior) > 0 ->
  F = fun() -> {Id, #rule{id=Id,
                          min=string_to_integer(Min),
                          max=string_to_integer(Max),
                          percent=Percent,
                          behavior=list_to_atom(Behavior)}} end,
  EF = fun() -> throw({error, bad_rule_def, Id}) end,
  validate_num_attrs({{Min, Max, F}, {Percent, F}, EF});

new_rule(Id, _, _, _, _) ->
  throw({error, bad_rule_def, Id}).

new_inspection(Attrs) ->
  Id = find_attr("id", Attrs),
  To = find_attr("to", Attrs, undefined),
  Min = find_attr("min", Attrs, 0),
  Max = find_attr("max", Attrs, 0),
  Percent = find_attr("percent", Attrs, 0.0),
  Behavior = find_attr("behavior", Attrs),
  Rule = find_attr("rule", Attrs),
  new_inspection(Id, To, Min, Max, Percent, Behavior, Rule).

new_inspection(Id, To, _, _, _, _, Rule) when length(Id) > 0,
                                              length(Rule) > 0 ->
  {Id, #inspection{id=Id, to=To, rule=Rule}};

new_inspection(Id, To, Min, Max, Percent, Behavior, _) when length(Behavior) > 0,
                                                            length(Id) > 0 ->
  F = fun() -> {Id, #inspection{id=Id,
                                to=To,
                                behavior=list_to_atom(Behavior),
                                min=string_to_integer(Min),
                                max=string_to_integer(Max),
                                percent=Percent}} end,
  EF = fun() -> throw({error, bad_inspection_def, Id}) end,
  validate_num_attrs({{Min, Max, F}, {Percent, F}, EF});

new_inspection(Id, _, _, _, _, _, _) ->
  throw({error, bad_inspection_def, Id}).

validate_num_attrs({{Min, Max, MinMaxFun}, {Percent, PercentFun}, ErrorFun}) ->
  if
    Min == 0 andalso Max == 0 ->
      if
        Percent == 0.0 ->
          ErrorFun();
        true ->
          PercentFun()
      end;
    true ->
      MinMaxFun()
  end.

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
