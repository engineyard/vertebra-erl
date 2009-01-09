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

-export([read/1]).

read(ConfigFileName) ->
  {ok, FileContents} = file:read_file(ConfigFileName),
  {ok, Config, _} = erlsom:parse_sax(FileContents, [{rules, []}, {inspections, []}], fun(Event, Acc) ->
                                                                                         handle_event(Event, Acc) end),
  {ok, Config}.

%% Internal functions
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
  io:format("Min: ~p, Max: ~p, Percent: ~p~n", [Min, Max, Percent]),
  if
    Min == 0 andalso Max == 0 ->
      if
        Percent == 0.0 ->
          throw({error, bad_rule_def, Id});
        true ->
          {Id, #rule{id=Id, min=Min, max=Max, percent=Percent, behavior=Behavior}}
      end;
    true ->
      {Id, #rule{id=Id, min=Min, max=Max, percent=Percent, behavior=Behavior}}
  end;

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
  if
    Min == 0 andalso Max == 0 ->
      if
        Percent == 0.0 ->
          throw({error, bad_inspection_def, Id});
        true ->
          {Id, #inspection{id=Id, to=To, behavior=Behavior, min=Min, max=Max, percent=Percent}}
      end;
    true ->
      {Id, #inspection{id=Id, to=To, behavior=Behavior, min=Min, max=Max, percent=Percent}}
  end;

new_inspection(Id, _, _, _, _, _, _) ->
  throw({error, bad_inspection_def, Id}).
