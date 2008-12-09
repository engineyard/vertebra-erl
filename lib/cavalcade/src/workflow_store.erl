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

-module(workflow_store).

-author("ksmith@engineyard.com").

-export([init/0, table_info/0, table_name/0]).
-export([store_workflow/1, find_workflow/1, delete_workflow/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("models.hrl").
-include("cavalcade.hrl").

init() ->
  gen_store:init(?MODULE).

table_info() ->
  [{table_attrs, record_info(fields, workflow_listing)}].

table_name() ->
  workflow_listing.

store_workflow({workflow, _, _, _} = Workflow) ->
  L = new_listing(Workflow),
  case gen_store:w(L) of
    {atomic, ok} ->
      ok;
    Error ->
      Error
  end.

find_workflow(Name) ->
  case gen_store:sq(?MODULE,
                    fun(WL) -> WL#workflow_listing.workflow end,
                    [fun(WL) -> WL#workflow_listing.name =:= Name end]) of
    {atomic, Results} ->
      case length(Results) > 0 of
        true ->
          [WF|_T] = Results,
          {ok, WF};
        false ->
          {ok, Results}
      end;
    Error ->
      Error
  end.

delete_workflow(Name) ->
  gen_store:dk(?MODULE, Name).

%% Internal functions
new_listing({workflow, Name, _, _} = Workflow) ->
  #workflow_listing{name=Name, workflow=Workflow}.
