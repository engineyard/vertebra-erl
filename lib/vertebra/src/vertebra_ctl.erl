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

-module(vertebra_ctl).

-export([start/0, process/1]).

%% Private API
-export([waitfor/1]).

-define(PRINT(Format, Args), io:format(Format, Args)).

-define(STATUS_OK, 0).
-define(STATUS_NOT_OK, 1).
-define(STATUS_USAGE, 2).
-define(STATUS_BADRPC, 3).

%% I doubt it will ever take any of our agents 5 minutes to come up, so it seems
%% like a reasonably large, but bearably small timeout.
-define(WAITFOR_TIMEOUT, 300000).
-define(WAITFOR_SLEEP, 100).

start() ->
  case init:get_plain_arguments() of
    [NodeStr | Args] ->
      Node = to_node(NodeStr),
      Status = case remote_add_path(Node) of
                 {badrpc, _} = BadRpc -> BadRpc;
                 _ -> remote_process(Node, Args)
               end,
      case Status of
        {badrpc, Reason} ->
          ?PRINT("RPC failed on the node ~p: ~p~n", [Node, Reason]),
          halt(?STATUS_BADRPC);
        _ ->
          halt(Status)
      end;
    _ ->
      halt(?STATUS_USAGE)
  end.

%% TODO: Consider making sure that the path is /exactly/ the same as the path
%% for the calling module. It is possible that the remote node could be loading
%% an old version of this module. But, for now, this is good.
remote_add_path(Node) ->
  File = code:which(?MODULE),
  Dir = filename:dirname(File),
  case rpc:call(Node, code, which, [?MODULE]) of
    {badrpc, _} = BadRpc ->
      BadRpc;
    non_existing ->
      case rpc:call(Node, code, add_patha, [Dir]) of
        {badrpc, _} = BadRpc -> BadRpc;
        _ -> ok
      end;
    _ ->
      ok
  end.

remote_process(Node, Args) ->
  case rpc:call(Node, ?MODULE, process, [Args]) of
    {badrpc, _} = BadRpc -> BadRpc;
    S -> S
  end.


to_node(Str) ->
  Node = case string:tokens(Str, "@") of
           [_Node, _Server] ->
             Str;
           _ ->
             case net_kernel:longnames() of
               true ->
                 string:join([Str, "@",
                              inet_db:gethostname(),
                              inet_db:res_option(domain)], "");
               false ->
                 string:join([Str, "@",
                              inet_db:gethostname()], "");
               _ ->
			     Str
             end
         end,
  list_to_atom(Node).

process(["ping"]) ->
  io:format("pong~n"),
  ?STATUS_OK;

process(["started", AppName]) ->
  Result = application_running(AppName),
  io:format("~p~n", [Result]),
  case Result of
    true -> ?STATUS_OK;
    false -> ?STATUS_NOT_OK
  end;

process(["stop"]) ->
  init:stop(),
  ?STATUS_OK;

process(["waitfor", AppName]) ->
  Pid = proc_lib:spawn_link(?MODULE, waitfor, [AppName]),
  Ref = erlang:monitor(process, Pid),
  Result = receive
             %% We don't care about reason because we will get killed on non-normal exit,
             %% so the only 'DOWN' message we'll ever get will be normal.
             {'DOWN', Ref, process, Pid, _Reason} ->
               io:format("ok~n"),
               ?STATUS_OK
           after ?WAITFOR_TIMEOUT ->
               io:format("Timed out waiting for ~p to start.~n", [AppName]),
               ?STATUS_NOT_OK
           end,
  exit(Pid, shutdown),
  erlang:demonitor(Ref),
  Result;

process(Args) ->
  io:format("Node: ~p~nArgs: ~p~n", [node(), Args]),
  ?STATUS_OK.

application_running(AppName) when is_list(AppName) ->
  application_running(list_to_atom(AppName));
application_running(AppName) when is_atom(AppName) ->
  lists:keymember(AppName, 1, application:which_applications()).

waitfor(AppName) ->
  case application_running(AppName) of
    true -> ok;
    false ->
      receive after ?WAITFOR_SLEEP -> ok end,
      waitfor(AppName)
  end.
