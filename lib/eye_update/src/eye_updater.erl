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

-module(eye_updater).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-define(DEFAULT_BOUNCE_INTERVAL, 500).

%% API
-export([start_link/1, bounce_node/0, bounce_node/1, bounce_node/2]).
-export([update_applications/0, update_applications/1, update_processes/0, update_processes/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(ConfigFilePath) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [ConfigFilePath], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([ConfigFilePath]) ->
  case find_master_node(ConfigFilePath) of
    ok ->
      eye_update_server:register_node(node()),
      {ok, #state{}};
    {error, Reason} ->
      {stop, Reason}
  end.

update_applications(Node) when is_atom(Node) ->
  gen_server:call({?SERVER, Node}, update_applications).

update_applications() ->
  gen_server:call(?SERVER, update_applications).

update_processes(Node) when is_atom(Node) ->
  gen_server:call({?SERVER, Node}, update_processes).

update_processes() ->
  gen_server:call(?SERVER, update_processes).

bounce_node(Interval) when is_number(Interval), interval > 100 ->
  gen_server:call(?SERVER, {restart_node, Interval});
bounce_node(Node) when is_atom(Node) ->
  bounce_node(Node, ?DEFAULT_BOUNCE_INTERVAL).

bounce_node() ->
  bounce_node(?DEFAULT_BOUNCE_INTERVAL).

bounce_node(Node, Interval) ->
  gen_server:call({?SERVER, Node}, {restart_node, Interval}).

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(update_processes, _From, State) ->
  Modules = changed_modules(),
  Pids = find_target_processes(Modules),
  load_updates(Pids, Modules),
  {reply, ok, State};

handle_call(update_applications, _From, State) ->
  Apps = detect_changed_apps(),
  restart_apps(Apps),
  {reply, ok, State};

handle_call({restart_node, Interval}, _From, State) ->
  timer:apply_after(Interval, init, restart, []),
  {reply, {ok, Interval}, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
load_updates(Pids, Modules) ->
  lists:foreach(fun(Pid) -> erlang:suspend_process(Pid) end, Pids),
  reload_modules(Modules),
  lists:foreach(fun(Pid) -> erlang:resume_process(Pid) end, Pids).

reload_modules([{Mod, _Loaded}|T]) ->
  code:purge(Mod),
  code:soft_purge(Mod),
  {module, Mod} = code:load_file(Mod),
  reload_modules(T);
reload_modules([]) ->
  ok.

find_target_processes(Modules) ->
  Pids = processes(),
  lists:filter(fun(Pid) -> does_process_use_module(Pid, Modules) end, Pids).

does_process_use_module(Pid, [{Module, _Loaded}|T]) ->
  case check_process_code(Pid, Module) of
    true ->
      true;
    false ->
      does_process_use_module(Pid, T)
  end;
does_process_use_module(_Pid, []) ->
  false.

restart_apps([]) ->
  ok;
restart_apps([H|T]) ->
  error_logger:info_report({stopping, H}),
  application:stop(H),
  error_logger:info_report({starting, H}),
  application:start(H),
  restart_apps(T).

detect_changed_apps() ->
  Modules = changed_modules(),
  lists:foldl(fun({Module, _Loaded}, Acc) ->
                  App = application:get_application(Module),
                  case lists:member(App, Acc) of
                    true ->
                      Acc;
                    false ->
                      [App|Acc]
                  end end, [], Modules).

changed_modules() ->
  lists:filter(fun({Module, Loaded}) -> Loaded /= preloaded andalso has_module_changed(Module) end, code:all_loaded()).

has_module_changed(Module) ->
  OrigInfo = Module:module_info(),
  OrigChecksum = proplists:get_value(vsn, proplists:get_value(attributes, OrigInfo)),
  OrigChecksum /= read_module_checksum_from_disk(Module).

read_module_checksum_from_disk(Module) ->
  {_Type, Locn} = code:is_loaded(Module),
  {ok, Contents, _Path} = erl_prim_loader:get_file(Locn),
  {ok, {_ModName, Checksum}} = beam_lib:version(Contents),
  Checksum.

find_master_node(ConfigFilePath) ->
  case file:consult(ConfigFilePath) of
    {ok, Config} ->
      MasterNode = proplists:get_value(master_node, Config),
      case ping(MasterNode, 5) of
        ok ->
          case find_servers(5) of
            ok ->
              ok;
            not_found ->
              {error, servers_not_found}
          end;
        not_found ->
          {error, master_node_not_found}
      end;
    Error ->
      Error
  end.

ping(MasterNode, Tries) when Tries > 0 ->
  case net_adm:ping(MasterNode) of
    pong ->
      ok;
    pang ->
      timer:sleep(1000),
      ping(MasterNode, Tries - 1)
  end;
ping(_MasterNode, Tries) when Tries == 0->
  not_found.

find_servers(Tries) when Tries > 0 ->
  Servers = global:registered_names(),
  case lists:member(eye_release_server, Servers) andalso
    lists:member(eye_update_server, Servers) of
    true ->
      ok;
    false ->
      timer:sleep(1000),
      find_servers(Tries - 1)
  end;
find_servers(Tries) when Tries == 0 ->
  not_found.
