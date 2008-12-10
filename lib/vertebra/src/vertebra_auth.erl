-module(vertebra_auth).

-author("kevin@hypotheticalabs.com").

%% Public API
-export([verify/3, verify/4]).


%% Public API

%% Same as verify/4 except the requestor jid is assumed to be
%% the one used to log into the Jabber server
verify(Config, HeraultJid, Resources) ->
  RequestorJid = lists:flatten([proplists:get_value(username, Config), "@", proplists:get_value(host, Config)]),
  verify(Config, HeraultJid, RequestorJid, Resources).


%% Queries Herault to determine if RequestorJid has authorization
%% to access Resources
%%
%% Config: [{host, <jabber_host},
%%          {username, <jabber_username>},
%%          {authentication, {password, <jabber_password>}}]
%%
%% HeraultJid: JID of the Herault instance to query
%%
%% RequestorJid: JID to query
%%
%% Resources: List of strings representing Vertebra resources
verify(Config, HeraultJid, RequestorJid, Resources) ->
  Login = convert_login(Config),
  Token = proplists:get_value(resource, Login),
  Inputs = [build_requestor(RequestorJid) | vertebra_xmpp:build_resources(Resources)],
  case vertebra_cmd:run(Login, "/security/authorize", Inputs, Token, HeraultJid) of
    {ok, _From, Data} ->
      case Data of
	[{string, [{"name", "response"}], <<"notauthorized">>}] ->
	  {ok, false};
	[{string, [{"name", "response"}], <<"authorized">>}] ->
	  {ok, true};
	Error ->
	  Error
      end;
    Error ->
      Error
  end.

build_requestor(RequestorJid) when is_binary(RequestorJid) ->
  {string, [{"name", "from"}], RequestorJid};
build_requestor(RequestorJid) when is_list(RequestorJid)->
  {string, [{"name", "from"}], list_to_binary(RequestorJid)}.

convert_login(Config) ->
  Token = uuid_server:generate_uuid(),
  [{resource, Token} | lists:filter(fun({Name, _Value}) ->
					if
					  Name =:= resource ->
					    false;
					  true ->
					    true
					end end, Config)].
