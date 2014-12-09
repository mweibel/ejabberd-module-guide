%%%'   HEADER
%% @author
%% @copyright
%% @doc example ejabberd module
%% @end

-module(ejabberd_auth_example).
-author('Michael Weibel').

%% Exports
%%
%% These exports
-export([start/1,
  plain_password_required/0,
  store_type/0,
  set_password/3,
  check_password/3,
  check_password/5,
  try_register/3,
  dirty_get_registered_users/0,
  get_vh_registered_users/1,
  get_vh_registered_users/2,
  get_vh_registered_users_number/1,
  get_vh_registered_users_number/2,
  get_password/2,
  get_password_s/2,
  is_user_exists/2,
  remove_user/2,
  remove_user/3
]).

start(_Server) ->
  ok.

plain_password_required() ->
  true.

store_type() ->
  external.

check_password(_User, _Server, _Password) ->
  true.

check_password(User, Server, Password, _StreamID, _Digest) ->
  check_password(User, Server, Password).

is_user_exists(User, Server) ->
  true.

set_password(_User, _Server, _Password) ->
  {error, not_implemented}.

try_register(_User, _Server, _Password) ->
  {error, not_implemented}.

dirty_get_registered_users() ->
  {error, not_implemented}.

get_vh_registered_users(Server) ->
  get_vh_registered_users(Server, none).

get_vh_registered_users(_Server, _Opts) ->
  [].

get_vh_registered_users_number(Server) ->
  get_vh_registered_users_number(Server, none).

get_vh_registered_users_number(_Server, _Opts) ->
  0.

get_password(_User, _Server) ->
  false.

get_password_s(_User, _Server) ->
  "".

%% @spec (User, Server) -> ok | error
%% @doc Remove user.
remove_user(_User, _Server) ->
  error.

%% @spec (User, Server, Password) -> ok | error | not_exists | not_allowed
%% @doc Remove user if the provided password is correct.
remove_user(User, Server, _Password) ->
  remove_user(User, Server).
