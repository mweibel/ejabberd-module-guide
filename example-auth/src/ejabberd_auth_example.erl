%%%'   HEADER
%% @author
%% @copyright
%% @doc example ejabberd module
%% @end

-module(ejabberd_auth_example).
-author('Michael Weibel').

%% Exports
%%
%% Marks all function within the list as accessible from outside the module.
%% All ejabberd auth modules need the listed functions to be exported in order to
%% work properly.
%%
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

%%%------------------------------------------------
%%% API functions we're going to implement
%%%------------------------------------------------

%%
%% When ejabberd is started, this method is called to start
%% the auth module.
%% You can do initialization code here if you have to,
%% just return with ok in the end.
%%
start(_Server) ->
  ok.

%%
%% If this module only accepts SASL PLAIN, specify
%% true here.
%%
plain_password_required() ->
  true.

%%
%% one of plain | scram | external
%% Usually you'll want to return `external` or `plain`
%%
store_type() ->
  external.

%%
%% This is the main function which checks
%% whether the supplied password matches for the supplied user.
%%
%% @spec (User, Server, Password) -> true | false | {error, Error}
check_password(_User, _Server, _Password) ->
  true.

%%
%% Checks password when a digest was supplied. Usually for external
%% authentication methods over TLS you don't really have to care about this.. ;-)
%%
%% @spec (User::string(), Server::string(), Password::string(),
%%        Digest::string(), DigestGen::function()) ->
check_password(User, Server, Password, _StreamID, _Digest) ->
  check_password(User, Server, Password).

%%
%% Whenever a message is being sent, this method is called
%% in order to make sure the recipient is existing.
%% Usually it's best to have such a method on e.g. your API
%% and check this.
%% It might be also not too bad if the check is expensive, that
%% you cache it for a certain time.
%%
is_user_exists(User, Server) ->
  true.

%%%------------------------------------------------
%%% Not implemented API functions
%%% You can but don't necessarily have to implement
%%% them.
%%%------------------------------------------------

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
