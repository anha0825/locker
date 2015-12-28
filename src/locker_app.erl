-module(locker_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  locker_sup:start_link(config(w), config(lease_expire_interval), config(lock_expire_interval), config(push_trans_interval)).

stop(_State) ->
    ok.

config(Key) ->
  case application:get_env(locker, Key) of
    undefined ->
      throw({error, {not_in_config, key}});
    {ok, Value} ->
      Value
  end.
