-module(locker_sup).

-behaviour(supervisor).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link(W, LeaseExpireInterval, LockExpireInterval, PushTransInterval) ->
    supervisor:start_link(
      {local, ?SERVER}, ?MODULE,
      [W, LeaseExpireInterval, LockExpireInterval, PushTransInterval]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([W, LeaseExpireInterval, LockExpireInterval, PushTransInterval]) ->

    LockerStats =
        {locker_stats,
         {locker_stats, start_link, []},
         permanent, 2000, worker, [locker_stats]},

    Locker =
        {locker,
         {locker, start_link,
          [W, LeaseExpireInterval, LockExpireInterval, PushTransInterval]},
         permanent, 2000, worker, [locker]},

    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [LockerStats, Locker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
