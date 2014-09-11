-module(locker_stats).

%% API
-export([start_link/0, add_handler/1, false_expire/3]).

%% Stat reporting API
-export([write/7,
         delete_handler/1,
         failed_write_lock/5]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler(Module) ->
    gen_event:add_handler(?SERVER, Module, []).

delete_handler(Module) ->
    gen_event:delete_handler(?SERVER, Module, []).

%% Report partial writes.
write(_Key, _Type, Nodes, _Quorum, OkWrites, [], [])
  when length(Nodes) =:= length(OkWrites) ->
    ok;
write(Key, Type, Nodes, Quorum, OkWrites, NotOkWrites, BadWriteNodes) ->
    notify({partial_write, Key, Type, Nodes, Quorum, OkWrites,
            NotOkWrites, BadWriteNodes}).

%% Report failed write locks. This is the first part of a cmd to locker
%% Read more in the readme.
%% NotAbortReplies are the nodes that didn't reply to the abort call.
%%   They might still have a write lock in locker for this key.
%%   You should be extra suspicius if they show up in OkNodes.
failed_write_lock(Key, Type, OkNodes, NotOkNodes, NotAbortReplies) ->
    notify({failed_write_lock, Key, Type, OkNodes, NotOkNodes,
            NotAbortReplies}).

false_expire(Key, PossibleValue, PossibleOtherExpire) ->
    notify({false_expire, Key, PossibleValue, PossibleOtherExpire}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

notify(Stat) ->
    gen_event:notify(?SERVER, Stat).
