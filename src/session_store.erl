-module(session_store).

-export([
         init/0,
         insert/1,
         delete_pid/1,
         delete_key/1,
         lookup/1
        ]).

-define(TABLE_ID, ?MODULE).

init() ->
  ets:new(?TABLE_ID, [public, named_table]),
  ok.

insert(Pid) ->
  Key = generate_key(),
  ets:insert(?TABLE_ID, {Key, Pid}).

lookup(Key) ->
  case ets:lookup(?TABLE_ID, Key) of
    [{Key, Pid}] -> {ok, Pid};
    []           -> {error, not_found}
  end.

delete_pid(Pid) ->
  ets:match_delete(?TABLE_ID, {'_', Pid}).

delete_key(Key) ->
  ets:match_delete(?TABLE_ID, {Key, '_'}).

generate_key() ->
  { MegaSeconds, Seconds, Microseconds } = now(),
  base64:encode(<<MegaSeconds:32,Seconds:32, Microseconds:32>>).
