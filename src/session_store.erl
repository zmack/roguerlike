-module(session_store).

-export([
         init/0,
         insert/1,
         delete/1,
         lookup/1
        ]).

-define(TABLE_ID, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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

delete({pid, Pid}) ->
  ets:match_delete(?TABLE_ID, {'_', Pid});
delete({key, Key}) ->
  ets:match_delete(?TABLE_ID, {Key, '_'});
delete(_Any) -> true.

generate_key() ->
  { MegaSeconds, Seconds, Microseconds } = now(),
  base64:encode(<<MegaSeconds:32,Seconds:32, Microseconds:32>>).

%% =========================================================
%% EUnit tests
%% =========================================================
-ifdef(TEST).

init_should_not_fail_test() ->
  InitResponse = init(),

  ?assertEqual(ok, InitResponse).

insert_should_be_successfully_performed_test() ->
  InsertResponse = insert(test_pid),

  ?assertEqual(true, InsertResponse).

loopup_for_nonexisting_key_should_return_not_found_test() ->
  Value = lookup(test_key),

  ?assertEqual({error, not_found}, Value).

lookup_for_valid_key_should_return_valid_value_test() ->
  ets:insert(?TABLE_ID, {test_key, some_value_here}),

  Value = lookup(test_key),

  ?assertEqual({ok, some_value_here}, Value).

delete_wrong_expression_should_return_true_test() ->
  DeleteResponse = delete("Some other expression"),

  ?assertEqual(true, DeleteResponse).

delete_nonexisting_key_should_return_true_test() ->
  DeleteResponse = delete({key, some_key}),

  ?assertEqual(true, DeleteResponse).

delete_nonexisting_pid_should_return_true_test() ->
  DeleteResponse = delete({pid, some_pid}),

  ?assertEqual(true, DeleteResponse).

delete_existing_key_should_return_true_test() ->
  ets:insert(?TABLE_ID, {some_key, some_pid}),

  DeleteResponse = delete({key, some_key}),

  ?assertEqual(true, DeleteResponse).

delete_existing_pid_should_return_true_test() ->
  ets:insert(?TABLE_ID, {some_key, some_pid}),

  DeleteResponse = delete({pid, some_pid}),

  ?assertEqual(true, DeleteResponse).

delete_nonexisting_key_should_not_affect_existing_entries_test() ->
  ets:insert(?TABLE_ID, {some_key, some_pid}),

  delete({key, some_wrong_key}),

  ?assertEqual([{some_key, some_pid}], ets:lookup(?TABLE_ID, some_key)).

-endif.
