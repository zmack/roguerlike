-module(roguerlike_resource).
-export([
  init/1,
  resource_exists/2,
  to_html/2,
  to_json/2,
  post_is_create/2,
  create_path/2,
  allowed_methods/2,
  content_types_accepted/2,
  content_types_provided/2,
  allow_missing_post/2,
  from_json/2
  ]).

-include("creatures.hrl").
-include_lib("webmachine/include/webmachine.hrl").
-record(context, { pid }).

init([]) -> {ok, undefined}.

allowed_methods(ReqData, Context) ->
  {['HEAD', 'GET', 'PUT', 'DELETE', 'POST'], ReqData, Context}.

allow_missing_post(ReqData, Context) ->
  case wrq:disp_path(ReqData) of
    [] -> { true, ReqData, Context };
    _ -> { false, ReqData, Context }
  end.

resource_exists(ReqData, Context) ->
  Key = list_to_binary(wrq:disp_path(ReqData)),
  case instance_store:lookup(Key) of
    { ok, Pid } -> { true, ReqData, #context{ pid = Pid } };
    _ -> { false, ReqData, Context }
  end.

content_types_accepted(ReqData, Context) ->
  Types = [{"application/json", from_json}],
  {Types, ReqData, Context}.

content_types_provided(ReqData, Context) ->
  Types = [{"application/json", to_json}],
  {Types, ReqData, Context}.

from_json(ReqData, Context) ->
  { ok, ReqData, Context }.

post_is_create(ReqData, Context) ->
  case wrq:disp_path(ReqData) of
    [] -> { true, ReqData, Context };
    _ -> { false, ReqData, Context }
  end.

create_path(ReqData, Context) ->
  Player = #player{ health = 10, damage = 10 },
  Dungeon = [],
  case instance:create(Player, Dungeon) of
    { ok, Key } -> { binary_to_list(Key), ReqData, Context};
    _ -> {undefined, ReqData, Context}
  end.

to_html(ReqData, Context) ->
  {"<html><body>Hello, new world</body></html>", ReqData, Context}.

to_json(ReqData, #context{ pid = Pid } = Context) ->
  InstanceInfo = instance:get_state(Pid),
  {instance_info_to_json(InstanceInfo), ReqData, Context}.

instance_info_to_json(InstanceInfo) ->
  {Player, _StartTime} = InstanceInfo,
  Struct = {struct,
    [{ player,
        {struct, [
            {health, Player#player.health},
            {damage, Player#player.damage}
            ]}
        }]
  },
  mochijson2:encode(Struct).
