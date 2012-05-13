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

init([]) -> {ok, undefined}.

allowed_methods(ReqData, State) ->
  {['HEAD', 'GET', 'PUT', 'DELETE', 'POST'], ReqData, State}.

allow_missing_post(ReqData, State) ->
  case wrq:disp_path(ReqData) of
    "" -> { true, ReqData, State };
    _ -> { false, ReqData, State }
  end.

resource_exists(ReqData, State) ->
  Key = list_to_binary(wrq:disp_path(ReqData)),
  case instance_store:lookup(Key) of
    { ok, _Pid } -> { true, ReqData, State };
    _ -> { false, ReqData, State }
  end.

content_types_accepted(ReqData, State) ->
  Types = [{"application/json", from_json}],
  {Types, ReqData, State}.

content_types_provided(ReqData, State) ->
  Types = [{"application/json", to_json}],
  {Types, ReqData, State}.

from_json(ReqData, State) ->
  { ok, ReqData, State }.

post_is_create(ReqData, State) ->
  case wrq:disp_path(ReqData) of
    "" -> { true, ReqData, State };
    _ -> { false, ReqData, State }
  end.

create_path(ReqData, Context) ->
  Player = #player{ health = 10, damage = 10 },
  Dungeon = [],
  case instance:create(Player, Dungeon) of
    { ok, Key } -> { binary_to_list(Key), ReqData, Context};
    _ -> {undefined, ReqData, Context}
  end.

to_html(ReqData, State) ->
  {"<html><body>Hello, new world</body></html>", ReqData, State}.

to_json(ReqData, State) ->
  {"{foo: bar }", ReqData, State}.
