-module(roguerlike_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  instance_store:init(),
  init_web_sup(),
  init_sup().

init_web_sup() ->
  {ok, _} = roguerlike_web_sup:start_link().

init_sup() ->
  {ok, _} = roguerlike_sup:start_link().

stop(_State) ->
    ok.
