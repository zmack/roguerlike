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
  case roguerlike_web_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Other ->
      {error, Other}
  end.

init_sup() ->
  case roguerlike_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Other ->
      {error, Other}
  end.

stop(_State) ->
    ok.
