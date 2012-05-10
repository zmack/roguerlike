-module(roguerlike).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
start() ->
    roguerlike_deps:ensure(),
    ensure_started(crypto),
    application:start(roguerlike).


%% @spec stop() -> ok
stop() ->
    application:stop(roguerlike).
