-module(roguerlike_web).

-export([start/1, stop/0, loop/2]).
-include("creatures.hrl").

%% External API

start(Options) ->
  {DocRoot, Options1} = get_option(docroot, Options),
  Loop = fun (Req) ->
      ?MODULE:loop(Req, DocRoot)
  end,
  mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
  mochiweb_http:stop(?MODULE).

loop(Req, _DocRoot) ->
  "/" ++ Path = Req:get(path),
  try
    case Req:get(method) of
      Method when Method =:= 'GET'; Method =:= 'HEAD' ->
        case Path of
          "instance" ->
            Player = #player{ health = 10, damage = 1 },
            Instance = instance:create(Player, []),
            serve_text(Req, io_lib:format("~p", [Instance]));
          _ ->
            serve_text(Req, Path)
        end;
      'POST' ->
        case Path of
          _ ->
            Req:not_found()
        end;
      _ ->
        Req:respond({501, [], []})
    end
  catch
    Type:What ->
      Report = ["web request failed",
                {path, Path},
                {type, Type}, {what, What},
                {trace, erlang:get_stacktrace()}],
      error_logger:error_report(Report),
      %% NOTE: mustache templates need \ because they are not awesome.
      Req:respond({500, [{"Content-Type", "text/plain"}],
                   "request failed, sorry\n"})
  end.

%% Internal API

serve_text(Req, Text) ->
  Req:respond({200, [{"Content-Type", "text/plain"}], Text }).

get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
  ?assertEqual("No, but I will!", "No, but I will!"),
  ok.

-endif.
