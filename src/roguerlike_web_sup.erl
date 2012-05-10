-module(roguerlike_web_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Player, Dungeon) ->
  Key = session_store:generate_key(),
  case supervisor:start_child(?SERVER, [Player, Dungeon, Key]) of
    { ok, _Pid } -> {ok, Key};
    Response -> Response
  end.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Web = web_specs(roguerlike_web, 8080),
    Processes = [Web],
    Strategy = {one_for_one, 10, 10},
    {ok,
     {Strategy, lists:flatten(Processes)}}.

web_specs(Mod, Port) ->
    WebConfig = [{ip, {0,0,0,0}},
                 {port, Port},
                 {docroot, roguerlike_deps:local_path(["priv", "www"])}],
    {Mod,
     {Mod, start, [WebConfig]},
     permanent, 5000, worker, dynamic}.
