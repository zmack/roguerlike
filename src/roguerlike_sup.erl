-module(roguerlike_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Player, Dungeon) ->
  Key = instance_store:generate_key(),
  case supervisor:start_child(?SERVER, [Player, Dungeon, Key]) of
    { ok, _Pid } -> {ok, Key};
    Response -> Response
  end.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  SupervisorFlags = {simple_one_for_one, 0, 1},
  ChildSpecs = [
      {instance,
       {instance, start_link, []},
       permanent,
       5000,
       worker,
       [instance]
      }
  ],
  {ok, {SupervisorFlags, ChildSpecs}}.

