
-module(roguerlike_sup).

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
  supervisor:start_child(?SERVER, [Player, Dungeon]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Element = ?CHILD(dungeoner, worker),
  Children = [Element],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, { RestartStrategy, Children} }.

