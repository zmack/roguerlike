-module(dungeoner).
-include("creatures.hrl").

-behaviour(gen_server).

-export([init/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).
-export([start_link/2, create/2 ]).

-define(SERVER, ?MODULE).
-define(DEFAULT_LEASE_TIME, 60 * 60 * 24).

-record(state, { dungeon, player, start_time }).

start_link(Player, Dungeon) ->
  gen_server:start_link(?MODULE, [Player, Dungeon], []).

create(Player, Dungeon) ->
  roguerlike_sup:start_child(Player, Dungeon).

init() ->
  ok.

init([Player, Dungeon]) ->
  Now = calendar:local_time(),
  StartTime = calendar:datetime_to_gregorian_seconds(Now),
  { ok,
    #state{
      player = Player,
      dungeon = Dungeon,
      start_time = StartTime
    }
  }.

handle_cast(_Message, State) ->
  { ok, State }.

handle_info(timeout, State) ->
  { stop, normal, State }.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

handle_call({ move, { PlayerX, PlayerY }}, _From, State) ->
  #state{
    dungeon = Dungeon,
    player = Player
  } = State,
  { NewPlayer, NewDungeon } = resolve:resolve_dungeon(Player#player{ x = PlayerX, y = PlayerY }, Dungeon),
  NewState = State#state{ player = NewPlayer, dungeon = NewDungeon }, 
  { reply, NewState, NewState };
handle_call(_Message, _From, State) ->
  { ok, State }.
