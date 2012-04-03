-module(resolve).
-include("creatures.hrl").
-export([resolve/2, getMobDamage/1]).


resolve(Player, Mob) ->
  Player#creature.health - Mob#creature.damage.
    

getMobDamage(#creature{ health = Health }) ->
  io:format("This mob has ~p life~n", [Health]).
