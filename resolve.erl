-module(resolve).
-include_lib("eunit/include/eunit.hrl").

-include("creatures.hrl").

-export([resolve/2, get_mob_damage/1]).


resolve(Player, Tile) when is_record(Tile, tile) ->
  #tile{ creatures = Creatures } = Tile,
  { NewPlayer, NewCreatures } = resolve_creatures(Player, Creatures),
  { NewPlayer, #tile{ creatures = NewCreatures }}.

resolve_creatures(_Player, []) ->
  [];

resolve_creatures(Player, [Creature|Tail]) when is_record(Creature, creature) ->
  { ResolvedPlayer, ResolvedCreature } = resolve_creature(Player, Creature),
  resolve_creatures(ResolvedPlayer, Tail, [ResolvedCreature]).

resolve_creatures(Player, [Creature|Tail], ResolvedCreatures) ->
  { ResolvedPlayer, ResolvedCreature } = resolve_creature(Player, Creature),
  resolve_creatures(ResolvedPlayer, Tail, [ResolvedCreature | ResolvedCreatures]);

resolve_creatures(Player, [], ResolvedCreatures) ->
  { Player, ResolvedCreatures }.


resolve_creature(Player, Creature) ->
  #creature{ health = CreatureHealth, damage = CreatureDamage } = Creature,
  #creature{ health = PlayerHealth, damage = PlayerDamage } = Player,
  ResolvedCreature = Creature#creature{ health = CreatureHealth - PlayerDamage },

  case ResolvedCreature#creature.health of
    Health when Health =< 0 ->
      { Player, ResolvedCreature#creature{ health = 0 } };
    _ ->
      { Player#creature{ health = PlayerHealth - CreatureDamage }, ResolvedCreature }
  end.

    
get_mob_damage(#creature{ health = Health }) ->
  io:format("This mob has ~p life~n", [Health]).


%% Tests
resolve_player_winning_test() ->
  Player = #creature { health = 20, damage = 5 },
  FirstCreature = #creature { health = 5, damage = 1 },
  SecondCreature = #creature { health = 5, damage = 1 },
  Tile = #tile { creatures = [ FirstCreature, SecondCreature ] },
  { NewPlayer, _NewTile } = resolve(Player, Tile),

  ?assertEqual(20, NewPlayer#creature.health).

resolve_player_doesnt_die_test() ->
  Player = #creature { health = 20, damage = 1 },
  FirstCreature = #creature { health = 5, damage = 1 },
  SecondCreature = #creature { health = 5, damage = 1 },
  Tile = #tile { creatures = [ FirstCreature, SecondCreature ] },
  { NewPlayer, _NewTile } = resolve(Player, Tile),

  ?assertEqual(18, NewPlayer#creature.health).

