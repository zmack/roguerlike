-module(resolve).
-include_lib("eunit/include/eunit.hrl").

-include("creatures.hrl").
-import(lists).

-export([resolve/2]).

is_tile_resolved(#tile{ creatures = [] }) ->
  true;

is_tile_resolved(#tile{ creatures = Creatures }) ->
  lists:all(fun (A) -> is_creature_dead(A) end, Creatures).

is_creature_dead(#creature{ health = Health }) ->
  Health == 0.

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

is_tile_resolved_works_for_empty_lists_test() ->
  Tile = #tile { creatures = [ ] },
  ?assertEqual(true, is_tile_resolved(Tile)).

is_tile_resolved_works_for_lists_test() ->
  FirstCreature = #creature { health = 0, damage = 1 },
  SecondCreature = #creature { health = 0, damage = 1 },
  Tile = #tile { creatures = [ FirstCreature, SecondCreature ] },

  ?assertEqual(true, is_tile_resolved(Tile)).

is_tile_resolved_returns_false_for_nondead_creature_lists_test() ->
  FirstCreature = #creature { health = 0, damage = 1 },
  SecondCreature = #creature { health = 1, damage = 1 },
  Tile = #tile { creatures = [ FirstCreature, SecondCreature ] },

  ?assertEqual(false, is_tile_resolved(Tile)).

