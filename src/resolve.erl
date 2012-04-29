-module(resolve).
-include_lib("eunit/include/eunit.hrl").

-include("creatures.hrl").

-export([resolve_dungeon/2]).

is_tile_resolved(#tile{ creatures = [] }) ->
  true;

is_tile_resolved(#tile{ creatures = Creatures }) ->
  lists:all(fun (A) -> is_creature_dead(A) end, Creatures).

is_creature_dead(#creature{ health = Health }) ->
  Health == 0.

resolve_dungeon(Player, Dungeon) ->
  { Player, Dungeon }.

get_tile_at([{ TileX, TileY, Tile }|_Tail], { X, Y }) when (TileX == X) and (TileY == Y) ->
  Tile;
get_tile_at([_Head|Tail], { X, Y }) ->
  get_tile_at(Tail, { X, Y });
get_tile_at([], { _, _ }) ->
  false.

replace_tile_at(Dungeon, { X, Y }, NewTile) ->
  lists:map(fun(Element) ->
        case Element of
          { X, Y, _Tile } -> { X, Y, NewTile };
          _ -> Element
        end
    end, Dungeon).


resolve_tile(Player, Tile) when is_record(Tile, tile) ->
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
create_creature_list([{Health, Damage}|Tail]) ->
  Creature = #creature{ health = Health, damage = Damage },
  create_creature_list(Tail, [Creature]).

create_creature_list([{Health, Damage}|Tail], Creatures) ->
  Creature = #creature{ health = Health, damage = Damage },
  create_creature_list(Tail, [Creature | Creatures]);
create_creature_list([], Creatures) ->
  Creatures.

resolve_player_winning_test() ->
  Player = #creature { health = 20, damage = 5 },
  FirstCreature = #creature { health = 5, damage = 1 },
  SecondCreature = #creature { health = 5, damage = 1 },
  Tile = #tile { creatures = [ FirstCreature, SecondCreature ] },
  { NewPlayer, _NewTile } = resolve_tile(Player, Tile),

  ?assertEqual(20, NewPlayer#creature.health).

resolve_player_doesnt_die_test() ->
  Player = #creature { health = 20, damage = 1 },
  FirstCreature = #creature { health = 5, damage = 1 },
  SecondCreature = #creature { health = 5, damage = 1 },
  Tile = #tile { creatures = [ FirstCreature, SecondCreature ] },
  { NewPlayer, _NewTile } = resolve_tile(Player, Tile),

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

create_creature_list_creates_a_list_of_creatures_test() ->
  Creatures = create_creature_list([{ 10, 1 }, { 10, 2 }]),
  ?assertEqual(2, length(Creatures)),
  ?assert(lists:all(
      fun(Creature) ->
          is_record(Creature, creature)
      end, Creatures)
  ).

resolve_dungeon_resolves_a_dungeon_using_a_player_test() ->
  Creatures = create_creature_list([{ 10, 1 }, { 10, 2 }]),
  Tile = #tile{ creatures = Creatures },
  Dungeon = [ { 1, 1, Tile } ],
  Player = #player{ x = 1, y = 1, health = 10, damage = 5 },
  { NewPlayer, _NewDungeon } = resolve_dungeon(Player, Dungeon),
  ?assertEqual(10, NewPlayer#player.health),
  ?assertEqual(5, NewPlayer#player.damage).

get_tile_at_gets_a_tile_at_that_position_test() ->
  Creatures = create_creature_list([{ 10, 1 }, { 10, 2 }]),
  Tile = #tile{ creatures = Creatures },
  OtherTile = #tile{ creatures = [] },
  Dungeon = [ { 0, 0, OtherTile }, { 1, 1, Tile } ],
  FirstTile = get_tile_at(Dungeon, { 1, 1 }),
  SecondTile = get_tile_at(Dungeon, { 0, 0 }),
  ?assertEqual(Tile, FirstTile),
  ?assertEqual(OtherTile, SecondTile).

replace_tile_at_replaces_a_tile_at_that_position_test() ->
  Creatures = create_creature_list([{ 10, 1 }, { 10, 2 }]),
  Tile = #tile{ creatures = Creatures },
  OtherTile = #tile{ creatures = [] },
  ReplacementTile = #tile{},
  Dungeon = [ { 0, 0, OtherTile }, { 1, 1, Tile } ],
  NewDungeon = replace_tile_at(Dungeon, { 0, 0 }, ReplacementTile),
  ?assertEqual([{ 0, 0, ReplacementTile }, { 1, 1, Tile }], NewDungeon).

%% resolve_dungeon_resolves_dungeon_test() ->
