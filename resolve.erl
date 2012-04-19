-module(resolve).
-include_lib("eunit/include/eunit.hrl").

-include("creatures.hrl").

-export([resolve/2, getMobDamage/1]).


resolve(Player, Tile) when is_record(Tile, tile) ->
  #tile{ creatures = Creatures } = Tile,
  { NewPlayer, NewCreatures } = resolveCreatures(Player, Creatures),
  { NewPlayer, #tile{ creatures = NewCreatures }}.

resolveCreatures(_Player, []) ->
  [];

resolveCreatures(Player, [Creature|Tail]) when is_record(Creature, creature) ->
  { ResolvedPlayer, ResolvedCreature } = resolveCreature(Player, Creature),
  resolveCreatures(ResolvedPlayer, Tail, [ResolvedCreature]).

resolveCreatures(Player, [Creature|Tail], ResolvedCreatures) ->
  { ResolvedPlayer, ResolvedCreature } = resolveCreature(Player, Creature),
  resolveCreatures(ResolvedPlayer, Tail, [ResolvedCreature | ResolvedCreatures]);

resolveCreatures(Player, [], ResolvedCreatures) ->
  { Player, ResolvedCreatures }.

resolveCreature(Player, Creature) ->
  #creature{ health = CreatureHealth, damage = CreatureDamage } = Creature,
  #creature{ health = PlayerHealth, damage = PlayerDamage } = Player,
  ResolvedCreature = Creature#creature{ health = CreatureHealth - PlayerDamage },

  case ResolvedCreature#creature.health of
    Health when Health =< 0 ->
      { Player, ResolvedCreature#creature{ health = 0 } };
    _ ->
      { Player#creature{ health = PlayerHealth - CreatureDamage }, ResolvedCreature }
  end.

    
getMobDamage(#creature{ health = Health }) ->
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

