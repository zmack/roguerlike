% 
% Things:
%   - a map is made up of several tiles
%   - a tile has a tile type, and a list of creatures
%   - replace creature thing when resolving
-record(tile, { tile_type = "Default", creatures = [] }).

-record(creature_type, { name = "Random Mob", health = 0, damage = 0 }).

-record(creature, { health = 0, damage = 0 }).

-record(player, { x = 0, y = 0, health = 0, damage = 0 }).
