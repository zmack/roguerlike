application:start(roguerlike).
rr("include/creatures.hrl").
Player = #player{ health = 10, damage = 1 }.
instance:create(Player, []).
% instance:move(Key, {1, 1}).
