application:start(roguerlike).
rr("include/creatures.hrl").
Player = #player{ health = 10, damage = 1 }.
{ok, Pid} = dungeoner:create(Player, []).
gen_server:call(Pid, { move, {1, 1}}).
