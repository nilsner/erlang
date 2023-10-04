-module(run).
% Remove the duplicate module definition
-export([start/0]).

start() ->
Leader = test:first(1,gms3,1000),   
test:add(2,gms3,Leader,1000),
test:add(3,gms3,Leader,1000),
timer:sleep(5000),
test:add(4,gms3,Leader,1000).
