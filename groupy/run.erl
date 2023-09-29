-module(run).
% Remove the duplicate module definition
-export([start/0]).

start() ->
Leader = worker2:start(1,gms3,12,200),
worker2:start(2,gms3,11,Leader,200),
worker2:start(3,gms3,13,Leader,200).
