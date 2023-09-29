-module(testVector).
-export([run/2]).

run(Sleep, Jitter) ->
Log = loggerVector:start([john,paul,ringo, george]),
A = workerVector:start(john, Log, 13, Sleep, Jitter),
B = workerVector:start(paul, Log, 23, Sleep, Jitter),
C = workerVector:start(ringo, Log, 36, Sleep, Jitter),
D = workerVector:start(george, Log, 49, Sleep, Jitter),
workerVector:peers(A, [B, C, D]),
workerVector:peers(B, [A, C, D]),
workerVector:peers(C, [A, B, D]),
workerVector:peers(D, [A, B, C]),
timer:sleep(5000),
loggerVector:stop(Log),
workerVector:stop(A),
workerVector:stop(B),
workerVector:stop(C),
workerVector:stop(D).