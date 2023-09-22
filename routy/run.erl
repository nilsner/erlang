-module(run).
-export([start/0, stop/0]).

start() ->

routy:start(r1, sthlm),

routy:start(r2, uppsala),

routy:start(r3, gbg),

routy:start(r4, malmo),

routy:start(r5, karlskrona),

r1 ! {add, uppsala, {r2, node()}},

r2 ! {add, sthlm, {r1, node()}},

r3 ! {add, uppsala, {r2, node()}},

r2 ! {add, gbg, {r3, node()}},

r4 ! {add, gbg, {r3, node()}},

% r5 ! {add, gbg, {r3, node()}}, % old

r5 ! {add, malmo, {r4, node()}}, % new

%r3 ! {add, malmo, {r4, node()}},

r3 ! {add, karlskrona, {r5, node()}},

r1 ! broadcast,
timer:sleep(500),
r2 ! broadcast,
timer:sleep(500),
r3 ! broadcast,
timer:sleep(500),
r4 ! broadcast,
timer:sleep(500),
r5 ! broadcast,
timer:sleep(500), 
r3 ! broadcast,
timer:sleep(500), 
r1 ! update,
timer:sleep(500),
r2 ! update,
timer:sleep(500),
r3 ! update,
timer:sleep(500),
r4 ! update,
timer:sleep(500),
r5 ! update,
timer:sleep(500),
r1 ! {send, malmo, 'Please answer me!'},
timer:sleep(500),
r3 ! {send, sthlm, 'Ohh, hello there!'},
timer:sleep(500),
r2 ! {send, sthlm, 'Hello sthlm'},
timer:sleep(500),
r5 ! {send, sthlm, 'All the way back'},
timer:sleep(500),
% r2 ! {send, gbg, 'Are you also here??'},

io:format("Start of status~n~n"),
r1 ! {status, self()},
r2 ! {status, self()},
r3 ! {status, self()},
r4 ! {status, self()},
r5 ! {status, self()},

io:format("End of status~n~n"),

r5 ! stop,

r2 ! {remove, sthlm}.

stop() ->
    routy:stop(r1),
    routy:stop(r2),
    routy:stop(r3),
    routy:stop(r4),
    routy:stop(r5).