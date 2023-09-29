-module(workerVector).
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive
    {peers, Peers} ->
        loop(Name, Log, Peers, Sleep, Jitter, timeVector:zero());
    stop ->
        ok
end.

peers(Wrk, Peers) ->
    % io:format("peers: ~w~n", [Peers]),
    % io:format("wrk: ~w~n", [Wrk]),
    % io:format("~n"),
Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Timestamp)->
    Wait = random:uniform(Sleep),
    receive
    {msg, Time, Msg} ->
        % io:format("Name: ~w~n", [Name]),
        % io:format("time: ~w~n", [Time]),
        % io:format("timestamp: ~w~n", [Timestamp]),
        IncrementedTime = timeVector:inc(Name,timeVector:merge(Timestamp, Time)),
        % io:format("incremented time: ~w~n", [IncrementedTime]),
        %io:format("~n"),
        Log ! {log, Name, IncrementedTime, {received, Msg}},
        loop(Name, Log, Peers, Sleep, Jitter, IncrementedTime);
    stop ->
        ok;
    Error ->
        Log ! {log, Name, time, {error, Error}}
    
    after Wait ->
        Selected = select(Peers),
        %io:format("Timestamp: ~w~n", [Timestamp]),
        %io:format("Name: ~w~n", [Name]),
        IncrementedTime = timeVector:inc(Name,Timestamp), % update the clock for a specific worker
        %io:format("Time: ~w~n", [Time]),
        Message = {hello, random:uniform(100)},
        Selected ! {msg, IncrementedTime, Message},
        jitter(Jitter),
        Log ! {log, Name, IncrementedTime, {sending, Message}},
        loop(Name, Log, Peers, Sleep, Jitter, IncrementedTime)
end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
