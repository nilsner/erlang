-module(loggerVector).
-export([start/1, stop/1]).

start(Nodes) ->
spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
Logger ! stop.

init(Nodes) ->
loop(timeVector:clock(Nodes), []).



loop(Counter, HoldBackQueue) ->
    receive
        {log, From, Time, Msg} ->
            %io:format("counter: ~w~n", [Counter]),
            % io:format("hold back queue: ~w~n", [HoldBackQueue]),
            NewCounter = timeVector:update(From, Time, Counter),
            %io:format("new counter: ~w~n", [NewCounter]),
            NewQueue = [{From, Time, Msg} | HoldBackQueue],
            % io:format("new queue: ~w~n", [NewQueue]),
            SortQueue = lists:keysort(2, NewQueue),
            io:format("sorted queue: ~w~n", [SortQueue]),
            NewHoldBQ = safeToPrint(SortQueue, NewCounter, []),
            io:format("new hold back queue: ~w~n~n", [NewHoldBQ]),
            io:format("~n~n", []),
            loop(NewCounter, NewHoldBQ);
        stop ->
        ok
    end.


log(From, Time, Msg) ->
io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

safeToPrint([], _, NewHBQ) ->
  NewHBQ; % end case, rest of the messages that are still not safe to log
safeToPrint([{From, Time, Msg} | T], Counter, NewHBQ) ->
  case timeVector:safe(Time, Counter) of
    true -> % can print the messages and remove from hold back queue
      log(From, Time, Msg), % print the message and remove from hold back queue
      safeToPrint(T, Counter, NewHBQ); % recursive call to check the rest of the messages
    false ->
      % push back to the end of hold back queue for message not safe to log atm
      safeToPrint(T, Counter, [{From, Time, Msg} | NewHBQ]) 
  end.

