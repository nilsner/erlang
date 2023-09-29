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
            NewQueue = [{From, lists:keysort(2, Time), Msg} | HoldBackQueue],
            %io:format("new queue: ~w~n", [NewQueue]),
            % io:format("new queue: ~w~n", [NewQueue]),
            %SortQueue = lists:keysort(2, NewQueue),
            %io:format("sorted queue: ~w~n", [SortQueue]),
            NewHoldBQ = safeToPrint(NewQueue, NewCounter, []),
            %io:format("new hold back queue: ~w~n~n", [NewHoldBQ]),
            %io:format("~n~n", []),
            loop(NewCounter, NewHoldBQ);
        stop ->
          Count = length(HoldBackQueue),
          io:format("Size of holdBackQueue: ~w~n", [Count]),
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
      %io:format("counter: ~w~n", [Counter]),
      safeToPrint(T, Counter, NewHBQ); % recursive call to check the rest of the messages
    false ->
      % push back to the end of hold back queue for message not safe to log atm
      SortedList = sort_tuples([{From, Time, Msg} | NewHBQ]),
      %safeToPrint(T, Counter, SortedList) 
      safeToPrint(T, Counter, SortedList) 
  end.

sort_tuples(TupleList) ->
    % Define a helper function to calculate the combined time for a tuple
    CalculateCombinedTime = fun(Tuple) ->
        {_, TimeList, _} = Tuple,
        lists:sum([Time || {_, Time} <- TimeList])
    end,

    % Sort the tuple list based on combined time
    lists:sort(fun(A, B) ->
        CombinedTimeA = CalculateCombinedTime(A),
        CombinedTimeB = CalculateCombinedTime(B),
        CombinedTimeA < CombinedTimeB
    end, TupleList).