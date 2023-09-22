-module(time).
-export([zero/0, inc/1, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
    0.

% return the time T incremented by one (you will probably ignore the Name, but we will use it later)
inc(T) ->
    T + 1.

merge(Ti, Tj) -> 
    max(Ti, Tj).

leq(Ti,Tj) ->
    if Tj > Ti -> 
            false;
        true ->
		    true
    end.

% return a clock with all nodes in the list Nodes and time zero
clock(Nodes) ->
    lists:map(fun(Node) -> {Node,0} end, Nodes).

% update the clock Clock to reflect that Node has time
update(Node, Time, Clock) ->
  UpdatedClock = lists:keyreplace(Node, 1, Clock, {Node, Time}),
  lists:keysort(2, UpdatedClock).

% return true if the clock Clock is safe
safe(Time, Clock) ->
    lists:all(fun({_, T}) -> leq(T, Time) end, Clock).