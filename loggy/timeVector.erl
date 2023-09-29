-module(timeVector).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

%returns initial timestamp
zero() -> [].

inc(Name, Time) ->
    case lists:keyfind(Name, 1, Time) of    
        {Name, Timestamp} ->
        lists:keyreplace(Name, 1, Time, {Name, Timestamp + 1});
    false ->
        [{Name, 1}|Time]
    end.

merge([], Time) ->
Time;
merge([{Name, Ti}|Rest], Time) ->
case lists:keyfind(Name, 1, Time) of
{Name, Tj} ->
[{Name, erlang:max(Ti,Tj)}|merge(Rest, lists:keydelete(Name, 1, Time))];
false ->
[{Name, Ti} |merge(Rest, Time)]
end.
    

% leq([], _) ->
% true;
% leq([{Name, Ti}|Rest],Time) ->
%     case lists:keyfind(Name, 1, Time) of
%         {Name, Tj} ->
%         if
%         Ti =< Tj ->
%             true;
%     true ->
%         false
%     end;
%     false ->
%         false
%     end.

leq([], _) ->
    true;
leq([{Name, Ti}|Rest],Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, Tj} ->
            if
            Ti =< Tj ->
                leq(Rest, Time);
            true ->
                false
            end;
        false ->
            false
    end.

clock(_) ->
    [].

% return a clock that has been updated with Time
% Time is the new Time vector and Clock is all the vectors
update(From, Time, Clock) ->
    Msg = lists:keyfind(From, 1, Time),
    %io:format("msg: ~w~n", [Msg]),
    %io:format("clock: ~w~n", [Clock]),
    %io:format("time: ~w~n", [Time]),
    %io:format("from: ~w~n", [From]),
    case lists:keyfind(From, 1, Clock) of
        {From, _} ->
            lists:keyreplace(From, 1, Clock, Msg);
        false ->
            [Msg| Clock]
    end.

safe(Time, Clock) ->
    %io:format("Time: ~w~n", [Time]),
    %io:format("Clock: ~w~n", [Clock]),
    leq(Time, Clock).
    %lists:all(fun({_, T}) -> leq(T, Time) end, Clock).