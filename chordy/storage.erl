-module(storage).
-export([create/0, add/3, lookup/2, split/3, merge/2]).

create() ->
    [].

add(Key, Value, Store) ->
    lists:keystore(Key, 1, Store, {Key, Value}).

lookup(Key, Store) ->
    lists:keyfind(Key, 1, Store).

split(From, To, Store) -> % split the store on from and to and return updated store
   lists:partition(fun({Key,_})-> key:between(Key, From, To) end, Store). % splits into two different list, the first part of the list is inbetween from and to

merge(Entries, Store) ->
    lists:merge(fun({Key1, _}, {Key2, _}) -> Key1 < Key2 end, Entries, Store).
