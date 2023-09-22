-module(interface).

-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() ->
    [].

add(Name, Ref, Pid, Intf) ->
    [{Name, Ref, Pid} | Intf].

remove(Name, Intf) ->
    lists:keydelete(Name, 1, Intf).

lookup(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
      {_, _, Pid} -> {ok, Pid};
      false -> notFound
    end.

ref(Name, Intf) ->
   case lists:keyfind(Name, 1, Intf) of
      {_, Ref, _} -> {ok, Ref};
      false -> notFound
    end.

name(Ref, Intf) ->
    case lists:keyfind(Ref, 2, Intf) of
      {Name, _, _} -> {ok, Name};
      false -> notFound
    end.

list([]) -> [];
list([{Name, _, _}|T]) ->
  [Name | list(T)].

broadcast(Message, Intf) ->
    lists:foreach(fun({_Name, _Ref, Pid}) -> Pid ! Message end, Intf).