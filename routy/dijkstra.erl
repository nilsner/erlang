-module(dijkstra).

-export([table/2, route/2]).

% return length of the shortest path to Node 0 if node is not found
entry(Node, Sorted) ->
  case lists:keyfind(Node, 1, Sorted) of
      {_, N, _} -> N;
      false ->  0
  end.

% replaces the entry for Node in Sorted with a new entry having a new length N and Gateway.
replace(Node, N, Gateway, Sorted) ->
  lists:keysort(2, lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway})).

% update the list Sorted given the information that Node can be reached in N hops using Gateway 
% and that its a shorter path.
update(Node, NewLength, Gateway, Sorted) ->
  Length = entry(Node, Sorted),
  if NewLength < Length -> replace(Node, NewLength, Gateway, Sorted);
     true -> Sorted
  end.

% constructs a table given a sorted list of nodes, a map and a table constructed so far
iterate([], _, Table) ->
  Table;
iterate([{_,inf,_}|_], _, Table) ->
  Table;

% take first entry in the sorted list, find the nodes reachable (linked to) from this entry
% for each of these nodes update the sorted list
% add the entry in the routing table
iterate([{Node, Length, Gateway}|T], Map, Table) ->
  ReachableNodes = map:reachable(Node, Map),
  NewSortedList = lists:foldl(fun(N, Sorted) ->
    update(N, Length+1, Gateway, Sorted)
  end, T, ReachableNodes),
  iterate(NewSortedList, Map, [{Node, Gateway} | Table]).

% constructs a routing table with one entry per node in the Map
% dummy entries with length set to infinity and gateway to unknown
% inf is greater than any integer
% the gateways should be set to themselves with the length 0
table(Gateways, Map) ->
  MapNodes = map:all_nodes(Map),
  InfList = lists:map(fun(Node) -> {Node, inf, unknown} end, MapNodes),
  SortedList = lists:foldl(fun(Node, List) -> update(Node, 0, Node, List) end, InfList, Gateways),
  %io:format("SortedList: ~p~n", [SortedList]),
  %io:format("Inflist: ~p~n", [InfList]),
  iterate(SortedList, Map, []).

% search the routing table
% return the gateway suitable to route messages to a node
route(Node, Table) ->
  case lists:keyfind(Node, 1, Table) of
      {_, Gateway} -> {ok, Gateway};
      false ->  notfound
  end.