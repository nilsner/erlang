-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

 % empty list of nodes
new() ->
    [].

% updates the Map to reflect that Node
% has directional links to all nodes in the list Links. The old entry is
% removed.
update(Node, Links, Map) ->
    NewMap = lists:keydelete(Node, 1, Map),
    [{Node, Links} | NewMap].

% returns the list of nodes directly reachable from Node.
reachable(Node, Map) ->
    case lists:keyfind(Node, 1, Map) of
      {Node, Links} -> Links;
      false ->  []
    end.

% returns a list of all nodes in the Map, also the ones without outgoing links.
all_nodes([]) -> [];
all_nodes([{Node, Links} | T ]) ->
  List = [Node, Links | all_nodes(T)],
  lists:usort(lists:flatten(List)).