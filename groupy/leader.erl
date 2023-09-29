-module(leader).
-export([leader/4, start/1, init/2]).

start(Id) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Self) end)}.

init(Id, Master) ->
    leader(Id, Master, [], [Master]).

leader(Id, Master, Slaves, Group) ->
    receive
    {mcast, Msg} -> % a message either from its own master or from a peer
                    % node. A message {msg, Msg} is multicasted to all peers, and a message
                    % Msg is sent to the application layer.
        erl:bcast(Id, {msg, Msg}, Slaves),
        Master ! Msg,
        leader(Id, Master, Slaves, Group);
    {join, Wrk, Peer} -> %  amessage from a peer or the master that is a
                         % request from a node to join the group. The message contains the pro-
                         % cess identifier of the application layer, Wrk, and the process identifier
                         % of its group process.
        Slaves2 = lists:append(Slaves, [Peer]),
        Group2 = lists:append(Group, [Wrk]),
        erl:bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
        Master ! {view, Group2},
        leader(Id, Master, Slaves2, Group2);
    stop ->
    ok
end.