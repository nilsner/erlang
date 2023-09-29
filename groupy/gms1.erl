-module(gms1).
-export([leader/4, start/1, init/2, slave/5, start/2, init/3]).
-define(arghh, 100).

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
        bcast(Id, {msg, Msg}, Slaves),
        Master ! Msg,
        leader(Id, Master, Slaves, Group);
    {join, Wrk, Peer} -> %  amessage from a peer or the master that is a
                         % request from a node to join the group. The message contains the pro-
                         % cess identifier of the application layer, Wrk, and the process identifier
                         % of its group process.
        Slaves2 = lists:append(Slaves, [Peer]),
        Group2 = lists:append(Group, [Wrk]),
        bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
        Master ! {view, Group2},
        leader(Id, Master, Slaves2, Group2);
    stop ->
    ok
end.

start(Id, Grp) -> % the slave process is started by the master process.
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) -> % the slave process is initialized with the process identifier
                         % of the master process.
    Self = self(),
    Grp ! {join, Master, Self},
    receive
    {view, [Leader|Slaves], Group} ->
        Master ! {view, Group},
        slave(Id, Master, Leader, Slaves, Group)
end.

slave(Id, Master, Leader, Slaves, Group) ->
receive
    {mcast, Msg} -> % a request from its master to multicast a message, the
                    % message is forwarded to the leader.
        Leader ! {mcast, Msg},
        slave(Id, Master, Leader, Slaves, Group);
    {join, Wrk, Peer} -> %  request from the master to allow a new node
                         % to join the group, the message is forwarded to the leader.
        Leader ! {join, Wrk, Peer},
        slave(Id, Master, Leader, Slaves, Group);
    {msg, Msg} -> % a multicasted message from the leader. A message Msg
                  % is sent to the master.
        Master ! Msg,
        slave(Id, Master, Leader, Slaves, Group);
    {view, [Leader|Slaves2], Group2} -> % a multicasted view from the leader. A view
                                        % is delivered to the master process.
        Master ! {view, Group2},
        slave(Id, Master, Leader, Slaves2, Group2);
    stop ->
    ok
end.

bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
    case random:uniform(?arghh) of
    ?arghh ->
        io:format("leader ~w: crash~n", [Id]),
        exit(no_luck);
    _ ->
        ok
    end.