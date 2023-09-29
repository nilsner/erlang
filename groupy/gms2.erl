-module(gms2).
-export([leader/4, start/1, init/3, slave/5, start/2, init/4, crash/1]).
-define(arghh, 1000).
-define(timeout, 2000).

start(Id) ->
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun() -> 
        init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
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
    io:format("grp ~w~n", [Grp]),
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Grp, Self, Rnd) end)}.

init(Id, Grp, Master, Rnd) -> % the slave process is initialized with the process identifier
    % of the master process.
    Self = self(),
    random:seed(Rnd, Rnd, Rnd),
    Grp ! {join, Master, Self},
    receive
        {view, [Leader|Slaves], Group} ->
            Master ! {view, Group},
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, Slaves, Group)
    after ?timeout ->
        Master ! {error, "no reply from leader"}
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
    {"DOWN", _Ref, process, Leader, _Reason} ->
        election(Id, Master, Slaves, Group);
    stop ->
    ok
end.

election(Id, Master, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
    [Self|Rest] ->
        bcast(Id, {view, Slaves, Group}, Rest),
        Master ! {view, Group},
        leader(Id, Master, Rest, Group);
    [Leader|Rest] ->
        erlang:monitor(process, Leader),
        slave(Id, Master, Leader, Rest, Group)
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