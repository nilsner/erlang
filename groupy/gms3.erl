-module(gms3).
-export([start/1, start/2]).
-define(arghh, 300).
-define(timeout, 1000).

start(Id) -> % the master process is started by the application layer.
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun() -> 
        init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) -> % the master process is initialized with its own process
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, 1, [], [Master]).

leader(Id, Master, N, Slaves, Group) -> % the leader process is in the state
    receive
        {mcast, Msg} -> % Multicast message to all group memebers
            io:format("gms ~w: received {mcast, ~w} ~n", [Id, Msg]),
            bcast(Id, {msg, N, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, N + 1, Slaves, Group);
        {join, Wrk, Peer} -> % Join request from a new worker
            io:format("gms ~w: forward join from ~w to master~n", [Id, Peer]),
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, N, [self() | Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, N + 1, Slaves2, Group2);
        stop ->
            ok
    end.


% Slave below

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
    io:format("Group ~w: join request sent to ~w~n", [Grp, Master]),
    receive
        {view, N, [Leader|Slaves], Group} -> 
            Master ! {view, Group},
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, N+1, {view, N,[Leader|Slaves]}, Slaves, Group)
    after ?timeout ->
        Master ! {error, "no reply from leader"}
end.

slave(Id, Master, Leader, N, Last, Slaves, Group) -> % the slave process is in the state
    receive
        {mcast, Msg} -> % a request from its master to multicast a message, the
                        % message is forwarded to the leader.
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, N2, _} when N2 < N -> % Check if the message is old
            io:format("OLD MESSAGE~n", []),
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, N, Msg} -> % a multicasted message from the leader. A message
            Master ! Msg,
            slave(Id, Master, Leader, N + 1, {msg, N, Msg}, Slaves, Group);
        {view, N, [Leader | Slaves2], Group2}  -> % a multicasted view from the leader. A view
            Master ! {view, Group2},
            slave(Id, Master, Leader, N + 1, {view, N, [Leader | Slaves2], Group2},
                Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, _} -> % Leader is down
            election(Id, Master, N, Last, Slaves, Group);
        stop ->
            ok
    end.

election(Id, Master, N, Last, Slaves, [_ | Group]) -> % election of a new leader
    Self = self(),
    case Slaves of
        [Self | Rest] -> 
            bcast(Id, Last, Rest),
            bcast(Id, {view, N, Slaves, Group}, Rest),
            Master ! {view, Group},
            io:format("Your new Leader is: ~w~n", [Self]),
            leader(Id, Master, N + 1, Rest, Group);
        [Leader | Rest] -> 
            erlang:monitor(process, Leader),
            io:format("Your new Leader is ~w~n", [Leader]),
            slave(Id, Master, Leader, N, Last, Rest, Group)
    end.

bcast(Id, Msg, Nodes) -> % the message Msg is multicasted to all nodes in the
    lists:foreach(fun(Node) -> 
        Node ! Msg, crash(Id) end, Nodes).

crash(Id) -> % the process crashes with probability 1/100.
    case random:uniform(?arghh) of
    ?arghh ->
        io:format("leader ~w: crash~n", [Id]),
        exit(no_luck);
    _ ->
        ok
    end.