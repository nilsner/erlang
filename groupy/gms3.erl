-module(gms3).
-export([leader/5, start/1, init/3, slave/7, start/2, init/4, crash/1]).
-define(arghh, 1000).
-define(timeout, 2000).

start(Id) ->
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun() -> 
        init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, [], [Master], 0).

leader(Id, Master, Slaves, Group, N) ->
    receive
    {mcast, Msg} -> 
        bcast(Id, {msg, N, Msg}, Slaves),
        Master ! Msg,
        leader(Id, Master, Slaves, Group, N+1);
    {join, Wrk, Peer} -> 
        Slaves2 = lists:append(Slaves, [Peer]),
        Group2 = lists:append(Group, [Wrk]),
        bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
        Master ! {view, Group2},
        leader(Id, Master, Slaves2, Group2, N+1);
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
        {view, N, [Leader|Slaves], Group} ->
            Master ! {view, Group},
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, Slaves, Group, N+1, {view, N, [Leader|Slaves], Group})
    after ?timeout ->
        Master ! {error, "no reply from leader"}
end.

slave(Id, Master, Leader, Slaves, Group, N, Last) ->
receive
    {msg, I, _} when I < N ->
			slave(Id,Master,Leader,N,Last,Slaves,Group);
    {mcast, Msg} -> % a request from its master to multicast a message, the
                    % message is forwarded to the leader.
        Leader ! {mcast, Msg},
        slave(Id, Master, Leader, N, Last, Slaves, Group);
    {join, Wrk, Peer} -> %  request from the master to allow a new node
                         % to join the group, the message is forwarded to the leader.
        Leader ! {join, Wrk, Peer},
        slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, N, Msg} -> % a multicasted message from the leader. A message Msg
                  % is sent to the master.
        Master ! Msg,
        slave(Id, Master, Leader, N, Last, Slaves, Group);
    {view, N, [Leader|Slaves2], Group2} -> % a multicasted view from the leader. A view
                                        % is delivered to the master process.
        Master ! {view, Group2},
        slave(Id, Master, Leader, N, Last, Slaves2, Group2);
    {"DOWN", _Ref, process, Leader, _Reason} ->
        election(Id, Master, Slaves, Group, N, Last);
    stop ->
    ok
end.

election(Id, Master, Slaves, [_|Group], N, Last) ->
    Self = self(),
    case Slaves of
    [Self|Rest] ->
        bcast(Id, Last, Rest),
        bcast(Id, {view, N, Slaves, Group}, Rest),
        Master ! {view, Group},
        leader(Id, Master, Rest, Group, N+1);
    [Leader|Rest] ->
        erlang:monitor(process, Leader),
        slave(Id, Master, Leader, N, Last, Rest, Group)
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