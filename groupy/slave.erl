-module(slave).
-export([slave/5, start/2, init/3]).

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