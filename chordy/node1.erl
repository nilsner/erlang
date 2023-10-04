-module(node1).
-export([node/3, start/1, start/2, connect/2]).
-define(Stabilize, 1000).
-define(Timeout, 1000).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor).

node(Id, Predecessor, Successor) ->
    receive
        {key, Qref, Peer} -> % peer needs to know our key
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);
        {notify, New} -> % new node informs us of its existence
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
        {request, Peer} -> % a predecessor needs to know our predecessor
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
        {status, Pred} -> % our successor informs us about its predecessor
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor)
    end.

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
        case Pred of
            nil ->
                Spid ! {notify, {Id, self()}};  
            {Id, _} ->
                Successor;
            {Skey, _} ->
                Spid ! {notify, {Id, self()}},
	            Successor;
            {Xkey, Xpid} -> % XKey is the precessor of our succesor
                case key:between(Xkey, Id, Skey) of
                true ->
                    Xpid ! {request, self()},
		            Pred;
                false ->
                    Spid ! {notify, {Id, self()}},
		            Successor
        end
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
        nil ->
            {Nkey, Npid};
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    {Nkey, Npid};
                false ->
                    Predecessor
                end
    end.

connect(Id, nil) ->
    {ok, {Id, self()}}; %ID??
connect(Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {Skey, Peer}}
    after ?Timeout ->
        io:format("Time out: no response~n",[])
    end.

create_probe(Id, Successor) ->
    {_Id, Pid} = Successor,
    Pid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

remove_probe(T, Nodes) ->
    TotalTime = erlang:system_time(micro_seconds) - T,
    io:format("Nodes: ~p~n", [Nodes]),
    io:format("Time: ~w micro seconds~n", [TotalTime]).

forward_probe(Ref, Time, Nodes, Id, {_, Spid}) ->
  Spid ! {probe, Ref, [Id | Nodes], Time}.