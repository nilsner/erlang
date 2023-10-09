-module(node3).
-export([node/5, start/1, start/2]).
-define(Stabilize, 1000).
-define(Timeout, 1000).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> 
        init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    io:format("Id: ~w~n", [Id]),
    io:format("Peer: ~w~n", [Peer]),
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, storage:create(), nil).

node(Id, Predecessor, Successor, Store, Next) ->
    receive
        {'DOWN', Ref, process, _, _} ->
            io:format("Got DOWN msg ~n", []),
            {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
            node(Id, Pred, Succ, Store, Nxt);
        {status, Pred, Nx} ->
            {Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
            node(Id, Predecessor, Succ, Nxt, Store);
        {key, Qref, Peer} -> % peer needs to know our key
            % io:format("In node, key~n"),
            % io:format("Qref: ~w~n", [Qref]),
            % io:format("Peer: ~w~n", [Peer]),
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store, Next);
        {notify, New} -> % new node informs us of its existence
            {Pred, UpdatedStore} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, UpdatedStore, Next);
        {request, Peer} -> % a predecessor needs to know our predecessor
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Store, Next);
        {status, Pred} -> % our successor informs us about its predecessor
            Succ = stabilize(Pred, Id, Successor, Next),
            node(Id, Predecessor, Succ, Store, Next);
        stabilize ->
            % io:format("Id: ~w~n", [Id]),
            % io:format("Predecessor: ~w~n", [Predecessor]),
            % io:format("Successor: ~w~n", [Successor]),
            % io:format("Store: ~w~n", [Store]),
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store, Next);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store, Next);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store, Next);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store, Next);
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added, Next);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store, Next);
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged, Next);
         status ->
            io:format("Node Id ~w \nPred ~w \nSucc ~w \nContent ~w ~n \n", [
                Id, Predecessor, Successor, Store
            ]),
            node(Id, Predecessor, Successor, Store, Next);
        state ->
	    io:format("ID: ~w~n", [Id]),
	    io:format("Predecessor: ~p, Successor: ~p~n", [Predecessor, Successor]),
	    io:format("Store: ~p~n", [Store]),
	    node(Id, Predecessor, Successor, Store, Next)
    end.

stabilize(Pred, Id, Successor, Next) ->
    {Skey, Spid} = Successor,
        case Pred of
            nil ->
                Spid ! {notify, {Id, self()}},
                {Successor, Next};
            {Id, _} ->
                {Successor, Next};
            {Skey, Spid} ->
                Spid ! {notify, {Id, self()}},
	            {Successor, Next};
            {Xkey, Xpid} -> % XKey is the precessor of our succesor
                case key:between(Xkey, Id, Skey) of
                true ->
                    Xpid ! {request, self()},
		            Pred;
                false ->
                    Spid ! {notify, {Id, self()}},
		            {Successor, Next}
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

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            Keep = handover(Id, Store, Nkey, Npid),
            {{Nkey, Npid}, Keep};
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                     Keep = handover(Id, Store, Nkey, Npid),
                    {{Nkey, Npid}, Keep};
                false ->
                    {Predecessor, Store}
        end
    end.

connect(Id, nil) ->
    {ok, {Id, self()}}; 
connect(_Id, Peer) ->
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

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Client ! {Qref, ok},
            storage:add(Key, Value, Store);
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            {_, Spid} = Successor,
            Spid ! {lookup, Key, Qref, Client}
    end.

handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.

monitor(Pid) ->
    io:format("Started to monitor a node ~w~n", [Pid]),
    erlang:monitor(process, Pid).

drop(nil) ->
    ok;
drop(Pid) ->
    erlang:demonitor(Pid, [flush]).

down(Ref, {Key, Ref, _}, Successor, Next) ->
    io:format("Predecessor with key ~w died. ~n", [Key]),
    {nil, Successor, Next};
down(Ref, Predecessor, {Key, Ref, _}, {Nkey, Npid}) ->
    Nref = monitor(Npid),
    self() ! stabilize,
    io:format("Successor with key ~w died. ~n", [Key]),
    {Predecessor, {Nkey, Nref, Npid}, nil}.

test1() ->
    Peer1 = test:start(node3),
    timer:sleep(2000),
    test:start(node3, 4, Peer1),
    timer:sleep(8000),
    Peer1 ! probe,
    timer:sleep(1000),
    Keys = test:keys(1000),
    timer:sleep(1000),
    test:add(Keys, Peer1),
    timer:sleep(5000),
    Peer1 ! status,
    test:check(Keys, Peer1),
    timer:sleep(5000),
    PeerExtra = start(nodeExtra, Peer1),
    timer:sleep(8000),
    Peer1 ! probe,
    PeerExtra ! stop,
    timer:sleep(8000),
    Peer1 ! probe,
    Peer1.

test2() ->
    Peer1 = test:start(node3),
    timer:sleep(4000),
    Peer1 ! probe,
    timer:sleep(1000),
    Keys = test:keys(5000),
    timer:sleep(3000),
    test:add(Keys, Peer1),
    timer:sleep(5000),
    Peer1 ! status,
    test:check(Keys, Peer1),
    timer:sleep(5000),
    Peer1.