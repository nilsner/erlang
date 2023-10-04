-module(node2).
-export([node/4, start/1, start/2, connect/2]).
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
    node(Id, Predecessor, Successor, storage:create()).

node(Id, Predecessor, Successor, Store) ->
    receive
        {key, Qref, Peer} -> % peer needs to know our key
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store);
        {notify, New} -> % new node informs us of its existence
            Pred = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, Store);
        {request, Peer} -> % a predecessor needs to know our predecessor
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Store);
        {status, Pred} -> % our successor informs us about its predecessor
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ, Store);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store);
        {add, Key, Value, Qref, Client} ->
            io:format("Key: ~w~n", [Key]),
            io:format("Value: ~w~n", [Value]),
            io:format("Qref: ~w~n", [Qref]),
            io:format("Client: ~w~n", [Client]),
            io:format("Id: ~w~n", [Id]),
            Added = add(Key, Value, Qref, Client,
            Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store);
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged)
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
    case storeage:between(Key, Pkey, Id) of
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