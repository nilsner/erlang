-module(test).

-compile(export_all).

-define(Timeout, 1000).


myStart() ->
    Peer1 = test:start(node2), %% Test for node2
    timer:sleep(4000),
    test:start(node2, Peer1),
    timer:sleep(4000),
    test:start(node2, Peer1),
    % timer:sleep(4000),
    % test:start(node2, Peer1),
    % timer:sleep(4000),
    % test:start(node2, Peer1),
    % timer:sleep(4000),
    % test:start(node2, Peer1),
    Peer1 ! probe,
    timer:sleep(1000),
    Keys = test:keys(15000),
    timer:sleep(3000),
    test:add(Keys, Peer1),
    %test:lookup(hd(Keys), Peer1),
    timer:sleep(1000),
    Peer1 ! status,
    test:check(Keys, Peer1),
    timer:sleep(1000),
    Peer1.
    % Peer1 = test:start(node1), %% Test for node1
    % timer:sleep(4000),
    % test:start(node1, Peer1),
    % timer:sleep(4000),
    % test:start(node1, Peer1),
    % timer:sleep(4000),
    % test:start(node1, Peer1),
    % timer:sleep(4000),
    % % test:start(node2, Peer1),
    % % timer:sleep(4000),
    % % test:start(node2, Peer1),
    % % timer:sleep(4000),
    % % test:start(node2, Peer1),
    % Peer1 ! probe,
    % Peer1 ! status,
    % Peer1.

start(Module) ->
    Id = key:generate(), 
    apply(Module, start, [Id]).


start(Module, P) ->
    Id = key:generate(), 
    apply(Module, start, [Id,P]).    

start(_, 0, _) ->
    ok;
start(Module, N, P) ->
    start(Module, P),
    start(Module, N-1, P).

%% The functions add and lookup can be used to test if a DHT works.

add(Key, Value , P) ->
    Q = make_ref(),
    P ! {add, Key, Value, Q, self()},
    receive 
	{Q, ok} ->
	   ok
	after ?Timeout ->
	    {error, "timeout"}
    end.

lookup(Key, Node) ->
    Q = make_ref(),
    Node ! {lookup, Key, Q, self()},
    receive 
	{Q, Value} ->
	    Value
    after ?Timeout ->
	    {error, "timeout"}
    end.


%% This benchmark can be used for a DHT where we can add and lookup
%% key. In order to use it you need to implement a store.

keys(N) ->
    lists:map(fun(_) -> key:generate() end, lists:seq(1,N)).

add(Keys, P) ->
    lists:foreach(fun(K) -> add(K, banan, P) end, Keys).

check(Keys, P) ->
    T1 = now(),
    {Failed, Timeout} = check(Keys, P, 0, 0),
    T2 = now(),
    Done = (timer:now_diff(T2, T1) div 1000),
    io:format("~w lookup operation in ~w ms ~n", [length(Keys), Done]),
    io:format("~w lookups failed, ~w caused a timeout ~n", [Failed, Timeout]).


check([], _, Failed, Timeout) ->
    {Failed, Timeout};
check([Key|Keys], P, Failed, Timeout) ->
    case lookup(Key,P) of
	{Key, _} -> 
	    check(Keys, P, Failed, Timeout);
	{error, _} -> 
	    check(Keys, P, Failed, Timeout+1);
	false ->
	    check(Keys, P, Failed+1, Timeout)
    end.