-module(routy).
-export([start/2, stop/1, init/1]).

start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).
stop(Node) ->
    Node ! stop,
    unregister(Node).

init(Name) ->
    Intf = interface:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = hist:new(Name),
    router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
 receive

    {add, Node, Pid} ->
        Ref = erlang:monitor(process,Pid),
        Intf1 = interface:add(Node, Ref, Pid, Intf),
        io:format("~w: ~w added~n", [Name, Node]),
        router(Name, N, Hist, Intf1, Table, Map);

    {remove, Node} ->
        {ok, Ref} = interface:ref(Node, Intf),
        erlang:demonitor(Ref),
        Intf1 = interface:remove(Node, Intf),
        io:format("~w: ~w removed~n", [Name, Node]),
        router(Name, N, Hist, Intf1, Table, Map);

    {'DOWN', Ref, process, _, _} ->
        {ok, Down} = interface:name(Ref, Intf),
        io:format("~w: exit recived from ~w~n", [Name, Down]),
        Intf1 = interface:remove(Down, Intf),
        router(Name, N, Hist, Intf1, Table, Map);

    {status, From} ->
        From ! {status, {Name, N, Hist, Intf, Table, Map}},
        io:format("Name: ~w\n N: ~w\nHistory: ~w\nInterfaces: ~w\nTable: ~w\nMap: ~w\n", [Name, N, Hist, Intf, Table, Map]),
        router(Name, N, Hist, Intf, Table, Map);

    {links, Node, R, Links} ->
        case hist:update(Node, R, Hist) of
            {new, Hist1} ->
            interface:broadcast({links, Node, R, Links}, Intf),
            Map1 = map:update(Node, Links, Map),
            router(Name, N, Hist1, Intf, Table, Map1);
            old ->
            router(Name, N, Hist, Intf, Table, Map)
        end;

    update ->
        Table1 = dijkstra:table(interface:list(Intf), Map),
        io:format("~w: update ~w~n", [Name, Table1]),
        router(Name, N, Hist, Intf, Table1, Map);

    broadcast ->
        Message = {links, Name, N, interface:list(Intf)},
        io:format("~w: broadcast ~w~n", [Name, Message]),
        interface:broadcast(Message, Intf),
        router(Name, N+1, Hist, Intf, Table, Map);

    {route, Name, From, Message} ->
	    io:format("~w: received message ~w from ~w~n", [Name, Message, From]),
	    router(Name, N, Hist, Intf, Table, Map);

	{route, To, From, Message} ->
        io:format("~w: routing message to ~w~n", [Name, To]),
	    case dijkstra:route(To, Table) of
		{ok, Gw} ->
		    io:format("GW: ~w~n", [Gw]),
		    case interface:lookup(Gw, Intf) of
			{ok, Pid} ->
			    Pid ! {route, To, From, Message};
			notfound ->
			    ok
		    end;
		notfound ->
		    io:format("GW to ~p not found~n", [To]),
		    ok
	    end,
	    router(Name, N, Hist, Intf, Table, Map);

    {send, To, Message} ->
        self() ! {route, To, Name, Message},
        router(Name, N, Hist, Intf, Table, Map);

    stop ->
        ok  
end.