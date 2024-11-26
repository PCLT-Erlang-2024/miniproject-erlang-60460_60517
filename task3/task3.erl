-module(task3).
-export([start/0]).

% --------------------MAIN FUNCTION--------------------
start() ->
    io:format("Number of belts: "),
    {ok, [NBelts]} = io:fread("", "~d"),
    io:format("Truck capacity: "),
    {ok, [TruckCapacity]} = io:fread("", "~d"),

    ProducerPid = spawn(fun() -> producer(TruckCapacity) end),
    put(producer_pid, ProducerPid),

    timer:sleep(500),
    lists:foreach(fun(Id) ->
        TruckPid = spawn(fun() -> truck(Id, TruckCapacity, undefined) end),
        put({truck, Id}, TruckPid),
        BeltPid = spawn(fun() -> belt(Id, TruckPid, TruckCapacity, ProducerPid) end),
        put({belt, Id}, BeltPid),
        TruckPid ! {set_belt_pid, BeltPid}
    end, lists:seq(1, NBelts)),

    receive
        _ -> ok
    end.
% -----------------------------------------------------



% --------------------CODE FOR THE PRODUCER--------------------
producer(TruckCapacity) ->
    io:format("Producer is starting...~n"),
    producer_loop(TruckCapacity, []).

producer_loop(TruckCapacity, Packages) ->
    receive
            {produce, Size} ->
                NewPackages = Packages ++ [Size],
                io:format("PRODUCER: Added package with size ~p.~n", [Size]),
                producer_loop(TruckCapacity, NewPackages);

            {request_package, Pid} ->
                case Packages of
                    [] ->
                        io:format("PRODUCER: No packages available. Notifying belt...~n"),
                        Pid ! {no_package},
                        producer_loop(TruckCapacity, Packages);
                    [Package | Rest] ->
                        io:format("PRODUCER: Sending package with size ~p to belt.~n", [Package]),
                        Pid ! {package, Package},
                        producer_loop(TruckCapacity, Rest)
                end
    
        after 0 ->
            self() ! {produce, rand:uniform(TruckCapacity)},
            timer:sleep(100),
            producer_loop(TruckCapacity, Packages)
    end.
% ----------------------------------------------------------



% --------------------CODE FOR THE BELTS--------------------
belt(BeltId, TruckPid, TruckCapacity, ProducerPid) ->
    io:format("CONVEYOR BELT: Belt ~p started working...~n", [BeltId]),
    belt_loop(BeltId, TruckPid, TruckCapacity, ProducerPid).

belt_work(BeltId, TruckPid, TruckCapacity, ProducerPid) ->
    receive
        {available} ->
            io:format("CONVEYOR BELT: Belt ~p WORKING...~n", [BeltId]),
            belt_loop(BeltId, TruckPid, TruckCapacity, ProducerPid)        
    end.

belt_loop(BeltId, TruckPid, TruckCapacity, ProducerPid) ->
    io:format("CONVEYOR BELT: Belt ~p requesting a package...~n", [BeltId]),
    ProducerPid ! {request_package, self()},
    receive
        {package, Package} ->
            io:format("CONVEYOR BELT: Belt ~p received package with size ~p.~n", [BeltId, Package]),
            TruckPid ! {add_package, Package},
            timer:sleep(300),
            belt_loop(BeltId, TruckPid, TruckCapacity, ProducerPid);         
        {no_package} ->
            io:format("CONVEYOR BELT: Belt ~p DID NOT receive a package.~n", [BeltId]),
            timer:sleep(1000),
            belt_loop(BeltId, TruckPid, TruckCapacity, ProducerPid);
        {not_available} ->
            io:format("CONVEYOR BELT: Belt ~p STOP.~n", [BeltId]),
            belt_work(BeltId, TruckPid, TruckCapacity, ProducerPid)
    end.

% ----------------------------------------------------------



% --------------------CODE FOR THE TRUCKS--------------------
truck(TruckId, TruckCapacity, BeltPid) ->
    io:format("TRUCK: Truck ~p started waiting for packages...~n", [TruckId]),
    truck_loop(TruckId, TruckCapacity, 0, BeltPid).

truck_loop(TruckId, TruckCapacity, CurrentLoad, BeltPid) ->
    receive
        {set_belt_pid, NewBeltPid} ->
            io:format("TRUCK: Truck ~p updated BeltPid: ~p~n", [TruckId, NewBeltPid]),
            truck_loop(TruckId, TruckCapacity, CurrentLoad, NewBeltPid);
        {add_package, Package} ->
            TotalLoad = CurrentLoad + Package,
            if
                TotalLoad > TruckCapacity ->
                    io:format("TRUCK: Truck ~p is overloaded because current load is ~p and received package with size ~p. Swapping trucks...~n", [TruckId, CurrentLoad, Package]),
                    BeltPid ! {not_available},
                    timer:sleep(rand:uniform(2500)+2500),
                    BeltPid ! {available},
                    NewTruckId = TruckId + 10,
                    self() ! {add_package, Package},
                    truck_loop(NewTruckId, TruckCapacity, 0, BeltPid);
                TotalLoad =:= TruckCapacity ->
                    io:format("TRUCK: Truck ~p is full. Swapping trucks...~n", [TruckId]),
                    BeltPid ! {not_available},
                   timer:sleep(rand:uniform(2500)+2500),
                    BeltPid ! {available},
                    NewTruckId = TruckId + 10,
                    truck_loop(NewTruckId, TruckCapacity, 0, BeltPid);
                TotalLoad < TruckCapacity ->
                    io:format("TRUCK: Added package with size ~p to truck ~p. Current load: ~p.~n", [Package, TruckId, TotalLoad]),
                    timer:sleep(300),
                    truck_loop(TruckId, TruckCapacity, TotalLoad, BeltPid)
            end;
        _ ->
            io:format("TRUCK: Truck ~p waiting for packages...~n", [TruckId]),
            truck_loop(TruckId, TruckCapacity, CurrentLoad, BeltPid)
    end.
% -----------------------------------------------------------