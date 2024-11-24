-module(task1).
-export([start/0]).

% --------------------MAIN FUNCTION--------------------
start() ->
    io:format("Number of belts: "),
    {ok, [NBelts]} = io:fread("", "~d"),
    io:format("Truck capacity: "),
    {ok, [TruckCapacity]} = io:fread("", "~d"),

    ProducerPid = spawn(fun producer/0),
    put(producer_pid, ProducerPid),

    timer:sleep(500),
    lists:foreach(fun(Id) ->
        TruckPid = spawn(fun() -> truck(Id, TruckCapacity) end),
        put({truck, Id}, TruckPid),
        BeltPid = spawn(fun() -> belt(Id, TruckPid, TruckCapacity, ProducerPid) end),
        put({belt, Id}, BeltPid)
    end, lists:seq(1, NBelts)),

    receive
        _ -> ok
    end.
% -----------------------------------------------------



% --------------------CODE FOR THE PRODUCER--------------------
producer() ->
    io:format("Producer is starting...~n"),
    producer_loop([]).

producer_loop(Packages) ->
    receive
            {produce, Size} ->
                NewPackages = Packages ++ [Size],
                io:format("PRODUCER: Added package with size ~p.~n", [Size]),
                producer_loop(NewPackages);

            {request_package, Pid} ->
                case Packages of
                    [] ->
                        io:format("PRODUCER: No packages available. Notifying belt...~n"),
                        Pid ! {no_package},
                        producer_loop(Packages);
                    [Package | Rest] ->
                        io:format("PRODUCER: Sending package with size ~p to belt.~n", [Package]),
                        Pid ! {package, Package},
                        producer_loop(Rest)
                end
    
        after 0 ->
            self() ! {produce, 2},
            producer_loop(Packages)
    end.
% ----------------------------------------------------------



% --------------------CODE FOR THE BELTS--------------------
belt(BeltId, TruckPid, TruckCapacity, ProducerPid) ->
    io:format("CONVEYOR BELT: Belt ~p started working...~n", [BeltId]),
    belt_loop(BeltId, TruckPid, TruckCapacity, ProducerPid).

belt_loop(BeltId, TruckPid, TruckCapacity, ProducerPid) ->
    io:format("CONVEYOR BELT: Belt ~p requesting a package...~n", [BeltId]),
    ProducerPid ! {request_package, self()},
    receive
        {package, Package} ->
            io:format("CONVEYOR BELT: Belt ~p received package with size ~p.~n", [BeltId, Package]),
            TruckPid ! {add_package, Package},
            belt_loop(BeltId, TruckPid, TruckCapacity, ProducerPid);         
        {no_package} ->
            io:format("CONVEYOR BELT: Belt ~p DID NOT receive a package.~n", [BeltId]),
            timer:sleep(1000),
            belt_loop(BeltId, TruckPid, TruckCapacity, ProducerPid)
    end.
% ----------------------------------------------------------



% --------------------CODE FOR THE TRUCKS--------------------
truck(TruckId, TruckCapacity) ->
    io:format("TRUCK: Truck ~p started waiting for packages...~n", [TruckId]),
    truck_loop(TruckId, TruckCapacity, 0).

truck_loop(TruckId, TruckCapacity, CurrentLoad) ->
    receive
        {add_package, Package} ->
            TotalLoad = CurrentLoad + Package,
            if
                TotalLoad > TruckCapacity ->
                    io:format("TRUCK: Truck ~p is overloaded. Swapping trucks...~n", [TruckId]),
                    NewTruckId = TruckId + 10,
                    self() ! {add_package, Package},
                    truck_loop(NewTruckId, TruckCapacity, 0);
                TotalLoad =:= TruckCapacity ->
                    io:format("TRUCK: Truck ~p is full. Swapping trucks...~n", [TruckId]),
                    NewTruckId = TruckId + 10,
                    truck_loop(NewTruckId, TruckCapacity, 0);
                TotalLoad < TruckCapacity ->
                    io:format("TRUCK: Added package with size ~p to truck ~p. Current load: ~p.~n", [Package, TruckId, TotalLoad]),
                    truck_loop(TruckId, TruckCapacity, TotalLoad)
            end;
        _ ->
            io:format("TRUCK: Truck ~p waiting for packages...~n", [TruckId]),
            truck_loop(TruckId, TruckCapacity, CurrentLoad)
    end.
% -----------------------------------------------------------