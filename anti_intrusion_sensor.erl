-module(anti_intrusion_sensor).
-export([start/0, stop/0]).

%%%%%%%%%%%%%%%%%%%%%%
%% anti_intrusion_sensor simulates behavior of alarm sensor.
%% Functions: start, stop, emit
%%%%%%%%%%%%%%%%%%%%%%

id() -> intrusion.

%%%%%%%%%%%%%%%%%%%%%%
%% Function start
%% Registers alarm sensor on the server,
%% Starts the alarm sensor on the given port.
%%%%%%%%%%%%%%%%%%%%%%

start() ->
    try
        io:format("Starting alarm sensor with Id: ~p...~n", [id()]),
        emitter_utils:register(controller:address(), controller:port(), id(), 0),
        process_manager:register(id(), self()),
        emit()
    catch
        _:_ -> io:format("Single process may handle only one alarm sensor!~n", []),
        error
    end.

%%%%%%%%%%%%%%%%%%%%%%
%% Function stop
%% Stops the alarm sensor.
%%%%%%%%%%%%%%%%%%%%%%

stop() ->
    try
        emitter_utils:unregister(controller:address(), controller:port(), id()),
        io:format("Alarm sensor which Id is ~p is being stopped~n", [id()]),
        process_manager:kill(id())
    catch
        _:_ -> io:format("There are no alarm sensors working on this process!~n"),
        error
    end.

%%%%%%%%%%%%%%%%%%%%%%
%% Function emit
%% Every 10 seconds sends to controller information whether intrusion has occured.
%%%%%%%%%%%%%%%%%%%%%%

emit() ->
    case random:uniform(1) of
        1 ->
            emitter_utils:sendData(controller:address(), controller:port(), id(), yes);
        _ ->
        emitter_utils:sendData(controller:address(), controller:port(), id(), no)
    end,
    timer:sleep(timer:seconds(10)),
    emit().
