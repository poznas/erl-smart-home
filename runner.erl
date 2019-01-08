-module(runner).
-export([start/0]).

launchTimeInterval() -> 2.

start() -> 

    % Controller

    ControllerPID = spawn(fun () -> controller:start() end),
    io:format("Run [controller] process: ~p~n", [ControllerPID]),
    timer:sleep(timer:seconds(launchTimeInterval())),

    % Signal consumers

    AlarmPID = spawn(fun () -> alarm:start() end),
    io:format("Run [Alarm] process: ~p~n", [AlarmPID]),
    timer:sleep(timer:seconds(launchTimeInterval())),

    AC_PID = spawn(fun () -> air_conditioning:start() end),
    io:format("Run [Air Conditioning] process: ~p~n", [AC_PID]),
    timer:sleep(timer:seconds(launchTimeInterval())),

    SprinklerPID = spawn(fun () -> fire_sprinkler:start() end),
    io:format("Run [Fire Sprinkler] process: ~p~n", [SprinklerPID]),
    timer:sleep(timer:seconds(launchTimeInterval())),

    

    % Signal emitters

    AI_PID = spawn(fun () -> anti_intrusion_sensor:start() end),
    io:format("Run [Anti Intrusion Sensor] process: ~p~n", [AI_PID]),
    timer:sleep(timer:seconds(launchTimeInterval())),

    TempPID = spawn(fun () -> temperature_sensor:start() end),
    io:format("Run [Temperature Sensor] process: ~p~n", [TempPID]),
    timer:sleep(timer:seconds(launchTimeInterval())),

    SmokePID = spawn(fun () -> smoke_sensor:start() end),
    io:format("Run [Smoke Sensor] process: ~p~n", [SmokePID]).