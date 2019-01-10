-module(runner).
-export([start/0, stop/0]).

launchTimeInterval() -> 2.
stopTimeInterval() -> 1.

start() -> 

    % Process Manager

    process_manager:init(),

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


stop() ->

    % Signal emitters

    smoke_sensor:stop(),
    timer:sleep(timer:seconds(stopTimeInterval())),

    temperature_sensor:stop(),
    timer:sleep(timer:seconds(stopTimeInterval())),

    anti_intrusion_sensor:stop(),
    timer:sleep(timer:seconds(stopTimeInterval())),

    % Signal consumers

    alarm:stop(),
    timer:sleep(timer:seconds(stopTimeInterval())),

    air_conditioning:stop(),
    timer:sleep(timer:seconds(stopTimeInterval())),

    fire_sprinkler:stop(),
    timer:sleep(timer:seconds(stopTimeInterval())),

    % Controller

    controller:stop(),

    % Process Manager

    process_manager:destroy().

