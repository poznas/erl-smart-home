-module(runner).
-export([start/0]).

start() -> 

    % Controller

    ControllerPID = spawn(fun () -> controller:start() end),
    io:format("Run controller: ~p~n", [ControllerPID]),
    timer:sleep(timer:seconds(3)),

    % Signal consumers

    AlarmPID = spawn(fun () -> dom_sms:start() end),
    io:format("Run Alarm: ~p~n", [AlarmPID]),

    AC_PID = spawn(fun () -> dom_ac:start() end),
    io:format("Run Air Conditioning: ~p~n", [AC_PID]),


    timer:sleep(timer:seconds(2)),

    % Signal emitters

    AI_PID = spawn(fun () -> dom_alarm:start() end),
    io:format("Run Anti Intrusion System: ~p~n", [AI_PID]).