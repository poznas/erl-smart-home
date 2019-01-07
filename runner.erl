-module(runner).
-export([start/0]).

start() -> 
    ControllerPID = spawn(fun () -> controller:start() end),
    io:format("Run controller: ~p~n", [ControllerPID]),
    timer:sleep(timer:seconds(3)),
    LedPID = spawn(fun () -> dom_sms:start() end),
    io:format("Run led: ~p~n", [LedPID]),
    timer:sleep(timer:seconds(2)),
    AlarmPID = spawn(fun () -> dom_alarm:start() end),
    io:format("Run alarm: ~p~n", [AlarmPID]).