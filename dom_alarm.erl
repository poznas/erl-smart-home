-module(dom_alarm).
-export([start/0, stop/0, loop/0]).

%%%-------------------
%%% dom_alarm symuluje zachowanie czujnika alarmu.
%%% funkcje: start, stop, loop
%%%-------------------

serverAddress() -> {127,0,0,1}.
serverPort() -> 5000.
id() -> alarm.
name() -> dom_alarm.

%%-------------------------
%% Funckja start
%% Rejestruje czujnik na serwerze,
%% uruchamia czujnik alarmu danym porcie.
%%-------------------------

start() ->
    try
        io:format("Uruchamiam czujnik alarmu o Id: ~p...~n", [id()]),
        PId = spawn(fun () -> loop() end),
        ets:new(dom_pids, [set, named_table]),
        ets:insert(dom_pids, {loop, PId}),
        dom_client:register(serverAddress(), serverPort(), id(), name(), 0),
        start
    catch
        _:_ -> io:format("Pojedynczy proces moze obslugiwac tylko jeden czujnik!~n", []),
        blad
    end.

%%-------------------------
%% Funckja stop
%% Zatrzymuje działanie czujnika alarmu.
%%-------------------------

stop() ->
    try
        dom_client:delete(serverAddress(), serverPort(), id()),
        PId = element(2, hd(ets:lookup(dom_pids, loop))),
        exit(PId, stop),
        ets:delete(dom_pids),
        io:format("Zatrzymuje czujnik alarmu o Id ~p...~n", [id()]),
        stop
    catch
        _:_ -> io:format("Brak dzialajacego czujnika na tym procesie!~n"),
        blad
    end.

%%-------------------------
%% Funckja stop
%% Wysyła co 10 sekund do serwera informacje, czy doszło do włamania.
%%-------------------------

loop() ->
    case random:uniform(1) of
        1 ->
            dom_client:data(serverAddress(), serverPort(), id(), tak);
        _ ->
        dom_client:data(serverAddress(), serverPort(), id(), nie)
    end,
    timer:sleep(timer:seconds(10)),
    loop().
