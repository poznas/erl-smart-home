-module(dom_alarm).
-export([start/0, stop/0, loop/0]).

%%%-------------------
%%% dom_alarm symuluje zachowanie czujnika alarmu.
%%% funkcje: start, stop, loop
%%%-------------------

id() -> alarm.
name() -> alarm.

%%-------------------------
%% Funckja start
%% Rejestruje czujnik na serwerze,
%% uruchamia czujnik alarmu danym porcie.
%%-------------------------

start() ->
    try
        io:format("Uruchamiam czujnik alarmu o Id: ~p...~n", [id()]),
        dom_client:register(controller:address(), controller:port(), id(), name(), 0),
        loop()
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
        dom_client:delete(controller:address(), controller:port(), id()),
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
            dom_client:data(controller:address(), controller:port(), id(), tak);
        _ ->
        dom_client:data(controller:address(), controller:port(), id(), nie)
    end,
    timer:sleep(timer:seconds(10)),
    loop().
