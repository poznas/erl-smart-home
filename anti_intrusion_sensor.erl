-module(anti_intrusion_sensor).
-export([start/0, stop/0]).

%%%-------------------
%%% dom_alarm symuluje zachowanie czujnika alarmu.
%%% funkcje: start, stop, loop
%%%-------------------

id() -> intrusion.

%%-------------------------
%% Funckja start
%% Rejestruje czujnik na serwerze,
%% uruchamia czujnik alarmu danym porcie.
%%-------------------------

start() ->
    try
        io:format("Uruchamiam czujnik alarmu o Id: ~p...~n", [id()]),
        emitter_utils:register(controller:address(), controller:port(), id(), 0),
        emit()
    catch
        _:_ -> io:format("Pojedynczy proces moze obslugiwac tylko jeden czujnik!~n", []),
        error
    end.

%%-------------------------
%% Funckja stop
%% Zatrzymuje działanie czujnika alarmu.
%%-------------------------

stop() ->
    try
        emitter_utils:unregister(controller:address(), controller:port(), id()),
        io:format("Zatrzymuje czujnik alarmu o Id ~p...~n", [id()]),
        exit(self(), normal)
    catch
        _:_ -> io:format("Brak dzialajacego czujnika na tym procesie!~n"),
        error
    end.

%%-------------------------
%% Funckja stop
%% Wysyła co 10 sekund do serwera informacje, czy doszło do włamania.
%%-------------------------

emit() ->
    case random:uniform(1) of
        1 ->
            emitter_utils:sendData(controller:address(), controller:port(), id(), yes);
        _ ->
        emitter_utils:sendData(controller:address(), controller:port(), id(), no)
    end,
    timer:sleep(timer:seconds(10)),
    emit().
