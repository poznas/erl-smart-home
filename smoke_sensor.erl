-module(smoke_sensor).
-export([start/0, stop/0]).

%%%-------------------
%%% dom_dym symuluje zachowanie czujnika dymu.
%%% funkcje: start, stop, loop
%%%-------------------

id() -> smoke.

%%-------------------------
%% Funckja start
%% Rejestruje czujnik na serwerze,
%% uruchamia czujnik dymu na danym porcie.
%%-------------------------
start() ->
    try
        io:format("Uruchamiam czujnik dymu o Id: ~p...~n", [id()]),
        emitter_utils:register(controller:address(), controller:port(), id(), 0),
        process_manager:register(id(), self()),
        emit()
    catch
        _:_ -> io:format("Pojedynczy proces moze obslugiwac tylko jeden czujnik!~n", []),
        error
    end.

%%-------------------------
%% Funckja stop
%% Zatrzymuje działanie czujnika dymu.
%%-------------------------

stop() ->
    try
        emitter_utils:unregister(controller:address(), controller:port(), id()),
        io:format("Zatrzymuje kontroler dym kurwa chuj o ID ~p...~n", [id()]),
        process_manager:kill(id())
    catch
        _:_ -> io:format("Brak dzialajacego czujnika na tym procesie!~n"),
        error
    end.

%%-------------------------
%% Funckja loop
%% Wysyła co 10 sekund do serwera informacje o obecności dymu.
%%-------------------------

emit() ->
    case random:uniform(2) of
        1 ->
        emitter_utils:sendData(controller:address(), controller:port(), id(), yes);
        _ ->
        emitter_utils:sendData(controller:address(), controller:port(), id(), no)
    end,
    timer:sleep(timer:seconds(10)),
    emit().
