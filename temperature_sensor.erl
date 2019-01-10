-module(temperature_sensor).
-export([start/0, stop/0]).

%%%-------------------
%%% dom_temp symuluje zachowanie czujnika temperatury
%%% funkcje: start, stop, loop
%%%-------------------

id() -> temp.

%%-------------------------
%% funckja start
%% Rejestruje czujnik na serwerze,
%% uruchamia czujnik temperatury na danym porcie.
%%-------------------------
start() ->
    try
        io:format("Uruchamiam czujnik temperatury o Id: ~p...~n", [id()]),
        emitter_utils:register(controller:address(), controller:port(), id(), 0),
        process_manager:register(id(), self()),
        emit()
    catch
        _:_ -> io:format("Pojedynczy proces moze obslugiwac tylko jeden czujnik!~n", []),
        error
    end.

%%-------------------------
%% Funckja stop
%% Zatrzymuje działanie czujnika temperatury.
%%-------------------------
stop() ->
    try
        emitter_utils:unregister(controller:address(), controller:port(), id()),
        io:format("Zatrzymuje czujnik alarmu o Id ~p...~n", [id()]),
        process_manager:kill(id())
    catch
        _:_ -> io:format("Brak dzialajacego czujnika na tym procesie!~n"),
        error
    end.

%%-------------------------
%% Funckja loop
%% Wysyła co 5 sekund do serwera informacje o temperaturze.
%%-------------------------

emit() ->
    emitter_utils:sendData(controller:address(), controller:port(), id(), random:uniform(40)),
    timer:sleep(timer:seconds(5)),
    emit().
