-module(smoke_sensor).
-export([start/0, stop/0, loop/0]).

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
        dom_client:register(controller:address(), controller:port(), id(), 0),
        loop()
    catch
        _:_ -> io:format("Pojedynczy proces moze obslugiwac tylko jeden czujnik!~n", []),
        blad
    end.

%%-------------------------
%% Funckja stop
%% Zatrzymuje działanie czujnika dymu.
%%-------------------------

stop() ->
    try
        dom_client:delete(controller:address(), controller:port(), id()),
        PId = element(2, hd(ets:lookup(dom_pids, loop))),
        exit(PId, stop),
        ets:delete(dom_pids),
        io:format("Zatrzymuje czujnik dymu o Id ~p...~n", [id()]),
        stop
    catch
        _:_ -> io:format("Brak dzialajacego czujnika na tym procesie!~n"),
        blad
    end.

%%-------------------------
%% Funckja loop
%% Wysyła co 10 sekund do serwera informacje o obecności dymu.
%%-------------------------

    loop() ->
         %-- random mdz 0 i 1
        case random:uniform(2) of
            1 ->
            dom_client:data(controller:address(), controller:port(), id(), tak);
            _ ->
            dom_client:data(controller:address(), controller:port(), id(), nie)
        end,
        timer:sleep(timer:seconds(10)),
        loop().
