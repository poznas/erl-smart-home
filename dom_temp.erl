-module(dom_temp).
-export([start/0, stop/0, loop/0]).

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
        dom_client:register(controller:address(), controller:port(), id(), 0),
        loop()
    catch
        _:_ -> io:format("Pojedynczy proces moze obslugiwac tylko jeden czujnik!~n", []),
        blad
    end.

%%-------------------------
%% Funckja stop
%% Zatrzymuje działanie czujnika temperatury.
%%-------------------------
stop() ->
    try
        dom_client:delete(controller:address(), controller:port(), id()),
        PId = element(2, hd(ets:lookup(dom_pids, loop))),
        exit(PId, stop),
        ets:delete(dom_pids),
        io:format("Zatrzymuje czujnik alarmu o Id ~p...~n", [id()])
    catch
        _:_ -> io:format("Brak dzialajacego czujnika na tym procesie!~n"),
        blad
    end.

%%-------------------------
%% Funckja loop
%% Wysyła co 5 sekund do serwera informacje o temperaturze.
%%-------------------------

loop() ->
    dom_client:data(controller:address(), controller:port(), id(), random:uniform(40)),
    timer:sleep(timer:seconds(5)),
    loop().
