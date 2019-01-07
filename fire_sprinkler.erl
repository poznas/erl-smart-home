-module(fire_sprinkler).
-export([start/0, stop/0, loop/0]).

%%%-------------------
%%% dom_okno symuluje zachowanie kontrolera wysyłania sms.
%%% funkcje: start, stop, loop
%%%-------------------

port() -> 8089.
id() -> sprinkler.

%%-------------------------
%% Funckja start
%% Rejestruje kontroler na serwerze,
%% uruchamia kontroler danym porcie.
%%-------------------------


start() ->
    try
        io:format("Launching fire sprinkler: ~p...~n", [id()]),
        dom_client:register(controller:address(), controller:port(), id(), port()),
        loop()
    catch
        _:_ -> io:format("Pojedynczy proces moze obslugiwac tylko jeden czujnik!~n", []),
        blad
    end.

%%-------------------------
%% Funckja stop
%% Zatrzymuje działanie kontrolera.
%%-------------------------

stop() ->
    try
        dom_client:delete(controller:address(), controller:port(), id()),
        io:format("Zatrzymuje kontroler kutas o ID ~p...~n", [id()]),
        stop
    catch
        _:_ -> io:format("Brak dzialajacego czujnika na tym procesie!~n"),
        blad
    end.

%%-------------------------
%% Funckja loop
%% Dostaje informacje z informacją, czy otworzyć okna.
%%-------------------------

loop() ->
    case dom_net:read(port()) of
        {_, _, otworz} ->
            io:format("Otwieram okna ~n");
        {_, _, zamknij} ->
            io:format("Zamykam okna ~n");
        _ ->
            nil
    end,
    loop().
