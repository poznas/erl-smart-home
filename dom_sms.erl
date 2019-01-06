-module(dom_sms).
-export([start/0, stop/0, loop/0]).

%%%-------------------
%%% dom_sms symuluje zachowanie kontrolera wysyłania sms.
%%% funkcje: start, stop, loop
%%%-------------------

port() -> 8084.

serverAddress() -> {127,0,0,1}.
serverPort() -> 5000.
id() -> sms.
name() -> sms.

%%-------------------------
%% Funckja start
%% Rejestruje kontroler na serwerze,
%% uruchamia kontroler danym porcie.
%%-------------------------

start() ->
    try
        io:format("Uruchamiam kontroler sms o id: ~p...~n", [id()]),
        dom_client:register(serverAddress(), serverPort(), id(), name(), port()),
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
        dom_client:delete(serverAddress(), serverPort(), id()),
        PID = element(2, hd(ets:lookup(dom_pids, loop))),
        exit(PID, stop),
        ets:delete(dom_pids),
        io:format("Zatrzymuje kontroler sms o ID ~p...~n", [id()]),
        stop
    catch
        _:_ -> io:format("Brak dzialajacego czujnika na tym procesie!~n"),
        blad
    end.

%%-------------------------
%% Funckja loop
%% Dostaje informacje aby wysłać sms o podanej treści.
%%-------------------------

loop() ->
    case dom_net:read(port()) of
        {_, _, Tresc} ->
            io:format("Wysylam sms: ~p ~n", [Tresc]);
        _ ->
            nil
    end,
    loop().
