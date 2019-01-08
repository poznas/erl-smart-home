-module(alarm).
-export([start/0, stop/0]).

%%%-------------------
%%% dom_sms symuluje zachowanie kontrolera wysyłania sms.
%%% funkcje: start, stop, loop
%%%-------------------

port() -> 8084.
id() -> alarm.

%%-------------------------
%% Funckja start
%% Rejestruje kontroler na serwerze,
%% uruchamia kontroler danym porcie.
%%-------------------------

start() ->
    try
        io:format("Uruchamiam kontroler sms o id: ~p...~n", [id()]),
        emitter_utils:register(controller:address(), controller:port(), id(), port()),
        listen(),
        start
    catch
        _:_ -> io:format("Pojedynczy proces moze obslugiwac tylko jeden czujnik!~n", []),
        error
    end.

%%-------------------------
%% Funckja stop
%% Zatrzymuje działanie kontrolera.
%%-------------------------

stop() ->
    try
        emitter_utils:unregister(controller:address(), controller:port(), id()),
        io:format("Zatrzymuje kontroler sms o ID ~p...~n", [id()]),
        exit(self(), normal)
    catch
        _:_ -> io:format("Brak dzialajacego czujnika na tym procesie!~n"),
        error
    end.

%%-------------------------
%% Funckja loop
%% Dostaje informacje aby wysłać sms o podanej treści.
%%------------------------- TODO: blink LED Rpi

listen() ->
    case consumer_utils:listen(port()) of
        {_, _, Tresc} ->
            io:format("Wysylam sms: ~p ~n", [Tresc]);
        _ ->
            nil
    end,
    listen().
