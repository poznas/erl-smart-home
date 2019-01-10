-module(fire_sprinkler).
-export([start/0, stop/0]).

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
        emitter_utils:register(controller:address(), controller:port(), id(), port()),
        process_manager:register(id(), self()),
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
        io:format("Zatrzymuje kontroler kutas o ID ~p...~n", [id()]),
        process_manager:kill(id())
    catch
        _:_ -> io:format("Brak dzialajacego czujnika na tym procesie!~n"),
        error
    end.

%%-------------------------
%% Funckja loop
%% Dostaje informacje z informacją, czy otworzyć okna.
%%-------------------------

listen() ->
    case consumer_utils:listen(port()) of
        {_, _, on} ->
            io:format("Otwieram okna ~n");
        {_, _, off} ->
            io:format("Zamykam okna ~n");
        _ ->
            nil
    end,
    listen().
