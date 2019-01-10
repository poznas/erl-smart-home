-module(air_conditioning).
-export([start/0, stop/0]).

%%%-------------------
%%% dom_ac symuluje zachowanie kontrolera klimatyzacji
%%% funkcje: start, stop, loop
%%%
%%%
%%%-------------------

port() -> 8081.
id() -> ac.


%%-------------------------
%% funckja start
%% rejestruje kontroler na serwerze,
%% uruchamia kontroler klimatyzacji na danym porcie
%%-------------------------
start() ->
    try
        io:format("Uruchamiam kontroler klimatyzacji o id: ~p...~n", [id()]),
        emitter_utils:register(controller:address(), controller:port(), id(), port()),
        process_manager:register(id(), self()),
        listen(),
        start
    catch
        _:_ -> io:format("Pojedynczy proces moze obslugiwac tylko jeden czujnik!~n", []),
        error
    end.

  %%-------------------------
  %% funckja stop
  %% zatrzymuje działanie kontrolera klimatyzacji
  %%-------------------------
stop() ->
    try
        emitter_utils:unregister(controller:address(), controller:port(), id()),
        io:format("Zatrzymuje kontroler klimatyzacji o ID ~p...~n", [id()]),
        process_manager:kill(id())
    catch
        _:_ -> io:format("Brak dzialajacego czujnika na tym procesie!~n"),
        error
    end.

  %%-------------------------
  %% funckja loop
  %% dostaje informacje czy włączyć czy wyłączyć klimatyzację
  %%-------------------------

listen() ->
    case consumer_utils:listen(port()) of
        {_, _, on} ->
            io:format("Wlaczam klimatyzacje ~n");
        {_, _, off} ->
            io:format("Wylaczam klimatyzacje ~n");
        _ ->
            nil
    end,
    listen().
