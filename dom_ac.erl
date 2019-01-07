-module(dom_ac).
-export([start/0, stop/0, loop/0]).

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
        dom_client:register(controller:address(), controller:port(), id(), port()),
        loop()
    catch
        _:_ -> io:format("Pojedynczy proces moze obslugiwac tylko jeden czujnik!~n", []),
        blad
    end.

  %%-------------------------
  %% funckja stop
  %% zatrzymuje działanie kontrolera klimatyzacji
  %%-------------------------
stop() ->
    try
        dom_client:delete(controller:address(), controller:port(), id()),
        PID = element(2, hd(ets:lookup(dom_pids, loop))),
        exit(PID, stop),
        ets:delete(dom_pids),
        io:format("Zatrzymuje kontroler klimatyzacji o ID ~p...~n", [id()]),
        stop
    catch
        _:_ -> io:format("Brak dzialajacego czujnika na tym procesie!~n"),
        blad
    end.

  %%-------------------------
  %% funckja loop
  %% dostaje informacje czy włączyć czy wyłączyć klimatyzację
  %%-------------------------

loop() ->
    case dom_net:read(port()) of
        {_, _, wlacz} ->
            io:format("Wlaczam klimatyzacje ~n");
        {_, _, wylacz} ->
            io:format("Wylaczam klimatyzacje ~n");
        _ ->
            nil
    end,
    loop().
