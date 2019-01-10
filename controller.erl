-module(controller).
-export([start/0, stop/0, port/0, address/0, handleTemperature/1, handleSmoke/1, hanleIntrusion/1]).
%%%-----------------------------------------------------------------------------
%%% Glowny serwer aplikacji.
%%% Zajmuje sie wymiana danych pomiedzy klientami oraz wykonywaniem odpowiednich
%%% funkcji w zaleznosci od otrzymanych danych.
%%%-----------------------------------------------------------------------------

port() -> 5000.
address() -> {127,0,0,1}.
id() -> controller.

%%------------------------------------------------------------------------------
%% Funkcja: start/1
%% Cel: Uruchamia serwer na podanym porcie oraz tworzy wszelkie potrzebne magazyny
%%     danych. Jesli podany port jest juz zajety to zwracany jest natychmiast blad.
%% Argumenty: Port - numer portu.
%%------------------------------------------------------------------------------
start() ->
    try
        ets:new(clientSet, [set, named_table, public]),
        ets:new(dataSet, [set, named_table, public]),
        ets:new(signalHandlers, [bag, named_table, public]),
        io:format("Uruchamiam serwer na porcie ~p...~n", [port()]),

        ets:insert(signalHandlers, {temp, fun handleTemperature/1}),
        ets:insert(signalHandlers, {smoke, fun handleSmoke/1}),
        ets:insert(signalHandlers, {intrusion, fun hanleIntrusion/1}),

        process_manager:register(id(), self()),
        listen(port()),
        start
    catch
        A:B -> io:format("Error while running controller process: ~p, ~p~n", [A, B]),
        error
    end.

%%------------------------------------------------------------------------------
%% Funkcja: start/1
%% Cel: Zatrzymuje serwer dzialajacy na podanym porcie oraz usuwa wszelkie magazyny
%%     danych. Jesli nie istnieje serwer na podanym porcie to zwracany jest natychmiast blad.
%% Argumenty: Port - numer portu.
%%------------------------------------------------------------------------------
stop() ->
    try
        ets:delete(clientSet),
        ets:delete(dataSet),
        ets:delete(signalHandlers),
        io:format("Zatrzymano serwer!~n"),
        process_manager:kill(id())
    catch
        _:_ -> io:format("Brak dzialajacego serwera na porcie ~p!~n", [port()]),
        error
    end.

%%------------------------------------------------------------------------------
%% Funkcja: read/1
%% Cel: Odczytuje dane przychodzace do serwera a nastepnie reaguje asynchronicznie
%%     w zaleznosci od otrzymanych danych.
%% Argumenty: Port - numer portu.
%%------------------------------------------------------------------------------
listen(Port) ->
    case consumer_utils:listen(Port) of
        {error, _} ->
            stop();
        {ClientAddress, _, Data} ->
            spawn(fun () -> act(ClientAddress, Data) end),
            listen(Port);
        _ ->
            stop()
    end.

%%------------------------------------------------------------------------------
%% Funkcja: act/3
%% Cel: Reaguje na otrzymane dane w zaleznosci od ich formatu.
%% Argumenty: Adres klienta, krotka z danymi.
%%------------------------------------------------------------------------------
act(ClientAddress, {register, Id, ClientPort}) ->
    io:format("register o id ~p.~n", [Id]),
    ets:insert(clientSet, {Id, ClientAddress, ClientPort}),
    io:format("Rejestruje klienta o id ~p~n", [Id]);
act(_, {data, Id, Data}) ->
    ets:insert(dataSet, {Id, Data}),
    io:format("Otrzymalem dane od ID ~p: ~p~n", [Id, Data]),
    handleSignal(Id);
act(_, {delete, Id}) ->
    try
        ets:delete(clientSet, Id),
        io:format("Usuwam klienta o id ~p.~n", [Id])
    catch
        error:badarg -> io:format("Brak dzialajacego klienta o id ~p!~n", [Id])
    end.


%%------------------------------------------------------------------------------
%% Funkcja: retrieveData/1
%% Cel: Zwraca dane, ktore przyszly od clienta z podanym ID.
%% Argumenty: ID klienta.
%% Zwraca: Zapisane dane lub nil, jesli nie odebrano danych od klienta o podanym ID.
%%------------------------------------------------------------------------------
retrieveData(Id) ->
    case ets:lookup(dataSet, Id) of
        [] -> nil;
        [{Id, Data}] -> Data
    end.

%%------------------------------------------------------------------------------
%% Funkcja: forwardSignal/2
%% Cel: Wysyla podane dane do klienta o podanym ID.
%% Argumenty: ID klienta, dane do wyslania.
%%------------------------------------------------------------------------------
forwardSignal(Id, Data) ->
    case ets:lookup(clientSet, Id) of
        [] -> nil;
        [{Id, ClientAddress, ClientPort}] ->
            emitter_utils:send(ClientAddress, ClientPort, Data)
    end.

%%------------------------------------------------------------------------------
%% Funkcja: exec_func/1
%% Cel: Wywoluje wszystkie funkcje reagujace na otrzymanie danych od klienta
%%     o podanym ID.
%% Argumenty: ID klienta
%%------------------------------------------------------------------------------
handleSignal(Id) ->
    case ets:lookup(signalHandlers, Id) of
        [] -> nil;
        Funcs ->
                lists:map(fun ({_, Func}) -> Func(retrieveData(Id)) end, Funcs)
    end.


%%------------------------------------------------------------------------------
%% Funkcja: temp/1
%% Cel: Reaguje na podana wartosc temperatury wlaczajac klimatyzacje w razie potrzeby.
%% Argumenty: Wartosc temperatury.
%%------------------------------------------------------------------------------
handleTemperature(nil) -> nil;
handleTemperature(Data) when Data > 28 ->
    io:format("Temperature is ~p, Air Conditioning ON~n", [Data]),
    forwardSignal(ac, on);
handleTemperature(_)  ->
    io:format("Temperature is fine, Air Conditioning OFF~n"),
    forwardSignal(ac, off).


%%------------------------------------------------------------------------------
%% Funkcja: alarm/1
%% Cel: Reaguje na stan alarmu, wysylajac SMS jesli zostal on aktywowany.
%% Argumenty: Stan alarmu.
%%------------------------------------------------------------------------------
hanleIntrusion(yes) ->
    io:format("Ktos wlamuje sie do domu, alarm!~n"),
    forwardSignal(alarm, "Ktos wlamuje sie do domu, alarm!");
hanleIntrusion(_) -> nil.

%%------------------------------------------------------------------------------
%% Funkcja: alarm/1
%% Cel: Reaguje na stan czujnika dymu, wysylajac SMS jesli zostal on aktywowany
%%     oraz otwierajac okna.
%% Argumenty: Stan czujnika dymu.
%%------------------------------------------------------------------------------
handleSmoke(yes) ->
    io:format("Czujnik wykryl dym!~n"),
    forwardSignal(alarm, "Czujnik wykryl dym!"),
    io:format("Otwieram okna...~n"),
    forwardSignal(sprinkler, on);
handleSmoke(_) ->
    forwardSignal(sprinkler, off).
