-module(dom_func).
-compile(export_all).
%%%-----------------------------------------------------------------------------
%%% Przykladowe funkcje reagujace dla zaimplementowanych czujnikow i odbiornikow.
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Funkcja: temp/1
%% Cel: Reaguje na podana wartosc temperatury wlaczajac klimatyzacje w razie potrzeby.
%% Argumenty: Wartosc temperatury.
%%------------------------------------------------------------------------------
temp(nil) -> nil;
temp(Data) when Data > 28 ->
    io:format("Temperature is ~p, Air Conditioning ON~n", [Data]),
    controller:send_to(ac, wlacz);
temp(_)  ->
    io:format("Temperature is fine, Air Conditioning OFF~n"),
    controller:send_to(ac, wylacz).


%%------------------------------------------------------------------------------
%% Funkcja: alarm/1
%% Cel: Reaguje na stan alarmu, wysylajac SMS jesli zostal on aktywowany.
%% Argumenty: Stan alarmu.
%%------------------------------------------------------------------------------
alarm(tak) ->
    io:format("Ktos wlamuje sie do domu, alarm!~n"),
    controller:send_to(sms, "Ktos wlamuje sie do domu, alarm!");
alarm(_) -> nil.

%%------------------------------------------------------------------------------
%% Funkcja: alarm/1
%% Cel: Reaguje na stan czujnika dymu, wysylajac SMS jesli zostal on aktywowany
%%     oraz otwierajac okna.
%% Argumenty: Stan czujnika dymu.
%%------------------------------------------------------------------------------
dym(tak) ->
    io:format("Czujnik wykryl dym!~n"),
    controller:send_to(sms, "Czujnik wykryl dym!"),
    io:format("Otwieram okna...~n"),
    controller:send_to(sprinkler, otworz);
dym(_) ->
    controller:send_to(sprinkler, zamknij).
