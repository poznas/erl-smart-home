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
    io:format("Temperatura wynosi ~p, wlaczam klimatyzacje...~n", [Data]),
    controller:send_to(klim, wlacz);
temp(_)  ->
    controller:send_to(klim, wylacz).


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
    controller:send_to(okno, otworz);
dym(_) ->
    controller:send_to(okno, zamknij).
