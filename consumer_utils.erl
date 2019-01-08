-module(consumer_utils).
-export([listen/1]).
%%%-----------------------------------------------------------------------------
%%% Funkcje pomocnicze do wygodnej komunikacji przez protokol UDP.
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Funkcja: read/2
%% Cel: Odczytuje przychodzace dane na protokole UDP i podanym porcie czekajac
%%     podana ilosc milisekund.
%% Argumenty: Port - numer portu, Timeout - ilosc milisekund czekania.
%% Zwraca: Krotka w formacie {SenderAddress, SenderPort, Data} lub {}
%%     jesli wystapil blad lub nie odebrano danych.
%%------------------------------------------------------------------------------
listen(Port, Timeout) ->
    case gen_udp:open(Port, [binary, {active, false}]) of
        {ok, Socket} ->
            Return = listenSocket(Socket, Timeout);
        {error, eaddrinuse} ->
            io:format("Port ~p jest już zajęty przez inny proces!~n", [Port]),
            Return = {error, eaddrinuse};
        {error, Reason} ->
            Return = {error, Reason}
    end,
    Return.

%%------------------------------------------------------------------------------
%% Funkcja: read/1
%% Cel: Odczytuje przychodzace dane na protokole UDP i podanym porcie czekajac
%%     1000 milisekund.
%% Argumenty: Port - numer portu.
%% Zwraca: Krotka w formacie {SenderAddress, SenderPort, Data} lub {}
%%     jesli wystapil blad lub nie odebrano danych.
%%------------------------------------------------------------------------------
listen(Port) ->
    listen(Port, 100000).

%%------------------------------------------------------------------------------
%% Function: recv/2
%% Purpose: Reads UDP data from given Socket with given Timeout.
%% Args: Connection Socket and Timeout in miliseconds.
%% Returns: Touple in format {SenderAddress, SenderPort, Data} or {error, Reason}
%%     if errror occured or {}.
%%------------------------------------------------------------------------------
listenSocket(Socket, Timeout) ->
    case gen_udp:recv(Socket, 0, Timeout) of
        {ok, {Address, Port, Packet}} ->
            Return = {Address, Port, binary_to_term(Packet)};
        {error, Reason} ->
            io:format("Error while listening to ~p: ~p~n", [Socket, Reason]),
            Return = {error, Reason}
    end,
    gen_tcp:close(Socket),
    Return.
