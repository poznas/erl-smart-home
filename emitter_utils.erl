-module(emitter_utils).
-compile(export_all).
%%%-----------------------------------------------------------------------------
%%% Funkcje pomocnicze realizujace podstawowe interakcje klienta z serwerem
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Funkcja: register/5
%% Cel: Rejestruje klienta na serwerze o podanym adresie i porcie. Klient jest
%%     rejestrowany z podanym ID, nazwa i portem odbiorczym.
%% Argumenty: Adres i port serwera, ID, nazwa i port odbiorczy klienta.
%%------------------------------------------------------------------------------
register(ServerAddress, ServerPort, Id, ClientPort) -> 
    send(ServerAddress, ServerPort, Id, {register, Id, ClientPort}).

%%------------------------------------------------------------------------------
%% Funkcja: data/4
%% Cel: Wysyla dane na podany adres i port serwera od klienta o podanym ID.
%% Argumenty: Adres i port serwera, ID i dane klienta.
%%------------------------------------------------------------------------------
sendData(ServerAddress, ServerPort, Id, Data) -> 
    send(ServerAddress, ServerPort, Id, {data, Id, Data}).

%%------------------------------------------------------------------------------
%% Funkcja: delete/3
%% Cel: Usuwa na serwerze o podanym adresie i porcie klienta o podanym ID.
%% Argumenty: Adres i port serwera, ID klienta.
%%------------------------------------------------------------------------------
unregister(ServerAddress, ServerPort, Id) -> 
    send(ServerAddress, ServerPort, Id, {delete, Id}).

send(ServerAddress, ServerPort, OperationId, Payload) -> 
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    io:format("~p : -> ~p:~p [~p]~n", [OperationId, ServerAddress, ServerPort, Payload]),
    gen_udp:send(Socket, ServerAddress, ServerPort, term_to_binary(Payload)).

%%------------------------------------------------------------------------------
%% Funkcja: send/3
%% Cel: Wysyla dane przez protokol UDP na podany adress i port.
%% Argumenty: Adres docelowy jako krotka, port oraz dane do wyslania.
%%------------------------------------------------------------------------------
send(Address, Port, Data) ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    gen_udp:send(Socket, Address, Port, term_to_binary(Data)).

    
