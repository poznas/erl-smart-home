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
        Wx=wx:new(),
        Frame=wxFrame:new(Wx, -1, "Alarm Frame"),
        %wxFrame:show(Frame),
        listen(Frame),
        process_manager:register(id(), self()),
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
        wxFrame:destroy(),
        process_manager:kill(id())
    catch
        _:_ -> io:format("Brak dzialajacego czujnika na tym procesie!~n"),
        error
    end.

%%-------------------------
%% Funckja loop
%% Dostaje informacje aby wysłać sms o podanej treści.

listen(Frame) ->
    case consumer_utils:listen(port()) of
        {_, _, Message} ->
            io:format("Show alarm dialog: ~p ~n", [Message]),
            D = wxMessageDialog:new (Frame, "ALARM: " ++ Message),
            wxMessageDialog:showModal (D);
        _ ->
            nil
    end,
    listen(Frame).
