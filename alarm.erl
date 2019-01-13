-module(alarm).
-export([start/0, stop/0]).

%%%%%%%%%%%%%%%%%%%%%%
%% alarm simulates behavior of controller that's sending SMS messages.
%% Functions: start, stop, listen
%%%%%%%%%%%%%%%%%%%%%%

port() -> 8084.
id() -> alarm.

%%%%%%%%%%%%%%%%%%%%%%
%% Function: start
%% Registers SMS controller on the server,
%% Starts the SMS controller on the given port.
%%%%%%%%%%%%%%%%%%%%%%

start() ->
    try
        io:format("Starting SMS controller with Id: ~p...~n", [id()]),
        emitter_utils:register(controller:address(), controller:port(), id(), port()),
        Wx=wx:new(),
        Frame=wxFrame:new(Wx, -1, "Alarm Frame"),
        %wxFrame:show(Frame),
        listen(Frame),
        process_manager:register(id(), self()),
        start
    catch
        _:_ -> io:format("Single process may handle only one SMS controller!~n", []),
        error
    end.

%%%%%%%%%%%%%%%%%%%%%%
%% Function: stop
%% Stops the SMS controller.
%%%%%%%%%%%%%%%%%%%%%%

stop() ->
    try
        emitter_utils:unregister(controller:address(), controller:port(), id()),
        io:format("SMS controller which Id is ~p is being stopped ~n", [id()]),
        wxFrame:destroy(),
        process_manager:kill(id())
    catch
        _:_ -> io:format("There are no SMS controllers working on this process!~n"),
        error
    end.

%%%%%%%%%%%%%%%%%%%%%%
%% Function: listen
%% Waits for information to send SMS with given text.
%%%%%%%%%%%%%%%%%%%%%%

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
