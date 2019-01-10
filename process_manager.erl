-module(process_manager).
-export([register/2, kill/1, init/0, destroy/0]).

%%%%%%%%%%%%%%%%%%%%%%
% Process manager
% Stores PIDs to kill if needed
%%%%%%%%%%%%%%%%%%%%%%

init() -> ets:new(pids, [set, named_table, public]).

register(Key, PID) -> ets:insert(pids, {Key, PID}).

kill(Key) -> 
    PID = element(2, hd(ets:lookup(pids, Key))),
    exit(PID, stop).

destroy() -> ets:delete(pids).
