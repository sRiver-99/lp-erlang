-module(reverse).
-export([start/0, stop/0]).
-export([master_loop/0, slave_reverse_string/2]).
-export([long_reverse_string/1, long_reverse_string_custom/2]).

start() ->
    case whereis(master) of
        undefined ->
            Result = register(master, spawn(?MODULE, master_loop, [])),
            io:format("Created MASTER ~p and registered as 'master'~n", [whereis(master)]),
            Result;
        _ -> io:format("The MASTER is already running!~n")
    end.

stop() ->
    case whereis(master) of
        undefined -> io:format("The MASTER isn't running!~n");
        MasterPid -> MasterPid ! {stop}
    end, ok.

master_loop() ->
    receive
        {stop} -> io:format("MASTER ~p: stopping~n", [self()]), exit(normal);
        {reverse_string, Str, NumberOfSlaves, Client} ->
            start_slaves(Str, string:length(Str), NumberOfSlaves, NumberOfSlaves, string:length(Str) rem NumberOfSlaves, 0),
            Client ! {reverse_string, lists:foldl(fun({_, Elem}, Acc) -> string:concat(Acc, Elem) end, "", lists:reverse(lists:ukeysort(1, wait_responses(NumberOfSlaves, [])))), self()};
        Other -> io:format("MASTER ~p: unexpected message '~p'~n", [self(), Other])
    end,
    master_loop().

start_slaves(_, _, _, 0, _, _) -> ok;
start_slaves(Str, StrLength, NumberOfSlaves, RemainingSlaves, Separator, CurrentStart) ->
    CurrentSlave = NumberOfSlaves - RemainingSlaves + 1,
    case CurrentSlave =< Separator of
        true ->
            Aumento = trunc(StrLength / NumberOfSlaves) + 1;
        false ->
            Aumento = trunc(StrLength / NumberOfSlaves)
    end,
    spawn(?MODULE, slave_reverse_string, [CurrentSlave, string:slice(Str, CurrentStart, Aumento)]),
    start_slaves(Str, StrLength, NumberOfSlaves, RemainingSlaves - 1, Separator, CurrentStart + Aumento).

slave_reverse_string(SlaveNumber, Str) ->
    master ! {reverse_string_done, SlaveNumber, lists:reverse(Str)},
    exit(normal).

wait_responses(0, Responses) -> Responses;
wait_responses(RemainingResponses, Responses) ->
    receive
        {reverse_string_done, SlaveNumber, ReversedString} ->
            io:format("MASTER ~p: received ~p from SLAVE ~p~n", [self(), ReversedString, SlaveNumber]),
            wait_responses(RemainingResponses - 1, [{SlaveNumber, ReversedString}|Responses])
        after 20000 -> exit(timeout)
    end.

long_reverse_string(Str) ->
    case whereis(master) of
        undefined -> io:format("The MASTER isn't running!~n");
        MasterPid ->
            MasterPid ! {reverse_string, Str, 10, self()},
            receive
                {reverse_string, Response, MasterPid} -> io:format("The reversed string is: ~p~n", [Response])
            end
    end, ok.

long_reverse_string_custom(Str, NumberOfSlaves) ->
    case whereis(master) of
        undefined -> io:format("The MASTER isn't running!~n");
        MasterPid ->
            case NumberOfSlaves < 1 of
                true -> MasterPid ! {reverse_string, Str, 1, self()};
                false ->
                    case string:length(Str) < NumberOfSlaves of
                        true -> MasterPid ! {reverse_string, Str, string:length(Str), self()};
                        false -> MasterPid ! {reverse_string, Str, NumberOfSlaves, self()}
                    end
            end,
            receive
                {reverse_string, Response, MasterPid} -> io:format("The reversed string is: ~p~n", [Response])
            end
    end, ok.