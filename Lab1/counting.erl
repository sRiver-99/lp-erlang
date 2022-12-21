-module(counting).
-export([start/0, loop/1, call_service/1, stop/0]).
-record(service, {id, calls = 0}).

start() ->
    case whereis(server) of
        undefined -> register(server, spawn(?MODULE, loop, [maps:new()])), server_started;
        _ -> io:format("The server is already running!~n"), server_already_running
    end.

loop(Map) ->
    receive
        {Client, tot} ->
            NewMap = maps:update_with(tot, fun(Calls) -> Calls + 1 end, 1, Map),
            List = maps:fold(fun(Key, Value, Acc) -> [#service{id = Key, calls = Value}|Acc] end, [], NewMap),
            Client ! lists:reverse(List),
            loop(NewMap);
        {Client, ServiceId} ->
            Client ! response,
            loop(maps:update_with(ServiceId, fun(Calls) -> Calls + 1 end, 1, Map));
        stop -> exit(normal);
        Other ->
            io:format("Unexpected ~p~n", [Other]),
            loop(Map)
    end.

call_service(Id) ->
    server ! {self(), Id},
    receive
        Response -> io:format("Server sent: ~p~n", [Response])
        after 5000 -> exit(timeout)
    end.

stop() ->
    case whereis(server) of
        undefined -> io:format("The server isn't running!~n"), server_not_running;
        Pid -> Pid ! stop, server_stopped
    end.