-module(mm).
-export([start/2]).

start(MMName, Server) ->
    register(MMName, self()),
    group_leader(whereis(user), self()),
    io:format("~nI'm the middle man ~p~n", [node()]),
    loop(Server, MMName, []).

loop(Server, MMName, Acc) ->
    receive
        {do_rev, {Index, Element}} ->
            io:format("Received element #~p: ~p~n", [Index, Element]),
            loop(Server, MMName, [{Index, Element}|Acc]);
        {do_rev, finish, Parity} ->
            send(Server, MMName, Acc, Parity, length(Acc) + 1),
            loop(Server, MMName, []);
        Other -> io:format("Unexpected message: ~p~n", [Other])
    end,
    loop(Server, MMName, Acc).

send(Server, MMName, [], Parity, _) -> Server ! {do_rev, MMName, finish, Parity};
send(Server, MMName, [{Index, Element}|T], Parity, Length) ->
    Server ! {do_rev, MMName, {Length - Index, Element}},
    send(Server, MMName, T, Parity, Length).