-module(mm).
-export([start/2]).

start(MMName, Server) ->
    register(MMName, self()),
    group_leader(whereis(user), self()),
    io:format("~nI'm the middle man ~p~n", [node()]),
    loop(Server, MMName, [], 0, -1, null).

loop(Server, MMName, Acc, Received, Total, Parity) ->
    receive
        {do_rev, {Index, Element}} ->
            io:format("Received element #~p: ~p~n", [Index, Element]),
            case Received + 1 == Total of
                false -> loop(Server, MMName, [{Index, Element}|Acc], Received + 1, Total, Parity);
                true ->
                    send(Server, MMName, [{Index, Element}|Acc], Parity, Total + 1),
                    loop(Server, MMName, [], 0, -1, null)
            end;
        {do_rev, finish, MsgParity, Length} ->
            case Received == Length of
                false -> loop(Server, MMName, Acc, Received, Length, MsgParity);
                true ->
                    send(Server, MMName, Acc, MsgParity, Length + 1),
                    loop(Server, MMName, [], 0, -1, null)
            end;
        Other -> io:format("Unexpected message: ~p~n", [Other])
    end,
    loop(Server, MMName, Acc, Received, Total, Parity).

send(Server, MMName, [], Parity, Length) -> Server ! {do_rev, MMName, finish, Parity, Length - 1};
send(Server, MMName, [{Index, Element}|T], Parity, Length) ->
    Server ! {do_rev, MMName, {Length - Index, Element}},
    send(Server, MMName, T, Parity, Length).