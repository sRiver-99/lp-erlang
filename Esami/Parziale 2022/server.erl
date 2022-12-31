-module(server).
-export([start/3]).

start(ServerName, MM1Name, MM2Name) ->
    register(ServerName, self()),
    group_leader(whereis(user), self()),
    io:format("~nI'm the Server!~n", []),
    loop(MM1Name, MM2Name, [], [], 0, 0, -1, null, null).

loop(MM1Name, MM2Name, Acc1, Acc2, Received1, Received2, Total, Parity1, Parity2) ->
    receive
        {do_rev, MM1Name, {Index, Element}} ->
            io:format("Received element #~p from ~p: ~p~n", [Index, MM1Name, Element]),
            case (Parity1 /= null) and (Parity2 /= null) and (Received1 + 1 == Total) and (Received2 == Total) of
                false -> loop(MM1Name, MM2Name, [{Index, Element}|Acc1], Acc2, Received1 + 1, Received2, Total, Parity1, Parity2);
                true ->
                    print_list(MM1Name, MM2Name, [{Index, Element}|Acc1], Acc2, Parity1, Parity2)
            end;
        {do_rev, MM2Name, {Index, Element}} ->
            io:format("Received element #~p from ~p: ~p~n", [Index, MM2Name, Element]),
            case (Parity1 /= null) and (Parity2 /= null) and (Received1 == Total) and (Received2 + 1 == Total) of
                false -> loop(MM1Name, MM2Name, Acc1, [{Index, Element}|Acc2], Received1, Received2 + 1, Total, Parity1, Parity2);
                true ->
                    print_list(MM1Name, MM2Name, Acc1, [{Index, Element}|Acc2], Parity1, Parity2)
            end;
        {do_rev, MM1Name, finish, Parity, Length} ->
            case (Parity2 /= null) and (Received1 == Total) and (Received2 == Total) of
                false -> loop(MM1Name, MM2Name, Acc1, Acc2, Received1, Received2, Length, Parity, Parity2);
                true ->
                    print_list(MM1Name, MM2Name, Acc1, Acc2, Parity, Parity2)
            end;
        {do_rev, MM2Name, finish, Parity, Length} ->
            case (Parity1 /= null) and (Received1 == Total) and (Received2 == Total) of
                false -> loop(MM1Name, MM2Name, Acc1, Acc2, Received1, Received2, Length, Parity1, Parity);
                true ->
                    print_list(MM1Name, MM2Name, Acc1, Acc2, Parity1, Parity)
            end;
        Msg ->
            io:format("Got ~p~n", [Msg]),
            loop(MM1Name, MM2Name, Acc1, Acc2, Received1, Received2, Total, Parity1, Parity2)
    end.

print_list(MM1Name, MM2Name, Sublist1, Sublist2, Parity1, Parity2) ->
    case Parity1 == Parity2 of
        false -> io:format("Something went wrong!~n");
        true ->
            case Parity1 of
                even ->
                    {_, List} = lists:unzip(lists:append(lists:keysort(1, Sublist2), lists:keysort(1, Sublist1))),
                    io:format("The reversed list is: ~p~n", [List]);
                odd ->
                    {_, List} = lists:unzip(lists:append(lists:droplast(lists:keysort(1, Sublist2)), lists:keysort(1, Sublist1))),
                    io:format("The reversed list is: ~p~n", [List])
            end
    end,
    loop(MM1Name, MM2Name, [], [], 0, 0, -1, null, null).