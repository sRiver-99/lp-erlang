-module(server).
-export([start/3]).

start(ServerName, MM1Name, MM2Name) ->
    register(ServerName, self()),
    group_leader(whereis(user), self()),
    io:format("~nI'm the Server!~n", []),
    loop(MM1Name, MM2Name, [], [], null, null).

loop(MM1Name, MM2Name, Acc1, Acc2, Parity1, Parity2) ->
    receive
        {do_rev, MM1Name, {Index, Element}} ->
            io:format("Received element #~p from ~p: ~p~n", [Index, MM1Name, Element]),
            loop(MM1Name, MM2Name, [{Index, Element}|Acc1], Acc2, Parity1, Parity2);
        {do_rev, MM2Name, {Index, Element}} ->
            io:format("Received element #~p from ~p: ~p~n", [Index, MM2Name, Element]),
            loop(MM1Name, MM2Name, Acc1, [{Index, Element}|Acc2], Parity1, Parity2);
        {do_rev, MM2Name, finish, Parity} ->
            case Parity1 of
                null -> loop(MM1Name, MM2Name, Acc1, Acc2, null, Parity);
                _ ->
                    print_list(Acc1, Acc2, Parity1, Parity),
                    loop(MM1Name, MM2Name, [], [], null, null)
            end;
        {do_rev, MM1Name, finish, Parity} ->
            case Parity2 of
                null -> loop(MM1Name, MM2Name, Acc1, Acc2, Parity, null);
                _ ->
                    print_list(Acc1, Acc2, Parity, Parity2),
                    loop(MM1Name, MM2Name, [], [], null, null)
            end;
        Msg ->
            io:format("Got ~p~n", [Msg]),
            loop(MM1Name, MM2Name, Acc1, Acc2, Parity1, Parity2)
    end.

print_list(Sublist1, Sublist2, Parity1, Parity2) ->
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
    end.