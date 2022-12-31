-module(client).
-export([start/0, close/0, do_reverse/1, create/0]).

start() -> start(whereis(client)).
start(undefined) -> register(client, spawn(?MODULE, create, []));
start(_) -> "Client is already running!".

close() -> close(whereis(client)).
close(undefined) -> "Client isn't running!";
close(_) -> client ! {stop}, stopping.

do_reverse(List) -> do_reverse(whereis(client), List).
do_reverse(undefined, _) -> "Client isn't running!";
do_reverse(_, List) -> client ! {do_reverse, List}, ok.

create() ->
    %Cambiare i dati dei middle man e del server qui per cambiarli ovunque
    MM1Name = mm1, MM1Host = 'mm1@simone-virtualbox',
    MM2Name = mm2, MM2Host = 'mm2@simone-virtualbox',
    ServerName = server, ServerHost = 'server@simone-virtualbox',
    spawn_link(MM1Host, mm, start, [MM1Name, {ServerName, ServerHost}]),
    spawn_link(MM2Host, mm, start, [MM2Name, {ServerName, ServerHost}]),
    spawn_link(ServerHost, server, start, [ServerName, MM1Name, MM2Name]),
    io:format("Server name ~p~nConnected nodes :- ~p~n", [net_adm:localhost(), nodes()]),
    loop({MM1Name, MM1Host}, {MM2Name, MM2Host}).

loop(MM1, MM2) ->
    receive
        {stop} -> exit(stopped);
        {do_reverse, List} ->
            {Sublist1, Sublist2} = lists:split(round(length(List) / 2), List),
            Length = length(Sublist1),
            case length(List) rem 2 of
                0 -> send(even, MM1, lists:enumerate(Sublist1), MM2, lists:enumerate(Sublist2), Length);
                1 -> send(odd, MM1, lists:enumerate(Sublist1), MM2, lists:enumerate([lists:last(Sublist1)|Sublist2]), Length)
            end;
        Other -> io:format("Unexpected message: ~p~n", [Other])
    end,
    loop(MM1, MM2).

send(Parity, MM1, [], MM2, [], Length) ->
    MM1 ! {do_rev, finish, Parity, Length},
    MM2 ! {do_rev, finish, Parity, Length};
send(Parity, MM1, [H1|T1], MM2, [H2|T2], Length) ->
    MM1 ! {do_rev, H1},
    MM2 ! {do_rev, H2},
    send(Parity, MM1, T1, MM2, T2, Length).