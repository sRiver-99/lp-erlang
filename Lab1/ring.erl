-module(ring).
-export([start_centralized/3, wait_pid/2, loop/2]).
-export([start/3, create/3]).

start_centralized(NumNodes, NumMsg, Msg) ->
    io:format("FATHER ~p started the creation of the ring~n", [self()]),
    register(firstnode, create_centralized(1, NumNodes, self())),
    receive
        ready -> ok;
        {error, Error} -> exit(Error)
        after 5000 -> exit(timeout)
    end,
    io:format("FATHER ~p finished the creation of the ring~n", [self()]),
    msg_dispatcher(self(), 1, NumMsg, Msg),
    firstnode ! stop,
    io:format("FATHER ~p sent {stop} to ~p [1]~n", [self(), whereis(firstnode)]),
    'FATHER_end'.

start(NumNodes, NumMsg, Msg) ->
    io:format("FATHER ~p is starting the creation of the ring~n", [self()]),
    register(firstnode, spawn(?MODULE, create, [NumNodes, 1, self()])),
    io:format("Registered ~p as 'firstnode'~n", [whereis(firstnode)]),
    receive
        ready -> ok;
        {error, Error} -> exit(Error)
        after 5000 -> exit(timeout)
    end,
    io:format("FATHER ~p finished the creation of the ring~n", [self()]),
    msg_dispatcher(self(), 1, NumMsg, Msg),
    firstnode ! stop,
    io:format("FATHER ~p sent {stop} to ~p [1]~n", [self(), whereis(firstnode)]),
    'FATHER_end'.

create_centralized(1, NumNodes, Father) ->
    Next = create_centralized(2, NumNodes, Father),
    Pid = spawn(?MODULE, loop, [Next, 1]),
    io:format("Created ~p as [1] and connected to ~p [2]~n", [Pid, Next]),
    lastnode ! Pid,
    io:format("Sent ~p [1] to ~p [~p]~n", [Pid, whereis(lastnode), NumNodes]),
    Pid;
create_centralized(NumNodes, NumNodes, Father) ->
    Pid = spawn(?MODULE, wait_pid, [NumNodes, Father]),
    register(lastnode, Pid),
    io:format("Created ~p as [~p] and registered as 'lastnode'~n", [Pid, NumNodes]),
    Pid;
create_centralized(N, NumNodes, Father) ->
    Next = create_centralized(N + 1, NumNodes, Father),
    Pid = spawn(?MODULE, loop, [Next, N]),
    io:format("Created ~p as [~p] and connected to ~p [~p]~n", [Pid, N, Next, N + 1]),
    Pid.

create(1, Who, Father) ->
    Father ! ready,
    io:format("Created ~p as [~p] and connected to ~p [1]~n", [self(), Who, whereis(firstnode)]),
    loop_last(whereis(firstnode), Who);
create(N, Who, Father) ->
    Next = spawn(?MODULE, create, [N - 1, Who + 1, Father]),
    io:format("Created ~p as [~p] and connected to ~p [~p]~n", [self(), Who, Next, Who + 1]),
    loop(Next, Who).

wait_pid(Who, Father) ->
    receive
        {error, Error} -> exit(Error);
        Pid ->
            unregister(lastnode),
            io:format("Unregistered ~p [~p] as 'lastnode' and connected to ~p [1]~n", [self(), Who, Pid]),
            Father ! ready,
            loop_last(Pid, Who)
        after 5000 -> exit(timeout)
    end.

loop(Next, Who) ->
    receive
        {Msg, N, Visited, Loops} ->
            io:format("[~p] Got {~p ~p}, this message visited ~s and made ~s~n", [Who, Msg, N, sing_plur(Visited, "node"), sing_plur(Loops, "loop")]),
            io:format("[~p] Is ~p [~p] alive? ~p~n", [Who, Next, Who + 1, erlang:is_process_alive(Next)]),
            Next ! {Msg, N, Visited + 1, Loops},
            io:format("[~p] Sent {~p ~p} to ~p [~p]~n", [Who, Msg, N, Next, Who + 1]),
            loop(Next, Who);
        stop ->
            io:format("[~p] Got {stop}~n", [Who]),
            io:format("[~p] Is ~p alive? ~p~n", [Who, Next, erlang:is_process_alive(Next)]),
            Next ! stop,
            io:format("[~p] Sent {stop} to ~p [~p]~n", [Who, Next, Who + 1]),
            io:format("Actor [~p] stops~n", [Who]);
        Other -> io:format("Unexpected message: ~p", [Other])
    end.

loop_last(Next, Who) ->
    receive
        {Msg, N, Visited, Loops} ->
            io:format("[~p] Got {~p ~p}, this message visited ~p nodes and made ~s~n", [Who, Msg, N, Visited, sing_plur(Loops, "loop")]),
            io:format("[~p] Is ~p [1] alive? ~p~n", [Who, Next, erlang:is_process_alive(Next)]),
            Next ! {Msg, N, Visited + 1, Loops + 1},
            io:format("[~p] Sent {~p ~p} to ~p [1]~n", [Who, Msg, N, Next]),
            loop_last(Next, Who);
        stop ->
            io:format("[~p] Got stop~n", [Who]),
            %io:format("[~p] I should unregister ~p~n", [Who, whereis(firstnode)]),
            %unregister(firstnode),
            io:format("Actor [~p] stops~n", [Who]),
            exit(normal);
        Other -> io:format("Unexprected message: ~p", [Other])
    end.

msg_dispatcher(Pid, M, M, Msg) ->
    firstnode ! {Msg, M, 1, 0},
    io:format("FATHER ~p sent message {~p ~p} to ~p [1]~n", [Pid, Msg, M, whereis(firstnode)]);
msg_dispatcher(Pid, N, M, Msg) ->
    firstnode ! {Msg, N, 1, 0},
    io:format("FATHER ~p sent message {~p ~p} to ~p [1]~n", [Pid, Msg, N, whereis(firstnode)]),
    msg_dispatcher(Pid, N + 1, M, Msg).

sing_plur(1, Word) -> string:concat("1 ", Word);
sing_plur(N, Word) -> string:concat(integer_to_list(N), string:concat(" ", string:concat(Word, "s"))).