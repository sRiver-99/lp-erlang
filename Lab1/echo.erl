-module(echo).
-export([start/0, print/1, stop/0]).
-export([loop/0]).
-export([link_client/0]).

start() ->
    case whereis(server) of
        undefined ->
            register(server, spawn(?MODULE, loop, [])),
            io:format("~p: created server ~p and registered as 'server'~n", [self(), whereis(server)]);
        _ -> io:format("The server is already running!~n")
    end, ok.

print(Msg) ->
    case whereis(server) of
        undefined -> io:format("The server isn't running!~n");
        Server -> Server ! {message, Msg}
    end, ok.

stop() ->
    case whereis(server) of
        undefined -> io:format("The server isn't running!~n");
        Server -> Server ! {stop}
    end, ok.

loop() ->
    receive
        {message, Msg} -> io:format("~p: ~p~n", [self(), Msg]), loop();
        {stop} -> io:format("~p: stopping~n", [self()]), exit(stopped)
    end.

link_client() ->
    case whereis(server) of
        undefined -> io:format("The server isn't running!~n");
        Server ->
            Client = spawn(fun() ->
                process_flag(trap_exit, true),
                link(Server),
                receive
                    {'EXIT', Pid, Why} -> io:format("~p: process ~p exited with reason '~p'~n", [self(), Pid, Why]), exit(normal)
                end
            end),
            io:format("~p: created client ~p and linked to the server!~n", [self(), Client])
    end, ok.