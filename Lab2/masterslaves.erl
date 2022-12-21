-module(masterslaves).
-export([start/1, to_slave/2]).
-export([create_slaves/3, slave_loop/1]).

start(NumberOfSlaves) ->
    Result = register(master, spawn(?MODULE, create_slaves, [NumberOfSlaves, 1, []])),
    io:format("MAIN ~p: created MASTER ~p and registered as 'master'~n", [self(), whereis(master)]),
    Result.

create_slaves(0, _, SlavesList) ->
    process_flag(trap_exit, true),
    FinalList = lists:reverse(SlavesList),
    io:format("MASTER ~p: my list of slaves is: ~p~n", [self(), FinalList]),
    master_loop(FinalList);
create_slaves(SlavesToBeCreated, SlaveNumber, SlavesList) ->
    SlavePid = spawn(?MODULE, slave_loop, [SlaveNumber]),
    link(SlavePid),
    io:format("MASTER ~p: created SLAVE ~p [~p] and linked to it~n", [self(), SlavePid, SlaveNumber]),
    create_slaves(SlavesToBeCreated - 1, SlaveNumber + 1, [{SlaveNumber, SlavePid}|SlavesList]).

master_loop(SlavesList) ->
    receive
        {'EXIT', SlavePid, Cause} ->
            {SlaveNumber, _} = lists:keyfind(SlavePid, 2, SlavesList),
            io:format("MASTER ~p: detected the death of SLAVE ~p [~p] with cause '~p'~n", [self(), SlavePid, SlaveNumber, Cause]),
            NewSlavePid = spawn(?MODULE, slave_loop, [SlaveNumber]),
            link(NewSlavePid),
            io:format("MASTER ~p: created SLAVE ~p [~p] and linked to it~n", [self(), NewSlavePid, SlaveNumber]),
            NewSlavesList = lists:keyreplace(SlaveNumber, 1, SlavesList, {SlaveNumber, NewSlavePid}),
            io:format("MASTER ~p: my list of slaves is: ~p~n", [self(), NewSlavesList]),
            master_loop(NewSlavesList);
        {Msg, SlaveNumber} ->
            {_, SlavePid} = lists:keyfind(SlaveNumber, 1, SlavesList),
            SlavePid ! {Msg};
        Other -> io:format("MASTER ~p: unexpected message: ~p~n", [self(), Other])
    end,
    master_loop(SlavesList).

slave_loop(SlaveNumber) ->
    receive
        {die} -> exit(normal);
        {Msg} -> io:format("SLAVE ~p [~p]: got message '~p'~n", [self(), SlaveNumber, Msg]);
        Other -> io:format("SLAVE ~p [~p]: unexpected message: ~p~n", [self(), SlaveNumber, Other])
    end,
    slave_loop(SlaveNumber).

to_slave(Msg, SlaveNumber) -> master ! {Msg, SlaveNumber}.