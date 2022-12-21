-module(controller).
-export([start/1, controller_loop/1]).

start(NumSieves) ->
    PrimesList = thunks:first_primes(NumSieves),
    register(first, spawn(sieve, spawn_sieves, [NumSieves, PrimesList])),
    link(whereis(first)),
    [Max|_] = lists:reverse(PrimesList),
    register(controller, self()),
    controller_loop(Max * Max).

controller_loop(Max) ->
    receive
        {new, N} when is_integer(N) ->
            Num = round(math:sqrt(N)),
            io:format("You asked for: ~p (~p)~n", [N, round(math:sqrt(N))]),
            case Num > Max of
                false -> first ! {new, Num};
                true -> {client, 'client@simone-virtualbox'} ! {result, uncheckable}
            end,
            controller_loop(Max);
        {res, R} ->
            {client, 'client@simone-virtualbox'} ! {result, R},
            controller_loop(Max);
        {quit} -> io:format("I'm closing ...~n"), exit(normal)
    end.