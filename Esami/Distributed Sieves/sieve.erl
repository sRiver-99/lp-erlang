-module(sieve).
-export([spawn_sieves/2]).

spawn_sieves(1, [Prime|_]) -> sieve_loop(whereis(first), Prime);
spawn_sieves(NumSieves, [Prime|PrimeList]) ->
    Next = spawn(?MODULE, spawn_sieves, [NumSieves - 1, PrimeList]),
    link(Next),
    sieve_loop(Next, Prime).

sieve_loop(Next, Prime) ->
    receive
        {new, N} -> check(N, Prime, Next);
        {pass, N} -> check(N, Prime, Next);
        {res, R} -> controller ! {res, R}
    end,
    sieve_loop(Next, Prime).

check(Prime, Prime, _) -> first ! {res, true};
check(N, Prime, _) when N rem Prime == 0 -> first ! {res, false};
check(N, _, Next) -> Next ! {pass, N}.