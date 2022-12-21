-module(thunks).
-export([first_primes/1]).

int_from(Start) -> [Start|fun() -> int_from(Start + 1) end].

filter(_, []) -> [];
filter(Predicate, [H|T]) ->
    case Predicate(H) of
        true -> [H|fun() -> filter(Predicate, T()) end];
        false -> filter(Predicate, T())
    end.

sift(P, L) -> filter(fun(N) -> N rem P /= 0 end, L).

sieve([H|T]) -> [H|fun() -> sieve(sift(H, T())) end].

primes() -> sieve(int_from(2)).

first(0, _) -> [];
first(N, [H|T]) -> [H|first(N-1, T())].

first_primes(N) -> first(N, primes()).