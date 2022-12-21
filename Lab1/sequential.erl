-module(sequential).
-export([is_palindrome/1, test_is_palindrome/0]).
-export([is_an_anagram/2, test_is_an_anagram/0]).
-export([factors/1, test_factors/0]).
-export([is_perfect/1, test_is_perfect/0]).

clean_string(Str) -> lists:filter(fun(Char) -> not lists:member(Char, [32, $,, $;, $., $., $!, $?, $', $"]) end, Str).

is_palindrome(Str) ->
    CleanString = string:casefold(clean_string(Str)),
    string:equal(CleanString, string:reverse(CleanString)).

are_anagrams(Anag, Anag, _) -> true;
are_anagrams(_, _, []) -> false;
are_anagrams(Str, _, [H|T]) -> are_anagrams(Str, lists:sort(H), T).

is_an_anagram(_, []) -> false;
is_an_anagram(Str, [H|T]) -> are_anagrams(lists:sort(Str), lists:sort(H), T).

factors(Num) -> factors(Num, [], thunks:primes()).
factors(1, Acc, _) -> lists:reverse(Acc);
factors(N, Acc, [N,_]) -> lists:reverse([N|Acc]);
factors(N, Acc, [H|_]=P) when N rem H == 0 -> factors(N div H, [H|Acc], P);
factors(N, Acc, [_|T]) -> factors(N, Acc, T()).


divisors(Num, Acc, [Num|_]) -> lists:reverse(Acc);
divisors(Num, Acc, [H|T]) when Num rem H == 0 -> divisors(Num, [H|Acc], T());
divisors(Num, Acc, [_|T]) -> divisors(Num, Acc, T()).
is_perfect(Num) -> lists:foldr(fun(A, B) -> A + B end, 0, divisors(Num, [], thunks:int_from(1))) == Num.

% Test

test_is_palindrome() -> tests:test(fun is_palindrome/1, [{"ciao",false}, {"mondo",false}, {"ciao.oaic",true}]).

test_is_an_anagram() -> tests:test(fun is_an_anagram/2, [{"ciao",["ci","ao"],false}, {"mondo",["mond","dhd","monod","ciao"],true}]).

test_factors() -> tests:test(fun factors/1, [{10,[2,5]}, {11,[11]}, {45,[3,3,5]}, {2310,[2,3,5,7,11]}]).

test_is_perfect() -> tests:test(fun is_perfect/1, [{6,true}, {10,false}, {28,true}, {31,false}, {101,false}, {250,false}, {496,true}, {1453,false}, {4652,false}, {8128,true}, {9999,false}]).