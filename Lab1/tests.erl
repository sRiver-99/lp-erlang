-module(tests).
-export([test/2]).

check_passed(Num, Args, ExpectedResult, ExpectedResult) -> io:format("Test ~w PASSED: ~p -> ~p~n", [Num, Args, ExpectedResult]), 1;
check_passed(Num, Args, Result, Expected) -> io:format("Test ~w FAILED: expected ~p -> ~p but was ~p~n", [Num, Args, Expected, Result]), 0.

do_tests(_, [], Pass, Tot) -> io:format("Total passed: ~p/~p (~.1f%)~n", [Pass, Tot - 1, (Pass / (Tot - 1)) * 100]);
do_tests(Function, [{Arg, Expected}|T], Pass, Tot) -> Result = Function(Arg), do_tests(Function, T, Pass + check_passed(Tot, Arg, Result, Expected), Tot + 1);
do_tests(Function, [{Arg1, Arg2, Expected}|T], Pass, Tot) -> Result = Function(Arg1, Arg2), do_tests(Function, T, Pass + check_passed(Tot, [Arg1, Arg2], Result, Expected), Tot + 1).

test(Function, Args) -> do_tests(Function, Args, 0, 1).