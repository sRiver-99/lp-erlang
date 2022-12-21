-module(client).
-export([is_prime/1, close/0]).

is_prime(N) ->
    register(client, self()),
    {controller, 'server@simone-virtualbox'} ! {new, N},
    receive
        {result, uncheckable} ->
            unregister(client),
            lists:flatten(io_lib:format("~p is uncheckable, too big value.", [N]));
        {result, R} ->
            unregister(client),
            lists:flatten(io_lib:format("is ~p prime? ~p", [N, R]));
        Other -> io:format("Unexpected response: ~p~n", Other)
    end.

close() ->
    {controller, 'server@simone-virtualbox'} ! {quit},
    "The service is closed!!!".