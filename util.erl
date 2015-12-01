-module(util).
-export([number_to_string/1, print/1]).

number_to_string(N) ->
    lists:flatten(io_lib:format("~p", [N])).

print(S) ->
    io:format("~ts~n", [S]).
