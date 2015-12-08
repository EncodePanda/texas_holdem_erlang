-module(util).
-export([number_to_string/1, print/1, shuffleList/1, perms/1]).

perms([]) -> [[]];
perms(L) ->[[H|T]  || H <- L,
		      T <- perms(L -- [H])].

number_to_string(N) ->
    lists:flatten(io_lib:format("~p", [N])).

print(S) ->
    io:format("~ts~n", [S]).

shuffleList(L) ->
    [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- L])].
