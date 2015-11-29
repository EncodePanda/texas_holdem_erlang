-module(hand).
-export([pickBest/1, compare/2]).
-import(lists, [sort/2, nth/2]).


perms([]) -> [[]];
perms(L) ->[[H|T]  || H <- L,
		      T <- perms(L -- [H])].

allHands(Cards) ->
    [T || [_,_|T] <- perms(Cards)].

pickBest(Cards) ->
    nth(1,sort(compare, allHands(Cards))).

%% thinking how to implement hands compare function...
compare(Hand1,Hand2) ->
    true.

