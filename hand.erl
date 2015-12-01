-module(hand).
-export([pickBest/1, compare/2, rank/1]).
-import(lists, [sort/2, nth/2, map/2]).


perms([]) -> [[]];
perms(L) ->[[H|T]  || H <- L,
		      T <- perms(L -- [H])].

allHands(Cards) ->
    [T || [_,_|T] <- perms(Cards)].

pickBest(Cards) ->
    nth(1,sort(compare/2, allHands(Cards))).

compare(Hand1,Hand2) ->
    true.

figure_to_int(jack) -> 11;
figure_to_int(queen) -> 12;
figure_to_int(king) -> 13;
figure_to_int(ace) -> 14;
figure_to_int(N) -> N.
    
rank_card({Figure, Color}) -> {figure_to_int(Figure), Color}.

arrange(RHand) ->
    sort(fun({N1, _},{N2, _}) -> N1 >= N2 end, RHand).

rank(Hand) -> 
    rankings(arrange(map(fun rank_card/1, Hand))).

rankings([{14,C}, {13,C}, {12,C}, {11,C}, {10,C}]) -> 
    {royal_flush, C};
rankings([{N5,C}, {N4,C}, {N3,C}, {N2,C}, {N1,C}]) 
  when N5 == N4 + 1, N4 == N3 + 1, N3 == N2 + 1, N2 == N1 + 1 -> 
    {flush, N5, C};
rankings([{F,_}, {F,_}, {F,_}, {F,_}, {O,C}]) ->
    {four_of_a_kind, F, {O,C}};
rankings([{O,C}, {F,_}, {F,_}, {F,_}, {F,_}]) ->
    {four_of_a_kind, F, {O,C}};
rankings([{F,_}, {F,_}, {F,_}, {O,_}, {O,_}]) ->
    {full_house, F, O};
rankings([{O,_}, {O,_}, {F,_}, {F,_}, {F,_}]) ->
    {full_house, F, O};
rankings([{V, C}|_]) -> {high_card, V, C}. 
    
