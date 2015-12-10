-module(hand).
-export([pickBest/1, compare/2, rank/1, arrange/1]).
-import(lists, [sort/2, nth/2, map/2]).
-import(figures, [f2i/1, i2f/1]).

allHands(Cards) ->
    [T || [_,_|T] <- util:perms(Cards)].

pickBest(Cards) ->
    arrange(nth(1,sort(fun(H1, H2) -> compare(H1,H2) >= 0 end, allHands(Cards)))).

compare(Hand1, Hand2) ->
    compareRanks(rank(Hand1), rank(Hand2)).

compareRanks({royal_flush, _},{royal_flush, _}) -> 0;
compareRanks({royal_flush, _}, _) -> 1; 
compareRanks(_, {royal_flush, _}) -> -1;
compareRanks({flush, F1, _},{flush, F2, _}) -> figures:compare(F1, F2);
compareRanks({flush, _, _}, _) -> 1; 
compareRanks(_, {flush, _, _}) -> -1;
compareRanks({four_of_a_kind, F1},{four_of_a_kind, F2}) -> figures:compare(F1, F2);
compareRanks({four_of_a_kind, _}, _) -> 1;
compareRanks(_, {four_of_a_kind, _}) -> -1;
compareRanks({full_house, F, O1}, {full_house, F, O2}) -> figures:compare(O1, O2);
compareRanks({full_house, F1, _}, {full_house, F2, _}) -> figures:compare(F1, F2);
compareRanks({full_house, _, _}, _) -> 1;
compareRanks(_, {full_house, _, _}) -> -1;
compareRanks({color, _, F1},{color, _, F2}) -> figures:compare(F1, F2);
compareRanks({color, _, _}, _) -> 1;
compareRanks(_, {color, _, _}) -> -1;
compareRanks({straight, F1},{straight, F2}) -> figures:compare(F1, F2);
compareRanks({straight, _}, _) -> 1; 
compareRanks(_, {straight, _}) -> -1;
compareRanks({three_of_a_kind, F1},{three_of_a_kind, F2}) -> figures:compare(F1, F2);
compareRanks({three_of_a_kind, _}, _) -> 1; 
compareRanks(_, {three_of_a_kind, _}) -> -1;
compareRanks({two_pairs, F, O1}, {two_pairs, F, O2}) -> figures:compare(O1, O2);
compareRanks({two_pairs, F1, _}, {two_pairs, F2, _}) -> figures:compare(F1, F2);
compareRanks({two_pairs, _, _}, _) -> 1;
compareRanks(_, {two_pairs, _, _}) -> -1;
compareRanks({pair, F1},{pair, F2}) -> figures:compare(F1, F2);
compareRanks({pair, _}, _) -> 1; 
compareRanks(_, {pair, _}) -> -1;
compareRanks({high_card, {F1,_}}, {high_card, {F2,_}}) -> figures:compare(F1, F2).
    
card_to_value({Figure, Color}) -> {f2i(Figure), Color}.

arrange(Hand) ->
    sort(fun({F1, _},{F2, _}) -> f2i(F1) >= f2i(F2) end, Hand).

rank(Hand) -> 
    rankings(map(fun card_to_value/1,arrange(Hand))).

rankings([{14,C}, {13,C}, {12,C}, {11,C}, {10,C}]) -> 
    {royal_flush, C};
rankings([{N5,C}, {N4,C}, {N3,C}, {N2,C}, {N1,C}]) 
  when N5 == N4 + 1, N4 == N3 + 1, N3 == N2 + 1, N2 == N1 + 1 -> 
    {flush, i2f(N5), C};
rankings([{F,_}, {F,_}, {F,_}, {F,_}, {_,_}]) ->
    {four_of_a_kind, i2f(F)};
rankings([{_,_}, {F,_}, {F,_}, {F,_}, {F,_}]) ->
    {four_of_a_kind, i2f(F)};
rankings([{F,_}, {F,_}, {F,_}, {O,_}, {O,_}]) ->
    {full_house, i2f(F), i2f(O)};
rankings([{O,_}, {O,_}, {F,_}, {F,_}, {F,_}]) ->
    {full_house, i2f(F), i2f(O)};
rankings([{H,C}, {_,C}, {_,C}, {_,C}, {_,C}]) ->
    {color, C, i2f(H)};
rankings([{N5,_}, {N4,_}, {N3,_}, {N2,_}, {N1,_}]) 
  when N5 == N4 + 1, N4 == N3 + 1, N3 == N2 + 1, N2 == N1 + 1 -> 
    {straight, i2f(N5)};
rankings([{F,_}, {F,_}, {F,_}, {_,_}, {_,_}]) ->
    {three_of_a_kind, i2f(F)};
rankings([{F,_}, {F,_}, {O,_}, {O,_}, {_,_}]) ->
    {two_pairs,  i2f(F),  i2f(O)};
rankings([{F,_}, {F,_}, {_,_}, {_,_}, {_,_}]) ->
    {pair,  i2f(F)};
rankings([{F, C}|_]) -> 
    {high_card, {i2f(F), C}}. 
    
