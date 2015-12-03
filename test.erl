-module(test).
-export([test/0, royal_flush/1, flush/2, four_kind/1, full_house/2]).

repeat(F, N) ->
    lists:sublist(lists:map(fun(C) -> {F,C} end, deck:colors()),N).

royal_flush(C) ->
    deck:shuffle([{ace,C}, {king,C}, {queen,C}, {jack,C}, {10,C}]).

flush(N, C) ->
    deck:shuffle([{N,C}, {N-1,C}, {N-2,C}, {N-3,C}, {N-4,C}]).

four_kind(F) ->
    deck:shuffle(repeat(F, 4) ++ [{2,heart}]).

full_house(F,O) ->
    deck:shuffle(repeat(F,3) ++ repeat(O, 2)).


test() ->
    [dimonds, heart, club, spades] = deck:colors(),
    [2,3,4,5,6,7,8,9,jack,queen,king,ace] = deck:figures(),
    Deck = deck:shuffle(deck:create()),
    "|A♣|" = deck:show({ace, club}),
    {royal_flush, heart} = hand:rank(royal_flush(heart)),
    {flush, 10, heart} = hand:rank(flush(10, heart)),
    {four_of_a_kind, 10} = hand:rank(four_kind(10)),
    {full_house, ace, king} = hand:rank(full_house(ace, king)),
    {test_worked}.
    
