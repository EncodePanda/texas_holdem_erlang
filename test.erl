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

color(C) ->
    Deck = deck:shuffle(deck:create()),
    OneColor = lists:filter(fun({_, CC}) -> CC == C end, Deck),
    lists:sublist(OneColor, 5).

straight(N5) ->
    deck:shuffle([{N5,heart}, {N5-1,club}, {N5-2,spades}, {N5-3,dimonds}, {N5-4,heart}]).

three_kind(F) ->
    deck:shuffle(repeat(F, 3) ++ [{2,heart}, {3, heart}]).

test() ->
    [dimonds, heart, club, spades] = deck:colors(),
    [2,3,4,5,6,7,8,9,jack,queen,king,ace] = deck:figures(),
    Deck = deck:shuffle(deck:create()),
    "|A♣|" = deck:show({ace, club}),
    {royal_flush, heart} = hand:rank(royal_flush(heart)),
    {flush, 10, heart} = hand:rank(flush(10, heart)),
    {four_of_a_kind, 10} = hand:rank(four_kind(10)),
    {full_house, ace, king} = hand:rank(full_house(ace, king)),
    {color, club, _} = hand:rank(color(club)),
    {straight, 8} = hand:rank(straight(8)),
    {three_of_a_kind, ace} = hand:rank(three_kind(ace)),
    {test_worked}.
    
