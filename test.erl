-module(test).
-export([test/0, royal_flush/1, flush/2, four_kind/1, color/1, full_house/2]).
-import(figures, [f2i/1, i2f/1]).

repeat(F, N) ->
    lists:sublist(lists:map(fun(C) -> {F,C} end, colors:list()),N).

sequence(S,E) ->
    SeqOfNum = lists:seq(f2i(S), f2i(E)),
    lists:map(fun(N) -> i2f(N) end, SeqOfNum).

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

two_pairs(F1, F2) ->
    deck:shuffle(repeat(F1, 2) ++ repeat(F2, 2) ++ [{2,heart}]).

pair(F) ->
    deck:shuffle(repeat(F, 2) ++ [{2,heart}, {3, spades}, {4, dimonds}]).

test() ->
    %% colors & figures
    [dimonds, heart, club, spades] = colors:list(),
    [2,3,4,5,6,7,8,9,jack,queen,king,ace] = figures:list(),
    %% create Deck and few Hands
    Deck = deck:shuffle(deck:create()),
    HeartRoyalFlush = royal_flush(heart),
    HeartFlush = flush(10, heart),
    SpadesFlush = flush(9, spades),
    FourOfAKindAce = four_kind(ace),
    %% rank hands
    {royal_flush, heart} = hand:rank(HeartRoyalFlush),
    {flush, 10, heart} = hand:rank(HeartFlush),
    {four_of_a_kind, ace} = hand:rank(FourOfAKindAce),
    {full_house, ace, king} = hand:rank(full_house(ace, king)),
    {color, club, _} = hand:rank(color(club)),
    {straight, 8} = hand:rank(straight(8)),
    {three_of_a_kind, ace} = hand:rank(three_kind(ace)),
    {two_pairs, ace, king} = hand:rank(two_pairs(ace, king)),
    {pair, king} = hand:rank(pair(king)),
    %% compare hands
    0 = hand:compare(HeartRoyalFlush, HeartRoyalFlush),
    -1 = hand:compare(HeartFlush, HeartRoyalFlush),
    1 = hand:compare(HeartRoyalFlush, HeartFlush),
    1 = hand:compare(HeartFlush, SpadesFlush),
    -1 = hand:compare(SpadesFlush, HeartFlush),
    1 = hand:compare(HeartFlush, FourOfAKindAce),
    -1 = hand:compare(FourOfAKindAce, HeartFlush),   
    %% print cards & hands
    "|A♣|" = deck:show({ace, club}),
    {test_worked}.
    
