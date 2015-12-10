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
    FourOfAKindKing = four_kind(king),
    FullHouseAceKing = full_house(ace, king),
    ColorClub = color(club),
    Straight8 = straight(8),
    Straight7 = straight(7),
    ThreeKindAce = three_kind(ace),
    ThreeKindKing = three_kind(king),
    TwoPairAceKing = two_pairs(ace, king),
    TwoPairKing10 = two_pairs(king, 10),
    PairKing = pair(king),
    PairQueen = pair(queen),
    %% rank hands
    {royal_flush, heart} = hand:rank(HeartRoyalFlush),
    {flush, 10, heart} = hand:rank(HeartFlush),
    {four_of_a_kind, ace} = hand:rank(FourOfAKindAce),
    {full_house, ace, king} = hand:rank(FullHouseAceKing),
    {color, club, _} = hand:rank(ColorClub),
    {straight, 8} = hand:rank(Straight8),
    {three_of_a_kind, ace} = hand:rank(ThreeKindAce),
    {two_pairs, ace, king} = hand:rank(TwoPairAceKing),
    {pair, king} = hand:rank(PairKing),
    %% compare hands
    0 = hand:compare(HeartRoyalFlush, HeartRoyalFlush),
    -1 = hand:compare(HeartFlush, HeartRoyalFlush),
    1 = hand:compare(HeartRoyalFlush, HeartFlush),
    1 = hand:compare(HeartFlush, SpadesFlush),
    -1 = hand:compare(SpadesFlush, HeartFlush),
    1 = hand:compare(HeartFlush, FourOfAKindAce),
    -1 = hand:compare(FourOfAKindAce, HeartFlush),   
    1 = hand:compare(FourOfAKindAce, FourOfAKindKing),
    -1 = hand:compare(FourOfAKindKing, FourOfAKindAce),
    1 = hand:compare(FourOfAKindAce, FullHouseAceKing),
    -1 = hand:compare(FullHouseAceKing, FourOfAKindAce),
    1 = hand:compare(ColorClub, Straight8),
    -1 = hand:compare(Straight8, ColorClub),
    1 = hand:compare(Straight8, Straight7),
    -1 = hand:compare(Straight7, Straight8),
    1 = hand:compare(Straight8, ThreeKindAce),
    1 = hand:compare(ThreeKindAce, ThreeKindKing),
    -1 = hand:compare(ThreeKindKing, ThreeKindAce),
    1 = hand:compare(ThreeKindAce, TwoPairAceKing),
    -1 = hand:compare(TwoPairAceKing, ThreeKindAce),
    1 = hand:compare(TwoPairAceKing, TwoPairKing10),    
    -1 = hand:compare(TwoPairKing10, TwoPairAceKing),    
    1 = hand:compare(PairKing, PairQueen),    
    -1 = hand:compare(PairQueen, PairKing),    
    %% print cards & hands
    "|A♣|" = deck:show({ace, club}),
    "|A♦| |K♦| |Q♦| |J♦| |10♦|" = deck:show(hand:arrange(royal_flush(dimonds))),
    {test_worked}.    
