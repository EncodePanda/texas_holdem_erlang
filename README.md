# texas_holdem_erlang

My implementation of distributed Texas Holdem


## Current status

Generate deck & shuffle cards

```
> Deck = deck:shuffle(deck:create()).
[{queen,dimonds},
 {ace,spades},
 {jack,dimonds},
 {8,dimonds},
 {6,dimonds},
 {king,spades},
 {4,dimonds},
 {ace,dimonds},
 {5,club},
 {...}|...]
```

Create few hands

```
HeartRoyalFlush = [{10,heart}, {king,heart}, {jack,heart}, {ace,heart}, {queen,heart}]
HeartFlush = [{queen,heart}, {ace,heart}, {jack,heart}, {10,heart}, {king,heart}]
FourOfAKindAce = [{ace,heart}, {ace,club}, {ace,dimonds}, {ace,spades}, {2,heart}]
FullHouseAceKing = [{ace,heart}, {king,dimonds}, {king,heart}, {ace,dimonds}, {ace,club}]
ColorClub = [{7,club},{4,club},{3,club},{8,club},{6,club}]
Straight8 = [{7,club},{4,heart},{6,spades},{5,dimonds},{8,heart}]
ThreeKindAce = [{2,heart},{3,heart},{ace,heart},{ace,dimonds},{ace,club}]
TwoPairsAceKing = [{2,heart}, {ace,heart}, {king,heart}, {king,dimonds}, {ace,dimonds}]
PairKing = [{king,dimonds}, {2,heart}, {king,heart}, {3,spades}, {4,dimonds}]
```

Rank hands

```
{royal_flush, heart} = hand:rank(HeartRoyalFlush),
{flush, 10, heart} = hand:rank(HeartFlush),
{four_of_a_kind, ace} = hand:rank(FourOfAKindAce),
{full_house, ace, king} = hand:rank(FullHouseAceKing),
{color, club, _} = hand:rank(ColorClub),
{straight, 8} = hand:rank(Straight8),
{three_of_a_kind, ace} = hand:rank(ThreeKindAce),
{two_pairs, ace, king} = hand:rank(TwoPairsAceKing),
{pair, king} = hand:rank(PairKing),
```

Arrange hands

```
> hand:arrange([{10,heart}, {king,heart}, {jack,heart}, {ace,heart}, {queen,heart}]).
[{ace,heart},
 {king,heart},
 {queen,heart},
 {jack,heart},
 {10,heart}]


> hand:arrange([{ace,heart}, {king,dimonds}, {king,heart}, {ace,dimonds}, {ace,club}]).
[{ace,heart},
 {ace,dimonds},
 {ace,club},
 {king,dimonds},
 {king,heart}]
```


Print single card

```
> Card = lists:nth(1, Deck).
{queen,dimonds}
> util:print(deck:show(Card)).
|Q♦|
```
Print few cards

```
> util:print(deck:show(lists:sublist(Deck, 5))).
|Q♦| |A♠| |J♦| |8♦| |6♦|
```

Select best hand (5 cards out of 7).


```
> Cards = lists:sublist(Deck, 7).
[{8,heart},
 {2,spades},
 {3,spades},
 {7,club},
 {4,spades},
 {5,spades},
 {ace,heart}]

> Hand = hand:pickBest(Cards).
[{3,spades},{7,club},{4,spades},{5,spades},{ace,heart}]

```


