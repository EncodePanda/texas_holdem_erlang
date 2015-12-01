# texas_holdem_erlang

My implementation of distributed Texas Holdem


## Current status

Generate deck & shuffle cards

```
> Deck = deck:shuffle(deck:create()).
[{8,heart},
 {2,spades},
 {3,spades},
 {7,club},
 {4,spades},
 {5,spades},
 {ace,heart},
 {king,dimonds},
 {3,heart},
 {...}|...]
```

Print single card

```
> Card = lists:nth(1, Deck).
{8,heart}
> util:print(deck:show(Card)).
|8♥|
```
Print few cards

```
> util:print(deck:sublist(Deck, 5)).
|8♥| |2♠| |3♠| |7♣| |4♠| 
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


[_as you can see not yet fully implemented_]

I'm currently thinkinging how to implement `compare` method in `hand.erl`, maybe you can help?

