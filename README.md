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

Take first 7 cards

```
> Cards = lists:sublist(Deck, 7).
[{8,heart},
 {2,spades},
 {3,spades},
 {7,club},
 {4,spades},
 {5,spades},
 {ace,heart}]
 ```
 
 Select best hand (5 cards out of 7).

[_as you can see not yet fully implemented_]

I'm currently thinkinging how to implement `compare` method in `hand.erl`, maybe you can help?
> Hand = hand:pickBest(Cards).
[{3,spades},{7,club},{4,spades},{5,spades},{ace,heart}]
```
