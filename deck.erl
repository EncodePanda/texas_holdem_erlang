-module(deck).
-export([create/0, shuffle/1]).

figures() ->
    lists:seq(2,9) ++ [jack, queen, king, ace].

colors() ->
    [dimonds, heart, club, spades].

create() ->
    [{Figure, Color} || Color <- colors(), Figure <- figures()].

shuffleList(L) ->
    [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- L])].

shuffle(Deck) ->
    shuffleList(Deck).
