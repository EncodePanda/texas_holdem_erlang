-module(colors).
-export([list/0, show/1]).

list() ->
    [dimonds, heart, club, spades].

show(spades) -> "♠";
show(heart) -> "♥";
show(club) -> "♣";
show(dimonds) -> "♦".



