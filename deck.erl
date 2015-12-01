-module(deck).
-export([create/0, shuffle/1, show/1]).

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

show(spades) -> "♠";	       
show(heart) -> "♥";			       
show(club) -> "♣";		     
show(dimonds) -> "♦";
show(jack) -> "J";
show(queen) -> "Q";
show(king) -> "K";
show(ace) -> "A";
show({Figure, Color}) -> "|" ++ show(Figure) ++ show(Color) ++ "|";
show([H|T]) -> show(H) ++ " " ++ show(T);
show([]) -> "";
show(N) -> util:number_to_string(N).
