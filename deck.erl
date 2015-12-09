-module(deck).
-export([create/0, shuffle/1, show/1]).

create() ->
    [{Figure, Color} || Color <- colors:list(), Figure <- figures:list()].

shuffle(Deck) ->
    util:shuffleList(Deck).

show({Figure, Color}) -> "|" ++ figures:show(Figure) ++ colors:show(Color) ++ "|";
show([H|T]) -> string:strip(show(H) ++ " " ++ show(T));
show([]) -> "".
