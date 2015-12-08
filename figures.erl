-module(figures).
-export([list/0, show/1,f2i/1, i2f/1]).

list() ->
    lists:seq(2,9) ++ [jack, queen, king, ace].

show(jack) -> "J";
show(queen) -> "Q";
show(king) -> "K";
show(ace) -> "A";
show(N) -> util:number_to_string(N).

f2i(jack) -> 11;
f2i(queen) -> 12;
f2i(king) -> 13;
f2i(ace) -> 14;
f2i(N) -> N.

i2f(11) -> jack;
i2f(12) -> queen;
i2f(13) -> king;
i2f(14 ) -> ace;
i2f(N) -> N.
