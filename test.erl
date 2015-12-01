-module(test).
-export([royal_flush/1, flush/2, four_kind/1]).

royal_flush(C) ->
    deck:shuffle([{ace,C}, {king,C}, {queen,C}, {jack,C}, {10,C}]).

flush(N, C) ->
    deck:shuffle([{N,C}, {N-1,C}, {N-2,C}, {N-3,C}, {N-4,C}]).

four_kind(F) ->
    deck:shuffle(lists:map(fun(C) -> {F,C} end, deck:colors()) ++ [{2,heart}]).
