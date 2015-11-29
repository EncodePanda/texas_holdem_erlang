-module(table).
-export([create/3]).

emptySeat() ->
    {seat, empty}.

createEmptySeats(Seats) ->
    [emptySeat() || S <- lists:seq(1, Seats)].

create(Seats, BuyIn, SmallBlind) ->
    {
      {seats, createEmptySeats(Seats)},
      {buyIn, BuyIn},
      {smallBlind, SmallBlind}
    }.
    

