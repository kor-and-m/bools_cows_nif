-module(bools_cows_nif).

-export([
  guess_num/1,
  next_guess/2
]).

-include("game.hrl").

-spec next_guess(history(), sync | {async, integer()}) -> guess().
next_guess(History, sync) ->
  SerializedHistory = lists:map(fun (#step{guess = G, result = #result{cows = C, bools = B}}) ->
    {G, {B, C}}
  end, History),
  bools:guess_sync(SerializedHistory);
next_guess(History, {async, C1}) ->
  SerializedHistory = lists:map(fun (#step{guess = G, result = #result{cows = C, bools = B}}) ->
    {G, {B, C}}
  end, History),
  bools:guess_async(SerializedHistory, C1).

guess_num(G) ->
  lists:map(
    fun ({Guess, {B, C}}) ->
      #step{result = #result{bools = B, cows = C}, guess = Guess}
    end, bools:guess(G)
  ).