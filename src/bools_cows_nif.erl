-module(bools_cows_nif).

-export([
  guess_num/1,
  next_guess/2
]).

-include("game.hrl").
-include("nif_api.hrl").

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

-spec guess_num([integer()]) -> {ok, history()} | api_error_types().
guess_num(G) ->
  case bools:guess(G) of
    {ok, Result} ->
      {ok, lists:map(fun ({Guess, {B, C}}) ->
        #step{result = #result{bools = B, cows = C}, guess = Guess}
      end, Result)};
    {error, Reason} ->
      {error, binary_to_atom(Reason, latin1)}
  end.