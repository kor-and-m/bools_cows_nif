-type(guess() :: [integer()]).

-record(result, {
  bools = 0 :: integer(),
  cows  = 0 :: integer()
}).

-record(step, {
  guess  :: guess(),
  result :: #result{}
}).

-type(history() :: [#step{}]).
