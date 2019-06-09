-module(bools).

-export([load/0]).
-on_load(load/0).

-export([
  guess_sync/1,
  guess/1,
  guess_async/2
]).

-define(PRIV, "./priv").


guess_sync(_Ref) ->
    not_loaded(?LINE).

guess(_Ref) ->
    not_loaded(?LINE).

guess_async(_Ref, _Ref2) ->
    not_loaded(?LINE).

load() ->
    erlang:load_nif(filename:join(?PRIV, "libbools"), none).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).
