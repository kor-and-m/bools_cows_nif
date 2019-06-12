-module(bools).

-export([load/0]).
-on_load(load/0).

-export([
  guess_sync/1,
  guess/1,
  guess_async/2
]).

-define(APPNAME, bools_cows_nif).
-define(LIBNAME, libbools).

guess_sync(_Ref) ->
    not_loaded(?LINE).

guess(_Ref) ->
    not_loaded(?LINE).

guess_async(_Ref, _Ref2) ->
    not_loaded(?LINE).

load() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).
