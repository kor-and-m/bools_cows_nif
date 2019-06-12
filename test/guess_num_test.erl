-module(guess_num_test).

-include_lib("eunit/include/eunit.hrl").

-export([
    guess_success_test/0,
    guess_wrong_length_test/0,
    guess_repeat_numbers_test/0
]).

guess_success_test() ->
    ?assertMatch({ok, _}, bools_cows_nif:guess_num([1,2,3,4])).

guess_wrong_length_test() ->
    ?assertEqual({error, wrong_length}, bools_cows_nif:guess_num([1,2,3])).

guess_repeat_numbers_test() ->
    ?assertEqual({error, numerals_repeat}, bools_cows_nif:guess_num([1,2,3,2])).
