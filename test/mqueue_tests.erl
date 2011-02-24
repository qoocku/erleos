%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 19-11-2010
%%% @doc TODO: Add description to new_file
%%% @end
%%% ==========================================================================
-module (mqueue_tests).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").

-include_lib ("eunit/include/eunit.hrl").

valid_parse_option_test () ->
  {QS, MMS, Rest} = mqueue:parse_options([]),
  check_parsing_result(QS, MMS, Rest),
  {QS2, MMS2, Rest2} = mqueue:parse_options([own]),
  check_parsing_result(QS2, MMS2, Rest2),
  {QS3, MMS3, Rest3} = mqueue:parse_options([{active, self()}]),
  check_parsing_result(QS3, MMS3, Rest3),
  {QS4, MMS4, Rest4} = mqueue:parse_options([{active, self()}, own]),
  check_parsing_result(QS4, MMS4, Rest4).  

check_parsing_result (QS, MMS, Rest) ->
  ?assert(is_integer(QS)),
  ?assert(QS > 0),
  ?assert(is_integer(MMS)),
  ?assert(MMS > 0),
  ?assert(Rest =:= [] orelse lists:member(own, Rest) orelse lists:keymember(self(), 2, Rest)).

invalid_parse_option_test () ->
  ?assertExit({badarg, _}, mqueue:parse_options([qoocku, na_mooniu, own])).

  