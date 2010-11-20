%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2010-11-20
%%% @doc TODO: Add description to mqueue_srv_tests
%%% @end
%%% ==========================================================================
-module (mqueue_srv_tests).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-compile (export_all).

-include_lib ("eunit/include/eunit.hrl").

init_test () ->
  Result = mqueue_srv:init(["/qq", self(), [own]]),
  ?assertMatch({ok, _}, Result),
  receive
    {timeout, _, tick} -> ok
  after
      1000 -> exit(timeout)
  end,
  {ok, State} = Result,
  ?assertEqual(0, mqueue_srv:terminate(normal, State)).
