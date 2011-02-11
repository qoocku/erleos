%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2010-12-20
%%% @doc TODO: Add description to can_messages_router_tests
%%% @end
%%% ==========================================================================
-module ('CAN_msg_router_srv_tests').
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").

-compile (export_all).
-include_lib ("eunit/include/eunit.hrl").

-record (ctx, {srv}).

setup1 () ->
  {ok, S} = erleos_utils:start('CAN_msg_router_srv', []),
  #ctx{srv = S}.

tear_down1 (#ctx{srv = S}) ->
  erleos_utils:stop(S).

'(1) API: user may define list of id ranges with targets as pids or names' (#ctx{srv = S}) ->
  List      = [{{1, 6}, tgt1},
               {{7, 10}, self()}],
  ?assertEqual(ok, erleos_ds:subscribe(S, List)).

'(1) API: user may define list of ida with targets as pids or names' (#ctx{srv = S}) ->
  List      = [{1, self()},
               {2, tgt2}],
  ?assertEqual(ok, erleos_ds:subscribe(S, List)).

%% -----------------------------------------------------------------------------
%% @doc Tests Runner.
%% @end
%% -----------------------------------------------------------------------------

'1_test_' () ->
  tests_runner:run(fun setup1/0, fun tear_down1/1, "(1)", ?MODULE).

