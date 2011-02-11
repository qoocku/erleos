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
  create_fake_CAN_driver(),
  {ok, S} = erleos_utils:start('CAN_msg_router_srv', [{args, [{can_path, "/dev/can0"}]}]),
  #ctx{srv = S}.

tear_down1 (#ctx{srv = S}) ->
  ok = erleos_utils:sync_stop(S),
  meck:unload('CAN').

setup2 () ->
  'CAN_drv':create_sample_device("can"),
  {ok, S} = erleos_utils:start('CAN_msg_router_srv', [{args, [{can_path, "can"}]}]),
  #ctx{srv = S}.

tear_down2 (#ctx{srv = S}) ->
  ok = erleos_utils:sync_stop(S).

'(1) API: user may define list of id ranges with targets as pids or names' (#ctx{srv = S}) ->
  List      = [{{1, 6}, [tgt1]},
               {{7, 10}, [self()]}],
  lists:foreach(fun ({Ds, Tgt}) ->
                  ?assertEqual(ok, erleos_ds:subscribe(S, Ds, Tgt))
                end, List).

'(1) API: user may define list of standalone ids with targets as pids or names' (#ctx{srv = S}) ->
  List      = [{1, [self()]},
               {2, [tgt2]}],
  lists:foreach(fun ({Ds, Tgt}) ->
                  ?assertEqual(ok, erleos_ds:subscribe(S, Ds, Tgt))
                end, List).


'(1) API: user may define list of ids list with targets as pids or names' (#ctx{srv = S}) ->
  List      = [{[1, 4, 5], [self()]},
               {[2], [tgt2]}],
  lists:foreach(fun ({Ds, Tgt}) ->
                  ?assertEqual(ok, erleos_ds:subscribe(S, Ds, Tgt))
                end, List).


'(2) Reading: an example' (#ctx{srv = S}) ->
  List      = [{[1, 4, 5], [self()]},
               {[2], [tgt2]}],
  lists:foreach(fun ({Ds, Tgt}) ->
                  ?assertEqual(ok, erleos_ds:subscribe(S, Ds, Tgt))
                end, List).

%% -----------------------------------------------------------------------------
%% @doc Tests Runner.
%% @end
%% -----------------------------------------------------------------------------

'1_test_' () ->
  tests_runner:run(fun setup1/0, fun tear_down1/1, "(1)", ?MODULE).

'2_test_' () ->
  tests_runner:run(fun setup2/0, fun tear_down2/1, "(2)", ?MODULE).

%%% ==================================================

create_fake_CAN_driver () ->
  meck:new('CAN'),
  lists:foreach(fun ({N, F}) -> meck:expect('CAN', N, F) end,
                [
                 {open, fun (_Path, _Opts) ->
                              {ok, fake_can} 
                        end},
                 {close, fun (_Dev) -> ok end}
                ]).
