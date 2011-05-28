%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 25-11-2010
%%% @doc TODO: Add description to erleos_ds_srv_tests
%%% @end
%%% ==========================================================================
-module (erleos_ds_srv_tests).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-compile (export_all).

-include_lib ("eunit/include/eunit.hrl").

-record (ctx, {mod, srv, samples}).

setup1 () ->
  #ctx{mod = create_callback_module()}.
  
tear_down1 (#ctx{mod = Mod}) ->
  meck:unload(Mod),
  ok.

setup2 () ->
  Ctx = setup1(),
  {ok, Pid} = erleos_utils:start(erleos_ds_srv, [{args, [test_ds,
                                                         [{Ctx#ctx.mod, [ds, specific]}]]}]),  
  Ctx#ctx{srv = Pid}.
  
tear_down2 (#ctx{mod = Mod, srv = S}) ->
  catch erleos_utils:sync_stop(S),
  meck:unload(Mod),
  ok.

setup3 () ->
  Ctx     = setup1(),
  Samples = lists:zip(lists:seq(1, 100), lists:reverse(lists:seq(1,100))), 
  {ok, Pid} = erleos_utils:start(erleos_ds_srv,
                                 [{args, [test_ds, 
                                          [{Ctx#ctx.mod, [DsType, DsId]} 
                                           ||  {DsType, DsId} <- Samples]]}]),
  Ctx#ctx{srv = Pid, samples = Samples}.
  
'(1) test init' (#ctx{mod = Mod}) ->
  Result = erleos_utils:start(erleos_ds_srv, [{args, [test_ds,
                                                      [{Mod, [ds, specific]}]]}]),
  ?assertMatch({ok, _}, Result),
  {ok, Pid} = Result,
  ?assert(is_pid(Pid)),
  ?assertMatch([_|_], ets:info(test_ds)),
  erleos_utils:sync_stop(Pid).

'(2) test subscribe' (#ctx{srv = S}) ->
  Self = self(),
  ?assertEqual(ok, erleos_ds:subscribe(S, {ds, specific})),
  ?assertMatch([{{ds, specific}, [{[Self], _, _}]}],
                 ets:lookup(test_ds, {ds, specific})).

'(2) test unsubscribe' (#ctx{srv = S} = Ctx) ->
  '(2) test subscribe'(Ctx),
  ?assertEqual(ok, erleos_ds:unsubscribe(S, {ds, specific})),
  ?assertEqual([], ets:lookup(test_ds, {ds, specific})).

'(2) test receiving data' (Ctx) ->
  '(2) test subscribe'(Ctx),
  test_receiving_data({ds, specific}).

'(3) test multi source subscribe' (#ctx{srv = S, samples = Samples}) ->
  Self = self(),
  lists:foreach(fun (Sample) ->
                    ?assertEqual(ok, erleos_ds:subscribe(S, Sample)),
                    ?assertMatch([{Sample, [{[Self], _, _}]}],
                                 ets:lookup(test_ds, Sample))
                end, Samples).

'(3) test multi source receiving data' (#ctx{samples = Samples} = Ctx) ->
  '(3) test multi source subscribe'(Ctx),
  lists:foreach(fun test_receiving_data/1, Samples).

'(3) test multi source receiving data [multi receive version]' (#ctx{samples = Samples} = Ctx) ->
  '(3) test multi source subscribe'(Ctx),
  lists:foreach(fun (_) ->
                    lists:foreach(fun test_receiving_data/1, Samples)
                end, lists:seq(1, 10)).

'(4) test server callbacks termination' (#ctx{srv = S, samples = Samples}) ->
  erlang:register(test_ds, self()),
  ok = erleos_utils:sync_stop(S),
  lists:foreach(fun (Sample) ->
                    ?assert(receive
                               {terminated, Sample} -> true
                             after
                               1000 -> false
                             end)
                end, Samples).

%% -----------------------------------------------------------------------------
%% @doc Tests Runner.
%% @end
%% -----------------------------------------------------------------------------

'1_test_' () ->
  tests_runner:run(fun setup1/0, fun tear_down1/1, "(1)", ?MODULE).

'2_test_' () ->
  tests_runner:run(fun setup2/0, fun tear_down2/1, "(2)", ?MODULE).

'3_test_' () ->
  tests_runner:run(fun setup3/0, fun tear_down2/1, "(3)", ?MODULE).

'4_test_' () ->
  tests_runner:run(fun setup3/0, fun tear_down1/1, "(4)", ?MODULE).

%%% ===========================================================================
%%% I n t e r n a l  F u n  c t i o n s
%%% ===========================================================================

create_callback_module () ->
  meck:new(mock_ds_callback),
  lists:foreach(fun ({N, F}) -> meck:expect(mock_ds_callback, N, F) end,
                [
                 {init,      fun ([DsType, DsId]) ->
                                 {ok, {DsType, DsId}, {DsType, DsId}} 
                                 end},
                 {run,       fun (Self, Env) ->
                                 {ok, _} = timer:apply_interval(100, mock_ds_callback,
                                                                get, [Self, Env])
                             end},
                 {terminate, fun (Env) -> catch (test_ds ! {terminated, Env}) end},
                 {get,       fun (Self, {Type, Id}) -> 
                                 Self ! {Type, Id, [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]}
                             end}
                ]),
  mock_ds_callback.

test_receiving_data ({Type, Id}) ->
  ?assert(receive
            {Type, Id, List} when is_list(List) -> true
           after
              1000 -> false
           end).
