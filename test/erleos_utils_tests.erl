%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 24-11-2010
%%% @doc TODO: Add description to erleos_sensor_tests
%%% @end
%%% ==========================================================================
-module(erleos_utils_tests).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-compile (export_all).

-include_lib ("eunit/include/eunit.hrl").

-record (ctx, {}).

setup () ->
  meck:new(test_srv),
  meck:expect(test_srv, init, fun ([]) ->
                                  {ok, undefined}
                              end),
  meck:expect(test_srv, handle_cast, fun (shutdown, S) ->
                                         {stop, normal, S}
                                     end),
  meck:expect(test_srv, terminate, fun (_, _) ->
                                       ok
                                   end),
  meck:expect(test_srv, handle_call, fun (shutdown, _, S) ->
                                         
                                         {stop, normal, ok, S}
                                     end),
  #ctx{}.
  
tear_down (#ctx{}) ->
  meck:unload(test_srv),
  ok.

'test start' (_) ->
  T = erleos_utils:start(test_srv),
  ?assertMatch({ok, _}, T),
  {ok, S1} = T,
  erleos_utils:stop(S1),
  Self = self(),
  spawn(fun () ->
            erlang:process_flag(trap_exit, true),            
            T2 = erleos_utils:start_link(test_srv),    
            ?assertMatch({ok, _}, T2),
            {ok, S2} = T2,
            erleos_utils:sync_stop(S2),
            receive
              {'EXIT', _, _} -> Self ! true
            after
                100 -> Self ! false
            end
        end),
 ?assert(receive V -> V end).
   
%% -----------------------------------------------------------------------------
%% @doc Tests Runner.
%% @end
%% -----------------------------------------------------------------------------

all_test_ () ->
  tests_runner:run(fun setup/0, fun tear_down/1, ?MODULE).
