%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 24-11-2010
%%% @doc TODO: Add description to erleos_sensor_tests
%%% @end
%%% ==========================================================================
-module (erleos_sensor_tests).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-compile (export_all).

-include_lib ("eunit/include/eunit.hrl").

-record (ctx, {}).

setup () ->
  #ctx{}.
  
tear_down (#ctx{}) ->
  ok.

'test start' (_) ->
  T = erleos_sensor:start(?MODULE),
  ?assertMatch({ok, _}, T),
  {ok, S1} = T,
  erleos_sensor:stop(S1),
  Self = self(),
  spawn(fun () ->
            erlang:process_flag(trap_exit, true),            
            T2 = erleos_sensor:start_link(?MODULE),    
            ?assertMatch({ok, _}, T2),
            {ok, S2} = T2,
            erleos_sensor:stop(S2),
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
  {foreach,
   fun setup/0,
   fun tear_down/1,
   [fun (C) -> 'test start'(C) end]}.

%% ----------------------------------------------------------------------------
%% gen_Server callbacks
%% ----------------------------------------------------------------------------

init ([]) ->
  {ok, 0}.

handle_cast (shutdown, S) ->
  {stop, normal, S}.

terminate (_Reason, _) ->
  ok.
