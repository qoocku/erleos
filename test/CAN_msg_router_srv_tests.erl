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
-include ("proto/data_source.hrl").

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
  List      = [{{1, 5}, [self()]},
               {{6, 11}, [tgt3]}],
  lists:foreach(fun ({Ds, Tgt}) ->
                  ?assertEqual(ok, erleos_ds:subscribe(S, Ds, Tgt))
                end, List).


-define (now_to_can_ts(Now), begin {_, B, _} = Now, {B, 0} end). 

'(1) Reading: an example' (#ctx{srv = S}) ->
  List      = [{Ids=lists:seq(1, 256), [self()]}],
  lists:foreach(fun ({Ds, Tgt}) ->
                  ?assertEqual(ok, erleos_ds:subscribe(S, Ds, Tgt))
                end, List),
  Data = [{Id, ?now_to_can_ts(now()), list_to_binary(integer_to_list(100*Id))} || Id <- Ids],
  % bomb the router with fake CAN signals:
  S ! {can, <<"fake-can">>, Data},
  % collect the results
  ?assertEqual({received, true}, receive
                                     #ds_data{readings = Readings} ->
                                       {received, Readings -- Data =:= []}
                                   after 500 ->
                                     false
                                   end).

'(1) Reading: many subscribers' (#ctx{srv = S}) ->
  test_many_subscribers(S, 16),
  test_many_subscribers(S, 32),
  test_many_subscribers(S, 64),
  test_many_subscribers(S, 128),
  test_many_subscribers(S, 256).

'(2) Reading and sending: an example' (#ctx{srv = S} = Ctx) ->
  Self = self(),
  Pid  = spawn(fun () ->
                 receive
                   Lst -> Self ! {data, Lst}
                 end
               end),  
  ?assertEqual(ok, erleos_ds:subscribe(S, {1, 6}, [Pid])),
  ?assert(receive
            {data, Data} ->
              lists:foldl(fun (Id, true) ->
                            lists:keymember(Id, 1, Data)
                          end, true, lists:seq(1, 6))                
           after 500 -> 
              false 
           end).

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

test_many_subscribers (S, N) ->
  % 16 ids ranges
  Ranges  = lists:zip(lists:seq(1, 16*N, 16), lists:seq(16, 16*N, 16)),
  % a subscriber process behavior
  SubProc = fun (Master, Parent, Range) ->
              ok = erleos_ds:subscribe(S, Range),
              Master ! {registered, self()},
              receive                        
                R -> Parent ! R,
                ok = erleos_ds:unsubscribe(S, Range)
              end
            end,
  % fake CAN data
  Data    = [{Id, ?now_to_can_ts(now()), list_to_binary(integer_to_list(100*Id))} || Id <- lists:seq(1, 16*N)],
  % ds_data collector process behavior
  Collect = fun (Parent, Collected, Loop) ->
              if
                length(Collected) == 16*N ->
                  Parent ! (Collected -- Data =:= []);
                true ->
                  receive
                    #ds_data{readings = Readings} ->                  
                      Loop(Parent, Collected ++ Readings, Loop)
                  after 1000 ->
                    Parent ! false
                  end
                end
            end,
  Self    = self(),
  % the collector process  
  Pid     = spawn(fun () -> Collect(Self, [], Collect) end),
  % 16 subscribers
  Subscr  = [spawn(fun () -> SubProc(Self, Pid, Range) end) || Range <- Ranges],
  % wait till all subscribers are registered
  ?assert(Subscr -- lists:foldl (fun (_, Acc) ->
                                    receive
                                      {registered, SubsPid} -> [SubsPid | Acc]
                                    after 1000 ->
                                      Acc
                                    end
                                  end, [], Subscr) =:= []),
  % check registered subscribers
  Registered = lists:sort(gen_server:call(S, registered)),
  ?assert(lists:foldl(fun ({_, Pids}, Bool) ->
                         Bool andalso length(Pids) == 1 andalso lists:member(hd(Pids), Subscr)
                       end, true, Registered)),
  % bomb the router with fake CAN signals:
  S ! {can, <<"fake-can">>, Data},
  % collect the results
  ?assert(receive Bool -> Bool end).
