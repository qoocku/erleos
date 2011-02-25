%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-02-24
%%% @doc TODO: Add description to erleos_usir_srv_tests
%%% @end
%%% ==========================================================================
-module  (erleos_usir_sensor_srv_tests).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>").
-compile (export_all).

-include_lib ("eunit/include/eunit.hrl").
-include ("proto/sensor.hrl").

-record (ctx, {can_ctx, this, router, queues, type}).

ir_setup () ->
  create_ctx_for(ir).

us_setup () ->
  create_ctx_for(us).

usir_setup () ->
  create_ctx_for(usir).

usir_tear_down (#ctx{can_ctx = Ctx, this = S, type = usir}) ->
  {S1, S2} = S,
  erleos_utils:sync_stop(S1),
  erleos_utils:sync_stop(S2),
  'CAN_msg_router_srv_tests':tear_down1(Ctx);
usir_tear_down (#ctx{can_ctx = Ctx, this = S}) ->
  erleos_utils:sync_stop(S),
  'CAN_msg_router_srv_tests':tear_down1(Ctx).

'(ir) reading messages' (#ctx{this = S, router = R, queues = Qs, type = ir}) ->
  {Msgs, Ids} = generate_usir_can_messages(ir, 16, R, Qs),
  test_usir_reading(S, R, Qs, ir, {Msgs, Ids}).

'(us) reading messages' (#ctx{this = S, router = R, queues = Qs, type = us}) ->
  {Msgs, Ids} = generate_usir_can_messages(us, 16, R, Qs),
  test_usir_reading(S, R, Qs, us, {Msgs, Ids}).

'(usir) reading messages' (#ctx{this = {S1, S2}, router = R, queues = [Qs1, Qs2], type = usir}) ->
  P1 = generate_usir_can_messages(us, 16, R, Qs1),
  P2 = generate_usir_can_messages(ir, 16, R, Qs2),
  test_usir_reading(S1, R, Qs1, us, P1),
  test_usir_reading(S2, R, Qs2, ir, P2).

ir_test_ () ->
  tests_runner:run(fun ir_setup/0, fun usir_tear_down/1, "(ir)", ?MODULE).

us_test_ () ->
  tests_runner:run(fun us_setup/0, fun usir_tear_down/1, "(us)", ?MODULE).

usir_test_ () ->
  tests_runner:run(fun usir_setup/0, fun usir_tear_down/1, "(usir)", ?MODULE).

create_ctx_for (Type) ->
  % we need a mocked CAN driver with CAN msgs router
  Ctx = 'CAN_msg_router_srv_tests':setup1(),
  % read erleos app config
  {ok, [Cfg]} = file:consult(filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "erleos.config"])),
  register(Ds = list_to_atom(lists:foldl(fun proplists:get_value/2, Cfg, [erleos, usir_can_router, candev])),
           element(2, Ctx)),
  ok          = lists:foldl(fun ({Key, Val}, ok) ->
                               application:set_env(erleos, Key, Val)
                            end, ok, ErlCfg = proplists:get_value(erleos, Cfg)),
  % and USIR sensor server(s)
  {ok, S} = case Type of
              usir ->
                Ss = [begin
                        {ok, S} = erleos_utils:start(erleos_usir_sensor_srv, [{args, [{type, T}]}]), 
                        S
                      end || T <- [us, ir]],
                {ok, list_to_tuple(Ss)};
              _    ->
                erleos_utils:start(erleos_usir_sensor_srv, [{args, [{type, Type}]}])
            end,
  #ctx{can_ctx = Ctx,
       this    = S,
       router  = Ds,
       queues  = case Type of
                    usir ->
                      [lists:foldl(fun proplists:get_value/2, ErlCfg, [T, mqueues, output]) || T <- [us_sensor, ir_sensor]];
                    _    -> 
                      lists:foldl(fun proplists:get_value/2, ErlCfg, [case Type of
                                                                         ir -> ir_sensor;
                                                                         us -> us_sensor
                                                                       end, mqueues, output])
                 end,
       type    = Type}.

test_usir_reading (S, R, Qs, Type, {Msgs, Ids}) ->    
  Length    = length(Ids),
  IdSortFun = fun (X, Y) -> element(1, X) =< element(1, Y) end,
  Latest    = lists:sublist(Msgs, length(Msgs)-Length+1, Length),
  Readings  = erleos_sensor:get_last_reading(S),
  ?assertEqual(length(Readings), length(Latest)), 
  ?assertEqual(Length, length(Readings)),
  ?assert(lists:foldl(fun ({Signal, Reading}, Bool) ->
                          {Id, _, Data = <<Value:16/little, _Cycle>>} = Signal,
                          Bool and case {Reading#reading.sid, Reading#reading.value} of  
                                      {Id, Value} -> true;
                                      _           -> false
                                   end
                       end, true, lists:zip(lists:sort(IdSortFun, Latest),
                                             lists:sort(fun (X, Y) -> X#reading.sid =< Y#reading.sid end,
                                                        Readings)))).

generate_usir_can_messages (Type, N, R, Qs) ->
  Ranges = lists:foldl(fun proplists:get_value/2,
                       application:get_all_env(erleos),
                       [usir_can_router, case Type of
                                           ir -> ir_data_id;
                                           us -> us_data_id
                                         end]),
  IdList = lists:flatten([[Id || Id <- lists:seq(Left, Right)] || {Left, Right} <- Ranges]),
  Msgs   = lists:flatten([[{Id, {Cycle, 0}, <<Cycle:16/little, Cycle>>} || Id <- IdList] || Cycle <- lists:seq(1, N)]),
  clear_outdated_packets_from_queues(Qs),
  R ! {can, atom_to_list(R), Msgs},
  timer:sleep(200),
  {Msgs, IdList}.

clear_outdated_packets_from_queues ([]) ->
  cleared;
clear_outdated_packets_from_queues ([Q|Tail]) when not is_list(Q) ->
  cleared = clear_outdated_packets_from_queues (Q),
  clear_outdated_packets_from_queues (Tail);
clear_outdated_packets_from_queues (Qs) ->
  Cleaner = fun
              (_, {error, eagain}, _) -> 
                cleared;
              (Q, {ok, _}, Loop) ->
                Loop(Q, mqueue:recv(Q), Loop)
            end,
  lists:foldl(fun (QName, cleared) ->
                {ok, Q} = mqueue:open(QName, [noblock]),
                Fun     = fun () -> Cleaner(Q, mqueue:recv(Q), Cleaner) end,
                Fun()
              end, cleared, Qs).
  
                          