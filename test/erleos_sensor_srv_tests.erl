%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-02-24
%%% @doc TODO: Add description to erleos_usir_srv_tests
%%% @end
%%% ==========================================================================
-module  (erleos_sensor_srv_tests).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>").
-compile (export_all).

-include_lib ("eunit/include/eunit.hrl").
-include ("proto/sensor.hrl").
-include ("proto/usir.hrl").
-include ("proto/adis.hrl").

-record (ctx, {can_ctx, this, router, queues, type}).

ir_setup () ->
  create_ctx_for(ir).

us_setup () ->
  create_ctx_for(us).

adis_setup () ->
  create_ctx_for({adis, undefined}).

usir_setup () ->
  create_ctx_for(usir).

tear_down (#ctx{can_ctx = Ctx, this = Ss, type = {adis, undefined}}) ->
  [ok = erleos_utils:sync_stop(S) || S <- Ss],
  'CAN_msg_router_srv_tests':tear_down1(Ctx);
tear_down (#ctx{can_ctx = Ctx, this = S, type = usir}) ->
  {S1, S2} = S,
  erleos_utils:sync_stop(S1),
  erleos_utils:sync_stop(S2),
  'CAN_msg_router_srv_tests':tear_down1(Ctx);
tear_down (#ctx{can_ctx = Ctx, this = S}) ->
  erleos_utils:sync_stop(S),
  'CAN_msg_router_srv_tests':tear_down1(Ctx).

'(ir) reading messages'(#ctx{this = S, router = R, queues = Qs, type = ir}) ->
    P = generate_can_messages(ir,16,R,Qs), 
    test_mqueue_output(ir,Qs,P), 
    test_reading(S,P).

'(us) reading messages'(#ctx{this = S, router = R, queues = Qs, type = us}) ->
    P = generate_can_messages(us,16,R,Qs), 
    test_mqueue_output(us,Qs,P), 
    test_reading(S,P).

'(usir) reading messages'(#ctx{this = {S1,S2}, router = R, queues = [Qs1,Qs2], type = usir}) ->
    P1 = generate_can_messages(us,16,R,Qs1), 
    P2 = generate_can_messages(ir,16,R,Qs2), 
    test_mqueue_output(ir,Qs1,P1), 
    test_mqueue_output(us,Qs2,P2), 
    test_reading(S1,P1), 
    test_reading(S2,P2).

'(adis) reading messages'(#ctx{this = S, router = R, queues = Qs, type = {adis, T}}) ->
    P = generate_can_messages({adis, T}, 16, R, Qs), 
    [test_mqueue_output({adis, T}, Qss, P) || Qss <- Qs], 
    test_reading(S, P).

ir_test_ () ->
  tests_runner:run(fun ir_setup/0, fun tear_down/1, "(ir)", ?MODULE).

us_test_ () ->
  tests_runner:run(fun us_setup/0, fun tear_down/1, "(us)", ?MODULE).

usir_test_ () ->
  tests_runner:run(fun usir_setup/0, fun tear_down/1, "(usir)", ?MODULE).

adis_test_ () ->
  tests_runner:run(fun adis_setup/0, fun tear_down/1, "(adis)", ?MODULE).  

create_ctx_for (Type) ->
  % we need a mocked CAN driver with CAN msgs router
  Ctx = 'CAN_msg_router_srv_tests':setup1(),
  % read erleos app config
  {ok, [Cfg]} = file:consult(filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "erleos.config"])),
  register(Ds = list_to_atom(lists:foldl(fun proplists:get_value/2, Cfg, [erleos, case Type of
                                                                                    {adis, _}  -> adis_can_router;
                                                                                    Other -> usir_can_router
                                                                                   end, candev])),
           element(2, Ctx)),
  ok          = lists:foldl(fun ({Key, Val}, ok) ->
                               application:set_env(erleos, Key, Val)
                            end, ok, ErlCfg = proplists:get_value(erleos, Cfg)),
  % and USIR sensor server(s)
  {ok, S} = case Type of
              usir ->
                Ss = [begin
                        {ok, S} = erleos_utils:start(erleos_sensor_srv, [{args, [{type, T}, {cmod,
                                                                                             [{mod, list_to_atom("erleos_" ++ T ++ "_sensor_module")},
                                                                                              {args, []}]}]}]), 
                        S
                      end || T <- ["us", "ir"]],
                {ok, list_to_tuple(Ss)};
              {adis, undefined} ->
                {ok, [begin
                        {ok, S} = erleos_utils:start(erleos_sensor_srv, [{args, [{type, "adis"}, {cmod,
                                                                                                [{mod, erleos_adis_sensor_module},
                                                                                                 {args, [{type, T}]}]}]}]), 
                        S
                      end || T <- [accel, angvel, pos, linvel, factors, movinfo]]};
              {adis, ST} ->
                erleos_utils:start(erleos_sensor_srv, [{args, [{type, "adis"}, {cmod, 
                                                                              [{mod, erleos_adis_sensor_module},
                                                                               {args, [{type, ST}]}]}]}]);
              Type ->
                erleos_utils:start(erleos_sensor_srv, [{args, [{type, atom_to_list(Type)}, {cmod, 
                                                                              [{mod, list_to_atom("erleos_" ++ atom_to_list(Type) ++ "_sensor_module")},
                                                                               {args, []}]}]}])
            end,
  #ctx{can_ctx = Ctx,
       this    = S,
       router  = Ds,
       queues  = case Type of
                   usir ->
                     [lists:foldl(fun proplists:get_value/2, ErlCfg, [T, mqueues, output]) || T <- [us_sensor, ir_sensor]];
                   {adis, undefined} ->
                     [lists:foldl(fun proplists:get_value/2, ErlCfg, [list_to_atom(T ++ "_sensor"), mqueues, output]) 
                      || T <- ["accel", "angvel", "pos", "linvel", "factors", "movinfo"]];
                   {adis, ST2} ->
                     lists:foldl(fun proplists:get_value/2, ErlCfg, [list_to_atom(atom_to_list(ST2)), mqueues, output]);
                   Other2    -> 
                     lists:foldl(fun proplists:get_value/2, ErlCfg, [list_to_atom(atom_to_list(Other2) ++ "_sensor"), mqueues, output])
                 end,
       type    = Type}.

test_reading (_, {[], _}) ->
  ok;
test_reading(S,{Msgs,Ids}) ->
  Length = length(Ids), 
  IdSortFun = fun (X,Y) -> element(1,X)=<element(1,Y) end, 
  Newest = lists:sublist(Msgs,length(Msgs)-Length+1,Length), 
  Readings = erleos_sensor:get_last_reading(S), 
  ?assertEqual(length(Readings),length(Newest)), 
  ?assertEqual(Length,length(Readings)), 
  ?assert(lists:foldl(fun 
                         ({{Id, _, <<Value:16/little, _>>},
                           #usir_data{id = Id, value = Value}}, Bool) -> 
                           Bool and true;
                         ({{Id, _, <<AX:16/little, AY:16/little, AZ:16/little>>},
                           #accel_data{id = Id, ax = AX, ay = AY, az = AZ}}, Bool) ->
                           Bool and true;
                         ({{Id, _, <<AX:16/little, AY:16/little, AZ:16/little>>},
                           #angvel_data{id = Id, ax = AX, ay= AY, az = AZ}}, Bool) ->
                           Bool and true;
                         ({{Id, _, <<X:16/little, Y:16/little, Phi:16/little>>},
                           #pos_data{id = Id, x = X, y= Y, phi = Phi}}, Bool) ->
                           Bool and true;
                         ({{Id, _, <<X:16/little>>},
                           #linvel_data{id = Id, x = X}}, Bool) ->
                           Bool and true;
                         ({{Id, _, <<N, Z, K:16/little, KV:16/little, KW:16/little>>},
                           #factors_data{id = Id, n = N, z = Z, k = K, kv = KV, kw = KW}}, Bool) ->
                           Bool and true;
                         ({{Id, _, <<Info>>},
                           #movinfo_data{id = Id, info = Info}}, Bool) ->
                           Bool and true;
                         (_, _) ->
                           false
                       end, true, lists:zip(lists:sort(IdSortFun,Newest),
                                             lists:sort(fun 
                                                          (X = #usir_data{}, Y = #usir_data{}) -> 
                                                            X#usir_data.id =< Y#usir_data.id;
                                                          (X, Y) when is_tuple(X), is_tuple(Y) ->
                                                            element(2, X) =< element(2, Y)
                                                        end,
                                                        Readings)))).

test_mqueue_output(_, _, {[], _}) ->
  ok;
test_mqueue_output(Type, Qs, {Msgs,Ids}) ->
  Length = length(Ids), 
  SFun   = get_sort_fun(Type), 
  Reader = fun (QName,undefined,Loop,Acc) ->
                 {ok,Q} = mqueue:open(QName,[noblock]), 
                 Loop(Q,mqueue:recv(Q),Loop,[]);
             (Q,{ok,Data},Loop,Acc) ->
               Loop(Q,mqueue:recv(Q),Loop,[Data| Acc]);
             (Q,{error,_},_,Acc) ->
               mqueue:close(Q), 
               lists:reverse(Acc)
           end, 
  Readings = [lists:sort(SFun,Reader(QName,undefined,Reader,[])) || QName <- Qs], 
  Newest = lists:sublist(Msgs,length(Msgs)-Length+1,Length), 
  [?assertEqual(Length,length(R)) || R <- Readings], 
  [?assertEqual(length(Newest),length(R)) || R <- Readings], 
  [?assert(lists:foldl(fun ({Signal,Reading},Bool) ->
                          {I1, _, <<V1:16/little, C1>>} = Signal, 
                          {I2, _, <<V2:16/little, C2>>} = erleos_sensor_srv:decode_mqueue_packet(Reading), 
                          Bool and case {I2,V2,C2} of
                                      {I1,V1,C1} -> true;
                                      _ -> false
                                    end
                        end, true, lists:zip(lists:sort(Newest),Rs))) || Rs <- Readings].

get_sort_fun ({adis, _}) ->
  fun (<<I1:16/little,T1:32/little,_/binary>>,
        <<I2:16/little,T2:32/little,_/binary>>) ->
    {I1,T1} =< {I2,T2}
  end;
get_sort_fun (_) ->
  fun (<<I1:16/little,T1:32/little,_V1:16/little,C1>>,
        <<I2:16/little,T2:32/little,_V2:16/little,C2>>) ->
    {I1,T1,C1}=<{I2,T2,C2}
  end.

generate_can_messages(Type, N, R, Qs) ->
  IdList = get_can_device_ids(Type), 
  Msgs   = generate_can_device_messages(Type, N, IdList), 
  case Type of
    {adis, undefined} ->
      [clear_outdated_packets_from_queues(Qss) || Qss <- Qs];
    Type ->
      clear_outdated_packets_from_queues(Qs)
  end, 
  R ! {can,atom_to_list(R),Msgs}, 
  timer:sleep(200), 
  {Msgs, IdList}.

get_can_device_ids (Type) when Type =:= ir orelse Type =:= us ->
  Ranges = lists:foldl(fun proplists:get_value/2,
                       application:get_all_env(erleos),
                       [usir_can_router,case Type of
                                          ir -> ir_data_id;
                                          us -> us_data_id;
                                          adis -> adis_data_ir
                                        end]), 
  lists:flatten([[Id || Id <- lists:seq(Left,Right)] || {Left,Right} <- Ranges]);
get_can_device_ids ({adis, ST}) ->
  Cfg  = proplists:get_value(adis_can_router, application:get_all_env(erleos)),
  DLst = case ST of
           undefined ->
             ["accel",
              "angvel",
              "pos",
              "linvel",
              "factors",
              "movinfo"];
           ST -> 
             atom_to_list(ST)
         end,
  [{list_to_atom(Key ++ "_data"),
    proplists:get_value(list_to_atom(Key ++ "_id"), Cfg)} || Key <- DLst].
generate_can_device_messages ({adis, _}, N, IdList) ->
  Generator = fun 
                (VelData, I) when VelData =:= accel_data orelse VelData =:= angvel_data ->
                   [AX, AY, AZ] = [I, I*2, I*3],
                   <<AX:16/little, AY:16/little, AZ:16/little>>;
                (pos_data, I) ->
                   [X, Y, Phi] = [I, I*2, I*3 rem 360],
                   <<X:16/little, Y:16/little, Phi:16/little>>;
                (linvel_data, I) ->
                   <<I:16/little>>;
                (factors_data, I) ->
                   [Z, K, KV, KW] = [I+1, I+2, I*10, I*20],
                   <<(I rem 8), (Z rem 8), K:16/little, KV:16/little, KW:16/little>>;
                (movinfo_data, I) ->
                   <<(I rem 8)>>
               end,
  lists:flatten([
                  [{Id, {I, 0}, Generator(D, I)} || {D, Id} <- IdList] 
                || I <- lists:seq(1, N)
                ]);
generate_can_device_messages (Usir, N, IdList) when Usir =:= usir orelse
                                                     Usir =:= us orelse
                                                     Usir =:= ir ->
  lists:flatten([[{Id,{Cycle,0},<<Cycle:16/little,Cycle>>} || Id <- IdList] || Cycle <- lists:seq(1,N)]).

clear_outdated_packets_from_queues ([]) ->
  cleared;
clear_outdated_packets_from_queues ([Q|Tail]) when not is_list(Q) ->
  cleared = clear_outdated_packets_from_queues (Q),
  clear_outdated_packets_from_queues (Tail);
clear_outdated_packets_from_queues (Qs) ->
  Cleaner = fun
              (Q, {error, eagain}, _) ->
                mqueue:close(Q),
                cleared;
              (Q, {ok, _}, Loop) ->
                Loop(Q, mqueue:recv(Q), Loop)
            end,
  lists:foldl(fun (QName, cleared) ->
                {ok, Q} = mqueue:open(QName, [noblock]),
                Fun     = fun () -> Cleaner(Q, mqueue:recv(Q), Cleaner) end,
                Fun()
              end, cleared, Qs).
  
                          