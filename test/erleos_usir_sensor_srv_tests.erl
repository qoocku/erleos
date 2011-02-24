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

-record (ctx, {can_ctx, this, router}).

ir_setup () ->
  % we need a mocked CAN driver with CAN msgs router
  Ctx = 'CAN_msg_router_srv_tests':setup1(),
  % read erleos app config
  {ok, [Cfg]} = file:consult(filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "erleos.config"])),
  register(Ds = list_to_atom(lists:foldl(fun proplists:get_value/2, Cfg, [erleos, usir_can_router, candev])),
           element(2, Ctx)),
  ok          = lists:foldl(fun ({Key, Val}, ok) ->
                               application:set_env(erleos, Key, Val)
                            end, ok, proplists:get_value(erleos, Cfg)),
  % and USIR sensor server
  {ok, S} = erleos_utils:start(erleos_usir_sensor_srv, [{args, [{type, ir}]}]),
  #ctx{can_ctx = Ctx, this = S, router = Ds}.

ir_tear_down (#ctx{can_ctx = Ctx, this = S}) ->
  erleos_utils:sync_stop(S),
  'CAN_msg_router_srv_tests':tear_down1(Ctx).

'(ir) reading messages' (#ctx{this = S, router = R}) ->
  Msgs = generate_usir_can_messages(16),
  R ! {can, atom_to_list(R), Msgs},
  timer:sleep(99),
  ?debugVal(erleos_sensor:get_last_reading(S)),
  ok.

ir_test_ () ->
  tests_runner:run(fun ir_setup/0, fun ir_tear_down/1, "(ir)", ?MODULE).

generate_usir_can_messages (N) ->
  Ids = lists:foldl(fun proplists:get_value/2, application:get_all_env(erleos), [usir_can_router, ir_data_id]),
  lists:flatten([[{Id, {0, 0}, <<Id:16/little, 0>>} || Id <- lists:seq(Left, Right)] || {Left, Right} <- Ids]).
  