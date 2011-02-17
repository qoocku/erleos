%% Author: leo
%% Created: 2011-02-10
%% Description: TODO: Add description to erleos_usir_sensor_srv
-module (erleos_usir_sensor_srv).
-behavior (gen_server).

-export ([init/1,
		  handle_call/3,
		  handle_cast/2,
		  handle_info/2,
		  terminate/2,
		  code_change/3]).

-include ("proto/sensor.hrl").
-include ("proto/usir.hrl").

-type readings () :: [reading()].
-record (state, {can_ds       = undefined :: atom() | pid() | {node(), atom()},
                 address_base = 0         :: integer(),
				         last_reading = []        :: readings(),
                 ir_queues    = []        :: [any()],
                 us_queues    = []        :: [any()]}).

init (Options) when is_list(Options) ->
  DefCANds = case application:get_env(usir_can_ds) of
			   undefined   -> {'CAN_ds', {can, 0}};
			   {ok, Other} -> Other
			 end,
  CANds    = proplists:get_value(can_ds, Options, DefCANds),
  ok       = subscribe_to_ds(CANds),
  {ok, #state{can_ds = CANds}}.

handle_call (#get_last_reading{}, _From, State) ->
  {reply, State#state.last_reading, State}.

handle_cast (shutdown, State) ->
  {stop, normal, State}.

handle_info (Rest, State) when is_list(Rest) ->
  Readings = process_data(Rest, [], State),
  spawn(fun () -> emit_data(Readings, State) end),
  {noreply, State#state{last_reading = Readings}}.

terminate (shutdown, #state{can_ds = Ds}) ->
  unsubscribe_from_ds(Ds).

code_change (_Vsn, State, _Extra) ->
  %% close all queues
  [
   [mqueue:close(Q) || Q <- Qs] || Qs <- [State#state.ir_queues, State#state.us_queues]
  ],
  %% re-open queues
  NewState = open_queues(State),
  {ok, NewState}.

%%% ---

open_queues (State) ->
  {ok, IRSensorCfg} = erleos_utils:get_arg(ir_sensor),
  {ok, USSensorCfg} = erleos_utils:get_arg(us_sensor),
  IRQueues          = proplists:get_value(output, proplists:get_value(mqueues, IRSensorCfg)),
  USQueues          =  proplists:get_value(output, proplists:get_value(mqueues, USSensorCfg)),
  S1 = State#state{ir_queues = [mqueue:open(Q) || Q <- IRQueues]},
  S1#state{us_queues = [mqueue:open(Q) || Q <- USQueues]}.
  
subscribe_to_ds ({CANSrv, {can, N}}) ->
  erleos_ds:subscribe(CANSrv, {can, N}).

unsubscribe_from_ds ({CANSrv, {can, N}}) ->
  erleos_ds:unsubscribe(CANSrv, {can, N}).

process_data ([], Acc, _) ->
  Acc;
process_data ([{Id, Timestamp, Data} | Rest], Acc, State = #state{address_base = AB}) ->
  X                  = (Id rem AB),
  <<V:16/little, C>> = Data,
  Data2              = if
                       X == 1 -> % US reading
                         #raw_data{type = us, value = V, cycle = C, time = Timestamp, id = Id};
                       X > 1 andalso X < 7 -> % IR reading
                         #raw_data{type = ir, value = V, cycle = C, time = Timestamp, id = Id}
                     end,
  process_data(Rest, [Data2 | Acc], State).
  
emit_data (Readings, State) ->
  lists:foreach(fun (#raw_data{id = Id, type = Type, time = Time, value = Value, cycle = Cycle}) ->
                  Packet = <<Id:16/little, Time:32/little, Value:16/little, Cycle>>,
                  write_queue(Type, Packet, State)
                end, Readings).

write_queue (ir, Packet, #state{ir_queues = Qs}) ->
  write_queue(Qs, Packet);
write_queue (us, Packet, #state{us_queues = Qs}) ->
  write_queue(Qs, Packet).

write_queue (Qs, Packet) ->
  [mqueue:send(Q, Packet) || Q <- Qs].

