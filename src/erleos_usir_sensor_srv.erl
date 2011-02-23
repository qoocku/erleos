%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-02-10
%%% @doc Callback module for USIR sensor actor.

-module (erleos_usir_sensor_srv).
-author ("Damian T. Dobroczy\\'snki <qoocku@gmail.com>").
-behavior (gen_server).
-include ("vsn").

-export ([init/1,
		      handle_call/3,
		      handle_cast/2,
		      handle_info/2,
		      terminate/2,
		      code_change/3]).

-include ("proto/sensor.hrl").
-include ("proto/usir.hrl").

-type readings    () :: [reading()].
-type can_readings() :: [can_reading()].
-record (state, {can_ds       = undefined :: {atom() | pid() | {node(), atom()}, {can, string()}},
                 address_base = 0         :: integer(),
				         last_reading = []        :: readings(),
                 queues       = []        :: [any()],
                 type         = ir        :: ir | us}).
-type state()    :: #state{}.
-type initArg()  :: {usir_can_ds, any()} | {type, ir | us}. 
-type initArgs() :: [initArg()].
-type call()     :: #get_last_reading{}.
-type reply()    :: readings().
-type cast()     :: shutdown.
-type info()     :: [can_reading()].

-spec init (initArgs()) -> {ok, state()}.
-spec handle_call (call(), any(), state()) -> {reply, reply(), state()}.
-spec handle_cast (cast(), state()) -> {stop, normal, state()}.
-spec handle_info (info(), state()) -> {noreply, state()}.
-spec terminate (shutdown, state()) -> any().
-spec code_change (any(), state(), any()) -> {ok, state()}.

init (Options) when is_list(Options) ->
  {ok, CANds} = erleos_utils:get_arg(usir_can_ds, Options, {can_ds, {can, "/dev/can0"}}),
  {ok, Type}  = erleos_utils:get_arg(type, Options),
  ok          = subscribe_to_ds(CANds),
  {ok, open_queues(#state{can_ds = CANds,
                          type   = Type})}.

handle_call (#get_last_reading{}, _From, State) ->
  {reply, State#state.last_reading, State}.

handle_cast (shutdown, State) ->
  {stop, normal, State}.

handle_info (Data, State) when is_list(Data) ->
  {Raw, Erl} = process_data(Data, {[], []}, State),
  spawn(fun () -> emit_data(Raw, State) end),
  {noreply, State#state{last_reading = Erl}}.

terminate (shutdown, State = #state{can_ds = Ds}) ->
  unsubscribe_from_ds(Ds),
  close_queues(State).

code_change (_Vsn, State, _Extra) ->
  %% close all queues
  S1 = close_queues(State),
  %% re-open queues
  S2 = open_queues(S1),
  {ok, S2}.

%%%%%%%%%%%%%%%%%%%% L o c a l  F u n c t i o n s %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec open_queues (state()) -> state().
-spec close_queues (state()) -> state().

open_queues (State) ->
  {ok, SensorCfg} = erleos_utils:get_arg(case State#state.type of
                                           ir -> ir_sensor;
                                           us -> us_sensor
                                         end),
  Queues          = proplists:get_value(output, proplists:get_value(mqueues, SensorCfg)),
  State#state{queues = [mqueue:open(Q) || Q <- Queues]}.
  
close_queues (State) ->
  [mqueue:close(Q) || Q <- State#state.queues],
  State#state{queues = []}.

-spec subscribe_to_ds ({erleos_ds:server_ref(), any()}) -> ok | {error, any()}.
-spec unsubscribe_from_ds ({erleos_ds:server_ref(), any()}) -> ok | {error, any()}.

subscribe_to_ds ({CANSrv, {can, N}}) ->
  erleos_ds:subscribe(CANSrv, {can, N}).

unsubscribe_from_ds ({CANSrv, {can, N}}) ->
  erleos_ds:unsubscribe(CANSrv, {can, N}).

-spec process_data (can_readings(), {[#raw_data{}], readings()}, state()) -> {[#raw_data{}], readings()}. 

process_data ([], Acc, _) ->
  Acc;
process_data ([{Id, Timestamp, Data} | Rest],
              {Acc1, Acc2},
              State = #state{address_base = AB, type = Type}) ->
  X                  = (Id rem AB),
  <<V:16/little, C>> = Data,
  {Data1, Data2}     = if
                         X == 1 andalso Type =:= us -> % US reading
                           {#raw_data{type  = Type,
                                      value = V,
                                      cycle = C,
                                      time  = ?can_ts_to_ms(Timestamp),
                                      id    = Id},
                            #reading{sid   = Id,
                                     ts    = ?can_ts_to_now(Timestamp),
                                     value = V}}; % TODO: Convert the US value and Timestamp to now()
                         X > 1 andalso X < 7 andalso Type =:= ir -> % IR reading
                           {#raw_data{type  = Type,
                                      value = V,
                                      cycle = C,
                                      time  = ?can_ts_to_ms(Timestamp),
                                      id    = Id},
                            #reading{sid   = Id,
                                     ts    = ?can_ts_to_now(Timestamp),
                                     value = V}} % TODO: Convert the IR value and Timestamp to now()
                       end,
  process_data(Rest,
               {[Data1 | Acc1], [Data2 | Acc2]},
               State).

-spec emit_data ([#raw_data{}], state()) -> any().

emit_data (Readings, State = #state{type = Type}) ->
  lists:foreach(fun (#raw_data{id   = Id,
                               type  = RType,
                               time  = Time,
                               value = Value,
                               cycle = Cycle}) when RType =:= Type ->
                  Packet = <<Id:16/little, Time:32/little, Value:16/little, Cycle>>,
                  write_queues(Packet, State)
                end, Readings).

-spec write_queues (binary(), state()) -> any().

write_queues (Packet, #state{queues = Qs}) ->
  [mqueue:send(Q, Packet) || Q <- Qs].

