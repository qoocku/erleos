%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-02-10
%%% @doc Callback module for USIR sensor actor.

-module (erleos_sensor_srv).
-author ("Damian T. Dobroczy\\'snki <qoocku@gmail.com>").
-behavior (gen_server).
-include ("vsn").

-export ([init/1,
		      handle_call/3,
		      handle_cast/2,
		      handle_info/2,
		      terminate/2,
		      code_change/3]).

-export ([encode_mqueue_packet/3,
          decode_mqueue_packet/1]).
-compile ([{inline, [{encode_mqueue_packet, 3},
                     {decode_mqueue_packet, 1}]}]).

-include_lib ("eunit/include/eunit.hrl").
-include ("erleos/include/proto/data_source.hrl").
-include ("erleos/include/proto/sensor.hrl").

-type readings    () :: [reading()].
-type can_readings() :: [can_reading()].
-type cinfo       () :: {module(), any()}.
-record (state, {can_ds       = undefined   :: atom() | pid() | {node(), atom()},
                 ids          = []          :: list(),
                 address_base = 0           :: integer(),
				         last_reading = []          :: readings(),
                 queues       = []          :: [any()],
                 cmod         = {undefined,
                                 undefined} :: cinfo()}).
-type state()    :: #state{}.
-type initArg()  :: {usir_can_ds, any()} | {type, ir | us}. 
-type initArgs() :: [initArg()].
-type call()     :: #get_last_reading{} | shutdown.
-type reply()    :: readings().
-type cast()     :: shutdown.
-type info()     :: [can_reading()].

-spec init (initArgs()) -> {ok, state()}.
-spec handle_call (call(), any(), state()) -> {reply, reply(), state()}.
-spec handle_cast (cast(), state()) -> {stop, normal, state()}.
-spec handle_info (info(), state()) -> {noreply, state()}.
-spec terminate (shutdown, state()) -> any().
-spec code_change (any(), state(), any()) -> {ok, state()}.

encode_mqueue_packet (Id, Time, Value) ->
  <<Id:16/little, Time:32/little, Value/binary>>.

decode_mqueue_packet (<<Id:16/little, Time:32/little, Rest/binary>>) ->
  {Id, Time, Rest}.

init (Options) when is_list(Options) ->
  {ok, ModInf} = erleos_utils:get_arg(cmod, Options),
  {ok, Mod}    = erleos_utils:get_arg(mod, ModInf),
  {ok, ModArg} = erleos_utils:get_arg(args, ModInf),
  {ok, CState} = Mod:init(ModArg),
  {ok, CANRt}  = erleos_utils:get_arg(Mod:option(router, CState), Options),
  CanDev       = 'CAN_msg_router_srv':server_id(begin 
                                                  {ok, V} = erleos_utils:get_arg(candev, CANRt),
                                                  V
                                                end),         
  {ok, Ids}    = erleos_utils:get_arg(Mod:option(data_id, CState), CANRt),
  ok           = subscribe_to_ds(CanDev, Ids),
  {ok, open_queues(#state{can_ds = CanDev,
                          ids    = Ids,
                          cmod   = {Mod, CState}})}.

handle_call (shutdown, From, State) ->
  gen_server:reply(From, ok),
  {stop, normal, State};
handle_call (#get_last_reading{}, _From, State) ->
  {reply, State#state.last_reading, State}.

handle_cast (shutdown, State = #state{can_ds = CANSrv, ids = Ids}) ->
  unsubscribe_from_ds(CANSrv, Ids),
  {stop, normal, State}.

handle_info (#ds_data{readings = Data}, State) ->
  {Erl, Raw} = process_data(Data, {[], []}, State),
  spawn(fun () -> emit_data(Raw, State) end),
  {noreply, State#state{last_reading = Erl}}.

terminate (Reason, State = #state{can_ds = Ds, ids = Ids, cmod = {Mod, CState}}) ->
  unsubscribe_from_ds(Ds, Ids),
  close_queues(State),
  Mod:terminate(Reason, CState).

code_change (_Vsn, State, _Extra) ->
  %% close all queues
  S1 = close_queues(State),
  %% re-open queues
  S2 = open_queues(S1),
  {ok, S2}.

%%%%%%%%%%%%%%%%%%%% L o c a l  F u n c t i o n s %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec open_queues (state()) -> state().
-spec close_queues (state()) -> state().

open_queues (State = #state{cmod = {Mod, CState}}) ->
  {ok, SensorCfg} = erleos_utils:get_arg(Mod:option(sensor, CState)),
  Queues          = proplists:get_value(output, proplists:get_value(mqueues, SensorCfg)),
  State#state{queues = [begin 
                          {ok, Q} = mqueue:open(Name, [{size, 256}, noblock]),
                          Q
                        end || Name <- Queues]}.
  
close_queues (State) ->
  [mqueue:close(Q) || Q <- State#state.queues],
  State#state{queues = []}.

-spec subscribe_to_ds (erleos_ds:server_ref(), any()) -> ok | {error, any()}.
-spec unsubscribe_from_ds (erleos_ds:server_ref(), any()) -> ok | {error, any()}.

subscribe_to_ds (CANSrv, Ids) ->
  erleos_ds:subscribe(CANSrv, Ids).

unsubscribe_from_ds (CANSrv, Ids) ->
  erleos_ds:unsubscribe(CANSrv, Ids).

-spec process_data (can_readings(), {[#raw_data{}], readings()}, state()) -> {[#raw_data{}], readings()}. 

process_data ([], Acc, _) ->
  Acc;
process_data ([{Id, Timestamp, Data} | Rest],
              {Acc1, Acc2},
              State = #state{cmod = {Mod, CState}}) ->
  {Data1, Data2}     = Mod:convert_data(Id, Timestamp, Data, CState),
  process_data(Rest,
               {[Data1 | Acc1], [Data2 | Acc2]},
               State).

-spec emit_data ([#raw_data{}], state()) -> any().

emit_data (Readings, State) ->
  NS = lists:foldl(fun (#raw_data{sid   = Id,
                                   ts    = Time,
                                   value = Value}, NotSent) ->
                  Packet = encode_mqueue_packet(Id, Time, Value),
                  case write_queues(Packet, State) of
                    []    -> NotSent;
                    Other -> [{Packet, Other} | NotSent] 
                  end
                end, [], Readings),
  case NS of
    [] -> ok;
    Lst -> error_logger:error_report([{mqueue, not_sent}, {count, length(Lst)}, {packets, Lst}])
  end.

-spec write_queues (binary(), state()) -> any().

write_queues (Packet, #state{queues = Qs}) ->
  lists:filter(fun
                 (ok) -> false;
                 ({error, _}) -> true
               end, [mqueue:send(Q, Packet) || Q <- Qs]).

