%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2010-11-21
%%% @doc CAN user API.
%%% @end
%%% ==========================================================================
-module ('CAN').
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").

-export([open/0,
         open/1,
         open/2,
         send/2,
         recv/1,
         recv/2,
         recv/3,
         close/1]).

%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

-type handle() :: 'CAN_drv':handle().

open () ->
  open (case application:get_env(can_device_path) of
          undefined -> "/dev/can0";
          Other -> Other
        end).

open (DevicePath) when is_list(DevicePath) ->
  open (DevicePath, []).

open (DevicePath, Options) when is_list(DevicePath),
                                 is_list(Options) ->
  DefBRate = case application:get_env(can_baudrate) of
               undefined   -> 1000000;
               Other1 when
                 is_integer(Other1),
                 Other1 > 0 -> Other1
             end, 
  DefMask  = case application:get_env(can_mask) of
               undefined    -> [0, 0, 0, 0, 0];
               Other2 when
                 is_list(Other2),
                 length(Other2) == 5 -> Other2
             end,
  DefRaw   = case application:get_env(can_raw_mode) of
               undefined -> 0;
               true      -> 1;
               false     -> 0
             end,
  DefChunk = case application:get_env(can_read_chunk_size) of
               undefined    -> 128;
               Other3 when
                 is_integer(Other3),
                 Other3 > 0 -> Other3
             end,
  DefTout  = case application:get_env(can_read_chunk_size) of
               undefined    -> 2500000; % 2.5 ms = 2.5*10^6 ns
               Other4 when
                 is_integer(Other4),
                 Other4 >= 0 -> Other4
             end,
  BaudRate = proplists:get_value(baudrate, Options, DefBRate),
  Mask     = proplists:get_value(mask, Options, DefMask),
  Raw      = proplists:get_value(raw, Options, DefRaw),
  Receiver = proplists:get_value(active, Options, undefined),
  Chunks   = proplists:get_value(chunk_size, Options, DefChunk),
  Timeout  = proplists:get_value(timeout, Options, DefTout),
  open_can(DevicePath, Receiver, BaudRate, Mask, Raw, Chunks, Timeout).

-spec send (handle(), [{pos_integer(), binary()}]) ->
         {ok, {non_neg_integer(), non_neg_integer()}} | {error, term()}.

send (Handle, Ms) ->
  Length    = length(Ms),
  case 'CAN_drv':send(Handle, Ms) of
    Valid = 
      {Length, TotalSize} -> case lists:foldl(fun ({_, Msg}, N) -> 
                                                    N + byte_size(Msg)
                                              end, 0, Ms) of
                               TotalSize -> {ok, Valid};
                               Almost    -> {error, {partial_transfer, Almost}}
                             end;
    Errornous = {_, _}  -> {error, {partial_transfer, Errornous}};
    -1000               -> {error, some_item_is_not_a_tuple};
    -1001               -> {error, tuple_item_should_have_two_elements};
    -1002               -> {errno, target_id_should_be_integer};
    -1003               -> {errno, message_should_be_binary};
    -1004               -> {errno, cannot_create_thread};
    -1005               -> {errno, message_too_long}
  end.

recv (Handle) ->
  recv(Handle, case application:get_env(can_recv_chunk_size) of
                 undefined -> 32;
                 Value     -> Value
               end).

recv (Handle, ChunkSize) ->
  'CAN_drv':recv(Handle, ChunkSize, 0).

recv (Handle, ChunkSize, Timeout) ->
  'CAN_drv':recv(Handle, ChunkSize, Timeout).

close (Handle) ->
  case 'CAN_drv':close(Handle) of
    0            -> ok;
    E when E < 0 -> {error, E}
  end.
  
%%% ==========================================================================
%%% Local Functions
%%% ==========================================================================

open_can (DevicePath, Pid, BaudRate, Mask, Raw, ChunkSize, Timeout) ->  
  case 'CAN_drv':open(DevicePath, Raw) of
    C when C >= 0 ->
      case 'CAN_drv':set_baudrate(C, BaudRate) of
        0 -> case apply('CAN_drv', set_filter, [C] ++ Mask) of
               0 ->
                 set_receiver(C, Pid, ChunkSize, Timeout);
               Other ->
                 {error, Other}
             end;
        Other2 -> {error, Other2}
      end;
    ErrorNumber when ErrorNumber < 0 -> {error, ErrorNumber}
  end.

set_receiver (_, undefined, _, _) ->
  ok;
set_receiver (Handle, Pid, ChunkSize, Timeout) ->
  case 'CAN_drv':listener(Handle, Pid, ChunkSize, Timeout) of
    -1004 ->
      {error, thread_could_not_be_created};
    Other when is_pid(Other) ->
      {ok, Handle}
  end.
