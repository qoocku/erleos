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
         open/3,
         open/4,
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
        end, self()).

open (DevicePath) when is_list(DevicePath) ->
  open (DevicePath, self(), case application:get_env(can_baudrate) of
                      undefined -> 1000000;
                      Other     -> Other
                    end).

open (DevicePath, BaudRate) when is_list(DevicePath),
                                 is_integer(BaudRate),
                                 BaudRate > 0 ->
  open (DevicePath, self(), BaudRate,
        case application:get_env(can_mask) of
          undefined -> 0;
          Other     -> Other
        end);
open (DevicePath, Receiver) when is_list(DevicePath) ->
  open (DevicePath, Receiver, case application:get_env(can_baudrate) of
                              undefined -> 1000000;
                              Other     -> Other
                            end).

  
open (DevicePath, BaudRate, Mask) when is_list(DevicePath), 
                                       is_integer(BaudRate),
                                       BaudRate > 0,
                                       is_integer(Mask),
                                       Mask  > -1 ->
  open(DevicePath, self(), BaudRate, Mask).

open (DevicePath, Receiver, BaudRate, Mask) when is_list(DevicePath),
                                                 is_integer(BaudRate),
                                                 BaudRate > 0,
                                                 is_integer(Mask),
                                                 Mask  > -1 ->
  open_can(DevicePath, Receiver, BaudRate, Mask).

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

open_can (DevicePath, Pid, BaudRate, Mask) ->  
  case 'CAN_drv':open(DevicePath, Pid) of
    Descriptor when Descriptor > 0 ->
      case 'CAN_drv':set_baudrate(Descriptor, BaudRate) of
        0 -> case 'CAN_drv':set_filter(Descriptor, 0, 0, 0, Mask, 0) of
               0 ->
                 case 'CAN_drv':listener(Pid, 64, 5000000) of
                   -1004 ->
                     {error, thread_could_not_be_created};
                   Other when is_pid(Other) ->
                     {ok, Descriptor}
                 end;
               Other ->
                 {error, Other}
             end;
        Other2 -> {error, Other2}
      end;
    ErrorNumber when ErrorNumber < 0 -> {error, ErrorNumber}
  end.
