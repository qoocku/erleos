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
         send/2,
         recv/1,
         recv/2,
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

open (DevicePath) ->
  open (DevicePath, case application:get_env(can_baudrate) of
                      undefined -> 1000000;
                      Other     -> Other
                    end).

open (DevicePath, BaudRate) ->
  open (DevicePath, BaudRate, case application:get_env(can_mask) of
                                undefined -> 0;
                                Other     -> Other
                              end).
  
open (DevicePath, BaudRate, Mask) ->
  case 'CAN_drv':open(DevicePath, BaudRate, Mask) of
    Descriptor when Descriptor > 0 ->
      case 'CAN_drv':set_baudrate(Descriptor) of
        0 -> case 'CAN_drv':set_filter(Descriptor, 0, 0, 0, Mask, 0) of
               0     -> {ok, Descriptor};
               Other -> {error, Other}
             end;
        Other2 -> {error, Other2}
      end;
    ErrorNumber when ErrorNumber < 0 -> {error, ErrorNumber}
  end.

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
    -1003               -> {errno, message_should_be_binary}
  end.

recv (Handle) ->
  recv(Handle, case application:get_env(can_recv_chunk_size) of
                 undefined -> 32;
                 Value     -> Value
               end).

recv (Handle, ChunkSize) ->
  'CAN_drv':recv(Handle, ChunkSize).

close (Handle) ->
  case 'CAN_drv':close(Handle) of
    0            -> ok;
    E when E < 0 -> {error, E}
  end.
  
%%% ==========================================================================
%%% Local Functions
%%% ==========================================================================

