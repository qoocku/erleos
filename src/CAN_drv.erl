%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2010-11-20
%%% @doc CAN NIF Driver API.
%%% @end
%%% ==========================================================================
-module ('CAN_drv').
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-on_load (init/0).

-export([open/1,
         set_baudrate/2,
         set_filter/6,
         send/2,
         recv/2,
         close/1,
         translate_errno/1]).

-define (LIB, "CAN_nif").

init () ->
  erlang:load_nif(filename:join([filename:dirname(code:which(?MODULE)),
                                 "..", "priv", ?LIB]), 0).

-type handle() :: any().

-spec open  (string()) -> handle() | integer().
-spec set_baudrate (handle(), pos_integer()) -> integer().
-spec set_filter (handle(),
                   non_neg_integer(),
                   non_neg_integer(),
                   non_neg_integer(),
                   non_neg_integer(),
                   non_neg_integer())  -> integer().
-spec send (handle(), [{non_neg_integer(), binary()}]) -> 
         neg_integer() | {non_neg_integer(), non_neg_integer()}.
-spec recv (handle(), pos_integer()) -> [{non_neg_integer(), binary()}] | integer().
-spec close (handle()) -> 0 | {error, integer()}.

open (DevicePath) when is_list(DevicePath) ->
  <<>>.

set_baudrate (Handle, BaudRate) when Handle =/= undefined,
                                     BaudRate > 0 ->
  0.

set_filter (Handle, Flags, QueueId, Cob, Id, Mask) 
  when Handle =/= undefined, 
       is_integer(Flags),
       is_integer(QueueId),
       is_integer(Cob),
       is_integer(Id),
       is_integer(Mask) ->
  0.

send (Handle, Ms) when Handle =/= undefined, is_list(Ms) ->
  case Handle of
    <<>> -> {0,  0};
    _    -> -1000
  end.

recv (Handle, ChunkSize) when ChunkSize > 0 -> 
  case Handle of
    <<>> -> [{0, <<>>}];
    _    -> 0
  end.

close (Handle) ->
  case Handle of
    <<>> -> 0;
    _    -> {error, 0}
  end.

translate_errno (ErrNo) when is_integer(ErrNo) ->
  'error';
translate_errno (_) ->
  badarg.

