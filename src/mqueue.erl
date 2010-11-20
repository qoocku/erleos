%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 19-11-2010
%%% @doc TODO: Add description to mqueue
%%% @end
%%% ==========================================================================
-module (mqueue).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").

%%% ==========================================================================
%%% I n c l u d e d  F i l e s
%%% ==========================================================================

%%%-include_lib ().
%%%-include ().

%%% ==========================================================================
%%% E x p o r t e d  A P I  F u n c t i o n s
%%% ==========================================================================

-export ([open/1,
          open/2,
          close/1,
          recv/1,
          recv/2,
          send/2,
          send/3,
          props/1]).

%%% ==========================================================================
%%% E x p o r t e d  I n t e rn a l  F u n c t i o n s
%%% ==========================================================================

-export ([]).

%%% ==========================================================================
%%% A P I  F u n c ti o n s
%%% ==========================================================================

-record (queue, {ref = make_ref() :: reference(),
                 hnd}).

open (QueueName) when is_list(QueueName) ->
  open(QueueName, []).

open (QueueName, Options) when is_list(QueueName), is_list(Options) ->
  QueueSize  = case lists:keyfind(size, 1, Options) of
                 {size, Size} -> Size;
                 false        -> 8
               end,
  MaxMsgSize = case lists:keyfind(msgsize, 1, Options) of
                 {msgsize, Size2} -> Size2;
                 _               -> 256
               end,
  Rest       = lists:filter(fun
                             (ValidOpt) when ValidOpt =:= noblock ;
                                              ValidOpt =:= own ->
                                true;
                             (_) ->
                                false
                            end, Options),
  case mqueue_drv:open(QueueName, QueueSize, MaxMsgSize, Rest) of
      {ok, Q} -> {ok, #queue{hnd = Q}};
      Other   -> Other
  end.

send (#queue{hnd = Q}, Binary) when is_binary(Binary) ->
  mqueue_drv:send(Q, Binary, 0).

send (#queue{hnd = Q}, Binary, Priority) when is_binary(Binary), is_integer(Priority) ->
  mqueue_drv:send(Q, Binary, Priority).

recv (#queue{hnd = Q}) ->
  mqueue_drv:recv(Q).

recv (#queue{hnd = Q}, Callback) when is_function(Callback) ->
  case mqueue_drv:recv(Q) of
    {ok, Msg} -> Callback(Msg);
    Other     -> Other
  end.

close (#queue{hnd = Q}) ->
  case mqueue_drv:close(Q) of
    0     -> ok;
    Error -> Error
  end.

props (#queue{hnd = Q}) ->
  {ok, mqueue_drv:props(Q)}.

%%% ==========================================================================
%%% I n t e r n a l / L o c a l  F u n c t i o n s
%%% ==========================================================================

