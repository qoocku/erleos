%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 19-11-2010
%%% @doc TODO: Add description to mqueue_drv
%%% @end
%%% ==========================================================================
-module (mqueue_drv).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-on_load (init/0).

%%% ==========================================================================
%%% I n c l u d e d  F i l e s
%%% ==========================================================================

%%%-include_lib ().
%%%-include ().

%%% ==========================================================================
%%% E x p o r t e d  A P I  F u n c t i o n s
%%% ==========================================================================

-export ([open/4,
          close/1,
          recv/1,
          send/3,
          props/1]).

%%% ==========================================================================
%%% E x p o r t e d  I n t e rn a l  F u n c t i o n s
%%% ==========================================================================

-export ([]).

%%% ==========================================================================
%%% A P I  F u n c ti o n s
%%% ==========================================================================

-type handle() :: any().
-type options() :: [pid()|own].

-spec open (string(), pos_integer(), pos_integer(), options()) -> {ok, handle()}.
-spec close (handle()) -> ok.
-spec recv (handle()) -> {ok, binary()} | {error, term()}.
-spec send (handle(), binary(), non_neg_integer()) -> ok | {error, term()}.
-spec props(handle()) -> {ok, [noblock|own]}.

open (Name, QueueSize, MaxMsgSize, Options) when is_list(Name),
                                                 QueueSize > 0,
                                                 MaxMsgSize > 0,
                                                 is_list(Options) ->
  {ok, <<>>}.

close (Handle) when Handle =/= undefined ->
  ok.

recv (Handle) when Handle =/= undefined ->
  case Handle of
    <<>> -> {ok, <<>>};
    Error = {error, _} -> Error
  end.

send (Handle, Binary, Priority) when Handle =/= undefined,
                                     is_binary(Binary),
                                     is_integer(Priority),
                                     Priority > -1 ->
  ok.

props (Handle) when Handle =/= undefined ->
  {ok, [own]}.

%%% ==========================================================================
%%% I n t e r n a l / L o c a l  F u n c t i o n s
%%% ==========================================================================

init () ->  
  erlang:load_nif(filename:join([filename:dirname(code:which(?MODULE)),
                                 "..", "priv",
                                 "mqueue_nif"]), 0).
