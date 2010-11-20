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

-export ([hello/0,
          open/4,
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

hello () ->
  exit(nif_library_not_loaded).

open (Name, QueueSize, MaxMsgSize, Options) ->
  exit(nif_library_not_loaded).

close (Handle) ->
  exit(nif_library_not_loaded).

recv (Handle) ->
  exit(nif_library_not_loaded).

send (Handle, Binary, Priority) when is_binary(Binary), is_integer(Priority) ->
  exit(nif_library_not_loaded).

props (Handle) ->
  exit(nif_library_not_loaded).

%%% ==========================================================================
%%% I n t e r n a l / L o c a l  F u n c t i o n s
%%% ==========================================================================

init () ->  
  erlang:load_nif(filename:join([filename:dirname(code:which(?MODULE)),
                                 "..", "priv",
                                 "mqueue_nif"]), 0).
