%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-05-29
%%% @doc Low level Sick Driver functions.
%%% @end
%%% ==========================================================================
-module  (sick_drv).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-on_load (init/0).
-include ("vsn").

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([open/0,
          configure/1,
          start/1,
          read_stream/1,
          get_current_scan/1,
          listener/2,
          close/1]).

%%% --------------------------------------------------------------------
%%% I n t e r n a l  e x p o r t s
%%% --------------------------------------------------------------------

-export ([]).

%%% --------------------------------------------------------------------
%%% M a c r o s
%%% --------------------------------------------------------------------

%%% --------------------------------------------------------------------
%%% R e c o r d s ,  T y p e s  a n d  S p e c s
%%% --------------------------------------------------------------------

%%% ============================================================================
%%% C l i e n t  A P I / E x p o r t e d  F u n c t i o n s
%%% ============================================================================

-define (LIB, "sick_nif").

%% @doc Initialises the NIF library.

init () ->
  erlang:load_nif(filename:join([filename:dirname(code:which(?MODULE)),
                                 "..", "priv", ?LIB]), 0).

open () ->
  <<>>.

close (Handle) ->
  ok.

configure (Handle) ->
  case Handle of
    <<>> -> 0;
    _    -> -1
  end.

start (Handle) ->
  case Handle of
    <<>> -> 0;
    _    -> -1
  end.

read_stream (Handle) ->
  case Handle of
    <<>> -> 0;
    _    -> -1
  end.

get_current_scan (_Handle) ->
  <<>>.

listener (Handle, Pid) when is_pid(Pid) ->
  case Handle of
    <<>> -> Pid;
    _    -> 0
  end.

%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================

