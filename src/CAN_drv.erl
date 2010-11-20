%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2010-11-20
%%% @doc CAN NIF Driver
%%% @end
%%% ==========================================================================
-module ('CAN_drv').
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-on_load (init/0).

-export([open/1, close/1]).

-define (LIB, "CAN_nif").

init () ->
  erlang:load_nif(filename:join([filename:dirname(code:which(?MODULE)),
                                 "..", "priv", ?LIB]), 0).

open (Port) when is_integer(Port) ->
  {ok, <<>>}.

close (Handle) ->
  0.


