%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 19-11-2010
%%% @doc TODO: Add description to new_file
%%% @end
%%% ==========================================================================
-module (mqueue_tests).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").

-include_lib ("eunit/include/eunit.hrl").

-record (ctx, {}).

setup () ->
  ok = application:start(erleos),
  #ctx{}.
  
tear_down (#ctx{} = Ctx) ->
  application:stop(erleos),
  ok.

hello_test () ->
  ?assertEqual("Hello world!", mqueue_drv:hello()).