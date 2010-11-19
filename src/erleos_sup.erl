%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 19-11-2010
%%% @doc TODO: Add description to erleos_sup
%%% @end
%%% ==========================================================================
-module (erleos_sup).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-behaviour(supervisor).

-export ([start_link/1]).
-export ([init/1]).

%%% ===========================================================================
%%% P u b l i c  A P I
%%% ===========================================================================

start_link (Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%%% ====================================================================
%%% Internal functions
%%% ====================================================================

init([]) ->
  {ok,{{one_for_all,0,1}, []}}.
