%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 25-11-2010
%%% @doc EUnit Tests Runner
%%% @end
%%% ==========================================================================
-module (tests_runner).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").

-include_lib ("eunit/include/eunit.hrl").

%%% ==========================================================================
%%% E x p o r t e d  A P I  F u n c t i o n s
%%% ==========================================================================

-export ([run/3, run/4]).

%%% ==========================================================================
%%% A P I  F u n c ti o n s
%%% ==========================================================================

run (Setup, Cleanup, Module) ->
  run (Setup, Cleanup, "test ", Module).

run (Setup, Cleanup, Prefix, Module) ->
  {foreach,
   Setup,
   Cleanup,
   [
    fun (C) ->
        [{atom_to_list(F), fun () -> Module:F(C) end}]
    end || 
    {F, 1} <- lists:filter(fun
                             ({E, 1}) ->
                             lists:prefix(Prefix, atom_to_list(E));
                             (_) ->
                             false
                           end, Module:module_info(exports))
   ]}.


%%% ==========================================================================
%%% I n t e r n a l / L o c a l  F u n c t i o n s
%%% ==========================================================================

