%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 24-11-2010
%%% @doc Sensor process API.
%%% @end
%%% ==========================================================================
-module (erleos_sensor).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").

%%% ==========================================================================
%%% I n c l u d e d  F i l e s
%%% ==========================================================================

%%%-include_lib ().
-include ("types.hrl").
-include ("proto/sensor.hrl").

%%% ==========================================================================
%%% E x p o r t e d  A P I  F u n c t i o n s
%%% ==========================================================================

-export ([start/1,
          start/2,
          start_link/1,
          start_link/2,
          stop/1,
          get_last_reading/1]).

%%% ==========================================================================
%%% E x p o r t e d  I n t e rn a l  F u n c t i o n s
%%% ==========================================================================

-export ([]).

%%% ==========================================================================
%%% A P I  F u n c ti o n s
%%% ==========================================================================

-type start_options() :: [{args, any()} | {name, atom()} | {globalname, atom()}].
-type start_result()  :: {ok, pid()} | {error, term()}.
-spec start (module()) -> {ok, pid()} | {error, term()}.
-spec start_link (module()) -> {ok, pid()} | {error, term()}.
-spec start (module(), start_options()) -> start_result().
-spec start_link (module(), start_options()) -> start_result().

%% @doc Starts standalone local sever using the given implementation module.
%% @equiv start(Mod, [])

start (Mod) when is_atom(Mod) ; is_tuple(Mod) ->
  start(Mod, []).

%% @doc Starts linked local server using the given implementation module.
%% @equiv start_link(Mod, [])

start_link (Mod)  when is_atom(Mod) ; is_tuple(Mod) ->
  start_link(Mod, []).

%% @doc Starts standalone server using the given implementation module.
%%      The options are:
%%      <ul>
%%         <li>`{name, Name}' - for locally registered server;</li>
%%         <li>`{globalname, Name}' - for globally registered server;</li>
%%         <li>`{args, Args}' - argument for `Mod:init/1' function.</li>
%%      </ul>
%%      If both names are specified then the global name has precedence.

start (Mod, Options) when (is_atom(Mod) orelse is_tuple(Mod)),
                           is_list(Options) ->
  start_server(Mod, Options, start).

%% @doc Starts standalone server using the given implementation module.
%%      For options see {@link start/2}.

start_link (Mod, Options) when (is_atom(Mod) orelse is_tuple(Mod)),
                           is_list(Options) ->
  start_server(Mod, Options, start_link).

%% @doc Stops the given server.

-spec stop (server_ref()) -> ok.

stop (Server) ->
  gen_server:cast(Server, shutdown).

%% @doc Gets last recorded scan from given sensor.

-spec get_last_reading (server_ref()) -> {ok, reading()}. 

get_last_reading (Sensor) ->
  gen_server:call(Sensor, get_last_reading).

%%% ==========================================================================
%%% I n t e r n a l / L o c a l  F u n c t i o n s
%%% ==========================================================================

start_server (Mod, Options, StartFun) ->
  LocalRef  = proplists:get_value(name, Options),
  GlobalRef = proplists:get_value(globalname, Options),
  Args      = case proplists:get_value(args, Options) of
                  undefined -> [];
                  Other     -> Other
                end,
  StartArgs = case {LocalRef, GlobalRef} of
                {undefined, undefined} ->
                  [Mod, Args, []];
                {A, undefined} when is_atom(A) ->
                  [{local, A}, Mod, Args, []];
                {undefined, A} when is_atom(A) ->
                  [{global, A}, Mod, []]
              end,
  apply(gen_server, StartFun, StartArgs).
