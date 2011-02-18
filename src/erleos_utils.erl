%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 25-11-2010
%%% @doc ErlLEOS Utilities.
%%%      
%%%      == Generic Server Utils ==
%%%
%%%      The defined utilities includes:
%%%      <ul>
%%%        <li>starting a server (see {@link start/1}, {@link start/2},
%%%            {@link start_link/1}, {@link start_link/2})</li>
%%%        <li>stopping a server in synced way ({@link sync_stop/1})
%%%            and async way ({@link stop/1})</li>
%%%      </ul>
%%%      Thus, the `gen_server' callback module MUST implement (besides `init/1'
%%%      and `terminate/2') `handle_call(shutdown, _, State)' and
%%%      `handle_cast(shutdown, State)' functions.
%%% @end
%%% ==========================================================================
-module (erleos_utils).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").

%%% ==========================================================================
%%% I n c l u d e d  F i l e s
%%% ==========================================================================

%%-include_lib ().
-include ("types.hrl").

%%% ==========================================================================
%%% E x p o r t e d  A P I  F u n c t i o n s
%%% ==========================================================================

-export ([start/1,
          start/2,
          start_link/1,
          start_link/2,
          stop/1,
          sync_stop/1,
          get_arg/1,
          get_arg/2,
          get_arg/3]).

%%% ==========================================================================
%%% E x p o r t e d  I n t e rn a l  F u n c t i o n s
%%% ==========================================================================

-export ([]).
-export_types ([server_ref/0]).

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

%% @doc Stops the given server and waits for its shutdown. 
%%      Usable mainly in unit tests ;)

-spec sync_stop (server_ref()) -> ok.

sync_stop (Server) ->
  gen_server:call(Server, shutdown).

-type get_arg_result () :: {ok, any()} | undefined.
-type args() :: [{atom(), any()}].

-spec get_arg (atom()) -> get_arg_result().
-spec get_arg (atom(), args() | term()) -> get_arg_result().
-spec get_arg (atom(), args(), term()) -> get_arg_result().         

get_arg (Key) ->
  application:get_env(erleos, Key).

get_arg (Key, Args) when is_list(Args) ->
  case proplists:get_value(Key, Args) of
    undefined -> get_arg(Key);
    Other     -> Other
  end;
get_arg (Key, Def) ->
  case get_arg(Key) of
    undefined -> {ok, Def};
    Other     -> Other
  end.

get_arg (Key, Args, Def) ->
  case get_arg(Key, Args) of
    undefined -> {ok, Def};
    Other     -> Other
  end.

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
