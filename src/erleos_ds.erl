%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 25-11-2010
%%% @doc ErlLEOS Data Source API.
%%% @headerfile "types.hrl"
%%% @headerfile "proto/data_source.hrl"
%%% @end
%%% ==========================================================================
-module (erleos_ds).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").

%%% ==========================================================================
%%% I n c l u d e d  F i l e s
%%% ==========================================================================

%%-include_lib ().
-include ("types.hrl").
-include ("proto/data_source.hrl").

%%% ==========================================================================
%%% E x p o r t e d  A P I  F u n c t i o n s
%%% ==========================================================================

-export ([behavior_info/1,
          subscribe/2,
          subscribe/3,
          subscribe/4,
          unsubscribe/2,
          unsubscribe/3]).

%%% ==========================================================================
%%% E x p o r t e d  I n t e rn a l  F u n c t i o n s
%%% ==========================================================================

-export ([]).

%%% ==========================================================================
%%% A P I  F u n c ti o n s
%%% ==========================================================================

behavior_info (callbacks) ->
  [{init, 1},
   {run, 2},
   {terminate, 1}];
behavior_info (_) ->
  undefined.

-spec subscribe (server_ref(), data_source()) -> ok | {error, undefined}.
-spec subscribe (server_ref(), data_source(), transform_fun()) -> ok | {error, undefined}.
-spec subscribe (server_ref(), data_source(), transform_fun(), receivers()) -> ok | {error, undefined}.

%% @doc Subscribes for data of `DataSource' type on data source `Server'.
%% @equiv subscribe(Server, DataSource, fun (_) -> true end, fun (X) -> X end)

subscribe (Server, DataSource) ->
  subscribe(Server,
			DataSource,
			fun (X) -> X end).

%% @doc Subscribes for data of `DataSource' type on data source `Server'.
%%      The data will be (on server side) transformed using supplied by the
%%      subscriber transform function.
%% @equiv subscribe(Server, DataSource, TransferFun, [self()])

subscribe (Server, DataSource, TransferFun) 
  when (is_pid(Server) orelse is_atom(Server) orelse is_tuple(Server)),
        is_function(TransferFun) ->
  subscribe (Server, DataSource, TransferFun, [self()]);
subscribe (Server, DataSource, Receivers) 
  when (is_pid(Server) orelse is_atom(Server) orelse is_tuple(Server)),
        is_list(Receivers) ->
  subscribe (Server, DataSource, fun (X) -> X end, Receivers).

subscribe (Server, DataSource, TransferFun, Receivers) 
  when (is_pid(Server) orelse is_atom(Server) orelse is_tuple(Server)),
        is_function(TransferFun) ->  
  gen_server:call(Server, #ds_subscribe{ds  = DataSource,
                                        tf  = TransferFun,
                                        rcv = Receivers}).

%% @doc Unsubscribes for data of `DataSource' type on data source `Server'.

-spec unsubscribe (server_ref(), data_source()) -> ok | {error, undefined}.
-spec unsubscribe (server_ref(), data_source(), receiver()) -> ok | {error, undefined}.

unsubscribe (Server, DataSource) when  is_pid(Server) orelse is_atom(Server) orelse is_tuple(Server) ->
  unsubscribe (Server, DataSource, self()).

unsubscribe (Server, DataSource, Receiver)
  when (is_pid(Server) orelse is_atom(Server) orelse is_tuple(Server)),
        (is_pid(Receiver) orelse is_atom(Receiver) orelse is_tuple(Receiver)) ->
  gen_server:call(Server, #ds_unsubscribe{ds  = DataSource,
                                          rcv = Receiver}).

%%% ==========================================================================
%%% I n t e r n a l / L o c a l  F u n c t i o n s
%%% ==========================================================================

