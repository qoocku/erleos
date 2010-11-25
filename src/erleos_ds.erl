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
          unsubscribe/2]).

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

%% @doc Subscribes for data of `DataSource' type on data source `Server'.
%% @equiv subscribe(Server, DataSource, fun (X) -> X end)

subscribe (Server, DataSource) ->
  subscribe(Server, DataSource, fun (X) -> X end).

%% @doc Subscribes for data of `DataSource' type on data source `Server'.
%%      The data will be (on server side) transformed using supplied by the
%%      subscriber transform function.
%% @equiv subscribe(Server, DataSource, fun (X) -> X end)

subscribe (Server, DataSource, TransferFun) ->
  gen_server:call(Server, #ds_subscribe{ds  = DataSource,
                                        tf  = TransferFun,
                                        rcv = self()}).

%% @doc Unsubscribes for data of `DataSource' type on data source `Server'.

unsubscribe (Server, DataSource) ->
  gen_server:call(Server, #ds_unsubscribe{ds  = DataSource,
                                          rcv = self()}).

%%% ==========================================================================
%%% I n t e r n a l / L o c a l  F u n c t i o n s
%%% ==========================================================================

