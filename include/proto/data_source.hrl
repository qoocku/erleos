-ifndef (ERLEOS_DATA_OURCE_HRL).
-define (ERLEOS_DATA_OURCE_HRL, true).

%% @type data_source() = any(). Data source type.
-type data_source()   :: any().

%% @type transform_fun() = fun((any()) -> {ok, any()} | {error, any()}). Function
%%       transforming source data from origin type to subscriber type. 
-type transform_fun() :: fun((any()) -> {ok, any()} | {error, any()}).

%% @type ds_subscribe() = #ds_subscribe{ds  = data_source(),
%%                                      tf  = transform_fun(),
%%                                      rcv = pid()}. Subscribe message.
-record (ds_subscribe,{ds  :: data_source(),
                       tf  :: transform_fun(),
                       rcv :: pid()}).
-type ds_subscribe() :: #ds_subscribe{}.

%% @type ds_subscribe() = #ds_subscribe{ds  = data_source(),
%%                                      rcv = pid()}. Unsubscribe message.
-record (ds_unsubscribe,{ds  :: data_source(),
                         rcv :: pid()}).
-type ds_unsubscribe() :: #ds_unsubscribe{}.

-endif.
