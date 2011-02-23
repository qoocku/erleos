-ifndef (ERLEOS_DATA_OURCE_HRL).
-define (ERLEOS_DATA_OURCE_HRL, true).

%% @type data_source() = any(). Data source type.
-type data_source()   :: any().

%% @type transform_fun() = fun((any()) -> {ok, any()} | {error, any()}). Function
%%       transforming source data from origin type to subscriber type. 
-type transform_fun() :: fun((any()) -> {ok, any()} | {error, any()}).
-type receiver()  :: pid() | atom() | {node(), atom()}.
-type receivers() :: [receiver()].

-type accept_fun() :: fun ((any()) -> boolean()).

%% @type ds_subscribe() = #ds_subscribe{ds  = data_source(),
%%                                      tf  = transform_fun(),
%%                                      rcv = [pid() | atom() | {node(), atom()}]}. Subscribe message.
-record (ds_subscribe,{ds  :: data_source(),
					             af  :: accept_fun(),
                       tf  :: transform_fun(),
                       rcv :: receivers()}).
-type ds_subscribe() :: #ds_subscribe{}.

%% @type ds_unsubscribe() = #ds_subscribe{ds  = data_source(),
%%                                      rcv = [pid() | atom() | {node(), atom()}]}. Unsubscribe message.
-record (ds_unsubscribe,{ds  :: data_source(),
                         rcv :: receiver()}).
-type ds_unsubscribe() :: #ds_unsubscribe{}.

-endif.
