-ifndef (ERLEOS_TYPES_HRL).
-define (ERLEOS_TYPES_HRL, true).

%% @type server_ref() = pid() | atom() | {node(), atom()}. Generic server reference.
-type server_ref() :: pid() | atom() | {node(), atom()}.

%% @type ts() = {non_neg_integer(), 
%%               non_neg_integer(), 
%%               non_neg_integer()} | non_neg_integer(). Type of a timestamp (could be now() or milliseconds).
-type ts()              :: {non_neg_integer(), 
                            non_neg_integer(), 
                            non_neg_integer()} | non_neg_integer().

-endif.