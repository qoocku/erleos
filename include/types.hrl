-ifndef (ERLEOS_TYPES_HRL).
-define (ERLEOS_TYPES_HRL, true).

%% @type server_ref() = pid() | atom() | {node(), atom()}. Generic server reference.
-type server_ref() :: pid() | atom() | {node(), atom()}.
  
-endif.