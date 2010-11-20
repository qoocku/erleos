-ifndef (MQUEUE_PROTO_HRL).
-define (MQUEUE_PROTO_HRL, true).

-record (options, {oper = get :: [get|set],
                   args = []  :: [any()]}).
-record (mqueue, {source     :: pid(),
                  msg = <<>> :: binary()}).
-endif.