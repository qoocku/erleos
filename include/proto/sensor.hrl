-ifndef (ERLEOS_SENSOR_HRL).
-define (ERLEOS_SENSOR_HRL, true).

-type ts()              :: {non_neg_integer(), 
                            non_neg_integer(), 
                            non_neg_integer()}.
-type sensor_id()       :: any().              
-type sensor_reading(T) :: [T].                

-record (reading, {sid   :: sensor_id(),
                   ts    :: ts(),
                   value :: sensor_reading(any())}).
-type reading () :: #reading{}.

-type distance()         :: non_neg_integer().
-type orientation()      :: non_neg_integer(). 
-type distance_reading() :: {orientation(), distance()}. 

-endif.