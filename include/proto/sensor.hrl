-ifndef (ERLEOS_SENSOR_HRL).
-define (ERLEOS_SENSOR_HRL, true).

-type ts()              :: {non_neg_integer(), 
                            non_neg_integer(), 
                            non_neg_integer()}.
-type sensor_id()       :: any().              
-type sensor_reading(T) :: [T].                

-record (get_last_reading, {}).
-record (reading, {sid   :: sensor_id(),
                   ts    :: ts(),
                   value :: sensor_reading(any())}).

-type reading          () :: #reading{}.
-type can_reading      () :: {integer(), {integer(), integer()}, binary()}.
-type get_last_reading () :: #get_last_reading{}.

-type distance()         :: non_neg_integer().
-type orientation()      :: non_neg_integer(). 
-type distance_reading() :: {orientation(), distance()}. 

-define (can_ts_to_ms(Ts), begin
                             {S, Us} = Ts,
                             S * 1000 + Us div 1000
                           end).

-define (can_ts_to_now(Ts), begin
                              {S, Us} = Ts,
                              Secs    = S + Us div 1000,
                              {Secs * 1000000, Secs, Secs div 1000000}                             
                            end).


-endif.