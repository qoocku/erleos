-ifndef (ERLEOS_PROTO_ADIS_HRL).
-define (ERLEOS_PROTO_ADIS_HRL, true).

-include ("proto/sensor.hrl").

-type now () :: ts().

-record (accel_data, {id    = 0 :: sensor_id(),
                      time  = now() :: now(),
                      ax    = 0 :: integer(),
                      ay    = 0 :: integer(),
                      az    = 0 :: integer()}).

-record (angvel_data, {id    = 0 :: sensor_id(),
                       time  = now() :: now(),
                       ax    = 0 :: integer(),
                       ay    = 0 :: integer(),
                       az    = 0 :: integer()}).
  
-record (pos_data, {id    = 0 :: sensor_id(),
                    time  = now() :: now(),
                    x    = 0 :: integer(),
                    y    = 0 :: integer(),
                    phi  = 0 :: integer()}).

-record (linvel_data, {id    = 0 :: sensor_id(),
                       time  = now() :: now(),
                       x    = 0 :: integer()}).

-record (factors_data, {id    = 0 :: sensor_id(),
                        time  = now() :: now(),
                        n     = 0 :: byte(),
                        z     = 0 :: byte(),
                        k     = 0 :: integer(),
                        kv    = 0 :: integer(),
                        kw    = 0 :: integer()}).

-record (movinfo_data, {id    = 0 :: sensor_id(),
                        time  = now() :: now(),
                        info  = 0 :: byte()}).

-endif.
