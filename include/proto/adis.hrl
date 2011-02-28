-ifndef (ERLEOS_PROTO_ADIS_HRL).
-define (ERLEOS_PROTO_ADIS_HRL, true).

-include ("proto/sensor.hrl").

-type now () :: ts().

-record (accel_data, {ax    = 0 :: integer(),
                      ay    = 0 :: integer(),
                      az    = 0 :: integer(),
                      time  = now() :: now(),
                      id    = 0 :: sensor_id()}).

-record (angvel_data, {ax    = 0 :: integer(),
                       ay    = 0 :: integer(),
                       az    = 0 :: integer(),
                       time  = now() :: now(),
                       id    = 0 :: sensor_id()}).
  
-record (pos_data, {x    = 0 :: integer(),
                    y    = 0 :: integer(),
                    phi  = 0 :: integer(),
                    time = now() :: now(),
                    id   = 0 :: sensor_id()}).

-record (linvel_data, {x    = 0 :: integer(),
                       time = now() :: now(),
                       id   = 0 :: sensor_id()}).

-record (factors_data, {n    = 0 :: byte(),
                        z    = 0 :: byte(),
                        k    = 0 :: integer(),
                        kv   = 0 :: integer(),
                        kw   = 0 :: integer(),
                        time = now() :: now(),
                        id   = 0 :: sensor_id()}).

-record (movinfo_data, {info  = 0 :: byte(),
                        time  = now() :: now(),
                        id    = 0 :: sensor_id()}).

-endif.
