%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 24-11-2010
%%% @doc Sensor process API.
%%% @end
%%% ==========================================================================
-module (erleos_sensor).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").

%%% ==========================================================================
%%% I n c l u d e d  F i l e s
%%% ==========================================================================

%%%-include_lib ().
-include ("erleos/include/types.hrl").
-include ("erleos/include/proto/sensor.hrl").

%%% ==========================================================================
%%% E x p o r t e d  A P I  F u n c t i o n s
%%% ==========================================================================

-export ([get_last_reading/1, behaviour_info/1]).

%%% ==========================================================================
%%% E x p o r t e d  I n t e rn a l  F u n c t i o n s
%%% ==========================================================================

-export ([]).

%%% ==========================================================================
%%% A P I  F u n c ti o n s
%%% ==========================================================================

behaviour_info (callbacks) ->
  [{init, 1},
   {option, 2},
   {convert_data, 4},
   {terminate, 2}];
behaviour_info (_) ->
  undefined.

%% @doc Gets last recorded scan from given sensor.

-spec get_last_reading (server_ref()) -> [reading()] | timeout. 

get_last_reading (Sensor) ->
  gen_server:call(Sensor, #get_last_reading{}).

%%% ==========================================================================
%%% I n t e r n a l / L o c a l  F u n c t i o n s
%%% ==========================================================================
