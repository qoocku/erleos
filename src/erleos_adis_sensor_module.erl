%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-02-28
%%% @doc TODO: Add description to erleos_us_sensor_module
%%% @end
%%% ==========================================================================
-module  (erleos_adis_sensor_module).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>").
-include ("vsn").

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([]).

-include_lib ("eunit/include/eunit.hrl").
-include ("proto/sensor.hrl").
-include ("proto/adis.hrl").

%%% --------------------------------------------------------------------
%%% I n t e r n a l  e x p o r t s
%%% --------------------------------------------------------------------

-export ([init/1, option/2, convert_data/4, terminate/2]).

%%% --------------------------------------------------------------------
%%% M a c r o s
%%% --------------------------------------------------------------------

%%% --------------------------------------------------------------------
%%% R e c o r d s ,  T y p e s  a n d  S p e c s
%%% --------------------------------------------------------------------

-type stype() :: accel | angvel | pos | linvel | factors | movinfo.
-record (state, {type = undefined :: stype()}).
-type state() :: #state{}.

%%% ============================================================================
%%% C l i e n t  A P I / E x p o r t e d  F u n c t i o n s
%%% ============================================================================

init (Options) when is_list(Options) ->
  {ok, Type} = erleos_utils:get_arg(type, Options),
  {ok, #state{type = Type}}.

option (router, _) ->
  adis_can_router;
option (sensor, #state{type = T}) ->
  list_to_atom(atom_to_list(T) ++ "_sensor");
option (type, #state{type = T}) ->
  T;
option (data_id, #state{type = T}) ->
  list_to_atom(atom_to_list(T) ++ "_id").

convert_data (Id, Timestamp, Data, #state{type = T}) ->
  to_data(T, Id, Timestamp, Data).

terminate (_, _) -> ok.

%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================

-compile ([{inline, [{to_data, 4}]}]).

to_data (accel, Id, Timestamp, Data) ->
  <<AX:16/little, AY:16/little, AZ:16/little>> = Data,
  {#accel_data{ax    = AX,
               ay    = AY,
               az    = AZ,
               time  = ?can_ts_to_ms(Timestamp),
               id    = Id}, #raw_data{sid   = Id,
                                      ts    = ?can_ts_to_ms(Timestamp),
                                      value = Data}};
to_data (angvel, Id, Timestamp, Data) ->
  <<AX:16/little, AY:16/little, AZ:16/little>> = Data,
  {#angvel_data{ax    = AX,
                ay    = AY,
                az    = AZ,
                time  = ?can_ts_to_ms(Timestamp),
                id    = Id}, #raw_data{sid   = Id,
                                       ts    = ?can_ts_to_ms(Timestamp),
                                       value = Data}};
to_data (pos, Id, Timestamp, Data) ->
  <<X:16/little, Y:16/little, Phi:16/little>> = Data,
  {#pos_data{x    = X,
             y    = Y,
             phi  = Phi,
             time = ?can_ts_to_now(Timestamp),
             id   = Id}, #raw_data{sid   = Id,
                                   ts    = ?can_ts_to_ms(Timestamp),
                                   value = Data}};
to_data (linvel, Id, Timestamp, Data) ->
  <<X:16/little>> = Data,
  {#linvel_data{x    = X,
                time = ?can_ts_to_now(Timestamp),
                id   = Id}, #raw_data{sid   = Id,
                                      ts    = ?can_ts_to_ms(Timestamp),
                                      value = Data}};
to_data (factors, Id, Timestamp, Data) ->
  <<N, Z, K:16/little, KV:16/little, KW:16/little>> = Data,
  {#factors_data{n    = N,
                 z    = Z,
                 k    = K,
                 kv   = KV,
                 kw   = KW,
                 time = ?can_ts_to_now(Timestamp),
                 id   = Id}, #raw_data{sid   = Id,
                                       ts    = ?can_ts_to_ms(Timestamp),
                                       value = Data}};
to_data (movinfo, Id, Timestamp, <<Data>>) ->
  {#movinfo_data{info = Data,
                 time  = ?can_ts_to_now(Timestamp),
                 id    = Id}, #raw_data{sid   = Id,
                                        ts    = ?can_ts_to_ms(Timestamp),
                                        value = <<Data>>}}.