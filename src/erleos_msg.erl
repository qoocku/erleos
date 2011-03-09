%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-03-08
%%% @doc Functions for managment of Erleos overall system messages.
%%% @end
%%% ==========================================================================
-module  (erleos_msg).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-include ("vsn").

-include ("erleos/include/types.hrl").
-include ("erleos/include/proto/robot.hrl").
-include ("erleos/include/proto/usir.hrl").
-include ("erleos/include/proto/adis.hrl").

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([to_binary/1,
          to_term  /2]).

%%% --------------------------------------------------------------------
%%% I n t e r n a l  e x p o r t s
%%% --------------------------------------------------------------------

-export ([]).

%%% --------------------------------------------------------------------
%%% M a c r o s
%%% --------------------------------------------------------------------

%%% --------------------------------------------------------------------
%%% R e c o r d s ,  T y p e s  a n d  S p e c s
%%% --------------------------------------------------------------------


%%% ============================================================================
%%% C l i e n t  A P I / E x p o r t e d  F u n c t i o n s
%%% ============================================================================

%% -----------------------------------------------------------------------------
%% @doc Converts a message to a binary. 
%% @end
%% -----------------------------------------------------------------------------
 
-spec to_binary (any()) -> binary().

-define (DHDR(N, D1), #N{id = Id, time = Ts, D1}).
-define (DHDR(N, D1, D2), #N{id = Id, time = Ts, D1, D2}).
-define (DHDR(N, D1, D2, D3), #N{id = Id, time = Ts, D1, D2, D3}).
-define (DHDR(N, D1, D2, D3, D4), #N{id = Id, time = Ts, D1, D2, D3, D4}).
-define (DHDR(N, D1, D2, D3, D4, D5), #N{id = Id, time = Ts, D1, D2, D3, D4, D5}).

-define (DBIN(D1), <<Id:32/little, Ts:32/little, D1>>).
-define (DBIN(D1, D2), <<Id:32/little, Ts:32/little, D1, D2>>).
-define (DBIN(D1, D2, D3), <<Id:32/little, Ts:32/little, D1, D2, D3>>).
-define (DBIN(D1, D2, D3, D4), <<Id:32/little, Ts:32/little, D1, D2, D3, D4>>).
-define (DBIN(D1, D2, D3, D4, D5), <<Id:32/little, Ts:32/little, D1, D2, D3, D4, D5>>).

to_binary ({VelData, Id, Ts, AX, AY, AZ}) when VelData =:= accel_data orelse VelData =:= angvel_data ->
  ?DBIN(AX:16/little, AY:16/little, AZ:16/little);
to_binary (?DHDR(pos_data, x = X, y = Y, phi = Phi)) ->
  ?DBIN(X:16/little, Y:16/little, Phi:16/little);
to_binary (?DHDR(linvel_data, x = X)) ->
  ?DBIN(X:16/little);
to_binary (?DHDR(factors_data, n = N, z = Z, k = K, kv = KV, kw = KW)) ->
  ?DBIN(N, Z, K:16/little, KV:16/little, KW:16/little);
to_binary (?DHDR(movinfo_data, info = Info)) ->
  ?DBIN(Info);
to_binary (?DHDR(usir_data, value = Value, cycle = Cycle)) ->
  ?DBIN(Value:16/little, Cycle);

to_binary (#msg_heartbeat{pid = Pid, time = Time}) ->
  <<?MSG_HEARTBEAT, Pid:32/little, (to_msec(Time)):16/little>>.


-spec to_term (atom(), binary()) -> any().

to_term (accel_data, ?DBIN(AX:16/little, AY:16/little, AZ:16/little)) -> 
  ?DHDR(accel_data, ax = AX, ay = AY, az = AZ);
to_term (angvel_data, ?DBIN(AX:16/little, AY:16/little, AZ:16/little)) -> 
  ?DHDR(angvel_data, ax = AX, ay = AY, az = AZ);
to_term (pos_data, ?DBIN(X:16/little, Y:16/little, Phi:16/little)) ->
  ?DHDR(pos_data, x = X, y = Y, phi = Phi);
to_term (linvel_data, ?DBIN(X:16/little)) ->
  ?DHDR(linvel_data, x = X);
to_term (factors_data, ?DBIN(N, Z, K:16/little, KV:16/little, KW:16/little)) ->
  ?DHDR(factors_data, n = N, z = Z, k = K, kv = KV, kw = KW);
to_term (movinfo_data, ?DBIN(Info)) ->
  ?DHDR(movinfo_data, info = Info);
to_term (UsirType, ?DBIN(Value:16/little, Cycle)) when UsirType =:= ir orelse UsirType =:= us->
  (?DHDR(usir_data, value = Value, cycle = Cycle))#usir_data{type = UsirType}.


  
%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================

to_msec ({_, Sec, USec}) ->
  Sec * 1000 + USec div 1000;
to_msec (Val) when is_integer(Val) ->
  Val.
