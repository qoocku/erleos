%% Author: leo
%% Created: 2011-02-10
%% Description: TODO: Add description to erleos_usir_sensor_srv
-module (erleos_usir_sensor_srv).
-behavior (gen_server).

-export ([init/1,
		  handle_call/3,
		  handle_cast/2,
		  handle_info/2,
		  terminate/2,
		  code_change/3]).

-include ("proto/sensor.hrl").

-type readings () :: [reading()].
-record (state, {can_ds       = undefined :: atom() | pid() | {node(), atom()},
				 last_reading = []        :: readings()}).

init (Options) when is_list(Options) ->
  DefCANds = case application:get_env(usir_can_ds) of
			   undefined   -> {'CAN_ds', {can, 0}};
			   {ok, Other} -> Other
			 end,
  CANds    = proplists:get_value(can_ds, Options, DefCANds),
  ok       = subscribe_to_ds(CANds),
  {ok, #state{can_ds = CANds}}.

handle_call (#get_last_reading{}, _From, State) ->
  {reply, State#state.last_reading, State}.

handle_cast (shutdown, State) ->
  {stop, normal, State}.

handle_info (_, State) ->
  {norelpy, State}.

terminate (shutdown, #state{can_ds = Ds}) ->
  unsubscribe_from_ds(Ds).

code_change (_Vsn, State, _Extra) ->
  {ok, State}.

%%% ---

subscribe_to_ds ({CANSrv, {can, N}}) ->
  erleos_ds:subscribe(CANSrv, {can, N}).

unsubscribe_from_ds ({CANSrv, {can, N}}) ->
  erleos_ds:unsubscribe(CANSrv, {can, N}).
