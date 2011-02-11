%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2010-12-20
%%% @doc Router of CAN messages. It routes received data to several processes
%%%      according to the defined rules which are simple mapping from range
%%%      of data source ids to processes. In general it's many-to-many mapping.
%%%
%%% @end
%%% ==========================================================================
-module ('CAN_msg_router_srv').
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").

-export ([init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2]).

-include_lib ("eunit/include/eunit.hrl").
-include ("proto/data_source.hrl").

%%% ==============================================================================
%%% A P I  F u n c t i o n s
%%% ==============================================================================

-type canId()        :: pos_integer().
-type targets()      :: [pid() | atom() | {node(), atom()}].
-type subPair()      :: {canId(), targets()} | {{canId(), canId()}, targets()}.    
-type subscription() :: [subPair()].

%%% ==============================================================================
%%% L o c a l / I n t e r n a l  F u n c t i o ns 
%%% ==============================================================================

-record (state, {tid     = undefined :: ets:tid(),
                 timeout = 50        :: pos_integer(),
                 can_dev = undefined :: any()}).

init (PropList) ->
  DefTabName = case application:get_env('CAN_msg_router_srv_tab_name') of
                 undefined -> 'CAN_msg_router';
                 {ok, Val} -> Val
               end,
  DefTimeout = case application:get_env('CAN_msg_router_timeout') of
                 undefined  -> 50;
                 {ok, Val2} -> Val2
               end,
  DefCanOpts = case application:get_env('CAN_msg_router_dev_opts') of
                 undefined  -> [];
                 {ok, Val3} -> Val3
               end,
  CanPath = case proplists:get_value(can_path, PropList) of
              undefined -> exit(no_can_path_defined);
              Else      -> Else
            end,
  TabName = proplists:get_value(tabname, PropList, DefTabName),
  Timeout = proplists:get_value(timeout, PropList, DefTimeout),
  CanOpts = proplists:get_value(dev_opts, PropList, DefCanOpts),
  Tid     = ets:new(TabName, []),
  {ok, C} = open_can(CanPath, CanOpts),
  erlang:send_after(Timeout, self(), tick),
  {ok, #state{tid = Tid, timeout = Timeout, can_dev = C}}.

handle_call (shutdown, From, State) ->
  NewState = close_can(State),
  gen_server:reply(From, ok),
  {stop, normal, NewState};
handle_call (#ds_subscribe{ds = DSList, rcv = Receivers}, _From, State) ->
  Result = do_subscribe(DSList, State),
  {reply, ok, State}.

handle_cast (shutdown, State) ->
  {stop, normal, State}.

handle_info ({timeout, _, tick}, State = #state{timeout = Timeout}) ->
  erlang:send_after(Timeout, self(), tick),
  {noreply, State};
handle_info ({can, _DevNo, Readings}, State) ->
  NewState = save_reading(Readings, State),
  {noreply, NewState}.

terminate (_Reason, State = #state{tid = Tid}) ->
  close_can(State),
  ets:delete(Tid).

%%% ===============================================================================

do_subscribe ([], _State) ->
  ok;
do_subscribe ([{Ids, Targets}|Rest], State) when is_list(Ids) ->
  lists:foreach(fun (Id) ->
                  ok = do_subscribe([{Id, Targets}], State)
                end, Ids);
do_subscribe ([{Id, Targets}|Rest], State) when is_integer(Id) ->
  ok = subscribe_one_source(Id, Targets, State),
  do_subscribe(Rest, State);
do_subscribe ([{{From, To}, Targets}|Rest], State) ->
  lists:foreach(fun (Id) ->
                  ok = subscribe_one_source(Id, Targets, State)
                end, lists:seq(From, To)),
  do_subscribe(Rest, State).
  
subscribe_one_source (Id, Targets, State = #state{tid = Tid}) ->
  RegisteredAlready = ets:lookup(Tid, Id),
  true = ets:insert(Tid, {Id, RegisteredAlready ++ Targets}),
  ok.
    
open_can (CanPath, CanOpts) ->
  Opts = lists:keydelete(active, 1, CanOpts), % remove unnecessary option
  {ok, Can} = 'CAN':open(CanPath, [{active, self()}] ++ CanOpts).

close_can (State = #state{can_dev = undefined}) ->
  State;
close_can (State = #state{can_dev = Dev}) ->
  'CAN':close(Dev),
  State#state{can_dev = undefined}.

save_reading ([], State) ->
  State;
save_reading ([R={Id, Timestamp, Data}|Rest], State) ->
  ?debugFmt("reading: ~p~n", [R]),
  save_reading(Rest, State).