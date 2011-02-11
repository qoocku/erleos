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
-include_lib ("stdlib/include/qlc.hrl").
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
                 rtid    = undefined :: ets:tid(),
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
  Tid     = ets:new(TabName, [set]),
  RTid    = ets:new(list_to_atom("R_" ++ atom_to_list(TabName)), [set]),
  {ok, C} = open_can(CanPath, CanOpts),
  erlang:send_after(Timeout, self(), tick),
  {ok, #state{tid = Tid, rtid = RTid, timeout = Timeout, can_dev = C}}.

handle_call (shutdown, From, State) ->
  NewState = do_terminate(State),
  gen_server:reply(From, ok),
  {stop, normal, NewState};
handle_call (#ds_subscribe{ds = DSList, rcv = Receivers}, _From, State) ->
  Result = do_subscribe(DSList, Receivers, State),
  {reply, ok, State}.

handle_cast (shutdown, State) ->
  {stop, normal, State}.

handle_info (tick, State = #state{timeout = Timeout, rtid = RTid}) ->
  Q = qlc:q([Rcv ! Data || {Rcv, Data} <- ets:table(RTid)]),
  qlc:e(Q),
  erlang:send_after(Timeout, self(), tick),
  {noreply, State};
handle_info ({can, _DevNo, Readings}, State) ->
  NewState = save_reading(Readings, State),
  {noreply, NewState}.

terminate (_Reason, State) ->
  do_terminate(State).

%%% ===============================================================================

do_subscribe ([], _, _State) ->
  ok;
do_subscribe ({From, To}, Targets, State) ->
  lists:foreach(fun (Id) ->
                  ok = do_subscribe(Id, Targets, State)
                end, lists:seq(From, To)),
  ok;
do_subscribe (Id, Targets, State) when is_integer(Id) ->
  subscribe_one_source(Id, Targets, State);
do_subscribe (Ids, Targets, State) when is_list(Ids) ->
  lists:foreach(fun (Id) ->
                  ok = do_subscribe(Id, Targets, State)
                end, Ids),
  ok.
  
subscribe_one_source (Id, Targets, State = #state{tid = Tid, rtid = RTid}) ->
  [{Id, RegisteredAlready}] = case ets:lookup(Tid, Id) of
                                []    -> [{Id, []}];
                                Other -> Other
                              end,
  true = ets:insert(Tid, {Id, RegisteredAlready ++ Targets}),
  lists:foldl(fun (Tgt, ok) ->
                  true = ets:insert(RTid, {Tgt, []}),
                  ok
              end, ok, Targets).
    
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
save_reading ([R={Id, Timestamp, Data}|Rest], State = #state{tid = Tid, rtid = RTid}) ->
  [{Id, Targets}]  = case ets:lookup(Tid, Id) of
                       []     -> [{Id, []}];
                       Others -> Others
                     end,
  lists:foreach(fun (Tgt) ->
                  [{Tgt, Readings}] = ets:lookup(RTid, Tgt),
                  true              = ets:update_element(RTid, Tgt, {2, [R|Readings]})
                end, Targets),
  save_reading(Rest, State).

do_terminate (State = #state{tid = Tid, rtid = RTid}) ->
  NS = close_can(State),
  lists:foreach(fun 
                  (undefined) -> ok ;
                  (T)         -> ets:delete(T)
                end, [RTid, Tid]),
  NS#state{tid = undefined, rtid = undefined}.
