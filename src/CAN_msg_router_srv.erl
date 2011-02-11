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
          terminate/2]).

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

-record (state, {tid     = ets:tid()}).

init (PropList) ->
  TabName = proplists:get_value(tabname, PropList),
  Tid     = ets:new(TabName, []),
  {ok, #state{tid = Tid}}.

handle_call (#ds_subscribe{ds = DSList, rcv = Receivers}, _From, State) ->
  Result = do_subscribe(DSList, State),
  {reply, ok, State}.

handle_cast (shutdown, State) ->
  {stop, normal, State}.

terminate (_Reason, #state{tid = Tid}) ->
  ets:delete(Tid).

%%% ===============================================================================

do_subscribe ([], _State) ->
  ok;
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
    