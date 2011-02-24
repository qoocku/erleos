%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2010-12-20
%%% @doc Router of CAN messages. It routes received data to several processes
%%%      according to the defined rules which are simple mapping from range
%%%      of data source ids to processes. In general it's many-to-many mapping.
%%%  
%%%      === The implementation ===
%%% 
%%%      The server uses ETS tables to store target-to-ids and id-to-reading maps.
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

-export ([server_id/1]).

-include_lib ("eunit/include/eunit.hrl").
-include_lib ("stdlib/include/qlc.hrl").
-include ("proto/data_source.hrl").
-include ("proto/sensor.hrl").

%%% ==============================================================================
%%% A P I  F u n c t i o n s
%%% ==============================================================================

-record (state, {tid     = undefined :: ets:tid() | undefined,
                 rtid    = undefined :: ets:tid() | undefined,
                 timeout = 50        :: pos_integer(),
                 can_dev = undefined :: any()}).
-type state() :: #state{}.

server_id (DevPath) when is_list(DevPath) ->
  list_to_atom(DevPath).

%% --------------------------------------------------------------------------------
%% @doc Initialises a server state. `PropList' is a property list which allows to pass
%%      the following properties:
%%
%%      * `can_path' - a CAN device path
%%      * `dev_opts' - arguments for {@link //erleos/CAN:open/2}
%%
%% @end
%% --------------------------------------------------------------------------------

-type initArgs() :: [{can_path, string()}].
-spec init (initArgs()) -> {ok, state()}.

init (PropList) ->
  DefTabName = case application:get_env('CAN_msg_router_srv_tab_name') of
                 undefined -> 'CAN_msg_router';
                 {ok, Val} -> Val
               end,
  DefTimeout = case application:get_env('CAN_msg_router_timeout') of
                 undefined  -> proplists:get_value(timeout, PropList, 50);
                 {ok, Val2} -> Val2
               end,
  DefCanOpts = case application:get_env('CAN_msg_router_dev_opts') of
                 undefined  -> [];
                 {ok, Val3} -> Val3
               end,
  CanPath    = case proplists:get_value(can_path, PropList) of
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
  {ok, #state{tid     = Tid,
              rtid    = RTid,
              timeout = Timeout,
              can_dev = C}}.

%% --------------------------------------------------------------------------------
%% @doc Handles a message call. The recognizable messages are:
%%
%%      * `shutdown' - synchronically stops the server
%%      * `ds_subscribe{}' - subscribes a subscriber to be notified about a 
%%        CAN device reading
%%      * `ds_unsubscribe{}' - unsubscribes a subscriber from the specific list
%%
%% @end
%% --------------------------------------------------------------------------------

-type call() :: registered | shutdown | ds_subscribe() | ds_unsubscribe().
-spec handle_call (call(), any(), state()) -> {reply | stop, normal | ok, state()}.

handle_call (registered, _From, State) ->
  {reply, qlc:e(qlc:q([R || R <- ets:table(State#state.tid)])), State};
handle_call (shutdown, From, State) ->
  NewState = close_can(State),
  gen_server:reply(From, ok),
  {stop, normal, NewState};
handle_call (#ds_subscribe{ds = DSList, rcv = Receivers}, _From, State) ->
  Result = do_subscribe(DSList, Receivers, State),
  {reply, Result, State};
handle_call (#ds_unsubscribe{ds = DSList, rcv = Receivers}, _From, State) ->
  Result = do_unsubscribe(DSList, Receivers, State),
  {reply, Result, State}.

%% --------------------------------------------------------------------------------
%% @doc Handles a message cast. The recognizable messages are:
%%
%%      * `shutdown' - asynchronicly stops the server
%%
%% @end
%% --------------------------------------------------------------------------------

-type cast() :: shutdown.
-spec handle_cast (cast(), state()) -> {stop, normal, state()}.

handle_cast (shutdown, State) ->  
  {stop, normal, close_can(State)}.

%% --------------------------------------------------------------------------------
%% @doc Handles non-generic message. The recognizable messages are:
%%
%%      * `tick' - the internal timeout signal to send to the subscribers notifications
%%        about last readings
%%      * `{can, CANDevPath, [{CANId, Timestamp, Data}, ...]}' - the real CAN readings
%%        coming from the internal reading thread.
%%
%% @end
%% --------------------------------------------------------------------------------

-type info() :: tick | {can, string(), can_reading()}.
-spec handle_info (info(), state()) -> {noreply, state()}.

handle_info (tick,State = #state{timeout = Timeout, rtid = RTid, tid = Tid}) ->
    notify_subscribers(RTid, Tid), 
    erlang:send_after(Timeout,self(),tick), 
    {noreply,State};
handle_info ({can,_DevPath,Readings},State) ->
    NewState = save_reading(Readings,State), 
    {noreply,NewState}.

%% --------------------------------------------------------------------------------
%% @doc Terminates the server.
%% @end
%% --------------------------------------------------------------------------------

-spec terminate (any(), state()) -> any().

terminate (_Reason, State) ->
  do_terminate(State).


%%% ==============================================================================
%%% L o c a l / I n t e r n a l  F u n c t i o ns 
%%% ==============================================================================

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

do_unsubscribe ([], _, _State) ->
  ok;
do_unsubscribe ({From, To}, Targets, State) ->
  lists:foreach(fun (Id) ->
                  ok = do_unsubscribe(Id, Targets, State)
                end, lists:seq(From, To)),
  ok;
do_unsubscribe (Id, Targets, State) when is_integer(Id) ->
  unsubscribe_one_source(Id, Targets, State);
do_unsubscribe (Ids, Targets, State) when is_list(Ids) ->
  lists:foreach(fun (Id) ->
                  ok = do_unsubscribe(Id, Targets, State)
                end, Ids),
  ok.
 
subscribe_one_source (Id, Targets, #state{tid = Tid, rtid = RTid}) ->
  % Id -> Targets mapping update
  [{Id, RegisteredAlready}] = case ets:lookup(Tid, Id) of
                                []    -> [{Id, []}]; % non-existent yet mapping
                                Other -> Other
                              end,
  true = ets:insert(Tid, {Id, RegisteredAlready ++ Targets}),
  true = ets:insert(RTid, {Id, undefined, undefined}),
  ok.

unsubscribe_one_source (Id, Targets, #state{tid = Tid}) ->
  % Id -> Targets mapping update
  [{Id, RegisteredAlready}] = case ets:lookup(Tid, Id) of
                                []    -> [{Id, []}]; % non-existent yet mapping
                                Other -> Other
                              end,
  true = ets:insert(Tid, {Id, RegisteredAlready -- Targets}),
  ok.

open_can (CanPath, CanOpts) ->
  Opts       = lists:keydelete(active, 1, CanOpts), % remove unnecessary option
  {ok, _Can} = 'CAN':open(CanPath, [{active, self()}] ++ Opts).

close_can (State = #state{can_dev = undefined}) ->
  State;
close_can (State = #state{can_dev = Dev}) ->
  'CAN':close(Dev),
  State#state{can_dev = undefined}.

save_reading ([], State) ->
  State;
save_reading ([R|Rest], State = #state{rtid = RTid}) ->
  ets:insert(RTid, R),
  save_reading(Rest, State).

notify_subscribers (RTid, Tid) ->
    Q = qlc:q([R || R = {_, Ts, _} <- ets:table(RTid), Ts =/= undefined]), 
    % assign readings to the targets
    D = qlc:fold(fun (R = {Id, _, _}, Acc) ->
                   lists:foldl(fun ({_Id, Targets}, Acc0) ->
                                 lists:foldl(fun (Target, Acc1) ->
                                               try 
                                                 dict:update(Target, fun (Old) -> [R| Old] end, Acc1)
                                               catch
                                                 _:badarg -> dict:store(Target, [R], Acc1)
                                               end
                                             end, Acc0, Targets)
                               end, Acc, ets:lookup(Tid, Id))
                 end, dict:new(), Q), 
    % notify the targets
    lists:foreach(fun ({Target, Readings}) ->
                    Target ! #ds_data{readings = Readings, ds = self()}
                  end, dict:to_list(D)), 
    % clear the readings table if 
    ets:delete_all_objects(RTid).

do_terminate (State = #state{tid = Tid, rtid = RTid}) ->
  NS = close_can(State),
  lists:foreach(fun 
                  (undefined) -> ok ;
                  (T)         -> ets:delete(T)
                end, [RTid, Tid]),
  NS#state{tid = undefined, rtid = undefined}.
