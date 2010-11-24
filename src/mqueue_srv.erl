%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2010-11-20
%%% @doc Server for mqueue reading and writing
%%% @end
%%% ==========================================================================
-module (mqueue_srv).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-behavior (gen_server).

-export ([init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3]).

-include ("proto/mqueue.hrl").

%%% ==========================================================================
%%% B e h a v i o r  A P I
%%% ==========================================================================

-type receiver() :: pid() | atom() | {node, atom()}.
-record (state, {queue        :: mqueue:mq(),
                 timeout  = 0 :: non_neg_integer(),
                 receiver     :: receiver()}).
-type state()     :: #state{}.
-type init_args() :: [string()        |
                      receiver()      |
                      {node(), atom()}|
                      mqueue:mqueue_options() | 
                      [{timeout, pos_integer()}]].

-spec init (init_args()) -> {ok, state()}.

init (Args) when is_list(Args) ->
  [QueueName, Receiver, Options]   = Args,
  {MQOptions, MyOptions}           = lists:foldl(fun
                                                   (Item = {Key, _}, {MQo, MyO}) when Key =:= timeout ->
                                                     {MQo, [Item|MyO]};
                                                   ({_, _} = Item, _) ->
                                                     exit({badarg, Item});
                                                   (Option, {MQo, MyO}) ->
                                                     {[Option|MQo], MyO}
                                                 end, {[], []}, Options),
  {QueueSize, MaxMsgSize, Rest} = mqueue:parse_options(MQOptions),
  {ok, Q} = mqueue_drv:open(QueueName, QueueSize, MaxMsgSize,
                            case lists:member(noblock, Rest) of
                              false -> [noblock|Rest];
                              true  -> Rest
                            end),
  erlang:start_timer(Timeout = proplists:get_value(timeout, MyOptions, 10), self(), tick),
  {ok, #state{queue    = Q,
              timeout  = Timeout,
              receiver = Receiver}}.

handle_call (#options{oper = get}, _From, State = #state{queue    = Q,
                                                         timeout  = T,
                                                         receiver = R}) ->
  P = mqueue_drv:props(Q),
  {reply, [{timeout, T}, {receiver, R}] ++ P, State};
handle_call (#options{oper = set, args = Args}, _From, State) ->
  NS = lists:foldl(fun
                     ({timeout, T}, S)  -> S#state{timeout = T};
                     ({receiver, R}, S) -> S#state{receiver = R}
                   end, State, Args),
  {reply, ok, NS}.

handle_cast (shutdown, State) ->
  {stop, normal, State}.

handle_info ({timeout, _, tick}, State = #state{queue =    Q,
                                                receiver = R,
                                                timeout  = T}) ->
  Reply = case mqueue_drv:recv(Q) of
            {error, eagain} ->             
              noreply;
            {ok, Bin} ->
              R ! #mqueue{source = self(), msg = Bin},
              noreply;
            {error, Other} ->
              error_logger:error_report([{mqueue_srv, self()}, {error, Other}]),
              {stop, Other}
          end,
  erlang:start_timer(T, self(), tick),
  case Reply of
    noreply        -> {noreply, State};
    {stop, Reason} -> {stop, Reason, State}
  end.

terminate (_Reason, #state{queue =Q}) ->
  mqueue_drv:close(Q).

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.
