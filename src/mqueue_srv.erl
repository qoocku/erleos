%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2010-11-20
%%% @doc Server for mqueue reading & writing
%%% @end
%%% ==========================================================================
-module (mqueue_srv).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-behavior (gen_server).

-export ([init/1,
          handle_call/3,
          handle_cast/2,
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

init (Args) when is_list(Args) ->
  [QueueName, Receiver, Options]   = Args,
  {MQOptions, MyOptions}           = lists:foldl(fun
                                                   (Item = {Key, _}, {MQo, MyO}) when Key =:= timeout ;
                                                                                      Key =:= receiver ->
                                                     {MQo, [Item|MyO]};
                                                   ({_, _} = Item, _) ->
                                                     exit({badarg, Item});
                                                   (Option, {MQo, MyO}) ->
                                                     {[Option|MQo], MyO}
                                                 end, {[], []}, Options),
  {QueueSize, MaxMsgSize, Options} = mqueue:parse_options(MQOptions),
  {ok, Q} = mqueue_drv:open(QueueName, QueueSize, MaxMsgSize,
                            case lists:member(noblock, Options) of
                              false -> [noblock|Options];
                              true  -> Options
                            end),
  erlang:start_timer(Timeout = proplists:get_value(timeout, MyOptions, 10), self(), tick),
  {ok, #state{queue    = Q,
              timeout  = Timeout,
              receiver = Receiver}}.

handle_call (#options{oper = get}, _From, State = #state{queue = Q}) ->
  {ok, P} = mqueue:props(Q),
  {reply, P, State};
handle_call (#options{oper = set, args = Args}, _From, State) ->
  NS = lists:foldl(fun
                     ({timeout, T}, S)  -> S#state{timeout = T};
                     ({receiver, R}, S) -> S#state{receiver = R}
                   end, State, Args),
  {reply, ok, NS}.

handle_cast ({timeout, _, tick}, State = #state{queue =    Q,
                                                receiver = R,
                                                timeout  = T}) ->
  erlang:start_timer(R, T, tick),
  Reply = case mqueue_drv:recv(Q) of
            {error, eagain} -> 
              noreply;
            {ok, Bin} ->
              R ! #mqueue{queue = Q, msg = Bin},
              noreply;
            {error, Other} ->
              {stop, Other}
          end,
  case Reply of
    noreply        -> {noreply, State};
    {stop, Reason} -> {stop, Reason, State}
  end;
handle_cast (shutdown, State) ->
  {stop, normal, State}.

terminate (Reason, State = #state{queue =Q}) ->
  mqueue_drv:close(Q).

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.
