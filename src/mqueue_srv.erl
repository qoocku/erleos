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

-record (state, {queue        :: mqueue:mq(),
                 timeout  = 0 :: non_neg_integer(),
                 receiver     :: pid() | atom()}).

init (Args) when is_list(Args) ->
  {ok, #state{}}.

handle_call (#options{oper = get}, _From, State = #state{queue = Q}) ->
  {ok, P} = mqueue:props(Q),
  {reply, P, State};
handle_call (#options{oper = set, args = Args}, _From, State) ->
  NS = lists:foldl(fun
                     ({timeout, T}, S)  -> S#state{timeout = T};
                     ({receiver, R}, S) -> S#state{receiver = R}
                   end, State, Args),
  {reply, ok, NS}.


handle_cast ({timeout, _, tick}, State = #state{queue = Q, receiver = R}) ->
  Reply = case mqueue:recv(Q) of
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
  end.

terminate (Reason, State = #state{queue =Q}) ->
  mqueue:close(Q).

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.
