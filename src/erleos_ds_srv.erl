%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 25-11-2010
%%% @doc TODO: Add description to erleos_ds_srv
%%% @end
%%% ==========================================================================
-module (erleos_ds_srv).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-behaviour(gen_server).

%%% ==========================================================================
%%% I n c l u d e d  F i l e s
%%% ==========================================================================
-include_lib ("eunit/include/eunit.hrl").
-include ("proto/data_source.hrl").

%%% ========================================================================== 
%%% A P I  E x p o r t s
%%% ==========================================================================
-export([]).

%%% ==========================================================================
%%% g e n _ s e r v er  C a l l b a c k s
%%% ==========================================================================
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%% ==========================================================================
%%% S e r v e r  F u n c t i o n s
%%% ==========================================================================

-record (state, {tbl :: ets:tid() | atom()}).
-type state()      :: #state{}.
-type initArgs()   :: list(). %% TODO init args
-type stopReason() :: normal. %% TODO stop reasons
-type call()       :: shutdown       |
                      ds_subscribe() |
                      ds_unsubscribe().
-type cast()       :: shutdown.  %% TODO casts
-type info()       :: any().  %% TODO infos

-spec init (initArgs()) ->
  {ok, state()}            |
  {ok, state(), timeout()} |
  ignore                   |
  {stop, stopReason()}.                           

-spec handle_call (call(), any(), state()) -> 
  {reply, Reply::any(), state()}              |
  {reply, Reply::any(), state(), timeout()}   |
  {noreply, state()}                          |
  {noreply, state(), timeout()}               |
  {stop, stopReason(), Reply::any(), state()} | 
  {stop, stopReason(), state()}.

-spec handle_cast (cast(), state()) ->
  {noreply, state()}            |
  {noreply, state(), timeout()} |
  {stop, stopReason(), state()}.
  
-spec handle_info (info(), state()) ->
  {noreply, state()}            |
  {noreply, state(), timeout()} |
  {stop, stopReason(), state()}.

-spec terminate (stopReason(), state()) -> any().

-spec code_change (any(), state(), any()) -> {ok, state()}.

%% ---------------------------------------------------------------------------
%% @doc Initializes the server process.
%% @end
%% ---------------------------------------------------------------------------

init ([Name, ModsDef]) when is_atom(Name), is_list(ModsDef) ->
  Tbl = case ets:info(Name) of
          undefined -> ets:new(Name, [named_table]);
          _         -> Name
        end,
  T   = fun ({Mod, Args}) ->
            {ok, Ds = {_DsType, _DsId}, Env} = Mod:init(Args),
            ets:insert(Name, {{source, Ds}, {Mod, Env}}),
            Mod:run(self(), Env)
        end,
  lists:foreach(T, ModsDef),  
  {ok, #state{tbl = Tbl}}.

%% ---------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%% ---------------------------------------------------------------------------

handle_call (#ds_subscribe{ds = Ds, tf = Tf, rcv = Rcv},
             _From, State = #state{tbl = T}) ->
  case ets:lookup(T, {source, Ds}) of
    [{{source, Ds}, _}] ->
      Receivers = case ets:lookup(T, Ds) of
                    [{Ds, Rs}] -> Rs;
                    []         -> []
                  end,
      ets:insert(T, {Ds, [{Rcv, Tf} | Receivers]}),
      {reply, ok, State};
    [] ->
      {reply, {error, undefined}, State}
  end;
handle_call (#ds_unsubscribe{ds = Ds, rcv = Rcv},
             _From, State = #state{tbl = T}) ->
  case ets:lookup(T, {source, Ds}) of
    [{{source, Ds}, _}] ->
      Receivers = case ets:lookup(T, Ds) of
                    [{Ds, Rs}] -> Rs;
                    []         -> []
                  end,
      case lists:keydelete(Rcv, 1, Receivers) of
        []    -> ets:delete(T, Ds);
        Other -> ets:insert(T, {Ds, Other})
      end,
      {reply, ok, State};
    [] ->
      {reply, {error, undefined}, State}
  end;
handle_call (shutdown, _From, State) ->
  {stop, normal, ok, State}.

%% ---------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%% ---------------------------------------------------------------------------

handle_cast (shutdown, State) ->
  {stop, normal, State}.

%% ---------------------------------------------------------------------------
%% @doc Handling all non call/cast messages (typically coming from REAL data sources).
%%      Used to accept real data from real devices.
%%      For example `DsType` would be `tcp` and `DsId` would be `{IP, Port}`
%%      and so on.
%% @end
%% ---------------------------------------------------------------------------

handle_info (Msg = {DsType, DsId, _Data}, State = #state{tbl = T}) ->
  % get the data source description (`Ds`)
  [{{source, _}, {Mod, Env}}] = ets:lookup(T, {source, Ds = {DsType, DsId}}),
  % get the signal reception function
  SFun                        = fun (Data, Env1) -> Mod:init_reception(DsType, DsId, Data, Env1) end,
  % the signal reception
  Env0 = lists:foldl(fun ({RList, Tf}, Acc) ->
                           [SFun(R, Tf(Msg), Acc) || R <- RList]
                     end,
                     Env,
                     case ets:lookup(T, Ds) of
                       []          -> [];
                       [{Ds, Rs}] -> Rs 
                     end),
  % the end of the joy
  Env1 = Mod:finish_reception(DsType, DsId, Env0),
  % save the callback module state
  true = ets:insert(T, {{source, {DsType, DsId}}, {Mod, Env1}}),
  {noreply, State}.

%% ---------------------------------------------------------------------------
%% @doc Shutdown the server.
%% @end
%% ---------------------------------------------------------------------------

terminate (_Reason, #state{tbl = T}) ->
  ets:foldl(fun 
              ({{source, _}, {Mod, Env}}, _) ->
                Mod:terminate(Env);
              (_, _) ->
                ok
            end, ok, T),
  ets:delete(T),
  ok.

%% --------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%% --------------------------------------------------------------------

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

%%% ====================================================================
%%% I n t e r n a l / L o c a l  F u n c t i o n s
%%% ====================================================================

