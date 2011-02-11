%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 25-11-2010
%%% @doc TODO: Add description to erleos_ds_tests
%%% @end
%%% ==========================================================================
-module (erleos_ds_tests).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-compile (export_all).

-include_lib ("eunit/include/eunit.hrl").
-include ("proto/data_source.hrl").

-record (ctx, {servers, mocks}).

setup () ->
  Mock = create_mock_ds_server(),
  {ok, Server} = erleos_utils:start(Mock),
  #ctx{servers = [Server], mocks = [Mock]}.
  
tear_down (#ctx{servers = Servers,
                mocks   = Mocks}) ->
  lists:foreach(fun erleos_utils:sync_stop/1, Servers),
  lists:foreach(fun meck:unload/1, Mocks).


'test simplest subscribe' (#ctx{servers = [S]}) ->
  ?assertEqual(ok, erleos_ds:subscribe(S, ds)).

'test subscribe with a transfer function' (#ctx{servers = [S]}) ->
  ?assertEqual(ok, erleos_ds:subscribe(S, ds,
									   fun (_) -> true end, 
									   fun (X) -> X end)).

'test subscribe with an acceptance function' (#ctx{servers = [S]}) ->
  ?assertEqual(ok, erleos_ds:subscribe(S, ds,
									   fun (_) -> true end, 
									   fun (X) -> X end)).

'test simplest unsubscribe' (#ctx{servers = [S]}) ->
  ?assertEqual(ok, erleos_ds:unsubscribe(S, ds)).

'test simplest subscribe on non-existsing data source' (#ctx{servers = [S]}) ->
  ?assertEqual({error, undefined}, erleos_ds:subscribe(S, nonexistent)).

%% -----------------------------------------------------------------------------
%% @doc Tests Runner.
%% @end
%% -----------------------------------------------------------------------------

all_test_ () ->
  tests_runner:run(fun setup/0, fun tear_down/1, ?MODULE).

%%% ===========================================================================
%%% I n t e r n a l  F u n  c t i o n s
%%% ===========================================================================

create_mock_ds_server () ->
  meck:new(mock_ds_srv),
  lists:foreach(fun ({N,F}) -> meck:expect(mock_ds_srv, N, F) end,
                [{init,        fun ([]) -> {ok, dict:from_list([{ds, []}])} end},
                 {handle_call, fun 
                                   (state, _, D) -> {reply, D, D};
                                   (#ds_subscribe{ds = ds, tf = Tf, rcv = Rcv}, _, D) ->
                                     UF = fun (List) -> [{Rcv, Tf}|List] end,
                                     {reply, ok, dict:update(ds, UF, D)};
                                   (#ds_subscribe{ds = _DS}, _, D) ->
                                     {reply, {error, undefined}, D};
                                   (#ds_unsubscribe{ds = ds, rcv = Rcv}, _, D) ->
                                     UF = fun (List) ->
                                              lists:keydelete(Rcv, 1, List)
                                          end,
                                     {reply, ok, dict:update(ds, UF, [], D)};
                                   (shutdown, _, D) ->
                                     {stop, normal, ok, D}
                               end},
                 {handle_cast, fun (shutdown, D) -> {stop, normal, D} end},
                 {terminate,   fun (_, _) -> ok end}]),
  mock_ds_srv.
