%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2010-11-20
%%% @doc CAN NIF Driver API.
%%%      The low-level driver API provides the following operations:
%%%      <ul>
%%%         <li>opening a CAN port in read/write mode (underneath using POSIX
%%%            `open(...)' function);</li>
%%%         <li>receiving CAN frames returning `raw' binary or list of Erlang objects
%%%             describing the frames contents;</li>
%%%         <li>sending a list of frames through the CAN port;</li>
%%%         <li>closing the port.</li>
%%%      </ul>
%%%      The driver may be used in <emph>passive</emph> or <emph>active</emph>
%%%      mode. The passive one gives possibility to recevive frames "on demand"
%%%      (at the moment of the receiving operation call) with some optional timeout.
%%%      The active mode ends up with using a thread (OS thread not Erlang process)
%%%      which listens on the port (with optional timeout) and reads incoming 
%%%      frames which will be sent to the given process. Both modes may return
%%%      "raw" or "high-level" frames contents.
%%% @end
%%% ==========================================================================
-module ('CAN_drv').
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-on_load (init/0).

-export([open/2,
         set_baudrate/2,
         set_filter/6,
         send/2,
         recv/3,
         close/1,
         listener/4,
         translate_errno/1,
         check_tuple/2,
         create_sample_device/1]).

-define (LIB, "CAN_nif").

%% @doc Initialises the NIF library.

init () ->
  erlang:load_nif(filename:join([filename:dirname(code:which(?MODULE)),
                                 "..", "priv", ?LIB]), 0).

-type handle() :: any().

-spec open  (string(), 1|0) -> handle() | integer().
-spec set_baudrate (handle(), pos_integer()) -> integer().
-spec set_filter (handle(),
                   non_neg_integer(),
                   non_neg_integer(),
                   non_neg_integer(),
                   non_neg_integer(),
                   non_neg_integer())  -> integer().
-spec send (handle(), [{non_neg_integer(), binary()}]) -> 
         neg_integer() | {non_neg_integer(), non_neg_integer()}.
-spec recv (handle(), pos_integer(), non_neg_integer()) -> [{non_neg_integer(), binary()}] | integer().
-spec listener (handle(), pid(), pos_integer(), pos_integer()) -> pid() | integer().
-spec close (handle()) -> 0 | {error, integer()}.

%% @doc Opens CAN port given as the system device path. The driver is expected
%%      to return "raw" binary frames iff `Mode' is `1' or Erlang lists iff `Mode'
%%      is `1'.

open (DevicePath, Raw) when is_list(DevicePath),
                            Raw == 0 orelse Raw == 1 ->
  case Raw of
    0 -> <<>>;
    1 -> Raw
  end.

%% @doc Sets the port reading and writing baud rate. The rate should be greater than `0'.

set_baudrate (Handle, BaudRate) when Handle =/= undefined,
                                     BaudRate > 0 ->
  0.

set_filter (Handle, Flags, QueueId, Cob, Id, Mask) 
  when Handle =/= undefined, 
       is_integer(Flags),
       is_integer(QueueId),
       is_integer(Cob),
       is_integer(Id),
       is_integer(Mask) ->
  0.

%% @doc Sends frames given as list of tuples `{Target, Data}'.
%%      The binary given as `Data' should not exceeds 8 bytes.
%%      The target is a frame id. The returned value may be
%%      a tuple describing the total number of sent bytes (the 1st item)
%%      and total sum of data bytes sent (the 2nd item).
%% @spec (Handle, OutputFrames) -> Status | neg_integer()
%% where OutputFrames = [{pos_integer(), binary()}],
%%       Status       = {pos_integer(), pos_integer()}

send (Handle, Ms) when Handle =/= undefined, is_list(Ms) ->
  case Handle of
    <<>> -> {0,  0};
    _    -> -1000
  end.

%% @doc Receives passively frames. The function may hang
%%      with the given `Timeout' (the value `0' puts the
%%      reading operation in non-block mode). The frames
%%      will be read in packets containing `ChunkSize'
%%      frames.

recv (Handle, ChunkSize, Timeout) when ChunkSize > 0,
                                       Timeout >= 0-> 
  case Handle of
    <<>> -> [{0, <<>>}];
    _    -> 0
  end.

%% @doc Closes the port. If a receiving thread has been created
%%      and still runs it will be stopped and destroyed before
%%      the port is shut down.

close (Handle) ->
  case Handle of
    <<>> -> 0;
    _    -> {error, 0}
  end.

%% @doc Puts the driver in <emph>active</emph> mode. A special
%%      thread is created which tries to read the CAN port with
%%      the given timeout (see {@link recv}). The received frames
%%      are sent to the `Receiver' process in a form specified
%%      during opening the port (se {@link open}). The function
%%      may be called more than once -- only one thread is 
%%      guaranteed to be created. The result is the previous
%%      receiver's pid, `0' if the function is called for the 
%%      1st time or negative integer indicating an error.

listener (Handle, Receiver, ChunkSize, Timeout) when is_pid(Receiver),
                                                     ChunkSize > 0,
                                                     Timeout > 0->
  case Handle of
    <<>> -> self();
    _    -> -1004
  end.

%% @doc Translates an error number to human-readable atom or string.

translate_errno (ErrNo) when is_integer(ErrNo) ->
  'error';
translate_errno (_) ->
  badarg.

check_tuple (A, B) -> 0.

create_sample_device (Path) ->
  {ok, F} = file:open(Path, [write]),
  ok      = file:close(F),
  C = open(Path, 0),
  {_, _} = send(C, [{I, list_to_binary(lists:seq(0, I rem 8))} || I <- lists:seq(0, 255)]),
  close(C).
