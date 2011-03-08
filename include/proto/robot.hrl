%%% Author: Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% Since: 2011-03-08
%%% Synopsis: Protocol definitions and like for communication with underlaying mobile robot driver.

-ifndef (ERLEOS_PROTO_ROBOT_HRL).
-define (ERLEOS_PROTO_ROBOT_HRL, true).

-define (PING_TMS, 16#0001).
-define (CONTROL_MODE, 16#0001).
-define (ROBOT_START_STOP, 16#0001).
-define (POWER_EN_DIS, 16#0001).
-define (SET_ROBOT_BASE, 16#0001).
-define (SET_ROBOT_W_RADIUS, 16#0001).
-define (SET_WHEEL_W_MAX, 16#0001).
-define (SET_WHEEL_W, 16#0001).
-define (SET_ROBOT_POS_X, 16#0001).
-define (SET_ROBOT_POS_Y, 16#0001).
-define (SET_ROBOT_ORI, 16#0001).
-define (GET_ROBOT_POS_X, 16#010C).
-define (GET_ROBOT_POS_Y, 16#010D).
-define (GET_ROBOT_ORI, 16#010E).
-define (GET_ROBOT_TASK_STATUS, 16#010F).

-record (ping_tms, {}).
-record (control_mode, {}).
-record (robot_start_stop, {}).
-record (power_en_dis, {}).
-record (set_robot_base, {}).
-record (set_robot_w_radius, {}).
-record (set_wheel_w_max, {}).
-record (set_wheel_w, {}).
-record (set_robot_pos_x, {}).
-record (set_robot_pos_y, {}).
-record (set_robot_ori, {}).
-record (get_robot_pos_x, {}).
-record (get_robot_pos_y, {}).
-record (get_robot_ori, {}).
-record (get_robot_task_status, {}).

-define (SET_V_W_MAX, 16#0010).
-define (SET_V_W, 16#0011).

-record (set_v_w_max, {}).
-record (set_v_w, {}).

-define (SET_VFO_PARAMS, 16#0012).
-define (SET_WAY_POINT_POS_X, 16#0013).
-define (SET_WAY_POINT_POS_Y, 16#0014).
-define (SET_WAY_POINT_ORI, 16#0015).
-define (SET_WAY_POINT_SIGN, 16#0016).
-define (SET_WAY_POINT_EPSILON_ETA, 16#0017).
-define (SET_WAY_POINT_VEL, 16#0018).
-define (SET_WAY_POINT_AC_DC, 16#0019).
-define (SET_AVOIDANCE, 16#001A).
-define (WAY_POINT_CANCEL, 16#001B).
-define (PATH_CANCEL, 16#001C).
-define (SET_W_P_SEQUENCE, 16#001D).

-define (MSG_LOCAL_CONTROL,, 16#001E). % fake msg type
-define (MSG_OK, 16#001F).
-define (MSG_ERROR, 16#0020).
-define (MSG_CLOSE, 16#0021).
-define (MSG_HEARTBEAT, 16#0022). % thread_id, timestamp
-define (MSG_AUTO, 16#0023). % fake msg type
-define (MSG_TEXT, 16#0024). % one text parameter filename
-define (MSG_PROGRAM_START, 16#0025).
-define (MSG_PROGRAM_STOP, 16#0026).
-define (MSG_PROGRAM_PAUSE, 16#0027). % pause
-define (MSG_FROM_AUTO, 16#8000). % auto msgtype modificator.
-define (GET_BIT_IDENTIFICATOR, 16#0100).

-record (set_vfo_params, {kp = 0 :: integer(),
                          k1 = 0 :: integer()}).
-record (msg_heartbeat, {pid  = undefined :: any(),
                         time = now()     :: ts()}).


-endif.