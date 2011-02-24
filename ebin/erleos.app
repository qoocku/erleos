{application, erleos,
 [
  {description, "Leonardi Onboard System"},
  {vsn, "1"},
  {registered, [erleos_sup]},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {modules, [erleos_app,
             erleos_sup,
             erleos_sensor,
             erleos_ds,
             erleos_ds_srv,
             erleos_sensor,
             erleos_utils,
             erleos_usir_sensor_srv,
             mqueue,
             mqueue_drv,
             mqueue_srv,
             'CAN',
             'CAN_drv',
             'CAN_msg_router_srv']},                 
  {mod, { erleos_app, []}},
  {env, []}
 ]}.
