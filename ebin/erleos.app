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
             mqueue,
             mqueue_drv,
             'CAN',
             'CAN_drv']},                 
  {mod, { erleos_app, []}},
  {env, []}
 ]}.
