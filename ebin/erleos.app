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
             mqueue,
             mqueue_drv]},                 
  {mod, { erleos_app, []}},
  {env, []}
 ]}.
