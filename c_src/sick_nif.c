/**
 * Erlang CAN NIF Driver.
 *
 * Based on LinCAN.
 *
 * @date 2010-11-20
 * @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
 */

#define MODULE "sick_drv"

#include <stdio.h>
#include <string.h>
#include <sys/io.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>
#include <unistd.h>
#include "erl_nif.h"
#include "nif_utils.h"
#include "sick_drv.h"

struct _sick_handle {
  sick_handle_t device;
  ErlNifPid       receiver;
  ErlNifTid       tid;
  int             threaded;
  long            timeout;
};

typedef struct _sick_handle sick_handle;

static ErlNifResourceType* sick_handle_type = 0;
static ERL_NIF_TERM        sick_atom;

static int
_load_module(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  ErlNifResourceFlags flags;
  sick_handle_type = enif_open_resource_type(env, MODULE, "sick_handle_type",
      NULL, ERL_NIF_RT_CREATE, &flags);
  sick_atom = make_atom(env, "sick");
  enif_fprintf(stdout, "*** " MODULE " loaded\n");
  return 0;
}

static int
_reload_module(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  enif_fprintf(stdout, "*** " MODULE " reloaded\n");
  return 0;
}

static void
_unload_module(ErlNifEnv* env, void* priv_data)
{
  enif_fprintf(stdout, "*** " MODULE " unloaded\n");
}

static int
_upgrade_module(ErlNifEnv* env, void** priv_data, void** old_priv_data,
    ERL_NIF_TERM load_info)
{
  ErlNifResourceFlags flags;
  sick_handle_type = enif_open_resource_type(env, MODULE, "sick_handle_type",
      NULL, ERL_NIF_RT_TAKEOVER, &flags);
  enif_fprintf(stdout, "*** " MODULE " upgraded\n");
  return 0;
}

static ERL_NIF_TERM
_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  sick_handle*  handle;
  handle = enif_alloc_resource(sick_handle_type, sizeof(sick_handle));
  memset(handle, 0, sizeof(sick_handle));
  handle->device = sick_open();
  handle->threaded = 0;
  enif_release_resource(handle);
  return enif_make_resource(env, handle);
}

static ERL_NIF_TERM
_receive_sick_messages (ErlNifEnv* env,
                           sick_handle* handle,
                           long timeout);

static void*
_reading_thread (void* arg)
{
  sick_handle* handle  = arg;
  ErlNifEnv*  env     = enif_alloc_env();
  handle->threaded = 1;
  while (handle->threaded)
    {
      int status;
      ERL_NIF_TERM msg = _receive_sick_messages(env, handle, handle->timeout);
      if (!enif_get_int(env, msg, &status))
        {
          enif_send(env, &handle->receiver, env, enif_make_tuple2(env, sick_atom, msg));
          enif_clear_env(env);
        }
      else if (status == 0)
        {
          enif_clear_env(env);
        }
      else break;
    }
  enif_free_env(env);
  return 0;
}

static ERL_NIF_TERM
_listener (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  sick_handle* handle;
  ErlNifPid pid = { 0 }; // NOTE: breaking opaque type!
  enif_get_resource(env, argv[0], sick_handle_type, (void**) &handle);
  if (handle->threaded) // there is a thread already and some pid!
  {
    pid = handle->receiver;
  }
  if (!enif_get_local_pid(env, argv[1], &handle->receiver)) // NOTE: use lock if pid type is structural!
    {
      handle->threaded = 0;
      return enif_make_badarg(env);
    }
  else
    {
      if (!handle->threaded) // a thread was not created already
        {
          if (enif_thread_create("sick_reading_thread",
                                    &handle->tid,
                                    _reading_thread,
                                    handle, 0))
            {
              handle->threaded = 0;
              return enif_make_int(env, -1004);
            }
        }
    }
  return pid.pid ? enif_make_pid(env, &pid) : enif_make_int(env, 0);
}

static ERL_NIF_TERM
_configure (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  sick_handle* handle;
  ERL_NIF_TERM result;
  if (!enif_get_resource(env, argv[0], sick_handle_type, (void**) &handle))
    return enif_make_badarg(env);
  else
    {
      int status = sick_configure(handle->device);
      result = enif_make_int(env, status);
    }
  return result;
}

static ERL_NIF_TERM
_start  (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  sick_handle* handle;
  ERL_NIF_TERM result;
  if (!enif_get_resource(env, argv[0], sick_handle_type, (void**) &handle))
    return enif_make_badarg(env);
  else
    {
      int status = sick_start(handle->device);
      result = enif_make_int(env, status);
    }
  return result;
}

static ERL_NIF_TERM
_read_stream  (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  sick_handle* handle;
  ERL_NIF_TERM result;
  if (!enif_get_resource(env, argv[0], sick_handle_type, (void**) &handle))
    return enif_make_badarg(env);
  int status = sick_read_stream(handle->device);
  result = enif_make_int(env, status);
  return result;
}

#define SCAN_LEN 362*sizeof(unsigned short int)

static ERL_NIF_TERM
_get_current_scan (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  sick_handle* handle;
  if (!enif_get_resource(env, argv[0], sick_handle_type, (void**) &handle))
    return enif_make_badarg(env);
  unsigned short int* data = sick_get_current_scan(handle->device);
  ERL_NIF_TERM bin;
  void* target = enif_make_new_binary(env, SCAN_LEN, &bin);
  memcpy(target, data, SCAN_LEN);
  return bin;
}


static ERL_NIF_TERM
_receive_sick_messages (ErlNifEnv* env,
                           sick_handle* handle,
                           long timeout)
{
  ERL_NIF_TERM result;
  int status = sick_read_stream(handle->device);
  if (status == 0)
    {
      unsigned short int* data = sick_get_current_scan(handle->device);
      ERL_NIF_TERM bin;
      void* target = enif_make_new_binary(env, SCAN_LEN, &bin);
      memcpy(target, data, SCAN_LEN);
      result = bin;
    }
  else
    {
      ERL_NIF_TERM bin;
      enif_make_new_binary(env, 0, &bin);
      result = bin;
    }
  return result;
}

static ERL_NIF_TERM
_close (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  sick_handle* handle;
  ERL_NIF_TERM result;
  if (!enif_get_resource(env, argv[0], sick_handle_type, (void**) &handle))
    return enif_make_badarg(env);;
  if (handle->threaded)
    {
      void* dont_care;
      handle->threaded = 0;
      enif_thread_join(handle->tid, &dont_care);
    }
  sick_close(handle->device);
  result = enif_make_int(env, 0);
  handle->device = NULL;
  return result;
}

static ErlNifFunc nif_funcs[] =
  {
    { "open", 0, _open },
    { "configure", 1, _configure },
    { "start", 1, _start },
    { "read_stream", 1, _read_stream },
    { "get_current_scan", 1, _get_current_scan },
    { "close", 1, _close },
    { "listener", 2, _listener }
  };

ERL_NIF_INIT(sick_drv, nif_funcs, _load_module, _reload_module, _upgrade_module, _unload_module);
