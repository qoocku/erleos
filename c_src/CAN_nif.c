/**
 * Erlang CAN NIF Driver.
 *
 * Based on LinCAN.
 *
 * @date 2010-11-20
 * @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
 */

#define MODULE "CAN_drv"

#include <stdio.h>
#include <string.h>
#include <sys/io.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>
#include <unistd.h>
#include "can.h"
#include "canmsg.h"
#include "erl_nif.h"
#include "nif_utils.c"

struct _CAN_Handle {
  int       device;
  ErlNifPid receiver;
  ErlNifTid tid;
  int       threaded;
  unsigned chunk_size;
  unsigned long timeout;
  int       raw;
};

typedef struct _CAN_Handle CAN_handle;

static ErlNifResourceType* CAN_handle_type = 0;
static ERL_NIF_TERM        can_atom;

static int
_load_module(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  ErlNifResourceFlags flags;
  CAN_handle_type = enif_open_resource_type(env, MODULE, "CAN_handle_type",
      NULL, ERL_NIF_RT_CREATE, &flags);
  can_atom = make_atom(env, "can");
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
  CAN_handle_type = enif_open_resource_type(env, MODULE, "CAN_handle_type",
      NULL, ERL_NIF_RT_TAKEOVER, &flags);
  enif_fprintf(stdout, "*** " MODULE " upgraded\n");
  return 0;
}

static ERL_NIF_TERM
_check_tuple (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  const ERL_NIF_TERM* items;
  int arity, i;
  enif_get_int(env, argv[0], &i);
  ERL_NIF_TERM head, tail, list;
  list = argv[1];
  while (enif_get_list_cell(env, list, &head, &tail))
    {
      list = tail;
      enif_get_tuple(env, head, &arity, &items);
      enif_fprintf(stdout, "arity: %i\n", arity);
      for (i = 0; i < arity; i++)
        {
          debug_enif_type(env, items[i]);
        }
    }
  return enif_make_int(env, arity);
}

static ERL_NIF_TERM
_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  CAN_handle*  handle;
  ERL_NIF_TERM result;
  char        dev_path[512];
  if (!enif_get_string(env, argv[0], dev_path, 512, ERL_NIF_LATIN1))
    return enif_make_int(env, -2000);
  handle = enif_alloc_resource(CAN_handle_type, sizeof(CAN_handle));
  memset(handle, 0, sizeof(CAN_handle));
  handle->device = open((const char*)dev_path,  O_RDWR | O_SYNC);
  if (!enif_get_int(env, argv[1], &handle->raw))
    return enif_make_int(env, -2001);
  handle->threaded = 0;
  if (handle->device >= 0)
    {
      result = enif_make_resource(env, handle);
    }
  else
    {
      result = enif_make_int(env, errno);
    }
  enif_release_resource(handle);
  return result;
}

static ERL_NIF_TERM
_receive_can_messages (ErlNifEnv* env,
                        CAN_handle* handle,
                        unsigned int chunk_size,
                        unsigned long timeout);

static void*
_reading_thread (void* arg)
{
  CAN_handle* handle  = arg;
  ErlNifEnv*  env     = enif_alloc_env();
  ERL_NIF_TERM device =   enif_make_int(env, handle->device);
  handle->threaded = 1;
  while (handle->threaded)
    {
      int status;
      ERL_NIF_TERM msg = _receive_can_messages(env, handle, handle->chunk_size, handle->timeout);
      if (!enif_get_int(env, msg, &status))
        {
          enif_send(env, &handle->receiver, env, enif_make_tuple3(env, can_atom, device, msg));
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
  CAN_handle* handle;
  ErlNifPid pid = { 0 }; // NOTE: breaking opaque type!
  enif_get_resource(env, argv[0], CAN_handle_type, (void**) &handle);
  if (handle->threaded) // there is a thread already and some pid!
    pid = handle->receiver;
  if (!enif_get_local_pid(env, argv[1], &handle->receiver)) // NOTE: use lock if pid type is structural!
    {
      handle->threaded = 0;
      return enif_make_badarg(env);
    }
  else
    {
      enif_get_uint(env, argv[2], &handle->chunk_size);
      enif_get_ulong(env, argv[3], &handle->timeout);
      if (!handle->threaded) // a thread was not created already
        {
          if (enif_thread_create("can_reading_thread",
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
_set_filter (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  CAN_handle* handle;
  int flags, queueid, cob, id, mask;
  ERL_NIF_TERM result;
  if (!enif_get_resource(env, argv[0], CAN_handle_type, (void**) &handle) ||
      !enif_get_int(env, argv[1], &flags) ||
      !enif_get_int(env, argv[2], &queueid) ||
      !enif_get_int(env, argv[1], &cob) ||
      !enif_get_int(env, argv[1], &id) ||
      !enif_get_int(env, argv[1], &mask))
    return enif_make_badarg(env);
  else
    {
    canfilt_t filter = {
        /*.flags = */flags,
        /*.queid = */queueid,
        /*.cob = */cob,
        /*.id = */id,
        /*.mask = */mask
    };
    int status = ioctl(handle->device, CANQUE_FILTER, &filter);
    result = enif_make_int(env, status != 0 ? errno : status);
    }
  return result;
}

static ERL_NIF_TERM
_set_baudrate  (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  CAN_handle* handle;
  int baudrate;
  ERL_NIF_TERM result;
  if (!enif_get_resource(env, argv[0], CAN_handle_type, (void**) &handle) ||
      !enif_get_int(env, argv[1], &baudrate))
    return enif_make_badarg(env);
  else
    {
    struct can_baudparams_t params = {-1, baudrate, -1, -1};
    int status = ioctl(handle->device, CONF_BAUDPARAMS, &params);
    result = enif_make_int(env, status != 0 ? errno : status);
    }
  return result;
}

static ERL_NIF_TERM
_send  (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  CAN_handle* handle;
  unsigned int i = 0, length, total_size = 0;
  ERL_NIF_TERM result;
  if (!enif_get_resource(env, argv[0], CAN_handle_type, (void**) &handle))
    return enif_make_int(env, -2000);
  if (!enif_get_list_length(env, argv[1], &length))
    return enif_make_int(env, -2001);
  canmsg_t* buffer = enif_alloc(length * sizeof(canmsg_t));
  memset(buffer, 0, length * sizeof(canmsg_t));
  ERL_NIF_TERM list = argv[1];
  ERL_NIF_TERM head, tail;
  while (enif_get_list_cell(env, list, &head, &tail))
    {
      canmsg_t* can_msg = &buffer[i++];
      int arity;
      canmsg_id_t target;
      ErlNifBinary msg;
      const ERL_NIF_TERM* items;
      list = tail;
      if (!enif_get_tuple(env, head, &arity, &items))
        {
          result = enif_make_int(env, -1000);
          goto end;
        }
      if (arity != 2)
        {
          result = enif_make_int(env, -1001);
          goto end;
        }
      if (!enif_get_ulong(env, items[0], &target))
        {
          result = enif_make_int(env, -1002);
          goto end;
        }
      if (!enif_inspect_binary(env, items[1], &msg))
        {
          result = enif_make_int(env, -1003);
          goto end;
        }
      if (msg.size > CAN_MSG_LENGTH)
        {
          result = enif_make_int(env, -1005);
          goto end;
        }
      can_msg->id = target;
      memcpy(&can_msg->data[0], msg.data, msg.size);
      can_msg->length = msg.size;
      total_size += msg.size;
    }
  {
    int status = write(handle->device, buffer, length * sizeof(canmsg_t));
    if (status != length * sizeof(canmsg_t)) status = errno;
    result = enif_make_tuple2(env,
                              enif_make_int(env, status),
                              enif_make_int(env, total_size));
  }
end:
  enif_free(buffer);
  return result;
}

static int
_wait_for_input (CAN_handle* handle, unsigned long timeout)
{
  int status;
  fd_set readSet;
  FD_ZERO(&readSet);
  FD_SET(handle->device, &readSet);
  struct timespec time = { 0, timeout };
  status = pselect(handle->device + 1, &readSet, NULL, NULL, &time, NULL);
  if (status == -1) {
      return errno;
  } else if (status > 0) {
      return 0;
  } else { //timeout
      return -1;
  }
}

#define BUFFER_LIMIT 500

static ERL_NIF_TERM
_receive_can_messages (ErlNifEnv* env,
                        CAN_handle* handle,
                        unsigned int chunk_size,
                        unsigned long timeout)
{
  int length        = 0,
      i              = 0,
      chunks         = 0;
  ERL_NIF_TERM *list, result;
  canmsg_t     buffer[sizeof(ERL_NIF_TERM) * BUFFER_LIMIT];
  do {
    int status = _wait_for_input(handle, timeout);
    if (status == -1) continue;
    if (status != 0)
      {
        result = enif_make_int(env, status);
        goto end;
      }
    length = read(handle->device, &buffer[chunks], sizeof(canmsg_t) * chunk_size);
    if (length < 0) break;
    chunks += length / sizeof(canmsg_t) ;
  } while (length > 0 && chunks <= BUFFER_LIMIT);
  if (chunks > 0)
    {
    if (handle->raw)
      {
        void* data = enif_make_new_binary(env, chunks * sizeof(canmsg_t), &result);
        memcpy(data, buffer, chunks * sizeof(canmsg_t));
      }
    else
      {
      list = enif_alloc(sizeof(ERL_NIF_TERM) * chunks);
      // rewrite canmsgs to list of tuples
      for (i = 0; i < chunks; i++)
        {
        canmsg_t* can_msg = buffer + i;
        ERL_NIF_TERM bin;
        void* data = enif_make_new_binary(env, can_msg->length, &bin);
        memcpy(data, can_msg->data, can_msg->length);
        list[i] = enif_make_tuple2(env, enif_make_int(env, can_msg->id), bin);
        }
      result = enif_make_list_from_array(env, list, chunks);
      enif_free(list);
      }
    }
  else if (length == 0)
    result = enif_make_int(env, 0);
  else
    result = enif_make_int(env, errno);
end:
  return result;
}

static ERL_NIF_TERM
_recv  (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  CAN_handle* handle;
  unsigned int chunk_size;
  ErlNifUInt64 timeout;
  ERL_NIF_TERM result;
  enif_get_resource(env, argv[0], CAN_handle_type, (void**) &handle);
  enif_get_uint(env, argv[1], &chunk_size);
  enif_get_uint64(env,argv[2], &timeout);
  result = _receive_can_messages(env, handle, chunk_size, timeout);
  return result;
}

static ERL_NIF_TERM
_close (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  CAN_handle* handle;
  ERL_NIF_TERM result;
  if (!enif_get_resource(env, argv[0], CAN_handle_type, (void**) &handle))
    return enif_make_badarg(env);;
  if (handle->threaded)
    {
      void* dont_care;
      handle->threaded = 0;
      enif_thread_join(handle->tid, &dont_care);
    }
  result = enif_make_int(env, close(handle->device));
  return result;
}

static ERL_NIF_TERM
_translate_errno (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int err;
  if (!enif_get_int(env, argv[0], &err))
    return enif_make_badarg(env);
  return errno_atom(env, err);
}

static ErlNifFunc nif_funcs[] =
  {
    { "open", 2, _open },
    { "set_baudrate", 2, _set_baudrate },
    { "set_filter", 6, _set_filter },
    { "send", 2, _send },
    { "recv", 3, _recv },
    { "close", 1, _close },
    { "listener", 4, _listener },
    { "translate_errno", 1, _translate_errno },
    { "check_tuple", 2, _check_tuple }
  };

ERL_NIF_INIT(CAN_drv, nif_funcs, _load_module, _reload_module, _upgrade_module, _unload_module);
