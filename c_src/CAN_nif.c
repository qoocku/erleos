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
#include "can.h"
#include "canmsg.h"
#include "erl_nif.h"
#include "nif_utils.c"

struct _CAN_Handle {
  int handle;
  ErlNifPid receiver;
  ErlNifTid tid;
  int threaded;
  unsigned chunk_size;
  unsigned long timeout;
};

typedef struct _CAN_Handle CAN_handle;

static ErlNifResourceType* CAN_handle_type = 0;
static ERL_NIF_TERM can_atom;

static int
load_module(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  ErlNifResourceFlags flags;
  CAN_handle_type = enif_open_resource_type(env, MODULE, "CAN_handle_type",
      NULL, ERL_NIF_RT_CREATE, &flags);
  can_atom = make_atom(env, "can");
  return 0;
}

static int
reload_module(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  printf("*** " MODULE " reload called\n");
  return 0;
}

static void
unload_module(ErlNifEnv* env, void* priv_data)
{
  printf("*** " MODULE " unload called\n");
}

static int
upgrade_module(ErlNifEnv* env, void** priv_data, void** old_priv_data,
    ERL_NIF_TERM load_info)
{
  ErlNifResourceFlags flags;
  CAN_handle_type = enif_open_resource_type(env, MODULE, "CAN_handle_type",
      NULL, ERL_NIF_RT_TAKEOVER, &flags);
  return 0;
}

static ERL_NIF_TERM
_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  CAN_handle* handle;
  ERL_NIF_TERM result;
  char dev_path[512];
  enif_get_string(env, argv[0], dev_path, 512, ERL_NIF_LATIN1);
  handle = enif_alloc_resource(CAN_handle_type, sizeof(CAN_handle));
  handle->handle = open((const char*)dev_path,  O_RDWR | O_SYNC);
  if (handle->handle >= 0)
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
  ERL_NIF_TERM device =   enif_make_int(env, handle->handle);
  while (handle->threaded)
    {
      ERL_NIF_TERM msg = _receive_can_messages(env, handle, handle->chunk_size, handle->timeout);
      enif_send(env, &handle->receiver, env, enif_make_tuple3(env, can_atom, device, msg));
      enif_clear_env(env);
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
    handle->threaded = 0;
  enif_get_uint(env, argv[2], &handle->chunk_size);
  enif_get_ulong(env, argv[3], &handle->timeout);
  if (!handle->threaded) // a thread was not created already
    {
      if (!enif_thread_create("can_reading_thread",
                              &handle->tid,
                              _reading_thread,
                              handle, 0))
        return enif_make_int(env, -1004);
    }
  return enif_make_pid(env, &pid);
}

static ERL_NIF_TERM
_set_filter (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  CAN_handle* handle;
  int flags, queueid, cob, id, mask;
  ERL_NIF_TERM result;
  enif_get_resource(env, argv[0], CAN_handle_type, (void**) &handle);
  enif_get_int(env, argv[1], &flags);
  enif_get_int(env, argv[2], &queueid);
  enif_get_int(env, argv[1], &cob);
  enif_get_int(env, argv[1], &id);
  enif_get_int(env, argv[1], &mask);
  {
    canfilt_t filter =
    {
    /*.flags = */flags,
    /*.queid = */queueid,
    /*.cob = */cob,
    /*.id = */id,
    /*.mask = */mask
    };
    int status = ioctl(handle->handle, CANQUE_FILTER, &filter);
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
  enif_get_resource(env, argv[0], CAN_handle_type, (void**) &handle);
  enif_get_int(env, argv[1], &baudrate);
  {
    struct can_baudparams_t params = {-1, baudrate, -1, -1};
    int status = ioctl(handle->handle, CONF_BAUDPARAMS, &params);
    result = enif_make_int(env, status != 0 ? errno : status);
  }
  return result;
}

static ERL_NIF_TERM
_send  (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  CAN_handle* handle;
  ERL_NIF_TERM messages;
  unsigned int i, length, total_size = 0;
  ERL_NIF_TERM result;
  enif_get_resource(env, argv[0], CAN_handle_type, (void**) &handle);
  messages = argv[1];
  enif_get_list_length(env, messages, &length);
  canmsg_t* buffer = enif_alloc(length * sizeof(canmsg_t));
  for (i = 0; i < length; i++)
    {
      canmsg_t* can_msg = &buffer[i];
      int arity;
      unsigned int target;
      ErlNifBinary msg;
      ERL_NIF_TERM head;
      ERL_NIF_TERM items[2];
      enif_get_list_cell(env, messages, &head, &messages);
      if (!enif_get_tuple(env, head, &arity, (const ERL_NIF_TERM**)&items))
        {
          result = enif_make_int(env, -1000);
          goto end;
        }
      if (arity != 2)
        {
          result = enif_make_int(env, -1001);
          goto end;
        }
      if (!enif_get_uint(env, items[0], &target))
        {
	  if (enif_is_atom(env, items[0]))
	    enif_fprintf(stdout, "atom\n");
          else if (enif_is_binary(env, items[0]))
	    enif_fprintf(stdout, "bin\n");
          else if (enif_is_empty_list(env, items[0]))
	    enif_fprintf(stdout, "empty list\n");
          else if (enif_is_fun(env, items[0]))
	    enif_fprintf(stdout, "fun\n");
          else if (enif_is_pid(env, items[0]))
	    enif_fprintf(stdout, "pid\n");
          else if (enif_is_port(env, items[0]))
	    enif_fprintf(stdout, "port\n");
          else if (enif_is_ref(env, items[0]))
	    enif_fprintf(stdout, "ref\n");
          else if (enif_is_tuple(env, items[0]))
	    enif_fprintf(stdout, "tuple\n");
          else if (enif_is_list(env, items[0]))
	    enif_fprintf(stdout, "list\n");
          result = enif_make_int(env, -1002);
          goto end;
        }
      can_msg->id = target;
      if (!enif_inspect_binary(env, items[1], &msg))
        {
          result = enif_make_int(env, -1003);
          goto end;
        }
      memcpy(can_msg->data, msg.data, msg.size);
      can_msg->length = msg.size;
      total_size += msg.size;
    }
  {
    int status = write(handle->handle, buffer, length);
    if (status != length) status = errno;
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
  FD_SET(handle->handle, &readSet);
  struct timespec time = { 0, timeout };
  status = pselect(handle->handle + 1, &readSet, NULL, NULL, &time, NULL);
  if (status == -1) {
      return errno;
  } else if (status > 0) {
      return 0;
  } else { //timeout
      return -1;
  }
}

static ERL_NIF_TERM
_receive_can_messages (ErlNifEnv* env,
                        CAN_handle* handle,
                        unsigned int chunk_size,
                        unsigned long timeout)
{
  int length, offset = 0, chunks = 1, i = 0;
  ERL_NIF_TERM* list;
  ERL_NIF_TERM result;
  canmsg_t* buffer = enif_alloc(sizeof(ERL_NIF_TERM) * chunk_size);
  do {
    if (timeout > 0)
      {
        int status = _wait_for_input(handle, timeout);
        if (status == -1) continue;
        if (status != 0)
          {
            result = enif_make_int(env, status);
            goto end;
          }
      }
    length = read(handle->handle, buffer, sizeof(canmsg_t));
    if (length < 0) break;
    offset += length;
    i += length;
    if (i > chunk_size)
      {
        chunks += 1;
        enif_realloc(buffer, chunks * chunk_size);
        i = 0;
      }

  } while (length <= 0);
  list = enif_alloc(sizeof(ERL_NIF_TERM) * chunk_size);
  // rewrite canmsgs to list of tuples
  for (i = 0; i < offset; i++)
    {
      canmsg_t* can_msg = &buffer[i];
      ERL_NIF_TERM bin;
      void* data = enif_make_new_binary(env, can_msg->length, &bin);
      memcpy(data, can_msg->data, can_msg->length);
      list[i] = enif_make_tuple2(env, enif_make_int(env, can_msg->id), bin);
    }
  result = enif_make_list_from_array(env, list, length);
  enif_free(list);
end:
  enif_free(buffer);
  return result;
}

static ERL_NIF_TERM
_recv  (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  CAN_handle* handle;
  unsigned int chunk_size;
  ERL_NIF_TERM result;
  enif_get_resource(env, argv[0], CAN_handle_type, (void**) &handle);
  enif_get_uint(env, argv[1], &chunk_size);
  result = _receive_can_messages(env, handle, chunk_size, 0L);
  return result;
}

static ERL_NIF_TERM
_close (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  CAN_handle* handle;
  ERL_NIF_TERM result;
  enif_get_resource(env, argv[0], CAN_handle_type, (void**) &handle);
  if (handle->threaded)
    {
      void* dont_care;
      handle->threaded = 0;
      enif_thread_join(handle->tid, &dont_care);
    }
  result = enif_make_int(env, close(handle->handle));
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
    { "open", 1, _open },
    { "set_baudrate", 2, _set_baudrate },
    { "set_filter", 6, _set_filter },
    { "send", 2, _send },
    { "recv", 2, _recv },
    { "close", 1, _close },
    { "listener", 4, _listener },
    { "translate_errno", 1, _translate_errno }
  };

ERL_NIF_INIT(CAN_drv, nif_funcs, load_module, reload_module, upgrade_module, unload_module);
