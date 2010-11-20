/*
 * mqueue_nif.c
 *
 *  Created on: 19-11-2010
 *      Author: Damian T. Dobroczy\\'nski <qoocku@gmail.com>
 */

#include "erl_nif.h"
#include <mqueue.h>
#include <errno.h>
#include <string.h>

#define MODULE "mqueue_drv"

static ERL_NIF_TERM
hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);
}

ErlNifResourceType* mqd_type;
struct mq_handle {
  mqd_t  queue;
  int    owned;
  ssize_t max_msg_size;
  char   name[256];
  int    blocked;
};

static int
load_module(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  mqd_type = enif_open_resource_type(env, MODULE, "mqd_t", NULL,
      ERL_NIF_RT_CREATE, NULL);
  return 0;
}

static int
reload_module(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  return 0;
}

static void
unload_module(ErlNifEnv* env, void* priv_data)
{
}

static int
upgrade_module (ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
  return 0;
}

static ERL_NIF_TERM
make_tuple2_result (ErlNifEnv* env, const char* fst, const char* snd)
{
  ERL_NIF_TERM atom, string;
  enif_make_existing_atom(env, fst, &atom, ERL_NIF_LATIN1);
  string = enif_make_string(env, snd, ERL_NIF_LATIN1);
  return enif_make_tuple2(env, atom, string);
}

static ERL_NIF_TERM
make_atom (ErlNifEnv* env, const char* str)
{
  ERL_NIF_TERM atom;
  if (!enif_make_existing_atom(env, str, &atom, ERL_NIF_LATIN1))
    atom = enif_make_atom(env, str);
  return atom;
}

static ERL_NIF_TERM
error_atom (ErlNifEnv* env, int err)
{
  const char* str;
  switch (err) {
  case EACCES:
    str = "eaccess"; break;
  case EEXIST:
    str = "eexist"; break;
  case EINVAL:
    str = "einval"; break;
  case EMFILE:
    str = "emfile"; break;
  case ENAMETOOLONG:
    str = "emfile"; break;
  case ENFILE:
    str = "enfile"; break;
  case ENOENT:
    str = "enoent"; break;
  case ENOMEM:
    str = "enomem"; break;
  case ENOSPC:
    str = "enospc"; break;
  case EAGAIN:
    str = "eagain"; break;
  default:
    str = (const char*)strerror(err);
  }
  return make_atom(env, str);
}

static ERL_NIF_TERM
_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  struct mq_attr attr;
  attr.mq_flags = 0; /* Flags: 0 or O_NONBLOCK */
  if (!enif_get_long(env, argv[1], &attr.mq_maxmsg)) /* Max. # of messages on queue */
    return make_tuple2_result(env, "error", "Invalid argument #2: should be an integer");
  if (!enif_get_long(env, argv[2], &attr.mq_msgsize)) /* Max. message size (bytes) */
    return make_tuple2_result(env, "error", "Invalid argument #3: should be an integer");
  attr.mq_curmsgs = 0; /* # of messages currently in queue */
  unsigned len;
  if (!enif_get_list_length(env, argv[0], &len))
    return make_tuple2_result(env, "error", "Invalid argument #1: should be a string");
  if (len > 255)
    return make_tuple2_result(env, "error", "Queue name too long");
  char* name = enif_alloc(len);
  if (!enif_get_string(env, argv[0],  name, len+1, ERL_NIF_LATIN1))
    return make_tuple2_result(env, "error", "Invalid argument #1: should be a string");
  unsigned optlen;
  if (!enif_get_list_length(env, argv[3], &optlen))
    return make_tuple2_result(env, "error", "Invalid argument #4: should be a list of options");
  int owned = 0, blocked = 1;
  {
    int i;
    ERL_NIF_TERM tail = argv[3];
    for (i = 0; i < optlen; i++)
      {
        char buf[32];
        ERL_NIF_TERM head, tl;
        enif_get_list_cell(env, tail, &head, &tl);
        tail = tl;
        if (!enif_is_atom(env, head))
          return make_tuple2_result(env, "error", "Invalid option type. Should be atom");
        enif_get_atom(env, head, buf, 32, ERL_NIF_LATIN1);
        if (strcmp(buf, "noblock") == 0)
          blocked = 0;
        if (strcmp(buf, "own") == 0)
          owned = 1;
      }
  }
  struct mq_handle* result = enif_alloc_resource(mqd_type, sizeof(struct mq_handle));
  result->blocked      = blocked;
  result->owned        = owned;
  int flags = O_RDWR | O_CREAT;
  if (!blocked) flags |= O_NONBLOCK;
  result->queue        = mq_open(name, flags, S_IWUSR | S_IRUSR, &attr);
  result->max_msg_size = attr.mq_msgsize;
  strncpy(result->name, name, len+1);
  ERL_NIF_TERM tuple;
  if (result->queue == -1)
    {
      tuple = enif_make_tuple2(env, make_atom(env, "error"), error_atom(env, errno));
    }
  else
    {
      tuple = enif_make_tuple2(env, make_atom(env, "ok"), enif_make_resource(env, result));
    }
  enif_free(name);
  enif_release_resource(result);
  return tuple;
}

static ERL_NIF_TERM
_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  struct mq_handle* handle;
  enif_get_resource(env, argv[0], mqd_type, (void**) &handle);
  int result = mq_close(handle->queue);
  if (result == 0 && handle->owned)
    result = mq_unlink(handle->name);
  enif_release_resource(handle);
  if (result == 0)
    return enif_make_int(env, 0);
  else
    return enif_make_tuple2(env, make_atom(env, "error"), error_atom(env, errno));
}

static ERL_NIF_TERM
_receive(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  struct mq_handle* handle;
  enif_get_resource(env, argv[0], mqd_type, (void**)&handle);
  void* buffer = enif_alloc(handle->max_msg_size);
  ssize_t size  = mq_receive(handle->queue, buffer, handle->max_msg_size, 0);
  ERL_NIF_TERM tuple;
  if (size == -1)
    {
      tuple = enif_make_tuple2(env, make_atom(env, "error"), error_atom(env, errno));
    }
  else
    {
      ERL_NIF_TERM bin;
      void* data = enif_make_new_binary(env, size, &bin);
      memcpy(data, buffer, size);
      tuple = enif_make_tuple2(env, make_atom(env, "ok"), bin);
    }
  enif_free(buffer);
  return tuple;
}

static ERL_NIF_TERM
_send(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  struct mq_handle* handle;
  if (!enif_get_resource(env, argv[0], mqd_type, (void**)&handle))
    return make_tuple2_result(env, "error", "Invalid arg #1: should be a queue handle");
  ErlNifBinary bin;
  if (!enif_inspect_binary(env, argv[1], &bin))
    return make_tuple2_result(env, "error", "Invalid arg #2: should be a binary");
  int prio;
  if (!enif_get_int(env, argv[2], &prio))
    return make_tuple2_result(env, "error", "Invalid arg #3: should be an integer");
  int status = mq_send(handle->queue, (const char*)bin.data, bin.size, prio);
  if (status != 0)
    return enif_make_tuple2(env, make_atom(env, "error"), error_atom(env, errno));
  return make_atom(env, "ok");
}

static ERL_NIF_TERM
_props (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  struct mq_handle* handle;
  if (!enif_get_resource(env, argv[0], mqd_type, (void**)&handle))
    return make_tuple2_result(env, "error", "Invalid arg #1: should be a queue handle");
  ERL_NIF_TERM arr[2];
  int len = 0;
  if (handle->owned)
    enif_make_existing_atom(env, "own", &arr[len++], ERL_NIF_LATIN1);
  if (!handle->blocked)
    enif_make_existing_atom(env, "noblock", &arr[len++], ERL_NIF_LATIN1);
  return enif_make_list_from_array(env, arr, len);
}

static ErlNifFunc nif_funcs[] =
  {
    { "hello", 0, hello },
    { "open", 4, _open },
    { "close", 1, _close },
    { "recv", 1, _receive },
    { "send", 3, _send },
    { "props", 1, _props }
  };

ERL_NIF_INIT(mqueue_drv,
             nif_funcs,
             load_module, reload_module, upgrade_module, unload_module);
