/**
 * mqueue NIF Driver
 *
 * @date 19-11-2010
 * @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
 */

#include <mqueue.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <time.h>

#define MODULE "mqueue_drv"

#include "erl_nif.h"
#include "nif_utils.c"

/** Some common atoms **/
static ERL_NIF_TERM error_atom;
static ERL_NIF_TERM ok_atom;
static ERL_NIF_TERM eagain_atom;
static ERL_NIF_TERM mqueue_atom;

static ERL_NIF_TERM
hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);
}

ErlNifResourceType* mqd_type = 0;
struct mq_handle {
  mqd_t  queue;
  int    owned;
  ssize_t max_msg_size;
  char   name[256];
  int    blocked;
  int    threaded;
  ErlNifPid receiver;
  ErlNifTid tid;
};

static int
load_module(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  ErlNifResourceFlags flags;
  mqd_type = enif_open_resource_type(env, MODULE, "mqd_t", NULL,
      ERL_NIF_RT_CREATE, &flags);
  error_atom  = make_atom(env, "error");
  ok_atom     = make_atom(env, "ok");
  eagain_atom = make_atom(env, "eagain");
  mqueue_atom = make_atom(env, "mqueue");
  return 0;
}

static int
reload_module(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  printf("***mqueue_nif reload called\n");
  return 0;
}

static void
unload_module(ErlNifEnv* env, void* priv_data)
{
  printf("***mqueue_nif unload called\n");
}

static int
upgrade_module (ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
  ErlNifResourceFlags flags;
  mqd_type = enif_open_resource_type(env, NULL, "mqd_t", NULL,
        ERL_NIF_RT_TAKEOVER, &flags);
  return 0;
}

#define THR_NAME "mqueue_reading_thread"
static void* reading_thread (void*);

static ERL_NIF_TERM
_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  struct mq_attr attr;
  ErlNifPid pid;
  int has_pid = 0;
  attr.mq_flags = 0; /* Flags: 0 or O_NONBLOCK */
  if (!enif_get_long(env, argv[1], &attr.mq_maxmsg)) /* Max. # of messages on queue */
    return make_tuple2_result(env, error_atom, "Invalid argument #2: should be an integer");
  if (!enif_get_long(env, argv[2], &attr.mq_msgsize)) /* Max. message size (bytes) */
    return make_tuple2_result(env, error_atom, "Invalid argument #3: should be an integer");
  attr.mq_curmsgs = 0; /* # of messages currently in queue */
  unsigned len;
  if (!enif_get_list_length(env, argv[0], &len))
    return make_tuple2_result(env, error_atom, "Invalid argument #1: should be a string");
  if (len > 255)
    return make_tuple2_result(env, error_atom, "Queue name too long");
  char* name = enif_alloc(len);
  if (!enif_get_string(env, argv[0],  name, len+1, ERL_NIF_LATIN1))
    return make_tuple2_result(env, error_atom, "Invalid argument #1: should be a string");
  unsigned optlen;
  if (!enif_get_list_length(env, argv[3], &optlen))
    return make_tuple2_result(env, error_atom, "Invalid argument #4: should be a list of options");
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
        if (!enif_get_atom(env, head, buf, 32, ERL_NIF_LATIN1))
          {
            if (!enif_get_local_pid(env, head, &pid))
              return make_tuple2_result(env, error_atom,
                                         "Invalid option type. Should be a pid or atom");
            else
              has_pid = 1;
          }
          else
            {
              if (strcmp(buf, "noblock") == 0)
                blocked = 0;
              if (strcmp(buf, "own") == 0)
                owned = 1;
            }
      }
  }
  struct mq_handle* result = enif_alloc_resource(mqd_type, sizeof(struct mq_handle));
  result->blocked      = blocked;
  result->owned        = owned;
  if (has_pid)
    {
      result->threaded = 1;
      result->receiver = pid;
    }
  else
    result->threaded = 0;
  int flags = O_RDWR | O_CREAT;
  if (!blocked)
    {
      flags |= O_NONBLOCK;
      attr.mq_flags |= O_NONBLOCK;
    }
  result->queue        = mq_open(name, flags, S_IWUSR | S_IRUSR, &attr);
  result->max_msg_size = attr.mq_msgsize;
  strncpy(result->name, name, len+1);
  ERL_NIF_TERM tuple;
  if (result->queue == -1)
    {
      tuple = enif_make_tuple2(env, error_atom, errno_atom(env, errno));
    }
  else
    {
      tuple = enif_make_tuple2(env, ok_atom, enif_make_resource(env, result));
    }
  if (has_pid)
    if (enif_thread_create(THR_NAME, &result->tid, reading_thread, (void*)result, 0))
        return enif_make_tuple2(env, error_atom, errno_atom(env, errno));
  enif_free(name);
  enif_release_resource(result);
  return tuple;
}

static void*
reading_thread (void* arg)
{
  struct mq_handle* handle = arg;
  char* buffer             = enif_alloc(handle->max_msg_size);
  ErlNifEnv* env            = enif_alloc_env();
  ErlNifPid Self;
  struct timespec t        = {0, 1000000}; // 1 msec of timeout
  enif_self(env, &Self);
  while (handle->threaded)
    {
      ERL_NIF_TERM tuple;
      struct timespec timeout = {0, 0};
      ssize_t size;
      int clockResult = clock_gettime(CLOCK_REALTIME, &timeout);
      if (clockResult != 0)
        {
          size = errno;
        }
      else
        {
          timeout.tv_sec += t.tv_sec;
          timeout.tv_nsec += t.tv_nsec;
          if (timeout.tv_nsec > 1000000000)
            {
              timeout.tv_sec ++;
              timeout.tv_nsec -= 1000000000;
            }
          size  = mq_timedreceive(handle->queue, buffer, handle->max_msg_size, 0, &timeout);
        }
      if (!handle->threaded) break;
      if (size == -1)
        {
          int err = errno;
          if (err == ETIMEDOUT) continue;
          tuple = enif_make_tuple2(env, mqueue_atom,
                                   enif_make_tuple2(env,
                                                    error_atom,
                                                    errno_atom(env, err)));
        }
      else if (size > 0)
        {
          ERL_NIF_TERM bin;
          void* data = enif_make_new_binary(env, size, &bin);
          memcpy(data, buffer, size);
          tuple = enif_make_tuple3(env, mqueue_atom, enif_make_pid(env, &Self), bin);
        }
      if (handle->threaded)
        {
          enif_send(0, &handle->receiver, env, tuple);
          //enif_fprintf(stdout, "sent %i bytes\n", size);
        }
      enif_clear_env(env);
      if (size == -1)
        break;
    }
  enif_free_env(env);
  enif_free(buffer);
  return 0;
}

static ERL_NIF_TERM
_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int result;
  struct mq_handle* handle;
  enif_get_resource(env, argv[0], mqd_type, (void**) &handle);
  if (handle->threaded)
    {
      void* dont_care;
      result = mq_close(handle->queue);
      handle->threaded = 0;
      enif_thread_join(handle->tid, &dont_care);
    }
  else
    result = mq_close(handle->queue);
  if (result == 0 && handle->owned)
    result = mq_unlink(handle->name);
  if (result == 0)
    return enif_make_int(env, 0);
  else
    return enif_make_tuple2(env, error_atom, errno_atom(env, errno));
}

static ERL_NIF_TERM
_receive(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  struct mq_handle* handle;
  if (!enif_get_resource(env, argv[0], mqd_type, (void**)&handle))
    return make_tuple2_result(env, error_atom, "Argument should be a mqueue handle");
  void* buffer = enif_alloc(handle->max_msg_size);
  ssize_t size  = mq_receive(handle->queue, buffer, handle->max_msg_size, 0);
  ERL_NIF_TERM tuple;
  if (size == -1)
    {
      tuple = enif_make_tuple2(env, error_atom, errno_atom(env, errno));
    }
  else
    {
      ERL_NIF_TERM bin;
      void* data = enif_make_new_binary(env, size, &bin);
      memcpy(data, buffer, size);
      tuple = enif_make_tuple2(env, ok_atom, bin);
    }
  enif_free(buffer);
  return tuple;
}

static ERL_NIF_TERM
_send(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  struct mq_handle* handle;
  if (!enif_get_resource(env, argv[0], mqd_type, (void**)&handle))
    return make_tuple2_result(env, error_atom, "Invalid arg #1: should be a queue handle");
  ErlNifBinary bin;
  if (!enif_inspect_binary(env, argv[1], &bin))
    return make_tuple2_result(env, error_atom, "Invalid arg #2: should be a binary");
  int prio;
  if (!enif_get_int(env, argv[2], &prio))
    return make_tuple2_result(env, error_atom, "Invalid arg #3: should be an integer");
  int status = mq_send(handle->queue, (const char*)bin.data, bin.size, prio);
  if (status != 0)
    return enif_make_tuple2(env, error_atom, errno_atom(env, errno));
  return ok_atom;
}

static ERL_NIF_TERM
_props (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  struct mq_handle* handle;
  if (!enif_get_resource(env, argv[0], mqd_type, (void**)&handle))
    return make_tuple2_result(env, error_atom, "Invalid arg #1: should be a queue handle");
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
    { "open",  4, _open },
    { "close", 1, _close },
    { "recv",  1, _receive },
    { "send",  3, _send },
    { "props", 1, _props }
  };

ERL_NIF_INIT(mqueue_drv, nif_funcs, load_module, reload_module, upgrade_module, unload_module);
