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

static int
load_module(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  mqd_type = enif_open_resource_type(env, MODULE, "mqd_t", NULL,
      ERL_NIF_RT_CREATE, NULL);
  return 0;
}

static void
unload_module(ErlNifEnv* env, void** priv_data)
{
}

static ERL_NIF_TERM
_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  struct mq_attr attr;
  attr.mq_flags = 0; /* Flags: 0 or O_NONBLOCK */
  enif_get_long(env, argv[1], &attr.mq_maxmsg); /* Max. # of messages on queue */
  enif_get_long(env, argv[2], &attr.mq_msgsize); /* Max. message size (bytes) */
  attr.mq_curmsgs = 0; /* # of messages currently in queue */
  mqd_t* result = enif_alloc_resource(mqd_type, sizeof(mqd_t));
  char name[512];
  enif_get_string(env, argv[0], name, 512, ERL_NIF_LATIN1);
  *result = mq_open(name, O_RDWR | O_CREAT, S_IWUSR | S_IRUSR, &attr);
  ERL_NIF_TERM tuple[2];
  if (*result == -1)
    {
      tuple[0] = enif_make_atom(env, "error");
      tuple[1] = enif_make_string(env, (const char*) strerror(errno),
          ERL_NIF_LATIN1);
    }
  else
    {
      tuple[0] = enif_make_atom(env, "ok");
      tuple[1] = enif_make_resource(env, result);
    }
  enif_release_resource(result);
  return enif_make_tuple_from_array(env, tuple, 2);
}

static ERL_NIF_TERM
_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mqd_t* queue;
  enif_get_resource(env, argv[0], mqd_type, (void**) &queue);
  int result = mq_close(*queue);
  enif_release_resource(queue);
  return enif_make_int(env, result);
}

static ERL_NIF_TERM
_receive(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mqd_t* queue;
  enif_get_resource(env, argv[0], mqd_type, (void**)&queue);
  char buffer[4096];
  ssize_t result = mq_receive(*queue, buffer, 4096, 0);
  ERL_NIF_TERM tuple[2];
  if (result == -1)
    {
      tuple[0] = enif_make_atom(env, "error");
      tuple[1] = enif_make_string(env, (const char*) strerror(errno),
          ERL_NIF_LATIN1);
    }
  else
    {
      tuple[0] = enif_make_atom(env, "ok");
      ErlNifBinary bin;
      enif_alloc_binary(result, &bin);
      memcpy(bin.data, buffer, result);
      tuple[1] = enif_make_binary(env, &bin);
    }
  return enif_make_tuple_from_array(env, tuple, 2);
}

static ErlNifFunc nif_funcs[] =
  {
    { "hello", 0, hello },
    { "open", 3, _open },
    { "close", 1, _close },
    { "recv", 1, _receive }
  };

ERL_NIF_INIT(mqueue_drv,nif_funcs,load_module,NULL,NULL,unload_module)
