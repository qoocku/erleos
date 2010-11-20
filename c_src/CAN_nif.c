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
#include "erl_nif.h"
#include "nif_utils.c"

/** Some common atoms **/
static ERL_NIF_TERM error_atom;
static ERL_NIF_TERM ok_atom;
static ERL_NIF_TERM eagain_atom;

struct CAN_handle {
  int device;
};

static ErlNifResourceType* CAN_handle_type = 0;

static int
load_module(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  ErlNifResourceFlags flags;
  CAN_handle_type = enif_open_resource_type(env, MODULE, "CAN_handle_type", NULL,
                                               ERL_NIF_RT_CREATE, &flags);
  error_atom  = make_atom(env, "error");
  ok_atom     = make_atom(env, "ok");
  eagain_atom = make_atom(env, "eagain");
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
upgrade_module (ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
  ErlNifResourceFlags flags;
  CAN_handle_type = enif_open_resource_type(env, MODULE, "CAN_handle_type", NULL,
                                               ERL_NIF_RT_TAKEOVER, &flags);
  return 0;
}

static ERL_NIF_TERM
_open (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  struct CAN_handle* handle = enif_alloc_resource(CAN_handle_type, sizeof(struct CAN_handle));
  ERL_NIF_TERM result;
  result = enif_make_tuple2(env, ok_atom, enif_make_resource(env, handle));
  enif_release_resource(handle);
  return result;
}

static ERL_NIF_TERM
_close (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  struct CAN_handle* handle;
  if (!enif_get_resource(env, argv[0], CAN_handle_type,(void**)&handle))
    return make_tuple2_result(env, error_atom, "Argument should be a handler");
  return enif_make_int(env, 0);
}

static ErlNifFunc nif_funcs[] =
  {
    { "open",  1, _open },
    { "close", 1, _close },
  };

ERL_NIF_INIT(CAN_drv, nif_funcs, load_module, reload_module, upgrade_module, unload_module);
