/**
 * Erlang NIF Utility Functions.
 *
 * @date 2010-11-20
 * @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
 */

#include <errno.h>

static ERL_NIF_TERM
make_tuple2_result (ErlNifEnv* env, ERL_NIF_TERM atom, const char* snd)
{
  ERL_NIF_TERM string;
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
errno_atom (ErlNifEnv* env, int err)
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
    return enif_make_string(env, (const char*)strerror(err), ERL_NIF_LATIN1);
  }
  return make_atom(env, str);
}

static void
debug_enif_type (ErlNifEnv* env, ERL_NIF_TERM e)
{
  if (enif_is_atom(env, e))
      enif_fprintf(stdout, "atom\n");
    else if (enif_is_binary(env, e))
      enif_fprintf(stdout, "bin\n");
    else if (enif_is_empty_list(env, e))
      enif_fprintf(stdout, "empty list\n");
    else if (enif_is_fun(env, e))
      enif_fprintf(stdout, "fun\n");
    else if (enif_is_pid(env, e))
      enif_fprintf(stdout, "pid\n");
    else if (enif_is_port(env, e))
      enif_fprintf(stdout, "port\n");
    else if (enif_is_ref(env, e))
      enif_fprintf(stdout, "ref\n");
    else if (enif_is_tuple(env, e))
      enif_fprintf(stdout, "tuple\n");
    else if (enif_is_list(env, e))
      enif_fprintf(stdout, "list\n");
}
