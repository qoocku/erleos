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
    str = (const char*)strerror(err);
  }
  return make_atom(env, str);
}
