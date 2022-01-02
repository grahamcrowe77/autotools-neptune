#include <erl_nif.h>

extern int double_up(int x);
extern int square(int y);

static ERL_NIF_TERM double_up_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int x, ret;
    if (!enif_get_int(env, argv[0], &x)) {
	return enif_make_badarg(env);
    }
    ret = double_up(x);
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM square_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int y, ret;
    if (!enif_get_int(env, argv[0], &y)) {
	return enif_make_badarg(env);
    }
    ret = square(y);
    return enif_make_int(env, ret);
}

static ErlNifFunc nif_funcs[] = {
    {"double_up", 1, double_up_nif},
    {"square", 1, square_nif}
};

int double_up(int x)
{
  return (x + x);
}

int square(int y)
{
  return (y * y);
}

ERL_NIF_INIT(neptune_nif, nif_funcs, NULL, NULL, NULL, NULL)
