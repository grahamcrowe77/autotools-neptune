#include <erl_nif.h>

extern int square(int y);

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
    {"square", 1, square_nif}
};

int square(int y)
{
  return (y * y);
}

ERL_NIF_INIT(%LC_APP_NAME%_nif, nif_funcs, NULL, NULL, NULL, NULL)
