-module(neptune_rel_in_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

groups() ->
    [{build_in_src_tree,
      [configure,
       build,
       mostlyclean,
       maintainer_clean,
       remove
      ]}].

all() ->
    [neptune,
     bootstrap,
     {group, build_in_src_tree}].

init_per_suite(Config) ->
    {ok, CWD} = file:get_cwd(),
    Env = os:env(),
    SrcDir = filename:join(CWD, 'uranus-system'),
    BuildDir = SrcDir,
    [{env, Env},
     {srcdir, SrcDir},
     {builddir, BuildDir} | Config].

end_per_suite(_Config) ->
    ok.

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

environment() ->
    TopBuildDir = ct:get_config(top_builddir),
    Env = os:env(),
    {"PATH", Path} = lists:keyfind("PATH", 1, Env),
    case re:run(Path, "neptune") of
	{match, _} ->
	    Env;
	nomatch ->
	    NewPath = filename:join(TopBuildDir, bin) ++ [$: | Path],
	    lists:keyreplace("PATH", 1, Env, {"PATH", NewPath})
    end.

neptune(_Config) ->
    Env = environment(),
    {ok, CWD} = file:get_cwd(),
    Port = open_port(
	     {spawn, "neptune --type rel uranus"},
	     port_opts(CWD, Env)),
    ok = get_response(Port, []).

bootstrap(Config) ->
    Env = ?config(env, Config),
    SrcDir = ?config(srcdir, Config),
    Port = open_port(
	     {spawn, "./bootstrap.sh"},
	     port_opts(SrcDir, Env)),
    ok = get_response(Port, []).

configure(Config) ->
    Env = ?config(env, Config),
    BuildDir = ?config(builddir, Config),
    Port = open_port(
	     {spawn, "./configure"},
	     port_opts(BuildDir, Env)),
    ok = get_response(Port, []).

build(Config) ->
    Env = ?config(env, Config),
    BuildDir = ?config(builddir, Config),
    Port = open_port(
	     {spawn, "make"},
	     port_opts(BuildDir, Env)),
    ok = get_response(Port, []).

mostlyclean(Config) ->
    Env = ?config(env, Config),
    BuildDir = ?config(builddir, Config),
    Port = open_port(
	     {spawn, "make mostlyclean"},
	     port_opts(BuildDir, Env)),
    ok = get_response(Port, []).

maintainer_clean(Config) ->
    Env = ?config(env, Config),
    BuildDir = ?config(builddir, Config),
    Port = open_port(
	     {spawn, "make maintainer-clean"},
	     port_opts(BuildDir, Env)),
    ok = get_response(Port, []).

remove(Config) ->
    SrcDir = ?config(srcdir, Config),
    ok = file:del_dir_r(SrcDir).

port_opts(Dir, Env) ->
    [{cd, Dir},
     {env, Env},
     stderr_to_stdout,
     exit_status].

get_response(Port, Acc) ->
    receive
	{Port, {data, Data}} ->
	    get_response(Port, [Data | Acc]);
	{Port, {exit_status, 0}}->
	    ok = pal(Acc);
	{Port, {exit_status, ExitStatus}}->
	    ok = pal(Acc),
	    {error, ExitStatus}
    end.

pal(Acc) ->
    ct:pal("~s", [lists:reverse(Acc)]).
