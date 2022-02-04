-module(neptune_out_of_source_tree_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

groups() ->
    [{build_out_of_src_tree,
      [configure,
       build,
       mostlyclean,
       maintainer_clean
      ]}
    ].

all() ->
    [neptune,
     bootstrap,
     make_build_dir,
     {group, build_out_of_src_tree}
    ].

init_per_suite(Config) ->
    Env = env(Config),
    ok = ct:pal("~p", [Env]),
    file:del_dir_r("/tmp/myapp"),
    file:del_dir_r("/tmp/build"),
    SrcDir = filename:join("/tmp", myapp),
    BuildDir = filename:join("/tmp", build),
    [{env, Env},
     {tmpdir, "/tmp"},
     {srcdir, SrcDir},
     {builddir, BuildDir} | Config].

end_per_suite(_Config) ->
    file:del_dir_r("/tmp/myapp"),
    file:del_dir_r("/tmp/build").

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

env(_Config) ->
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

neptune(Config) ->
    Env = ?config(env, Config),
    Dir = ?config(tmpdir, Config),
    Port = open_port(
	     {spawn, "neptune --outdir " ++ Dir ++ " myapp"},
	     port_opts(Dir, Env)),
    ok = get_response(Port, []).

bootstrap(Config) ->
    Env = ?config(env, Config),
    SrcDir = ?config(srcdir, Config),
    Port = open_port(
	     {spawn, "./bootstrap.sh"},
	     port_opts(SrcDir, Env)),
    ok = get_response(Port, []).

make_build_dir(Config) ->
    BuildDir = ?config(builddir, Config),
    ok = file:make_dir(BuildDir).

configure(Config) ->
    Env = ?config(env, Config),
    SrcDir = ?config(srcdir, Config),
    BuildDir = ?config(builddir, Config),
    Port = open_port(
	     {spawn, filename:join(SrcDir, configure)},
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
