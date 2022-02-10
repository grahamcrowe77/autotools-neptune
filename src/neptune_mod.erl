%%%-------------------------------------------------------------------
%%% @author Graham Crowe <graham.crowe@telia.com>
%%% @copyright (C) 2022, Graham Crowe
%%% @doc Neptune application
%%%
%%% creates a minimal skeleton code for either an Erlang application
%%% or an Erlang release.
%%% @end
%%% Created :  30 Jan 2022 by Graham Crowe <graham.crowe@telia.com>
%%%-------------------------------------------------------------------
-module(neptune_mod).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

-include_lib("kernel/include/file.hrl").

-export([parse_args/1, skeleton/1]).

%%--------------------------------------------------------------------
%% @doc
%% This function is called to parse the command line arguments and
%% return a map expressing the options and arguments as parameters
%% for creating skeleton code.
%% @end
%%--------------------------------------------------------------------
-spec parse_args(Args) -> Pars when
      Args :: [string()],
      Pars :: map().

parse_args(Args) ->
    Funs = [fun strings_to_tuples/1,
	    fun tuples_to_map/1],
    Pars = lists:foldl(
	    fun(Fun, Input) ->
		    apply(Fun, [Input])
	    end,
	    Args,
	    Funs),
    default_parameters(Pars).

%%--------------------------------------------------------------------
%% @doc
%% This function creates skeleton Erlang code based upon the input
%% expressed in the form of a map. It results in a minimal Erlang
%% application or release directory structure, built with GNU
%% Autotools.
%% @end
%%--------------------------------------------------------------------
-spec skeleton(Pars) -> ok | {error, Reason} when
      Pars   :: map(),
      Reason :: atom().
skeleton(#{name := Name, type := <<"app">>}=Pars) ->
    case filename:basename(Name) of
	Name ->
	    case read_template_tree(Pars) of
		{ok, Tree} ->
		    process_tree(Tree, Pars);
		{error, Reason} ->
		    {error, Reason}
	    end;
	_ ->
	    {error, bad_name}
    end;
skeleton(#{name := _Name, type := <<"rel">>}=_Pars) ->
    {error, not_implemented};
skeleton(_) ->
    {error, no_name}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
strings_to_tuples(Strings) ->
    Fun = fun([ $-, $- | OptName], Elements) ->
		  [{option_name, list_to_atom(OptName)} | Elements];
	     (Value, Elements) ->
		  [{value, Value} | Elements]
	  end,
    lists:foldr(Fun, [], Strings).

tuples_to_map(Tuples) ->
    tuples_to_map(Tuples, #{}).

tuples_to_map([], Map) ->
    Map;
tuples_to_map([{option_name, OptName}, {value, Value} | T], Map) ->
    tuples_to_map(T, Map#{OptName => list_to_binary(Value)});
tuples_to_map([{option_name, OptName} | T], Map) ->
    tuples_to_map(T, Map#{OptName => null});
%% Ignore values without option names except the last argument
tuples_to_map([{value, Value}], Map) ->
    Map#{name => list_to_binary(Value)};
tuples_to_map([_|T], Map) ->
    tuples_to_map(T, Map).

default_parameters(Pars) ->
    Defaults = [{type, <<"app">>},
		{outdir, <<".">>}],
    lists:foldl(
      fun({Name, Value}, Acc) ->
	      case maps:is_key(Name, Acc) of
		  true ->
		      Acc;
		  false ->
		      Acc#{Name => Value}
	      end
      end,
      Pars,
      Defaults).

read_template_tree(#{name := Name, sysconfdir := SysConfDir, type := Type}) ->
    Dir = filename:join(SysConfDir, Type),
    case file:read_file_info(Dir) of
	{ok, #file_info{type = directory}} ->
	    {ok, #{directory => Name, content => read_dir(Dir)}};
	_ ->
	    {error, name_is_dir}
    end.

read_dir(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    read_files(Dir, Files).

read_files(Dir, Files) ->
    Fun = fun(File) ->
		  FilePath = filename:join(Dir, File),
		  case file:read_file_info(FilePath) of
		      {ok, #file_info{type = regular}} ->
			  {ok, Binary} = file:read_file(FilePath),
			  #{file => filename:basename(FilePath),
			    content => Binary};
		      {ok, #file_info{type = directory}} ->
			  #{directory => filename:basename(FilePath),
			    content => read_dir(FilePath)}
		  end
	  end,
    lists:map(Fun, Files).

process_tree(Tree, Pars) ->
    Funs = [fun substitute_values/2,
	    fun rename_files/2,
	    fun write_app_structure/2],
    lists:foldl(
      fun(Fun, Input) ->
	      apply(Fun, [Input, Pars])
      end,
      Tree,
      Funs).

substitute_values(Tree, Pars) ->
    Substitutions = substitutions(Pars),
    Dir = maps:get(directory, Tree),
    Content = maps:get(content, Tree),
    NewContent = substitute_dir_values(Content, Substitutions),
    #{directory => Dir, content => NewContent}.

substitute_dir_values(DirContent, Substitutions) ->
    Fun = fun(#{file := _, content := Content}=File) ->
		  File#{content =>
			    substitute_file_values(
			      Content,
			      Substitutions)};
	     (#{directory := _, content := SubDirContent}=Dir) ->
		  Dir#{content =>
			   substitute_dir_values(
			     SubDirContent,
			     Substitutions)}
	  end,
    lists:map(Fun, DirContent).

substitute_file_values(Content, Substitutions) ->
    Fun = fun({RE, Replacement}, Subject) ->
		  re:replace(
		    Subject, RE, Replacement,
		    [{return, binary}, global])
	  end,
    lists:foldl(Fun, Content, Substitutions).

rename_files(Tree, #{name := Name}) ->
    Dir = maps:get(directory, Tree),
    Content = maps:get(content, Tree),
    NewContent = rename_dir_values(Content, Name),
    #{directory => Dir, content => NewContent}.

rename_dir_values(DirContent, Name) ->
    Fun = fun(#{file := FilePath}=File) ->
		  File#{file =>
			    re:replace(
			      FilePath, <<"name">>, Name,
			      [{return, binary}])};
	     (#{directory := _, content := SubDirContent}=Dir) ->
		  Dir#{content =>
			   rename_dir_values(
			     SubDirContent,
			     Name)}
	  end,
    lists:map(Fun, DirContent).

write_app_structure(Tree, #{outdir := OutDir}) ->
    Name = maps:get(directory, Tree),
    Dir = filename:join(OutDir, Name),
    case file:read_file_info(Dir) of
	{ok, #file_info{}} ->
	    {error, name_exists};
	_ ->
	    Content = maps:get(content, Tree),
	    ok = create_dir_content(Dir, Content)
    end.
 
create_dir_content(Dir, Content) ->
    ok = file:make_dir(Dir),
    Fun = fun(#{file := FilePath, content := FileContent}) ->
		  FullFilePath = filename:join(Dir, FilePath),
		  ok = file:write_file(FullFilePath, FileContent),
		  ok = set_file_access_mode(filename:basename(FilePath), FullFilePath);
	     (#{directory := Name, content := SubDirContent}) ->
		  SubDir = filename:join(Dir, Name),
		  ok = create_dir_content(SubDir, SubDirContent)
	  end,
    ok = lists:foreach(Fun, Content).

set_file_access_mode(<<"bootstrap.sh">>, FullFilePath) ->
    ok = file:write_file_info(FullFilePath, #file_info{mode = 8#00755});
set_file_access_mode(_, _) ->
    ok.

substitutions(Pars) ->
    [lowercase_package_name(Pars),
     uppercase_package_name(Pars),
     titlecase_package_name(Pars),
     package_version(Pars),
     erts_version(Pars),
     email(Pars),
     author(Pars),
     year(Pars),
     date(Pars)].

lowercase_package_name(#{name := Name}) ->
    {<<"%LC_PACKAGE_NAME%">>, string:lowercase(Name)}.

uppercase_package_name(#{name := Name}) ->
    {<<"%UC_PACKAGE_NAME%">>, string:uppercase(Name)}.

titlecase_package_name(#{name := Name}) ->
    {<<"%TC_PACKAGE_NAME%">>, string:titlecase(Name)}.

package_version(Pars) ->
    {<<"%PACKAGE_VERSION%">>, maps:get(version, Pars, <<"0.1.0">>)}.

erts_version(Pars) ->
    {<<"%ERLANG_ERTS_VER%">>, maps:get(erts_version, Pars, <<"10">>)}.

email(Pars) ->
    {<<"%EMAIL%">>, maps:get(email, Pars, <<"undisclosed email address">>)}.

author(Pars) ->
    {<<"%AUTHOR%">>, maps:get(author, Pars, <<"undeclared author">>)}.

year(_Pars) ->
    {{Year, _Month, _Day}, _Time} = calendar:local_time(),
    {<<"%YEAR%">>, integer_to_binary(Year)}.

date(_Pars) ->
    {{Year, Month, Day}, _Time} = calendar:local_time(),
    {<<"%DATE%">>,
     <<(integer_to_binary(Day))/binary, 32,
       (to_enum(month, Month))/binary, 32,
       (integer_to_binary(Year))/binary>>}.

to_enum(month, 1)  -> <<"Jan">>;
to_enum(month, 2)  -> <<"Feb">>;
to_enum(month, 3)  -> <<"Mar">>;
to_enum(month, 4)  -> <<"Apr">>;
to_enum(month, 5)  -> <<"May">>;
to_enum(month, 6)  -> <<"Jun">>;
to_enum(month, 7)  -> <<"Jul">>;
to_enum(month, 8)  -> <<"Aug">>;
to_enum(month, 9)  -> <<"Sep">>;
to_enum(month, 10) -> <<"Oct">>;
to_enum(month, 11) -> <<"Nov">>;
to_enum(month, 12) -> <<"Dec">>.

%% -------------------------------------------------------------------
%% Internal eunit tests
%% -------------------------------------------------------------------
-ifdef(TEST).

square_test_() ->
    [?_assertMatch(
	#{version := null},
	parse_args(["--version"])),
     ?_assertMatch(
	#{help := null},
	parse_args(["--help"])),
     ?_assertMatch(
	#{author := <<"John Doe">>,
	  name := <<"myapp">>},
	parse_args(["--author", "John Doe",
	       "myapp"])),
     ?_assertMatch(
	#{author := <<"John Doe">>,
	  email := <<"john.doe@neptune.org">>,
	  type := <<"application">>,
	  name := <<"myapp">>},
	parse_args(["--author", "John Doe",
	       "--email", "john.doe@neptune.org",
	       "--type", "application",
	       "myapp"])),
     ?_assertMatch(
	#{author := <<"John Doe">>,
	  email := <<"john.doe@neptune.org">>,
	  type := <<"release">>,
	  name := <<"myrel">>},
	parse_args(["--author", "John Doe",
	       "--email", "john.doe@neptune.org",
	       "--type", "release",
	       "myrel"]))
    ].

read_template_tree_test_() ->
    [?_assertMatch(
	{ok, #{content := _}},
	read_template_tree(
	  #{name => <<"myapp">>,
	    sysconfdir => os:getenv("SYSCONFDIR"),
	    type => "app"}))
    ].

substitute_dir_values_test_() ->
    [?_assert(
	begin
	    {ok, Tree} =
		read_template_tree(
		  #{name => <<"myapp">>,
		    sysconfdir => os:getenv("SYSCONFDIR"),
		    type => "app"}),
	    Content = maps:get(content, Tree),
	    Substitutions =
		[{<<"%LC_PACKAGE_NAME%">>,<<"myapp">>},
		 {<<"%UC_PACKAGE_NAME%">>,<<"MYAPP">>},
		 {<<"%TC_PACKAGE_NAME%">>,<<"Myapp">>},
		 {<<"%PACKAGE_VERSION%">>,<<"0.1.0">>},
		 {<<"%ERLANG_ERTS_VER%">>,<<"11">>},
		 {<<"%EMAIL%">>,<<"undisclosed email address">>},
		 {<<"%AUTHOR%">>,<<"undeclared author">>},
		 {<<"%YEAR%">>,<<"2022">>},
		 {<<"%DATE%">>,<<"30 Jan 2022">>}],
	    NewContent = substitute_dir_values(Content, Substitutions),
	    (is_list(NewContent)
	     andalso
	     length(NewContent) > 1)
	end)
    ].

rename_files_test_() ->
    [?_assertMatch(
	#{content := _},
	begin
	    {ok, Tree} =
		read_template_tree(
		  #{name => <<"myapp">>,
		    sysconfdir => os:getenv("SYSCONFDIR"),
		    type => "app"}),
	    rename_files(Tree, #{name => <<"myapp">>})
	end)
    ].

-endif.
%% -------------------------------------------------------------------
