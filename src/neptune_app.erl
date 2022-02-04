%%%-------------------------------------------------------------------
%%% @author Graham Crowe <graham.crowe@telia.com>
%%% @copyright (C) 2022, Graham Crowe
%%% @doc Neptune application
%%%
%%% creates a minimal Erlang application.
%%% @end
%%% Created :  30 Jan 2022 by Graham Crowe <graham.crowe@telia.com>
%%%-------------------------------------------------------------------
-module(neptune_app).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

-include_lib("kernel/include/file.hrl").

-export([create/1]).

%%--------------------------------------------------------------------
%% @doc
%% This function creates an Erlang application based upon the input
%% expressed in the form of a map. It results in a minimal Erlang
%% application directory structure, built with GNU Autotools.
%% @end
%%--------------------------------------------------------------------
-spec create(Pars :: map()) -> ok | {error, Reason :: io_lib:chars()}.
create(#{name := Name}=Pars) ->
    case filename:basename(Name) of
	Name ->
	    case read_template_tree(Pars) of
		{ok, Tree} ->
		    process_tree(Tree, Pars);
		{error, Reason} ->
		    {error, Reason}
	    end;
	_ ->
	    {error, io_lib:format("~s includes a path", [Name])}
    end;
create(_) ->
    {error, "no name for application was provided"}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
read_template_tree(#{name := Name, sysconfdir := SysConfDir, type := Type}) ->
    Dir = filename:join(SysConfDir, Type),
    case file:read_file_info(Dir) of
	{ok, #file_info{type = directory}} ->
	    {ok, #{directory => Name, content => read_dir(Dir)}};
	_ ->
	    Error = io_lib:format("~s is not a directory", [Dir]),
	    {error, Error}
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
    Substitutions = neptune_subs:main(Pars),
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
	    Reason = io_lib:format("~s already exists", [Name]),
	    {error, Reason};
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

%% -------------------------------------------------------------------
%% Internal eunit tests
%% -------------------------------------------------------------------
-ifdef(TEST).

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
