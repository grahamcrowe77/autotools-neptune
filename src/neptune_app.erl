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

-include_lib("kernel/include/file.hrl").

-export([create/1]).

-compile([export_all]).

%%--------------------------------------------------------------------
%% @doc
%% This function creates an Erlang application based upon the input
%% expressed in the form of a map. It results in a minimal Erlang
%% application directory structure, built with GNU Autotools.
%% @end
%%--------------------------------------------------------------------
-spec create(Pars :: map()) -> ok | {error, Reason :: io_lib:chars()}.
create(#{name := _}=Pars) ->
    %% Read app template structure as a tree
    %% #{dir := Name, list := [files and directories]}
    %% #{file := Name, data := data}
    %% Traverse tree and search/replace all data accordingly
    %% Traverse tree and rename files according to Name
    %% Use to tree to create files and dirs accordingly
    case read_template_tree(Pars) of
	{ok, Tree} ->
	    ok = process_tree(Tree, Pars);
	{error, Reason} ->
	    {error, Reason}
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
			  #{file => FilePath, content => Binary};
		      {ok, #file_info{type = directory}} ->
			  #{directory => FilePath, content => read_dir(FilePath)}
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
    Fun = fun(#{file := FilePath, content := Content}) ->
		  substitute_file_values(FilePath, Content, Substitutions);
	     (#{directory := SubDir, content := SubDirContent}) ->
		  #{directory => SubDir,
		    content =>
			substitute_dir_values(
			  SubDirContent,
			  Substitutions)}
	  end,
    lists:map(Fun, DirContent).

substitute_file_values(FilePath, Content, Substitutions) ->
    Fun = fun({RE, Replacement}, Subject) ->
		  re:replace(
		    Subject, RE, Replacement,
		    [{return, binary}, global])
	  end,
    NewContent = lists:foldl(Fun, Content, Substitutions),
    #{file => FilePath, content => NewContent}.

rename_files(Tree, #{name := Name}) ->
    Dir = maps:get(directory, Tree),
    Content = maps:get(content, Tree),
    NewContent = rename_dir_values(Content, Name),
    #{directory => Dir, content => NewContent}.

rename_dir_values(DirContent, Name) ->
    Fun = fun(#{file := FilePath, content := Content}) ->
		  #{file =>
			re:replace(
			  FilePath, <<"name">>, Name,
			  [{return, binary}]),
		    content => Content};
	     (#{directory := SubDir, content := SubDirContent}) ->
		  #{directory => SubDir,
		    content =>
			rename_dir_values(
			  SubDirContent,
			  Name)}
	  end,
    lists:map(Fun, DirContent).

write_app_structure(Tree, _Pars) ->
    Tree.
