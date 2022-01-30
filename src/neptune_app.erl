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
	    {ok, (read_dir(Dir))#{name => Name}};
	_ ->
	    Error = io_lib:format("~s is not a directory", [Dir]),
	    {error, Error}
    end.

read_dir(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    #{directory => read_files(Dir, Files)}.

read_files(Dir, Files) ->
    Fun = fun(File) ->
		  FilePath = filename:join(Dir, File),
		  case file:read_file_info(FilePath) of
		      {ok, #file_info{type = regular}} ->
			  {ok, Binary} = file:read_file(FilePath),
			  #{file => File, content => Binary};
		      {ok, #file_info{type = directory}} ->
			  #{name => File, directory => read_dir(FilePath)}
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

substitute_values(Tree, _Pars) ->
    Tree.

rename_files(Tree, _Pars) ->
    Tree.

write_app_structure(_Tree, _Pars) ->
    ok.

lowercase_package_name(#{name := Name}) ->
    string:lowercase(Name).

uppercase_package_name(#{name := Name}) ->
    string:uppercase(Name).

titlecase_package_name(#{name := Name}) ->
    string:titlecase(Name).

%LC_PACKAGE_NAME%
%UC_PACKAGE_NAME%
%CAPS_PACKAGE_NAME%
%PACKAGE_VERSION%
%EMAIL%
%ERLANG_ERTS_VER%
%YEAR%
%AUTHOR%
%DATE%
