%%%-------------------------------------------------------------------
%%% @author Graham Crowe <graham.crowe@telia.com>
%%% @copyright (C) %YEAR%, %CAP_AUTHOR%
%%% @doc Neptune arguments
%%%
%%% parses the command line arguments returning an erlang map.
%%% @end
%%% Created :  %DATE% by Graham Crowe <graham.crowe@telia.com>
%%%-------------------------------------------------------------------
-module(neptune_args).

-export([parse/1]).

%%--------------------------------------------------------------------
%% @doc
%% This function is called to parse the command line and return a map
%% expressing the command line options and arguments.
%% @end
%%--------------------------------------------------------------------
-spec parse(CmdLine :: string()) -> Cmd :: map().
parse(_CmdLine) ->
    #{}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
