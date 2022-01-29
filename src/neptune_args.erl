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

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

-export([parse/1]).

%%--------------------------------------------------------------------
%% @doc
%% This function is called to parse the command line and return a map
%% expressing the command line options and arguments.
%% @end
%%--------------------------------------------------------------------
-spec parse(CmdLine :: [string()]) -> Cmd :: map().
parse(CmdLine) ->
    Funs = [fun strings_to_tuples/1,
	    fun tuples_to_map/1],
    lists:foldl(
      fun(Fun, Input) ->
	      apply(Fun, [Input])
      end,
      CmdLine,
      Funs).

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
    tuples_to_map(T, Map#{OptName => Value});
tuples_to_map([{option_name, OptName} | T], Map) ->
    tuples_to_map(T, Map#{OptName => null});
tuples_to_map([_|T], Map) ->
    tuples_to_map(T, Map).

%% -------------------------------------------------------------------
%% Internal eunit tests
%% -------------------------------------------------------------------
-ifdef(TEST).

square_test_() ->
    [?_assertMatch(
	#{version := null},
	parse(["--version"])),
     ?_assertMatch(
	#{author := "John Doe",
	  version := null},
	parse(["--author","John Doe","--version"])),
     ?_assertMatch(
	#{author := "John Doe",
	  email := "john.doe@neptune.org",
	  version := null},
	parse(["--author","John Doe","--version","--email","john.doe@neptune.org"]))
].

-endif.
%% -------------------------------------------------------------------
