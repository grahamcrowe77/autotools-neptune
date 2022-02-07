-module(%LC_APP_NAME%_SUITE).

-include("%LC_APP_NAME%.hrl").
-include("%LC_APP_NAME%_priv.hrl").

-compile(export_all).

all() ->
    [application,
     version,
     square_integer,
     square_string].

application(_) ->
    false = lists:keyfind(%LC_APP_NAME%, 1, application:which_applications()),
    ok = application:start(%LC_APP_NAME%),
    {%LC_APP_NAME%, "%TC_APP_NAME% application", "%APP_VERSION%"} =
	lists:keyfind(%LC_APP_NAME%, 1, application:which_applications()),
    ok = application:stop(%LC_APP_NAME%),
    false = lists:keyfind(%LC_APP_NAME%, 1, application:which_applications()),
    ok.

version(_) ->
    ok = application:start(%LC_APP_NAME%),
    {ok, Version} = %LC_APP_NAME%_server:version(),
    ok = ct:pal("%LC_APP_NAME% Version: ~s", [Version]),
    ok = application:stop(%LC_APP_NAME%).

square_integer(_) ->
    ok = application:start(%LC_APP_NAME%),
    {ok, Message} = %LC_APP_NAME%_server:message({square, 6}),
    ok = ct:pal("~s", [Message]),
    ok = application:stop(%LC_APP_NAME%).

square_string(_) ->
    ok = application:start(%LC_APP_NAME%),
    {ok, Message} = %LC_APP_NAME%_server:message({square, "6"}),
    ok = ct:pal("~s", [Message]),
    ok = application:stop(%LC_APP_NAME%).
