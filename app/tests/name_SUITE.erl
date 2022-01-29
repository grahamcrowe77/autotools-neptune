-module(%LC_PACKAGE_NAME%_SUITE).

-include("%LC_PACKAGE_NAME%.hrl").
-include("%LC_PACKAGE_NAME%_priv.hrl").

-compile(export_all).

all() ->
    [application,
     version,
     square_integer,
     square_string].

application(_) ->
    false = lists:keyfind(%LC_PACKAGE_NAME%, 1, application:which_applications()),
    ok = application:start(%LC_PACKAGE_NAME%),
    {%LC_PACKAGE_NAME%, "%LC_PACKAGE_NAME% application", _} =
	lists:keyfind(%LC_PACKAGE_NAME%, 1, application:which_applications()),
    ok = application:stop(%LC_PACKAGE_NAME%),
    false = lists:keyfind(%LC_PACKAGE_NAME%, 1, application:which_applications()),
    ok.

version(_) ->
    ok = application:start(%LC_PACKAGE_NAME%),
    {ok, Version} = %LC_PACKAGE_NAME%_server:version(),
    ok = ct:pal("%LC_PACKAGE_NAME% Version: ~s", [Version]),
    ok = application:stop(%LC_PACKAGE_NAME%).

square_integer(_) ->
    ok = application:start(%LC_PACKAGE_NAME%),
    {ok, Message} = %LC_PACKAGE_NAME%_server:message({square, 6}),
    ok = ct:pal("~s", [Message]),
    ok = application:stop(%LC_PACKAGE_NAME%).

square_string(_) ->
    ok = application:start(%LC_PACKAGE_NAME%),
    {ok, Message} = %LC_PACKAGE_NAME%_server:message({square, "6"}),
    ok = ct:pal("~s", [Message]),
    ok = application:stop(%LC_PACKAGE_NAME%).
