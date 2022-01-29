%%%-------------------------------------------------------------------
%%% @author %CAP_AUTHOR% %email%
%%% @copyright (C) %YEAR%, %CAP_AUTHOR%
%%% @doc %CAP_PACKAGE_NAME% NIF Module
%%%
%%% demonstrates a trivial example of a top level supervisor,
%%% supervising a trivial server.
%%% @end
%%% Created :  %DATE% by %CAP_AUTHOR% %email%
%%%-------------------------------------------------------------------
-module(%LC_PACKAGE_NAME%_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, {already_started, Pid :: pid()}} |
	  {error, {shutdown, term()}} |
	  {error, term()} |
	  ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
	  {ok, {SupFlags :: supervisor:sup_flags(),
		[ChildSpec :: supervisor:child_spec()]}} |
	  ignore.
init([]) ->

    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    AChild = #{id => %LC_PACKAGE_NAME%_server,
	       start => {%LC_PACKAGE_NAME%_server, start_link, []},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => [%LC_PACKAGE_NAME%_server]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
