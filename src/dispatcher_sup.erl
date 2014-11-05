%%%-------------------------------------------------------------------
%%% @author shepver
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Окт. 2014 11:05
%%%-------------------------------------------------------------------
-module(dispatcher_sup).
-author("shepver").

-behaviour(supervisor).
-include("statement.hrl").
%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([start_script/1]).
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


start_script(Script)->
   {ok, Pid} = supervisor:start_child(dispatcher_sv, {
      Script#script.name,
      {executor, start_link, [Script]},
      transient,
      2000,
      worker,
      [executor]
   }),
   Pid.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).


init([disp]) ->
  {ok, {{one_for_one, 1, 10}, []}};

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  Pserver = {dispatcher_srv, {dispatcher_srv, start_link, []},
    Restart, Shutdown, Type, [dispatcher_srv]},
  Supervisor =
    {dispatcher_sv,
      {supervisor, start_link, [{local, dispatcher_sv}, ?MODULE, [disp]]},
      permanent,
      infinity,
      supervisor,
      []
    },
  {ok, {SupFlags, [Supervisor,Pserver]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
