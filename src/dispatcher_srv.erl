%%%-------------------------------------------------------------------
%%% @author shepver
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Окт. 2014 17:01
%%%-------------------------------------------------------------------
-module(dispatcher_srv).
-author("shepver").
-behaviour(gen_server).
-include("statement.hrl").

-define(Config, "config/script.conf").
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(script_enable, {name, port}).
-record(script_list, {list, timer}).
-record(state, {scripts, list_enable, base_list,timer_script}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  Scripts = dict:new(),
  case file:consult(?Config) of
    {ok, List} ->
      TimerList = erlang:send_after(120000, self(), chesk_list),
      TimerScript  = erlang:send_after(100, self(), {run_list,List}),
      {ok, #state{
        scripts = Scripts,
        list_enable = dict:new(),
        base_list = #script_list{list = List, timer = TimerList},
        timer_script = TimerScript
      }};
    {error, Reason} -> {stop, Reason}
  end.

add_dict(Db, []) ->
  Db;
add_dict(Db, [H | T]) ->
  {ok, Pid} = supervisor:start_child(dispatcher_sv, {
    element(1, H),
    {executor, start_link, [
      #script{
        name = element(1, H),
        command = element(2, H),
        interval = element(3, H)
      }]},
    transient,
    2000,
    worker,
    [executor]
  }),
  NewDb = dict:store(element(1, H),
    Pid
%%   #script{
%%      name = element(1, H),
%%      command = element(2, H),
%%      interval = element(3, H),
%%      timer = erlang:send_after(1,
%%        self(),
%%        {run, element(1, H)})
%%    }
    , Db),

  add_dict(NewDb, T).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_info({run, Name}, #state{scripts = Scripts, list_enable = ListEnable} = State) ->
  case dict:find(Name, Scripts) of
    error -> {noreply, State};
    {ok, Script} ->
      erlang:cancel_timer(Script#script.timer),
      Port = open_port({spawn, Script#script.command},
        [{line, 100}, exit_status, stderr_to_stdout, binary]),
      error_logger:info_msg("script start work ~p .~n", [Port]),
      Timer = erlang:send_after(Script#script.interval * 1000, self(), {run, Name}),
      NewScript = Script#script{timer = Timer, port = Port},
      NewScripts = dict:store(Name, NewScript, Scripts),
      NewListEnable = dict:store(Port, #script_enable{name = Name, port = Port}, ListEnable),
      {noreply, State#state{scripts = NewScripts, list_enable = NewListEnable}}
  end;

handle_info({run_list,List}, #state{scripts = Scripts,base_list = BaseList,timer_script = Timer} = State) ->
  %% проверка на изменение списка запускаемых скриптов
  erlang:cancel_timer(Timer),
  NewScripts = add_dict(Scripts,List),
  error_logger:info_msg("Check base list ~p .~n", [BaseList]),
  {noreply, State#state{scripts = NewScripts}};

handle_info(chesk_list, #state{base_list = BaseList} = State) ->
  %% проверка на изменение списка запускаемых скриптов
  error_logger:info_msg("Check base list ~p .~n", [BaseList]),
  {noreply, State};

handle_info({_Port, {data, Data}}, State) ->
  case Data of
    {eol, Line} ->
      error_logger:info_msg("Message ~ts .~n", [Line]);
  %%error_logger:info_msg("Message ~ts .~n", [unicode:characters_to_list(erlang:binary_to_list(Line),utf8)]);
    {noeol, Line} ->
      error_logger:info_msg("Message ~p .~n", [Line])
  end,
  {noreply, State};

handle_info({Port, {exit_status, Status}}, State) ->
  error_logger:info_msg("script finish work ~p status ~p.~n", [Port, Status]),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
