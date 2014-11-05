%%%-------------------------------------------------------------------
%%% @author shepver
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Окт. 2014 9:40
%%%-------------------------------------------------------------------
-author("shepver").
-define(Starter, php).
-define(Dir,"priv/").

-record(script, {name, command, interval, timer, port, copy}).

