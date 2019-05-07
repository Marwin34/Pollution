%%%-------------------------------------------------------------------
%%% @author Marcin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2019 11:47 AM
%%%-------------------------------------------------------------------
-module(pollution_server_sup).
-author("Marcin").

%% API
-export([start/0]).

start() ->
  process_flag(trap_exit, true),
  register(p_server, spawn_link(pollution_server, init, [])),
  io:format("Pollution server started.\n"),
  receive
    {'EXIT', _, _} -> io:format("EXIT.\n"), start()
  end.