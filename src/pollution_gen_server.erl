%%%-------------------------------------------------------------------
%%% @author Marcin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2019 12:25 PM
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-behaviour(gen_server).

-author("Marcin").

%% API
-export([init/1, handle_call/3, handle_cast/2]).
-export([start_link/0, addStation/2]).


%% START %%
start_link() ->
  gen_server:start_link(pollution_gen_server, pollution:createMonitor(), []).

%% CLIENT->SERVER INTERFACE %%
addStation(Pid, Name, {Lattitude, Longitude}) ->
  gen_server:call(Pid, get)

init(Args) ->
  erlang:error(not_implemented).

handle_call(Request, From, State) ->
  erlang:error(not_implemented).

handle_cast(Request, State) ->
  erlang:error(not_implemented).