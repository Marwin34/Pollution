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
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([start_link/0, addStation/3, addValue/5, removeValue/4, getOneValue/4,
  getStationMean/3, getDailyMean/3, getCorrelation/3, stop/1]).


%% START %%
start_link() ->
  gen_server:start_link({local, pollution_server}, pollution_gen_server, pollution:createMonitor(), []).

%% USER INTERFACE%%
addStation(Pid, Name, {Latitude, Longitude}) ->
  gen_server:call(Pid, {addStation, Name, {Latitude, Longitude}}).

addValue(Pid, Key, Date, Type, Value) ->
  gen_server:call(Pid, {addValue, Key, Date, Type, Value}).

removeValue(Pid, Key, Date, Type) ->
  gen_server:call(Pid, {removeValue, Key, Date, Type}).

getOneValue(Pid, Key, Date, Type) ->
  gen_server:call(Pid, {genOneValue, Key, Date, Type}).

getStationMean(Pid, Key, Type) ->
  gen_server:call(Pid, {getStationMean, Key, Type}).

getDailyMean(Pid, Date, Type) ->
  gen_server:call(Pid, {getDailyMean, Date, Type}).

getCorrelation(Pid, Type1, Type2) ->
  gen_server:call(Pid, {getCorrelation, Type1, Type2}).

init(InitialValue) ->
  {ok, InitialValue}.

stop(Pid) ->
  gen_server:cast(Pid, stop).

%% CALLBACKS %%
handle_call(Message, _From, State) ->
  case Message of
    {addStation, Name, Coordinates} -> Result = pollution:addStation(Name, Coordinates, State);
    {addValue, Key, Date, Type, Value} -> Result = pollution:addValue(Key, Date, Type, Value, State);
    {removeValue, Key, Date, Type} -> Result = pollution:removeValue(Key, Date, Type, State);
    {genOneValue, Key, Date, Type} -> Result = pollution:getOneValue(Key, Date, Type, State);
    {getStationMean, Key, Type} -> Result = pollution:getStationMean(Key, Type, State);
    {getDailyMean, Date, Type} -> Result = pollution:getDailyMean(Date, Type, State);
    {getCorrelation, Type1, Type2} -> Result = pollution:getCorrelation(Type1, Type2, State)
  end,
  case Result of
    {error, Message} -> {reply, Message, State};
    NewState -> {reply, ok, NewState}
  end.

handle_cast(stop, State) ->
  {stop, normal, State}.

terminate(Reason, Value) ->
  io:format("Server: exit with value ~p~n", [Value]),
  Reason.