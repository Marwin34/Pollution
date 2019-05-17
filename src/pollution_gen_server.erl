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
-export([start/0, addStation/3, addValue/5, removeValue/4, getOneValue/4,
  getStationMean/3, getDailyMean/3, getCorrelation/3, stop/1, crash/1]).
-export([test/0]).

%% START %%
start() ->
  gen_server:start_link({local, pollution_server}, pollution_gen_server, pollution:createMonitor(), []).

%% USER INTERFACE%%
addStation(Pid, Name, {Latitude, Longitude}) ->
  gen_server:call(Pid, {addStation, Name, {Latitude, Longitude}}).

addValue(Pid, Key, Date, Type, Value) ->
  gen_server:call(Pid, {addValue, Key, Date, Type, Value}).

removeValue(Pid, Key, Date, Type) ->
  gen_server:call(Pid, {removeValue, Key, Date, Type}).

getOneValue(Pid, Key, Date, Type) ->
  gen_server:call(Pid, {getOneValue, Key, Date, Type}).

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

crash(Pid) ->
  gen_server:cast(Pid, crash).

%% CALLBACKS %%
handle_call(Message, _From, State) ->
  case Message of
    {addStation, Name, Coordinates} -> Result = {monitor, pollution:addStation(Name, Coordinates, State)};
    {addValue, Key, Date, Type, Value} -> Result = {monitor, pollution:addValue(Key, Date, Type, Value, State)};
    {removeValue, Key, Date, Type} -> Result = {monitor, pollution:removeValue(Key, Date, Type, State)};
    {getOneValue, Key, Date, Type} -> Result = {value, pollution:getOneValue(Key, Date, Type, State)};
    {getStationMean, Key, Type} -> Result = {value, pollution:getStationMean(Key, Type, State)};
    {getDailyMean, Date, Type} -> Result = {value, pollution:getDailyMean(Date, Type, State)};
    {getCorrelation, Type1, Type2} -> Result = {value, pollution:getCorrelation(Type1, Type2, State)}
  end,
  fetchResult(Result, State).

handle_cast(Request, State) ->
  case Request of
    stop -> {stop, normal, State};
    crash -> 1 / 0
  end.

terminate(Reason, Value) ->
  io:format("Server: exit with value ~p~n", [Value]),
  Reason.

fetchResult(Result, State) ->
  case Result of
    {monitor, Value} ->
      case Value of
        {error, Message} -> {reply, Message, State};
        _ -> {reply, ok, Value}
      end;
    {value, Value} ->
      case Value of
        {error, Message} -> {reply, Message, State};
        _ -> {reply, Value, State}
      end
  end.

test() ->
  {ok, Pid} = start(),
  Value1 = addStation(Pid, "Ionia", {45.6, 78.9}),
  io:format("Response: ~p~n", [Value1]),
  Value2 = addValue(Pid, "Ionia", {{2019, 5, 17}, {15, 51, 4}}, "PM10", 125.0),
  io:format("Response: ~p~n", [Value2]),
  Value3 = getOneValue(Pid, "Ionia", {{2019, 5, 17}, {15, 51, 4}}, "PM10"),
  io:format("Response: ~p~n", [Value3]),
  Value4 = getStationMean(Pid, "Ionia", "PM10"),
  io:format("Response: ~p~n", [Value4]),
  Value5 = getDailyMean(Pid, {{2019, 5, 17}, {15, 51, 4}}, "PM10"),
  io:format("Response: ~p~n", [Value5]),
  Value6 = getCorrelation(Pid, "PM10", "PM2,5"),
  io:format("Response: ~p~n", [Value6]),
  Value7 = removeValue(Pid, "Ionia", {{2019, 5, 17}, {15, 51, 4}}, "PM10"),
  io:format("Response: ~p~n", [Value7]),
  Value8 = stop(Pid),
  io:format("Response: ~p~n", [Value8]).